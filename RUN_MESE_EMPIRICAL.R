# Distribution launcher for the archived empirical engine.
# New replication entry point; not the historical launcher.
# Default: checks only. No automatic package installation.

RUN_STAGE <- "check" # check | pilot | full | collect

# Leave blank when using source() or Rscript.
# When pasting into the console, an explicit directory can be supplied.
PROJECT_DIR <- ""

DATASET <- "tourism_monthly"
WORKERS <- 6L
ORIGIN_OFFSETS <- 0L

OUTPUT_ROOT <- "replication_runs/empirical"
COLLECT_FOLDER <- ""

CONFIRM_FULL_RUN <- FALSE
RETRY_FAILED_FITS <- FALSE
RETRY_FAILED_INTERVALS <- FALSE

local({
  must <- function(ok, message) {
    if (!isTRUE(ok)) {
      stop(message, call. = FALSE)
    }
  }
  
  scalar_string <- function(x) {
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x)
  }
  
  absolute_path <- function(path) {
    grepl("^(?:[A-Za-z]:|/|\\\\)", path, perl = TRUE)
  }
  
  must(
    scalar_string(RUN_STAGE) &&
      RUN_STAGE %in% c("check", "pilot", "full", "collect"),
    "Invalid RUN_STAGE."
  )
  
  must(
    scalar_string(PROJECT_DIR) &&
      scalar_string(OUTPUT_ROOT) &&
      scalar_string(COLLECT_FOLDER),
    "Directory settings must be single, nonmissing strings."
  )
  
  must(
    length(WORKERS) == 1L &&
      is.finite(WORKERS) &&
      WORKERS >= 1 &&
      WORKERS == as.integer(WORKERS),
    "WORKERS must be a positive integer."
  )
  
  must(
    nzchar(trimws(OUTPUT_ROOT)),
    "OUTPUT_ROOT must not be empty."
  )
  
  must(
    is.logical(CONFIRM_FULL_RUN) &&
      length(CONFIRM_FULL_RUN) == 1L &&
      !is.na(CONFIRM_FULL_RUN) &&
      is.logical(RETRY_FAILED_FITS) &&
      length(RETRY_FAILED_FITS) == 1L &&
      !is.na(RETRY_FAILED_FITS) &&
      is.logical(RETRY_FAILED_INTERVALS) &&
      length(RETRY_FAILED_INTERVALS) == 1L &&
      !is.na(RETRY_FAILED_INTERVALS),
    "Confirmation and retry settings must be TRUE or FALSE."
  )
  
  # Resolve the project directory without changing getwd().
  project <- NULL
  
  if (nzchar(trimws(PROJECT_DIR))) {
    project <- normalizePath(
      path.expand(PROJECT_DIR),
      winslash = "/",
      mustWork = TRUE
    )
    
    must(
      dir.exists(project),
      "PROJECT_DIR is not a directory."
    )
  } else {
    frames <- sys.frames()
    
    source_files <- unlist(
      lapply(frames, function(frame) {
        value <- frame$ofile
        
        if (
          is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          nzchar(value)
        ) {
          value
        } else {
          character()
        }
      }),
      use.names = FALSE
    )
    
    if (length(source_files)) {
      script <- normalizePath(
        tail(source_files, 1L),
        winslash = "/",
        mustWork = TRUE
      )
      
      project <- dirname(script)
    } else {
      script_args <- grep(
        "^--file=",
        commandArgs(trailingOnly = FALSE),
        value = TRUE
      )
      
      if (length(script_args)) {
        script <- sub(
          "^--file=",
          "",
          script_args[1L]
        )
        
        script <- normalizePath(
          script,
          winslash = "/",
          mustWork = TRUE
        )
        
        project <- dirname(script)
      } else {
        project <- normalizePath(
          getwd(),
          winslash = "/",
          mustWork = TRUE
        )
      }
    }
  }
  
  locate_engines <- function(directory) {
    files <- list.files(
      directory,
      full.names = TRUE,
      all.files = TRUE,
      recursive = FALSE
    )
    
    info <- file.info(files)
    
    files <- files[
      !is.na(info$isdir) & !info$isdir
    ]
    
    find <- function(filename) {
      matches <- files[
        tolower(basename(files)) == tolower(filename)
      ]
      
      must(
        length(matches) <= 1L,
        paste("Ambiguous engine filenames:", filename)
      )
      
      if (!length(matches)) {
        return(NA_character_)
      }
      
      normalizePath(
        matches,
        winslash = "/",
        mustWork = TRUE
      )
    }
    
    c(
      core = find("MESE_v7_engine_native_fix.R"),
      empirical = find("MESE_EMPIRICAL_REVIEW_ENGINE_v2.R")
    )
  }
  
  engines <- locate_engines(project)
  
  # Interactive fallback is limited to check mode.
  if (
    anyNA(engines) &&
    RUN_STAGE == "check" &&
    interactive() &&
    !nzchar(trimws(PROJECT_DIR))
  ) {
    cat(
      "\nEngine files were not both found in:\n",
      project,
      "\nSelect either original engine from the directory ",
      "containing both files.\n",
      sep = ""
    )
    
    selected <- normalizePath(
      file.choose(),
      winslash = "/",
      mustWork = TRUE
    )
    
    project <- dirname(selected)
    engines <- locate_engines(project)
  }
  
  must(
    !anyNA(engines),
    paste0(
      "Engine missing in: ", project,
      "\nPlace both original engines beside this launcher, ",
      "or set PROJECT_DIR explicitly.",
      "\nRequired files:",
      "\n  MESE_v7_engine_native_fix.R",
      "\n  MESE_EMPIRICAL_REVIEW_ENGINE_v2.R"
    )
  )
  
  expected_md5 <- c(
    core = "0d71c34bf9302516657ea3b078a8c5ad",
    empirical = "d73ca6558f38809f82580b98167fa964"
  )
  
  observed_md5 <- unname(
    tools::md5sum(unname(engines))
  )
  
  must(
    identical(observed_md5, unname(expected_md5)),
    paste(
      "An engine hash differs from the archived study version.",
      "Do not bypass this check or modify historical engines silently."
    )
  )
  
  cat("\n--- Resolved project directory ---\n")
  cat(project, "\n")
  
  cat("\n--- Verified engine files ---\n")
  
  print(
    data.frame(
      role = names(engines),
      file = basename(unname(engines)),
      md5 = observed_md5,
      row.names = NULL
    ),
    row.names = FALSE
  )
  
  invisible(
    parse(
      file = engines[["empirical"]],
      keep.source = FALSE
    )
  )
  
  e <- new.env(parent = globalenv())
  
  sys.source(
    engines[["empirical"]],
    envir = e,
    keep.source = FALSE
  )
  
  e$ep_self_tests()
  
  resolve_directory <- function(path, must_exist = FALSE) {
    path <- path.expand(path)
    
    if (!absolute_path(path)) {
      path <- file.path(project, path)
    }
    
    normalizePath(
      path,
      winslash = "/",
      mustWork = must_exist
    )
  }
  
  if (RUN_STAGE == "collect") {
    must(
      nzchar(trimws(COLLECT_FOLDER)),
      "Set COLLECT_FOLDER explicitly for collection."
    )
    
    folder <- resolve_directory(
      COLLECT_FOLDER,
      must_exist = TRUE
    )
    
    must(
      dir.exists(folder) &&
        file.exists(file.path(folder, "configuration.rds")),
      "Collection requires an existing result directory and configuration."
    )
    
    e$ep_collect(folder)
    
    cat(
      "\nCollection finished. No model was fitted.\n",
      "Derived summaries may have been rewritten.\n",
      sep = ""
    )
  } else {
    e$ep_load_core(engines[["core"]])
    
    cfg <- e$ep_config(
      dataset = DATASET,
      stage = if (RUN_STAGE == "check") "full" else RUN_STAGE,
      workers = as.integer(WORKERS),
      offsets = ORIGIN_OFFSETS,
      engine_file = engines[["empirical"]],
      core_file = engines[["core"]]
    )
    
    e$ep_core$ms_self_tests()
    
    data <- e$ep_data(cfg)
    cfg$data_md5 <- data$data_md5
    
    tasks <- e$ep_tasks(data, cfg)
    
    cfg$evaluated_series <- unique(
      tasks$manifest$series_id
    )
    
    cfg$task_manifest_md5 <- e$ep_md5(
      tasks$manifest
    )
    
    cfg$distribution_launcher <- "RUN_MESE_EMPIRICAL.R"
    
    identity <- cfg
    identity$workers <- NULL
    cfg$protocol_hash <- e$ep_md5(identity)
    
    cat("\n--- Planned empirical configuration ---\n")
    
    print(
      cfg[
        c(
          "engine", "dataset", "design", "stage",
          "H", "B", "S", "S_grid", "origin_offsets",
          "workers", "primary_metric", "software"
        )
      ]
    )
    
    cat(
      "\nDataset population:", nrow(data$manifest),
      "\nScheduled origin tasks:", nrow(tasks$manifest),
      "\nEligible tasks:",
      sum(tasks$manifest$eligible %in% TRUE),
      "\nData hash:", cfg$data_md5,
      "\n"
    )
    
    if (identical(cfg$origin_offsets, 0L)) {
      cat(
        "One official origin and the first 12 test observations;",
        "not rolling-origin.\n"
      )
    }
    
    if (RUN_STAGE == "check") {
      cat(
        "\nCHECK ONLY: self-tests, data loading and task planning.\n",
        "No fitting, bootstrap generation or future simulation.\n",
        "No result files were written.\n",
        sep = ""
      )
    } else {
      if (RUN_STAGE == "full") {
        must(
          isTRUE(CONFIRM_FULL_RUN),
          paste(
            "Full run blocked.",
            "Review pilot results and resources before setting",
            "CONFIRM_FULL_RUN = TRUE."
          )
        )
      }
      
      output_root <- resolve_directory(OUTPUT_ROOT)
      
      tag <- paste0(
        "offsets_",
        paste(cfg$origin_offsets, collapse = "_")
      )
      
      folder <- file.path(
        output_root,
        DATASET,
        paste0(
          cfg$stage, "_", tag,
          "_B", cfg$B, "_S", cfg$S
        )
      )
      
      cat(
        "\nStarting only the requested stage in:\n",
        folder,
        "\n",
        sep = ""
      )
      
      e$ep_run(
        cfg,
        data,
        tasks,
        folder,
        retry_fits = RETRY_FAILED_FITS,
        retry_intervals = RETRY_FAILED_INTERVALS
      )
      
      e$ep_collect(folder)
      
      cat(
        "\nStage finished. Inspect availability and diagnostics.\n",
        "No next stage starts automatically.\n",
        sep = ""
      )
    }
  }
})
