# Distribution launcher for the archived empirical engine.
# NEW replication entry point; not the historical launcher.
# Run from the repository root. No installation is performed.
RUN_STAGE <- "check" # check | pilot | full | collect
DATASET <- "tourism_monthly"
WORKERS <- 6L
ORIGIN_OFFSETS <- 0L
CORE_FILE <- "MESE_v7_engine_native_fix.R"
EMPIRICAL_ENGINE <- "MESE_EMPIRICAL_REVIEW_ENGINE_v2.R"
OUTPUT_ROOT <- "replication_runs/empirical"
CONFIRM_FULL_RUN <- FALSE
RETRY_FAILED_FITS <- FALSE
RETRY_FAILED_INTERVALS <- FALSE
COLLECT_FOLDER <- ""

local({
  must <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
  must(length(RUN_STAGE) == 1L && RUN_STAGE %in% c("check", "pilot", "full", "collect"), "Invalid RUN_STAGE")
  must(length(WORKERS) == 1L && is.finite(WORKERS) &&
    WORKERS >= 1 && WORKERS == as.integer(WORKERS), "WORKERS must be a positive integer")
  must(file.exists(CORE_FILE) && file.exists(EMPIRICAL_ENGINE), "Run from the repository root; engine missing")
  must(identical(unname(tools::md5sum(CORE_FILE)), "0d71c34bf9302516657ea3b078a8c5ad"), "Core engine hash mismatch")
  must(identical(unname(tools::md5sum(EMPIRICAL_ENGINE)), "d73ca6558f38809f82580b98167fa964"), "Empirical engine hash mismatch")
  parse(file = EMPIRICAL_ENGINE, keep.source = FALSE)
  e <- new.env(parent = globalenv())
  sys.source(EMPIRICAL_ENGINE, envir = e, keep.source = FALSE)
  e$ep_self_tests()
  if (RUN_STAGE == "collect") {
    must(length(COLLECT_FOLDER) == 1L && nzchar(COLLECT_FOLDER) &&
      dir.exists(COLLECT_FOLDER), "Set COLLECT_FOLDER to an existing completed empirical result directory")
    e$ep_collect(COLLECT_FOLDER)
    cat("Collection finished; no models were fitted. Derived summaries may have been rewritten.\n")
  } else {
    e$ep_load_core(CORE_FILE)
    cfg <- e$ep_config(DATASET, if (RUN_STAGE == "check") "full" else RUN_STAGE,
      as.integer(WORKERS), ORIGIN_OFFSETS, EMPIRICAL_ENGINE, CORE_FILE)
    e$ep_core$ms_self_tests()
    data <- e$ep_data(cfg)
    cfg$data_md5 <- data$data_md5
    tasks <- e$ep_tasks(data, cfg)
    cfg$evaluated_series <- unique(tasks$manifest$series_id)
    cfg$task_manifest_md5 <- e$ep_md5(tasks$manifest)
    cfg$distribution_launcher <- "RUN_MESE_EMPIRICAL.R"
    identity <- cfg
    identity$workers <- NULL
    cfg$protocol_hash <- e$ep_md5(identity)
    print(cfg[c("engine", "dataset", "design", "stage", "H", "B", "S",
      "S_grid", "origin_offsets", "workers", "primary_metric", "software")])
    cat("Dataset population:", nrow(data$manifest), "; scheduled origins:", nrow(tasks$manifest), "\n")
    if (identical(cfg$origin_offsets, 0L))
      cat("One official origin, first 12 test observations; not rolling-origin.\n")
    if (RUN_STAGE == "check") {
      cat("CHECK ONLY: self-tests, data loading and task planning; no fitting or simulation.\n")
    } else {
      if (RUN_STAGE == "full") must(isTRUE(CONFIRM_FULL_RUN),
        "Full run blocked. Review pilot results and resources before setting CONFIRM_FULL_RUN = TRUE.")
      tag <- paste0("offsets_", paste(cfg$origin_offsets, collapse = "_"))
      folder <- file.path(OUTPUT_ROOT, DATASET,
        paste0(cfg$stage, "_", tag, "_B", cfg$B, "_S", cfg$S))
      cat("Starting only the requested stage in:", folder, "\n")
      e$ep_run(cfg, data, tasks, folder,
        retry_fits = RETRY_FAILED_FITS, retry_intervals = RETRY_FAILED_INTERVALS)
      e$ep_collect(folder)
      cat("Stage finished. Inspect availability and diagnostics; no next stage starts automatically.\n")
    }
  }
})
