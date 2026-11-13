# Distribution launcher for the archived MESE v7 simulation engine.
# NEW replication entry point; not the historical launcher.
# Run from the repository root. No installation is performed.
RUN_STAGE <- "check" # check | timing | precision | validation | collect
ENGINE_FILE <- "MESE_v7_engine_native_fix.R"
OUTPUT_ROOT <- "replication_runs/simulation"
COLLECT_FOLDER <- ""
WORKERS <- 2L
RETRY_FAILED <- FALSE
CONFIRM_VALIDATION <- FALSE
PROTOCOL_FROZEN <- FALSE
PRECISION_CHECKED <- FALSE
VALIDATION_APPROVAL_NOTE <- ""

local({
  must <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
  must(length(RUN_STAGE) == 1L && RUN_STAGE %in%
    c("check", "timing", "precision", "validation", "collect"), "Invalid RUN_STAGE")
  must(length(WORKERS) == 1L && is.finite(WORKERS) &&
    WORKERS >= 1 && WORKERS == as.integer(WORKERS), "WORKERS must be a positive integer")
  must(file.exists(ENGINE_FILE), "Run from the repository root; simulation engine not found")
  must(identical(unname(tools::md5sum(ENGINE_FILE)),
    "0d71c34bf9302516657ea3b078a8c5ad"),
    "Engine hash differs from the archived study engine. Review explicitly before proceeding.")
  for (p in c("forecast", "meboot"))
    must(requireNamespace(p, quietly = TRUE), paste("Install the recorded dependency explicitly:", p))
  parse(file = ENGINE_FILE, keep.source = FALSE)
  eng <- new.env(parent = globalenv())
  sys.source(ENGINE_FILE, envir = eng, keep.source = FALSE)
  eng$ms_self_tests()
  if (RUN_STAGE == "collect") {
    must(length(COLLECT_FOLDER) == 1L && nzchar(COLLECT_FOLDER) &&
      dir.exists(COLLECT_FOLDER), "Set COLLECT_FOLDER to an existing completed result directory")
    must(file.exists(file.path(COLLECT_FOLDER, "configuration.rds")), "Missing saved configuration")
    result <- eng$ms_collect(COLLECT_FOLDER)
    saveRDS(result, file.path(COLLECT_FOLDER, "distribution_collected_result.rds"))
    cat("Collection finished; no models were fitted. Derived summaries may have been rewritten.\n")
  } else {
    mode <- if (RUN_STAGE == "check") "timing" else RUN_STAGE
    cfg <- eng$ms_config(mode, workers = as.integer(WORKERS))
    eng$ms_backend_check(cfg)
    must(identical(cfg$engine, "MESE_v7_frozen_original_selection_1"), "Unexpected engine identity")
    print(cfg[c("engine", "mode", "n", "H", "m", "cases", "R", "R_bag",
      "R_ablation", "B", "S", "B_grid", "S_grid", "workers", "software")])
    cat("Configured counts are planned counts, not completed outcomes.\n")
    if (RUN_STAGE == "check") {
      cat("CHECK ONLY: lightweight self-tests and configuration checks; no fitting or simulation.\n")
    } else {
      if (RUN_STAGE == "validation") {
        must(isTRUE(CONFIRM_VALIDATION) && isTRUE(PROTOCOL_FROZEN) &&
          isTRUE(PRECISION_CHECKED) && length(VALIDATION_APPROVAL_NOTE) == 1L &&
          !is.na(VALIDATION_APPROVAL_NOTE) && nzchar(trimws(VALIDATION_APPROVAL_NOTE)),
          paste("Validation blocked. Review the protocol and precision evidence, then provide",
            "explicit confirmations and an accurate note. Do not set flags merely to bypass this gate."))
        cfg$protocol_frozen <- TRUE
        cfg$precision_checked <- TRUE
        cfg$validation_approval_note <- VALIDATION_APPROVAL_NOTE
      }
      cfg$launcher_engine_file_md5 <- unname(tools::md5sum(ENGINE_FILE))
      cfg$distribution_launcher <- "RUN_MESE_v7.R"
      destination <- file.path(OUTPUT_ROOT, paste0("MESE_v7_", RUN_STAGE))
      cat("Starting only the requested stage in:", destination, "\n")
      eng$ms_run(cfg, destination,
        confirm_validation = RUN_STAGE == "validation" && isTRUE(CONFIRM_VALIDATION),
        retry_failed = RETRY_FAILED)
      result <- eng$ms_collect(destination)
      saveRDS(result, file.path(destination, "distribution_collected_result.rds"))
      cat("Stage finished. Inspect statuses and numerical issues; no next stage starts automatically.\n")
    }
  }
})
