# Auditing and extracting existing results

## Archived scripts

The four scripts in this deposit preserve the supplied bytes, including original line endings and any byte-order mark. Filename extensions are standardized to .R. The supplied AUDIT_MESE_v7_PER_REPLICA.r is stored as AUDIT_MESE_v7_PER_REPLICA_FINAL.R, matching its internal header and the audit version required by the extraction script: v7_audit_separated_diagnostics_1. This is a filename change only, not a code modification.

The manifest records both supplied and repository filenames, sizes and checksums. These files were inspected and packaged without executing them in R. Previous result reports were supplied separately; packaging does not establish which exact script bytes historically produced each report.

## Inputs required

These scripts do not fit models or generate bootstrap samples or future paths. They are not all read-only in the filesystem sense: they create reports, tables and metadata in their configured output directories. Do not source them merely to check syntax. To check syntax without executing the script body, use:

```r
invisible(parse(file = "AUDIT_MESE_v7_PER_REPLICA_FINAL.R"))
invisible(parse(file = "FINAL_EMPIRICAL_DIAGNOSTICS.R"))
invisible(parse(file = "EXTRACT_MESE_v7_PAPER_RESULTS.R"))
invisible(parse(file = "COPY_MESE_EMPIRICAL_RESULTS.R"))
```

The curated tables under reports/ are not substitutes for the original execution directories. In particular, checkpoint scans require original *_fit.rds files. This deposit does not include the full execution directories or checkpoint archive.

## Default historical directory layout

The default scripts expect paths relative to the working directory:

```text
MESE_v7_validation/
    configuration.rds
    status.csv
    point_events.csv
    interval_events.csv
    ... collected outputs and *_fit.rds checkpoints
MESE_EMPIRICAL_REVIEW_v2/
    tourism_monthly/
        full_offsets_0_B100_S20000/
            configuration.rds
            task_manifest.csv
            ... collected outputs and *_fit.rds checkpoints
```

The distribution launchers instead default to new replication_runs/ directories. For new runs, use working copies of the audit/extraction scripts and point their folder variables to the intended completed results. Preserve archived script files unchanged when comparing manifest checksums. Audit/extraction scripts do not start fitting automatically when results are missing.

## Simulation workflow

1. Use a stable completed validation directory. Do not run collection or export writers concurrently with auditing.
2. In a working copy, set FOLDER and AUDIT_ROOT as needed, retaining SCAN_CHECKPOINTS = TRUE and AUDIT_INTERVALS = TRUE for the documented full audit.
3. Run AUDIT_MESE_v7_PER_REPLICA_FINAL.R. Review integrity_checks.csv and all availability and diagnostic summaries. The additional final block prints native codes for replication 1 only and is illustrative, not the full audit.
4. In a working copy of EXTRACT_MESE_v7_PAPER_RESULTS.R, set SOURCE_FOLDER and AUDIT_ROOT. Prefer an explicit AUDIT_FOLDER identifying the completed compatible audit. The extractor checks configuration, integrity flags and hashes before exporting.
5. Use the paired tables with their reported supports. Complete-panel summaries do not replace primary available-pair comparisons. Do not reinterpret missing BLD fits as wins for other methods.

Some extractor inputs, including multiway_wins_same_support.csv and largest_three_loss_differences.csv, are optional. The deposited audit need not produce them. The required primary contrasts, diagnostics and integrity tables are separate. Do not cite an optional table as available without checking that it was actually generated and deposited.

## Empirical workflow

1. COPY_MESE_EMPIRICAL_RESULTS.R reads the collected full empirical outputs and exports a text report; its trailing calls display sections in the console.
2. FINAL_EMPIRICAL_DIAGNOSTICS.R checks event scores and original mean contrasts, then exports influence, descriptive flag sensitivity, hypothetical grid perturbations and future-path sensitivity. Its default FULL_FOLDER points to the historical layout above.
3. Grid perturbations require the saved bootstrap criterion matrices in the affected fit checkpoints. The script may record not_completed for a grid scenario without invalidating every other diagnostic; inspect grid_sensitivity separately from the main integrity checks.
4. The comparison checks reproduce original mean differences and sample counts. Do not describe them as verification of every original p-value or confidence-interval endpoint unless separately checked.

## Interpretation

Neither audit repairs optimization or certifies global optima. Native codes, source flags and incomplete diagnostics are separate. Main comparisons retain valid jointly available results; flag subsets are descriptive sensitivity analyses. Mean-loss confidence intervals use t approximations; bootstrap coverage intervals in the collector concern a different quantity. Holm families must be described exactly as implemented. Endpoint sensitivity uses finite nested simulation prefixes, not exact quantiles. Stable grid choices under the specified perturbation do not establish unchanged forecasts under alternative fitted coefficients.

Before any public release, remove private data and credentials from local working directories; do not publish checkpoint or source-data archives without reviewing their contents and redistribution terms. The four code files themselves are deposited here without a new claim of full end-to-end replication.
