# Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing

Code and selected result summaries accompanying the manuscript *Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing: Model Selection, Parameter and Forecast Aggregation*, by Livio Fenga and Luca Biazzo.

## Scope

The repository contains archived computational engines, new distribution launchers, original audit and extraction scripts, 33 curated result tables, execution instructions and provenance records. Full execution directories, fitted-model checkpoints and source-data snapshots are not included. The package has not undergone an independent end-to-end rerun during assembly. Local check-mode results and their scope are documented separately.

## Quick start

Use the recorded environment: R 4.4.3, forecast 8.23.0, meboot 1.4.9.4 and, for the tourism data, Tcomp 1.0.1. Dependencies are not installed automatically. Keep the root-level R files together, leave RUN_STAGE = "check" and confirmation flags FALSE, and run from the repository root:

```r
source("RUN_MESE_v7.R")
source("RUN_MESE_EMPIRICAL.R")
```

Check mode does not fit models, generate bootstrap series or simulate futures. The empirical check loads data and plans tasks. The revised empirical launcher supports directory resolution and an interactive fallback in check mode; the simulation launcher expects the repository root as working directory. Full runs can be expensive and require explicit configuration.

- [Execution instructions](documentation/RUNNING.md)
- [Audit and extraction instructions](documentation/AUDITING.md)
- [User-reported local checks](provenance/CHECK_STATUS.md)
- [Result-summary documentation](reports/README.md)

## Code map

| File | Purpose |
| --- | --- |
| MESE_v7_engine_native_fix.R | Archived simulation core and shared fitting, aggregation and interval routines |
| MESE_EMPIRICAL_REVIEW_ENGINE_v2.R | Archived empirical workflow |
| RUN_MESE_v7.R | Check-first simulation distribution launcher |
| RUN_MESE_EMPIRICAL.R | Check-first empirical distribution launcher |
| AUDIT_MESE_v7_PER_REPLICA_FINAL.R | Simulation audit and paired analysis |
| FINAL_EMPIRICAL_DIAGNOSTICS.R | Empirical integrity, influence and sensitivity analysis |
| EXTRACT_MESE_v7_PAPER_RESULTS.R | Simulation table extraction from a compatible completed audit |
| COPY_MESE_EMPIRICAL_RESULTS.R | Export of collected empirical tables |

## Executed configurations

- Monte Carlo: six designs, 240 training observations, 12-step holdout, B = 100 and S = 2000 for residual intervals. Primary, bagged-benchmark and ablation schedules use 200, 100 and 50 replications per design. Consult the tables for actual supports and failures.
- Empirical: 366 monthly Tourism Forecasting Competition series, official split, first 12 test observations, one origin per series, B = 100 and S = 20000. This is not M3 or rolling-origin evaluation.

B counts fitted bootstrap members and S counts future paths. Native automatic ETS intervals retain their separate construction. A historical Monte Carlo approval note mentions S = 20000, whereas the executed validation records S = 2000. The executed configuration governs interpretation; historical metadata are preserved.

## Methods and limitations

MESE averages smoothing and damping coefficients within a common structure and re-estimates initial states on the observed series. MEB-FA combines member forecasts. Selection uses observed-series, bootstrap-averaged or replicate-specific AICc; replicate-specific selection applies only to forecast aggregation. Prediction intervals distinguish residual plug-in and mixture simulation. Native bagged ETS is a point-forecast benchmark only.

Controlled fitting uses a restricted candidate grid and a numerical protocol distinct from native automatic ETS. No uniform superiority, nominal coverage guarantee or certified global optimum is claimed. Failures, source flags and native optimizer codes remain visible. Complete-panel summaries and available-pair contrasts use explicitly different supports; diagnostic subsets do not replace primary analyses.

## Provenance and rerunning audits

Original engines and audit/extraction scripts were preserved byte for byte under the documented repository filenames. Distribution launchers are new entry points, not historical launcher copies. Checksums and file mappings are under provenance/. The current launcher manifest uses explicitly labelled Git blob identifiers.

Curated CSV contents were extracted from supplied reports without recomputation or rounding. Audit scripts requiring checkpoints cannot be run from the summary tables alone: obtain those inputs by rerunning the corresponding experiments or through a separately arranged checkpoint transfer. No checkpoint distribution is promised by this repository. New distribution runs default to replication_runs/, whereas archived audits default to historical folder names. Follow documentation/AUDITING.md to configure working copies for the intended completed run. Collection can rewrite derived summaries; do not run writers concurrently with integrity audits.

## Data

The empirical engine loads the monthly tourism data through Tcomp. The manuscript credits Athanasopoulos et al. (2011). A source-data extract is not included. Third-party datasets and package dependencies retain their own terms and are not relicensed by this repository.

## License and citation

The authors' original code and associated documentation are available under the [MIT License](LICENSE). Author-generated summary tables and provenance metadata are also made available under MIT to the extent the authors hold rights in them; this does not grant rights in underlying third-party data. See [license scope](LICENSE_SCOPE.md).

Please cite the associated manuscript when using these materials. This is an academic request, not an additional MIT license condition. CITATION.cff supplies author and repository metadata without asserting a publication DOI or published release.
