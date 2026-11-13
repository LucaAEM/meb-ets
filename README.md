# Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing

Code and selected result summaries accompanying the manuscript *Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing: Model Selection, Parameter and Forecast Aggregation*, by Livio Fenga and Luca Biazzo.

## Status and scope

This repository contains the archived computational engines, new check-first distribution launchers, audit and extraction scripts, curated result tables, and provenance records. It is being prepared for resubmission. The authors have supplied historical result reports and local check-mode transcripts; the repository has not undergone an independent end-to-end rerun during assembly. Full execution directories, fitted-model checkpoints and source-data snapshots are not included.

## Start here

1. Download or clone the repository and extract it if necessary. Keep the root-level R files together.
2. Use the recorded software environment described below. The launchers do not install dependencies.
3. Leave RUN_STAGE = "check" and confirmation flags FALSE. From the repository root, run:

```r
source("RUN_MESE_v7.R")
source("RUN_MESE_EMPIRICAL.R")
```

The revised empirical launcher can also resolve its directory when sourced using an explicit path, or request a directory interactively in check mode. The simulation launcher expects the repository root as working directory. Check mode performs self-tests and configuration checks; the empirical check additionally loads data and plans tasks. It does not fit models or generate bootstrap series or future paths. Do not source the audit scripts simply to check syntax: they read existing execution outputs and write reports.

See [execution instructions](documentation/RUNNING.md), [audit instructions](documentation/AUDITING.md), and [reported check status](provenance/CHECK_STATUS.md). Full runs can be computationally expensive and require deliberate configuration.

## Repository contents

| File or directory | Purpose |
| --- | --- |
| MESE_v7_engine_native_fix.R | Archived simulation core and shared fitting, aggregation and interval routines |
| MESE_EMPIRICAL_REVIEW_ENGINE_v2.R | Archived empirical workflow using the shared core |
| RUN_MESE_v7.R | New check-first simulation distribution launcher |
| RUN_MESE_EMPIRICAL.R | New check-first empirical distribution launcher with directory resolution |
| AUDIT_MESE_v7_PER_REPLICA_FINAL.R | Audit and paired analysis of saved simulation outputs and checkpoints |
| FINAL_EMPIRICAL_DIAGNOSTICS.R | Empirical score checks, loss influence, flag sensitivity, criterion perturbations and future-path sensitivity |
| EXTRACT_MESE_v7_PAPER_RESULTS.R | Table extraction from simulation results and a compatible completed audit |
| COPY_MESE_EMPIRICAL_RESULTS.R | Export of collected empirical tables into a text report |
| reports/ | Curated simulation and empirical summary CSVs |
| provenance/ | Engine and script identifiers, extraction manifests, executed empirical configuration and check notes |
| documentation/ | Execution and audit requirements |

## Recorded environment and experimental configurations

The recorded runs used R 4.4.3, forecast 8.23.0 and meboot 1.4.9.4. The empirical dataset was loaded through Tcomp 1.0.1. The core checks forecast and meboot versions. Preserve session information and review differences before using another environment.

- **Monte Carlo:** six designs, 240 training observations and a 12-step holdout. The primary, bagged-benchmark and ablation schedules contain 200, 100 and 50 replications per design, respectively. MEB fitting uses B = 100; residual intervals use S = 2000 future paths. Completed supports and failures are reported explicitly.
- **Empirical:** 366 monthly Tourism Forecasting Competition series; official training/test split; first 12 test observations; one origin per series; B = 100 and S = 20000. This is neither the M3 competition nor a rolling-origin evaluation.

B counts fitted bootstrap members; S counts simulated future paths. Native automatic ETS intervals use their separate construction. A historical simulation approval note mentions S = 20000, but the executed validation configuration records S = 2000. The actual run configuration takes precedence; historical metadata are not silently rewritten.

## Methods and interpretation

Parameter aggregation (MESE) averages smoothing and damping coefficients within a common structure, then re-estimates initial states on the observed series. Forecast aggregation (MEB-FA) retains member-specific fits and combines forecasts. Selection uses observed-series, bootstrap-averaged or replicate-specific AICc; the last applies to forecast aggregation. Residual plug-in and mixture intervals are distinct. Native bagged ETS is a point-forecast benchmark only.

The controlled ETS grid and numerical fitting protocol differ from native automatic ETS. Comparisons assess the specified procedures, not a guaranteed isolated variance-reduction effect. Numerical flags, native codes, failures and unfavorable outcomes remain visible. Primary paired results and complete-panel summaries have different support definitions. Diagnostic subsets do not replace the primary analyses. No uniform forecasting superiority, nominal coverage guarantee or certified global optimum is claimed.

## Results and provenance

The 33 curated result tables were extracted from supplied reports without recomputing or rounding their values. See reports/README.md and provenance/summary_extraction_manifest.csv. Original engine and audit/extraction file checksums are recorded separately. The current launcher manifest uses Git blob identifiers and labels the hash algorithm explicitly. Distribution launchers are not historical launcher copies.

Local check-mode success is documented in provenance/CHECK_STATUS.md. It does not constitute a new full simulation or empirical validation. Audit scripts that inspect checkpoints require the original checkpoint files or files generated by rerunning the corresponding experiments; the summary CSVs alone do not provide those inputs. The new launchers write under replication_runs/, whereas archived audits default to historical folder names. Use working copies with explicit folder settings as described in documentation/AUDITING.md.

## Data

The empirical engine loads the monthly tourism dataset through Tcomp. A source-data extract and full data snapshot are not deposited here. Dataset origin is documented in the manuscript through Athanasopoulos et al. (2011). Review source licensing and redistribution terms before distributing data or checkpoint archives.

## Citation and licensing

Please cite the associated manuscript when using these materials. Author and repository metadata are provided in CITATION.cff; no publication DOI or release version is asserted. A code license has not yet been selected. This README does not grant an open-source license or relicense third-party data or dependencies.
