# Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing

Replication project for the manuscript **Maximum-Entropy Bootstrap Ensembles for Exponential Smoothing: Model Selection, Parameter and Forecast Aggregation**, by Livio Fenga and Luca Biazzo.

## Repository status

This repository is being assembled for manuscript resubmission. The initial documentation does not constitute a complete replication release. Code, selected outputs and provenance records will be added and checked before public release. No claim of reproducing every manuscript number is made at this stage.

## Scientific scope

The study distinguishes parameter aggregation (MESE) from forecast aggregation (MEB-FA), and compares observed-series, bootstrap-averaged and replicate-specific AICc selection. Parameter aggregation averages smoothing and damping coefficients and re-estimates initial states on the observed series. Forecast aggregation retains member-specific fits. Prediction intervals use residual plug-in or residual mixture simulation; native automatic ETS retains its own interval construction. Native bagged ETS is evaluated only for point forecasts.

## Executed study configurations

- Monte Carlo: six designs, 240 training observations, a 12-step holdout, B = 100 fitted bootstrap members and S = 2000 simulated future paths for residual intervals. Primary, bagged-benchmark and ablation schedules use 200, 100 and 50 replications per design, respectively; completed counts and failures must be read from the result tables.
- Empirical application: 366 monthly Tourism Forecasting Competition series, the official training/test split, the first 12 test observations, one origin per series, B = 100 and S = 20000. This is not the M3 competition or a rolling-origin evaluation.
- Recorded software: R 4.4.3, forecast 8.23.0, meboot 1.4.9.4 and Tcomp 1.0.1 for the empirical data.

B counts fitted bootstrap members; S counts future paths. Native ETS intervals use a separate construction. A historical Monte Carlo approval note mentions S = 20000, but the executed validation configuration records S = 2000. Historical metadata will not be silently rewritten.

## Replication principles

Original engine files and their recorded hashes will be preserved. Safe distribution launchers will be distinguished from historical run settings. Fitting, auditing and extraction are separate operations: auditing saved outputs does not repair optimization or regenerate forecasts. Audit scripts that read fit checkpoints require those checkpoints, whether distributed separately or recreated by running the experiments. Text reports alone are not sufficient inputs for every audit.

Numerical flags, native optimizer codes, failed fits and missing results must remain visible. Primary comparisons use valid jointly available outcomes; diagnostic subsets are descriptive sensitivities rather than automatic exclusion rules. Parameter aggregation and state re-estimation are evaluated jointly. No uniform forecasting superiority, nominal coverage guarantee or global optimization guarantee is claimed.

## Data and licensing

The empirical engine loads the monthly tourism data through Tcomp. Data redistribution terms must be checked before including an extract. No data extract or third-party license is asserted to be included by this initial commit. A code license will be selected with the authors before public release.

## Computational safety

Full fitting runs can be expensive. Review configurations before execution. Creating or reading this README does not launch any fitting or simulation.
