# Distribution launcher checks and provenance

## Evidence and scope

On 13 November 2026 the author supplied R console transcripts showing successful check-mode execution of the simulation distribution launcher and the revised empirical distribution launcher. These are user-reported local checks, not an independent execution by the assistant and not new fitting runs. The empirical transcript shows console evaluation with the interactive directory fallback. Direct source() and Rscript execution, full fitting, parallel worker execution, checkpoint resume and collection were not tested by that transcript.

No local launcher-file checksum was printed in the transcripts. The repository identifiers in distribution_launcher_manifest.csv identify the files inspected on GitHub; they do not independently authenticate the local transcript. The displayed revised empirical code corresponds to the launcher inspected in the repository.

## Reported successful checks

- Simulation: lightweight pooling, interval-score and scale-conversion self-tests; archived core hash check; configuration and backend-version checks. The printed timing configuration was inspected only, not executed.
- Empirical: both archived engine hash checks; module and core self-tests; data loading and task planning; 366 monthly Tourism series, 366 scheduled and eligible origin tasks; H = 12, B = 100, S = 20000, origin_offsets = 0.
- Reported environment: R 4.4.3, forecast 8.23.0, meboot 1.4.9.4 and Tcomp 1.0.1 for the empirical check.
- Empirical data hash: b1c3321b2e11faa51cff026cccfcb1de, matching the recorded study configuration.

The empirical check builds a configuration with stage = full but does not call ep_run() while RUN_STAGE = check. No model fitting, bootstrap generation, future-path simulation or result-file writing was performed in the reported checks. A restored workspace was visible in an earlier session; these checks are not claimed to be clean-session end-to-end replication.

## Revised empirical path handling

RUN_MESE_EMPIRICAL.R now resolves engines relative to PROJECT_DIR when supplied, otherwise from its source/Rscript location or the current working directory. An interactive file-selection fallback is allowed only in check mode. Relative output and collection paths are resolved against the selected project directory without changing getwd(). The simulation launcher still expects execution from the repository root. This note supplements documentation/RUNNING.md, whose original root-directory instructions remain applicable.

## Manifest format

The current distribution launcher manifest records Git blob SHA-1 identifiers returned by GitHub and byte sizes. These are Git object identifiers, not ordinary SHA-1 file checksums or SHA-256 checksums. The prior manifest remains in Git history; its SHA-256 for the earlier empirical launcher must not be applied to the revised file. Original engine MD5 and SHA-256 records remain separate in provenance/engine_manifest.csv and have not been changed.

## Remaining replication scope

Passing check mode does not establish successful full runs, predictive calibration, optimizer global optimality, checkpoint compatibility or manuscript-wide reproducibility. Historical result configurations and audits remain the evidence for the reported experiments. Historical engines and result tables are unchanged by this documentation update.
