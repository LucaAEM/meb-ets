# Original engine provenance

The two root-level .R files are byte-identical copies of the supplied engines associated with the recorded runs. Their extensions have been capitalized for compatibility with the filenames used by the launchers; no file content, line ending, coefficient, default, or comment has been modified.

Recorded MD5 identifiers:

- MESE_v7_engine_native_fix.R: 0d71c34bf9302516657ea3b078a8c5ad
- MESE_EMPIRICAL_REVIEW_ENGINE_v2.R: d73ca6558f38809f82580b98167fa964

The manifest additionally records SHA-256 checksums and byte sizes. MD5 is retained for comparison with historical metadata, not as an authenticity guarantee.

## Scope of this deposit

This deposit contains the two engines and their provenance only. It does not contain launchers, checkpoints, data extracts, results, audit scripts, or a complete replication workflow. These engines have not been executed in R during packaging. Hash equality establishes correspondence with the recorded identifiers, not successful execution or scientific validity.

The empirical engine requires the simulation core through ep_load_core(). Native and controlled fitting procedures are intentionally distinct. Historical Monte Carlo validation defaults use S = 2000; empirical full-run configuration uses S = 20000. Do not edit historical source files to reconcile an approval note with an executed configuration.

Retain these originals unchanged. Future fixes should be separately committed and explicitly distinguished from the engines used for the reported runs.
