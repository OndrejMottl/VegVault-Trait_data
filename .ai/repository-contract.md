# VegVault-Trait_data Repository Contract

## Role and outputs

This repository owns acquisition, cleaning, harmonisation, review, and export of TRY and BIEN plant-trait data. VegVault consumes the reviewed TRY product and the partitioned BIEN trait products under `Outputs/Data/`.

Treat filenames and partitions, taxon identifiers, trait identifiers, value and unit fields, provenance, measurement metadata, and missing-value conventions as release interfaces.

## Safety

- TRY inputs are private or licence-controlled, and BIEN inputs may have redistribution constraints. Never expose credentials, requests, contributor-level records, ignored extracts, or data beyond their licence.
- Do not add ignored TRY/BIEN inputs or large processed products to Git.
- Broad downloads, raw-data refreshes, taxonomic or unit harmonisation reruns, repartitioning, and output replacement require explicit user authorization.
- Preserve attribution, dataset and observation provenance, measurement units, and existing manual review decisions.
- Use small representative trait/taxon subsets for validation and keep debugging products in ignored temporary locations.

## Trait export under the unit contract

- `R/01_Data_download/01_TRY.R` and `R/02_Process_data/01_BIEN_process.R` take no arguments for a release run: they read `Data/Input/` and write to `Outputs/Data/`, `Outputs/Tables/` and `Data/Temp/trait_unit_audit/`. For validation, pass an input (the TRY file or the BIEN folder) and an output folder; the output folder receives `Data/`, `Tables/` and `Audit/`. Keep validation runs inside `Data/Temp/`.
- Every raw trait record gets one disposition:
  - `published`: the unit contract converted and verified the value.
  - `quarantined`: the value or unit could not be verified.
  - `dropped`: the record was removed by a rule. The reasons are a configured DataID exclusion, a missing value, another value of the same trait in the same TRY observation (`resolve_repeated_try_values()`), a TRY record flagged as a copy of a published original (`resolve_try_duplicate_records()`), or a BIEN record without a species name.
- `validate_trait_unit_reconciliation()` aborts unless raw records equal published plus quarantined plus dropped, per provider and trait.
- Unit rules live in `config/trait_units/`. Add an alias or conversion only with a stated source. TRY conversions are verified against TRY's standardised value (`StdValue`), and dataset-specific overrides cover the known source errors.
- Artefacts are written by `save_trait_artifact()` to exact paths named `<stem>_<date>__<hash>__.qs` (`qs2` format). TRY is one file (`data_trait_try`). BIEN is four parts (`data_traits_bien_<0-3>`), checked by `validate_trait_artifact_parts()`. Never select artefacts by folder listing or modification time.
- The manifest (`trait_artifact_manifest_<provider>`) lists the exact inputs, the registry files and the outputs, with SHA-256 checksums and row counts.
- Published artefacts carry the unit-contract columns (`source_*`, `original_*`, `conversion_*`, `derivation_rule_key`, `trait_value`, `trait_unit`, `disposition`) plus the provider columns. TRY adds `obs_data_id`, `value_kind_name`, `orig_obs_data_id` and numeric WGS84 `latitude` and `longitude`. The coordinates take TRY's standardised value first, and `latitude_source` and `longitude_source` record where each came from.
- The audit summary, the reconciliation and the manifest in `Outputs/Tables/` are aggregate tables. Quarantined and dropped records are record-level licensed data: they are kept only in `Data/Temp/trait_unit_audit/trait_unit_unpublished_<provider>.qs2`, which is git-ignored, and are never committed.

## Change and validation contract

For changes to output shape or meaning, trace consumers in `../VegVault/R/02_Main_analyses/04_Import_try_data.R` and `../VegVault/R/02_Main_analyses/05_Import_bien_trait_data.R`, coordinate producer documentation and a reviewed tag, and update the pinned integration reference. Validate both the TRY contract and every affected BIEN partition before proposing a release.
