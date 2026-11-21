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

- The export is a `{targets}` pipeline in three stages (acquisition, processing, export), run by `R/Analyses/00_run_main.R`; `.ai/analysis-structure.md` describes the stages, stores and public targets. The BIEN duplicate check reads the published TRY records, so the TRY chain of the processing stage comes first.
  - Release run: the `default` profile reads `Data/Input/` and writes to `Outputs/Data/`, `Outputs/Tables/` and `Data/Temp/trait_unit_audit/`.
  - Test run: the `dev` profile (`R_CONFIG_ACTIVE=dev`) writes to the git-ignored `Outputs/Data_temp/`, with the tables in `Tables/` and the audit files in `Audit/`. Use it for every run that is not a release.
- Every raw trait record gets one disposition:
  - `published`: the unit contract converted and verified the value.
  - `quarantined`: the value or unit could not be verified, or the source is listed in `config/trait_quarantine/source_quarantine.csv` because its values are on an impossible scale (`resolve_source_quarantines()`). That list is reviewed with the scale diagnostics of the audit tables; each entry states its evidence.
  - `dropped`: the record was removed by a rule. The reasons are a configured DataID exclusion, a missing value, another value of the same trait in the same TRY observation (`resolve_repeated_try_values()`), a TRY record flagged as a copy of a published original (`resolve_try_duplicate_records()`), a BIEN record without a species name, or a BIEN record duplicated in TRY (`resolve_bien_try_duplicates()`).
- When TRY and BIEN hold the same measurement, the TRY copy is kept. A BIEN record is a duplicate only when its source is listed in `config/trait_duplicates/bien_sources_shared_with_try.csv` and the published TRY artifact has the same trait domain, species and value at three significant digits. The list holds the BIEN sources that copy compilations held by TRY. It was selected from the data by the rule in its `selection_basis` column. Every BIEN run recomputes the rule (`summarise_bien_try_twins()`), writes `trait_bien_try_twins_by_source` to the tables, and warns when a source qualifies but is not listed, or the reverse (`diagnose_bien_shared_sources()`). Review the list before a release whenever that warning appears.
- `validate_trait_unit_reconciliation()` aborts unless raw records equal published plus quarantined plus dropped, per provider and trait.
- The export stage checks the saved artefacts against the VegVault contracts `try_traits` and `bien_traits` with `validate_producer_artefact()` (`validate_trait_export()`; see "VegVault contract copy" below). The manifests and tables depend on the checked artefacts, so a failing check stops the run before any of them is written. The check targets read the saved files back without the published records in memory.
- Unit rules live in `config/trait_units/`. Add an alias or conversion only with a stated source. TRY conversions are verified against TRY's standardised value (`StdValue`), and dataset-specific overrides cover the known source errors.
- Artefacts are written by `save_trait_artifact()` to exact paths named `<stem>_<date>__<hash>__.qs` (`qs2` format). TRY is one file (`data_trait_try`). BIEN is four parts (`data_traits_bien_<0-3>`), checked by `validate_trait_artifact_parts()`. The published BIEN records are sorted by record ID before the split, so the parts do not depend on the row order of a BIEN download. Never select artefacts by folder listing or modification time.
- The manifest (`trait_artifact_manifest_<provider>`) lists the exact inputs, the registry files and the outputs, with SHA-256 checksums and row counts. Paths are relative to the repository root; `build_trait_artifact_manifest()` refuses a file outside it, so no machine path reaches a committed table.
- Published artefacts carry the unit-contract columns (`source_*`, `original_*`, `conversion_*`, `derivation_rule_key`, `trait_value`, `trait_unit`, `disposition`) plus the provider columns. TRY adds `obs_data_id`, `value_kind_name`, `orig_obs_data_id`, `record_reference`, `reference_source`, `dataset_reference_citation` and numeric WGS84 `latitude` and `longitude`. Other TRY ancillary variables (altitude, climate, site descriptions, ...) are not exported: holding all of them took more memory than the build machine has. BIEN carries its own `latitude` and `longitude`.
- Coordinates: TRY takes TRY's standardised value first (`prepare_try_coordinates()`); BIEN keeps the value BIEN gives (`prepare_bien_coordinates()`). In both providers, a coordinate outside -90 to 90 (latitude) or -180 to 180 (longitude), or one without its pair, is set missing, and the record stays published. `latitude_source` and `longitude_source` record where each coordinate came from or why it is missing.
- References:
  - TRY `record_reference` is TRY's `Reference` column: the citation of the publication behind each record, given for every record and mostly one per dataset. It is `NA` when TRY gives none.
  - `reference_source` ("Reference / source") and `dataset_reference_citation` ("Dataset reference (citation)") are ancillary variables of single observations, given for some datasets only.
  - BIEN references (`url_source`, `source_citation`, `project_pi`) are given once per source, not per measurement. BIEN delivers part of this text, and of `method`, mis-encoded; `normalise_text_encoding()` repairs it when the data are read, before any source key is built, so `config/trait_duplicates/` and `config/trait_quarantine/` hold repaired text.
  - Contributor names and contact details are never exported: TRY `LastName` and `FirstName`, the TRY contributor and author ancillary variables, and BIEN `project_pi_contact`.
- The audit summary, the reconciliation, the reference completeness (`trait_reference_completeness_<provider>`, from `summarise_trait_reference_completeness()`), the coordinate summary (`trait_coordinate_summary_<provider>`, from `summarise_trait_coordinates()`) and the manifest in `Outputs/Tables/` are aggregate tables. Quarantined and dropped records are record-level licensed data: they are kept only in `Data/Temp/trait_unit_audit/trait_unit_unpublished_<provider>.qs2`, which is git-ignored, and are never committed.

## VegVault contract copy

- `R/Functions/Contracts/` and `Data/Contracts/` hold a copy of VegVault's contract validator and of this repository's contracts (`try_traits.yml`, `bien_traits.yml`), taken from one VegVault commit by VegVault's `save_contract_copy()`. `Data/Contracts/contract_copy.yml` records that commit, the version of each contract and the fingerprint of every copied file.
- Never edit the copied files or add other files to these two folders. To change a contract, ask the VegVault pipeline lane (VegVault issue 92); to update the copy, make it again.
- The copied functions are tested in VegVault. This is the one exception to the one-test-file-per-function rule: the only local test is `R/Tests/testthat/test-validate_contract_copy.R`. It checks that the copy is unchanged and, when a sibling `../VegVault` checkout exists, that it equals what that checkout would copy now. A failure against a sibling checkout on another branch does not mean the copy is outdated.
- Updating the copy, after VegVault announces a new contract version in its release umbrella issue (VegVault issue 86): in a clean VegVault checkout on the release branch, up to date with GitHub, follow "Making or updating the copy" in VegVault's `Data/Contracts/README.md` with `path_producer = "../VegVault-Trait_data"` and `repository = "OndrejMottl/VegVault-Trait_data"`. Then run the tests, validate an export, and commit the copy and the record together.
- A release states `vegvault_commit` and the contract versions of `contract_copy.yml` in its release notes.

## Change and validation contract

For changes to output shape or meaning, trace consumers in `../VegVault/R/02_Main_analyses/04_Import_try_data.R` and `../VegVault/R/02_Main_analyses/05_Import_bien_trait_data.R`, coordinate producer documentation and a reviewed tag, and update the pinned integration reference. Validate both the TRY contract and every affected BIEN partition before proposing a release.
