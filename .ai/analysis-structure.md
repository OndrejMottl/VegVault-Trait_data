# Analysis structure

The project layout is a contract. It follows `OndrejMottl/Project_template`. Change it deliberately, update this file in the same change, and check every caller.

## Layout

```text
config.yml                 project configuration, read via load_config_value()
secrets.yaml               git-ignored machine-local values (see secrets_example.yaml)
config/
  trait_selection/         selected TRY and BIEN traits, excluded TRY DataIDs
  trait_units/             unit aliases, conversions and derivations
  trait_quarantine/        reviewed source quarantines
  trait_duplicates/        BIEN sources shared with TRY
Data/
  Contracts/               copy of the VegVault producer contracts (see below)
  Input/                   raw inputs, under the data root (not modified by code)
    TRY/Raw/28498.txt      TRY data request, manual download, git-ignored
    BIEN/<trait>_<date>__<hash>__.qs   one or more per BIEN trait, git-ignored
  Temp/                    disposable scratch, debug scripts and the audit folder (git-ignored)
  Targets_data/            one {targets} store per pipeline (git-ignored)
Outputs/
  Data/                    release artefacts read by VegVault (tracked)
  Tables/                  aggregate tables and manifests of a release (tracked)
  Data_temp/               test-run exports of the `dev` profile (git-ignored)
R/
  ___Init_project___.R     one-time environment restore or initialisation
  ___setup_project___.R    shared setup sourced by every script and pipeline
  package_dependencies.R   generated; do not edit
  Functions/Traits/        trait functions, one function per file, named after the function
  Functions/Workflow/      template workflow helpers
  Functions/Contracts/     copied from VegVault; never edit here
  Analyses/
    00_run_main.R          runs the stages in order (01 to 03)
    01_Data_acquisition/   TRY request check and read, trait selection tables, BIEN raw files
    02_Data_processing/    unit contract, dispositions, reconciliation, published records, aggregate tables
    03_Data_export/        qs2 artefacts, contract checks, manifests, tables, audit files
  Tests/
    Run_tests.R            full suite runner
    testthat/test-<function_name>.R
```

## Data root

- `resolve_data_storage_path()` gives the data root: the `Data:` entry of `secrets.yaml` if present, otherwise `data_storage_path` in `config.yml` (`Data` in the project).
- Raw inputs live under `<data root>/Input/` at the paths named in `config.yml` (`try: raw_file`, `bien: raw_dir`). The stores live under `<data root>/Targets_data/`.
- The manifests record paths relative to the project and refuse a file outside it (`build_trait_artifact_manifest()`), so an export needs a data root inside the project.
- A second checkout or worktree points its `Data:` entry at the data root that holds the raw inputs. Never copy raw-data trees between checkouts.

## Stages and pipelines

- Each stage is a numbered folder under `R/Analyses/` with a `00_run.R` that sources the setup and calls `run_target_pipeline()`, and a `pipeline.R` that only declares targets. `00_run_main.R` runs stages 01 to 03.
- Each pipeline owns one store, resolved with `resolve_pipeline_store_path()`: `data_acquisition`, `data_processing` and `data_export`. Never share a store between pipelines.
- Every pipeline stores its targets with `format = "qs"` (`qs2` in `{targets}` 1.12), `memory = "transient"` and garbage collection after each target, because the TRY records and the about 10 million BIEN records take several gigabytes in memory.
- Target names are quoted strings. Above each target, a short plain comment says what it provides and why it is in the graph.
- When a target command is more than one function call, move the logic into a function in `R/Functions/`. The upstream-import target is the one exception.

## Raw inputs and downloads

- The TRY data request is a manual download from the TRY database. `path_try_raw_file` is a `format = "file"` target that checks the file against `try: raw_file_sha256`; a missing or different file stops the run. `load_try_records()` reads the file once, with only `try: columns_selected`, and keeps the records of the selected traits and their coordinate and reference records (`try: ancillary`).
- BIEN is downloaded only when `bien: flag_download` is `true`. Switching the flag on reruns the branches below, which download the selected traits again; switching it back off reruns them once more, and they then choose the new files, which carry the latest date. `path_bien_raw_file` branches over the traits of `config/trait_selection/bien_traits.csv`; each branch is a `format = "file"` target that takes the file with the latest date in its name (`resolve_bien_trait_raw_file()`), never the modification time, and stops when two files share that date. A file placed in `Data/Input/BIEN/` by hand is used only after `path_bien_raw_file` is invalidated. Keep the flag `false` in committed configuration.
- The rows of `bien_traits.csv` are in the order in which the old script read the files (`list.files()` order, so `leaf area per leaf dry mass` comes before `leaf area`); the raw records are combined in this order.

## Rule tables

- `config/trait_selection/try_traits.csv` and `bien_traits.csv` map each selected trait to its trait domain (Díaz et al. 2016), its full name and the derivation applied after the unit contract (`sla_to_lma` for specific leaf area). The trait domain `Plant heigh` is spelled as VegVault reads it.
- `config/trait_selection/try_data_id_exclusions.csv` lists the excluded TRY DataIDs with TRY's name and trait of each: early-growth heights and the minimum and maximum bounds of ranges. The old script recorded no reason for them.
- Every rule table is read with `load_trait_rule_table()`, as character, and tracked as a `format = "file"` target, so an edit reruns what depends on it.

## Cross-pipeline contracts

- A downstream pipeline imports only the public targets of an upstream store, listed in its "Upstream contract" section with the upstream runner.
- Import pattern: a `fingerprint_<stage>` target calls `compute_target_store_fingerprint()` with `cue = targets::tar_cue(mode = "always")`, and each import target references the fingerprint and calls `load_target_store_value()`.
- A missing upstream store, or one whose latest run failed, is an error that names the runner to execute first (`validate_target_store_contract()`). A reconciliation that does not balance therefore stops the export as well.
- Public targets:
  - acquisition: `path_try_raw_file`, `data_try_trait_selection`, `data_try_data_id_exclusions`, `data_try_trait_rows`, `data_try_ancillary`, `data_try_data_ids`, `data_bien_trait_selection`, `path_bien_raw_file`, `data_bien_raw`;
  - processing: `data_try_resolved`, `data_try_published`, `list_try_tables`, `data_bien_resolved`, `data_bien_published`, `list_bien_tables`;
  - export: `path_try_export`, `path_try_tables`, `path_try_unpublished`, `path_bien_export`, `path_bien_tables`, `path_bien_unpublished`.
- Renaming or removing a public target is a contract change: search the downstream pipelines first.

## Export and producer contracts

- The export stage writes `data_trait_try_<YYYY-MM-DD>__<hash>__.qs` and `data_traits_bien_<part>_<YYYY-MM-DD>__<hash>__.qs` (parts from 0, `export: n_bien_parts`) to `export: dir_data` (`qs2` content, `<hash>` is `rlang::hash()` of the object). An existing file of the same name holds the same object and is not rewritten; earlier versions are never removed.
- The published BIEN records are sorted by record ID before the split (`select_bien_published_records()`), so the parts do not depend on the row order of a download.
- Each artefact is checked with `validate_producer_artefact()` against its contract in `Data/Contracts/` (`validate_trait_export()`, all BIEN parts together); a violation stops the run. The check targets read the saved files, so the published records are not in memory during the check.
- The manifests and the aggregate tables (`export: dir_tables`) depend on the checked artefacts, so they are written only after the check passes. `save_trait_tables()` writes each table as `<name>_<YYYY-MM-DD>.csv` with `RUtilpol::save_latest_file()`.
- The quarantined and dropped records are written to `export: dir_audit` (`trait_unit_unpublished_<provider>.qs2`). They are licensed record-level data; the folder must stay git-ignored.
- `Data/Contracts/` (`try_traits.yml`, `bien_traits.yml`, `contract_copy.yml`) and `R/Functions/Contracts/` are a copy made by VegVault's `save_contract_copy()` from one VegVault commit, recorded in `contract_copy.yml`. Never edit them here (`.ai/repository-contract.md`).

## Configuration

- Analysis values (seed, columns, ancillary variables, rule-table paths, flags, export folders, package lists) live in `config.yml` and are read only through `load_config_value()`. Do not call `config::get()` directly.
- The `dev` profile (`R_CONFIG_ACTIVE=dev`) writes the artefacts to `Outputs/Data_temp/`, the tables to `Outputs/Data_temp/Tables/` and the audit files to `Outputs/Data_temp/Audit/`, so a test run never touches the tracked outputs. Use it for test runs, such as the comparison with the old scripts. The acquisition and processing stores are shared by both profiles; only the export targets rerun when the profile changes.

## Setup and packages

- Every stage runner, pipeline and test run starts with `library(here)` and `source(here::here("R/___setup_project___.R"))`.
- The setup aborts if the data root does not exist, loads every file in `R/Functions/` (including the copied contract functions), attaches `packages: attach` from `config.yml`, regenerates `R/package_dependencies.R`, sets the seed and ggplot theme, and resolves `path_data_storage`.
- Add a package by listing it in `config.yml` (attach or tools) or by calling it as `pkg::fun()`, then install it with `renv::install()` and snapshot deliberately.

## Tests

- Every function in `R/Functions/` outside `Contracts/` has `R/Tests/testthat/test-<function_name>.R`, written before the implementation. Shared fixtures go in `helper-*.R` files.
- Tests use small synthetic inputs, never the real TRY or BIEN data. BIEN calls are mocked with `testthat::local_mocked_bindings(.package = "BIEN")`. The export tests use the contract fixtures of the copy (`build_try_traits_fixture()`, `build_bien_traits_fixture()`).
- `Rscript R/Tests/Run_tests.R` validates the function files (`validate_function_names()`) and runs the suite in a clean subprocess (`run_project_tests()`).
- Focused run: source the setup, then `run_test_file(test_file = ...)`.
