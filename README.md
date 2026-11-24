# VegVault: trait data

This repository produces the plant-trait data of [VegVault](https://github.com/OndrejMottl/VegVault). It takes trait measurements from [TRY](https://www.try-db.org) and [BIEN](https://biendata.org), converts every value to one unit per trait, removes duplicated measurements, checks the result and saves it as files that the VegVault database build reads from a tagged release.

It is part of the [BIODYNAMICS project](https://ondrejmottl.github.io/projects/BIODYNAMICS/).

## Traits and sources

The six traits follow Díaz et al. (2016): plant height, leaf area, leaf mass per area, leaf nitrogen content per leaf dry mass, seed (diaspore) mass and stem specific density (wood density).

- **TRY:** a data request of public TRY data, released under CC BY. The request file is obtained from TRY by hand and is not part of this repository.
- **BIEN:** downloaded with the BIEN R package (`save_bien_trait_download()`, when `bien: flag_download` is `true`). The package states that public BIEN data are licensed under CC-BY.

The exact input files of a release, with their SHA-256 checksums, are listed in its manifest tables (see below). When you use the data, cite TRY and BIEN as their terms ask.

## What a release contains

Artefacts in `Outputs/Data/`, saved in `qs2` format under a `.qs` extension (read them with `qs2::qs_read()`):

- `data_trait_try_<date>__<hash>__.qs`: the published TRY records, one file.
- `data_traits_bien_<part>_<date>__<hash>__.qs`: the published BIEN records, in four parts numbered from 0, sorted by BIEN record ID.

Each record holds the trait value in the canonical unit of its trait (`trait_value`, `trait_unit`), the value and unit as the provider wrote them, the conversion rule that was applied, and its provenance: provider, dataset, record and trait identifiers, species name, coordinates and references.

Aggregate tables in `Outputs/Tables/`, one set per provider (`try`, `bien`):

| Table | Content |
| --- | --- |
| `trait_unit_audit_summary_<provider>` | Records per source, trait, original unit, rule and disposition, with value quantiles |
| `trait_unit_reconciliation_<provider>` | Records per trait by disposition (published, quarantined, dropped) and reason; they add up to the raw records |
| `trait_reference_completeness_<provider>` | Per trait, the share of published records with a value in each reference column |
| `trait_coordinate_summary_<provider>` | Records and datasets per coordinate source, and whether the coordinates were kept |
| `trait_bien_try_twins_by_source` | Per BIEN source, how many records have a TRY twin, and whether the source qualifies for and is on the list of shared sources (BIEN only) |
| `trait_data_id_summary_try` | TRY trait records per trait and DataID, with TRY's name of each DataID, for reviewing the excluded DataIDs (TRY only) |
| `trait_artifact_manifest_<provider>` | Every input, registry file and output, with size, SHA-256 and row count |

Records that are not published stay out of the repository: they are kept only in the git-ignored `Data/Temp/trait_unit_audit/` and are counted in the tables.

## How the data are processed

- **Units:** a record is published only when its unit is registered and the conversion to the canonical unit is verified. The rules live in `config/trait_units/`, each with its source; TRY conversions are checked against TRY's standardised values. Everything else is quarantined.
- **Duplicates:** one value is kept per TRY observation and trait, TRY records that TRY flags as copies of a published original are dropped, and BIEN records that duplicate a TRY measurement from a shared compilation are dropped, keeping the TRY copy (`config/trait_duplicates/`).
- **Implausible sources:** source and trait combinations with values on an impossible scale are quarantined (`config/trait_quarantine/`).
- **Coordinates:** a coordinate outside -90 to 90 (latitude) or -180 to 180 (longitude), or one without its pair, is set missing; the record stays published.
- **References:** TRY records carry the citation of their publication (`record_reference`); BIEN gives its references per source. Mis-encoded BIEN text is repaired. Contributor names and contact details are never exported.

The reconciliation must balance for every provider and trait: raw records equal published plus quarantined plus dropped records. Before any table is written, the saved artefacts must also pass the VegVault contracts (see below); otherwise the run stops.

## Running the pipeline

The project uses R 4.6.2 and `renv`, and follows the layout of the [project template](https://github.com/OndrejMottl/Project_template). Restore the packages once with `Rscript R/___Init_project___.R`. The raw inputs are not in Git: put the TRY request file in `Data/Input/TRY/Raw/` and the BIEN downloads in `Data/Input/BIEN/`. The TRY file is checked against the SHA-256 checksum in `config.yml`, and the newest BIEN file of each trait (by the date in its name) is used.

The export is a [`{targets}`](https://docs.ropensci.org/targets/) pipeline in three stages: data acquisition, data processing and data export, each with its own store under `Data/Targets_data/`. Run all three with:

```sh
Rscript R/Analyses/00_run_main.R
```

This release run writes to `Outputs/Data/`, `Outputs/Tables/` and `Data/Temp/trait_unit_audit/`. A test run with the `dev` profile writes everything to the git-ignored `Outputs/Data_temp/` instead:

```sh
R_CONFIG_ACTIVE=dev Rscript R/Analyses/00_run_main.R
```

Targets that are up to date are skipped, so a run after a change rebuilds only what depends on it. The traits, the excluded TRY DataIDs and the other rules are tracked tables in `config/`; `.ai/analysis-structure.md` describes the stages and their targets.

BIEN is downloaded only when `bien: flag_download` is set to `true` in `config.yml`; keep it `false` otherwise.

Run the tests with `Rscript R/Tests/Run_tests.R`. One test compares the contract copy with a VegVault checkout next to this one (`../VegVault`), when there is one.

## Use in VegVault

VegVault reads the artefacts of a tagged release of this repository, pinned by tag and SHA-256 checksum, and checks them against its producer contracts `try_traits` and `bien_traits`.

This repository holds a copy of those contracts and of VegVault's contract validator (`Data/Contracts/` and `R/Functions/Contracts/`), taken from one VegVault commit. The export stage checks its artefacts with this copy, so the check can be repeated from a tag of this repository alone. `Data/Contracts/contract_copy.yml` records the VegVault commit and the contract versions; the copy is not edited here but made again from VegVault when a contract changes (see `Data/Contracts/README.md` in VegVault).

Guidance for contributors and coding assistants is in `AGENTS.md` and `.ai/repository-contract.md`.

## Licence

The code is released under the MIT licence (`LICENSE`). The trait data keep the licences of their providers.
