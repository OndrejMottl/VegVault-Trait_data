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

## Change and validation contract

For changes to output shape or meaning, trace consumers in `../VegVault/R/02_Main_analyses/04_Import_try_data.R` and `../VegVault/R/02_Main_analyses/05_Import_bien_trait_data.R`, coordinate producer documentation and a reviewed tag, and update the pinned integration reference. Validate both the TRY contract and every affected BIEN partition before proposing a release.
