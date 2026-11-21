# VegVault-Trait_data Agent Guide

This repository produces the TRY and BIEN trait inputs consumed by VegVault. It follows the project template (`{targets}` stages, `config.yml`, one function per file with a test).

## Required reading

1. Read `.ai/repository-contract.md` for the binding local data and execution safeguards.
2. Read `.ai/analysis-structure.md` for the layout, stages, configuration, producer contracts and tests.
3. If `../VegVault/AGENTS.md` exists, read it and the task-specific canonical files it routes to, especially `.ai/suite-architecture.md`, `.ai/r-coding.md`, `.ai/r-functions.md`, `.ai/r-environment.md`, `.ai/git-workflow.md`, and `.ai/debugging.md`. Where `../VegVault/.ai/r-environment.md` describes unconverted producers (`renv/library_list.lock`, `tests/testthat.R`), follow `.ai/analysis-structure.md` here instead.
4. If the sibling guide is unavailable, use the local contract, `.ai/analysis-structure.md` and existing repository conventions for narrow, well-scoped work. Ask before broad refactors, expensive reruns, output-contract changes, destructive actions, or release work.
5. For commit-message requests, read `../VegVault/.ai/commit-messages.md` when available; otherwise use the self-contained fallback in `.ai/commit-messages.md`. Inspect the actual changed scope before proposing a message.

Local safeguards remain binding even when the central guide is available. Tool-native files are compatibility adapters and must remain short.

## Key commands

Run from the project root in a fresh session; judge success from the output text, not the exit code.

| Purpose | Command |
| --- | --- |
| Restore or initialise the R environment | `Rscript R/___Init_project___.R` |
| Run the full test suite | `Rscript R/Tests/Run_tests.R` |
| Run the stages (acquisition, processing, export) | `Rscript R/Analyses/00_run_main.R` |
| Run one stage | `Rscript R/Analyses/<NN>_<Stage>/00_run.R` |
| Test run into `Outputs/Data_temp/` | set `R_CONFIG_ACTIVE=dev`, then run the stages |

Every pipeline run, every BIEN download (`bien: flag_download: true`) and every change of the raw inputs needs the user's explicit approval first (`.ai/repository-contract.md`).
