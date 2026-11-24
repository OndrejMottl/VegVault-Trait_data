#' @title Validate a trait export against its VegVault contract
#' @description
#' Check the saved files of one trait artefact, a single file or all parts
#' together, with the VegVault contract validator (the copy in
#' `R/Functions/Contracts/`).
#' @param path_artefact
#' Character vector. Paths of the saved artefact files: one file, or every
#' part of a partitioned artefact.
#' @param path_contract
#' Character scalar. Path of the contract (`.yml`) in the project's copy of
#' the VegVault contracts (`Data/Contracts/`).
#' @return
#' The input `path_artefact`, unchanged, once the artefact passes, so that
#' the function can be the command of a `format = "file"` target.
#' @details
#' The check is `validate_producer_artefact()`: file names, parts, object
#' class, columns, values and keys. The parts of a partitioned artefact are
#' checked together, so that keys must be unique across parts. Every
#' violation is reported at once and the function aborts with an error of
#' class `producer_contract_error`. The files are never changed.
#' @seealso [save_trait_export()], [save_trait_export_parts()]
#' @export
validate_trait_export <- function(
  path_artefact,
  path_contract
) {
  assertthat::assert_that(
    base::is.character(path_artefact),
    base::length(path_artefact) > 0L,
    base::all(!base::is.na(path_artefact)),
    base::all(base::file.exists(path_artefact)),
    msg = "'path_artefact' must be the paths of existing artefact files."
  )

  assertthat::assert_that(
    base::is.character(path_contract),
    base::length(path_contract) == 1L,
    !base::is.na(path_contract),
    base::file.exists(path_contract),
    msg = "'path_contract' must be the path of an existing contract file."
  )

  validate_producer_artefact(
    contract = path_contract,
    path_artefact = path_artefact,
    verbose = FALSE
  )

  res <- path_artefact

  return(res)
}
