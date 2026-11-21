#' @title Diagnose the list of BIEN sources shared with TRY
#' @description
#' Compare the tracked list of BIEN sources that copy compilations held by
#' TRY with the sources that qualify in the current data, and warn when
#' they differ.
#' @param data_twins
#' The output of [summarise_bien_try_twins()], with the columns
#' `bien_source_key`, `qualifies` and `listed`.
#' @param data_shared_sources
#' The tracked list, with the column `bien_source_key`.
#' @return
#' A character vector, possibly empty: the sources whose `qualifies` and
#' `listed` differ, followed by the listed sources that are absent from
#' `data_twins`.
#' @details
#' A source that qualifies but is not listed, or the reverse, needs a
#' review of the list before a release. The function warns, naming these
#' sources, and never changes the list. Listed keys are compared without
#' surrounding white space.
#' @seealso [summarise_bien_try_twins()], [resolve_bien_try_duplicates()]
#' @export
diagnose_bien_shared_sources <- function(
  data_twins,
  data_shared_sources
) {
  vec_twin_columns <-
    c("bien_source_key", "qualifies", "listed")

  assertthat::assert_that(
    base::is.data.frame(data_twins),
    base::all(vec_twin_columns %in% base::names(data_twins)),
    msg = stringr::str_c(
      "'data_twins' must be a data frame with the columns: ",
      stringr::str_c(vec_twin_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_shared_sources),
    "bien_source_key" %in% base::names(data_shared_sources),
    msg = stringr::str_c(
      "'data_shared_sources' must be a data frame with the column",
      " 'bien_source_key'."
    )
  )

  vec_sources_differing <-
    data_twins |>
    dplyr::filter(.data$qualifies != .data$listed) |>
    dplyr::pull("bien_source_key")

  vec_sources_absent <-
    base::setdiff(
      base::trimws(dplyr::pull(data_shared_sources, "bien_source_key")),
      dplyr::pull(data_twins, "bien_source_key")
    )

  res <-
    c(vec_sources_differing, vec_sources_absent)

  if (
    base::length(res) > 0L
  ) {
    cli::cli_warn(
      c(
        "The list of BIEN sources shared with TRY differs from the data.",
        "i" = "Review the list before a release.",
        "x" = "Sources: {res}"
      )
    )
  }

  return(res)
}
