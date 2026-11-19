#' @title Build the BIEN source key
#' @description
#' Identify the source of each BIEN record by its most specific source
#' field.
#' @param url_source
#' Character vector of BIEN `url_source` values.
#' @param source_citation
#' Character vector of BIEN `source_citation` values, the same length.
#' @param project_pi
#' Character vector of BIEN `project_pi` values, the same length.
#' @return
#' A character vector: the first non-missing of `url_source`,
#' `source_citation` and `project_pi`, with surrounding spaces removed, or
#' `"BIEN"` when all three are missing.
#' @seealso [resolve_bien_try_duplicates()], [summarise_bien_try_twins()]
#' @export
build_bien_source_key <- function(
  url_source,
  source_citation,
  project_pi
) {
  assertthat::assert_that(
    base::length(source_citation) == base::length(url_source),
    base::length(project_pi) == base::length(url_source),
    msg = stringr::str_c(
      "'url_source', 'source_citation' and 'project_pi' must have the same",
      " length."
    )
  )

  res <-
    dplyr::coalesce(
      base::as.character(url_source),
      base::as.character(source_citation),
      base::as.character(project_pi),
      "BIEN"
    ) |>
    base::trimws()

  return(res)
}
