#' @title Select the published BIEN records in record-ID order
#' @description
#' Keep the published BIEN records, sorted by BIEN record ID, so that the
#' export parts do not depend on the row order of a download.
#' @param data_resolved
#' BIEN records after [resolve_bien_try_duplicates()], with the columns
#' `disposition` and `source_record_id` (character).
#' @return
#' A tibble with the records whose `disposition` is `"published"` and every
#' column of `data_resolved`, sorted by `source_record_id` read as a number,
#' with the text breaking ties.
#' @details
#' BIEN record IDs are whole numbers stored as text; an ID that is not a
#' number sorts after the numbers. The radix method makes the order the
#' same in every locale. The published rows are copied once.
#' @export
select_bien_published_records <- function(data_resolved) {
  assertthat::assert_that(
    base::is.data.frame(data_resolved),
    msg = "'data_resolved' must be a data frame."
  )

  vec_columns_missing <-
    base::setdiff(
      c("disposition", "source_record_id"),
      base::names(data_resolved)
    )

  assertthat::assert_that(
    base::length(vec_columns_missing) == 0L,
    msg = stringr::str_c(
      "'data_resolved' lacks the columns: ",
      stringr::str_c(vec_columns_missing, collapse = ", "),
      "."
    )
  )

  vec_published_rows <-
    base::which(dplyr::pull(data_resolved, "disposition") == "published")

  vec_published_ids <-
    dplyr::pull(data_resolved, "source_record_id")[vec_published_rows]

  # One reordering subset copies the published records only once
  res <-
    data_resolved |>
    dplyr::slice(
      vec_published_rows[
        base::order(
          base::suppressWarnings(base::as.numeric(vec_published_ids)),
          vec_published_ids,
          method = "radix"
        )
      ]
    )

  return(res)
}
