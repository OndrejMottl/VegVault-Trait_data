#' @title Select the published TRY records
#' @description
#' Keep the published TRY records in the column order of the TRY artefact.
#' @param data_resolved
#' TRY records after [resolve_try_trait_dispositions()], with the columns
#' `disposition`, `name`, `trait_value`, `trait_unit`, `trait_domain`,
#' `trait_full_name` and `observation_id`.
#' @return
#' A tibble with the records whose `disposition` is `"published"`, in input
#' order, without the column `name`, and with `trait_value`, `trait_unit`,
#' `trait_domain` and `trait_full_name` moved, in this order, before
#' `observation_id`. The other columns keep their order.
#' @details
#' `name` repeats `source_trait_id` and is not exported.
#' @export
select_try_published_records <- function(data_resolved) {
  vec_columns_required <-
    c(
      "disposition",
      "name",
      "trait_value",
      "trait_unit",
      "trait_domain",
      "trait_full_name",
      "observation_id"
    )

  assertthat::assert_that(
    base::is.data.frame(data_resolved),
    msg = "'data_resolved' must be a data frame."
  )

  vec_columns_missing <-
    base::setdiff(vec_columns_required, base::names(data_resolved))

  assertthat::assert_that(
    base::length(vec_columns_missing) == 0L,
    msg = stringr::str_c(
      "'data_resolved' lacks the columns: ",
      stringr::str_c(vec_columns_missing, collapse = ", "),
      "."
    )
  )

  res <-
    data_resolved |>
    dplyr::filter(.data$disposition == "published") |>
    dplyr::select(-"name") |>
    dplyr::relocate(
      "trait_value",
      "trait_unit",
      "trait_domain",
      "trait_full_name",
      .before = "observation_id"
    )

  return(res)
}
