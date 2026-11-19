#' @title Summarise the completeness of trait reference columns
#' @description
#' Count, per provider and trait domain, how many records carry a value in
#' each reference column, and how many distinct values there are.
#' @param data_source
#' A data frame of trait records with the columns `source_provider`,
#' `trait_domain` and every column named in `reference_columns`.
#' @param reference_columns
#' Character vector of the reference columns to summarise.
#' @return
#' A tibble with one row per provider, trait domain and reference column,
#' sorted by these three, and the columns `source_provider`, `trait_domain`,
#' `reference_column`, `n_records`, `n_present`, `share_present` and
#' `n_distinct_present`.
#' @details
#' A value is present when it is not `NA` and not blank after surrounding
#' spaces are removed. `share_present` is `n_present / n_records`. The
#' table holds counts only, no reference text, so it can be published with
#' the other aggregate tables.
#' @seealso [summarise_trait_unit_audit()]
#' @export
summarise_trait_reference_completeness <- function(
  data_source,
  reference_columns
) {
  assertthat::assert_that(
    base::is.data.frame(data_source),
    msg = "'data_source' must be a data frame."
  )

  assertthat::assert_that(
    base::all(c("source_provider", "trait_domain") %in%
      base::names(data_source)),
    msg = "'data_source' must contain 'source_provider' and 'trait_domain'."
  )

  assertthat::assert_that(
    base::is.character(reference_columns),
    base::length(reference_columns) > 0L,
    base::all(reference_columns %in% base::names(data_source)),
    msg = "'reference_columns' must name one or more columns of 'data_source'."
  )

  # One reference column at a time on a narrow table, so a large input is
  #   not copied whole
  res <-
    reference_columns |>
    purrr::map(
      .f = ~ {
        sel_column <- .x

        data_source |>
          dplyr::select(
            "source_provider",
            "trait_domain",
            reference_value = dplyr::all_of(sel_column)
          ) |>
          dplyr::mutate(
            reference_value = base::as.character(.data$reference_value),
            flag_present = !base::is.na(.data$reference_value) &
              base::nzchar(base::trimws(.data$reference_value))
          ) |>
          dplyr::group_by(.data$source_provider, .data$trait_domain) |>
          dplyr::summarise(
            reference_column = sel_column,
            n_records = dplyr::n(),
            n_present = base::sum(.data$flag_present),
            n_distinct_present = dplyr::n_distinct(
              .data$reference_value[.data$flag_present]
            ),
            .groups = "drop"
          )
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      share_present = .data$n_present / .data$n_records
    ) |>
    dplyr::select(
      "source_provider", "trait_domain", "reference_column", "n_records",
      "n_present", "share_present", "n_distinct_present"
    ) |>
    dplyr::arrange(
      .data$source_provider,
      .data$trait_domain,
      .data$reference_column
    )

  return(res)
}
