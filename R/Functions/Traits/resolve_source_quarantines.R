#' @title Resolve reviewed source-level quarantines
#' @description
#' Quarantine the published records of provider sources whose values were
#' reviewed and found to be on an impossible scale.
#' @param data_source
#' Trait records after [apply_trait_unit_contract()], with the columns
#' `source_provider`, `source_trait_id`, `source_dataset_key`,
#' `disposition` and `disposition_reason`.
#' @param data_quarantine
#' The reviewed list (`config/trait_quarantine/source_quarantine.csv`),
#' with `source_provider`, `source_trait_id`, `source_dataset_key` and
#' `disposition_reason`.
#' @return
#' `data_source` with the same rows and columns. Published records whose
#' provider, trait and source match a listed entry get
#' `disposition = "quarantined"` and the listed `disposition_reason`.
#' @details
#' The match is exact on all three keys. Records that are already
#' quarantined or dropped are not changed. A listed entry may match no
#' record (for example after a source update); the list is reviewed with
#' the scale diagnostics of the audit tables.
#' @export
resolve_source_quarantines <- function(
  data_source,
  data_quarantine
) {
  vec_key_columns <-
    c("source_provider", "source_trait_id", "source_dataset_key")

  assertthat::assert_that(
    base::is.data.frame(data_source),
    base::all(
      c(vec_key_columns, "disposition", "disposition_reason") %in%
        base::names(data_source)
    ),
    msg = stringr::str_c(
      "'data_source' must have the columns source_provider,",
      " source_trait_id, source_dataset_key, disposition and",
      " disposition_reason."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_quarantine),
    base::all(
      c(vec_key_columns, "disposition_reason") %in%
        base::names(data_quarantine)
    ),
    msg = stringr::str_c(
      "'data_quarantine' must have the columns source_provider,",
      " source_trait_id, source_dataset_key and disposition_reason."
    )
  )

  data_listed <-
    data_quarantine |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(vec_key_columns), base::as.character)
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(vec_key_columns)),
      .keep_all = TRUE
    ) |>
    dplyr::select(
      dplyr::all_of(vec_key_columns),
      quarantine_reason = "disposition_reason"
    )

  res <-
    data_source |>
    dplyr::left_join(
      data_listed,
      by = vec_key_columns,
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      flag_quarantine = .data$disposition == "published" &
        !base::is.na(.data$quarantine_reason),
      disposition = dplyr::if_else(
        .data$flag_quarantine,
        "quarantined",
        .data$disposition
      ),
      disposition_reason = dplyr::if_else(
        .data$flag_quarantine,
        .data$quarantine_reason,
        .data$disposition_reason
      )
    ) |>
    dplyr::select(
      dplyr::all_of(base::names(data_source))
    )

  return(res)
}
