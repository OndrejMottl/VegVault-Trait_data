#' @title Resolve BIEN records duplicated in TRY
#' @description
#' Mark published BIEN records as dropped when they come from a source that
#' copies a compilation also held by TRY and a published TRY record holds
#' the same measurement. The TRY copy is kept.
#' @param data_bien
#' BIEN records after [resolve_trait_unit_contract()], with the columns
#' `disposition`, `disposition_reason`, `trait_domain`,
#' `scrubbed_species_binomial`, `trait_value`, `url_source`,
#' `source_citation` and `project_pi`.
#' @param data_try_published
#' The published TRY artifact, with the columns `trait_domain`,
#' `acc_species_name` and `trait_value`.
#' @param data_shared_sources
#' A data frame with the column `bien_source_key`: the BIEN sources that
#' copy compilations held by TRY (see
#' `config/trait_duplicates/bien_sources_shared_with_try.csv`).
#' @return
#' `data_bien` with the same rows and columns. Duplicated records get
#' `disposition = "dropped"` and
#' `disposition_reason = "duplicate_of_try_record"`.
#' @details
#' The BIEN source key comes from [build_bien_source_key()]. A record is a
#' duplicate when its source is listed and a published TRY record has the
#' same [build_trait_measurement_key()]: trait domain, species name and
#' value at three significant digits (both values in the unit-contract
#' unit). Records from unlisted sources are never compared, because value
#' matches there are mostly chance.
#' @seealso [resolve_try_duplicate_records()]
#' @export
resolve_bien_try_duplicates <- function(
  data_bien,
  data_try_published,
  data_shared_sources
) {
  vec_bien_columns <-
    c(
      "disposition",
      "disposition_reason",
      "trait_domain",
      "scrubbed_species_binomial",
      "trait_value",
      "url_source",
      "source_citation",
      "project_pi"
    )

  vec_try_columns <-
    c("trait_domain", "acc_species_name", "trait_value")

  assertthat::assert_that(
    base::is.data.frame(data_bien),
    base::all(vec_bien_columns %in% base::names(data_bien)),
    msg = stringr::str_c(
      "'data_bien' must be a data frame with the columns: ",
      stringr::str_c(vec_bien_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_try_published),
    base::all(vec_try_columns %in% base::names(data_try_published)),
    msg = stringr::str_c(
      "'data_try_published' must be a data frame with the columns: ",
      stringr::str_c(vec_try_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_shared_sources),
    "bien_source_key" %in% base::names(data_shared_sources),
    msg = "'data_shared_sources' must have the column 'bien_source_key'."
  )

  vec_shared_keys <-
    data_shared_sources |>
    dplyr::pull("bien_source_key") |>
    base::trimws()

  vec_try_keys <-
    build_trait_measurement_key(
      trait_domain = dplyr::pull(data_try_published, "trait_domain"),
      species = dplyr::pull(data_try_published, "acc_species_name"),
      trait_value = dplyr::pull(data_try_published, "trait_value")
    ) |>
    base::unique()

  res <-
    data_bien |>
    dplyr::mutate(
      bien_source_key = build_bien_source_key(
        url_source = .data$url_source,
        source_citation = .data$source_citation,
        project_pi = .data$project_pi
      ),
      measurement_key = build_trait_measurement_key(
        trait_domain = .data$trait_domain,
        species = .data$scrubbed_species_binomial,
        trait_value = .data$trait_value
      ),
      flag_duplicate = .data$disposition == "published" &
        .data$bien_source_key %in% vec_shared_keys &
        .data$measurement_key %in% vec_try_keys,
      disposition = dplyr::if_else(
        .data$flag_duplicate,
        "dropped",
        .data$disposition
      ),
      disposition_reason = dplyr::if_else(
        .data$flag_duplicate,
        "duplicate_of_try_record",
        .data$disposition_reason
      )
    ) |>
    dplyr::select(
      dplyr::all_of(base::names(data_bien))
    )

  return(res)
}
