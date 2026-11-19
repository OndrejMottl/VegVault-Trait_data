#' @title Summarise BIEN records with a TRY twin by BIEN source
#' @description
#' Count, per BIEN source, the published BIEN records that have a published
#' TRY record with the same measurement key, compare with a chance level,
#' and flag the sources that copy a compilation held by TRY.
#' @param data_bien
#' BIEN records after [apply_trait_unit_contract()], with the columns
#' `disposition`, `trait_domain`, `scrubbed_species_binomial`,
#' `trait_value`, `url_source`, `source_citation` and `project_pi`. Only
#' published records are counted.
#' @param data_try_published
#' The published TRY artifact, with `trait_domain`, `acc_species_name` and
#' `trait_value`.
#' @param data_shared_sources
#' Optional data frame with `bien_source_key`: the configured list of shared
#' sources, used to fill the `listed` column.
#' @param min_twins
#' Integer. Minimum number of BIEN records with a TRY twin. Default 100.
#' @param min_share
#' Numeric. Minimum share of BIEN records with a TRY twin. Default 0.5.
#' @param min_excess
#' Numeric. Minimum difference between the twin share and the chance
#' share. Default 0.25.
#' @return
#' A tibble with one row per BIEN source: `bien_source_key`,
#' `trait_domains`, `n_bien`, `n_twin`, `n_chance`, `share_twin`,
#' `share_chance`, `qualifies` and `listed` (missing when
#' `data_shared_sources` is `NULL`), sorted by `n_twin` (descending).
#' @details
#' A twin has the same [build_trait_measurement_key()] as a published TRY
#' record. The chance level repeats the comparison with every BIEN value
#' multiplied by 1.05, which keeps species and value ranges but breaks true
#' copies. A source qualifies when `n_twin >= min_twins`,
#' `share_twin >= min_share` and `share_twin - share_chance >= min_excess`.
#' Shares are rounded to three decimals.
#' @seealso [resolve_bien_try_duplicates()], [build_bien_source_key()]
#' @export
summarise_bien_try_twins <- function(
  data_bien,
  data_try_published,
  data_shared_sources = NULL,
  min_twins = 100L,
  min_share = 0.5,
  min_excess = 0.25
) {
  vec_bien_columns <-
    c(
      "disposition",
      "trait_domain",
      "scrubbed_species_binomial",
      "trait_value",
      "url_source",
      "source_citation",
      "project_pi"
    )

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
    base::all(
      c("trait_domain", "acc_species_name", "trait_value") %in%
        base::names(data_try_published)
    ),
    msg = stringr::str_c(
      "'data_try_published' must have the columns trait_domain,",
      " acc_species_name and trait_value."
    )
  )

  assertthat::assert_that(
    base::is.null(data_shared_sources) ||
      "bien_source_key" %in% base::names(data_shared_sources),
    msg = "'data_shared_sources' must be NULL or have 'bien_source_key'."
  )

  assertthat::assert_that(
    assertthat::is.count(min_twins),
    assertthat::is.number(min_share),
    min_share >= 0,
    min_share <= 1,
    assertthat::is.number(min_excess),
    min_excess >= 0,
    min_excess <= 1,
    msg = stringr::str_c(
      "'min_twins' must be a positive count; 'min_share' and 'min_excess'",
      " must be numbers between 0 and 1."
    )
  )

  vec_try_keys <-
    build_trait_measurement_key(
      trait_domain = dplyr::pull(data_try_published, "trait_domain"),
      species = dplyr::pull(data_try_published, "acc_species_name"),
      trait_value = dplyr::pull(data_try_published, "trait_value")
    ) |>
    base::unique()

  vec_listed_keys <-
    if (
      base::is.null(data_shared_sources)
    ) {
      NULL
    } else {
      base::trimws(dplyr::pull(data_shared_sources, "bien_source_key"))
    }

  res <-
    data_bien |>
    dplyr::filter(.data$disposition == "published") |>
    dplyr::mutate(
      bien_source_key = build_bien_source_key(
        url_source = .data$url_source,
        source_citation = .data$source_citation,
        project_pi = .data$project_pi
      ),
      flag_twin = build_trait_measurement_key(
        trait_domain = .data$trait_domain,
        species = .data$scrubbed_species_binomial,
        trait_value = .data$trait_value
      ) %in% vec_try_keys,
      flag_chance = build_trait_measurement_key(
        trait_domain = .data$trait_domain,
        species = .data$scrubbed_species_binomial,
        trait_value = .data$trait_value * 1.05
      ) %in% vec_try_keys
    ) |>
    dplyr::group_by(.data$bien_source_key) |>
    dplyr::summarise(
      trait_domains = stringr::str_c(
        base::sort(base::unique(.data$trait_domain)),
        collapse = "; "
      ),
      n_bien = dplyr::n(),
      n_twin = base::sum(.data$flag_twin),
      n_chance = base::sum(.data$flag_chance),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      share_twin = base::round(.data$n_twin / .data$n_bien, 3),
      share_chance = base::round(.data$n_chance / .data$n_bien, 3),
      qualifies = .data$n_twin >= min_twins &
        .data$share_twin >= min_share &
        .data$share_twin - .data$share_chance >= min_excess,
      listed = if (
        base::is.null(vec_listed_keys)
      ) {
        NA
      } else {
        .data$bien_source_key %in% vec_listed_keys
      }
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_twin))

  return(res)
}
