#' @title Validate and reconcile canonical trait records
#' @description
#' Enforce the publication rules of the unit contract and count every raw
#' record by provider, trait, disposition and reason, so that published,
#' quarantined and dropped records add up to the raw input.
#' @param data_source
#' A canonical trait data frame from [apply_trait_unit_contract()], with
#' the columns `source_provider`, `source_record_id`, `source_trait_id`,
#' `conversion_status`, `disposition`, `disposition_reason`, `trait_value`,
#' `trait_unit` and `conversion_rule_key`.
#' @param data_raw_counts
#' Optional data frame with `source_provider`, `source_trait_id` and
#' `n_raw`: the number of raw records per provider and trait before any
#' record was dropped.
#' @param data_dropped_counts
#' Optional data frame with `source_provider`, `source_trait_id`,
#' `disposition_reason` and `n_records`: records removed before the
#' contract was applied, counted by reason.
#' @return
#' A tibble with `source_provider`, `source_trait_id`, `disposition`,
#' `conversion_status`, `disposition_reason` and `n_records`. Records
#' dropped before the contract have the disposition `"dropped"` and a
#' missing `conversion_status`.
#' @details
#' Every record must have one disposition: `"published"`, `"quarantined"`
#' or `"dropped"`. Published records need a finite value, a unit and a
#' conversion rule, and must be unique by provider, record and trait. When
#' `data_raw_counts` is given, the counts per provider and trait must equal
#' `n_raw`; otherwise the function aborts and names the traits that do not
#' balance.
#' @export
validate_trait_unit_reconciliation <- function(
  data_source,
  data_raw_counts = NULL,
  data_dropped_counts = NULL
) {
  vec_required_columns <-
    c(
      "source_provider",
      "source_record_id",
      "source_trait_id",
      "conversion_status",
      "disposition",
      "disposition_reason",
      "trait_value",
      "trait_unit",
      "conversion_rule_key"
    )

  if (
    !base::all(vec_required_columns %in% base::names(data_source))
  ) {
    cli::cli_abort("Trait-unit reconciliation received an invalid contract.")
  }

  assertthat::assert_that(
    base::is.null(data_raw_counts) ||
      (
        base::is.data.frame(data_raw_counts) &&
          base::all(
            c("source_provider", "source_trait_id", "n_raw") %in%
              base::names(data_raw_counts)
          )
      ),
    msg = stringr::str_c(
      "'data_raw_counts' must be NULL or a data frame with the columns",
      " source_provider, source_trait_id and n_raw."
    )
  )

  assertthat::assert_that(
    base::is.null(data_dropped_counts) ||
      (
        base::is.data.frame(data_dropped_counts) &&
          base::all(
            c(
              "source_provider",
              "source_trait_id",
              "disposition_reason",
              "n_records"
            ) %in% base::names(data_dropped_counts)
          )
      ),
    msg = stringr::str_c(
      "'data_dropped_counts' must be NULL or a data frame with the",
      " columns source_provider, source_trait_id, disposition_reason and",
      " n_records."
    )
  )

  vec_disposition <-
    dplyr::pull(data_source, "disposition")

  if (
    base::any(base::is.na(vec_disposition))
  ) {
    cli::cli_abort("Every trait record must have exactly one disposition.")
  }

  if (
    !base::all(vec_disposition %in% c("published", "quarantined", "dropped"))
  ) {
    cli::cli_abort(
      "A trait record disposition must be published, quarantined or dropped."
    )
  }

  data_published <-
    data_source |>
    dplyr::filter(.data$disposition == "published")

  if (
    base::any(!base::is.finite(dplyr::pull(data_published, "trait_value"))) ||
      base::any(base::is.na(dplyr::pull(data_published, "trait_unit"))) ||
      base::any(
        base::is.na(dplyr::pull(data_published, "conversion_rule_key"))
      )
  ) {
    cli::cli_abort(
      "Published trait records require finite values, units, and rules."
    )
  }

  data_duplicates <-
    data_published |>
    dplyr::count(
      .data$source_provider,
      .data$source_record_id,
      .data$source_trait_id,
      name = "n_records"
    ) |>
    dplyr::filter(.data$n_records > 1L)

  if (
    base::nrow(data_duplicates) > 0L
  ) {
    cli::cli_abort("Published source trait records must be unique.")
  }

  data_contract_counts <-
    data_source |>
    dplyr::mutate(
      source_provider = base::as.character(.data$source_provider),
      source_trait_id = base::as.character(.data$source_trait_id)
    ) |>
    dplyr::count(
      .data$source_provider,
      .data$source_trait_id,
      .data$disposition,
      .data$conversion_status,
      .data$disposition_reason,
      name = "n_records"
    )

  data_dropped_before <-
    if (
      base::is.null(data_dropped_counts)
    ) {
      NULL
    } else {
      data_dropped_counts |>
        dplyr::mutate(
          source_provider = base::as.character(.data$source_provider),
          source_trait_id = base::as.character(.data$source_trait_id),
          disposition = "dropped",
          conversion_status = NA_character_,
          disposition_reason = base::as.character(.data$disposition_reason),
          n_records = base::as.integer(.data$n_records)
        ) |>
        dplyr::select(
          "source_provider",
          "source_trait_id",
          "disposition",
          "conversion_status",
          "disposition_reason",
          "n_records"
        )
    }

  res <-
    dplyr::bind_rows(data_contract_counts, data_dropped_before) |>
    dplyr::arrange(
      .data$source_provider,
      .data$source_trait_id,
      .data$disposition,
      .data$conversion_status,
      .data$disposition_reason
    )

  if (
    !base::is.null(data_raw_counts)
  ) {
    data_balance <-
      dplyr::full_join(
        data_raw_counts |>
          dplyr::mutate(
            source_provider = base::as.character(.data$source_provider),
            source_trait_id = base::as.character(.data$source_trait_id)
          ) |>
          dplyr::select("source_provider", "source_trait_id", "n_raw"),
        res |>
          dplyr::group_by(.data$source_provider, .data$source_trait_id) |>
          dplyr::summarise(
            n_accounted = base::sum(.data$n_records),
            .groups = "drop"
          ),
        by = dplyr::join_by(source_provider, source_trait_id)
      ) |>
      dplyr::filter(
        base::is.na(.data$n_raw) |
          base::is.na(.data$n_accounted) |
          .data$n_raw != .data$n_accounted
      )

    if (
      base::nrow(data_balance) > 0L
    ) {
      vec_unbalanced <-
        data_balance |>
        dplyr::mutate(
          label = stringr::str_glue(
            "{source_provider} {source_trait_id}: raw {n_raw},",
            " accounted {n_accounted}"
          )
        ) |>
        dplyr::pull("label")

      cli::cli_abort(
        c(
          "Raw trait records do not balance the reconciliation.",
          rlang::set_names(
            vec_unbalanced,
            base::rep("x", base::length(vec_unbalanced))
          )
        )
      )
    }
  }

  return(res)
}
