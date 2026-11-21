testthat::test_that("select_bien_published_records() validates its input", {
  testthat::expect_error(
    select_bien_published_records(data_resolved = "records"),
    "data_resolved"
  )
  testthat::expect_error(
    select_bien_published_records(
      data_resolved = tibble::tibble(disposition = "published")
    ),
    "source_record_id"
  )
})

testthat::test_that("select_bien_published_records() sorts by record ID", {
  data_resolved <-
    tibble::tibble(
      source_record_id = c("10", "2", "b", "a", "1", "0"),
      trait_value = c(10, 2, 98, 97, 1, 0),
      disposition = c(
        "published", "published", "published", "published", "published",
        "dropped"
      )
    )

  testthat::expect_no_warning(
    res <-
      select_bien_published_records(data_resolved = data_resolved)
  )
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, base::names(data_resolved))
  testthat::expect_identical(
    dplyr::pull(res, "source_record_id"),
    c("1", "2", "10", "a", "b")
  )
  testthat::expect_identical(
    dplyr::pull(res, "trait_value"),
    c(1, 2, 10, 97, 98)
  )
})

testthat::test_that("select_bien_published_records() ignores input order", {
  data_resolved <-
    tibble::tibble(
      source_record_id = base::as.character(base::seq_len(1000L)),
      disposition = "published"
    )

  res_forward <-
    select_bien_published_records(data_resolved = data_resolved)

  res_reversed <-
    select_bien_published_records(
      data_resolved = dplyr::arrange(
        data_resolved,
        dplyr::desc(dplyr::row_number())
      )
    )

  testthat::expect_identical(res_forward, res_reversed)
})

testthat::test_that("select_bien_published_records() handles no published", {
  res <-
    select_bien_published_records(
      data_resolved = tibble::tibble(
        source_record_id = "1",
        disposition = "quarantined"
      )
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("source_record_id", "disposition"))
})
