testthat::test_that("validate_try_raw_file() validates its arguments", {
  sha_valid <-
    base::strrep("a", 64)

  testthat::expect_error(
    validate_try_raw_file(
      path_file = 1,
      sha256_expected = sha_valid,
      version_label = "TRY test request"
    ),
    "path_file"
  )
  testthat::expect_error(
    validate_try_raw_file(
      path_file = "28498.txt",
      sha256_expected = "abc",
      version_label = "TRY test request"
    ),
    "sha256_expected"
  )
  testthat::expect_error(
    validate_try_raw_file(
      path_file = "28498.txt",
      sha256_expected = base::toupper(sha_valid),
      version_label = "TRY test request"
    ),
    "sha256_expected"
  )
  testthat::expect_error(
    validate_try_raw_file(
      path_file = "28498.txt",
      sha256_expected = sha_valid,
      version_label = ""
    ),
    "version_label"
  )
})

testthat::test_that("validate_try_raw_file() names the manual download", {
  path_missing <-
    base::file.path(base::tempdir(), "no_such_try_request.txt")

  testthat::expect_error(
    validate_try_raw_file(
      path_file = path_missing,
      sha256_expected = base::strrep("a", 64),
      version_label = "TRY test request"
    ),
    "manual"
  )
  testthat::expect_error(
    validate_try_raw_file(
      path_file = path_missing,
      sha256_expected = base::strrep("a", 64),
      version_label = "TRY test request"
    ),
    "TRY test request"
  )
})

testthat::test_that("validate_try_raw_file() returns the recorded file", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines("TraitID\tDataID", con = path_file)

  sha_file <-
    base::unname(tools::sha256sum(path_file))

  res <-
    validate_try_raw_file(
      path_file = path_file,
      sha256_expected = sha_file,
      version_label = "TRY test request"
    )

  testthat::expect_identical(res, path_file)
})

testthat::test_that("validate_try_raw_file() rejects another version", {
  path_file <-
    withr::local_tempfile(fileext = ".txt")

  base::writeLines("another TRY request", con = path_file)

  sha_file <-
    base::unname(tools::sha256sum(path_file))

  testthat::expect_error(
    validate_try_raw_file(
      path_file = path_file,
      sha256_expected = base::strrep("0", 64),
      version_label = "TRY test request"
    ),
    "checksum"
  )
  testthat::expect_error(
    validate_try_raw_file(
      path_file = path_file,
      sha256_expected = base::strrep("0", 64),
      version_label = "TRY test request"
    ),
    sha_file
  )
})
