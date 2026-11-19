#' @title Repair mis-encoded text
#' @description
#' Reverse the mis-encoding of UTF-8 text that was read as Latin-1 or
#' Windows-1252 and stored again as UTF-8, once or twice. In such text an
#' accented letter shows as two or more characters that start with an
#' A-tilde or A-circumflex, and a dash or quote can become an invisible
#' control character.
#' @param vec_text
#' Character vector of text values.
#' @param max_rounds
#' Integer. The largest number of mis-encoding rounds reversed per value.
#' @param verbose
#' Logical. If `TRUE` (default), the number of repaired distinct values is
#' printed via `cli`.
#' @return
#' A character vector of the same length as `vec_text`. Mis-encoded values
#' are repaired; other values, including `NA`, are returned unchanged.
#' @details
#' Each distinct value is converted back at most `max_rounds` times. One
#' round writes the text as Windows-1252 bytes, or as Latin-1 bytes when
#' the text holds C1 control characters, and reads the bytes as UTF-8. A
#' round is kept only when it gives valid UTF-8. Correct text with accented
#' letters practically never does (it would need an accented capital
#' directly followed by a symbol such as a curly quote), so it is left as
#' it is. Remaining C1 control characters (U+0080 to U+009F) come from
#' Windows-1252 bytes read as Latin-1 and are replaced by their
#' Windows-1252 characters (U+0096 becomes an en dash). A value that still
#' contains a mis-encoded sequence after the repair, for example because it
#' also holds correct accented letters, is reported with a warning.
#' @seealso [build_bien_source_key()]
#' @export
repair_mis_encoded_text <- function(
  vec_text,
  max_rounds = 3L,
  verbose = TRUE
) {
  assertthat::assert_that(
    base::is.character(vec_text),
    msg = "'vec_text' must be a character vector."
  )

  assertthat::assert_that(
    assertthat::is.count(max_rounds),
    msg = "'max_rounds' must be a single positive whole number."
  )

  assertthat::assert_that(
    assertthat::is.flag(verbose),
    !base::is.na(verbose),
    msg = "'verbose' must be TRUE or FALSE."
  )

  vec_distinct <-
    base::unique(vec_text[!base::is.na(vec_text)])

  if (
    base::length(vec_distinct) == 0L
  ) {
    return(vec_text)
  }

  # Each round is kept only where the bytes form valid UTF-8; Encoding<-
  #   declares the bytes as UTF-8 without changing them
  vec_rounds_reversed <-
    purrr::reduce(
      .x = base::seq_len(max_rounds),
      .f = ~ {
        vec_bytes <-
          dplyr::coalesce(
            base::iconv(.x, from = "UTF-8", to = "windows-1252"),
            base::iconv(.x, from = "UTF-8", to = "latin1")
          )

        base::Encoding(vec_bytes) <- "UTF-8"

        vec_valid <-
          !base::is.na(vec_bytes) & base::validUTF8(vec_bytes)

        base::ifelse(vec_valid, vec_bytes, .x)
      },
      .init = vec_distinct
    )

  # Windows-1252 characters for the C1 control characters; the five codes
  #   that Windows-1252 leaves undefined keep their control character
  vec_c1_codes <-
    128:159

  vec_c1_characters <-
    purrr::map_chr(
      .x = vec_c1_codes,
      .f = ~ base::intToUtf8(.x)
    )

  vec_cp1252_characters <-
    purrr::map_chr(
      .x = vec_c1_codes,
      .f = ~ base::iconv(
        base::rawToChar(base::as.raw(.x)),
        from = "windows-1252",
        to = "UTF-8"
      )
    )

  vec_c1_defined <-
    !base::is.na(vec_cp1252_characters) &
    vec_cp1252_characters != vec_c1_characters

  vec_c1_replacements <-
    rlang::set_names(
      x = vec_cp1252_characters[vec_c1_defined],
      nm = vec_c1_characters[vec_c1_defined]
    )

  vec_repaired <-
    stringr::str_replace_all(
      string = vec_rounds_reversed,
      pattern = vec_c1_replacements
    )

  n_repaired <-
    base::sum(vec_repaired != vec_distinct)

  # A mis-encoded character is a UTF-8 lead byte followed by its
  #   continuation bytes, each shown as a Latin-1 or Windows-1252
  #   character
  regex_continuation <-
    stringr::str_c(
      "[\u0080-\u00bf",
      stringr::str_c(vec_c1_replacements, collapse = ""),
      "]"
    )

  regex_mis_encoded <-
    stringr::str_glue(
      "[\u00c2-\u00df]{regex_continuation}",
      "|[\u00e0-\u00ef]{regex_continuation}{{2}}",
      "|[\u00f0-\u00f4]{regex_continuation}{{3}}"
    )

  n_unrepaired <-
    base::sum(
      stringr::str_detect(
        string = vec_repaired,
        pattern = regex_mis_encoded
      )
    )

  if (
    n_unrepaired > 0L
  ) {
    cli::cli_warn(
      c(
        "Distinct values still mis-encoded after the repair: {n_unrepaired}.",
        "i" = "They are returned as the repair left them."
      )
    )
  }

  if (
    base::isTRUE(verbose)
  ) {
    cli::cli_inform(
      stringr::str_c(
        "Repaired {n_repaired} of {base::length(vec_distinct)} distinct",
        " value{?s}."
      )
    )
  }

  res <-
    vec_repaired[base::match(vec_text, vec_distinct)]

  return(res)
}
