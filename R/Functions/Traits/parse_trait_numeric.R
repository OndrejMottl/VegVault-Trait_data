#' @title Parse strict numeric trait values
#' @description Convert complete numeric strings to double values.
#' @param value A vector coercible to character containing candidate values.
#' @return A double vector with the same length as `value`.
#' @details Ranges, qualifiers, partial matches, blanks, and errors fail.
#' @export
parse_trait_numeric <- function(value) {
  value_character <-
    base::trimws(base::as.character(value))

  pattern_numeric <-
    "^[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"

  flag_numeric <-
    !base::is.na(value_character) &
    base::nzchar(value_character) &
    base::grepl(pattern_numeric, value_character, perl = TRUE)

  res <-
    purrr::map2_dbl(
      .x = value_character,
      .y = flag_numeric,
      .f = ~ if (.y) base::as.numeric(.x) else NA_real_
    )

  return(res)
}
