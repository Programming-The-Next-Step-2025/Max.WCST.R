#' Normalize a WCST user response string
#'
#' This function trims leading and trailing whitespace and converts all characters
#' to lowercase. Useful for standardizing user responses before scoring in WCST tasks.
#'
#' @param response A character string representing a user's rule choice (e.g., "Color", " shape ").
#'
#' @return A cleaned character string in lowercase with no leading or trailing spaces.
#' @export
#'
#' @examples
#' normalize_response(" Color ")
#' normalize_response("  SHAPE")
#' normalize_response("number ")
normalize_response <- function(response) {
  stringr::str_trim(stringr::str_to_lower(response))
}
