#' Normalize and validate a WCST user response
#'
#' @param response A character string input from the user.
#'
#' @return A cleaned string if valid, or NA if invalid.
#' @export
normalize_response <- function(response) {
  cleaned <- stringr::str_trim(stringr::str_to_lower(response))
  valid_rules <- c("color", "shape", "number")

  if (!(cleaned %in% valid_rules)) {
    warning("Invalid response. Must be 'color', 'shape', or 'number'. Returning NA.")
    return(NA)
  }

  return(cleaned)
}
