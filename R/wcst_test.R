#' Simulate a Wisconsin Card Sorting Task (WCST) trial
#'
#' This function compares the user's selected rule to the correct sorting rule
#' for a given WCST trial. It returns TRUE if the user selected the correct rule,
#' and FALSE otherwise. This is a simplified scoring function and does not consider
#' card attributes.
#'
#' @param rule A character string indicating the current correct rule.
#'   Must be one of: "color", "shape", or "number".
#' @param user_response A character string indicating the user's selected rule.
#'
#' @return A logical value: TRUE if the user response matches the rule, FALSE otherwise.
#' @export
#'
#' @examples
#' wcst_trial("color", "color")   # TRUE
#' wcst_trial("shape", "number")  # FALSE
wcst_trial <- function(rule, user_response) {
  return(rule == user_response)
}
