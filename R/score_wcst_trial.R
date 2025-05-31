#' Score WCST trial by comparing guessed rule to actual matching feature
#'
#' @param current The current card (data.frame row or list)
#' @param reference The previous card to compare against
#' @param user_choice The rule guessed by the user ("color", "shape", "number")
#'
#' @return TRUE if user's guessed rule matches the actual matching feature
#' @export
#'
#' @examples
#' current <- list(color = "red", shape = "circle", number = 2)
#' reference <- list(color = "green", shape = "circle", number = 4)
#' score_wcst_trial(current, reference, "shape")  # TRUE
#' score_wcst_trial(current, reference, "color")  # FALSE
score_wcst_trial <- function(current, reference, user_choice) {
  if (length(user_choice) != 1) {
    stop("`user_choice` must be a single character string (e.g., 'color').")
  }

  if (is.data.frame(current)) current <- as.list(current)
  if (is.data.frame(reference)) reference <- as.list(reference)

  valid_rules <- c("color", "shape", "number")
  if (!user_choice %in% valid_rules) {
    stop("user_choice must be one of: 'color', 'shape', 'number'")
  }

  # Identify which feature actually matched
  matching_feature <- NULL
  for (feature in valid_rules) {
    if (as.character(current[[feature]]) == as.character(reference[[feature]])) {
      matching_feature <- feature
      break  # only one match expected by deck generation
    }
  }

  return(user_choice == matching_feature)
}
