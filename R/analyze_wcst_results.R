#' Analyze WCST session results
#'
#' Summarizes performance from a WCST session simulation.
#'
#' @param session_df A data frame from \code{simulate_wcst_session()}
#'
#' @return A list with performance metrics: total trials, accuracy, perseverative errors.
#' @export
#' @examples
#' res <- simulate_wcst_session(15)
#' analyze_wcst_results(res)
analyze_wcst_results <- function(session_df) {
  total_trials <- nrow(session_df)
  accuracy <- mean(session_df$correct)
  perseverative_errors <- sum(duplicated(session_df$user_choice) & !session_df$correct)
  
  return(list(
    total_trials = total_trials,
    accuracy = round(accuracy, 2),
    perseverative_errors = perseverative_errors
  ))
}
