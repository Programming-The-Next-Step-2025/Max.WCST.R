#' Analyze WCST session results
#'
#' Summarizes performance from a WCST session simulation.
#'
#' @param session_df A data frame from \code{simulate_wcst_session()}
#'
#' @return A list with performance metrics: total trials, accuracy, number of correct trials, number of rule shifts, perseverative errors.
#' @export
#' @examples
#' res <- simulate_wcst_session(15)
#' analyze_wcst_results(res)
analyze_wcst_results <- function(session_df) {
  if (!all(c("trial", "rule", "user_choice", "correct") %in% colnames(session_df))) {
    stop("session_df must contain columns: trial, rule, user_choice, correct")
  }

  total_trials <- nrow(session_df)
  n_correct <- sum(session_df$correct)
  accuracy <- round(n_correct / total_trials, 2)

  # Identify rule switches
  rule_change <- c(FALSE, session_df$rule[-1] != session_df$rule[-nrow(session_df)])
  switch_points <- which(rule_change)

  # Count perseverative errors: after a rule switch, if user keeps using old rule AND gets it wrong
  perseverative_errors <- 0
  for (idx in switch_points) {
    if (idx < total_trials) {
      previous_rule <- session_df$rule[idx - 1]
      for (j in idx:min(idx + 3, total_trials)) {  # optional: only check a few trials after switch
        if (!session_df$correct[j] && session_df$user_choice[j] == previous_rule) {
          perseverative_errors <- perseverative_errors + 1
        } else if (session_df$correct[j]) {
          break  # user has switched, so stop counting
        }
      }
    }
  }

  return(list(
    total_trials = total_trials,
    correct_trials = n_correct,
    accuracy = accuracy,
    rule_shifts = length(switch_points),
    perseverative_errors = perseverative_errors
  ))
}

