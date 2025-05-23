#' Simulate a WCST session
#'
#' Runs a simplified WCST session with rule shifts and feedback.
#'
#' @param n_trials Number of trials to simulate.
#' @param rules Sequence of sorting rules (e.g., c("color", "shape", "number")).
#'
#' @return A data frame with trial-by-trial results.
#' @export
#' @examples
#' simulate_wcst_session(12)
simulate_wcst_session <- function(n_trials = 30, rules = c("color", "shape", "number")) {
  deck <- generate_wcst_deck(n_trials)
  rule_index <- 1
  rule <- rules[rule_index]
  streak <- 0
  results <- data.frame()
  
  for (i in 1:n_trials) {
    card <- deck[i, ]
    user_choice <- sample(c("color", "shape", "number"), 1)
    correct <- score_wcst_trial(rule, as.list(card), user_choice)
    
    streak <- if (correct) streak + 1 else 0
    if (streak >= 6 && rule_index < length(rules)) {
      rule_index <- rule_index + 1
      rule <- rules[rule_index]
      streak <- 0
    }
    
    results <- rbind(results, data.frame(
      trial = i,
      rule = rule,
      user_choice = user_choice,
      correct = correct
    ))
  }
  
  return(results)
}
