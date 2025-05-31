#' Simulate a WCST session
#'
#' Simulates a Wisconsin Card Sorting Task (WCST) session with rule shifts and customizable agent strategies.
#' Can be used to simulate different cognitive profiles or behaviors.
#'
#' @param n_trials Number of trials to simulate (default = 30). First trial is always shown as reference only.
#' @param rules A character vector indicating the sequence of sorting rules (default = c("color", "shape", "number")).
#' @param deck Optional pre-generated WCST deck. If NULL, a new deck is generated.
#' @param seed Optional seed for reproducibility.
#' @param strategy A character string defining the agent strategy. One of:
#'   \itemize{
#'     \item \code{"perfect"}: always selects the correct rule.
#'     \item \code{"random"}: chooses randomly between color, shape, number.
#'     \item \code{"probabilistic"}: chooses correctly with probability \code{p_correct}.
#'     \item \code{"perseverative"}: sticks to the previous rule 60\% of the time.
#'   }
#' @param p_correct Probability of selecting the correct rule when \code{strategy = "probabilistic"} (default = 0.7).
#'
#' @return A data frame with trial-by-trial results, including:
#'   \itemize{
#'     \item \code{trial}: the trial number.
#'     \item \code{rule}: the correct rule used to score the trial.
#'     \item \code{user_choice}: the rule selected by the simulated agent.
#'     \item \code{correct}: whether the response was correct.
#'   }
#' @export
#'
#' @examples
#' simulate_wcst_session(n_trials = 20, strategy = "perfect")
#' simulate_wcst_session(n_trials = 20, strategy = "random")
#' simulate_wcst_session(n_trials = 20, strategy = "probabilistic", p_correct = 0.8)
#' simulate_wcst_session(n_trials = 20, strategy = "perseverative")
simulate_wcst_session <- function(n_trials = 30,
                                  rules = c("color", "shape", "number"),
                                  deck = NULL,
                                  seed = NULL,
                                  strategy = c("perfect", "random", "probabilistic", "perseverative"),
                                  p_correct = 0.7) {
  if (!is.null(seed)) set.seed(seed)
  strategy <- match.arg(strategy)

  if (is.null(deck)) {
    deck <- generate_wcst_deck(n_trials)
  }

  rule_index <- 1
  rule <- rules[rule_index]
  streak <- 0
  previous_rule <- rule

  results <- data.frame(
    trial = integer(n_trials - 1),
    rule = character(n_trials - 1),
    user_choice = character(n_trials - 1),
    correct = logical(n_trials - 1),
    stringsAsFactors = FALSE
  )

  for (i in 2:n_trials) {
    reference <- deck[i - 1, ]
    current <- deck[i, ]
    rule <- rules[rule_index]

    user_choice <- switch(strategy,
                          perfect = rule,
                          random = sample(c("color", "shape", "number"), 1),
                          probabilistic = {
                            if (runif(1) < p_correct) rule else sample(setdiff(c("color", "shape", "number"), rule), 1)
                          },
                          perseverative = {
                            if (i == 2) {
                              rule
                            } else if (runif(1) < 0.6) {
                              previous_rule
                            } else {
                              sample(c("color", "shape", "number"), 1)
                            }
                          }
    )

    # FIXED: correct argument order
    correct <- score_wcst_trial(current, reference, user_choice)

    if (correct) {
      streak <- streak + 1
      if (streak >= 6 && rule_index < length(rules)) {
        rule_index <- rule_index + 1
        streak <- 0
      }
    } else {
      streak <- 0
    }

    results[i - 1, ] <- list(i, rule, user_choice, correct)
    previous_rule <- rule
  }

  return(results)
}
