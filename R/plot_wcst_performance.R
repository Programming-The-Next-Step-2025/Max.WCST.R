#' Plot WCST session performance
#'
#' Visualizes trial-by-trial correctness and rule switches.
#'
#' @param session_df A data frame from \code{simulate_wcst_session()}.
#'
#' @return A ggplot object showing trial-by-trial performance.
#' @export
#'
#' @examples
#' res <- simulate_wcst_session(30, seed = 123)
#' plot_wcst_performance(res)
plot_wcst_performance <- function(session_df) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install the ggplot2 package to use this function.")
  }

  library(ggplot2)

  if (!all(c("trial", "rule", "user_choice", "correct") %in% colnames(session_df))) {
    stop("session_df must contain columns: trial, rule, user_choice, correct")
  }

  # Detect rule changes
  session_df$rule_change <- c(FALSE, session_df$rule[-1] != session_df$rule[-nrow(session_df)])

  ggplot(session_df, aes(x = trial, y = as.numeric(correct))) +
    geom_point(aes(color = correct), size = 3, shape = 21, fill = NA) +
    scale_color_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen")) +
    geom_vline(data = subset(session_df, rule_change), aes(xintercept = trial),
               linetype = "dashed", color = "blue", alpha = 0.4) +
    scale_y_continuous(breaks = c(0, 1), labels = c("Incorrect", "Correct")) +
    labs(title = "WCST Trial Performance",
         x = "Trial",
         y = "Response Accuracy",
         color = "Correct") +
    theme_minimal()
}
