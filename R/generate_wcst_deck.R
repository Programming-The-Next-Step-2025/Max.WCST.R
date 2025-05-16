#' Generate a WCST-style deck
#'
#' Creates a randomized data frame of cards for use in the Wisconsin Card Sorting Task.
#'
#' @param n_cards Number of cards to generate.
#'
#' @return A data frame with columns: color, shape, number
#' @export
#' @examples
#' generate_wcst_deck(10)
generate_wcst_deck <- function(n_cards = 64) {
  colors <- c("red", "green", "blue", "yellow")
  shapes <- c("circle", "triangle", "star", "cross")
  numbers <- 1:4
  
  deck <- data.frame(
    color = sample(colors, n_cards, replace = TRUE),
    shape = sample(shapes, n_cards, replace = TRUE),
    number = sample(numbers, n_cards, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  return(deck)
}
