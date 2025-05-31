#' Generate a WCST-style deck with rule changes
#'
#' Creates a data frame of cards for the Wisconsin Card Sorting Task.
#' Each card matches the previous on exactly one feature, and the matching rule changes
#' every 4 to 8 trials.
#'
#' @param n_cards Number of cards to generate (must be ≥ 2).
#'
#' @return A data frame with columns: color, shape, number, and rule (the feature matched on each trial).
#' @export
#'
#' @examples
#' generate_wcst_deck(20)
generate_wcst_deck <- function(n_cards = 64) {
  if (n_cards < 2) stop("Need at least 2 cards to create a WCST deck.")

  colors <- c("red", "green", "blue", "yellow")
  shapes <- c("circle", "triangle", "star", "cross")
  numbers <- 1:4

  valid_features <- c("color", "shape", "number")
  rules <- character(n_cards)

  # Rule change points (random gaps of 4–8 trials)
  change_points <- c()
  i <- 2
  while (i <= n_cards) {
    change_points <- c(change_points, i)
    i <- i + sample(4:8, 1)
  }

  # Preallocate deck
  deck <- data.frame(
    color = character(n_cards),
    shape = character(n_cards),
    number = integer(n_cards),
    rule = character(n_cards),
    stringsAsFactors = FALSE
  )

  # First card is random
  deck[1, ] <- list(
    color = sample(colors, 1),
    shape = sample(shapes, 1),
    number = sample(numbers, 1),
    rule = NA
  )

  current_rule <- sample(valid_features, 1)

  for (i in 2:n_cards) {
    prev_card <- deck[i - 1, ]

    # Change rule if this is a change point
    if (i %in% change_points) {
      current_rule <- sample(setdiff(valid_features, current_rule), 1)
    }
    rules[i] <- current_rule

    # Start building new card
    new_card <- list(
      color = sample(colors, 1),
      shape = sample(shapes, 1),
      number = sample(numbers, 1)
    )

    # Enforce match on current_rule
    new_card[[current_rule]] <- prev_card[[current_rule]]

    # Force other features to differ
    other_features <- setdiff(valid_features, current_rule)
    for (feat in other_features) {
      new_card[[feat]] <- sample(
        setdiff(switch(feat,
                       color = colors,
                       shape = shapes,
                       number = numbers),
                prev_card[[feat]]
        ),
        1
      )
    }

    deck[i, c("color", "shape", "number")] <- new_card
    deck$rule[i] <- current_rule
  }

  return(deck)
}
