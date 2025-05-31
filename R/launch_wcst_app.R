#' Launch the WCST Shiny App
#'
#' Opens the Wisconsin Card Sorting Task app in the default browser.
#'
#' @export
launch_wcst_app <- function() {
  app_dir <- system.file("wcst_app", package = "Max.WCST.R")
  if (app_dir == "") {
    stop("Could not find wcst_app directory. Try reinstalling the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
