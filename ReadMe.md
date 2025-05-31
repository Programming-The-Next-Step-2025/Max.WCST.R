# Max.WCST.R

**Max.WCST.R** is an R package for simulating and analyzing the **Wisconsin Card Sorting Task (WCST)** — a classic neuropsychological test of cognitive flexibility and executive function.

## Features

- Generate randomized WCST card decks with hidden rule changes  
- Simulate task sessions with trial-by-trial feedback  
- Analyze performance (accuracy, perseverative errors, rule adaptation)  
- Launch an interactive **Shiny app** for live task execution  
- Export session results to a PDF report

## Installation

```r
# From GitHub (after development or publication):
devtools::install_github("your_username/Max.WCST.R")
Quick Start
r
Copy
Edit
library(Max.WCST.R)

# Generate deck and simulate session
deck <- generate_wcst_deck(20)
session <- simulate_wcst_session(20)

# Analyze results
analyze_wcst_results(session)
plot_wcst_performance(session)
Launch the Shiny App
r
Copy
Edit
run_wcst_app()
The app guides users through a WCST session, gives feedback, and generates a summary PDF.
