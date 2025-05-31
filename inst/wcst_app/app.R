library(shiny)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(Max.WCST.R)

ui <- fluidPage(
  titlePanel("Wisconsin Card Sorting Task (WCST)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_trials", "Number of Trials:", value = 30, min = 10, max = 100, step = 1),
      actionButton("start_btn", "Start Session"),
      br(), br(),
      uiOutput("response_ui")
    ),
    mainPanel(
      h4("Comparison Card"),
      tableOutput("comparison_card"),
      h4("Current Card"),
      tableOutput("current_card"),
      verbatimTextOutput("feedback"),
      conditionalPanel(
        condition = "output.showResults == true",
        h4("Performance Summary"),
        plotOutput("summary_plot"),
        verbatimTextOutput("analysis"),
        downloadButton("download_pdf", "Download PDF Report")
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    deck = NULL,
    trial = 1,
    streak = 0,
    results = data.frame(),
    complete = FALSE,
    started = FALSE
  )

  observeEvent(input$start_btn, {
    rv$deck <- generate_wcst_deck(input$num_trials)
    rv$trial <- 1
    rv$streak <- 0
    rv$results <- data.frame()
    rv$complete <- FALSE
    rv$started <- TRUE
  })

  observeEvent(input$next_btn, {
    req(rv$started)
    if (rv$complete || rv$trial > input$num_trials) return()

    if (rv$trial == 1) {
      rv$trial <- 2  # Skip to first comparison
      return()
    }

    current <- rv$deck[rv$trial, ]
    reference <- rv$deck[rv$trial - 1, ]
    user_choice <- input$user_choice

    correct <- score_wcst_trial(current, reference, user_choice)

    rv$results <- rbind(rv$results, data.frame(
      trial = rv$trial,
      rule = rv$deck$rule[rv$trial],
      user_choice = user_choice,
      correct = correct
    ))

    rv$streak <- if (correct) rv$streak + 1 else 0
    rv$trial <- rv$trial + 1

    if (rv$trial > input$num_trials) {
      rv$complete <- TRUE
    }
  })

  output$response_ui <- renderUI({
    req(rv$started)
    tagList(
      selectInput("user_choice", "Select the matching feature:", choices = c("color", "shape", "number")),
      actionButton("next_btn", "Submit Response")
    )
  })

  output$comparison_card <- renderTable({
    req(rv$started)
    if (rv$trial == 1) {
      rv$deck[1, c("color", "shape", "number"), drop = FALSE]
    } else if (rv$trial <= input$num_trials) {
      rv$deck[rv$trial - 1, c("color", "shape", "number"), drop = FALSE]
    } else {
      NULL
    }
  })

  output$current_card <- renderTable({
    req(rv$started)
    if (rv$trial >= 2 && rv$trial <= input$num_trials) {
      rv$deck[rv$trial, c("color", "shape", "number"), drop = FALSE]
    } else {
      NULL
    }
  })

  output$feedback <- renderText({
    req(rv$started)
    if (rv$trial > 2) {
      last <- tail(rv$results, 1)
      if (last$correct) "Correct!" else "Incorrect!"
    } else if (rv$trial == 2) {
      "Trial begins. Make your first selection."
    } else {
      "This is your reference card. Press Submit to begin."
    }
  })

  output$summary_plot <- renderPlot({
    req(rv$complete)
    plot_wcst_performance(rv$results)
  })

  output$analysis <- renderPrint({
    req(rv$complete)
    analyze_wcst_results(rv$results)
  })

  output$showResults <- reactive({ rv$complete })
  outputOptions(output, "showResults", suspendWhenHidden = FALSE)

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("wcst_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tmp <- tempfile(fileext = ".pdf")
      rmarkdown::render(
        input = "wcst_report.Rmd",
        output_file = tmp,
        params = list(
          results = rv$results,
          summary = analyze_wcst_results(rv$results),
          plot = plot_wcst_performance(rv$results)
        ),
        envir = new.env(parent = globalenv())
      )
      file.copy(tmp, file)
    }
  )
}

shinyApp(ui, server)
