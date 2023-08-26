#' plot_scores UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_scores_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("score_plot_scores"), width = plotWidth),
    plotOutput(ns("score_plot_alive"), width = plotWidth, height = plotWidth*.2)

  )
}

#' plot_scores Server Functions
#'
#' @noRd
mod_plot_scores_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    sc_plot_data <- reactive({
      validate(
        need(length(input$experiment) == 1, "Score plots can only be shown for single experiments")
      )
      validate(
        need(any(grepl("fur|posture|activity|skin", tbl_exp_data()$metric)), "Selected experiment has no scores recorded")
      )
      plot_scores(tbl_exp_data())
    })
    output$score_plot_scores <- renderPlot(sc_plot_data()$summary)
    output$score_plot_alive <- renderPlot(sc_plot_data()$surviving)

  })
}


score_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    mod_list_exps_ui("demo"),
    mod_plot_scores_ui("demo")
  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_list_exps_server("demo", exps_df=exps$exps_df)
    mod_plot_scores_server("demo", exps$tbl_exp_data)
  }
  shinyApp(ui, server)
}
# score_demo()


## To be copied in the UI
# mod_plot_scores_ui("plot_scores_1")

## To be copied in the server
# mod_plot_scores_server("plot_scores_1")
