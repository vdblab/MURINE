#' plot_weights UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_weights_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("weight_plot"), width = plotWidth),

  )
}

#' plot_weights Server Functions
#'
#' @noRd
mod_plot_weights_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    tbl_wt_info <- reactive({
      validate(
        need(length(unique(tbl_exp_data()$experimental_group_id)) < 25, "Too many selected experimental groups; experiments must contain fewer than 25")
      )
      get_wt_tbl(tbl_exp_data())
    })
    wt_plts <- reactive({
      plot_weights(tbl_wt_info()$tbl_wt, tbl_wt_info()$tbl_means)
    })

    output$weight_plot <- renderPlot(wt_plts()$dot)
    return(list(tbl_wt_info=tbl_wt_info, wt_plts=wt_plts))
  })
}

weight_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    mod_plot_weights_ui("plot_weights_1"),
    plotOutput("tmp")
  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_list_exps_server("demo", exps_df=exps$exps_df)
    tbl_wt <- mod_plot_weights_server("plot_weights_1", exps$tbl_exp_data)
    output$tmp <- renderPlot( tbl_wt$wt_plts()$dot)
#    observe(print(str(tbl_wt$tbl_wt_info()$tbl_wt)))
  }
  shinyApp(ui, server)
}
# weight_demo()
