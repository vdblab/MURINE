#' pie plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_piegraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("pie_investigator"))
  )
}

#' pie Server Functions
#'
#' @noRd
mod_piegraph_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$pie_investigator <- plotly::renderPlotly({
      tbl_exp_data() %>% select(investigator, mouse_id) %>% distinct() %>%
        group_by(investigator) %>%
        tally() %>%
        plotly::plot_ly(
          labels = ~investigator, values = ~n,
          textposition = "inside",
          textinfo = "label+percent",
          showlegend = FALSE
        ) %>%
        plotly::add_pie(hole = 0.5) %>%
        plotly::layout(
          title = "Mice per investigator",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    })
  })
}
piegraph_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    mod_piegraph_ui("dygraph_1")

  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_piegraph_server("dygraph_1", exps$tbl_exp_data)
  }
  shinyApp(ui, server)
}

# piegraph_demo()
