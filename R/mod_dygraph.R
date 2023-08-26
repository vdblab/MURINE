#' dygraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dygraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    dygraphOutput(ns("dygraph"))
  )
}

#' dygraph Server Functions
#'
#' @noRd
mod_dygraph_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # this section takes the date_range from input and plots the number of mice within that range
    output$dygraph <- renderDygraph({
      # # print(nrow(tbl_exp_data()))
      # timeline_df <- as.data.frame(
      #   # retain live mice
      #   left_join(all_mouse_observation %>% filter(metric_id == 7 & value == 1),
      #     all_observation,
      #     by = "observation_id"
      #   ) %>%
      #     filter(date >= str_to_julian(input$daterange1[1]) & date <= str_to_julian(input$daterange1[2])) %>%
      #     group_by(date) %>%
      #     tally() %>%
      #     mutate(date = unjul(date)) %>%
      #     column_to_rownames("date")
      # )
      #
      dygraph(tbl_exp_data() %>% filter(metric == "alive" & value == 1) %>% rename(date = score_date) %>%
                group_by(date) %>%
                tally() %>% mutate(date = unjul(date)) %>% column_to_rownames("date"), main = "Mouse transplants overview") %>%
        dyOptions(stackedGraph = TRUE) %>%
        dyOptions(fillGraph = TRUE, fillAlpha = 0.4)
    })

  })
}
dygraph_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    # tableOutput("thistbl"),
    # hr(),
    # tableOutput("thistbl2"),
    # hr(),
    mod_dygraph_ui("dygraph_1")

  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    # output$thistbl <- renderTable(exps$exps_df() %>% head(1))
    # output$thistbl2 <- renderTable(exps$tbl_exp_data() %>% head(1))
    mod_dygraph_server("dygraph_1", exps$tbl_exp_data)
  }
  shinyApp(ui, server)
}

# dygraph_demo()
