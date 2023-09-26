#' data_qc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bad_exps_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("bad_experiments_table"))
  )
}

#' data_qc Server Functions
#'
#' @noRd
mod_bad_exps_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$bad_experiments_table <- DT::renderDataTable({
      if(is.null(tbl_exp_data())){
        experiments_with_known_issues
      } else{
        tbl_exp_data() %>% group_by(experiment_id, name, investigator) %>%
          summarize(issue = case_when(
            any(metric == "death_date" & value <= unique(start_date)) ~ "death_date preceeds start date",
            any(metric == "weight" & value > 50 ) ~ "mouse weight exceeds 50g",
            is.na(start_date) ~ "missing start date",
            TRUE ~ "no issues")
          ) %>%
          filter(issue != "no issues")
      }
    },
    server = TRUE,
    )
  })
}




qc_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    hr(),
    mod_bad_exps_ui("problems")

  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_bad_exps_server("problems", exps$tbl_exp_data)
  }
  shinyApp(ui, server)
}
# qc_demo()
