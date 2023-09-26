#' census UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_census_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("colony_census"))
  )
}

#' census Server Functions
#'
#' @noRd
mod_census_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$colony_census <- DT::renderDataTable({
      db_pull_flex(
        db,
        inv = NULL,
        group_str = "*",
        filtdatestart = Sys.Date() - (5*365),
        filtdateend = Sys.Date(), strain = NULL, sex = NULL
      ) %>%
        get_experiments_mice(db=db) %>%
        group_by(experiment_id, mouse_id) %>%
        filter(experiment_complete == 0) %>%
        filter(metric == "alive") %>%
        filter(all(value==1)) %>%
        select(investigator, mouse_id, cage, exp_grp_name, ear_tag, start_date, day) %>%
        filter(day==max(day)) %>% 
        distinct() %>%
        mutate(start_date=unjul(start_date)) %>%
        DT::datatable(      
          class = 'cell-border stripe',
          filter = 'top', extensions = 'Buttons',
          options=list(autoWidth = TRUE,
                       pageLength=500,
                       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    },
    server = TRUE,
    )
  })
}




census_demo  <- function() {
  ui <- fluidPage(
    mod_census_ui("mice")

  )
  server <- function(input, output, session) {
    exps <- mod_census_server("mice")
  }
  shinyApp(ui, server)
}
# census_demo()
