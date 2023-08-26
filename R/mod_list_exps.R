
mod_exp_dt_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("experimenttable"))
  )

}
mod_get_tbl_exps_server <- function(id, exps_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$experimenttable <- DT::renderDataTable(
      exps_df() %>% select(experiment_id, name, investigator) %>% distinct(),
      server = TRUE
    )
    output$tbl_selected <- reactive(input$`experiment-experimenttable_rows_selected`)
  })
}

# mod_update_exp_list_from_dt <- function(id, exps_df){
#   # this has some unpleasant "experiment"- hardcoding
#   # if row on experiment overview gets cllicked update selectInput
#     moduleServer( id, function(input, output, session){
#       ns <- session$ns
#       observe({
#         x <- reactive(input$`experiment-experimenttable_rows_selected`)
#         # Can use character(0) to remove all choices
#         if (is.null(x())) {
#           x <- function(){character(0)}
#         } else {
#           # Can also set the label and select items
#           updateSelectInput(session, input$`experiment-experiment`,
#                             choices = exps_df() %>% pull(name) %>% unique(),
#                             selected = all_experiment$name[x]
#           )
#         }
#       })
#     })
# }

mod_purpose_tbl_server <- function(id, tbl_exp_data){
  stopifnot(is.reactive(tbl_exp_data))
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # note this needs to happen outside the reactive otherwise it won't evaluate
    # and will silently fail
    tmp <- input$experiment
    reactive({
      validate(need(length(tmp) >= 1, "Must select one or more experiments"))
      tbl_exp_data() %>% select(name, purpose) %>% distinct()
    })
  })
}


# this is no longer needed as we output a table so
# we can send it to multiple UI elements,
#.rather than rendering separately for each
# mod_purpose_table_ui <- function(id){
#   ns = NS(id)
#   tagList(
#     tableOutput(ns("exp_purpose_table"))
#   )
# }


list_exps_demo  <- function() {
  ui <- fluidPage(
    mod_get_exp_data_ui("experiment"),

    tableOutput("purpose1"),
    #    tableOutput("experiment"),
    verbatimTextOutput("summ"),
    tableOutput("purpose2"),
    # tableOutput("thistbl"),
    # hr(),
    # tableOutput("thistbl2"),
    # hr(),
    mod_exp_dt_ui("experiment")

  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("experiment")
    # output$thistbl <- renderTable(exps$exps_df() %>% head(1))
    # output$thistbl2 <- renderTable(exps$tbl_exp_data() %>% head(1))
    mod_list_exps_server("experiment", exps_df=exps$exps_df)
    mod_get_tbl_exps_server("experiment",  exps_df=exps$exps_df)

    observe({
      x <- input$`experiment-experimenttable_rows_selected`
      if (is.null(x)) {
        x <- character(0)
      } else {
        (
          updateSelectInput(session, "experiment-experiment",
            choices = exps$exps_df() %>% pull(name) %>% unique(),
            selected = all_experiment$name[x]
          )
        )
      }
    })


    output$purpose2 <- output$purpose1 <- DT::renderTable( mod_purpose_tbl_server("experiment", tbl_exp_data=exps$tbl_exp_data)(),
                                    spacing="xs")
    output$summ <-  reactive({paste("nrow exps_df:", nrow(exps$exps_df()), "; nrow tbl_exp_data:", nrow(exps$tbl_exp_data() ))})

  }
  shinyApp(ui, server)
}

# list_exps_demo()

