
#' surv_data Server Functions
#'
#' @noRd
mod_surv_data_server <- function(id, tbl_exp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # # table of death data for final curve (possibly will make this a function soon)
    tbl_sc <- reactive({
      validate(need(length(input$experiment) >= 1,  "No experiment(s) selected"))
      tmp <- get_sc_tbl(tbl_exp_data())
#      print(input$death_checkbox)
      checkbox_exclusion_grep <- paste0(collapse = "|", input$death_checkbox)
#      print(checkbox_exclusion_grep)
      if (checkbox_exclusion_grep != "") {
        return(tmp %>% filter(!grepl(checkbox_exclusion_grep, cause_of_death)))
      } else {
        return(tmp)
      }
    })
  })
}


#' surv_outcomes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_surv_outcomes_ui <- function(id){
  ns <- NS(id)
  tagList(
    tableOutput(ns("survival_outcomes_table"))
  )
}

#' surv_outcomes Server Functions
#'
#' @noRd
mod_surv_outcomes_server <- function(id, tbl_sc){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$survival_outcomes_table <- renderTable({
      tmp_surv_outcomes <- tbl_sc() %>% select(longlab, ear_tag, cause_of_death) %>%
        filter(cause_of_death != "") %>%
        rename(
          `Group/Experiment` = longlab,
          `Ear Tag` = ear_tag,
          `Cause of Death` = cause_of_death
        )
      if(nrow(tmp_surv_outcomes) == 0){
        tmp_surv_outcomes <-tibble(
          `Group/Experiment` = "",
          `Ear Tag` = "",
          `Cause of Death` = "no causes of death found"
        )
      }
      tmp_surv_outcomes
    }
    )

  })
}
surv_data_demo  <- function() {
  library(shiny)
  ui <- fluidPage(
    mod_get_exp_data_ui("demo"),
    tableOutput("thistbl"),
    mod_surv_outcomes_ui("surv_data_1")
  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_list_exps_server("demo", exps_df=exps$exps_df)
    tbl_sc <- mod_surv_data_server("demo", exps$tbl_exp_data)
    output$thistbl <- renderTable(head(tbl_sc() ))
    mod_surv_outcomes_server("surv_data_1", tbl_sc)
  }
  shinyApp(ui, server)
}
# surv_data_demo()

