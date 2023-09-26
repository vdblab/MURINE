#' get_exp_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_get_exp_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      id = "myfilters",
      hr(),
      uiOutput(ns("experiment")),
      hr(),
      checkboxInput(ns("is_ongoing"), "Only On-going Experiments", FALSE),
      checkboxInput(ns("allow_legacy"), "Include Legacy Experiments", TRUE),
      dateRangeInput(ns("daterange1"), "Date range",
                     start = min(unjul(all_experiment$start_date)),
                     startview = "year",
                     end = Sys.Date()
      ),
      selectInput(ns("investigator"),
                  label = "Select investigator", multiple = TRUE, selectize = TRUE,
                  choices = unique(all_experiment$investigator)
      ),
      selectInput(ns("exp_groups"),
                  label = "Select Groups", multiple = TRUE, selectize = TRUE,
                  choices = c(distinct(all_experimental_group, description) %>% as.list())
      ),
      selectInput(
        width = "50%",
        ns("organism_line"),
        label = "Strain", multiple = TRUE, selectize = TRUE,
        choices = unique(all_experimental_group$organism_line)
      ),
      selectInput(
        width = "50%",
        ns("organism_sex"),
        label = "Sex", multiple = TRUE, selectize = TRUE,
        choices = unique(all_experimental_group$organism_sex)
      ),
      textInput(ns("purpose_str"), label = "Search by Experiment Purpose", width = "100%", value = "*"),
      textInput(ns("group_str"), label = "Search by Group Name", width = "100%", value = "*"),
      textInput(ns("treat_str"), label = "Search by Treatment Name", width = "100%", placeholder = "try out a regex like [bB][aA][lL][bB]"),
      actionButton(ns("reset"), "Reset filters"),
      hr(),
    )
  )


}

#' get_exp_data Server Functions
#'
#' @noRd
mod_get_exp_server <- function(id){
  moduleServer( id, function(input, output, session){
    tmp <- reactive(db_pull_flex(
      db,
      exp = NULL,
      inv = input$investigator,
      groups = input$exp_groups,
      group_str = input$group_str,
      filtdatestart = input$daterange1[1],
      filtdateend = input$daterange1[2],
      treat_str = input$treat_str,
      purpose_str = input$purpose_str,
      strain = input$organism_line,
      sex = input$organism_sex,
      is_ongoing = input$is_ongoing,
      allow_legacy = input$allow_legacy

    ))
    skip_filter_exps <- reactive(length(input$experiment) == 0)
    list("exps_df" = tmp,
         "tbl_exp_data" = reactive(get_experiments_mice(db=db,
           tmp() %>%
             filter(skip_filter_exps() | name %in% input$experiment))  %>%
             filter(score_date >= str_to_julian(input$daterange1[1]) & score_date <= str_to_julian(input$daterange1[2])))
    )
  })
}

#' list_exps Server Functions
#'
#' @noRd

mod_list_exps_server <- function(id, exps_df){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$experiment <- renderUI({
      # exp_list <- filter(all_experiment, start_date >= input$daterange1[1] & start_date <= input$daterange1[2])$name
      selectInput(ns("experiment"), "Select experiment", unique(exps_df()$name), selectize = TRUE, multiple = TRUE, selected = 2)
    })
  })
}


exp_data_demo  <- function() {
  ui <- dashboardPage(
    dashboardHeader(title = "MURINE"),
    dashboardSidebar(
      sidebarMenu(
        mod_get_exp_data_ui("demo")
      )
    ),
    dashboardBody(
      verbatimTextOutput("summ"),
    tableOutput("thistbl"),
    hr(),
    tableOutput("thistbl2")
    )
  )
  server <- function(input, output, session) {
    exps <- mod_get_exp_server("demo")
    mod_list_exps_server("demo", exps_df = exps$exps_df)
    output$thistbl <- renderTable(exps$exps_df() %>% head())
    output$thistbl2 <- renderTable(exps$tbl_exp_data() %>% head())
    output$summ <-  reactive({paste("nrow exps_df:", nrow(exps$exps_df()), "; nrow tbl_exp_data:", nrow(exps$tbl_exp_data() ))})
  }
  shinyApp(ui, server)
}
# dotenv::load_dot_env()
# assign_dev_globals()
# exp_data_demo()
