' Main Function
#'
#' @name MURINE
#' @rdname murine
#' @export
#' @import shinydashboard
#' @param db_path path to your sqlite db
#' @return A shiny dashboard for all your beautiful mouse data
MURINE <- function(db_path, ...){
  rvs <- reactiveValues(pull_time = Sys.time())
  if(missing(db_path)) {
    filename <<- system.file("extdata", "murine_data.db", package = "MURINE")
  } else{
    filename <<- db_path
  }
  DEBUG <- interactive()
  load_redcap_data(DEBUG=DEBUG, filename=filename)
  
  sqlite.driver <- DBI::dbDriver("SQLite")
  db <<- DBI::dbConnect(sqlite.driver, dbname = filename)
  
  load_nonreactive_global_data(filename = filename)
  
  plotWidth <<- 900
  

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "MURINE"),
    shinydashboard::dashboardSidebar(
      shinyjs::useShinyjs(),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Give Feedback!", icon = icon("comments"), href = "https://forms.office.com/r/HVKE1BkS7G"),
        shinydashboard::menuItem("Home", icon = icon("tachometer-alt"), tabName = "dashboard"),
        shinydashboard::menuItem("Survival Curves", icon = icon("chart-line"), tabName = "survivalCurves"),
        shinydashboard::menuItem("Weight Plots", icon = icon("weight"), tabName = "weightPlots"),
        shinydashboard::menuItem("Score Plots", icon = icon("check"), tabName = "scorePlots"),
        shinydashboard::menuItem("Data Cleaning", icon = icon("hands-wash"), tabName = "cleaning"),
        shinydashboard::menuItem("Query", icon = icon("table"), tabName = "queries"),
        shinydashboard::menuItem("Census", icon = icon("building"), tabName = "census")
      ),
      mod_get_exp_data_ui("experiments"),
      hr(),
      br(),
      br(),
      br(),
      br(),
      hr(),
      HTML(paste("<div style='color:gray;  white-space: normal' >Download data meeting above filtering criteria</div>")),
      downloadButton("downloadData", "CSV"),
      downloadButton("downloadDataPrism", "Prism"),
      downloadButton("report", "PDF"),
      hr(),
      br(),
      br(),
      br(),
      HTML(paste("<div style='color:gray;  white-space: normal' >Data is pulled from REDCap periodically; to refresh manually, click the button below. Should take 20-60 seconds.<br>Last update:</div>")),
      htmlOutput("repull_text"),
      actionButton("repull", "Refresh Local DB Now", icon = icon("sync"), style = "padding:4px; font-size:80%")
    ),
    shinydashboard::dashboardBody(
      tags$head(tags$style(HTML(".sidebar-menu>li>a { padding: 4px 5px 4px 15px; } section.sidebar .shiny-input-container {
    padding: 4px 15px 0px 15px;
    white-space: normal;
}"))),
      tabItems(
        tabItem(
          tabName = "dashboard",
          fluidRow(
            column(
              width = 12,
              mod_dygraph_ui("dyg")
            )
          ),
          HTML(paste("<div style='color:gray;  white-space: normal' >",
                     "Total number of Experiments: ", length(unique(all_experiment$experiment_id)),
                     "\nExperiments in need of Cleaning: ", nrow(experiments_with_known_issues), "</div>")),
          textOutput("experiment_stats"),
          fluidRow(
            column(
              width = 4,
              shinydashboard::box(
                title = "", width = NULL, status = "primary",
                div(style = "height:450px", mod_piegraph_ui("pie"))
              )
            ),
            column(
              width = 8,
              shinydashboard::box(
                title = textOutput("experiment_title"), width = NULL, status = "primary",
                div(style = "height:450px;overflow-x: scroll;overflow-y: scroll", mod_exp_dt_ui("experiments"))
              )
            )
          )
        ),
        tabItem(
          tabName = "survivalCurves",
          h2("Survival curves"),
          div(style = "height:100px;overflow-x: scroll;overflow-y: scroll", tableOutput("purpose_1")),
          plotOutput("survival_plot", width = "auto"),
          selectInput("stat_test",
                      label = "Statistic", multiple = FALSE, selectize = FALSE,selected = "Log-rank",
                      choices = c("Log-rank", "Cox Prop. Hazards", "Pairwise Log-rank")
          ),
          verbatimTextOutput("survival_plot_stats"),
          fluidRow(
            column(
              width = 3,
              uiOutput("death_checkbox"),
            ),
            column(
              width = 9,
              div(style = "height:450px;overflow-x: scroll;overflow-y: scroll", mod_surv_outcomes_ui("surv_outcomes"))
            )
          ),
        ),
        tabItem(
          tabName = "weightPlots",
          h2("Weight plots"),
          mod_plot_weights_ui("wp")
        ),
        tabItem(
          tabName = "scorePlots",
          div(style = "height:100px;overflow-x: scroll;overflow-y: scroll", tableOutput("purpose_2")),
          h2("Score plots"),
          mod_plot_scores_ui("experiments")
        ),
        tabItem(
          tabName = "cleaning",
          h3("Issues have been detected in the following datasets:"),
          div(style = "height:450px;overflow-x: scroll;overflow-y: scroll", mod_bad_exps_ui("problems")),

        ),
        tabItem(
          tabName = "queries",
          h2("Query the Mouse DB"),
          DT::dataTableOutput("querytab")
          #tableOutput("querytab"),
        ),
        tabItem(
          tabName = "census",
          h3("According to Redcap, the following mice are alive:"),
          div(style = "height:450px;overflow-x: scroll;overflow-y: scroll", mod_census_ui("census")),
        )
      )
    )
  )


  server <- function(input, output, session) {

    # @@@@@@@@@@@@@@@@@@
    # main page
    # @@@@@@@@@@@@@@@@@@
    exps <- mod_get_exp_server("experiments")

    observeEvent(input$reset, {
      shinyjs::reset("myfilters")
    })
    mod_dygraph_server("dyg", exps$tbl_exp_data)
    mod_piegraph_server("pie", exps$tbl_exp_data)
    mod_list_exps_server("experiments", exps$exps_df)

    output$experiment_title <- renderText(str_glue("Experiments matching criteria: {length(unique(exps$tbl_exp_data()$name))}"))
    mod_get_tbl_exps_server("experiments",  exps_df=exps$exps_df)
    # TODO: get this to work: mod_update_exp_list_from_dt("experiments", exps_df=exps$exps_df)
    observe({
      x <- input$`experiments-experimenttable_rows_selected`
      if (is.null(x)) {
        x <- character(0)
      } else {
        (
          updateSelectInput(session, "experiments-experiment",
                            choices = exps$exps_df() %>% pull(name) %>% unique(),
                            selected = all_experiment$name[x]
          )
        )
      }
    })
    output$purpose_1 <- output$purpose_2 <- renderTable(
      mod_purpose_tbl_server("experiments", tbl_exp_data=exps$tbl_exp_data)(),
      spacing="xs")

    mod_bad_exps_server("problems",  tbl_exp_data=exps$tbl_exp_data)



    # this uses the experiments imput so we have to use the same namespace
    tbl_sc <- mod_surv_data_server("experiments", exps$tbl_exp_data)
    mod_surv_outcomes_server("surv_outcomes", tbl_sc)





    # grabs various info needed for plot
    plot_max <- reactive(
      ifelse(
        any(tbl_sc()$event_type == 0),
        max(tbl_sc()$event_day) + 1,
        max(tbl_sc()$event_day, na.rm = TRUE)
      )
    )

    tbl_grps <- reactive(distinct(tbl_sc(), longlab))
    n_groups <- reactive(distinct(tbl_sc(), longlab) %>% nrow())
    # generates a "survival object" which can then be plotted by ggsurvplot
    surv_obj <- reactive({
      surv_make_surv_obj(
        ev_d = tbl_sc()$event_day,
        ev_t = tbl_sc()$event_type,
        z = tbl_sc()$longlab,
        n_experiments = length(input$`experiments-experiment`),
        stat_test=input$stat_test
      )
    }
    )

    priz_df <- reactive(
      list(
        "survival" = tbl_sc() %>% select(mouse_id, event_day,  longlab, event_type)  %>%
          tidyr::pivot_wider(names_from = longlab, values_from=event_type) %>%
          select(-mouse_id) %>% rename("day"="event_day"),
        "weight" =bind_rows(
          tbl_wt$tbl_wt_info()$tbl_wt %>%  select(exp_grp_name, mouse_id, baseline_wt)  %>% distinct() %>% rename(weight=baseline_wt) %>% mutate(day=-1),
          tbl_wt$tbl_wt_info()$tbl_wt %>%  select(exp_grp_name, mouse_id, day, weight)
        ) %>%
          tidyr::pivot_wider(names_from=c(exp_grp_name, mouse_id), names_sep = " - mouse", values_from = "weight") %>%
          arrange(day)
      )
    )

    output$downloadDataPrism <- downloadHandler(
      filename = function() {
        paste(input$`experiments-experiment`, ".pzfx", sep = "")
      },
      content = function(file) {
        pzfx::write_pzfx(priz_df() , file, row_names = FALSE, x_col = "day")
      }
    )
    surv_plot <- reactive({
      validate(need(length(input$`experiments-experiment`) > 0, "Select one or more experiments"))
      plot_surv2(
        surv_obj()[[1]],
        n_groups = n_groups(),
        title = str_wrap(paste(collapse = ",", sep = "", basename(input$`experiments-experiment`)), 50),
        plot_min = 0,
        leg_lab = t(tbl_grps()),
        plot_max = max(tbl_sc()$day),
        jitter = TRUE
      )
    })
    output$survival_plot <- renderPlot(surv_plot())
    output$survival_plot_stats <- renderText(sep = "\n", utils::capture.output(surv_obj()[[2]]))
    #
    # @@@@@@@@@@@@@@@@@@
    # weight plots
    # @@@@@@@@@@@@@@@@@@

    tbl_wt <- mod_plot_weights_server("wp", exps$tbl_exp_data)
    mod_plot_scores_server("experiments", exps$tbl_exp_data)


    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$`experiments-experiment`, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(exps$tbl_exp_data(), file, row.names = FALSE)
      }
    )
    output$querytab <- DT::renderDataTable(
      exps$tbl_exp_data(),
      options = list(
        pageLength = 35,
        buttons = c("copy", "csv", "excel"),
        autoWidth = TRUE,
        scrollX = TRUE,
        width = "100%"
        # initComplete = I("function(settings, json) {alert('Done.');}")
      )
    )
    mod_census_server("census", exps$tbl_exp_data)
    # output$querytab <- renderTable(
    #   exps$tbl_exp_data()
    # )
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          df = exps$tbl_exp_data(),
          wt_plot = tbl_wt$wt_plts()$dot,
          surv_plot = surv_plot()
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

    observeEvent(input$repull, {
      REDCAP_TOKEN <- Sys.getenv("MURINE_REDCAP_TOKEN")
      REDCAP_URI <- Sys.getenv("MURINE_REDCAP_URI")
      ammend_db_from_redcap(
        token = REDCAP_TOKEN,
        api_uri = REDCAP_URI,
        core_db_filename = filename,
        live_db_filename = "tmp_murine_data.db"
      )
      rvs$pull_time <- Sys.time()
    })

    output$repull_text <- renderUI({
      HTML(paste("<div style='color:gray;  white-space: normal' >", rvs$pull_time, "</div>"))
    })


    output$death_checkbox <- renderUI({
      check_choices <- unique(exps$tbl_exp_data()$notes)
      check_choices <- check_choices[!check_choices == ""]
      check_choices <- check_choices[!is.na(check_choices)]
      check_choices <- unique(str_trim(unlist(strsplit(check_choices, split = ",", fixed = TRUE))))
      checkboxGroupInput("death_checkbox", "Exclude these Causes of Death", choices = check_choices)
    })
  }

  if (DEBUG) {
    shinyApp(ui, server, options = list(display.mode = "showcase"), ...)
  } else {
    shinyApp(ui, server, ...)
  }
}
