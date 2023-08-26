try_amend_local_db <- function(){
  required_vars <- c("MURINE_REDCAP_TOKEN", "MURINE_REDCAP_PROJECTID", "MURINE_REDCAP_URI")
  REDCAP_ACCESSIBLE <- TRUE
  for (required_var in required_vars){
    if (Sys.getenv(required_var) == ""){
      REDCAP_ACCESSIBLE = FALSE
      warning(paste("Env variable ", required_var, "must be set to access redcap; only loading legacy data"))
    }
  }
  if (REDCAP_ACCESSIBLE) {
    REDCAP_TOKEN <- Sys.getenv("REDCAP_TOKEN")
    REDCAP_URI <- Sys.getenv("REDCAP_URI")
    filename <- "data/db/tmp_vdb_mouse_app.db"
    ammend_db_from_redcap(
      token = Sys.getenv("MURINE_REDCAP_TOKEN"),
      api_uri = Sys.genenv("MURINE_REDCAP_URI"),
      core_db_filename = "data/db/vdb_mouse_app.db",
      live_db_filename = filename
    )
  } else {
    print("Warning! Only loading legacy data!")
    filename <- "data/db/vdb_mouse_app.db"
  }
  return(filename)
}



  try_amend_local_db_demo <- function(){
  dotenv::load_dot_env()
  try_amend_local_db()
}

redcap_pull_ui <- function(){
  output$repull_text <- renderUI({
    HTML(paste("<div style='color:gray;  white-space: normal' >", rvs$pull_time, "</div>"))
  })

}
redcap_pull_server <- function(){
}
redcap_pull_demo <- function(x){
  dotenv::load_dot_env()

}
