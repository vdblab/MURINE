#' Ammend legacy data with fresh pull from REDcap
#'
#' @param token REDcap API token.
#' @param api_uri REDcap API uri
#' @param core_db_filename sqlite db with legacy data
#' @param live_db_filename sqlite db to be created, containing legacy data and live
#' @return The number of records added to db
#' @examples
#' ammend_db_from_redcap(token = "12345ABCDE")
ammend_db_from_redcap <- function(token,
                                  api_uri = "https://redcap.mskcc.org/api/",
                                  core_db_filename = "data/db/vdb_mouse_app.db",
                                  live_db_filename = "data/db/tmp_vdb_mouse_app.db") {

  # if(file.exists(live_db_filename)){
  #   file.remove(live_db_filename)
  # }
  file.copy(from = core_db_filename, to = live_db_filename, overwrite = TRUE)
  # pull from redcap
  dict_and_data <- get_all_data_from_redcap(token, api_uri)
  keys <- dict_and_data$keys
  DF <- dict_and_data$data

  # split up the data into diffent forms
  dat <- tidy_up_redcap(keys, DF)


  # connect to local
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = live_db_filename)
  DBI::dbExecute(conn = con, "PRAGMA foreign_keys=ON")
  make_redcap_and_db_match(dat, con)
  DBI::dbDisconnect(con)
}


get_all_data_from_redcap <- function(token, api_uri) {

  # if strings are good enough for REDCap, they are good enough for us
  # ...
  #  ::barf emoji::
  col_types <- readr::cols(.default = readr::col_character())

  print("Loading dictionary")
  # keys <- read.csv(sep=",", "../murine-backend/VDBMouseAppMURINEDataCollectio_DataDictionary_2021-12-20.csv")
  keys <- REDCapR::redcap_metadata_read(
    redcap_uri  = api_uri,
    token       = token,
  )$data
  print("Loading REDcap data")
  DF <-
    REDCapR::redcap_read(
      col_types = col_types,
      redcap_uri = api_uri,
      token = token,
    )$data %>%
    mutate(form = ifelse(is.na(redcap_repeat_instrument), "protocol", redcap_repeat_instrument))
  return(list(keys = keys, data = DF))
}

convert_punchid <- function(x) {
  case_when(
    x == 3 ~ 2,
    x == 30 ~ 4,
    x == 10 ~ 3,
    x == 1 ~ 1,
    x == 0 ~ 0,
    TRUE ~ x
  )
}
tidy_up_redcap <- function(keys, DF) {
  ymd <- lubridate::ymd
  instrument_cols <- keys %>%
    select(field_name, form_name) %>%
    distinct()
  # use the data dictionary to split apart data-dump into tables for each instrumnet/form
  forms <- unique(instrument_cols$form_name)
  names(forms) <- unique(instrument_cols$form_name)

  keeper_cols <- c("redcap_protocol_id", "redcap_repeat_instrument", "redcap_repeat_instance")
  form_ids <- DF %>% select(all_of(keeper_cols))

  # no idea how these arise
  colnames(DF) <- gsub("___1", "", colnames(DF))
  split_dfs <- purrr:::map(
    forms, function(x) {
      # get the columns used by this form
      these_instr_cols <- instrument_cols %>%
        filter(form_name == x) %>%
        pull(field_name)
      # add in the auto-generated <form>_complete column
      these_instr_cols <- c(these_instr_cols, paste0(x, "_complete"))

      # get rid of missing ones.  Not sure what the story is, why there are ~1492 variables in the data byt 1175 in the dict, Types? choices? meh
      these_instr_cols <- these_instr_cols[these_instr_cols %in% colnames(DF)]
      # get rid of ones already in `keeper_cols`.  This really only happens in the protocol form
      these_instr_cols <- these_instr_cols[!these_instr_cols %in% keeper_cols]

      thisdf <- DF %>% select(all_of(keeper_cols), all_of(these_instr_cols))
      if (x %in% unique(DF$redcap_repeat_instrument)) {
        thisdf <- thisdf %>% filter(redcap_repeat_instrument == x)
      } else {
        thisdf <- thisdf %>% filter(rowSums(is.na(.) | . == "") != length(these_instr_cols))
      }
      # time for some wide-to-tall magic.  Get all the bases without numeric identifiers
      col_prefixes <- gsub("(.*?)(_\\d.*)", "\\1", colnames(thisdf))
      # get the ones in need of pivotting
      col_prefixes_for_pivots <- names(table(col_prefixes)[table(col_prefixes) != 1])

      # now pivot
      if (x %in% c("score_and_weights_sw", "autopsy_sheet")) {
        if (length(col_prefixes_for_pivots) != 0) {
          thisdf <- thisdf %>%
            tidyr::pivot_longer(cols = starts_with(col_prefixes_for_pivots)) %>%
            mutate(
              colbase = gsub("(.*?)_(\\d*)_(\\d*)", "\\1", name),
              groupid = gsub("(.*?)_(\\d*)", "\\2", gsub("(.*?)_(\\d*)_(\\d*)", "\\1_\\2", name)),
              rawmouseid = as.numeric(gsub("(.*?)_(\\d*)_(\\d*)", "\\3", name))
            ) %>%
            mutate(mouseid = as.character(
              ifelse(colbase %in% c("as_mouse_eoe", "as_mouse_excluded"), convert_punchid(rawmouseid), rawmouseid)
            )) %>%
            # mutate(mouseid = convert_mouseid_to_punch(rawmouseid)) %>%
            filter(!(is.na(value) | value == "")) %>%
            distinct() # %>% mutate(rn=row_number())
        }
      }
      thisdf
    }
  )
  split_dfs$protocol <- full_join(
    split_dfs$protocol, split_dfs$injections,
    by = c("redcap_protocol_id", "redcap_repeat_instrument", "redcap_repeat_instance")
  )
  split_dfs$score_and_weights_sw$value <- as.numeric(split_dfs$score_and_weights_sw$value)

  # View(split_dfs$protocol)
  # View(split_dfs$groups)
  # View(split_dfs$antibiotics)
  # View(split_dfs$score_and_weights_sw)
  # View(split_dfs$protocol)



  # identify forms filled out without a separate initial weight by pulling out those where
  # the earliest sheet has both initial and that score sheets weight.  In these cases, we select only the sv_mouse_iw
  # and change the sc_date to the experiment_start_date minus 1.  the minus one is because in some cases the start date and the date of the first scoring is the same (eg #13)
  fixed_initial_weights <- split_dfs$score_and_weights_sw %>%
    filter(colbase %in% c("sc_mouse_iw", "sc_mouse_weight")) %>%
    select(redcap_protocol_id, mouseid, sc_date, colbase, value, groupid) %>%
    group_by(redcap_protocol_id) %>%
    filter(sc_date == min(sc_date)) %>%
    filter(length(unique(colbase)) == 2) %>%
    filter(colbase == "sc_mouse_iw") %>%
    left_join(split_dfs$protocol %>% select(redcap_protocol_id, experiment_start_date)) %>%
    select(-sc_date) %>%
    mutate(sc_date = as.character(ymd(experiment_start_date) - 1)) %>%
    select(-experiment_start_date) %>%
    mutate(colbase = "sc_mouse_weight") %>%
    ungroup() # %>% filter(redcap_protocol_id == 13)

  # turn the initial weights into normal weight observations, filter our ones we had to fix above, and bind in the fixed ones
  initial_weights <- split_dfs$score_and_weights_sw %>%
    filter(colbase %in% c("sc_mouse_iw", "sc_mouse_weight")) %>%
    select(redcap_protocol_id, mouseid, sc_date, colbase, value, groupid) %>%
    group_by(redcap_protocol_id) %>%
    filter(colbase == "sc_mouse_iw" & sc_date == min(sc_date)) %>%
    mutate(colbase = "sc_mouse_weight") %>%
    ungroup() %>%
    filter(!redcap_protocol_id %in% fixed_initial_weights$redcap_protocol_id) %>%
    bind_rows(fixed_initial_weights) # %>% filter(redcap_protocol_id == 13)

  # pull in data from dropdowns
  make_key_val_pair <- function(x) {
    x <- data.frame(raw = str_split(string = x, pattern = " \\| ")[[1]])
    x$id <- gsub("^(\\d+), .*", "\\1", x$raw) # not a numeic! gotcha!
    x$val <- gsub("^(\\d+), (.*)", "\\2", x$raw)
    x$raw <- NULL
    x
  }
  people <- keys[keys$field_name == "investigator_id", ] %>%
    pull(select_choices_or_calculations) %>%
    make_key_val_pair() %>%
    rename(name = val)

  strains <- keys[keys$field_name == "host_strain", ] %>%
    pull(select_choices_or_calculations) %>%
    make_key_val_pair() %>%
    rename(strain = val)


  dat_pre <- left_join(
    split_dfs$protocol %>%
      rename(repeat_number = `repeat`) %>%
      select(redcap_protocol_id, experiment_name, investigator_id, scoring_tech, repeat_number, experiment_start_date, hypothesis, ends_with("_date")) %>%
      mutate(transplant_date = ymd(ifelse(!is.na(injections_tcell_date), injections_tcell_date, experiment_start_date))) %>%
      select(redcap_protocol_id, experiment_name, investigator_id, scoring_tech, repeat_number, experiment_start_date, hypothesis, transplant_date) %>%
      left_join(people, by = c("investigator_id" = "id")) %>% rename(investigator = name) %>%
      left_join(people, by = c("scoring_tech" = "id")) %>% select(-scoring_tech) %>% rename(scoring_tech = name) %>%
      select(-investigator_id) %>%
      mutate(
        full_experiment_name = paste0(experiment_start_date, " ",
                                      investigator, " ",
                                      experiment_name,
                                      ifelse(is.na(repeat_number), "", paste0(" ", repeat_number)))
      ),
    split_dfs$groups %>%
      select(redcap_protocol_id, groups_group_name, cage_num, redcap_repeat_instance) %>%
      left_join(split_dfs$protocol %>% select(redcap_protocol_id, host_strain, host_sex, num_host_mice)) %>%
      left_join(strains, by = c("host_strain" = "id")) %>% select(-host_strain) %>%
      mutate(host_sex = ifelse(host_sex == 1, "Female", "Male")), # lazy,
    by = c("redcap_protocol_id")
  ) %>%
    left_join(
      split_dfs$score_and_weights_sw %>% filter(colbase %in% c("sc_mouse_weight")) %>%
        select(redcap_protocol_id, mouseid, sc_date, colbase, value, groupid) %>%
        bind_rows(., initial_weights),
      by = c("redcap_protocol_id", "redcap_repeat_instance" = "groupid")
    ) %>%
    mutate(
      day = ymd(sc_date) - transplant_date,
      metric = ifelse(colbase == "sc_mouse_weight", "weight", colbase)
    ) %>%
    mutate(value = as.numeric(value)) %>%
    left_join(
      split_dfs$autopsy_sheet %>% filter(colbase %in% c("as_mouse_deathdate", "as_mouse_cause", "as_mouse_eoe")) %>%
        select(-name, -rawmouseid) %>% distinct() %>%
        tidyr::pivot_wider(names_from = colbase, values_from = value) %>%
        mutate(death_date = as.numeric(ymd(as_mouse_deathdate))) %>%
        select(-as_mouse_deathdate, -redcap_repeat_instance, -redcap_repeat_instrument),
      by = c("redcap_protocol_id", "mouseid", "redcap_repeat_instance" = "groupid")
    ) %>%
    filter(!is.na(value)) %>%
    select(-colbase, -as_groups_number)

  dat <-
    bind_rows(
      dat_pre %>% select(-death_date),
      dat_pre %>% filter(!is.na(death_date)) %>% mutate(sc_date = as.character(as.Date(death_date, origin = "1970-01-01")), metric = "alive", value = 0, day = ymd(sc_date) - transplant_date) %>% select(-death_date),
      dat_pre %>% mutate(alive = ifelse(is.na(death_date) | as.Date(death_date, origin = "1970-01-01") > ymd(sc_date), 1, 0)) %>% filter(alive == 1) %>% select(-death_date, -value) %>% mutate(metric = "alive") %>% rename(value = alive),
      dat_pre %>% select(-value) %>% rename(value = death_date) %>% mutate(metric = "death_date") %>% filter(!is.na(value))
    ) %>%
    rename(
      score_date = sc_date,
      exp_grp_name = groups_group_name,
      name = full_experiment_name,
      experimental_group_id = redcap_repeat_instance
    ) %>%
    mutate(
      mouse_id = paste(redcap_protocol_id, experimental_group_id, mouseid, sep = "_")
    ) %>%
    select(-mouseid)


  # exclusions
  old_bad_mice <- split_dfs$autopsy_sheet %>%
    filter(colbase == "as_mouse_cause") %>%
    filter(grepl("exclude", value, ignore.case = TRUE)) %>%
    mutate(
      mouse_id = paste(redcap_protocol_id, groupid, mouseid, sep = "_")
    ) %>%
    pull(mouse_id)

  bad_mice <- split_dfs$autopsy_sheet %>%
    filter(colbase == "as_mouse_excluded") %>%
    filter(value == 1) %>%
    mutate(
      mouse_id = paste(redcap_protocol_id, groupid, mouseid, sep = "_")
    ) %>%
    pull(mouse_id)
  if (dplyr::n_distinct(sort(c(old_bad_mice, bad_mice))) != dplyr::n_distinct(sort(bad_mice))) {
    warning("Some mice excluded in the 'cause' column have not been fixed!")
  }
  return(dat %>% filter(!mouse_id %in% bad_mice))
}


make_redcap_and_db_match <- function(dat, con) {
  # debug
  # file.copy("data/db/vdb_mouse_app.db", "data/db/tmp_vdb_mouse_app.db", overwrite = TRUE)
  # this makes use of a lot of the logic defined in load_legacy_data.R
  print("Adding the following from REDcap:")
  ################   Experiment
  # note redcap encodes complete as 2, incomplete as 0, and partial/mixed as 1.  WE convert that to complete being 1
  exp_insert_dat <- dat %>%
    rename(
      purpose = hypothesis, start_date = experiment_start_date,
      experiment_complete = autopsy_sheet_complete
    ) %>%
    mutate(
      comments = NA, protocol_file_path = name, is_legacy_data = FALSE,
      start_date = str_to_julian(start_date),
      experiment_complete = ifelse(experiment_complete == 2, 1, 0)
    ) %>%
    select(
      name, purpose, investigator, start_date, comments,
      protocol_file_path, is_legacy_data, experiment_complete
    ) %>%
    distinct() %>%
    as.data.frame()

  print(paste("  - ", nrow(exp_insert_dat), "experiments"))
  DBI::dbWriteTable(con, "experiment", exp_insert_dat, append = TRUE, )


  ##################### Group
  new_protocol_ids <- as_tibble(dplyr::tbl(con, "experiment")) %>%
    filter(!is_legacy_data) %>%
    select(experiment_id, name)

  group_insert_dat <- dat %>%
    rename(
      number = num_host_mice,
      group_name_raw = exp_grp_name,
      description = exp_grp_name,
      organism_line = strain,
      organism_sex = host_sex,
    ) %>%
    mutate(
      group_number = 11111 + as.numeric(gsub("\\d+_(\\d+)_\\d+", "\\1", mouse_id)),
      legacy_subgroup = NA,
      organism = "Mus musculus"
    ) %>%
    left_join(new_protocol_ids, by = "name") %>%
    arrange(experiment_id, group_number) %>%
    select(
      experiment_id, group_number, legacy_subgroup,
      description, number, organism, organism_line, organism_sex
    ) %>%
    distinct()

  print(paste("  - ", nrow(group_insert_dat), "experimental groups"))
  DBI::dbWriteTable(con, "experimental_group", group_insert_dat, append = TRUE)


  ##################### Mouse
  new_exp_group_ids <- as_tibble(dplyr::tbl(con, "experimental_group")) %>%
    filter(experiment_id %in% new_protocol_ids$experiment_id) %>%
    select(experimental_group_id, description, experiment_id)


  mouse_insert_dat <- dat %>%
    left_join(new_protocol_ids, by = "name") %>%
    select(experiment_id, exp_grp_name, mouse_id, cage_num) %>%
    distinct() %>%
    left_join(new_exp_group_ids, by = c("experiment_id", "exp_grp_name" = "description")) %>%
    select(-exp_grp_name, -experiment_id) %>%
    rename(
      ear_tag = mouse_id,
      cage = cage_num
    ) %>%
    mutate(
      litter = "-",
      date_of_birth = 1
    )

  print(paste("  - ", nrow(mouse_insert_dat), "mice"))

  DBI::dbWriteTable(con, "mouse", mouse_insert_dat, append = TRUE)

  ####################### Manipulations

  manip_insert_dat <- dat %>%
    select(-experimental_group_id, -name) %>%
    rename(name = exp_grp_name) %>%
    mutate(
      description = name,
      date_administered = NA,
      dose = NA, dose_units = NA,
      route = NA
    ) %>%
    left_join(new_exp_group_ids %>% select(-experiment_id), by = c("description" = "description")) %>%
    select(experimental_group_id, name, description, date_administered, dose, dose_units, route) %>%
    distinct()
  print(paste("  - ", nrow(manip_insert_dat), "manipulations"))
  DBI::dbWriteTable(con, "manipulation", manip_insert_dat, append = TRUE)

  ##################### Observation
  obs_insert_dat <- dat %>%
    select(name, score_date) %>%
    distinct() %>%
    left_join(new_protocol_ids, by = "name") %>%
    mutate(
      date = str_to_julian(score_date),
      scores_file_path = NA,
      observer = NA,
      is_legacy_data = FALSE,
    ) %>%
    select(-score_date)

  print(paste("  - ", nrow(obs_insert_dat), "observation events"))

  DBI::dbWriteTable(con, "observation", obs_insert_dat %>% select(-name), append = TRUE)

  ##################### Mouse_Observation
  new_mouse_ids <- as_tibble(dplyr::tbl(con, "mouse")) %>%
    filter(experimental_group_id %in% new_exp_group_ids$experimental_group_id) %>%
    select(mouse_id, ear_tag) %>%
    rename(
      mouse_id_new = mouse_id,
      mouse_id = ear_tag
    )

  new_abs_ids <- as_tibble(dplyr::tbl(con, "observation")) %>%
    filter(!is_legacy_data) %>%
    select(observation_id, experiment_id, date)

  metric_names <- as_tibble(dplyr::tbl(con, "metric")) %>%
    select(-units) %>%
    rename(metric = name)

  mouse_obs_insert_dat <- dat %>%
    left_join(new_protocol_ids, by = "name") %>%
    select(redcap_protocol_id, experiment_id, mouse_id, metric, value, score_date, as_mouse_cause) %>%
    mutate(score_date = str_to_julian(score_date)) %>%
    left_join(new_abs_ids, by = c("experiment_id", "score_date" = "date")) %>%
    select(-score_date) %>%
    left_join(metric_names, by = "metric") %>%
    mutate(notes = ifelse(metric == "death_date", as_mouse_cause, "")) %>%
    select(-as_mouse_cause) %>%
    left_join(new_mouse_ids, by = "mouse_id") %>%
    select(-mouse_id) %>%
    rename(mouse_id = mouse_id_new) %>%
    select(mouse_id, observation_id, metric_id, value, notes) %>%
    distinct()

  print(paste("  - ", nrow(mouse_obs_insert_dat), "mouse observations"))
  DBI::dbWriteTable(con, "mouse_observation", mouse_obs_insert_dat, append = TRUE)
}
