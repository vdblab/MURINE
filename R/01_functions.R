mycolors <- c(
  "#511087", "#4ecdc4", "#ffd447", "#13c01f", "#b78900", "#e66d17",
  "#8e3400", "#729b79", "#bf0000", "#907ad6", "#3f6356", "#ffbfb7",
  "#92bccc", "#c0e8ab", "#890466", "#382438", "#dad2bc", "#0048ff",
  "#f5f749", "#146d25", "#c08552", "#fc60a8", "#89fc00", "#3df8ff",
  "#ef2917", "#101f75", "#bfcccc"
)


# extract transplant date for currently-selected experiment to reference against death date
db_get_tplant <- function(db, user_choice) {
  statement <- sprintf("SELECT `start_date` FROM `experiment` WHERE (`name` = '%s')", user_choice)
  exp_info <- dbGetQuery(db, statement)
  paste(as.Date(exp_info$start_date, origin = "1970-01-01"))
}

str_to_julian <- function(x) {
  julian(lubridate::ymd(x))
}
unjul <- function(x) {
  as.Date(x, origin = "1970-01-01")
}


filter_by_experiment_params <- function(exps, purpose_str, inv, jstart, jend, is_ongoing, allow_legacy) {
  if (purpose_str != "*") {
    exps <- exps %>% filter(grepl({{ purpose_str }}, purpose, ignore.case = FALSE))
  }
  if (!is.null(inv)) {
    exps <- exps %>% filter(investigator %in% {{ inv }})
  }
  if (!is.null(jstart)) {
    exps <- exps %>% filter(start_date >= {{ jstart }})
  }
  if (!is.null(jend)) {
    exps <- exps %>% filter(start_date <= {{ jend }})
  }
  if (is_ongoing) {
    exps <- exps %>% filter(experiment_complete != 1)
  }
  if (!allow_legacy) {
    exps <- exps %>% filter(is_legacy_data != 1)
  }
  exps
}

filter_by_group_params <- function(exps, group_str, strain, sex) {
  if (!is.null(strain)) {
    exps <- exps %>% filter(organism_line == {{ strain }})
  }
  if (!is.null(sex)) {
    exps <- exps %>% filter(organism_sex == {{ sex }})
  }
  exps
}

filter_by_manipulation_params <- function(exps, groups) {
  if (!is.null(groups)){
    exps <- exps %>% filter(description %in% {{ groups }})
  }
  exps
}

db_pull_flex <- function(db, exp = NULL, inv = NULL, groups = NULL, group_str = "*", treat_str = "*", purpose_str = "*",
                         filtdatestart = "1900-01-01",
                         filtdateend = "2100-01-01", strain = NULL,
                         is_ongoing = FALSE,
                         allow_legacy = TRUE,
                         sex = NULL, organism = "Mus musculus") {
  # this looks really awful but its a  way of getting around dbplyr limitations.
  # Essentially, I (Nick) wanted to do this:
  # ---------
  # dat <- tbl(db, "experiment") %>%
  #   filter(ifelse(is.na(exp), TRUE, name == get(exp))) %>%
  #   filter(ifelse(is.na(inv), TRUE, investigator == get(inv))) %>%
  #   left_join(tbl(db, "experimental_group"), by = "experiment_id") %>%
  #   left_join(tbl(db, "mouse"), by = "experimental_group_id") %>%
  #   left_join(tbl(db, "observation"), by = "experiment_id") %>%
  #   left_join(tbl(db, "mouse_observation"), by = c("mouse_id", "observation_id")) %>%
  #   left_join(tbl(db, "metric") %>% rename(metric = name), by = c("metric_id")) %>%
  #   filter(grepl( {{ group_str }}, description)) %>%
  #   collect() %>%
  #   filter((ymd(date) >= ymd(filtdatestart)) & (ymd(date) <= ymd(filtdateend ))) %>%
  #   rename(exp_grp_name = description) %>%
  #   select(
  #     experiment_id, name, investigator, experimental_group_id, exp_grp_name, group_number,
  #     mouse_id, cage, start_date, date, metric, value
  #   )
  #   mutate(day = ymd(date) - ymd(start_date)) %>%
  #   rename(score_date = date)
  #----------
  # but that broke due to how dbplyr translates the grepl calls to SQL
  # see https://github.com/tidyverse/dbplyr/issues/289 for similar issues with sqlite
  # Update:  I got partway there changing the DB to store dates as ints
  jstart <- str_to_julian(filtdatestart)
  jend <- str_to_julian(filtdateend)

  if (is.null(exp)) {
    exps <- dplyr::tbl(db, "experiment") %>% collect()
  } else {
    exps <- dplyr::tbl(db, "experiment") %>%
      collect() %>%
      filter(name %in% {{ exp }})
  }
  exps <- filter_by_experiment_params(
    exps, purpose_str = purpose_str, inv = inv,
    jstart = jstart, jend = jend, is_ongoing = is_ongoing,
    allow_legacy = allow_legacy)

  exps <- exps %>%
    left_join(tbl(db, "experimental_group"), by = "experiment_id", copy = TRUE) %>%
    collect() %>%
    filter(grepl({{ group_str }}, description, ignore.case = FALSE))

  exps <- filter_by_group_params(exps, group_str = group_str, strain = strain, sex = sex)

  exps <- exps %>%
    left_join(tbl(db, "manipulation") %>% rename(treat_desc = description) %>% select(experimental_group_id, treat_desc), by = "experimental_group_id", copy = TRUE) %>%
    collect() %>%
    filter(grepl({{ treat_str }}, treat_desc, ignore.case = FALSE))

  exps <- filter_by_manipulation_params(exps, groups)
  return(exps)
}

get_experiments_mice <- function(exps){

  # Note:  it thought might be more performant to join all the db calls before joining to the in-memory `exps` object!
  # ... ... it wasn't ...
  # mouse_dat <- tbl(db, "mouse") %>%
  #   left_join(tbl(db, "mouse_observation"), by = c("mouse_id")) %>%
  #   left_join(tbl(db, "observation"), by = "observation_id") %>%
  # left_join(tbl(db, "metric") %>% rename(metric = name), by = c("metric_id")) %>% collect()

  exps %>%
    left_join(tbl(db, "mouse"), by = "experimental_group_id", copy = TRUE) %>%
    left_join(tbl(db, "observation"), by = "experiment_id", copy = TRUE) %>%
    left_join(tbl(db, "mouse_observation"), by = c("mouse_id", "observation_id"), copy = TRUE) %>%
    filter(!is.na(value)) %>%
    left_join(tbl(db, "metric") %>% rename(metric = name), by = c("metric_id"), copy = TRUE) %>%
    rename(exp_grp_name = description) %>%
    select(
      experiment_id, experiment_complete, name, investigator, purpose, experimental_group_id, treat_desc, exp_grp_name, group_number,
      mouse_id, cage, ear_tag, start_date, date, metric, value, notes
    ) %>%
    mutate(day = date - start_date) %>%
    dplyr::rename("score_date" = "date") %>%
    distinct()
}


load_nonreactive_global_data <- function(filename){
  sqlite.driver <-  DBI::dbDriver("SQLite")
  db <- DBI::dbConnect(sqlite.driver,
                  dbname = filename
  )
  # These tables are read in to populate the sliders and stuff -- doing  it on load is a bit slow, but it saves
  # having to requery each time
  all_mouse_observation <<- tibble(DBI::dbReadTable(db, "mouse_observation"))
  all_observation <<- tibble(DBI::dbReadTable(db, "observation"))
  all_experiment <<- tibble(DBI::dbReadTable(db, "experiment"))

  all_experiment <<- all_experiment %>%
    mutate(start_date = unjul(start_date))
  all_experimental_group <<- tibble(DBI::dbReadTable(db, "experimental_group"))
  print("checking for issues with data")
  experiments_with_known_issues <<-  db_pull_flex(db) %>% get_experiments_mice() %>% group_by(experiment_id, name, investigator) %>%
    summarize(issue = case_when(
      any(metric == "death_date" & value <= unique(start_date)) ~ "death_date preceeds start date",
      any(metric == "weight" & value > 50 ) ~ "mouse weight exceeds 50g",
      TRUE ~ "no issues")
    ) %>%
    filter(issue != "no issues")
  print("done")
  DBI::dbDisconnect(db)
}

load_redcap_data <- function(DEBUG, filename){
  tryCatch({
    REDCAP_TOKEN <- Sys.getenv("MURINE_REDCAP_TOKEN")
    REDCAP_URI <- Sys.getenv("MURINE_REDCAP_URI")
    ammend_db_from_redcap(
      token = REDCAP_TOKEN,
      api_uri = REDCAP_URI,
      core_db_filename = filename,
      live_db_filename = system.file("extdata", "tmp_murine_data.db", package = "MURINE")
    )
    # if successful, make filename global
    filename <<- system.file("extdata", "tmp_murine_data.db", package = "MURINE")
  }, error=function(x){
    print(x)
    print("Only loading legacy data!")
  }
  )
}



db_pull_flex_slow <- function(db, exp = NULL, inv = NULL, group_str = "*", treat_str = "*", purpose_str = "*",
                              filtdatestart = "1900-01-01",
                              filtdateend = "2100-01-01", strain = NULL, sex = NULL, organism = "Mus musculus") {
  # this was my attempt at speeding up the above function after re-typing the mouse db dates as ints
  # it runs fast, but resolving the lazy evaluation is slow
  jstart <- julian(ymd(filtdatestart))
  jend <- julian(ymd(filtdateend))

  exps <- tbl(db, "experiment") %>%
    filter(start_date >= {{ jstart }}) %>%
    filter(start_date <= {{ jend }}) %>%
    left_join(tbl(db, "experimental_group"), by = "experiment_id") %>%
    left_join(tbl(db, "mouse"), by = "experimental_group_id") %>%
    left_join(tbl(db, "observation"), by = "experiment_id") %>%
    left_join(tbl(db, "mouse_observation"), by = c("mouse_id", "observation_id")) %>%
    left_join(tbl(db, "metric") %>% rename(metric = name), by = c("metric_id")) %>%
    left_join(tbl(db, "manipulation") %>% rename(treat_desc = description) %>% select(experimental_group_id, treat_desc), by = "experimental_group_id")
  if (!is.null(exp)) exps <- exps %>% filter(name %in% {{ exp }})
  if (!is.null(inv)) exps <- exps %>% filter(investigator %in% {{ inv }})

  exps %>%
    rename(exp_grp_name = description) %>%
    select(
      experiment_id, name, investigator, experimental_group_id, treat_desc, exp_grp_name, group_number,
      mouse_id, cage, start_date, date, metric, value
    ) %>%
    mutate(day = date - start_date) %>%
    rename(score_date = date) %>%
    distinct()
}

# subset experiment table to extract weight details and generate fold changes and % change
get_wt_tbl <- function(dat) {
  wts <- dat %>%
    select(exp_grp_name, experimental_group_id, mouse_id, score_date, metric, value, day) %>%
    distinct() %>%
    filter(metric == "weight") %>%
    tidyr::pivot_wider(names_from = metric, values_from = value) %>%
    mutate(id = paste("grp", experimental_group_id, "m", mouse_id, sep = "_"))
  # wts <- select(tbl, exp_grp_name, experimental_group_id, mouse_id, date, weight) %>%
  #         mutate(id = paste("grp", experimental_group_id, "m", mouse_id, sep = "_"))
  bl_weights <- distinct(arrange(wts, score_date), id, .keep_all = T)
  d_zero <- distinct(bl_weights, score_date)$score_date
  bl_weights <- select(bl_weights, id, weight) %>%
    rename(baseline_wt = weight)
  wts <- left_join(wts, bl_weights, by = "id") %>% mutate(
    pct_change = (weight - baseline_wt) / baseline_wt * 100,
  )
  #                                            day = as.numeric(date - d_zero))
  wts <- filter(wts, !is.na(pct_change))

  wts$wt_fold_change <- (wts$weight / wts$baseline_wt) * 100

#' Title
#'
#' @param x 
#'
#' @return
#' @importFrom stats sd
#'
#' @examples
  sem.func <- function(x) {
    sd(x, na.rm = TRUE) / sqrt(length(x))
  }

  # this seems to achieve same thing as Amina's ddply function
  means <- group_by(wts, exp_grp_name, day) %>%
    summarise(
      mean = mean(wt_fold_change, na.rm = TRUE),
      sd = sd(wt_fold_change, na.rm = TRUE),
      sem = sem.func(wt_fold_change),
      .groups = "drop"
    ) %>%
    as.data.frame()

  final_table <- list(tbl_wt = wts, tbl_means = means)
  final_table
}


# Generate weight plots (eventually will toggle between the two options)
plot_weights <- function(wt_tbl, means_tbl) {
  wt_boxplots <- ggplot(means_tbl, aes(day, mean, color)) +
    geom_point(data = means_tbl, aes(x = day, y = mean, color = exp_grp_name), size = 5, shape = 0) +
    geom_line(data = means_tbl, aes(x = day, y = mean, color = exp_grp_name), linewidth = 1) +
    scale_x_continuous() +
    geom_errorbar(data = means_tbl, aes(x = day, y = mean, ymin = mean - sem, ymax = mean + sem, color = exp_grp_name), width = 0.4) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1", name = "Groups") +
    labs(x = "Day", y = "Weightloss (%)", title = "Weights") +
    theme(
      title = element_text(size = 20, color = "black", face = "bold"),
      axis.title = element_text(size = 20, color = "black", face = "bold"),
      legend.title = element_text(size = 20, color = "black", face = "bold"),
      legend.text = element_text(size = 20, color = "black", face = "bold"),
      axis.text.x = element_text(face = "bold", color = "#000000", size = 14), # angle = 50, hjust = 1),
      axis.text.y = element_text(face = "bold", size = 14, color = "#000000"),
      plot.title = element_text(hjust = 0.5)
    )
  # font("title", size = 20, color = "black", face = "bold") +
  # font("xy.text", size = 14, color = "black") +
  # font("axis.title", size = 16, color = "black") +
  # font("legend.title", size = 16, color = "black") +
  # font("legend.text", size = 12, color = "#696969")


  wt_dotplots <- ggplot(wt_tbl, aes(day, wt_fold_change, color)) +
    geom_point(aes(color = exp_grp_name), alpha = 0.4) +
    scale_x_continuous() +
    geom_line(data = means_tbl, aes(x = day, y = mean, color = exp_grp_name), linewidth = 1) +
    geom_errorbar(data = means_tbl, aes(x = day, y = mean, ymin = mean - sem, ymax = mean + sem, color = exp_grp_name), width = 0.4) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1", name = "Groups") +
    labs(x = "Day", y = "Weightloss (%)", title = "Weights") +
    theme(
      axis.text.x = element_text(face = "bold", color = "#000000", size = 14),
      axis.text.y = element_text(face = "bold", size = 14, color = "#000000"),
      plot.title = element_text(hjust = 0.5)
    )
  # ) +
  # font("title", size = 20, color = "black", face = "bold") +
  # font("xy.text", size = 14, color = "black") +
  # font("axis.title", size = 16, color = "black") +
  # font("legend.title", size = 16, color = "black") +
  # font("legend.text", size = 12, color = "#696969")



  final_plots <- list(box = wt_boxplots, dot = wt_dotplots)
  final_plots
}

plot_scores <- function(tbl_exp_data){
  # need to get weight scores too
  scoring_metrics <- c("skin", "fur", "posture", "activity")
  score_days <- sort(unique(tbl_exp_data$day))
  # get the day of last weigh-in.
  # enter a death date as the next scoring day
  # give dummy entries which we can match up with scores of
  # 6 which we are labelling as NA (death) in the plot, so it appears about symptom scores of 6 (more serious).
  # the *1.1 is because some experiment weight mice on sac days and others do not.
  # this ensures we don't add a pseudovalue on the same day an actual value exists
  # dead_mouse_last_weighin<- tbl_exp_data %>% filter(metric %in% c("alive", "weight")) %>%
  #   group_by(name, exp_grp_name, mouse_id, day) %>%
  #   summarize(day_alive = ifelse(any(metric == "alive" & value ==1), day, NA)) %>%
  #   group_by(name, exp_grp_name, mouse_id) %>%
  #   summarize(last_weigh_day  = max(day_alive, na.rm = TRUE)) %>%
  #   filter(last_weigh_day != max(score_days)) %>%
  #   mutate(day = score_days[match(last_weigh_day, score_days) + 1] * 1, dummy="NA") %>%
  #   ungroup() %>%
  #   left_join(data.frame(dummy="NA", value=rep(6, length(scoring_metrics)), metric=scoring_metrics )) %>%
  #   select(-dummy)
  #
  # # this pivoting uis used to fill in the remaining dates
  # dead_mouse_last_weighin %>%
  #   group_by(name, exp_grp_name, mouse_id) %>%
  #   left_join(data.frame(day=score_days)) %>%



  p_summary <- tbl_exp_data %>% filter(metric %in% scoring_metrics) %>% select(-treat_desc) %>% distinct() %>%
    # bind_rows(dead_mouse_last_weighin) %>%
    mutate(longlab = paste(exp_grp_name, strtrim(name, width = 30), sep = "\n")) %>%
    ggplot(aes(color=longlab, y=value, x=day)) +
    geom_violin(aes(group=day), position = position_nudge(x=.75), width=.5) +
    geom_point(position = position_nudge(x=-.75)) + #position=position_jitter(width = 0.5, height=.1, seed = 123)) +
    #  geom_line(aes(group=mouse_id), position=position_jitter(width = 0.5, height=.1, seed = 123), alpha=.8) +
    scale_y_continuous(limits=c(0, 7), breaks=c(0:7), expand=c(0, 0), labels=c(0:5, "NA (death)", "")) +
    theme_minimal() +
    scale_x_continuous(minor_breaks = 1, breaks = scales::pretty_breaks()) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    labs(x="Day", y="Score", color="") +
    theme( panel.grid.major.y = element_line(color="gray90"), legend.position = "bottom") +
    facet_grid(metric~longlab)
  p_alive <- tbl_exp_data %>%     mutate(longlab = paste(exp_grp_name, strtrim(name, width = 30), sep = "\n")) %>%
    filter(metric == "alive" & value == 1) %>%
    group_by(longlab, day) %>%
    summarize(n_alive = n_distinct(mouse_id)) %>%
    ggplot(aes(x=day, y=n_alive)) +
    geom_point() +
    geom_line() +
    facet_grid(~longlab) + theme_minimal() +
    labs(x="Day", y="Number of Mice Alive", color="") +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    theme( panel.grid.major.y = element_line(color="gray90"))

    list(summary = p_summary, surviving=p_alive)
}




get_sc_tbl <- function(tbl_exp_data) {
  #  pivot to wide, then group per mouse. day is max day if no death date is present,
  wid <- tbl_exp_data %>%
    tidyr::pivot_wider(names_from = "metric", values_from = "value") %>%
    mutate(longlab = paste(exp_grp_name, strtrim(name, width = 30), sep = " @ "))

  max_day_df <- tbl_exp_data %>%
    group_by(experiment_id) %>%
    filter(metric == "weight") %>%
    summarise(maxday = ifelse(unique(experiment_complete) == 1, max(day), str_to_julian(Sys.Date() - unique(start_date))))

  if ("death_date" %in% colnames(wid)) {
    res <- wid %>%
      mutate(death_day = death_date - start_date) %>%
      left_join(max_day_df, by = "experiment_id") %>%
      group_by(name, mouse_id, longlab, exp_grp_name) %>%
      summarize(
        cause_of_death = ifelse(any(notes != ""), unique(notes[notes != ""]), ""),
        ear_tag = unique(ear_tag),
        event_day = ifelse(any(!is.na(death_date)), unique(na.omit(death_day)), maxday),
        event_type = ifelse(any(!is.na(death_date)), 1, 0),
        .groups = "drop"
      )
  } else {
    res <- wid %>%
      group_by(experiment_id) %>%
      filter(score_date == min(score_date)) %>%
      distinct() %>%
      left_join(max_day_df, by = "experiment_id") %>%
      group_by(name, mouse_id, longlab, exp_grp_name) %>%
      summarize(
        cause_of_death = "",
        ear_tag = unique(ear_tag),
        event_day = maxday,
        event_type = 0,
        .groups = "drop"
      )
  }
  return(res)
}


#' Title
#'
#' @param ev_d 
#' @param ev_t 
#' @param z 
#' @param n_experiments 
#' @param stat_test 
#' @importFrom  survival Surv survfit survdiff coxph
#' @return
#'
#' @examples
surv_make_surv_obj <- function(ev_d, ev_t, z, n_experiments, stat_test) {
  # get rid of long label for single experiments
  if (n_experiments == 1) z <- gsub("(.*) \\@ .*", "\\1", z)
  survdata <- data.frame(
    event_day = as.numeric(ev_d),
    event_type = c(ev_t),
    Z = z
  )
  # fit survival
  fit <- survfit(as.formula(paste0("Surv(event_day, event_type) ~ Z")), data = survdata)
  fitstat = "statistics only performed on single experiments"
  if (n_experiments == 1){
      if (stat_test == "Log-rank"){
        fitstat <- survdiff(as.formula(paste0("Surv(event_day, event_type) ~ Z")), data = survdata)
      } else if(stat_test == "Pairwise Log-rank"){
        if (!"pairwise_survdiff" %in% ls()){
          # our R 3.6 rconnect instance struggles with dependencies, so we source this directly :( 
          source("https://raw.githubusercontent.com/kassambara/survminer/f82ff9902e473d6ac60d3500c9b7b6d41c42646d/R/pairwise_survdiff.R")
        }
        fitstat <- pairwise_survdiff(as.formula(paste0("Surv(event_day, event_type) ~ Z")), data = survdata)
      } else if (startsWith(stat_test, "Cox")){
        fitstat <-  coxph(as.formula(paste0("Surv(event_day, event_type) ~ Z")), data = survdata)
      } else{
        fitstat <- "Unknown test"
      }
  }
  list(fit, fitstat, survdata)
}

# plot survival object
# depreciated this which used ggsurvplot because the package was not installing on the Rconnect instance
# plot_surv <- function(surv, survdata, n_groups, names_dict, title, rationale, plot_min, plot_max, leg_lab, DEBUG = FALSE, jitter=FALSE, these_colors=NULL) {
#   if (DEBUG) {
#     print(n_groups)
#     print(plot_max)
#     print(leg_lab)
#     print(surv)
#   }
#   if (n_groups == 1) {
#     group_color <- "Red"
#   } else {
#     group_color <- "Z"
#   }
#
#   if (plot_max > 100) {
#     x_breaks <- 50
#   } else {
#     x_breaks <- 10
#   }
#
#   gg <- survminer::ggsurvplot(fit = surv,
#     size = 1,pval = TRUE,
#     color = group_color,
#     title = title,
#     xlab = "Days",
#     ylab = "Survival",
#     alpha = .7,
#     legend = "right",
#     font.title = c(20, "bold", "black"),
#     font.x = c(18, "plain", "black"),
#     font.y = c(18, "plain", "black"),
#     legend.title = "Groups",
#     # legend.labs = leg_lab,
#     font.tickslab = c(16, "plain", "black"),
#     # linetype = line_types,
#     palette = "Set1",
#     axes.offset = FALSE, # this just removes the little extra space before 0 is marked in the x axis
#     xlim = c(plot_min, plot_max),
#     break.time.by = x_breaks
#   )
#   # This is the remainder of Amina's additions. It seems like some of it I already added up top (x axis font and such) but it does add percentages to the y axis. Will see how this goes
#
#   gg$plot <-
#     gg$plot +
# #    font("xy.text", size = 14, color = "black") +
# #    font("legend.title", size = 16, color = "black") +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       plot.title.position = "plot",
#       legend.text = element_text(size = 12, color = "#696969")
#     ) +
#     scale_y_continuous(labels = scales::percent, expand = c(0, 0.1)) #+geom_point(aes())
#
#
#   if(jitter){
#     # this mess puls apart the ggplot/ggsuv object and manually manipulates the values to
#     # avoid overlap at the 100% survival line
#     gg_decon <- ggplot_build(gg$plot)
#     ng <- length(unique(gg_decon$data[[1]]$colour))
#     gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
#       mutate(y=ifelse(y==1, 1 + (.01*as.numeric(as.factor(colour))), y))
#     gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
#       mutate(y=ifelse(y==1, 1 + (.01*as.numeric(as.factor(colour))), y))
#
#     gtable <- ggplot_gtable(gg_decon)
#
#
#     grid::grid.newpage()
#     grid::grid.draw(gtable)
#     return(invisible(gtable))
#   } else{
#     return(gg)
#   }
# }

plot_surv2 <- function(surv, survdata, n_groups, names_dict, title, rationale, plot_min, plot_max, leg_lab, DEBUG = FALSE, jitter = FALSE, these_colors = NULL) {
  if (missing(these_colors)) {
    these_colors <- mycolors[1:length(surv$strata)]
  }
  if (all(surv$n.censor == 0)) {
    print("Warning: no censored points")
    plot_cens <- FALSE
  } else {
    plot_cens <- TRUE
  }
  #  names(these_colors) <-names(surv$strata)
  gg <- GGally::ggsurv(
    surv,
    order.legend = FALSE, surv.col = these_colors,
    cens.col = these_colors, plot.cens = plot_cens,
    cens.shape = 2,
  ) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 18, face = "plain", color = "grey30"),
      title = element_text(size = 20, face = "bold", color = "black"),
      legend.title = element_text(size = 16, color = "black"),
      plot.title = element_text(hjust = 0.5),
      plot.title.position = "plot",
      legend.text = element_text(size = 12, color = "#696969")
    ) +
    # scale_color_manual(values=these_colors, breaks=names_dict$clean, labels=names_dict$raw) +
    # scale_linetype_manual(values = rep(1, length(surv$strata)), breaks=names_dict$clean, labels=names_dict$raw) +
    labs(title = title, x = "Days", linetype = "Group", color = "Group") +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0.1), limits = c(0, 1)) #+geom_point(aes())
  if (jitter) {
    # this mess puls apart the ggplot/ggsuv object and manually manipulates the values to
    # avoid overlap at the 100% survival line
    gg_decon <- ggplot_build(gg)
    ng <- length(unique(gg_decon$data[[1]]$colour))
    jitter_map <- data.frame(
      colour = unique(gg_decon$data[[1]]$colour)
    ) %>% mutate(
      numcol = as.numeric(as.factor(colour)) - 1,
      newy = 1 + (numcol * .01)
    )
    # fix lines
    gg_decon$data[[1]] <- gg_decon$data[[1]] %>%
      left_join(jitter_map, by = "colour") %>%
      mutate(y = ifelse(y == 1, newy, y))
    # censor_dat_df <- bind_rows(lapply(2:length(gg_decon$data), function(x) gg_decon$data[[x]]))
    # fix points per group
    for (grpcol in jitter_map$colour) {
      if (length(gg_decon$data) >= 2) {
        for (i in 2:length(gg_decon$data)) {
          tmp <- gg_decon$data[[i]] %>% rownames_to_column("rowid")
          tmp <- tmp %>%
            filter(colour == grpcol) %>%
            left_join(jitter_map, by = "colour") %>%
            mutate(y = ifelse(y == 1, newy, y)) %>%
            select(-numcol, -newy)
          rows_to_replace <- as.numeric(tmp$rowid)
          tmp$rowid <- NULL
          if (length(rows_to_replace) > 0) {
            gg_decon$data[[i]][rows_to_replace, ] <- tmp
          }
        }
      }
    }

    gtable <- ggplot_gtable(gg_decon)


    grid::grid.newpage()
    grid::grid.draw(gtable)
    return(invisible(gtable))
  } else {
    return(gg)
  }
}


# fixing digits displayed
write_pzfx <- function(x, path, row_names=TRUE, x_col=NA) {
  # figure out if x is a single table or multiple of them
  if (inherits(x, c("data.frame", "matrix"))) {
    x_lst <- list("Data 1"=x)
  } else if (inherits(x, "list")) {
    x_lst <- x
    if (is.null(names(x_lst))) names(x_lst) <- paste("Data", seq_len(length(x_lst)))
    are_dfs <- sapply(x_lst, function(x) inherits(x, c("data.frame", "matrix")))
    if (any(!are_dfs)) stop(sprintf("These elements are not data frame or matrix: %s",
                                    paste(names(x_lst)[!are_dfs], collapse=", ")))
  } else {
    stop(sprintf("Cannot process x of class %s", paste(class(x), collapse=", ")))
  }
  # make sure all elements are numeric
  are_nums <- sapply(x_lst, function(x) all(sapply(x, is.numeric)))
  if (any(!are_nums)) {
    stop(paste0("These tables are not all numeric: ",
                paste(names(x_lst)[!are_nums], collapse=", "),
                ". Such tables are not supported by Prism GraphPad. ",
                "You may want to spread / pivot the input data by non-numeric columns into a 'wide' format, ",
                "where the table elements are all numeric."
    ))
  }
  # make sure row_names matches the length of x_lst
  if (length(row_names) == 1) row_names <- rep(row_names, length(x_lst))
  if (length(row_names) != length(x_lst)) {
    stop("Argument 'row_names' can only be of length 1 or the length of 'x'")
  }
  # convert other kinds of x_col specifications to a vector of integers
  if (length(x_col) == 1) x_col <- rep(x_col, length(x_lst))
  if (length(x_col) != length(x_lst)) {
    stop("Argument 'x_col' can only be of length 1 or the length of 'x'")
  }
  x_col[is.na(x_col)] <- 0
  if (is.numeric(x_col)) {
    x_col <- as.integer(x_col)
    if (any(x_col > sapply(x_lst, ncol))) {
      vio <- which(x_col > sapply(x_lst, ncol))
      stop(sprintf("Not enough columns for table %s", paste(names(x_lst)[vio], collapse=", ")))
    }
  }
  if (is.character(x_col)) {
    for (i in seq_len(length(x_col))) {
      idx <- which(colnames(x_lst[[i]]) == x_col[i])
      if (length(idx) == 0) {
        warning(sprintf(
          "Column %s is not in table %s, not used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- 0
      } else if (length(idx) > 1) {
        warning(sprintf(
          "Column %s has multiple occurance in table %s, only the first one used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- idx[1]
      } else {
        x_col[i] <- idx
      }
    }
    x_col <- as.integer(x_col)
  }

  lst <- base_lst()
  lst$GraphPadPrismFile$TableSequence <- table_seq_lst(x_lst)
  lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, table_lst(x_lst, row_names, x_col))
  attr(lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
  xml <- xml2::as_xml_document(lst)
  xml2::write_xml(xml, path)
  invisible(x)
}

# The basic list for a pzfx xml
base_lst <- function() {
  lst <- list(
    "GraphPadPrismFile"=list(
      "Created"=list(
        "OriginalVersion"=structure(
          list(),
          CreatedByProgram="GraphPad Prism",
          CreatedByVersion="6.0f.254",
          Login="",
          DateTime=strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
        )
      ),
      "InfoSequence"=list(
        "Ref"=structure(
          list(),
          "ID"="Info0",
          "Selected"="1"
        )
      ),
      "Info"=structure(
        list(
          "Title"=list("Project info 1"),
          "Notes"=list(""),
          "Constant"=list("Name"=list("Experiment Date"), "Value"=list("")),
          "Constant"=list("Name"=list("Experiment ID"), "Value"=list("")),
          "Constant"=list("Name"=list("Notebook ID"), "Value"=list("")),
          "Constant"=list("Name"=list("Project"), "Value"=list("")),
          "Constant"=list("Name"=list("Experimenter"), "Value"=list("")),
          "Constant"=list("Name"=list("Protocol"), "Value"=list(""))
        ),
        "ID"="Info0"
      )
      # Then, the "TableSequence" list goes here
      # Then, all "Table" lists go here
    )
  )
  return(lst)
}

# "TableSequence" element of the list for a pzfx xml
# Number of Refs corresponds to number of tables
table_seq_lst <- function(x_lst) {
  ret <- lapply(seq_len(length(x_lst)), function(i) {
    ref <- structure(
      list(),
      "ID"=sprintf("Table%d", i - 1)
    )
    if (i == 1) attr(ref, "Selected") <- "1"
    return(ref)
  })
  names(ret) <- rep("Ref", length(x_lst))
  return(ret)
}

# "Table" elements of the list for a pzfx xml
# As many tables as you have
# Currently only supports pzfx's "Column" type of tables
table_lst <- function(x_lst, row_names, x_col) {
  if (length(x_lst) != length(row_names)) {
    stop("Argument 'row_names' can only be of the same length as 'x_lst'")
  }
  if (length(x_lst) != length(x_col)) {
    stop("Argument 'x_col' can only be of the same length as 'x_lst'")
  }
  if (!is.integer(x_col)) {
    stop("Argument 'x_col' can only be of type 'integer'")
  }
  subcol_helper <- function(v) {
    v <- as.vector(v)
    lapply(v, function(e) list("d"=list(as.character(e))))
  }
  ret <- lapply(seq_len(length(x_lst)), function(i) {
    this_df <- x_lst[[i]]
    # check type
    all_ints <-all(sapply(this_df[,2:ncol(this_df)], function(n){
      isTRUE(all.equal(as.integer(n), n))}))
    cols <- lapply(seq_len(ncol(this_df)), function(c) {
      structure(
        list(
          "Title"=list(colnames(this_df)[c]),
          "Subcolumn"=subcol_helper(this_df[, c, drop=TRUE])
        ),
        Width="89",
        Decimals=ifelse(all_ints, "0", "2"),
        Subcolumns="1"
      )
    })
    names(cols) <- rep("YColumn", ncol(this_df))
    names(cols)[x_col[i]] <- "XColumn"
    one_table_lst <- list("Title"=list(names(x_lst)[i]))
    if (row_names[i]) {
      one_table_lst[["RowTitlesColumn"]] <- structure(
        list("Subcolumn"=subcol_helper(row.names(this_df))),
        Width="39"
      )
    }
    one_table <- structure(
      append(one_table_lst, cols),
      ID=sprintf("Table%d", i - 1),
      XFormat="none",
      #YFormat="replicates",
      #Replicates="1",
      TableType="OneWay",
      EVFormat="AsteriskAfterNumber"
    )
    if (x_col[i] > 0) {
      attr(one_table, "XFormat") <- "numbers"
      attr(one_table, "YFormat") <- "replicates"
      attr(one_table, "Replicates") <- "1"
      attr(one_table, "TableType") <- "XY"
    }
    return(one_table)
  })
  names(ret) <- rep("Table", length(x_lst))
  return(ret)
}

