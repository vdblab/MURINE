library(dplyr)

sqlite.driver <-  DBI::dbDriver("SQLite")
db <- DBI::dbConnect(sqlite.driver,
                     dbname = "~/GitHub/vdb_mouse_app/app/murine_data.db"
)

# get all experimental groups matching the string "A20"
experiment_groups_mentioning_A20 <- MURINE:::db_pull_flex(db=db, group_str="*A20*")


# get all the data from any experiment selected above
experiments_mentioning_A20 <- MURINE:::db_pull_flex(db=db, exp = unique(experiment_groups_mentioning_A20$name)) %>%
  MURINE:::get_experiments_mice()

                                                    
unique(experiments_mentioning_A20$exp_grp_name)

groups_of_interest <- c("BM only", "BM + A20", "A20", 
                        "A20 only", "BM Only + A20", "BM + A20 + control chow" ,  "BM + T + A20 + control chow")


filtered_data <- experiments_mentioning_A20 %>% filter(exp_grp_name %in% groups_of_interest)

# extracting the data to be used by the survival package
surv_data <- MURINE:::get_sc_tbl(filtered_data)

# creating the actual survival object (and doing stats)
survival_object <- MURINE:::surv_make_surv_obj(
  ev_d = surv_data$event_day,
  ev_t = surv_data$event_type,
  z = surv_data$longlab,
  n_experiments = length(unique(experiments_mentioning_A20$experiment_id)),
  stat_test="Pairwise Log-rank"
)

# generatign survival curves
MURINE:::plot_surv2(
  survival_object[[1]],
  n_groups = length(unique(filtered_data$exp_grp_name)),
  title = "",
  plot_min = 0,
  #leg_lab = t(tbl_grps()),
  plot_max = max(filtered_data$day),
  jitter = TRUE
)

