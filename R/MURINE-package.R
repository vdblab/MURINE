#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom tibble as_tibble tibble column_to_rownames rownames_to_column
#' @importFrom tidyselect starts_with
#' @importFrom stats na.omit sd 'as.formula'
#' @importFrom utils head write.csv
#' @importFrom RSQLite SQLite
#' @importFrom rlang .data
#' @importFrom stringr str_split str_glue str_trim str_wrap
## usethis namespace: end
NULL
utils::globalVariables(
  c("alive", "all_experiment", "all_experimental_group", "as_groups_number",
    "as_mouse_cause", "as_mouse_deathdate", "autopsy_sheet_complete",
    "baseline_wt", "cage", "cage_num", "cause_of_death", "colbase",
    "color", "color_base", "colour", "comments", "date_administered",
    "day", "db", "death_date", "death_day", "description",
    "dose", "dose_units", "ear_tag", "event_day", "event_type", "exp_grp_name",
    "experiment_complete", "experiment_id", "experiment_name", "experiment_start_date",
    "experimental_group_id", "experiments_with_known_issues", "field_name",
    "filename", "form_name", "full_experiment_name", "group_number",
    "groupid", "groups_group_name", "host_sex", "host_strain", "hypothesis",
    "indv_color", "injections_tcell_date", "investigator", "investigator_id",
    "is_legacy_data", "issue", "legacy_subgroup", "longlab", "maxday",
    "metric", "metric_id", "mouse_id", "mouse_id_new", "mouseid",
    "n_alive", "n_mice", "name", "newy", "notes", "num_host_mice",
    "number", "numcol", "observation_id", "organism", "organism_line",
    "organism_sex", "pairwise_survdiff", "pct_change", "plotWidth",
    "protocol_file_path", "purpose", "rawmouseid", "redcap_protocol_id",
    "redcap_repeat_instance", "redcap_repeat_instrument", "repeat_number",
    "route", "rvs", "sc_date", "score_date", "scoring_tech", "select_choices_or_calculations",
    "sem", "start_date", "strain", "transplant_date", "treat_desc",
    "val", "value", "weight", "wt_fold_change", "y")
)