make_project_state <- function(rv, input) {
 list(
  schema_version = 1L,
  app_version = "0.1.0",
  saved_at = as.character(Sys.time()),
  
  tasks = rv$tasks,
  criteria = rv$criteria,
  evaluations = rv$evaluations,
  
  counters = list(
   task_counter = rv$task_counter,
   criterion_counter = rv$criterion_counter,
   evaluation_counter = rv$evaluation_counter
  ),
  
  settings = list(
   mode = input$mode,
   use_strength = isTRUE(input$use_strength),
   default_strength = as.numeric(input$default_strength),
   direction_only = isTRUE(input$direction_only),
   shortlist_only = isTRUE(input$shortlist_only)
  )
 )
}

validate_project_state <- function(x) {
 if (!is.list(x)) {
  stop("Project file is not a list.")
 }
 
 required_top <- c(
  "schema_version", "app_version", "saved_at",
  "tasks", "criteria", "evaluations",
  "counters", "settings"
 )
 
 missing_top <- setdiff(required_top, names(x))
 if (length(missing_top) > 0) {
  stop(sprintf(
   "Project file is missing fields: %s",
   paste(missing_top, collapse = ", ")
  ))
 }
 
 if (!is.data.frame(x$tasks)) {
  stop("Project field 'tasks' is not a data.frame.")
 }
 if (!is.data.frame(x$criteria)) {
  stop("Project field 'criteria' is not a data.frame.")
 }
 if (!is.data.frame(x$evaluations)) {
  stop("Project field 'evaluations' is not a data.frame.")
 }
 
 required_task_cols <- c("id", "label", "tag", "today", "active")
 required_crit_cols <- c("id", "label", "invert", "active")
 required_eval_cols <- c("id", "level", "criterion_id", "left_id", "right_id", "choice", "strength")
 
 if (!all(required_task_cols %in% names(x$tasks))) {
  stop("Project tasks table has unexpected columns.")
 }
 if (!all(required_crit_cols %in% names(x$criteria))) {
  stop("Project criteria table has unexpected columns.")
 }
 if (!all(required_eval_cols %in% names(x$evaluations))) {
  stop("Project evaluations table has unexpected columns.")
 }
 
 required_counter_names <- c("task_counter", "criterion_counter", "evaluation_counter")
 if (!is.list(x$counters) || !all(required_counter_names %in% names(x$counters))) {
  stop("Project counters are missing or malformed.")
 }
 
 required_setting_names <- c("mode", "use_strength", "default_strength", "direction_only", "shortlist_only")
 if (!is.list(x$settings) || !all(required_setting_names %in% names(x$settings))) {
  stop("Project settings are missing or malformed.")
 }
 
 invisible(TRUE)
}

restore_project_state <- function(rv, session, proj) {
 validate_project_state(proj)
 
 # Reset dynamic observer binding caches so loaded rows get rebound cleanly
 rv$task_delete_bound_ids <- character()
 rv$task_today_bound_ids <- character()
 rv$criterion_delete_bound_ids <- character()
 rv$criterion_invert_bound_ids <- character()
 
 rv$tasks <- proj$tasks
 rv$criteria <- proj$criteria
 rv$evaluations <- proj$evaluations
 
 rv$task_counter <- as.integer(proj$counters$task_counter)
 rv$criterion_counter <- as.integer(proj$counters$criterion_counter)
 rv$evaluation_counter <- as.integer(proj$counters$evaluation_counter)
 
 rv$criteria_progress_done <- 0L
 rv$criteria_progress_total <- 0L
 
 # Restore UI settings
 updateRadioButtons(session, "mode", selected = proj$settings$mode)
 updateCheckboxInput(session, "use_strength", value = isTRUE(proj$settings$use_strength))
 updateNumericInput(
  session,
  "default_strength",
  value = max(2, min(9, as.numeric(proj$settings$default_strength)))
 )
 updateCheckboxInput(session, "direction_only", value = isTRUE(proj$settings$direction_only))
 updateCheckboxInput(session, "shortlist_only", value = isTRUE(proj$settings$shortlist_only))
 
 invisible(TRUE)
}