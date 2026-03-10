empty_tasks <- function() {
 data.frame(
  id = character(),
  label = character(),
  tag = character(),
  today = logical(),
  active = logical(),
  stringsAsFactors = FALSE
 )
}

empty_criteria <- function() {
 data.frame(
  id = character(),
  label = character(),
  invert = logical(),
  active = logical(),
  stringsAsFactors = FALSE
 )
}

empty_evaluations <- function() {
 data.frame(
  id = character(),
  level = character(),
  criterion_id = character(),
  left_id = character(),
  right_id = character(),
  choice = character(),
  strength = numeric(),
  stringsAsFactors = FALSE
 )
}

new_task_row <- function(id, label, tag = "", today = FALSE, active = TRUE) {
 data.frame(
  id = id,
  label = label,
  tag = tag,
  today = today,
  active = active,
  stringsAsFactors = FALSE
 )
}

new_criterion_row <- function(id, label, invert = FALSE, active = TRUE) {
 data.frame(
  id = id,
  label = label,
  invert = invert,
  active = active,
  stringsAsFactors = FALSE
 )
}

new_evaluation_row <- function(id, level, criterion_id, left_id, right_id, choice, strength) {
 data.frame(
  id = id,
  level = level,
  criterion_id = criterion_id,
  left_id = left_id,
  right_id = right_id,
  choice = choice,
  strength = strength,
  stringsAsFactors = FALSE
 )
}

init_app_state <- function() {
 reactiveValues(
  tasks = empty_tasks(),
  criteria = empty_criteria(),
  evaluations = empty_evaluations(),
  
  task_counter = 0L,
  criterion_counter = 0L,
  evaluation_counter = 0L,
  
  task_delete_bound_ids = character(),
  task_today_bound_ids = character(),
  
  criterion_delete_bound_ids = character(),
  criterion_invert_bound_ids = character(),
  
  criteria_progress_done = 0L,
  criteria_progress_total = 0L
 )
}