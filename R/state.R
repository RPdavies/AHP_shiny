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

init_app_state <- function() {
 reactiveValues(
  tasks = empty_tasks(),
  criteria = empty_criteria(),
  task_counter = 0L,
  criterion_counter = 0L,
  bound_delete_ids = character()
 )
}