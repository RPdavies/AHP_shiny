setup_server <- function(input, output, session, rv) {
 
 # ---- helpers ----
 
 normalise_label <- function(x) {
  tolower(trimws(x))
 }
 
 next_task_id <- function() {
  rv$task_counter <- rv$task_counter + 1L
  sprintf("task_%03d", rv$task_counter)
 }
 
 next_criterion_id <- function() {
  rv$criterion_counter <- rv$criterion_counter + 1L
  sprintf("crit_%03d", rv$criterion_counter)
 }
 
 parse_bulk_tasks <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
   return(character())
  }
  parts <- unlist(strsplit(x, "[,\n\r]+"))
  trimws(parts)
 }
 
 add_task_labels <- function(labels) {
  labels <- trimws(labels)
  labels <- labels[nzchar(labels)]
  
  if (length(labels) == 0) {
   return(invisible(NULL))
  }
  
  existing_norm <- normalise_label(rv$tasks$label)
  added <- 0L
  skipped <- character()
  
  for (lab in labels) {
   lab_norm <- normalise_label(lab)
   
   if (lab_norm %in% existing_norm) {
    skipped <- c(skipped, lab)
    next
   }
   
   rv$tasks <- rbind(
    rv$tasks,
    new_task_row(
     id = next_task_id(),
     label = lab,
     tag = "",
     today = FALSE,
     active = TRUE
    )
   )
   
   existing_norm <- c(existing_norm, lab_norm)
   added <- added + 1L
  }
  
  if (added > 0) {
   showNotification(
    sprintf("Added %d task%s.", added, if (added == 1) "" else "s"),
    type = "message"
   )
  }
  
  if (length(skipped) > 0) {
   showNotification(
    sprintf(
     "Skipped %d duplicate task%s.",
     length(skipped),
     if (length(skipped) == 1) "" else "s"
    ),
    type = "warning"
   )
  }
  
  invisible(NULL)
 }
 
 add_criterion_labels <- function(labels, invert = FALSE) {
  labels <- trimws(labels)
  labels <- labels[nzchar(labels)]
  
  if (length(labels) == 0) {
   return(invisible(NULL))
  }
  
  if (length(invert) == 1L) {
   invert <- rep(invert, length(labels))
  }
  
  existing_norm <- normalise_label(rv$criteria$label)
  added <- 0L
  skipped <- character()
  
  for (i in seq_along(labels)) {
   lab <- labels[i]
   lab_norm <- normalise_label(lab)
   
   if (lab_norm %in% existing_norm) {
    skipped <- c(skipped, lab)
    next
   }
   
   rv$criteria <- rbind(
    rv$criteria,
    new_criterion_row(
     id = next_criterion_id(),
     label = lab,
     invert = isTRUE(invert[i]),
     active = TRUE
    )
   )
   
   existing_norm <- c(existing_norm, lab_norm)
   added <- added + 1L
  }
  
  if (added > 0) {
   showNotification(
    sprintf("Added %d criterion%s.", added, if (added == 1) "" else "s"),
    type = "message"
   )
  }
  
  if (length(skipped) > 0) {
   showNotification(
    sprintf(
     "Skipped %d duplicate criterion%s.",
     length(skipped),
     if (length(skipped) == 1) "" else "s"
    ),
    type = "warning"
   )
  }
  
  invisible(NULL)
 }
 
 filtered_tasks <- reactive({
  tasks <- rv$tasks
  
  if (nrow(tasks) == 0) {
   return(tasks)
  }
  
  if (isTRUE(input$show_shortlist)) {
   tasks <- tasks[tasks$today, , drop = FALSE]
  }
  
  q <- input$task_search
  if (is.null(q)) q <- ""
  q <- trimws(q)
  
  if (nzchar(q)) {
   keep <- grepl(q, tasks$label, ignore.case = TRUE) |
    grepl(q, tasks$tag, ignore.case = TRUE)
   tasks <- tasks[keep, , drop = FALSE]
  }
  
  tasks
 })
 
 # ---- top bar status ----
 
 output$status_chips_ui <- renderUI({
  div(
   class = "topbar-center",
   span(class = "chip", "Status:", strong("Ready")),
   span(
    class = "chip",
    "Tasks:",
    strong(nrow(rv$tasks)),
    span(class = "muted", sprintf("(%d shortlisted)", sum(rv$tasks$today)))
   ),
   span(class = "chip", "Criteria:", strong(nrow(rv$criteria))),
   span(class = "chip", "Comparisons:", strong("0 / 0"))
  )
 })
 
 # ============================================================
 # TASKS
 # ============================================================
 
 observeEvent(input$add_task, {
  label <- trimws(input$new_task)
  
  if (!nzchar(label)) {
   showNotification("Please enter a task name first.", type = "error")
   return()
  }
  
  add_task_labels(label)
  updateTextInput(session, "new_task", value = "")
 })
 
 observeEvent(input$add_all, {
  labels <- parse_bulk_tasks(input$bulk_tasks)
  
  if (length(labels) == 0) {
   showNotification("No tasks found in the bulk input box.", type = "error")
   return()
  }
  
  add_task_labels(labels)
  updateTextAreaInput(session, "bulk_tasks", value = "")
 })
 
 observeEvent(input$clear_bulk, {
  updateTextAreaInput(session, "bulk_tasks", value = "")
 })
 
 output$tasks_table_ui <- renderUI({
  tasks <- filtered_tasks()
  
  if (nrow(rv$tasks) == 0) {
   return(div(class = "placeholder-note", "No tasks yet. Add one above to get started."))
  }
  
  if (nrow(tasks) == 0) {
   return(div(class = "placeholder-note", "No tasks match the current filters."))
  }
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("Task"),
     tags$th("Tag"),
     tags$th("⭐ Today"),
     tags$th("")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(tasks)), function(i) {
     task <- tasks[i, , drop = FALSE]
     
     tags$tr(
      tags$td(task$label),
      tags$td(if (nzchar(task$tag)) task$tag else ""),
      tags$td(
       actionButton(
        inputId = paste0("toggle_today_task_", task$id),
        label = if (isTRUE(task$today)) "☑" else "☐",
        class = "btn btn-sm btn-outline-secondary"
       )
      ),
      tags$td(
       actionButton(
        inputId = paste0("delete_task_", task$id),
        label = NULL,
        icon = icon("trash"),
        class = "btn btn-sm btn-outline-danger"
       )
      )
     )
    })
   )
  )
 })
 
 observe({
  ids <- rv$tasks$id
  new_ids <- setdiff(ids, rv$task_delete_bound_ids)
  
  if (length(new_ids) == 0) return()
  
  for (id in new_ids) {
   local({
    this_id <- id
    observeEvent(input[[paste0("delete_task_", this_id)]], {
     rv$tasks <- rv$tasks[rv$tasks$id != this_id, , drop = FALSE]
     showNotification("Task deleted.", type = "message")
    }, ignoreInit = TRUE)
   })
  }
  
  rv$task_delete_bound_ids <- c(rv$task_delete_bound_ids, new_ids)
 })
 
 observe({
  ids <- rv$tasks$id
  new_ids <- setdiff(ids, rv$task_today_bound_ids)
  
  if (length(new_ids) == 0) return()
  
  for (id in new_ids) {
   local({
    this_id <- id
    observeEvent(input[[paste0("toggle_today_task_", this_id)]], {
     idx <- match(this_id, rv$tasks$id)
     if (!is.na(idx)) {
      rv$tasks$today[idx] <- !isTRUE(rv$tasks$today[idx])
     }
    }, ignoreInit = TRUE)
   })
  }
  
  rv$task_today_bound_ids <- c(rv$task_today_bound_ids, new_ids)
 })
 
 # ============================================================
 # CRITERIA
 # ============================================================
 
 observeEvent(input$add_crit, {
  label <- trimws(input$new_crit)
  
  if (!nzchar(label)) {
   showNotification("Please enter a criterion name first.", type = "error")
   return()
  }
  
  add_criterion_labels(label, invert = FALSE)
  updateTextInput(session, "new_crit", value = "")
 })
 
 observeEvent(input$add_defaults, {
  add_criterion_labels(
   labels = c("Urgency", "Importance", "Difficulty"),
   invert = c(FALSE, FALSE, TRUE)
  )
 })
 
 observeEvent(input$clear_criteria, {
  rv$criteria <- empty_criteria()
  showNotification("Criteria cleared.", type = "message")
 })
 
 output$criteria_table_ui <- renderUI({
  crit <- rv$criteria
  
  if (nrow(crit) == 0) {
   return(div(class = "placeholder-note", "No criteria yet. Add one above or use suggested defaults."))
  }
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("Criterion"),
     tags$th("Invert?"),
     tags$th("")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(crit)), function(i) {
     x <- crit[i, , drop = FALSE]
     
     tags$tr(
      tags$td(x$label),
      tags$td(
       actionButton(
        inputId = paste0("toggle_invert_criterion_", x$id),
        label = if (isTRUE(x$invert)) "☑" else "☐",
        class = "btn btn-sm btn-outline-secondary"
       )
      ),
      tags$td(
       actionButton(
        inputId = paste0("delete_criterion_", x$id),
        label = NULL,
        icon = icon("trash"),
        class = "btn btn-sm btn-outline-danger"
       )
      )
     )
    })
   )
  )
 })
 
 observe({
  ids <- rv$criteria$id
  new_ids <- setdiff(ids, rv$criterion_delete_bound_ids)
  
  if (length(new_ids) == 0) return()
  
  for (id in new_ids) {
   local({
    this_id <- id
    observeEvent(input[[paste0("delete_criterion_", this_id)]], {
     rv$criteria <- rv$criteria[rv$criteria$id != this_id, , drop = FALSE]
     showNotification("Criterion deleted.", type = "message")
    }, ignoreInit = TRUE)
   })
  }
  
  rv$criterion_delete_bound_ids <- c(rv$criterion_delete_bound_ids, new_ids)
 })
 
 observe({
  ids <- rv$criteria$id
  new_ids <- setdiff(ids, rv$criterion_invert_bound_ids)
  
  if (length(new_ids) == 0) return()
  
  for (id in new_ids) {
   local({
    this_id <- id
    observeEvent(input[[paste0("toggle_invert_criterion_", this_id)]], {
     idx <- match(this_id, rv$criteria$id)
     if (!is.na(idx)) {
      rv$criteria$invert[idx] <- !isTRUE(rv$criteria$invert[idx])
     }
    }, ignoreInit = TRUE)
   })
  }
  
  rv$criterion_invert_bound_ids <- c(rv$criterion_invert_bound_ids, new_ids)
 })
}