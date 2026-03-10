setup_server <- function(input, output, session, rv) {
 
 # ---- helpers ----
 
 normalise_label <- function(x) {
  tolower(trimws(x))
 }
 
 next_task_id <- function() {
  rv$task_counter <- rv$task_counter + 1L
  sprintf("task_%03d", rv$task_counter)
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
 
 parse_bulk_tasks <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
   return(character())
  }
  
  parts <- unlist(strsplit(x, "[,\n\r]+"))
  trimws(parts)
 }
 
 filtered_tasks <- reactive({
  tasks <- rv$tasks
  
  if (nrow(tasks) == 0) {
   return(tasks)
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
   span(class = "chip", "Tasks:", strong(nrow(rv$tasks))),
   span(class = "chip", "Criteria:", strong(nrow(rv$criteria))),
   span(class = "chip", "Comparisons:", strong("0 / 0"))
  )
 })
 
 # ---- add single task ----
 
 observeEvent(input$add_task, {
  label <- trimws(input$new_task)
  
  if (!nzchar(label)) {
   showNotification("Please enter a task name first.", type = "error")
   return()
  }
  
  add_task_labels(label)
  updateTextInput(session, "new_task", value = "")
 })
 
 # ---- bulk add ----
 
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
 
 # ---- render task table ----
 
 output$tasks_table_ui <- renderUI({
  tasks <- filtered_tasks()
  
  if (nrow(rv$tasks) == 0) {
   return(
    div(class = "placeholder-note", "No tasks yet. Add one above to get started.")
   )
  }
  
  if (nrow(tasks) == 0) {
   return(
    div(class = "placeholder-note", "No tasks match the current search.")
   )
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
      tags$td(if (isTRUE(task$today)) "☑" else "☐"),
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
 
 # ---- bind delete observers once per task id ----
 
 observe({
  ids <- rv$tasks$id
  new_ids <- setdiff(ids, rv$bound_delete_ids)
  
  if (length(new_ids) == 0) {
   return()
  }
  
  for (id in new_ids) {
   local({
    this_id <- id
    
    observeEvent(input[[paste0("delete_task_", this_id)]], {
     rv$tasks <- rv$tasks[rv$tasks$id != this_id, , drop = FALSE]
     showNotification("Task deleted.", type = "message")
    }, ignoreInit = TRUE)
   })
  }
  
  rv$bound_delete_ids <- c(rv$bound_delete_ids, new_ids)
 })
}