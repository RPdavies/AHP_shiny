project_server <- function(input, output, session, rv) {
 
 output$download_project <- downloadHandler(
  filename = function() {
   paste0("ahp_project_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
  },
  content = function(file) {
   proj <- make_project_state(rv, input)
   saveRDS(proj, file = file)
  }
 )
 
 observeEvent(input$load, {
  showModal(
   modalDialog(
    title = "Load project",
    fileInput(
     "upload_project_file",
     "Choose an .rds project file",
     accept = c(".rds")
    ),
    easyClose = TRUE,
    footer = modalButton("Cancel")
   )
  )
 })
 
 observeEvent(input$upload_project_file, {
  req(input$upload_project_file$datapath)
  
  tryCatch({
   proj <- readRDS(input$upload_project_file$datapath)
   restore_project_state(rv, session, proj)
   
   removeModal()
   
   showNotification(
    sprintf("Project loaded successfully (saved at %s).", proj$saved_at),
    type = "message",
    duration = 4
   )
  }, error = function(e) {
   showNotification(
    paste("Failed to load project:", conditionMessage(e)),
    type = "error",
    duration = 8
   )
  })
 })
 
 observeEvent(input$reset, {
  rv$tasks <- empty_tasks()
  rv$criteria <- empty_criteria()
  rv$evaluations <- empty_evaluations()
  
  rv$task_counter <- 0L
  rv$criterion_counter <- 0L
  rv$evaluation_counter <- 0L
  
  rv$task_delete_bound_ids <- character()
  rv$task_today_bound_ids <- character()
  rv$criterion_delete_bound_ids <- character()
  rv$criterion_invert_bound_ids <- character()
  
  rv$criteria_progress_done <- 0L
  rv$criteria_progress_total <- 0L
  
  updateTextInput(session, "new_task", value = "")
  updateTextAreaInput(session, "bulk_tasks", value = "")
  updateTextInput(session, "task_search", value = "")
  updateTextInput(session, "new_crit", value = "")
  updateRadioButtons(session, "mode", selected = "Daily")
  updateCheckboxInput(session, "use_strength", value = TRUE)
  updateNumericInput(session, "default_strength", value = 3)
  updateCheckboxInput(session, "direction_only", value = FALSE)
  updateCheckboxInput(session, "shortlist_only", value = TRUE)
  
  showNotification("Project reset.", type = "message")
 })
}