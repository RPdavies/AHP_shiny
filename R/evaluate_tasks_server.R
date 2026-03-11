# invert_task_pcm_if_needed <- function(obj, criterion_id, rv) {
#  if (is.null(obj)) {
#   return(NULL)
#  }
#  
#  idx <- match(criterion_id, rv$criteria$id)
#  if (!is.na(idx) && isTRUE(rv$criteria$invert[idx])) {
#   obj$pcm <- t(obj$pcm)
#  }
#  
#  obj
# }

evaluate_tasks_server <- function(input, output, session, rv) {
 
 observe({
  crit <- rv$criteria
  
  if (nrow(crit) == 0) {
   updateSelectInput(session, "criterion_sel", choices = character(), selected = character())
   updateSelectInput(session, "results_criterion_sel", choices = character(), selected = character())
  } else {
   choices <- stats::setNames(crit$id, crit$label)
   
   selected_eval <- isolate(input$criterion_sel)
   if (is.null(selected_eval) || !selected_eval %in% crit$id) {
    selected_eval <- crit$id[1]
   }
   
   selected_results <- isolate(input$results_criterion_sel)
   if (is.null(selected_results) || !selected_results %in% crit$id) {
    selected_results <- selected_eval
   }
   
   updateSelectInput(session, "criterion_sel", choices = choices, selected = selected_eval)
   updateSelectInput(session, "results_criterion_sel", choices = choices, selected = selected_results)
  }
 })
 
 tasks_for_comparison <- reactive({
  tasks <- rv$tasks
  
  if (nrow(tasks) == 0) {
   return(tasks)
  }
  
  if (isTRUE(input$shortlist_only)) {
   tasks <- tasks[tasks$today, , drop = FALSE]
  }
  
  tasks
 })
 
 valid_task_evals_for_selected_criterion <- reactive({
  cid <- selected_task_criterion_id()
  if (is.na(cid)) {
   return(rv$evaluations[0, , drop = FALSE])
  }
  
  tasks <- tasks_for_comparison()
  valid_ids <- tasks$id
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "task" & ev$criterion_id == cid, , drop = FALSE]
  
  if (nrow(ev) == 0) {
   return(ev)
  }
  
  ev[
   ev$left_id %in% valid_ids & ev$right_id %in% valid_ids,
   ,
   drop = FALSE
  ]
 })
 
 selected_task_criterion_id <- reactive({
  cid <- input$criterion_sel
  if (is.null(cid) || !nzchar(cid)) {
   return(NA_character_)
  }
  cid
 })
 
 task_pairs_all <- reactive({
  tasks <- tasks_for_comparison()
  all_pairs_df(tasks$id)
 })
 
 task_eval_keys <- reactive({
  ev <- valid_task_evals_for_selected_criterion()
  
  if (nrow(ev) == 0) {
   return(character())
  }
  
  pair_key(ev$left_id, ev$right_id)
 })
 
 task_pairs_remaining <- reactive({
  pairs <- task_pairs_all()
  
  if (nrow(pairs) == 0) {
   return(pairs)
  }
  
  keys_all <- pair_key(pairs$left_id, pairs$right_id)
  done_keys <- task_eval_keys()
  
  pairs[!(keys_all %in% done_keys), , drop = FALSE]
 })
 
 current_task_pair <- reactive({
  tasks <- tasks_for_comparison()
  rem <- task_pairs_remaining()
  
  if (nrow(rem) == 0) {
   return(NULL)
  }
  
  pair <- rem[1, , drop = FALSE]
  
  left_idx <- match(pair$left_id, tasks$id)
  right_idx <- match(pair$right_id, tasks$id)
  
  if (is.na(left_idx) || is.na(right_idx)) {
   return(NULL)
  }
  
  list(
   left_id = pair$left_id,
   right_id = pair$right_id,
   left_label = tasks$label[left_idx],
   right_label = tasks$label[right_idx]
  )
 })
 
 record_task_choice <- function(choice) {
  cid <- selected_task_criterion_id()
  if (is.na(cid)) {
   return(invisible(NULL))
  }
  
  pair <- current_task_pair()
  if (is.null(pair)) {
   return(invisible(NULL))
  }
  
  key_now <- pair_key(pair$left_id, pair$right_id)
  if (key_now %in% task_eval_keys()) {
   return(invisible(NULL))
  }
  
  strength <- if (choice == "equal") {
   1
  } else if (isTRUE(input$use_strength)) {
   max(2, as.numeric(input$strength_task))
  } else {
   max(2, as.numeric(input$default_strength))
  }
  
  rv$evaluations <- rbind(
   rv$evaluations,
   new_evaluation_row(
    id = next_evaluation_id(rv),
    level = "task",
    criterion_id = cid,
    left_id = pair$left_id,
    right_id = pair$right_id,
    choice = choice,
    strength = strength
   )
  )
 }
 
 observeEvent(input$pick_left_task, {
  record_task_choice("left")
 }, ignoreInit = TRUE)
 
 observeEvent(input$pick_equal_task, {
  record_task_choice("equal")
 }, ignoreInit = TRUE)
 
 observeEvent(input$pick_right_task, {
  record_task_choice("right")
 }, ignoreInit = TRUE)
 
 selected_results_criterion_id <- reactive({
  cid <- input$results_criterion_sel
  if (is.null(cid) || !nzchar(cid)) {
   return(NA_character_)
  }
  cid
 })
 
 task_pcm_selected <- reactive({
  cid <- selected_results_criterion_id()
  if (is.na(cid)) {
   return(NULL)
  }
  
  tasks <- tasks_for_comparison()
  if (nrow(tasks) < 2) {
   return(NULL)
  }
  
  valid_ids <- tasks$id
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "task" & ev$criterion_id == cid, , drop = FALSE]
  
  if (nrow(ev) > 0) {
   ev <- ev[
    ev$left_id %in% valid_ids & ev$right_id %in% valid_ids,
    ,
    drop = FALSE
   ]
  }
  
  obj <- build_pcm(tasks[, c("id", "label"), drop = FALSE], ev)
  invert_task_pcm_if_needed(obj, cid, rv)
 })
 
 task_weights_selected_df <- reactive({
  obj <- task_pcm_selected()
  
  if (is.null(obj) || !isTRUE(obj$complete)) {
   return(NULL)
  }
  
  compute_weights_df(obj$pcm, item_col = "Task")
 })
 
 task_eval_log_selected_df <- reactive({
  cid <- selected_results_criterion_id()
  if (is.na(cid)) {
   return(NULL)
  }
  
  valid_ids <- rv$tasks$id
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "task" & ev$criterion_id == cid, , drop = FALSE]
  
  if (nrow(ev) == 0) {
   return(NULL)
  }
  
  ev <- ev[
   ev$left_id %in% valid_ids & ev$right_id %in% valid_ids,
   ,
   drop = FALSE
  ]
  
  if (nrow(ev) == 0) {
   return(NULL)
  }
  
  data.frame(
   Left = rv$tasks$label[match(ev$left_id, rv$tasks$id)],
   Right = rv$tasks$label[match(ev$right_id, rv$tasks$id)],
   Choice = ev$choice,
   Strength = ev$strength,
   stringsAsFactors = FALSE
  )
 })
 
 output$task_progress_ui <- renderUI({
  cid <- selected_task_criterion_id()
  
  if (is.na(cid)) {
   return(div(class = "chip", "Progress: 0 / 0 comparisons"))
  }
  
  total <- nrow(task_pairs_all())
  done <- total - nrow(task_pairs_remaining())
  
  div(class = "chip", sprintf("Progress: %d / %d comparisons", done, total))
 })
 
 output$task_compare_area_ui <- renderUI({
  if (nrow(rv$criteria) == 0) {
   return(div(class = "placeholder-note", "Add at least 1 criterion first."))
  }
  
  tasks <- tasks_for_comparison()
  
  if (nrow(tasks) < 2) {
   if (isTRUE(input$shortlist_only)) {
    return(div(
     class = "placeholder-note",
     "You need at least 2 ⭐ Today tasks to compare under the current shortlist filter."
    ))
   } else {
    return(div(
     class = "placeholder-note",
     "Add at least 2 tasks in Setup → Tasks to begin task comparisons."
    ))
   }
  }
  
  cid <- selected_task_criterion_id()
  if (is.na(cid)) {
   return(div(class = "placeholder-note", "Select a criterion first."))
  }
  
  crit_label <- rv$criteria$label[match(cid, rv$criteria$id)]
  pair <- current_task_pair()
  
  if (is.null(pair)) {
   return(
    div(
     class = "alert alert-success",
     strong("All task comparisons completed for this criterion."),
     tags$div(sprintf("Criterion: %s", crit_label))
    )
   )
  }
  
  tagList(
   div(class = "muted", sprintf(
    "Which task has higher priority with respect to: %s?",
    crit_label
   )),
   div(class = "spacer12"),
   
   layout_columns(
    col_widths = c(5, 2, 5),
    
    div(
     class = "pick-tile",
     actionButton("pick_left_task", pair$left_label, class = "btn")
    ),
    
    div(
     class = "pick-tile equal",
     actionButton("pick_equal_task", "Equal", class = "btn")
    ),
    
    div(
     class = "pick-tile",
     actionButton("pick_right_task", pair$right_label, class = "btn")
    )
   ),
   
   if (isTRUE(input$use_strength)) {
    tagList(
     div(class = "spacer12"),
     sliderInput("strength_task", "Strength", min = 2, max = 9, value = 3, step = 1)
    )
   } else {
    tagList(
     div(class = "spacer12"),
     div(
      class = "placeholder-note",
      sprintf(
       "Strength is off, so each non-equal choice uses default strength = %s.",
       input$default_strength
      )
     )
    )
   }
  )
 })
 
 output$task_weights_preview_ui <- renderUI({
  if (nrow(rv$criteria) == 0) {
   return(div(class = "placeholder-note", "No criteria available yet."))
  }
  
  cid <- selected_results_criterion_id()
  if (is.na(cid)) {
   return(div(class = "placeholder-note", "Select a criterion."))
  }
  
  tasks <- tasks_for_comparison()
  if (nrow(tasks) < 2) {
   return(div(class = "placeholder-note", "Not enough tasks in the current comparison set."))
  }
  
  obj <- task_pcm_selected()
  if (is.null(obj) || !isTRUE(obj$complete)) {
   return(
    div(
     class = "placeholder-note",
     sprintf(
      "Complete all task comparisons for this criterion first (%d / %d done).",
      if (is.null(obj)) 0 else obj$n_done,
      if (is.null(obj)) 0 else obj$n_total
     )
    )
   )
  }
  
  wdf <- task_weights_selected_df()
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("Rank"),
     tags$th("Task"),
     tags$th("Weight")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(wdf)), function(i) {
     tags$tr(
      tags$td(i),
      tags$td(wdf$Task[i]),
      tags$td(sprintf("%.4f", wdf$Weight[i]))
     )
    })
   )
  )
 })
 
 output$task_eval_log_ui <- renderUI({
  logdf <- task_eval_log_selected_df()
  
  if (is.null(logdf) || nrow(logdf) == 0) {
   return(div(class = "placeholder-note", "No task comparisons recorded yet for this criterion."))
  }
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("Left"),
     tags$th("Right"),
     tags$th("Choice"),
     tags$th("Strength")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(logdf)), function(i) {
     tags$tr(
      tags$td(logdf$Left[i]),
      tags$td(logdf$Right[i]),
      tags$td(logdf$Choice[i]),
      tags$td(logdf$Strength[i])
     )
    })
   )
  )
 })
}