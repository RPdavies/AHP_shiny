evaluate_criteria_server <- function(input, output, session, rv) {
 
 criteria_pairs_all <- reactive({
  ids <- rv$criteria$id
  all_pairs_df(ids)
 })
 
 criteria_eval_keys <- reactive({
  ev <- rv$evaluations
  ev <- ev[ev$level == "criteria", , drop = FALSE]
  
  if (nrow(ev) == 0) {
   return(character())
  }
  
  pair_key(ev$left_id, ev$right_id)
 })
 
 criteria_pairs_remaining <- reactive({
  pairs <- criteria_pairs_all()
  
  if (nrow(pairs) == 0) {
   return(pairs)
  }
  
  keys_all <- pair_key(pairs$left_id, pairs$right_id)
  done_keys <- criteria_eval_keys()
  
  pairs[!(keys_all %in% done_keys), , drop = FALSE]
 })
 
 current_criteria_pair <- reactive({
  rem <- criteria_pairs_remaining()
  
  if (nrow(rem) == 0) {
   return(NULL)
  }
  
  pair <- rem[1, , drop = FALSE]
  
  left_idx <- match(pair$left_id, rv$criteria$id)
  right_idx <- match(pair$right_id, rv$criteria$id)
  
  if (is.na(left_idx) || is.na(right_idx)) {
   return(NULL)
  }
  
  list(
   left_id = pair$left_id,
   right_id = pair$right_id,
   left_label = rv$criteria$label[left_idx],
   right_label = rv$criteria$label[right_idx]
  )
 })
 
 observe({
  crit <- rv$criteria
  
  if (nrow(crit) == 0) {
   updateSelectInput(
    session,
    "criteria_reset_sel",
    choices = character(),
    selected = character()
   )
  } else {
   choices <- stats::setNames(crit$id, crit$label)
   
   selected_now <- isolate(input$criteria_reset_sel)
   if (is.null(selected_now) || !selected_now %in% crit$id) {
    selected_now <- crit$id[1]
   }
   
   updateSelectInput(
    session,
    "criteria_reset_sel",
    choices = choices,
    selected = selected_now
   )
  }
 })
 
 observe({
  total <- nrow(criteria_pairs_all())
  done <- total - nrow(criteria_pairs_remaining())
  
  rv$criteria_progress_total <- total
  rv$criteria_progress_done <- done
 })
 
 record_criteria_choice <- function(choice) {
  pair <- current_criteria_pair()
  if (is.null(pair)) {
   return(invisible(NULL))
  }
  
  key_now <- pair_key(pair$left_id, pair$right_id)
  if (key_now %in% criteria_eval_keys()) {
   return(invisible(NULL))
  }
  
  strength <- if (choice == "equal") {
   1
  } else if (isTRUE(input$use_strength)) {
   max(2, as.numeric(input$strength_crit))
  } else {
   max(2, as.numeric(input$default_strength))
  }
  
  rv$evaluations <- rbind(
   rv$evaluations,
   new_evaluation_row(
    id = next_evaluation_id(rv),
    level = "criteria",
    criterion_id = NA_character_,
    left_id = pair$left_id,
    right_id = pair$right_id,
    choice = choice,
    strength = strength
   )
  )
  
  invisible(NULL)
 }
 
 observeEvent(input$pick_left_crit, {
  record_criteria_choice("left")
 }, ignoreInit = TRUE)
 
 observeEvent(input$pick_equal_crit, {
  record_criteria_choice("equal")
 }, ignoreInit = TRUE)
 
 observeEvent(input$pick_right_crit, {
  record_criteria_choice("right")
 }, ignoreInit = TRUE)
 
 observeEvent(input$clear_selected_criteria_comparisons, {
  cid <- input$criteria_reset_sel
  
  if (is.null(cid) || !nzchar(cid)) {
   showNotification("Please select a criterion first.", type = "error")
   return()
  }
  
  old_n <- nrow(rv$evaluations)
  
  rv$evaluations <- rv$evaluations[
   !(
    rv$evaluations$level == "criteria" &
     (rv$evaluations$left_id == cid | rv$evaluations$right_id == cid)
   ),
   ,
   drop = FALSE
  ]
  
  removed_n <- old_n - nrow(rv$evaluations)
  crit_label <- rv$criteria$label[match(cid, rv$criteria$id)]
  
  showNotification(
   sprintf(
    "Removed %d criteria comparison%s involving %s.",
    removed_n,
    if (removed_n == 1) "" else "s",
    crit_label
   ),
   type = "message"
  )
 }, ignoreInit = TRUE)
 
 criteria_pcm <- reactive({
  crit <- rv$criteria
  
  if (nrow(crit) < 2) {
   return(NULL)
  }
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "criteria", , drop = FALSE]
  
  build_pcm(crit[, c("id", "label"), drop = FALSE], ev)
 })
 
 criteria_weights_df <- reactive({
  obj <- criteria_pcm()
  
  if (is.null(obj) || !isTRUE(obj$complete)) {
   return(NULL)
  }
  
  compute_weights_df(obj$pcm, item_col = "Criterion")
 })
 
 criteria_eval_log_df <- reactive({
  ev <- rv$evaluations
  ev <- ev[ev$level == "criteria", , drop = FALSE]
  
  if (nrow(ev) == 0) {
   return(NULL)
  }
  
  left_lab <- rv$criteria$label[match(ev$left_id, rv$criteria$id)]
  right_lab <- rv$criteria$label[match(ev$right_id, rv$criteria$id)]
  
  data.frame(
   Left = left_lab,
   Right = right_lab,
   Choice = ev$choice,
   Strength = ev$strength,
   stringsAsFactors = FALSE
  )
 })
 
 output$criteria_progress_ui <- renderUI({
  div(
   class = "chip",
   sprintf(
    "Progress: %d / %d comparisons",
    rv$criteria_progress_done,
    rv$criteria_progress_total
   )
  )
 })
 
 output$criteria_compare_area_ui <- renderUI({
  if (nrow(rv$criteria) < 2) {
   return(
    div(
     class = "placeholder-note",
     "Add at least 2 criteria in Setup → Criteria to begin comparing them."
    )
   )
  }
  
  pair <- current_criteria_pair()
  
  if (is.null(pair)) {
   return(
    div(
     class = "alert alert-success",
     strong("All criteria comparisons completed."),
     tags$div("The resulting criteria ranking is now visible in Results.")
    )
   )
  }
  
  tagList(
   div(class = "muted", "Which matters more for overall priority?"),
   div(class = "spacer12"),
   
   layout_columns(
    col_widths = c(5, 2, 5),
    
    div(
     class = "pick-tile",
     actionButton("pick_left_crit", pair$left_label, class = "btn")
    ),
    
    div(
     class = "pick-tile equal",
     actionButton("pick_equal_crit", "Equal", class = "btn")
    ),
    
    div(
     class = "pick-tile",
     actionButton("pick_right_crit", pair$right_label, class = "btn")
    )
   ),
   
   if (isTRUE(input$use_strength)) {
    tagList(
     div(class = "spacer12"),
     sliderInput("strength_crit", "Strength", min = 2, max = 9, value = 3, step = 1)
    )
   } else {
    tagList(
     div(class = "spacer12"),
     div(
      class = "placeholder-note",
      sprintf(
       "Strength is off, so each non-equal choice will be recorded with default strength = %s.",
       input$default_strength
      )
     )
    )
   }
  )
 })
 
 output$criteria_weights_preview_ui <- renderUI({
  crit <- rv$criteria
  
  if (nrow(crit) < 2) {
   return(div(class = "placeholder-note", "Add at least 2 criteria to compute weights."))
  }
  
  if (rv$criteria_progress_done < rv$criteria_progress_total) {
   return(
    div(
     class = "placeholder-note",
     sprintf(
      "Complete all criteria comparisons first (%d / %d done).",
      rv$criteria_progress_done,
      rv$criteria_progress_total
     )
    )
   )
  }
  
  wdf <- criteria_weights_df()
  
  if (is.null(wdf) || nrow(wdf) == 0) {
   return(div(class = "placeholder-note", "No criteria weights available yet."))
  }
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("Rank"),
     tags$th("Criterion"),
     tags$th("Weight")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(wdf)), function(i) {
     tags$tr(
      tags$td(i),
      tags$td(wdf$Criterion[i]),
      tags$td(sprintf("%.4f", wdf$Weight[i]))
     )
    })
   )
  )
 })
 
 output$criteria_eval_log_ui <- renderUI({
  logdf <- criteria_eval_log_df()
  
  if (is.null(logdf) || nrow(logdf) == 0) {
   return(div(class = "placeholder-note", "No criteria comparisons recorded yet."))
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