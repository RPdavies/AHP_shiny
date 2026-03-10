evaluate_server <- function(input, output, session, rv) {
 
 pair_key <- function(left_id, right_id) {
  ifelse(
   left_id <= right_id,
   paste(left_id, right_id, sep = "||"),
   paste(right_id, left_id, sep = "||")
  )
 }
 
 all_pairs_df <- function(ids) {
  if (length(ids) < 2) {
   return(data.frame(
    left_id = character(),
    right_id = character(),
    stringsAsFactors = FALSE
   ))
  }
  
  cmb <- t(combn(ids, 2))
  data.frame(
   left_id = cmb[, 1],
   right_id = cmb[, 2],
   stringsAsFactors = FALSE
  )
 }
 
 next_evaluation_id <- function() {
  rv$evaluation_counter <- rv$evaluation_counter + 1L
  sprintf("eval_%04d", rv$evaluation_counter)
 }
 
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
  total <- nrow(criteria_pairs_all())
  done <- total - nrow(criteria_pairs_remaining())
  
  rv$criteria_progress_total <- total
  rv$criteria_progress_done <- done
 })
 
 observe({
  crit <- rv$criteria
  
  if (nrow(crit) == 0) {
   updateSelectInput(session, "criterion_sel", choices = character(), selected = character())
  } else {
   updateSelectInput(
    session,
    "criterion_sel",
    choices = stats::setNames(crit$id, crit$label),
    selected = crit$id[1]
   )
  }
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
   input$strength_crit
  } else {
   input$default_strength
  }
  
  rv$evaluations <- rbind(
   rv$evaluations,
   new_evaluation_row(
    id = next_evaluation_id(),
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
     tags$div("You can now move on to task comparisons next.")
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
     sliderInput("strength_crit", "Strength", min = 1, max = 9, value = 3, step = 1)
    )
   } else {
    tagList(
     div(class = "spacer12"),
     div(class = "placeholder-note", sprintf(
      "Strength is off, so each non-equal choice will be recorded with default strength = %s.",
      input$default_strength
     ))
    )
   }
  )
 })
}