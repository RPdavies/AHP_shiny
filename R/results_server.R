results_server <- function(input, output, session, rv) {
 
 tasks_for_global <- reactive({
  tasks <- rv$tasks
  
  if (nrow(tasks) == 0) {
   return(tasks)
  }
  
  if (isTRUE(input$shortlist_only)) {
   tasks <- tasks[tasks$today, , drop = FALSE]
  }
  
  tasks
 })
 
 criteria_weights_named <- reactive({
  crit <- rv$criteria
  if (nrow(crit) < 2) {
   return(NULL)
  }
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "criteria", , drop = FALSE]
  
  crit_obj <- build_pcm(crit[, c("id", "label"), drop = FALSE], ev)
  
  if (is.null(crit_obj) || !isTRUE(crit_obj$complete)) {
   return(NULL)
  }
  
  pcm <- crit_obj$pcm
  eig <- eigen(pcm)
  w <- Re(eig$vectors[, 1])
  w <- abs(w)
  w <- w / sum(w)
  names(w) <- rownames(pcm)
  w
 })
 
 local_task_weights_matrix <- reactive({
  crit <- rv$criteria
  tasks <- tasks_for_global()
  
  if (nrow(crit) == 0 || nrow(tasks) < 2) {
   return(NULL)
  }
  
  task_labels <- tasks$label
  crit_labels <- crit$label
  
  mat <- matrix(
   NA_real_,
   nrow = length(task_labels),
   ncol = length(crit_labels),
   dimnames = list(task_labels, crit_labels)
  )
  
  valid_task_ids <- tasks$id
  
  for (i in seq_len(nrow(crit))) {
   cid <- crit$id[i]
   clab <- crit$label[i]
   
   ev <- rv$evaluations
   ev <- ev[ev$level == "task" & ev$criterion_id == cid, , drop = FALSE]
   
   if (nrow(ev) > 0) {
    ev <- ev[
     ev$left_id %in% valid_task_ids & ev$right_id %in% valid_task_ids,
     ,
     drop = FALSE
    ]
   }
   
   obj <- build_pcm(tasks[, c("id", "label"), drop = FALSE], ev)
   
   if (is.null(obj) || !isTRUE(obj$complete)) {
    return(NULL)
   }
   
   pcm <- obj$pcm
   eig <- eigen(pcm)
   w <- Re(eig$vectors[, 1])
   w <- abs(w)
   w <- w / sum(w)
   names(w) <- rownames(pcm)
   
   mat[names(w), clab] <- as.numeric(w)
  }
  
  mat
 })
 
 global_weights_df <- reactive({
  crit_w <- criteria_weights_named()
  local_mat <- local_task_weights_matrix()
  
  if (is.null(crit_w) || is.null(local_mat)) {
   return(NULL)
  }
  
  common_criteria <- intersect(names(crit_w), colnames(local_mat))
  if (length(common_criteria) == 0) {
   return(NULL)
  }
  
  crit_w <- crit_w[common_criteria]
  local_mat <- local_mat[, common_criteria, drop = FALSE]
  
  global_w <- as.numeric(local_mat %*% crit_w)
  names(global_w) <- rownames(local_mat)
  global_w <- global_w / sum(global_w)
  
  out <- data.frame(
   Task = names(global_w),
   GlobalWeight = as.numeric(global_w),
   stringsAsFactors = FALSE
  )
  
  out <- out[order(out$GlobalWeight, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
 })
 
 output$global_ranking_ui <- renderUI({
  crit_w <- criteria_weights_named()
  tasks <- tasks_for_global()
  
  if (nrow(rv$criteria) < 2) {
   return(div(class = "placeholder-note", "Add at least 2 criteria first."))
  }
  
  if (is.null(crit_w)) {
   return(div(class = "placeholder-note", "Complete all criteria comparisons first."))
  }
  
  if (nrow(tasks) < 2) {
   return(div(class = "placeholder-note", "Not enough tasks in the current comparison set."))
  }
  
  gdf <- global_weights_df()
  
  if (is.null(gdf)) {
   return(
    div(
     class = "placeholder-note",
     "Complete all task comparisons for every criterion in the current task set before the final ranking can be computed."
    )
   )
  }
  
  tags$table(
   class = "table table-sm",
   tags$thead(
    tags$tr(
     tags$th("#"),
     tags$th("Task"),
     tags$th("Global weight")
    )
   ),
   tags$tbody(
    lapply(seq_len(nrow(gdf)), function(i) {
     tags$tr(
      tags$td(i),
      tags$td(gdf$Task[i]),
      tags$td(sprintf("%.4f", gdf$GlobalWeight[i]))
     )
    })
   )
  )
 })
}