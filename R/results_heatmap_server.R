results_heatmap_server <- function(input, output, session, rv) {
 
 tasks_for_global_heatmap <- reactive({
  tasks <- rv$tasks
  
  if (nrow(tasks) == 0) {
   return(tasks)
  }
  
  if (isTRUE(input$shortlist_only)) {
   tasks <- tasks[tasks$today, , drop = FALSE]
  }
  
  tasks
 })
 
 criteria_weights_named_heatmap <- reactive({
  crit <- rv$criteria
  if (nrow(crit) < 2) {
   return(NULL)
  }
  
  ev <- rv$evaluations
  ev <- ev[ev$level == "criteria", , drop = FALSE]
  
  obj <- build_pcm(crit[, c("id", "label"), drop = FALSE], ev)
  if (is.null(obj) || !isTRUE(obj$complete)) {
   return(NULL)
  }
  
  pcm <- obj$pcm
  eig <- eigen(pcm)
  w <- Re(eig$vectors[, 1])
  w <- abs(w)
  w <- w / sum(w)
  names(w) <- rownames(pcm)
  w
 })
 
 local_task_weights_matrix_heatmap <- reactive({
  crit <- rv$criteria
  tasks <- tasks_for_global_heatmap()
  
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
 
 global_weights_named_heatmap <- reactive({
  crit_w <- criteria_weights_named_heatmap()
  local_mat <- local_task_weights_matrix_heatmap()
  
  if (is.null(crit_w) || is.null(local_mat)) {
   return(NULL)
  }
  
  common_criteria <- intersect(names(crit_w), colnames(local_mat))
  if (length(common_criteria) == 0) {
   return(NULL)
  }
  
  crit_w <- crit_w[common_criteria]
  local_mat <- local_mat[, common_criteria, drop = FALSE]
  
  g <- as.numeric(local_mat %*% crit_w)
  names(g) <- rownames(local_mat)
  g <- g / sum(g)
  g
 })
 
 output$task_criterion_heatmap <- renderPlot({
  crit_w <- criteria_weights_named_heatmap()
  local_mat <- local_task_weights_matrix_heatmap()
  global_w <- global_weights_named_heatmap()
  
  if (nrow(rv$criteria) < 2) {
   plot.new()
   text(0.5, 0.5, "Add at least 2 criteria first.")
   return()
  }
  
  if (is.null(crit_w)) {
   plot.new()
   text(0.5, 0.5, "Complete all criteria comparisons first.")
   return()
  }
  
  tasks <- tasks_for_global_heatmap()
  if (nrow(tasks) < 2) {
   plot.new()
   text(0.5, 0.5, "Not enough tasks in the current comparison set.")
   return()
  }
  
  if (is.null(local_mat) || is.null(global_w)) {
   plot.new()
   text(
    0.5, 0.5,
    "Complete all task comparisons for every criterion\nbefore the heatmap can be drawn."
   )
   return()
  }
  
  # order rows/cols by priority
  task_order <- names(sort(global_w, decreasing = TRUE))
  crit_order <- names(sort(crit_w, decreasing = TRUE))
  
  z <- local_mat[task_order, crit_order, drop = FALSE]
  
  nr <- nrow(z)
  nc <- ncol(z)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(mar = c(8, 10, 2, 2))
  
  image(
   x = seq_len(nc),
   y = seq_len(nr),
   z = t(z[nr:1, , drop = FALSE]),
   axes = FALSE,
   xlab = "",
   ylab = ""
  )
  
  axis(1, at = seq_len(nc), labels = crit_order, las = 2)
  axis(2, at = seq_len(nr), labels = rev(task_order), las = 2)
  box()
  
  title(main = "Task × criterion weights")
 }, res = 110)
}