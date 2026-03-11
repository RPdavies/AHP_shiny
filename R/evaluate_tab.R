evaluate_tab_ui <- function() {
 nav_panel(
  "Evaluate",
  
  navset_tab(
   nav_panel(
    "Criteria comparisons",
    
    layout_columns(
     col_widths = c(7, 5),
     selectInput(
      "criteria_reset_sel",
      "Criterion to revise",
      choices = character()
     ),
     div(
      class = "pt-31",
      actionButton(
       "clear_selected_criteria_comparisons",
       "Clear comparisons involving this criterion",
       class = "btn-outline-danger w-100"
      )
     )
    ),
    
    div(class = "spacer8"),
    uiOutput("criteria_progress_ui"),
    div(class = "spacer12"),
    uiOutput("criteria_compare_area_ui")
   ),
   
   nav_panel(
    "Task comparisons",
    
    layout_columns(
     col_widths = c(7, 5),
     selectInput(
      "criterion_sel",
      "Criterion",
      choices = character()
     ),
     checkboxInput("shortlist_only", "Only compare ⭐ Today tasks", value = TRUE)
    ),
    
    div(
     class = "btn-row",
     actionButton(
      "clear_task_comparisons_for_criterion",
      "Clear task comparisons for this criterion",
      class = "btn-outline-danger"
     )
    ),
    
    div(class = "spacer8"),
    uiOutput("task_progress_ui"),
    div(class = "spacer12"),
    uiOutput("task_compare_area_ui")
   )
  )
 )
}