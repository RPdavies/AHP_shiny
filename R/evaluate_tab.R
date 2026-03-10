evaluate_tab_ui <- function() {
 nav_panel(
  "Evaluate",
  
  navset_tab(
   nav_panel(
    "Criteria comparisons",
    
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
    
    div(class = "placeholder-note", "Task comparisons will be wired next.")
   )
  )
 )
}