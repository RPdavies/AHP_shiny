results_tab_ui <- function() {
 nav_panel(
  "Results",
  
  navset_tab(
   nav_panel(
    "Summary",
    
    layout_columns(
     col_widths = c(7, 5),
     gap = "12px",
     
     card(
      card_header("Final ranking"),
      div(class = "muted", "Global task ranking from criteria weights × local task weights."),
      div(class = "spacer8"),
      div(class = "smalltable", uiOutput("global_ranking_ui"))
     ),
     
     card(
      card_header("Criteria weights (live)"),
      uiOutput("criteria_weights_preview_ui")
     )
    ),
    
    div(class = "spacer12"),
    
    card(
     card_header("Task × criterion heatmap"),
     div(
      class = "muted",
      "Rows are tasks ordered by global priority; columns are criteria ordered by criteria weight; cells are local task weights."
     ),
     div(class = "spacer8"),
     plotOutput("task_criterion_heatmap", height = "340px")
    )
   ),
   
   nav_panel(
    "Raw logs",
    
    layout_columns(
     col_widths = c(7, 5),
     gap = "12px",
     
     card(
      card_header("Criteria comparison log"),
      div(class = "smalltable", uiOutput("criteria_eval_log_ui"))
     ),
     
     card(
      card_header("Task comparison log"),
      selectInput(
       "results_criterion_sel",
       "Criterion",
       choices = character()
      ),
      div(class = "smalltable", uiOutput("task_eval_log_ui"))
     )
    ),
    
    div(class = "spacer12"),
    
    card(
     card_header("Task weights by criterion (raw table)"),
     div(
      class = "muted",
      "This is the same local-weight information that feeds the heatmap, shown as a plain table for checking."
     ),
     div(class = "spacer8"),
     div(class = "smalltable", uiOutput("task_weights_preview_ui"))
    )
   )
  )
 )
}