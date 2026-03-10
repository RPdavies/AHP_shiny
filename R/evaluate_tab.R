evaluate_tab_ui <- function() {
 nav_panel(
  "Evaluate",
  
  navset_tab(
   nav_panel(
    "Criteria comparisons",
    
    div(class = "spacer8"),
    div(class = "muted", "Choose the winner by clicking the item itself."),
    div(class = "spacer8"),
    div(class = "chip", "Progress: 0 / 0 comparisons"),
    div(class = "spacer12"),
    
    layout_columns(
     col_widths = c(5, 2, 5),
     
     div(
      class = "pick-tile",
      actionButton("pick_left_crit", "Criterion A", class = "btn")
     ),
     
     div(
      class = "pick-tile equal",
      actionButton("pick_equal_crit", "Equal", class = "btn")
     ),
     
     div(
      class = "pick-tile",
      actionButton("pick_right_crit", "Criterion B", class = "btn")
     )
    ),
    
    conditionalPanel(
     "input.use_strength == true",
     div(class = "spacer12"),
     sliderInput("strength_crit", "Strength", min = 1, max = 9, value = 3, step = 1)
    ),
    
    div(class = "spacer12"),
    div(
     class = "btn-row center",
     actionButton("back_crit", "Back"),
     actionButton("next_crit", "Next", class = "btn-primary"),
     actionButton("skip_crit", "Skip")
    ),
    
    div(class = "spacer16"),
    div(class = "placeholder-note", "Matrix preview / CR preview will appear here later.")
   ),
   
   nav_panel(
    "Task comparisons",
    
    layout_columns(
     col_widths = c(7, 5),
     selectInput(
      "criterion_sel",
      "Criterion",
      choices = c("Urgency", "Importance", "Difficulty")
     ),
     checkboxInput("shortlist_only", "Only compare ⭐ Today tasks", value = TRUE)
    ),
    
    div(class = "muted", "Choose the winner by clicking the task itself."),
    div(class = "spacer8"),
    div(class = "chip", "Progress: 0 / 0 comparisons"),
    div(class = "spacer12"),
    
    layout_columns(
     col_widths = c(5, 2, 5),
     
     div(
      class = "pick-tile",
      actionButton("pick_left_task", "Task A", class = "btn")
     ),
     
     div(
      class = "pick-tile equal",
      actionButton("pick_equal_task", "Equal", class = "btn")
     ),
     
     div(
      class = "pick-tile",
      actionButton("pick_right_task", "Task B", class = "btn")
     )
    ),
    
    conditionalPanel(
     "input.use_strength == true",
     div(class = "spacer12"),
     sliderInput("strength_task", "Strength", min = 1, max = 9, value = 3, step = 1)
    ),
    
    div(class = "spacer12"),
    div(
     class = "btn-row center",
     actionButton("back_task", "Back"),
     actionButton("next_task", "Next", class = "btn-primary"),
     actionButton("skip_task", "Skip")
    ),
    
    div(class = "spacer16"),
    div(class = "placeholder-note", "Completed comparisons / matrix preview will appear here later.")
   )
  )
 )
}