setup_tab_ui <- function() {
 nav_panel(
  "Setup",
  
  navset_tab(
   nav_panel(
    "Tasks",
    
    layout_columns(
     col_widths = c(7, 5),
     
     card(
      card_header("Add tasks"),
      
      layout_columns(
       col_widths = c(9, 3),
       textInput("new_task", "New task", placeholder = "Type a task..."),
       div(
        class = "pt-31",
        actionButton("add_task", "Add Task", class = "btn-primary w-100")
       )
      ),
      
      accordion(
       accordion_panel(
        "Bulk add",
        textAreaInput(
         "bulk_tasks",
         NULL,
         rows = 6,
         placeholder = "Paste tasks here, one per line or comma-separated..."
        ),
        div(
         class = "btn-row",
         actionButton("add_all", "Add All"),
         actionButton("clear_bulk", "Clear")
        ),
        div(class = "muted mt-6", "One task per line is easiest, but commas also work.")
       ),
       open = FALSE
      ),
      
      div(class = "spacer8"),
      div(class = "placeholder-note", "Add tasks one by one or paste them in bulk.")
     ),
     
     card(
      card_header("Task list"),
      
      layout_columns(
       col_widths = c(8, 4),
       textInput("task_search", "Search", placeholder = "Filter tasks..."),
       checkboxInput("show_shortlist", "Show only ⭐ Today", value = FALSE)
      ),
      
      div(class = "section-subtitle", "Tasks"),
      div(
       class = "smalltable",
       uiOutput("tasks_table_ui")
      ),
      
      div(class = "spacer8"),
      div(class = "placeholder-note", "You can now add, delete, search, and mark tasks for today.")
     )
    )
   ),
   
   nav_panel(
    "Criteria",
    
    layout_columns(
     col_widths = c(7, 5),
     
     div(
      class = "stack-col",
      
      card(
       card_header("Add criteria"),
       
       layout_columns(
        col_widths = c(9, 3),
        textInput("new_crit", "New criterion", placeholder = "e.g. Urgency"),
        div(
         class = "pt-31",
         actionButton("add_crit", "Add", class = "btn-primary w-100")
        )
       ),
       
       div(
        class = "btn-row",
        actionButton("add_defaults", "Add suggested defaults"),
        actionButton("clear_criteria", "Clear criteria")
       ),
       
       div(class = "spacer8"),
       div(class = "placeholder-note", "You can now add criteria and toggle whether they should be inverted.")
      ),
      
      card(
       card_header("General settings"),
       
       checkboxInput("use_strength", "Use strength (1-9)", value = TRUE),
       
       conditionalPanel(
        "input.use_strength == false",
        numericInput(
         "default_strength",
         "Default strength",
         value = 3,
         min = 1,
         max = 9,
         step = 1
        )
       ),
       
       checkboxInput("direction_only", "Direction-only consistency checks", value = FALSE),
       
       div(class = "spacer8"),
       div(class = "placeholder-note", "These settings will control the evaluation tab later.")
      )
     ),
     
     card(
      card_header("Criteria list"),
      
      div(class = "section-subtitle", "Criteria"),
      div(
       class = "smalltable",
       uiOutput("criteria_table_ui")
      ),
      
      div(class = "spacer8"),
      div(class = "placeholder-note", "Suggested defaults: Urgency, Importance, Difficulty.")
     )
    )
   )
  )
 )
}