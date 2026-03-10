setup_tab_ui <- function() {
 nav_panel(
  "Setup",
  
  layout_columns(
   col_widths = c(7, 5),
   gap = "12px",
   
   card(
    card_header("Tasks"),
    
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
      div(class = "muted mt-6", "Preview not wired yet")
     ),
     open = FALSE
    ),
    
    div(class = "spacer12"),
    
    layout_columns(
     col_widths = c(8, 4),
     textInput("task_search", "Search", placeholder = "Filter tasks..."),
     checkboxInput("show_shortlist", "Show only ⭐ Today", value = FALSE)
    ),
    
    div(class = "section-subtitle", "Tasks list"),
    div(
     class = "smalltable",
     tags$table(
      tags$thead(
       tags$tr(
        tags$th("Task"),
        tags$th("Tag"),
        tags$th("⭐ Today"),
        tags$th("")
       )
      ),
      tags$tbody(
       tags$tr(
        tags$td("Finish slides"),
        tags$td("Presentation"),
        tags$td("☐"),
        tags$td("🗑")
       ),
       tags$tr(
        tags$td("Review paper"),
        tags$td("Research"),
        tags$td("☑"),
        tags$td("🗑")
       ),
       tags$tr(
        tags$td("Admin emails"),
        tags$td("Admin"),
        tags$td("☐"),
        tags$td("🗑")
       )
      )
     )
    ),
    
    div(class = "spacer8"),
    div(class = "placeholder-note", "Later this can become an editable spreadsheet-style table.")
   ),
   
   div(
    class = "stack-col",
    
    card(
     card_header("Criteria"),
     
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
     
     div(class = "spacer12"),
     
     div(class = "section-subtitle", "Criteria list"),
     div(
      class = "smalltable",
      tags$table(
       tags$thead(
        tags$tr(
         tags$th("Criterion"),
         tags$th("Invert?"),
         tags$th("")
        )
       ),
       tags$tbody(
        tags$tr(tags$td("Urgency"), tags$td("☐"), tags$td("🗑")),
        tags$tr(tags$td("Importance"), tags$td("☐"), tags$td("🗑")),
        tags$tr(tags$td("Difficulty"), tags$td("☑"), tags$td("🗑"))
       )
      )
     ),
     
     div(class = "spacer8"),
     div(class = "placeholder-note", "Invert is useful for effort/difficulty-style criteria.")
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
   )
  )
 )
}