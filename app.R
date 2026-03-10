# app.R
# MVP dashboard UI for an AHP-style task prioritiser
# UI only: no interactivity, no calculations yet

library(shiny)
library(bslib)

ui <- page_fillable(
 theme = bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter")
 ),
 
 tags$head(
  tags$style(HTML("
      html, body {
        height: 100%;
      }

      .topbar {
        padding: 10px 12px;
        border-bottom: 1px solid rgba(0,0,0,0.08);
        background: rgba(255,255,255,0.85);
      }

      .topbar h4 {
        margin: 0;
      }

      .muted {
        color: rgba(0,0,0,0.55);
      }

      .chip {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 6px 10px;
        border-radius: 999px;
        background: rgba(0,0,0,0.05);
        font-size: 0.9rem;
      }

      .smalltable table {
        width: 100%;
        margin-bottom: 0;
      }

      .smalltable th,
      .smalltable td {
        padding: 8px 10px;
        border-bottom: 1px solid rgba(0,0,0,0.07);
        vertical-align: middle;
      }

      .smalltable th {
        font-weight: 700;
      }

      .spacer8  { height: 8px; }
      .spacer12 { height: 12px; }
      .spacer16 { height: 16px; }

      .nav-tabs .nav-link {
        font-weight: 600;
      }

      .pick-tile > button {
        width: 100%;
        min-height: 120px;
        border-radius: 18px;
        border: 1px solid rgba(0,0,0,0.12);
        background: rgba(255,255,255,0.78);
        font-weight: 700;
        font-size: 1.25rem;
        padding: 18px;
        white-space: normal;
        line-height: 1.25;
      }

      .pick-tile > button:hover {
        background: rgba(0,0,0,0.03);
      }

      .pick-tile.equal > button {
        font-weight: 700;
        opacity: 0.9;
      }

      .placeholder-note {
        color: rgba(0,0,0,0.5);
        font-style: italic;
      }

      .section-subtitle {
        font-weight: 600;
        margin-bottom: 6px;
      }
    "))
 ),
 
 # ===== Top bar =====
 div(
  class = "topbar",
  layout_columns(
   col_widths = c(4, 4, 4),
   
   div(
    h4("AHP Task Prioritiser"),
    div(class = "muted", "MVP dashboard • UI only")
   ),
   
   div(
    style = "display:flex; align-items:center; justify-content:center; gap:10px; flex-wrap:wrap;",
    span(class = "chip", "Status:", strong("Ready")),
    span(class = "chip", "Tasks:", strong("0"), span(class = "muted", "(0 shortlisted)")),
    span(class = "chip", "Criteria:", strong("0")),
    span(class = "chip", "Comparisons:", strong("0 / 0"))
   ),
   
   div(
    style = "display:flex; align-items:center; justify-content:flex-end; gap:8px; flex-wrap:wrap;",
    radioButtons(
     "mode",
     label = NULL,
     choices = c("Daily", "Weekly"),
     selected = "Daily",
     inline = TRUE
    ),
    actionButton("save", "Save"),
    actionButton("load", "Load"),
    actionButton("reset", "Reset")
   )
  )
 ),
 
 # ===== Main layout =====
 layout_columns(
  col_widths = c(3, 5, 4),
  gap = "12px",
  
  # ------------------------------------------------------------------
  # LEFT COLUMN: Inputs
  # ------------------------------------------------------------------
  div(
   card(
    card_header("Tasks"),
    
    layout_columns(
     col_widths = c(9, 3),
     textInput("new_task", "New task", placeholder = "Type a task..."),
     div(style = "padding-top: 31px;",
         actionButton("add_task", "Add Task", class = "btn-primary w-100"))
    ),
    
    accordion(
     accordion_panel(
      "Bulk add",
      textAreaInput(
       "bulk_tasks",
       NULL,
       rows = 5,
       placeholder = "Paste tasks here, one per line or comma-separated..."
      ),
      div(
       style = "display:flex; gap:8px; flex-wrap:wrap;",
       actionButton("add_all", "Add All"),
       actionButton("clear_bulk", "Clear")
      ),
      div(class = "muted", style = "margin-top:6px;", "Preview not wired yet")
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
    div(class = "placeholder-note", "Later this will become an editable spreadsheet-like table.")
   ),
   
   card(
    card_header("Criteria"),
    
    layout_columns(
     col_widths = c(9, 3),
     textInput("new_crit", "New criterion", placeholder = "e.g. Urgency"),
     div(style = "padding-top: 31px;",
         actionButton("add_crit", "Add", class = "btn-primary w-100"))
    ),
    
    div(
     style = "display:flex; gap:8px; flex-wrap:wrap;",
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
       tags$tr(tags$td("Urgency"),    tags$td("☐"), tags$td("🗑")),
       tags$tr(tags$td("Importance"), tags$td("☐"), tags$td("🗑")),
       tags$tr(tags$td("Difficulty"), tags$td("☑"), tags$td("🗑"))
      )
     )
    ),
    
    div(class = "spacer8"),
    div(class = "placeholder-note", "Invert is useful for effort/difficulty-style criteria.")
   )
  ),
  
  # ------------------------------------------------------------------
  # MIDDLE COLUMN: Pairwise workbench
  # ------------------------------------------------------------------
  div(
   card(
    full_screen = TRUE,
    
    card_header(
     layout_columns(
      col_widths = c(8, 4),
      
      div(
       strong("Pairwise comparison workbench"),
       div(class = "muted", "Choose the winner directly by clicking the item itself.")
      ),
      
      div(
       style = "display:flex; justify-content:flex-end; align-items:center; gap:12px; flex-wrap:wrap;",
       checkboxInput("use_strength", "Use strength (1-9)", value = TRUE),
       conditionalPanel(
        "input.use_strength == false",
        numericInput("default_strength", "Default strength", value = 3, min = 1, max = 9, step = 1)
       )
      )
     )
    ),
    
    navset_tab(
     nav_panel(
      "Criteria comparisons",
      
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
      
      div(class = "spacer8"),
      div(
       style = "display:flex; gap:8px; justify-content:center; flex-wrap:wrap;",
       actionButton("back_crit", "Back"),
       actionButton("next_crit", "Next", class = "btn-primary"),
       actionButton("skip_crit", "Skip")
      ),
      
      div(class = "spacer16"),
      div(class = "placeholder-note", "Matrix preview / CR preview will appear here later.")
     ),
     
     nav_panel(
      "Task comparisons",
      
      div(class = "spacer8"),
      layout_columns(
       col_widths = c(7, 5),
       selectInput(
        "criterion_sel",
        "Criterion",
        choices = c("Urgency", "Importance", "Difficulty")
       ),
       checkboxInput("shortlist_only", "Only compare ⭐ Today tasks", value = TRUE)
      ),
      
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
      
      div(class = "spacer8"),
      div(
       style = "display:flex; gap:8px; justify-content:center; flex-wrap:wrap;",
       actionButton("back_task", "Back"),
       actionButton("next_task", "Next", class = "btn-primary"),
       actionButton("skip_task", "Skip")
      ),
      
      div(class = "spacer16"),
      div(class = "placeholder-note", "Completed comparisons / matrix preview will appear here later.")
     )
    )
   )
  ),
  
  # ------------------------------------------------------------------
  # RIGHT COLUMN: Results
  # ------------------------------------------------------------------
  div(
   card(
    card_header("Current ranking"),
    
    div(class = "muted", "This will update live once the calculations are wired in."),
    div(class = "spacer8"),
    
    div(
     class = "smalltable",
     tags$table(
      tags$thead(
       tags$tr(
        tags$th("#"),
        tags$th("Task"),
        tags$th("Global weight")
       )
      ),
      tags$tbody(
       tags$tr(tags$td("1"), tags$td("Finish slides"), tags$td("0.31")),
       tags$tr(tags$td("2"), tags$td("Review paper"), tags$td("0.25")),
       tags$tr(tags$td("3"), tags$td("Admin emails"), tags$td("0.17"))
      )
     )
    ),
    
    div(class = "spacer12"),
    div(
     style = "display:flex; gap:8px; flex-wrap:wrap;",
     actionButton("pin_top3", "Pin top 3"),
     actionButton("export_csv", "Download CSV"),
     actionButton("copy_clip", "Copy")
    )
   ),
   
   card(
    card_header("Criteria weights"),
    
    tags$ul(
     tags$li(strong("Urgency: "), "0.58"),
     tags$li(strong("Importance: "), "0.28"),
     tags$li(strong("Difficulty: "), "0.14")
    ),
    
    div(class = "placeholder-note", "CR badges / cycle checks can go here later.")
   ),
   
   card(
    card_header("Consistency diagnostics"),
    
    div(
     class = "smalltable",
     tags$table(
      tags$thead(
       tags$tr(
        tags$th("Matrix"),
        tags$th("CR"),
        tags$th("OK?")
       )
      ),
      tags$tbody(
       tags$tr(tags$td("Criteria"),         tags$td("0.13"), tags$td("⚠")),
       tags$tr(tags$td("Urgency tasks"),    tags$td("0.11"), tags$td("⚠")),
       tags$tr(tags$td("Importance tasks"), tags$td("0.03"), tags$td("✓"))
      )
     )
    ),
    
    div(class = "spacer8"),
    actionButton("review_problem_pairs", "Review problem pairs")
   )
  )
 )
)

server <- function(input, output, session) {
 # UI only for now
}

shinyApp(ui, server)