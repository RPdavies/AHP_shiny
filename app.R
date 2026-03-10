# app.R
# Minimal MVP "dashboard" UI for your AHP task prioritiser.
# UI only: no interactivity, no calculations.

library(shiny)
library(bslib)

ui <- page_fillable(
 theme = bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter")
 ),
 
 tags$style(HTML("
    .topbar { padding: 10px 12px; border-bottom: 1px solid rgba(0,0,0,0.08); }
    .topbar h4 { margin: 0; }
    .chip {
      display: inline-flex; align-items: center; gap: 8px;
      padding: 6px 10px; border-radius: 999px;
      background: rgba(0,0,0,0.05);
      font-size: 0.9rem;
    }
    .muted { color: rgba(0,0,0,0.55); }
    .tile {
      border: 1px solid rgba(0,0,0,0.10);
      border-radius: 14px;
      padding: 14px;
      background: rgba(255,255,255,0.70);
      min-height: 92px;
      display: flex;
      align-items: center;
      justify-content: center;
      text-align: center;
      font-weight: 600;
    }
    .vs { font-weight: 700; font-size: 1.2rem; opacity: 0.55; }
    .bigbtn .btn { padding: 14px 16px; font-weight: 600; }
    .btnrow { display: flex; gap: 10px; justify-content: center; flex-wrap: wrap; }
    .btnrow .btn { min-width: 140px; }
    .smalltable table { width: 100%; }
    .smalltable th, .smalltable td { padding: 6px 8px; border-bottom: 1px solid rgba(0,0,0,0.07); }
    .smalltable th { font-weight: 700; }
    .spacer8 { height: 8px; }
    .spacer12 { height: 12px; }
    .spacer16 { height: 16px; }
    .pick-tile > button {
  width: 100%;
  min-height: 110px;
  border-radius: 16px;
  border: 1px solid rgba(0,0,0,0.12);
  background: rgba(255,255,255,0.75);
  font-weight: 700;
  font-size: 1.2rem;
  padding: 16px;
}

.pick-tile > button:hover {
  background: rgba(0,0,0,0.03);
}

.pick-tile.equal > button {
  min-height: 110px;
  font-weight: 700;
  opacity: 0.9;
}

.pick-vs {
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 110px;
  font-weight: 800;
  font-size: 1.2rem;
  opacity: 0.45;
}
  ")),
 
 # --- Top bar ---
 div(
  class = "topbar",
  layout_columns(
   col_widths = c(4, 4, 4),
   div(
    h4("AHP Task Prioritiser (MVP UI)"),
    div(class = "muted", "Dashboard layout • UI elements only")
   ),
   div(
    style = "display:flex; align-items:center; justify-content:center; gap:10px;",
    span(class = "chip", "Status: ", strong("Ready")),
    span(class = "chip", "Tasks: ", strong("0"), span(class="muted", " (0 shortlisted)")),
    span(class = "chip", "Criteria: ", strong("0"))
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
 
 # --- Main columns ---
 layout_columns(
  col_widths = c(3, 5, 4),
  gap = "12px",
  
  # =========================
  # Left column: Inputs
  # =========================
  div(
   card(
    full_screen = FALSE,
    card_header("Tasks"),
    div(
     style = "display:flex; gap:8px; align-items:flex-end;",
     textInput("new_task", "New task", placeholder = "Type a task…"),
     actionButton("add_task", "Add Task", class = "btn-primary")
    ),
    accordion(
     accordion_panel(
      "Bulk add",
      textAreaInput("bulk_tasks", NULL, placeholder = "Paste tasks here (one per line, or comma-separated)…", rows = 5),
      div(style="display:flex; gap:8px;",
          actionButton("add_all", "Add All"),
          actionButton("clear_bulk", "Clear")
      ),
      div(class="muted", style="margin-top:6px;", "Preview: (not wired yet)")
     ),
     open = FALSE
    ),
    div(class="spacer12"),
    div(
     style="display:flex; gap:8px; align-items:center;",
     textInput("task_search", "Search", placeholder = "Filter tasks…"),
     checkboxInput("show_shortlist", "Show only ⭐ Today", value = FALSE)
    ),
    div(class="spacer8"),
    div(class="muted", "Tasks list (placeholder):"),
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
       tags$tr(tags$td("Example: Finish slides"), tags$td("Presentation"), tags$td("☐"), tags$td("🗑")),
       tags$tr(tags$td("Example: Review paper"), tags$td("Research"), tags$td("☑"), tags$td("🗑")),
       tags$tr(tags$td("Example: Admin emails"), tags$td("Admin"), tags$td("☐"), tags$td("🗑"))
      )
     )
    ),
    div(class="muted", style="margin-top:8px;",
        "In the real app this becomes an editable table (spreadsheet-style).")
   ),
   
   card(
    full_screen = FALSE,
    card_header("Criteria"),
    div(
     style = "display:flex; gap:8px; align-items:flex-end;",
     textInput("new_crit", "New criterion", placeholder = "e.g. Urgency"),
     actionButton("add_crit", "Add", class = "btn-primary")
    ),
    div(style="display:flex; gap:8px; flex-wrap:wrap; margin-top:8px;",
        actionButton("add_defaults", "Add suggested defaults"),
        actionButton("clear_criteria", "Clear criteria")
    ),
    div(class="spacer12"),
    div(class="muted", "Criteria list (placeholder):"),
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
    div(class="muted", style="margin-top:8px;",
        "Invert is useful for Effort/Difficulty where lower may mean do sooner.")
   )
  ),
  
  # =========================
  # Middle column: Workbench
  # =========================
  div(
   card(
    full_screen = TRUE,
    card_header(
     div(style="display:flex; justify-content:space-between; align-items:center; gap:10px; flex-wrap:wrap;",
         span("Pairwise comparison workbench"),
         div(style="display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
             checkboxInput("use_strength", "Use strength (1–9)", value = TRUE),
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
      div(class="spacer8"),
      div(class="chip", "Progress: 0 / 0 comparisons"),
      div(class="spacer12"),
      layout_columns(
       col_widths = c(5, 2, 5),
       div(class="tile", "Criterion A"),
       div(class="tile vs", "VS"),
       div(class="tile", "Criterion B")
      ),
      div(class="spacer12"),
      div(class="btnrow bigbtn",
          actionButton("a_wins_crit", "A wins", class="btn-outline-primary"),
          actionButton("equal_crit", "Equal", class="btn-outline-secondary"),
          actionButton("b_wins_crit", "B wins", class="btn-outline-primary")
      ),
      div(class="spacer12"),
      sliderInput("strength_crit", "Strength", min = 1, max = 9, value = 3, step = 1),
      div(class="spacer8"),
      div(style="display:flex; gap:8px; justify-content:center; flex-wrap:wrap;",
          actionButton("back_crit", "Back"),
          actionButton("next_crit", "Next", class="btn-primary"),
          actionButton("skip_crit", "Skip")
      ),
      div(class="spacer16"),
      div(class="muted", "Matrix preview / CR preview (placeholder)")
     ),
     
     nav_panel(
      "Task comparisons",
      div(class="spacer8"),
      div(
       style="display:flex; gap:10px; align-items:flex-end; flex-wrap:wrap;",
       selectInput("criterion_sel", "Criterion", choices = c("Urgency", "Importance", "Difficulty")),
       checkboxInput("shortlist_only", "Only compare ⭐ Today tasks", value = TRUE)
      ),
      div(class="chip", "Progress: 0 / 0 comparisons"),
      div(class="spacer12"),
      layout_columns(
       col_widths = c(5, 2, 5),
       div(class="tile", "Task A"),
       div(class="tile vs", "VS"),
       div(class="tile", "Task B")
      ),
      div(class="spacer12"),
      div(class="btnrow bigbtn",
          actionButton("a_wins_task", "A wins", class="btn-outline-primary"),
          actionButton("equal_task", "Equal", class="btn-outline-secondary"),
          actionButton("b_wins_task", "B wins", class="btn-outline-primary")
      ),
      div(class="spacer12"),
      sliderInput("strength_task", "Strength", min = 1, max = 9, value = 3, step = 1),
      div(class="spacer8"),
      div(style="display:flex; gap:8px; justify-content:center; flex-wrap:wrap;",
          actionButton("back_task", "Back"),
          actionButton("next_task", "Next", class="btn-primary"),
          actionButton("skip_task", "Skip")
      ),
      div(class="spacer16"),
      div(class="muted", "Completed comparisons list / matrix preview (placeholder)")
     )
    )
   )
  ),
  
  # =========================
  # Right column: Results
  # =========================
  div(
   card(
    card_header("Current ranking (placeholder)"),
    div(class="muted", "Updates live once calculations are wired."),
    div(class="spacer8"),
    div(
     class = "smalltable",
     tags$table(
      tags$thead(tags$tr(tags$th("#"), tags$th("Task"), tags$th("Global weight"))),
      tags$tbody(
       tags$tr(tags$td("1"), tags$td("Finish slides"), tags$td("0.31")),
       tags$tr(tags$td("2"), tags$td("Review paper"), tags$td("0.25")),
       tags$tr(tags$td("3"), tags$td("Admin emails"), tags$td("0.17"))
      )
     )
    ),
    div(class="spacer12"),
    div(style="display:flex; gap:8px; flex-wrap:wrap;",
        actionButton("pin_top3", "Pin top 3"),
        actionButton("export_csv", "Download CSV"),
        actionButton("copy_clip", "Copy")
    )
   ),
   
   card(
    card_header("Criteria weights (placeholder)"),
    div(
     tags$ul(
      tags$li(strong("Urgency: "), "0.58"),
      tags$li(strong("Importance: "), "0.28"),
      tags$li(strong("Difficulty: "), "0.14")
     )
    ),
    div(class="muted", "CR badges / cycle checks will appear here later.")
   ),
   
   card(
    card_header("Consistency diagnostics (placeholder)"),
    div(class="smalltable",
        tags$table(
         tags$thead(tags$tr(tags$th("Matrix"), tags$th("CR"), tags$th("OK?"))),
         tags$tbody(
          tags$tr(tags$td("Criteria"), tags$td("0.13"), tags$td("⚠")),
          tags$tr(tags$td("Urgency tasks"), tags$td("0.11"), tags$td("⚠")),
          tags$tr(tags$td("Importance tasks"), tags$td("0.03"), tags$td("✓"))
         )
        )
    ),
    actionButton("review_problem_pairs", "Review problem pairs")
   )
  )
 )
)

server <- function(input, output, session) {
 # Intentionally empty: UI-only MVP
}

shinyApp(ui, server)