results_tab_ui <- function() {
 nav_panel(
  "Results",
  
  layout_columns(
   col_widths = c(7, 5),
   gap = "12px",
   
   card(
    card_header("Final ranking"),
    
    div(class = "muted", "This will update once the AHP calculations are wired in."),
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
     class = "btn-row",
     actionButton("pin_top3", "Pin top 3"),
     actionButton("export_csv", "Download CSV"),
     actionButton("copy_clip", "Copy")
    )
   ),
   
   div(
    class = "stack-col",
    
    card(
     card_header("Criteria weights"),
     
     tags$ul(
      tags$li(strong("Urgency: "), "0.58"),
      tags$li(strong("Importance: "), "0.28"),
      tags$li(strong("Difficulty: "), "0.14")
     ),
     
     div(class = "placeholder-note", "Local task weights can also go here later.")
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
        tags$tr(tags$td("Criteria"), tags$td("0.13"), tags$td("⚠")),
        tags$tr(tags$td("Urgency tasks"), tags$td("0.11"), tags$td("⚠")),
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
}