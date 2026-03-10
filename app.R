library(shiny)
library(bslib)

source("R/setup_tab.R", local = TRUE)
source("R/evaluate_tab.R", local = TRUE)
source("R/results_tab.R", local = TRUE)

ui <- page_fillable(
 theme = bs_theme(
  version = 5,
  bootswatch = "flatly"
 ),
 
 tags$head(
  includeCSS("www/styles.css")
 ),
 
 div(
  class = "topbar",
  layout_columns(
   col_widths = c(4, 4, 4),
   
   div(
    h4("AHP Task Prioritiser"),
    div(class = "muted", "MVP • UI only")
   ),
   
   div(
    class = "topbar-center",
    span(class = "chip", "Status:", strong("Ready")),
    span(class = "chip", "Tasks:", strong("0")),
    span(class = "chip", "Criteria:", strong("0")),
    span(class = "chip", "Comparisons:", strong("0 / 0"))
   ),
   
   div(
    class = "topbar-right",
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
 
 div(
  class = "app-body",
  navset_card_tab(
   id = "main_tabs",
   height = "100%",
   
   setup_tab_ui(),
   evaluate_tab_ui(),
   results_tab_ui()
  )
 )
)

server <- function(input, output, session) {
 # UI only for now
}

shinyApp(ui, server)