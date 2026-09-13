source('ui/dashboardBody/tabItem_Insights.R')
source('ui/dashboardBody/tabItem_Simulator.R')
source('ui/dashboardBody/tabItem_glossary.R')
dashboardBody = dashboardBody(
    tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
   ),
  div(
      p('Operations Cadre Resource Planning')
      ),
  tabItems(
    tabItem_Simulator,
    tabItem_Insights,
    tabItem_glossary
  )
)
