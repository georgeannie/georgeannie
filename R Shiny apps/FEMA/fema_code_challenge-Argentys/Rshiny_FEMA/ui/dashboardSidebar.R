dashboardSidebar = dashboardSidebar(
  tags$img(src="fema_logo_new.png",class='image'),

  sidebarMenu(
    id = "tabs",
    menuItem(" Data Insights", tabName = "insights", icon = icon("th")),
    menuItem("Decision Tool", tabName = "simulator", icon = icon("dashboard")),
    menuItem("Glossary", tabName = "glossary", icon = icon("list"))
  )
)
