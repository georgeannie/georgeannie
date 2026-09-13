library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
source("sinter_function.R")

sinter= read_rename_sinter()

#PLOTS
#Correlation plot for dependent variables                                
corr_plot_dep= plotOutput('correlation_dependent', height="400px", width = 'auto')


#Histogram dependent
hist_dep  =   plotOutput('hist_dependent', width = "auto", height="350px")

#Scatter plot - dependent vs kpi
scatter_dep =plotOutput('scatter_dependent', width = "auto", height="350px")

#radio button potential dependent
#1.List only required independent variables
dep_var=names(sinter)[c(3, 5, 7:9)]
choice_dep = c("All", dep_var[-1])



#4. 
sidebar = sidebarLayout(
  sidebarMenu(
    menuItem("Histogram", tabName = "histogram", startExpanded = FALSE),
    menuItem("Correlation", tabName = "correlation")
  )
)
#Tab layouts
#1. NavBar Menu= Dependent variables
#a. Stationary layout
dependent_layout_row1 = fluidRow( 
  column(width=2, 
         sidebar
  ),
  column(width=10, 
         plotOutput("dependent_plot", height=400, width=970)
      )
)

#2. Navbar - Independent variables
#a. List only required independent variables
ind_var=names(sinter)[c(2:3, 11:19)]

#b. choice of independent var
radio_but_indep_var = radioButtons("radio_choice", "Potential Dependent Variables", 
                                   choices = ind_var,
                                   selected = ind_var[1])

#choice of plots
plot_type=selectInput("select_plot", "Type of Plot", 
                      choices = c("Correlation Plot/Histogram", "Scatter Plot (numerical variables)",
                                  "Box Plot (categorical variables)",
                                  "Box plot"))


model_layout = fluidRow()


#List the tabs
tab_dependent = tabsetPanel(
        tabPanel(id="taba", "Exploratory Analysis",
                         dependent_layout_row1)

tab_model = tabPanel("Model and Recommendation",
                     sidebarLayout(
                       sidebarPanel(
                         sidebarMenu (
                            menuItem("Histogram", tabname="Histogram"),
                       mainPanel(
                         plot("Attacks")
                       )
                     )
                     model_layout)


#Change background color of header
title_color=tags$head(tags$style(HTML('
            /* logo */
                .skin-black .main-header .logo {
                    background-color: #4B0082;},
            /* logo when hovered */
                .skin-black .main-header .logo:hover {
                    background-color: #4B0082;}
                            ')))

#Increase height of header
header_height=tags$li(class = "dropdown",
                      tags$style(".main-header {max-height: 580px}"),
                      tags$style(".main-header .logo {height: 70px}")
)

#Add logo to header
title_logo=span(column(1, tags$img(src='logo.jpg', height='73', width='220', border='0', 
                                   style='margin-left:-2px; padding:0; margin:0; display:block; font-size:0')), 
                column(8, class="title-box",
                       tags$h2(class="primary-title", style='margin-top:20px; margin-left:20px; color:white; 
                               font-size:8', 
                               "SINTER PLANT PROJECT")
                )
)


#DASHBOARDING
shinyUI(dashboardPage(skin="black", 
                      dashboardHeader(header_height, title = title_logo,
                                      titleWidth='100%'),
                      
                      dashboardSidebar(disable = TRUE),
                      
                      dashboardBody(
                        title_color,
                        navbarPage(tags$h4("Sinter plant Viz", 
                                           style='margin-top:2px; margin-left:10px; color:black; 
                                      font-size:10; font-weight:bold'),     
                                  tab_dependent,
                                   tab_model
                        ) 
                      )
))
