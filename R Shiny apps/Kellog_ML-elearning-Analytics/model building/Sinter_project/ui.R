library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
set.seed(123)
source("sinter_function.R")

sinter= read_rename_sinter()
choice_feed = names(feed_data())


#radio button potential dependent
#1. choice of dependent var
radio_dep = radioButtons("radio_dep_choice", "Potential Dependent Variables Plots", 
                          choices = list("Histogram" = 1, 
                                         "Correlation matrix" =2,
                                         "Data Summary" = 3, 
                                         "Overall Plant Summary" =4),
                          selected = 1)

#Tab layouts
#1. NavBar Menu= Dependent variables
#a. Stationary layout - change panel based on choice

dependent_layout_row1 = fluidRow( 
    column(width=2, 
           wellPanel(radio_dep)
    ),
    column(width=10, 
             conditionalPanel(
                 condition="input.radio_dep_choice == 1", 
                 
                 fluidRow(selectInput("plant", "Select Plant:", 
                                           choices=c('Plant 1' =1, 
                                                     'Plant 2' =2, 
                                                     'Plant 3' = 3,
                                                     'Plant 4'  = 4))
                         ),
                 fluidRow(
                   box(title="", 
                     solidHeader = TRUE, width="auto", 
                     plotOutput("histogram", height="350") )
                 )),
             conditionalPanel(
                 condition=("input.radio_dep_choice == 2"), 
                 fluidRow(selectInput("plant_tab2", "Select Plant:", 
                                      choices=c('Plant 1' =1, 
                                                'Plant 2' =2, 
                                                'Plant 3' = 3,
                                                'Plant 4'  = 4))
                 ),
                 fluidRow(
                     box(title="", 
                         solidHeader = TRUE, width="auto",
                         plotOutput("scatter_matrix", height="380"))
                     ),
                 fluidRow(
                    box(title="", 
                     solidHeader = TRUE, width="auto",
                     plotOutput("correlation_matrix", width = "auto", height="400"))
                )),
             
             conditionalPanel(
               condition=("input.radio_dep_choice == 3"),
               fluidRow(selectInput("plant_tab3", "Select Plant:", 
                                    choices=c('Plant 1' = 1, 
                                              'Plant 2' = 2, 
                                              'Plant 3' = 3,
                                              'Plant 4' = 4))
               ),
               fluidRow(
                   DT::dataTableOutput("summary")
                 )
             ),
             conditionalPanel(
                condition=("input.radio_dep_choice== 4"),
                tabsetPanel(
                    tabPanel(id="overall", "Overall Plant Summary",
                             h3(style="color:navy; font-weight:bold", "Number of Observations"),
                             DT::dataTableOutput("data_no"), br(),
                             h3(style="color:navy; font-weight:bold", "Mean"),
                             DT::dataTableOutput("data_mean"), br(),
                             
                             h3(style="color:navy; font-weight:bold", "Standard Deviation"),
                             DT::dataTableOutput("data_sd"), br(),
                             h3(style="color:navy; font-weight:bold", "Number of missing data"),
                             DT::dataTableOutput("data_na")),
                    tabPanel("Sample Data",
                             fluidRow(selectInput("plant_tab4", "Select Plant:", 
                                                  choices=c('Plant 1' =1, 
                                                            'Plant 2' =2, 
                                                            'Plant 3' = 3,
                                                            'Plant 4' = 4))
                             ),
                             fluidRow(
                                 DT::dataTableOutput("data_plant")
                             )
                    )
                    
                ))
    )
)

section_select = selectInput("section_select", "Select Input Section of Sinter:",
                             choices = c("All" = 0,
                                          "Feed" = 1,
                                          "Ignition Hood" =2,
                                          "Sinter Bed" = 3,
                                          "Stack" = 4,
                                          "ESP"  = 5,
                                          "Cooler Bed"  = 6),
                            selected=0)

feed_choices=c("Select All", names(feed_data()))
ignition_choices=c("Select All", names(ignition_data()))
stack_choices =c("Select All", names(stack()))
sinter_choices = c("Select All", names(sinter_bed()))
esp_choices = c("Select All", names(esp()))
cooler_choices = c("Select All", names(cooler()))


independent_layout_row1 = fluidRow(
    tabsetPanel(
        tabPanel(id="ind_summary", "Summary Table",
                 fluidRow(column(width=3, section_select)),
                 fluidRow(column(width=12,
                    conditionalPanel(
                       condition="input.section_select == 0",
                       DT::dataTableOutput("all_input")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 1",
                        DT::dataTableOutput("data_feed")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 2",
                        DT::dataTableOutput("ignition_hood")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 3",
                        DT::dataTableOutput("sinter_bed")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 4",
                        DT::dataTableOutput("stack")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 5",
                        DT::dataTableOutput("esp")
                    ),
                    conditionalPanel(
                        condition="input.section_select == 6",
                        DT::dataTableOutput("cooler")
                    ))
                )
        ),
          tabPanel("Correlation",
                   column(3, 
                          wellPanel(
                              selectizeInput("in_feed", "Select variables for Feed",
                                        choices=feed_choices,
                                        multiple=TRUE, selected=feed_choices[1]),

                              selectizeInput("in_ignition", "Select variables for Ignition Hood",
                                         choices=ignition_choices,
                                         multiple=TRUE),

                              selectizeInput("in_sinter_bed", "Select variables for Sinter Bed",
                                      choices=sinter_choices,
                                      multiple=TRUE),

                             selectizeInput("in_stack", "Select variables for Stack",
                                 choices=stack_choices,
                                 multiple=TRUE),

                             selectizeInput("in_esp", "Select variables for ESP",
                                    choices=esp_choices,
                                     multiple=TRUE),

                             selectizeInput("in_cooler", "Select variables for Cooler Bed",
                                      choices=cooler_choices,
                                      multiple=TRUE)
                             ###add button
                             ) 
                        ),
                   column(9, 
                          plotOutput("corrPlot", height = 600, width="auto"),
                          uiOutput("warning")
                          )
          )
    )
)

model_layout = fluidRow(
    tabsetPanel(
        tabPanel("Linear Regression",
                 column(3,
                        wellPanel(radioButtons("lm_choice", "Linear model results", 
                                               choices=c('Correlation coefficients' = 1, 
                                                         'Model Performance' = 2),
                                               selected = 1))
                        ),
                 column(9,
                        conditionalPanel(
                            condition="input.lm_choice == 1",
                            DT::dataTableOutput("coeff_lm")
                        ),
                        conditionalPanel(
                          condition="input.lm_choice == 2",
                          column(7,
                             plotOutput("feature_imp_lm", height="400px", width="auto")
                          ),
                          column(4,
                             tableOutput("results_lm")
                          )  
                        )
                 )),
        tabPanel("Random Forest Model",
                 column(3,
                        wellPanel(radioButtons("model_rf", "Select the random forest model:", 
                                               choices=c('Using complete observations' = 1, 
                                                         'Removing columns with null values' = 2, 
                                                         'Final model' = 3),
                                               selected = 1))
                        ),
                 column(5,
                        
                        plotOutput("feature_imp", height="500px", width = "500px")
                 ),
                 column(4,
                        tableOutput("results")
                 )  
        ),
        tabPanel("XgBoost",
                 column(3,
                        wellPanel(radioButtons("model_xg", "Select the XgBoost model:", 
                                              choices=c('All observations' = 4), selected=4)
                        )),
                 column(5,
                        
                        plotOutput("feature_imp_xg", height="500px", width = "500px")
                 ),
                 column(4,
                        tableOutput("results_xg")
                 )  
        )
    )
)


#List the tabs
tab_dependent = tabPanel(id="taba", "Exploratory Analysis - Dependent Variables",
                         dependent_layout_row1)

tab_independent = tabPanel(id="tabb", "Exploratory Analysis - Independent Variables",
                         independent_layout_row1)

tab_model = tabPanel(id="tabb", "Model and Recommendation",
                     model_layout)


#Change background color of header
title_color=tags$head(tags$style(HTML('
            /* logo */
                .skin-black .main-header .logo {
                    background-color: #white;},                  ## #4B0082;},
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
title_logo=span(column(1, tags$img(src='Kellogg_logo.jpg', height='40', width='260', border='0', 
                                   style='margin-left:-10px; padding:0px 0px; margin-top:13px; 
                                   display:block; font-size:0')), 
                column(8, class="title-box",
                       tags$h2(class="primary-title", style='margin-top:20px; margin-left:470px; 
                       color:#4B0082; 
                               font-size:8', 
                               "SINTER PLANT ANALYSIS")
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
                              tab_independent,
                              tab_model
                          ) 
                      )
))
