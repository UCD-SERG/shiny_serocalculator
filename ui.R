
library(shiny)
library(shinythemes)

# Set up the application ui
shinyUI(navbarPage("Serocalculator",
                   theme = shinythemes::shinytheme("united"),
# define the tabs to be used in the app ----------------------------------------
tabPanel("Data",sidebarLayout(position = "left",
                                   sidebarPanel(width = 3,
                                                h4("Data Upload"),
                                                helpText("Use this section to upload the different types of data"),


                                                #------------------ LOAD DATA ----------------------
                                                # select data type
                                                selectInput("file_name",
                                                            "Choose Data:",
                                                            choices = c("Pop Data",
                                                                        "Curve Data",
                                                                        "Noise Data",
                                                                        "DMCMC Data"),
                                                            selected = "Pop"
                                                ),
                                                fileInput(
                                                  label = "Upload Data",
                                                  "upload", NULL,
                                                  buttonLabel = "Upload...",
                                                  multiple = TRUE,
                                                  accept = c(".csv", ".xlsx", ".rda","rds")),

                                                # choose preview
                                                radioButtons("radio_display",
                                                             "Choose One",
                                                             c("preview", "summary","str"),
                                                             selected = "preview",
                                                             inline = TRUE),


                                                # Select input widget for column selection
                                                selectInput("updatedData", "Uploaded Data", choices = NULL),
                                   ),


                                   mainPanel("",
                                             tabsetPanel(
                                               tabPanel("Load File", uiOutput("radio_buttons")),
                                               tabPanel("Second"),
                                               tabPanel('View',fluidRow("",
                                                                        column(4,plotOutput('y0')),
                                                                        column(4,plotOutput('y1')),
                                                                        column(4,plotOutput('t1')),
                                                                        column(4,plotOutput('alpha')),
                                                                        column(4,plotOutput('r'))
                                               ),
                                               tabPanel('Step 3: Estimate Seroincidence'),
                                               tabPanel('Step 4: Export')))

                                   ))),
# manage
tabPanel("Manage",sidebarLayout(position = "left",
                                   sidebarPanel(width = 3,
                                                h4("Manage Data"),
                                                helpText("Use this section to upload the different types of data"),


                                                #------------------ LOAD DATA ----------------------

                                                # Select input widget for column selection
                                                selectInput("selectedData",
                                                            "Upload Data",
                                                            choices = NULL),

                                                # Select input widget for column selection
                                                uiOutput("column_selector"),
                                   ),


                                   mainPanel("",
                                             tabsetPanel(
                                               tabPanel("Load File"),
                                               tabPanel("Second"),
                                               tabPanel('View',fluidRow("",
                                                                        column(4,plotOutput('y0')),
                                                                        column(4,plotOutput('y1')),
                                                                        column(4,plotOutput('t1')),
                                                                        column(4,plotOutput('alpha')),
                                                                        column(4,plotOutput('r'))
                                               ),
                                               tabPanel('Step 3: Estimate Seroincidence'),
                                               tabPanel('Step 4: Export')))

                                   ))),

#report
#tabPanel("Report")
navbarMenu("Report",
           tabPanel("Rmd",DT::dataTableOutput("table"),
           icon = icon("file-code")),
           tabPanel("R",icon = icon("code"))),

))
