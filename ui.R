
library(shiny)
library(shinythemes)
library(cli)

suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

# Set up the application ui
shinyUI(navbarPage("Serocalculator",
                   theme = shinythemes::shinytheme("united"),

                   # define the tabs to be used in the app ----------------------------------------
                   tabPanel("Import Data",
                            sidebarLayout(position = "left",
                                          sidebarPanel(width = 3,
                                                       h4("Data Upload"),
                                                       helpText("Use this section to upload the different types of data"),


                                                       #------------------ LOAD DATA ----------------------
                                                       # select data type
                                                       selectInput("file_name",
                                                                   "Choose Data:",
                                                                   choices = c("Pop Data","Curve Data","Noise Data"),
                                                                   selected = "Pop"
                                                       ),

                                                       # provide age name
                                                       textInput("age", "Age", "Age"),

                                                       # provide value column
                                                       textInput("value", "Value", "result"),

                                                       # provide index column name
                                                       textInput("index", "Index", "index_id"),

                                                       # upload file
                                                       fileInput(label = "Choose File from Computer (.csv, .xsls, .rda, .rds)","upload", NULL,
                                                                 buttonLabel = "Upload...",
                                                                 multiple = TRUE,
                                                                 accept = c(".csv", ".xlsx", ".rda","rds")),

                                                       # provide OSF URL
                                                       textInput("url_input", "Provide OSF URL:", value = "https://osf.io/download//n6cp3/"),

                                                       # fetch OSF data
                                                       actionButton("action_btn", "Upload"),

                                                       # progress bar
                                                       uiOutput("progress_bar"),

                                                       # select input widget for column selection
                                                       selectInput("updatedData", "Uploaded Data", choices = NULL),

                                          ),


                                          mainPanel("",
                                                    tabsetPanel(tabPanel("File Preview", uiOutput("head"),uiOutput("other_head")),
                                                    )
                                          )
                            )
                   ),

                   # ----------------------------------------- INSPECT DATA ----------------------------------------------
                   tabPanel("Inspect Data",
                            sidebarLayout(position = "left",
                                          sidebarPanel(width = 3,
                                                       h4("Available Data"),
                                                       helpText("This section allows the selection of uploaded data and visualize."),
                                                       # select input widget for column selection
                                                       selectInput("updatedData_ext", "Available Data to Choose", choices = NULL),
                                                       selectInput("file_out",
                                                                   "Choose Data Type:",
                                                                   choices = c("Population","Curve Param"),
                                                                   selected = "Population"
                                                       ),
                                                       uiOutput("sub_dropdown_ui"),
                                                       checkboxInput("check_stratify", "Stratify", value = TRUE),
                                                       checkboxInput("checklog", "Log", value = TRUE),
                                                       #textOutput("selected_options")
                                          ),
                                          mainPanel("",
                                                    tabsetPanel(tabPanel("Numeric Summary",uiOutput("numeric_summary")),
                                                                tabPanel("Visualize", plotOutput('visualize'))
                                                    )

                                          )
                            )
                   ),

                   # ----------------------------------------- ESTIMATE SEROINCIDENCE ------------------------------------
                   tabPanel("Estimate Seroincidence",
                            sidebarLayout(position = "left",
                                          sidebarPanel(width = 3,
                                                       h4("Data Upload"),
                                                       helpText("Use this section to upload")
                                          ),
                                          mainPanel("",
                                                    tabsetPanel(tabPanel("Estimate Seroincidence"),
                                                                tabPanel("Visualize")
                                                    )
                                          )
                            )
                   ),

                   # --------------------------------------- REPORT -------------------------------------------------------
                   navbarMenu("Report",
                              tabPanel("Rmd",DT::dataTableOutput("table"),
                                       icon = icon("file-code")),
                              tabPanel("R",icon = icon("code")))
)
)
