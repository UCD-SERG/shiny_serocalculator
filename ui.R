
library(shiny)
library(shinythemes)

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

                                    # upload file
                                    fileInput(label = "Upload Data","upload", NULL,
                                              buttonLabel = "Upload...",
                                              multiple = TRUE,
                                              accept = c(".csv", ".xlsx", ".rda","rds")),

                                    # Select input widget for column selection
                                    selectInput("updatedData", "Uploaded Data", choices = NULL),
                                   ),


                      mainPanel("",
                                tabsetPanel(tabPanel("File Preview", tableOutput("head")),
                                            tabPanel("Numeric Summary",uiOutput("numeric_summary")),
                                            )
                      )
            )
          ),

# ----------------------------------------- INSPECT DATA ----------------------------------------------
tabPanel("Inspect Data",
         sidebarLayout(position = "left",
                       sidebarPanel(width = 3,
                                    h4("Data Upload"),
                                    helpText("This section allows for data upload")
                                    ),
                       mainPanel("",
                                 tabsetPanel(tabPanel("Numeric Summary"),
                                             tabPanel("Visualize")
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
