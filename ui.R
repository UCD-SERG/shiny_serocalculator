library(shiny)
library(shinythemes)
library(DT)

suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

# Set up the application ui
shinyUI(navbarPage("Serocalculator",
  theme = shinythemes::shinytheme("united"),

  # project summary
  tabPanel(
    "Summary",
    h2("Serocalculator"),
    tags$a("Project Website", href = "https://ucd-serg.github.io/serocalculator/"),
    helpText("Antibody levels measured in a cross–sectional population sample can be translated into an estimate of the frequency with which seroconversions (infections) occur in the sampled population. In other words, the presence of many high antibody titers indicates that many individuals likely experienced infection recently and the burden of disease is high in the population, while low titers indicate a low frequency of infections in the sampled population and therefore a lower burden of disease."),
    helpText("The serocalculator package was designed to use the longitudinal response characteristics using a set of modeled parameters characterizing the longitudinal response of the selected serum antibodies.")
  ),

  # define the tabs to be used in the app ----------------------------------------
  tabPanel(
    "Import Data",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Data Upload"),
        helpText("Use this section to upload the different types of data"),


        #------------------ LOAD DATA ----------------------
        # select data type
        selectInput("file_name",
          "Choose Data:",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          selected = "Pop"
        ),

        # provide age name
        uiOutput("select_age"),

        # select age
        uiOutput("age_selected"),

        # provide value column
        uiOutput("select_value"),

        # provide index column name
        uiOutput("select_id"),

        # upload file
        fileInput(
          label = "Choose File from Computer (.csv, .xsls, .rda, .rds)", "upload", NULL,
          buttonLabel = "Upload...",
          multiple = TRUE,
          accept = c(".csv", ".xlsx", ".rda", "rds")
        ),

        # provide OSF URL
        textInput("url_input", "Provide OSF URL:", value = "https://osf.io/download//n6cp3/"),

        # fetch OSF data
        actionButton("action_btn", "Upload"),

        # progress bar
        uiOutput("progress_bar"),

        # select input widget for column selection
        selectInput("updatedData", "Uploaded Data", choices = NULL),
      ),
      mainPanel(
        "",
        tabsetPanel(tabPanel("File Preview", DTOutput("head"), DTOutput("other_head")), )
      )
    )
  ),

  # ----------------------------------------- INSPECT DATA ----------------------------------------------
  tabPanel(
    "Inspect Data",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Available Data"),

        # description
        helpText("This section allows the selection of uploaded data and visualize."),

        # select input widget for column selection
        selectInput("updatedData_ext", "Available Data to Choose", choices = NULL),

        # choose type of visualization
        uiOutput("choose_visualization"),

        # choose stratifying column
        uiOutput("stratify"),

        # choose log
        uiOutput("log"),
      ),
      mainPanel(
        "",
        tabsetPanel(
          tabPanel("Numeric Summary", uiOutput("numeric_summary")),
          tabPanel("Visualize", plotOutput("visualize"))
        )
      )
    )
  ),

  # ----------------------------------------- ESTIMATE SEROINCIDENCE ------------------------------------
  tabPanel(
    "Estimate Seroincidence",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,

        # title
        h4("Estimation Filters"),

        # description
        helpText("Provide the parameters for filtering estimation of seroincidence"),

        uiOutput("stratify_by"),

        # choose antigen
        uiOutput("choose_antigen"),

      ),

      mainPanel(
        "",
        tabsetPanel(tabPanel("Estimate Seroincidence", tableOutput("est_incidence")))
      )
    )
  ),

  # --------------------------------------- REPORT -------------------------------------------------------
  navbarMenu(
    "Report",
    tabPanel("Rmd", DT::dataTableOutput("table"),
      icon = icon("file-code")
    ),
    tabPanel("R", icon = icon("code"))
  )
))
