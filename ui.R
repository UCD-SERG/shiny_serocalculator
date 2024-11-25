
suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)

# Set the maximum request size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

# Set up the application ui
shinyUI(navbarPage(
  title = "Serocalculator",

  useShinyjs(),  # Initialize shinyjs

  # Add the busy spinner
  header = add_busy_spinner(
    spin = "atom",
    position = "top-right",
    margins = c(200, 800),
    timeout = 100
  ),
  theme = shinythemes::shinytheme("united"),

  # project summary
  tabPanel(
    "Summary",
    h2("Serocalculator"),
    htmlOutput("output_html"),
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
                    choices = c("Pop Data",
                                "Curve Data",
                                "Noise Data"),
                    selected = "Pop Data"
        ),

        # how to upload pop_data ("OSF" | "File Upload")
        uiOutput("pop_type"),

        # OSF or File Upload
        uiOutput("pop_upload_type"),

        uiOutput("average"),
        uiOutput("antigen"),
        uiOutput("y_low"),
        uiOutput("y_high"),
        uiOutput("eps"),
        uiOutput("nu"),
        uiOutput("provide_averages"),

        # provide age name
        uiOutput("select_age"),

        # select age
        uiOutput("age_selected"),

        # provide value column
        uiOutput("select_value"),

        # provide index column name
        uiOutput("select_id"),

        # progress bar
        uiOutput("progress_bar"),

        # select input widget for column selection
        selectInput("updatedData",
                    "Uploaded Data",
                    choices = NULL),

        actionButton("clear_btn", "Clear Environment"),
        textOutput("status")
      ),
      mainPanel(
        "",
        tabsetPanel(
          tabPanel("Data Requirements",
                   htmlOutput("data_requirement")),
          tabPanel(
            "File Preview",
            #DTOutput("head"),
            tableOutput("head"),
            DTOutput("other_head")
          ),
        )
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

        # choose 'yes' or 'no' for stratification
        uiOutput("stratification_radio"),

        # choose visualization stratification
        uiOutput("stratification"),

        # choose log
        uiOutput("log"),

      ),
      mainPanel(
        "",
        tabsetPanel(
          tabPanel("Numeric Summary", uiOutput("numeric_summary")),
          tabPanel("Visualize",
                   plotOutput("visualize"))
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

        # choose antigen_iso type
        uiOutput("antigen_type"),

        # choose stratification type
        radioButtons(
          inputId = "stratification_type",
          label = "Choose Stratification Type:",
          choices = list("Overall" = "overall", "Stratified" = "stratified"),
          selected = "overall"
        ),


        # choose stratification column
        uiOutput("stratify_by"),

        textOutput("status1"),

        # display computation results
        textOutput("result")
      ),
      mainPanel(
        "",
        tabsetPanel(
          tabPanel("Estimate Seroincidence",
                   tableOutput("est_incidence")))
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
