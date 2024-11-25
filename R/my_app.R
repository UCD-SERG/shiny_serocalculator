
suppressWarnings(
  try(
    rm("registerShinyDebugHook", envir = as.environment("tools:rstudio")),
    silent = TRUE
  )
)


# Set the maximum request size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)


library(shiny)

# reactive object to hold upload population data
pop_data <- reactiveVal(NULL)

# reactive object to hold curve data
curve_data <- reactiveVal(NULL)

# reactive object to hold noise data
noise_data <- reactiveVal(NULL)

# reactive object to hold list of uploaded files
uploaded_files <- reactiveValues(files = character(0))

# reactive object to hold file name of uploaded data
filename <- reactiveVal(NULL)

# reactive object (data frame) for default noise values
noise_values <- reactiveValues(new_val = data.frame(
  antigen = character(),
  y_low = numeric(),
  y_high = numeric(),
  eps = numeric(),
  nu = numeric(),
  stringsAsFactors = FALSE
))


# file name ----
filename <- reactiveVal(NULL)

## data df
data_df <- reactiveVal(NULL)

# get uploaded column names ----
column_names <- reactive({
  # ensure data is uploaded
  req(input$upload)

  # load file
  null_file <- is.null(input$upload$datapath)

  if (null_file) {
    return(NULL)
  }

  # read file
  df <- vroom::vroom(input$upload$datapath, delim = ",")

  # get column names
  df %>% names()
})



shiny_serocalculator_app <- function(...) {

# Set up the application ui
ui <- shinyUI(navbarPage(
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


server <- function(input, output, session) {

  # reactive object to hold uploaded data
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)

    # Read the data based on the file extension
    switch(ext,
           "csv" = read.csv(input$upload$datapath),
           "rds" = readRDS(input$upload$datapath),
           return(NULL) # Return NULL for unsupported extensions
    )
  })

  # Assign class to uploaded data
  uploadedDF <- assign_class_to_data(data = data(),file_name = input$file_name)

  # Update the appropriate reactive value based on the class of uploadedDF
  if ("pop_data" %in% class(uploadedDF)) {
    pop_data(uploadedDF)
  } else if ("curve_data" %in% class(uploadedDF)) {
    curve_data(uploadedDF)
  } else if ("noise_data" %in% class(uploadedDF)) {
    noise_data(uploadedDF)
  }

  # Select how to get pop data
  observeEvent(input$file_name, {
    req(input$file_name)

    if (input$file_name == "Pop Data") {
      output$pop_type <- renderUI({
        selectInput(
          inputId = "pop_type",
          label = "Choose Type",
          choices = c("Upload", "OSF"),
          selected = "Upload"
        )
      })
    } else {
      # Ensures UI element is cleared for non "Pop Data" file names
      output$pop_type <- renderUI(NULL)
    }
  })

  # Dynamically render UI based on conditions
  output$pop_upload_type <- renderUI({
    req(input$pop_type) # Ensure pop_type is available
    req(input$file_name) # Ensure file_name is available

    if (input$file_name == "Pop Data") {
      if (input$pop_type == "Upload") {
        # File upload interface for Pop Data
        fileInput(
          inputId = "pop_upload",
          label = "Choose File from Computer (.csv, .rds)",
          buttonLabel = "Upload...",
          multiple = TRUE,
          accept = c(".csv", ".rds")
        )
      } else if (input$pop_type == "OSF") {
        # URL input and download button for OSF
        tagList(
          textInput(
            inputId = "pop_data_url",
            label = "Provide OSF URL:"
          ),
          actionButton(
            inputId = "pop_data_url_btn",
            label = "Download Data"
          )
        )
      }
    } else if (input$file_name == "Curve Data") {
      # File upload interface for Curve Data
      fileInput(
        inputId = "curve_upload",
        label = "Choose File from Computer (.csv, .rds)",
        buttonLabel = "Upload...",
        multiple = TRUE,
        accept = c(".csv", ".rds")
      )
    } else if (input$file_name == "Noise Data") {
      # File upload interface for Noise Data
      fileInput(
        inputId = "noise_upload",
        label = "Choose File from Computer (.csv, .rds)",
        buttonLabel = "Upload...",
        multiple = TRUE,
        accept = c(".csv", ".rds")
      )
    }
  })

  #  PROCESS INPUTS ----

  observeEvent(c(
    input$upload,
    input$file_name,
    data()
  ), {
    # Ensure a file is uploaded and data() exists
    req(input$upload)
    req(data()) # Ensure data() is not NULL

    # Naming and saving the file based on file_name
    if (input$file_name == "Pop Data") {
      named_file <- "Pop Data"

      # Store the uploaded file on the server as 'Pop Data.csv'
      write.csv(data(), file = paste0(named_file, ".csv"), row.names = FALSE)
    } else {
      # Use input$upload$name or fallback to a default name if it's NULL
      upload_name <- input$upload$name
      if (is.null(upload_name) || upload_name == "") {
        upload_name <- "uploaded_file" # Fallback filename
      }

      named_file <- paste0(input$file_name, " | Upload | ", upload_name)

      # Get the file name without the extension
      filename <- tools::file_path_sans_ext(upload_name)

      # Store the uploaded file on the server
      write.csv(data(), file = paste0(filename, ".csv"), row.names = FALSE)
    }

    # Add the named file to the list of uploaded files
    uploaded_files$files <- c(uploaded_files$files, named_file)

    # Append to the list of uploaded files
    uploaded_files$files <- c(
      uploaded_files$files,
      "Pop Data"
    )



    # Update dropdown lists for uploaded files
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    # updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
  })

  ## choose uploaded data ----
  observeEvent(input$statsSelectedData, {
    # ensure file is uploaded
    req(input$statsSelectedData)

    # split
    name_of_file <- statsSelectedData %>%
      strsplit(split = " | ")

    file_name <- name_of_file[[1]][4]

    # file sans extension
    file_sans_ext <- strsplit(x = file_name, split = "\\.")[[1]][1]

    # read data
    get_uploaded_data <- read.csv(paste0(file_sans_ext, ".csv"))
  })



  # # display data when the button is clicked ----
  # observeEvent(input$pop_data_url_btn, {
  #
  #   # Function to check if a URL is working
  #   check_url <- function(url) {
  #     # Make a GET request to the URL
  #     response <- tryCatch(
  #       {
  #         GET(url)
  #       },
  #       error = function(e) {
  #         return(NULL) # Return NULL if the URL causes an error
  #       }
  #     )
  #
  #     # Check if the response is valid
  #     if (!is.null(response) && status_code(response) == 200) {
  #       return(TRUE) # URL is valid and working
  #     } else {
  #       return(FALSE) # URL is not working or invalid
  #     }
  #   }
  #
  #   if (check_url(input$pop_data_url)) {
  #     withProgress(message = "Downloading...", value = 0, {
  #       for (i in 1:10) {
  #         incProgress(1 / 15)
  #
  #         pop_data(serocalculator::load_pop_data(file_path = input$pop_data_url))
  #
  #       }
  #     })
  #
  #     # create list of uploaded files
  #     provided_file <- strsplit(x = input$pop_data_url, split = "/")[[1]][6]
  #
  #     # append uploaded files
  #     uploaded_files$files <- c(
  #       uploaded_files$files,
  #       paste0(
  #         input$file_name,
  #         " | OSF | ",
  #         provided_file
  #       )
  #     )
  #
  #       # update drop down for uploaded data on secondary tab
  #       updateSelectInput(session,
  #         "updatedData_ext",
  #         choices = uploaded_files$files
  #       )
  #
  #       # update drop down for uploaded data on secondary tab
  #       updateSelectInput(session,
  #         "updatedData",
  #         choices = uploaded_files$files
  #       )
  #
  #       # display data
  #       output$head <- renderTable({
  #         if(!is.null(pop_data())){
  #           pop_data()
  #           #%>% head()
  #         }
  #       })
  #
  #
  #   } else {
  #     showNotification("URL provided is not valid", , type = "error")
  #   }
  # })
  #

  # Handle URL-based download ----
  observeEvent(input$pop_data_url_btn, {

    # Check if the URL is valid
    if (check_url(input$pop_data_url)) {
      # Download the file from the URL and store it in pop_data
      withProgress(message = "Downloading...", value = 0.1, {
        pop_data(serocalculator::load_pop_data(file_path = input$pop_data_url))
        incProgress(1)
      })

      # Append to the list of uploaded files
      uploaded_files$files <- c(
        uploaded_files$files,
        paste0("Pop Data")
      )

      # Update drop-down list for uploaded data
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)

      # Display the data in the table
      output$head <- renderTable({
        if (!is.null(pop_data())) {
          head(pop_data())
        }
      })
    } else {
      # Notify if the URL is invalid
      showNotification("URL provided is not valid", type = "error")
    }
  })

  # Observe the file upload for Curve Data
  observeEvent(input$curve_upload, {
    req(input$curve_upload) # Ensure that a file is uploaded

    # Update the uploaded_files with new files
    uploaded_files$files <- c(uploaded_files$files, "Curve Data")

    # Update the select input with the new list of uploaded files
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
  })

  # Observe the file upload for Noise Data
  observeEvent(input$noise_upload, {
    req(input$noise_upload) # Ensure that a file is uploaded

    # Update the uploaded_files with new files
    uploaded_files$files <- c(uploaded_files$files, "Noise Data")

    # Update the select input with the new list of uploaded files
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
  })

  # Observe the file upload for Pop Data
  observeEvent(input$pop_upload, {
    req(input$pop_upload) # Ensure that a file is uploaded

    # Update the uploaded_files with new files
    uploaded_files$files <- c(uploaded_files$files, "Pop Data")

    # Update the select input with the new list of uploaded files
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)

  })

  # Observe changes in updatedData and uploaded files
  observeEvent(c(
    input$updatedData,
    input$noise_upload,
    input$curve_upload,
    input$pop_upload
  ), {
    req(input$updatedData) # Ensure updatedData is available

    output$head <- renderTable({
      if (input$updatedData == "Noise Data") {
        # Check if a file has been uploaded for Noise Data
        req(input$noise_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$noise_upload)

        # Update the reactiveVal with the new noise data
        noise_data(df)

        head(noise_data()) # Display the head of the noise data
      } else if (input$updatedData == "Curve Data") {
        # Check if a file has been uploaded for Curve Data
        req(input$curve_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$curve_upload)

        # Update the reactiveVal with the new curve data
        curve_data(df)

        head(curve_data()) # Display the head of the curve data
      } else if (input$updatedData == "Pop Data") {
        # Check if a file has been uploaded for Pop Data
        req(input$pop_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$pop_upload)

        # Update the reactiveVal with the new pop data
        pop_data(df)

        head(pop_data()) # Display the head of the pop data
      }
    })
  })



  # check file uploaded
  observeEvent(c(input$updatedData_ext), {
    req(input$updatedData_ext)

    # uploaded file
    #file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

    output$choose_visualization <- renderUI({
      if (input$updatedData_ext == "Pop Data") {
        selectInput("type_visualization",
                    "Choose Type of Visualization",
                    choices = c("Density", "Age Scatter")
        )
      } else if (input$updatedData_ext == "Curve Data") {
        selectInput("type_visualization",
                    "Choose Type of Visualization",
                    choices = c("Distribution", "Decay"),
                    selected = "Distribution"
        )
      }
    })
  })

  ## check correct file is uploaded (pop, curve, noise)
  observeEvent(c(
    input$updatedData_ext,
    input$updatedData
  ), {
    output$log <- renderUI({

      # dynamically create drop down list of column name
      if (input$updatedData_ext == "Pop Data") {
        checkboxInput("check_log", "Log", value = TRUE)
      }
    })

    output$stratification_radio <- renderUI({

      # dynamically create drop down list of column name
      if (input$updatedData_ext == "Pop Data") {
        # choose stratification type
        radioButtons(
          inputId = "stratification_choice",
          label = "Do you want to stratify?",
          choices = list(
            "Yes" = "yes",
            "No" = "no"
          ),
          selected = "yes"
        )
      }
    })

    output$stratify_option <- renderUI({
      # dynamically create drop down list of column name
      if (input$updatedData_ext == "Pop Data") {
        checkboxInput("check_stratify",
                      "Stratify",
                      value = TRUE
        )
      }
    })

    # UI to select age column in drop-down
    output$select_age <- renderUI({
      if (!is.null(pop_data()) & input$file_name == "Pop Data") {
        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Dynamically create drop-down list of column names
        selectInput(
          "age_select",
          "Select Age Column:",
          choices = cols
        )
      }
    })

    # UI to select value column in drop-down
    output$select_value <- renderUI({
      if (!is.null(pop_data()) & input$file_name == "Pop Data") {
        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Dynamically create drop-down list of column names
        selectInput(
          "value_select",
          "Select Value Column:",
          choices = cols
        )
      }
    })

    # UI to select age column in drop-down
    output$select_id <- renderUI({
      if (!is.null(pop_data()) & input$file_name == "Pop Data") {
        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Dynamically create drop-down list of column names
        selectInput(
          "id_select",
          "Select Id Column:",
          choices = cols
        )
      }
    })

    output$other_head <- renderDT({
      # population data
      if (input$file_name == "Pop Data") {
        check_age <- all(is.element(column_names(), c("Country", "age", "ageCat")))

        if (!check_age) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.")
        } else {
          # display data
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      } else if (input$file_name == "Curve Data") {
        # curve data check
        curve_data_check <- any(is.element(column_names(), c("y0", "y1", "t1", "alpha")))

        if (!curve_data_check) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH CURVE DATA.")
        } else {
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      } else if (input$file_name == "Noise Data") {
        # noise data check
        noise_data_check <- any(is.element(column_names(), c("y.low", "eps", "y.high")))

        if (!noise_data_check) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH NOISE DATA.")
        } else {
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      }
    })
  })


  ## file numeric summary ----
  observeEvent(c(input$updatedData_ext,
                 input$noise_upload,
                 input$curve_upload,
                 input$pop_upload), {

                   req(input$updatedData_ext)

                   output$numeric_summary <- renderTable({
                     if(input$updatedData_ext == "Pop Data"){

                       pop_df <- read_data_file(input$pop_upload)
                       pop_data(pop_df)

                       pop_data() %>%
                         head() %>%
                         skimr::skim() %>%
                         yank("numeric")

                     } else if (input$updatedData_ext == "Noise Data"){

                       noise_df <- read_data_file(input$noise_upload)
                       noise_data(noise_df)

                       noise_data() %>%
                         head() %>%
                         skimr::skim() %>%
                         yank("numeric")

                     } else if (input$updatedData_ext == "Curve Data"){

                       curve_df <- read_data_file(input$curve_upload)
                       curve_data(curve_df)

                       curve_data() %>%
                         head() %>%
                         skimr::skim() %>%
                         yank("numeric")

                     }
                   })
                 })

  # stratify
  observeEvent(c(
    input$stratification_choice,
    input$updatedData_ext
  ), {
    req(input$stratification_choice)
    req(input$updatedData_ext)

    output$stratification <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      if (input$stratification_choice == "yes" && input$updatedData_ext == "Pop Data") {
        selectInput("choosen_stratification",
                    "Stratify By:",
                    #df %>% names(),
                    pop_data() %>% names(),
                    selected = "Country"
        )
      }
    })
  })

  observeEvent(c(
    input$updatedData_ext,
    input$stratify,
    input$check_log,
    input$type_visualization,
    input$choosen_stratification,
    input$stratification_choice,
    input$curve_upload,
    input$pop_data
  ), {

    ## visualization
    output$visualize <- renderPlot({
      viz_type <- input$type_visualization

      #### Distribution plot
      if (viz_type == "Distribution") {
        curve_df <- read_data_file(input$curve_upload)
        curve_data(curve_df)

        curve_data() %>%
          pivot_longer(
            cols = `y0`:`r`,
            names_to = "parameter",
            values_to = "value"
          ) %>%
          ggplot2::ggplot(aes(x = value)) +
          geom_density() +
          facet_grid(parameter ~ .) +
          scale_y_continuous(limits = c(0, 0.0075)) +
          scale_x_continuous(limits = c(0, 600)) +
          theme_minimal()

        #### Age Scatter plot
      } else if (viz_type == "Age Scatter") {
        if (input$stratification_choice == "yes") {
          # visualize age-scatter

          pop_df <- read_data_file(input$pop_upload)
          pop_data(pop_df)

          pop_data() %>% serocalculator:::autoplot.pop_data(
            type = "age-scatter",
            strata = input$choosen_stratification,
            log = input$check_log
          )
        } else {
          # visualize age-scatter without stratification

          pop_df <- read_data_file(input$pop_upload)
          pop_data(pop_df)

          pop_data() %>% serocalculator:::autoplot.pop_data(
            type = "age-scatter",
            strata = NULL,
            log = input$check_log
          )
        }

        #### Density plot
      } else if (viz_type == "Density") {
        if (input$stratification_choice == "yes") {
          # visualize density with stratification
          pop_df <- read_data_file(input$pop_upload)
          pop_data(pop_df)

          pop_data() %>% serocalculator:::autoplot.pop_data(
            type = "density",
            strata = input$choosen_stratification,
            log = input$check_log
          )
        } else {
          # visualize density without stratification
          pop_df <- read_data_file(input$pop_upload)
          pop_data(pop_df)

          pop_data() %>% serocalculator:::autoplot.pop_data(
            type = "density",
            strata = NULL,
            log = input$check_log
          )
        }

        #### Decay plot
      } else if (viz_type == "Decay") {
        # visualize decay
        curve_df <- read_data_file(input$curve_upload)
        curve_data(curve_df)

        curve_data() %>% serocalculator:::plot_curve_params_one_ab()
      }
    })
  })

  observeEvent(input$stratify_by, {
    # Show busy spinner by simulating a long computation
    Sys.sleep(3)
    output$result <- renderText("Calculation Complete")
  })

  #-----------------------------------------------------------------------------
  #                     NOISE
  #-----------------------------------------------------------------------------

  observeEvent(input$file_name, {
    req(input$file_name)

    if (input$file_name == "Noise Data") {
      output$average <- renderUI({
        radioButtons("noise_choice", "Do you want to use average values:",
                     choices = c(
                       "Yes" = "yes",
                       "No" = "no"
                     ),
                     selected = "no"
        )
      })
    } else if (input$file_name == "Curve Data") {
      output$average <- renderUI({
        NULL
      })
    } else if (input$file_name == "Pop Data") {
      output$average <- renderUI({
        NULL
      })
    }
  })

  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$y_low <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("y_low", "y low:", value = 0.479)
        } else if (input$noise_choice == "no") {
          fileInput(
            label = "Choose File from Computer (.csv, .rds)",
            "upload",
            NULL,
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", "rds")
          )
        }
      }
    })
  })

  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$y_high <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("y_high", "y high:", value = 5000000)
        }
      }
    })
  })

  # eps
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$eps <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("eps", "eps:", value = 0.259)
        }
      }
    })
  })

  # nu
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$nu <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("nu", "nu:", value = 2.60)
        }
      }
    })
  })

  # antigen
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$antigen <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          textInput("antigen", "antigen:", value = "HlyE_IgA")
        }
      }
    })
  })

  # action button
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$provide_averages <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          actionButton("set_average", "Set Averages")
        }
      }
    })
  })

  #-----------------------------------------------------------------------------
  #                     SEROCALCULATOR SUMMARY
  #-----------------------------------------------------------------------------

  output$output_html <- renderUI({
    HTML("<p>The <strong>serocalculator </strong>R package provides a rapid and computationally simple method for calculating seroconversion rates,
         as originally published in <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.3592> Simonsen (2009) </a></cite> and <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2012) </a></cite>,
         and further developed in subsequent publications by <cite><a href=https://www.sciencedirect.com/science/article/pii/S1755436514000371?via%3Dihub>de Graaf (2014)</a></cite>,
         <cite><a href=https://www.sciencedirect.com/science/article/pii/S1755436516300135?via%3Dihub>Teunis (2016)</a></cite>, and <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2020)</a>.</cite> </p>

         <p>In short, longitudinal seroresponses from confirmed cases with a known symptom onset date are
         assumed to represent the time course of human serum antibodies against a specific pathogen. Therefore, by using these longitudinal
         antibody dynamics with any cross–sectional sample of the same antibodies in a human population, an incidence estimate can be calculated.</p>

         <p>Further details on the methodology can be found on the  <a href=https://ucd-serg.github.io/serocalculator/articles/serocalculator.html> main package website.  </a></p>

         <p>This app provides a user-friendly interface to use the serocalculator methodology without the need for specialized coding knowledge.
         Users should follow the steps to: </p>


         <ul>
            <li> Import the required datasets</li>
            <li> Inspect their data</li>
            <li> Estimate seroincidence</li>
            <li> Prepare a report (optional)</li>
        </ul> </p>
         <p>Required datasets:
         <ul>
            <li> Cross-sectional population-based dataset with age and quantitative antibody results</li>
            <li> Noise parameters </li>
            <li> Longitudinal curve parameters </li>
        </ul></p>
         <p>If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on  <a href=https://github.com/UCD-SERG/serocalculator/issues> GitHub </p>")
  })

  output$data_requirement <- renderText({
    HTML("<p>Required datasets:

        <ul>
          <li> <strong>Cross-sectional Population Data (Pop Data)</strong>
                  <p>A dataset with one row per sample and columns for antigen isotype, quantitative antibody results, and age in years. Additional columns and variables can be included for stratification.</p>
          <li> <strong>Noise Data</strong>
                  <p>A dataset containing the following variables, specifying noise parameters for each antigen isotype.
                  Note that variable names <u>must</u> follow these guidelines. For more information see <a hfref=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2020)</a>.
                  <ul>
                    <li>antigen_iso: antigen isotype whose noise parameters are being specified on each row</li>
                    <li>nu: biological noise</li>
                    <li>y.low: Lower limit of detection of the antibody assay</li>
                    <li>y.high: Upper limit of detection of the antibody assay</li>
                    <li>eps: measurement noise</li>
                  </ul></p>
          <li><strong>Antibody Decay Curve Data</strong></li>
          <p>A data set containing antibody decay curve parameters fit using a Bayesian hierarchical framework obtaining predictive posterior samples using Markov chain Monte Carlo sampling. Note that variable names <u>must</u> follow these guidelines. For more information see <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.5322>Teunis (2012)</a></p>
            <ul>
              <li>y0: baseline antibody level</li>
              <li>y1: antibody peak level (ELISA units)</li>
              <li>t1: duration of infection</li>
              <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
              <li>r: shape factor of antibody decay</li>
            </ul>
        </ul>
        </p>
         <p>File limit: <strong>500MB</strong></p>")
  })

  #------------------------------------------------------------------------------
  #                     BUSY SPINNER
  #------------------------------------------------------------------------------

  observeEvent(input$stratify_by, {
    # Simulate a long-running task
    Sys.sleep(3)

    # Update the output
    output$result <- renderText("Task completed")
  })

  # -----------------------------------------------------------------------------
  #                     ESTIMATE SEROINCIDENCE
  # ----------------------------------------------------------------------------

  # defaults noise values
  noise_data_params <- serocalculator::load_noise_params("https://osf.io/download/h64cw")
  noise_data(noise_data_params)

  # noise value from uploaded file
  observeEvent(c(input$file_name, input$upload), {
    req(input$file_name)
    req(input$upload)

    if (input$file_name == "Noise Data") {
      noise_df <- read_data_file(input$noise_upload)
      noise_data(noise_df)

    }
  })

  # noise value from UI
  observeEvent(input$set_average, {
    new_row <- data.frame(
      antigen = input$antigen,
      y_low = input$y_low,
      y_high = input$y_high,
      eps = input$eps,
      nu = input$nu,
      stringsAsFactors = FALSE
    )

    noise_data_params <- noise_values$new_val
    noise_data(noise_data_params)
  })

  # choose antigen type
  observeEvent(input$updatedData_ext, {
    req(input$updatedData_ext)


    output$antigen_type <- renderUI({
      # uploaded file
      #file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

      df <- data()

      # column names
      antigen_types <- df$antigen_iso %>% unique()

      if (input$updatedData_ext == "Pop Data") {
        selectInput(
          inputId = "output_antigen",
          label = "Choose antigen Type:",
          choices = antigen_types,
          multiple = TRUE
        )
      }
    })
  })

  observeEvent(c(
    input$updatedData_ext,
    input$pop_upload,
    input$stratification_type
  ), {
    req(
      input$updatedData_ext,
      input$stratification_type
    )

    output$stratify_by <- renderUI({
      if (input$stratification_type == "stratified" && input$updatedData_ext %in% c("Pop Data") ) {
        # Fetch the uploaded data
        #g <- get_uploaded_data(input$updatedData_ext)

        pop_df <- read_data_file(input$pop_upload)
        pop_data(pop_df)

        # Column names excluding specific columns
        cols <- pop_data %>%
          select(where(~ !is.numeric(.))) %>%
          select(-antigen_iso) %>%
          names()

        # Conditionally display the selectInput based on available columns
        if (any(is.element(cols, c("age")))) {
          selectInput("stratify_by",
                      "Stratify By:",
                      choices = cols,
                      multiple = TRUE
          )
        } else {
          h4("No applicable columns for stratification found.")
        }
      }
    })
  })


  observeEvent(c(
    input$stratify_by,
    input$stratification_type,
    input$curve_upload,
    input$pop_upload,
    input$noise_upload
  ), {
    req(input$stratification_type)

    pop_df <- read_data_file(input$pop_upload)
    pop_data(pop_df)

    noise_df <- read_data_file(input$noise_upload)
    noise_data(noise_df)

    curve_df <- read_data_file(input$curve_upload)
    curve_data(pop_df)

    output$est_incidence <- renderTable({
      if (input$stratification_type == "stratified") {
        est <- serocalculator::est.incidence.by(
          pop_data = pop_data(),
          curve_params = curve_data(),
          noise_params = noise_data(),
          strata = input$stratify_by,
          antigen_isos = input$output_antigen,
          verbose = TRUE
        )
      } else if (input$stratification_type == "overall") {
        est <- serocalculator::est.incidence(
          pop_data = pop_data(),
          curve_params = curve_data(),
          noise_params = noise_data(),
          antigen_isos = input$output_antigen,
          verbose = TRUE
        )
      }

      summary(est)
    })
  })

  ## clear environment
  observeEvent(input$clear_btn, {
    req(input$clear_btn)
    #runjs("location.reload();")  # JavaScript to reload the page

    # set reactive objects to NULL
    pop_data(NULL)
    curve_data(NULL)
    noise_data(NULL)

    # clear enviroment
    rm(list = ls())

    #clear dropdown files
    uploaded_files$files <- setdiff(uploaded_files$files, "Pop Data")
    uploaded_files$files <- setdiff(uploaded_files$files, "Noise Data")
    uploaded_files$files <- setdiff(uploaded_files$files, "Curve Data")

    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)



    # clear outputs
    output$est_incidence <- renderTable({ NULL})
    output$stratify_by <- renderUI({NULL})
    output$antigen_type <- renderUI({NULL})
    output$visualize <- renderPlot({NULL})
    output$stratification <- renderUI({NULL})
    output$stratification <- renderUI({NULL})
    output$other_head <- renderTable({NULL})
    output$head <- renderTable({NULL})
    output$numeric_summary <- renderTable({NULL})


  })
}


shinyApp(ui, server)
}
