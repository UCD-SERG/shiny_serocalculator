




#ui <- fluidPage(
#  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
#  tableOutput("files")
#)


u <- shinyUI(fluidPage(
  titlePanel("Serocalculator"),
  sidebarLayout(position = "left",
                sidebarPanel("",
                             selectInput(inputId = "inpx",label = "Choose data of type:", choices = c("rds |rda |rdata","csv","rds","xslx","OSF (url)"), selected = "csv"),
                             checkboxInput('change_sign','Add/edit data description'),
                             checkboxInput('change_sign','Rename data'),
                             radioButtons(
                               "Display",
                               "Choose One",c("preview" = "p", "str" = "l","summary" = "s"),
                               inline = TRUE),
                              fileInput(label = "Upload Data","upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
                             selectInput(inputId = "inpd",label = "Save data of type:", choices = c("rds |rda |rdata","csv","rds","xslx"), selected = "csv"),
                             downloadButton('downloadData', 'Download File', style="display: block; margin: 0 auto; width: 230px;color: black;"),
                             checkboxInput('change_sign','Show R-Code'),
                             ),


                mainPanel("",
                          tabsetPanel(
                            tabPanel("Step 1: Load File", tableOutput("head")),
                            tabPanel('Step 2: Inspect Pop Data',fluidRow("",
                                              column(4,plotOutput('y0')),
                                              column(4,plotOutput('y1')),
                                              column(4,plotOutput('t1')),
                                              column(4,plotOutput('alpha')),
                                              column(4,plotOutput('r'))
                                              ),
                            tabPanel('Step 3: Estimate Seroincidence'),
                            tabPanel('Step 4: Export')))

                ))))
