Sys.setenv(RETICULATE_PYTHON = "epmv/Scripts/python.exe")
library(reticulate)
#reticulate::virtualenv_create("epmv")
#reticulate::virtualenv_install("epmv", packages = c("EpigeneticPacemaker", "matplotlib","numpy"))
#reticulate::use_virtualenv("epmv", required = TRUE)
#reticulate::use_virtualenv('C:/Users/XZzzZzz/Desktop/EPM/epmvenv', required = TRUE)
#library(car)
#conda_create("r-scrublet")
#conda_install(envname="r-scrublet", packages ="numpy","pip","git")
#conda_python(envname =  "r-scrublet")
#reticulate::conda_create("my-environment")

#reticulate::use_condaenv("epmvenv", required = TRUE)

#py_install('matplotlib',pip=TRUE)
#py_module_available('numpy')
source_python("data_get.py")
source_python("epi_function.py")

#source_python("test_python.py")


### SHINY UI ###
ui <- fluidPage(
  # Application title
  navbarPage(title="Epigenetic Pacemaker Model",id="mainpage",
             tabPanel("Home",value="tabhome",
                      h1("Welcome to Epigenetic Pacemaker",
                         style="color:grey;front-size:35px;text-align:center;"),
                      br(),
                      fluidRow(
                        column(10,offset=1,
                               fluidRow(
                                 img(src="formula.png",width="20%"),align="center")
                        )
                        #style = "margin-bottom:20%; margin-top:20%"
                      ),
                      br(),
                      p("DNA methylation is widely used to model physiological phenotypes, 
                          such as aging and type II diabetes. The Epigenetic Pacemaker, EPM, 
                          is an implementation of a fast conditional expectation maximization algorithm that models epigenetic 
                          states under and evolutionary framework. The EPM was first introduced by Snir et al. 
                          as an extension of the Universal Pacemaker (UPM) model of genome evolution. 
                          In contrast to regression bases approaches, the EPM does not assume a linear relationship 
                          between the epigenetic state and a trait of interest. As a result the EPM can model non-linear 
                          epigenetic trait associations directly without transformation of the phenotype of interest.",
                        style = "text-align:justify;font-family:'times'; font-si16pt; width:80%; margin-left:10%; margin-right:10%", 
                        align="center"),
                      p("Reference: https://epigeneticpacemaker.readthedocs.io/en/latest/",
                        style = "font-family:'times'; font-si16pt; width:80%; margin-left:10%; margin-right:10%")
             ),
             navbarMenu("Datasets",
                        tabPanel("Example Dataset",value="example_dataset",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file",label=h3("Choose CSV file"),
                                               multiple = TRUE,
                                               accept = c("text/csv", ".csv")),
                                     tags$hr(),
                                     fluidRow(column(4, verbatimTextOutput("value")))
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Simple",plotOutput("prediction")),
                                       tabPanel("Scatter",plotOutput("scat"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Individual Dataset",value="individual_dataset",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file",label=h3("File Input")),
                                     hr()
                                   ),
                                   mainPanel(
                                     
                                   )
                                 )
                        ),
			tabPanel("Test Dataset",value="test_dataset",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("test_file",label=h3("File Input")),
                                     hr()
                                   ),
                                   mainPanel(
                                      tableOutput("myresult")  
                                   )
                                 )
                        )
                            
                                 
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$prediction <- renderPlot({
    plot(test_ages, test_predict, main="EpigeneticPacemaker", 
         xlab="Chronological Age",ylab="Epigenetic Age",pch=19)
    abline(lm(log(test_predict)~log(test_ages)),col="red")
  })
  output$scat <- renderPlot({
    scatterplot(test_ages, test_predict, 
                xlab="Weight of Car", ylab="Miles Per Gallon", 
                main="Enhanced Scatter Plot")
  })

  output$test_contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$test_file)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
        df <- read.csv(input$test_file$datapath,                 
                 sep = "\t" )
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    myresult <- load(df)
    return(myresult)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
