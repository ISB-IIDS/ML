####################################################
#      Discriminant Analysis  App                  #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("OLS App"),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Discriminant Analysis App", style="bold")), windowTitle	='Discriminant Analysis'),
  #titlePanel(title=div(img(src="logo.png",align='right'),"OLS App")),
  # Input in sidepanel:
  sidebarPanel(

    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    selectInput("select", "Choose algorithm", 
                c("Linear","Quadratic","Regularized"), selected = "Linear"),
    h4(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
 #   submitButton(text = "Apply Changes", icon("refresh")),br(),
    htmlOutput("fxvarselect"),
 #   fileInput("filep", "Upload new data for prediction (csv file with header)"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            segement membership variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           ",align="justify"),
                         br(),
                        # h4(p("Download Sample Input Files")),
                         # br(),
                        # downloadButton('downloadData', 'Download Sample Data (works only in browsers)'),
                        # br(),
                        # br(),
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Summary Stats",# h4("Data"), verbatimTextOutput("head"),verbatimTextOutput("tail"),
                         h4("Data Summary"),verbatimTextOutput("summary"),
                         h4("Missing Data"),verbatimTextOutput("missing")),
                tabPanel("Correlation",h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
                         h4("Correlation Visulization - Input Data"),plotOutput("corplot")),
                tabPanel("Discriminant Analysis",h4("Summary Discrimant Model"),verbatimTextOutput("olssummary")),
                         
                tabPanel("Input Data with Predictions", 
                         h4("First 10 rows of predictions for input data"),
                         br(),tableOutput("datatable"),
                         h4("Download input data with prediction"),
                         downloadButton('downloadData2', 'Download Data (Works only in browser)')),
              # tabPanel("MeanStd",h4("Mean and Std"),verbatimTextOutput("meanstd1")),
                tabPanel("Residuals Plot",
                         h4("Class Scatter Plot - First 2 Discriminant Dimensions"),verbatimTextOutput("mscount"),plotOutput("resplot3",height = 800),
                        # h4("Fitted Values vs Residuals - Input Data"), plotOutput("resplot2",height = 800),
                        # h4("Residuals plot - Input Data"), plotOutput("resplot1",height = 800)
                        ),
                tabPanel("Prediction New Data", br(),
                         h4("Upload new data for prediction should be in the same format as input data (csv file with header) "),
                         fileInput("filep",""),
                         h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),tableOutput("datatablep"),
                         h4("Download new data with predictions"),
                         downloadButton('downloadData1', 'download predictions for new data')      )
                         
                )
      ) 
    ) 
  )
