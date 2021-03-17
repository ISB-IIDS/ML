####################################################
#      Summary & OLS App IIDS                          #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("OLS App"),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Regression App", style="bold")), windowTitle	='Regression'),
  #titlePanel(title=div(img(src="logo.png",align='right'),"OLS App")),
  # Input in sidepanel:
  sidebarPanel(

    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
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
                         p("This shiny application requires one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           ",align="justify"), 
                         tags$a(href="https://en.wikipedia.org/wiki/Regression_analysis", "-Wikipedia"),
                         br(),br(),
                         # h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'download sample data'),
                         br()
                          ),
                         
                tabPanel("Data Summary",h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Data Summary of Selected Y and X Varaibles"),verbatimTextOutput("summary"),h4("Missing Data"),
                          verbatimTextOutput("missing"),br()),
               # tabPanel("Correlation",h4("Correlation Table - Input data"), 

                tabPanel("Summary OLS",  
                         h4("Summary OLS Model"),verbatimTextOutput("olssummary"),
                         h4("Correlation Table"),verbatimTextOutput("correlation"),
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         h4("Correlation Visulization - Input Data"),plotOutput("corplot"),br()
                         ),
                tabPanel("Standardized OLS", h4("Summary Standardized Input Data (mean=0 variance=1)"),verbatimTextOutput("summarystd"),
                         h4("OLS Standardized Input Data"), verbatimTextOutput("olssummarystd")),
                tabPanel("Input Data with Predictions", 
                         h4("Download input data with predicted Y values"),
                         downloadButton('downloadData2', 'download predictions for input data'),
                         #h4("First 10 rows of predictions for input data"),
                         br(),br(),h4('"Yhat" column is the predicted value.'),
                         #verbatimTextOutput('inputprediction'),
                         dataTableOutput("inputprediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                         br(),br()
                         #br(),br(),tableOutput("datatable")
                         ),
                # tabPanel("Corr-Hist",h4("Discriptive Analytics - Random 100 Input Data Rows"),plotOutput("heatmap1")),
                tabPanel("Residuals Plot",
                         h4('Missing Data Rows Count'),verbatimTextOutput("mscount"),
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         h4('Mean Square Error'),verbatimTextOutput("validation"),
                         h4("Fitted Values vs. Y (Input Data)"),plotOutput("resplot3",height = 800),
                         h4("Fitted Values vs. Residuals (Input Data)"), plotOutput("resplot2",height = 800),
                         h4("Residual Errors Plot (Input Data)"), plotOutput("resplot1",height = 800), br()),
                tabPanel("Prediction New Data",
                         h4("Upload new data for prediction, it should be in the same format as input data file (csv file with header) "),
                         fileInput("filep", ""),
                         h4("Download new data with predictions"),
                         downloadButton('downloadData1', 'download predictions for new data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),h4('"Yhat" column is the predicted Y values.'),
                         #verbatimTextOutput('prediction'),
                         dataTableOutput("prediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         ) 
                )
      ) 
    ) 
  )
