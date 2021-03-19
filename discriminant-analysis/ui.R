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
                         p("This shiny application requires one data input from the user. To do so, click on 'Browse' (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names."),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            segement membership variable (Y Variable) from drop-down menu. Y must be a factor/categorical varaible. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model. 
                            If any of the variables selected in explanatory variables is a factor variable, you can define that variable as factor variable just
                            by selecting that variable in the last list of variables
                           "),
                         p("Dicriminant analysis is like a regression, for categorical outcomes. Data input required is also like regression input data file."),
                         tags$a(href="https://en.wikipedia.org/wiki/Linear_discriminant_analysis", "-Wikipedia"),
                         br(),br(),
                         # h4(p("Download Sample Input Files")),
                         downloadButton('downloadData', 'download sample data'),
                         br(),br(),
                         
                        # h4(p("Download Sample Input Files")),
                         # br(),
                        # downloadButton('downloadData', 'Download Sample Data'),
                        # br(),
                        # br(),
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Data Summary", h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Data Summary of Selecetd X Variables"),verbatimTextOutput("summary"),
                         h4("Missing Data"),verbatimTextOutput("missing"),br()),
               # tabPanel("Correlation",
                        
                tabPanel("Discriminant Analysis",br(), (p('Y must be factor (categorical) variable',style="color:red")),
                         h4("Summary Discrimant Model"),verbatimTextOutput("olssummary"),
                         h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
                         
                         h4("Correlation Visulization - Input Data"),
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         plotOutput("corplot"),br()
                         ),
                         
                tabPanel("Input Data with Predictions", 
                         h4("Confusion Matrix"),verbatimTextOutput("confusion"),br(),
                         downloadButton('downloadData2', 'Download predictions for new data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),#h4('"Yhat" column is the predicted value.'),
                         #verbatimTextOutput('prediction'),
                         dataTableOutput("datatable"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         
                         # h4("First 10 rows of predictions for input data"),
                         # tableOutput("datatable"),
                         # h4("Download input data with prediction"),
                         # downloadButton('downloadData2', 'Download Data (Works only in browser)')
                         ),
                
              # tabPanel("MeanStd",h4("Mean and Std"),verbatimTextOutput("meanstd1")),
                tabPanel("Discriminant Plot",br(),
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),verbatimTextOutput("mscount"),
                         h4("Class Scatter Plot - Primary Discriminant Dimensions"),
                         plotOutput("resplot1",height = 800),
                         #h4("Class Scatter Plot - First & Third Discriminant Dimensions"),
                         plotOutput("resplot2",height = 800),
                         #h4("Class Scatter Plot - Second & Third Discriminant Dimensions"),
                         plotOutput("resplot3",height = 800),br()
                        # h4("Fitted Values vs Residuals - Input Data"), plotOutput("resplot2",height = 800),
                        # h4("Residuals plot - Input Data"), plotOutput("resplot1",height = 800)
                        ),
                tabPanel("Prediction New Data",
                         h4("Upload new data for prediction, it should be in the same format as input data file (csv file with header)"),
                         fileInput("filep",""),
                         downloadButton('downloadData1', 'Download predictions for new data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),#h4('"Yhat" column is the predicted value.'),
                         #verbatimTextOutput('prediction'),
                         dataTableOutput("datatablep"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         
                         # h4("First 10 rows of predictions for new data (upload prediction data)"),
                         # ,br(),tableOutput("datatablep"),
                         # h4("Download new data with predictions"),
                         # downloadButton('downloadData1', 'download predictions for new data')      
                         )
                         
                )
      ) 
    ) 
  )
