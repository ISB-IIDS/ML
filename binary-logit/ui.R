####################################################
#      Summary & Binary App                           #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("Logistic Regression (Binary Logit) App"),
  headerPanel( title=div(img(src="isb.png",align = "right"), h2("Logistic Regression (Binary Logit) App", style="bold")), windowTitle	='Binary Logit'),
  # Input in sidepanel:
  sidebarPanel(

    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    h4(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("BaseAlternativeselect"),
    htmlOutput("xvarselect"),
  #  submitButton(text = "Apply Changes", icon("refresh")),br(),
    htmlOutput("fxvarselect"),
    htmlOutput("samsel"),
    htmlOutput("imputemiss"),
  #  fileInput("filep", "Upload new data for prediction (csv file with header)"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         br(),
                         p("In statistics, the logistic regression (or binary logit model) is used to model the probability of a certain class 
                           or event existing such as pass/fail, win/lose, alive/dead or healthy/sick."),
                         tags$a(href="https://en.wikipedia.org/wiki/Logistic_regression", "-Wikipedia"),
                         br(),
                         h4(p("How to use this shiny application")),
                         p("This shiny application requires one data input from the user. To do so, click on 'Browse' (in the panel on the left)
                            and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",
                           align="justify"),
                         p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model analysis. 
                            If any of the explanatory variables is a factor variable, you can define that variable as factor variable by selecting that variable in the last list of variables.",
                           align="justify"),
                         p("Binary logit classification model trains better when observations are equal distributed between two classes (0 and 1 outcomes).",
                           align="justify"),
                         #h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'download sample data'),
                         br(), br(),
                         p('Note - If your data has more than two classes (categorical outcomes) use discriminant analysis or bayes classifier (check classification app).',style="color:black"),
                         #h4(tags$a(href= 'https://isb-iids.shinyapps.io/classification/',"Click here to open classification app")),
                         br(),
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected X Variables"),htmlOutput("imout"),verbatimTextOutput("summary"),
                         h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Data Visualization",br(),
                         #h4("Select variable for er's outlier test"),
                         h4("Be patient generating plots"),
                         plotOutput("dens"),
                         h4("Histograms"),
                         plotOutput("hist"),br(),
                         h4("Bi-Variate Plots"),
                         #(p('remove missing data variable(s) if any, or impute or drop rows - check  "Data Summary" tab and options in the panel on the left',style="color:black")),
                         plotOutput("corplot"),
                         br(),
                         br()),
                  tabPanel("Summary Logit", # br(), h4(p('Y must be binary variable ',style="color:red")),
                        # h4("Confusion Matrix"),verbatimTextOutput("validation"),
                         h4("Summary Logistic Regression Model"),
                         htmlOutput("yout"),
                         p("Interpretation - one unit increase in the variable X, increases the probability of outcome (Y equal to 1) over probability of not-outcome (Y not equal to 1) by multiple of, exponent of 'coefficient's estimate' (exp[coeff.estimate])."),
                         verbatimTextOutput("olssummary"),
                         br(),
                         h4("Correlation Table"),verbatimTextOutput("correlation"),
                         
                         #h4('Confusion Matrix'), verbatimTextOutput("validation")),
                         #h4("Summary OLS standardized model"), verbatimTextOutput("olssummarystd")),
                         br()),
                
                tabPanel("ROC", br(),
                         sliderInput('cutoff','Cutoff Probability',0,1,0.5),
                         p("Suggested Optimal Cutoff Probability - Choose Cutoff to Maximize, Sensitivity + Specificity"),
                         h4("Missing Data Rows Count"),verbatimTextOutput("mscount"),
                         (p('remove missing data variable(s) if any, or impute or drop rows - check  "Data Summary" tab',style="color:black")),
                         htmlOutput("yout1"),
                         #verbatimTextOutput("mscount"),
                         #(p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         #(p('Y must be binary variable',style="color:red")),
                         #h4("Fitted Values vs Y - Input Data"),
                         h4("Confusion Matrix Summary"),
                         verbatimTextOutput("confusionmatrix"),
                         (p('remove missing data variable(s) if any, or impute or drop rows - check  "Data Summary" tab',style="color:black")),
                         plotOutput("resplot3"),
                         h4("ROC Curve"),plotOutput("roc"),
                         br()),
                #tabPanel("Residuals Plot",
                #   h4("Fitted Values vs Residuals - Input Data"), plotOutput("resplot2"),
                #   h4("Fitted Values vs Y - Input Data"), plotOutput("resplot3")),
                
                tabPanel("Prediction Input Data",
                         
                         h4("Download input data with predictions"),
                         downloadButton('downloadData2', 'download predictions for input data'),
                         #h4("First 10 rows of predictions for input data"),
                         br(),br(),
                         #h4('Y.log.odds" column is predicted log-odds. log[prob(Y=1)/1-Prob(Y=1)].'),
                         htmlOutput("yout2"),
                         #verbatimTextOutput('inputprediction'),
                         dataTableOutput("inputprediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                         br(),br()  #,tableOutput("datatable")
                         
                         # br(), h4('First 10 rows of predictions for input data'),
                         # p('"Y.Prob" column is the predicted probability of Y=1.'),
                         # verbatimTextOutput('inputprediction'),
                         # h4("Download input data with predictions"),
                         # downloadButton('downloadData2', 'download predictions for input data'),
                         # br(),br(),#tableOutput("datatable")
                         
                         ),
               # tabPanel("Correlation",h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
               #          h4("Correlation Visulization - Input Data"),plotOutput("corplot")),

               
               tabPanel("Prediction New Data",
                         h4("Upload new data for prediction, it should have all selected X varaibles (csv file with header) "),
                         fileInput("filep", ""),
                         h4("New data"),
                         dataTableOutput("readdatap"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br(),
                        htmlOutput("yout3"),
                        verbatimTextOutput("confusionmatrix1"), 
                        h4("Download new data with predictions"),
                         downloadButton('downloadData1', 'download predictions for new data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),
                         #h4('Y.log.odds column is predicted log-odds, log[prob(Y=1)/1-prob(Y=1)]'),
                         
                         #verbatimTextOutput('prediction'),
                         dataTableOutput("prediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         
                         # br(),
                         # h4("Upload new data for prediction, it should be in the same format as input data file (csv file with header) "),
                         # fileInput("filep",""),
                         # h4("First 10 rows of predictions for new data (upload prediction data)"),
                         # p('"Y.Prob" column is the predicted probability of Y=1.'),
                         # verbatimTextOutput('prediction'),
                         # h4("Download new data with predictions"),
                         # downloadButton('downloadData1', 'download predictions for new data')      
                         )
               
               # tabPanel("Class Visualization",br(),
               #          numericInput("perp", "Set Perplexity Parameter",25 ,min=5,max=95, step=10),
               #          numericInput("iter", "Set Max Iterations",500, min=500, max=5000, step=500),
               #          h4(p('Note: projecting numerical input data in two dimensions; it takes a while, be patient.',style="color:red")),
               #          tags$a(href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding", "-Wikipedia"),br(),
               #          br(),
               #          plotOutput("resplot4",height = 800) )
                
               )
      ) 
    ) 
  )
