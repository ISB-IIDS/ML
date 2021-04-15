####################################################
#      Classification Analysis  App                  #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  #headerPanel("OLS App"),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Classification App", style="bold")), windowTitle	='Classification'),
  #titlePanel(title=div(img(src="logo.png",align='right'),"OLS App")),
  # Input in sidepanel:
  sidebarPanel(

    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    selectInput("select", "Choose algorithm", 
                c("Linear Discriminant Classification","Naive Bayes Classification","Quadratic Discriminant Classification","Support Vector Machine"), selected = "Naive Bayes Classification"),
    h4(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
    htmlOutput("fyvarselect"),
 #   submitButton(text = "Apply Changes", icon("refresh")),br(),
    htmlOutput("fxvarselect"),
    htmlOutput("samsel"),
    htmlOutput("imputemiss"),
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
                         #p("Dicriminant analysis is like a regression, for categorical outcomes. Data input required is also like regression input data file."),
                         tags$a(href="https://en.wikipedia.org/wiki/Linear_discriminant_analysis", "Discriminant Analysis - Wikipedia"),
                         br(),
                         tags$a(href="https://en.wikipedia.org/wiki/Naive_Bayes_classifier", "Naive Bayes Classifier - Wikipedia"),
                         br(),                         
                         tags$a(href="https://en.wikipedia.org/wiki/Support-vector_machine", "Support Vector Machine - Wikipedia"),
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
                tabPanel("Data Summary", #h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),                         
                         h4("Data Summary of Selecetd X Variables"),htmlOutput("imout"),verbatimTextOutput("summary"),
                         h4("Missing Data"),
                         verbatimTextOutput("missing"),
                         #dataTableOutput("missing1"),
                         br()),
               # tabPanel("Correlation",
               tabPanel("Data Visualization",br(),
                        #h4("Select variable for er's outlier test"),
                        h4("Be patient generating plots"),
                        plotOutput("dens"),
                        h4("Histograms"),
                        plotOutput("hist"),br(),
                        h4("Bi-variate Plots"),
                       # (p('remove missing data variable(s) if any, or impute or drop rows. Check "Data Summary" tab and options in the panel on the left.',style="color:black")),
                        plotOutput("corplot"),
                        br(),
                        br()),
                        
                tabPanel("Model Output",br(), 
                         htmlOutput("warning"),
                         (p('Y must be factor (categorical) variable. Check mark it under "Select factor (categorical) variables" in the panel on the left.',style="color:red")),
                         h4("Summary Model"),htmlOutput("yout") , verbatimTextOutput("olssummary"),
                         h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
                         h4("2D Visulization (PCA)"),
                         plotOutput("pcaplot"),
                         br(),br()),
                         
                tabPanel("Prediction Input Data", br(), 
                         (p('Y must be factor (categorical) variable. Check mark it under "Select factor (categorical) variables" in the panel on the left.',style="color:red")),
                         h4("Confusion Matrix"),
                         verbatimTextOutput("confusion"),
                         h4("Dowloand Input Data with Predictions"),#verbatimTextOutput("confusion"),br(),
                         downloadButton('downloadData1', 'Download predictions for input data'),
                         #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),#h4('"Yhat" column is the predicted value.'),
                         #verbatimTextOutput('prediction'),
                         htmlOutput("yout1"), 
                         dataTableOutput("datatable"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         
                         # h4("First 10 rows of predictions for input data"),
                         # tableOutput("datatable"),
                         # h4("Download input data with prediction"),
                         # downloadButton('downloadData2', 'Download Data (Works only in browser)')
                         ),
                
                # tabPanel("Discriminant Plot",
                #          h4(p('Note: Y must have more than two classes to calculate two discriminant axes for 
                #               plotting data (it may take a while for the plot to appear, be patient)',style="color:black")),
                #          #h4("Plotting data on primary linear discriminant axes (calculations may take a while be patient)"),
                #          (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                #          plotOutput("resplot1",height = 800),
                #          #h4("Class Scatter Plot - First & Third Discriminant Dimensions"),
                #          plotOutput("resplot2",height = 800),
                #          #h4("Class Scatter Plot - Second & Third Discriminant Dimensions"),
                #          plotOutput("resplot3",height = 800),br()
                #         # h4("Fitted Values vs Residuals - Input Data"), plotOutput("resplot2",height = 800),
                #         # h4("Residuals plot - Input Data"), plotOutput("resplot1",height = 800)
                #         ),
               
                tabPanel("Prediction New Data",
                         h4("Upload new data for prediction, it should have all selected X variables (csv file with header)"),
                         fileInput("filep",""),
                         h4("New data"),
                         dataTableOutput("readdatap"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br(),
                         verbatimTextOutput("confusion1"),
                         h4("Download new data with predictions"),
                         downloadButton('downloadDatap', 'Download predictions for new data'),
                                                  #h4("First 10 rows of predictions for new data (upload prediction data)"),
                         br(),br(),#h4('"Yhat" column is the predicted value.'),
                         #verbatimTextOutput('prediction'),
                         htmlOutput("yout3"),
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
