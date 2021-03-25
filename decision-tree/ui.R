###########################################################
#   Classification and Regression Tree App (ui)           #
###########################################################
library("visNetwork")
shinyUI(
  fluidPage(
    
  #  titlePanel("Classification and Regression Tree"),
    headerPanel(title=div(img(src="isb.png",align = "right"), h2("Decision Tree App", style="bold")),windowTitle	='Decision Tree'),
    
    sidebarLayout(
      
      sidebarPanel(
        # Upload data:
        h4(p(" Data Input")),
        fileInput("file", "Upload data (csv file)"),
        sliderInput('sample','Set test sample percentage',10,40,15),
        # h4(p("Select Response Variable")),
        sliderInput('cp','Set complexity parameter',0,0.1,0),
        h4(p(" Data Selection")),
        htmlOutput("yvarselect"),
        htmlOutput("xvarselect"),
      #  submitButton(text = "Apply Changes", icon("refresh")),br(),
        htmlOutput("fyvarselect"),
        htmlOutput("fxvarselect"),
        htmlOutput("samsel"),
     #   fileInput("filep", "Upload new data for prediction (csv file)")
      ),   # end of sidebar panel
      
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overview",
                             h4(p("How to use this shiny application")),
                             p("This shiny application requires a data input from the user. To do so, click on the 'Browse' (in the panel on the left) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                             p("Once csv file is uploaded successfully, variables in the data file will reflect in the 'Data Selection' panel on the left. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model.
                            ",align="justify"),
                             p('You can adjust the test sample proportion from the slider in the panel on the left. Test sample will be randomly selected from the input data set. 
                               If you have a similar data set on which you want to make the prediction based on decision tree, You can upload that data set in the "Prediction New Data" tab. 
                               Please note that prediction data should have all explanatory variables similar to model data.',align="justify"),
                             p('You can also adjust the complexity parameter in decision tree model to control size of the tree. A decision tree is a decision support tool that uses a tree-like model of decisions and their possible consequences, including chance event outcomes, resource costs, and utility. It is one way to display an algorithm that only contains conditional control statements.'),
  
                             tags$a(href="https://en.wikipedia.org/wiki/Decision_tree", "-Wikipedia"),
                             br(),br(),
                             #h4(p("Download Sample Input File")),
                             downloadButton('downloadData', 'download sample data'),
                             br(), br(),
                             br(),
                             
                          #   p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers.")
                            ), # close Overview tab
                            
                    #tabPanel("Data Summary",verbatimTextOutput('summarydata')),
                    tabPanel("Data Summary",
                             h4("Uploaded Data"), 
                             dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                             #verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                            h4("Data Summary of Selected X Variables"),verbatimTextOutput("summarydata"),
                            h4("Missing Data Rows"),verbatimTextOutput("missing"),
                            br()),
                    tabPanel("Model Output",br(),
                             (p('Is your outcome (Y) variable factor (catregorical)? 
                                  If yes, please, make sure that you have check-marked it as a factor variable 
                                in the left panel.',style="color:red")),
                             h4('Variable importance'),
                             verbatimTextOutput('imp'),
                             h4('Number of Rows and Columns in Training Data'),
                             verbatimTextOutput('trainobs'),
                             h4('Model Accuracy/Error of Training Data'),
                             verbatimTextOutput("validation"),
                             h4('Number of Rows and Columns in Test Data'),
                             verbatimTextOutput('testobs'),
                             h4('Model Accuracy/Error of Test Data'),
                             verbatimTextOutput("validation1"),
                             #h4("Download Input Data with Predictions"),
                             #downloadButton('downloadData0', 'Download predictions'),
                             h4('Model Result Summary'),
                             verbatimTextOutput("results"),
                            # h4("First 10 predictions of train data"),
                            # p('"Yhat" column is the predicted value.'),
                            # verbatimTextOutput('predictionorg'),
                             br()
                             ),
                    tabPanel('Summary of Splits',#h5('Be patient model may take some time to finish estimation'),
                             verbatimTextOutput("mod_sum"),
                             #verbatimTextOutput("summary"),
                             br()),
                    
                    tabPanel("Decision Tree",#h5('Be patient model may take some time to finish estimation'),
                            #br(),
                            h4("Missing Data Rows Count"),verbatimTextOutput("mscount"),
                            #(p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")), 
                            visNetworkOutput("plot33",height = 600, width = 850), br(),br(),
                            (p('Is your outcome (Y) variable factor (catregorical)? 
                                  If yes, please, make sure that you have check-marked it as a factor variable 
                                in the left panel.',style="color:red")),
                            h5("To optimally prune the tree, set the complexity parameter (in the left panel) = 0 and
                            find the row with lowest 'xerror' value in 'Model Results Summary' under 'Model output' tab."),
                            h5("Look at the corresponding value of CP
                              in the lowest 'xerror' value row and set the complexity parameter (in the left panel) close to that value."),
                            plotOutput("plot3",height = 1600),
                            br(),
                       #      h4('Visualize cross-validation results'),
                        #     plotOutput("plot1",height = 800)
                             br()),                   
               #     tabPanel("Node labels",plotOutput("plot2",height = 600, width = 850),
                #             #h4("First column is assigned node number for each obsrvn in model training data"),
                 #            #verbatimTextOutput("nodesout1"),
                  #           dataTableOutput("nodesout"),
                   #          br(),
                    #         h4("Download nodes data from model training data"),
                     #        br(),
                      #       downloadButton('downloadData3','Download nodes data (Works only in browser)')
                       #      ),
                    #tabPanel("Variable",verbatimTextOutput('imp')),
                tabPanel("Input Data with Predictions",

                         h4('Model Accuracy/Error of Input Data'),
                         verbatimTextOutput("validation2"),

                         (p('Is your outcome (Y) variable factor (catregorical)? 
                                  If yes, please, make sure that you have check-marked it as a factor variable 
                                in the left panel.',style="color:red")),

                         h4("Download input data with predictions"),
                        downloadButton('downloadData0', 'download predictions for input data'),
                        #h4("First 10 rows of predictions for new data (upload prediction data)"),
                        br(),br(),#h4('"Yhat" column is the predicted value.'),
                        #verbatimTextOutput('prediction'),
                        dataTableOutput("predictionorg"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                ),
               
                    tabPanel("Prediction New Data",
                             h4("Upload new data for prediction, it should be in the same format as 
                                input data file (csv file with header)"),
                             fileInput("filep",""),
                             h4("Download new data with predictions"),
                             downloadButton('downloadData1', 'download predictions for new data'),
                             #h4("First 10 rows of predictions for new data (upload prediction data)"),
                             br(),br(),#h4('"Yhat" column is the predicted value.'),
                             #verbatimTextOutput('prediction'),
                             dataTableOutput("prediction"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                             
                             # #h4('Number of Rows and Columns in New Prediction Data'),
                             # #verbatimTextOutput('predictobs'),
                             # h4("First 10 rows of new data with predictions"),
                             # p('"Yhat" column is the predicted value.'),
                             # verbatimTextOutput('prediction'),
                             # h4("Download new data with predictions"),
                             # downloadButton('downloadData1', 'download predictions for new data')
                             )
                             
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



