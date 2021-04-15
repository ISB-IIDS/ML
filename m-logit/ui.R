####################################################
#                ISB mlogit                        #
####################################################

library("shiny")
#library("foreign")
#library(shinydashboard)
#library(leaflet)



shinyUI(pageWithSidebar(
  # Header:
  
  headerPanel(title=div(img(src="isb.png",align = "right"), h2("Multinomial Logit App", style="bold")), windowTitle	='Multinomial Logit'),
  
  # Input in sidepanel:
  sidebarPanel(
    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    h4(p("Data Selection")),
    htmlOutput("Individualvarselect"),
    htmlOutput("Alternativesvarselect"),
    htmlOutput("Choicevarselect"),
    
    
    htmlOutput("Alternativefeaturesvarselect"),
    htmlOutput("Individualfeaturesvarselect"),
    
   # submitButton(text = "Apply Changes", icon("refresh")),br(),
    
 #   fileInput("filep", "Upload new data for prediction (csv file with header)"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         br(),
                         p("In statistics, multinomial logistic regression is a classification method that generalizes 
                           logistic regression to multiclass problems, i.e. with two or more than two possible discrete outcomes.
                           That is, it is a model that is used to predict the probabilities of the different possible 
                           outcomes of a categorically distributed dependent variable, given a set of independent variables."),
                         tags$a(href="https://en.wikipedia.org/wiki/Multinomial_logistic_regression", "-Wikipedia"),
                         br(),
                         
                         h4(p("Sample Data Description")),
                         p("The data, collected as part of a 1987 intercity mode choice study, are a sub-sample of 210 non-business trips between Sydney, 
                           Canberra and Melbourne in which the traveler chooses a mode from four alternatives (plane, car, bus and train). 
                           The sample, 210 individuals choice observations, is choice based with over-sampling of the less popular modes (plane, train and bus) and under-sampling of the more popular mode, car. The level of service data was derived from highway and transport networks in Sydney, Melbourne, non-metropolitan N.S.W. and Victoria, including the Australian Capital Territory.",align="justify"),
                      #  h4(p("Download Sample Input Files")),
                         downloadButton('downloadData', 'download sample data'),
                         br(),br(),
                         p("Number of observations: 210 individuals' choice context for 4 modes of transporataion, 840 rows.",align="justify"),
                         h4('model specification variables'),
                         h5('individual (observation) ID column for each choice context, every observation must have a row for each alternative'),
                      h5('all available/possible alternatives column = mode (plane, air, bus, car)'),   
                      h5('choice/outcome column = 1 (0 otherwise)'),
                         
                         
                         h4('individual specific X variables'),
                         h5('income      = household income'),
                         h5('size       = traveling group size'),
                         h4('alternative-specific X variables (varaibles that differ across alternatives for an individual)'),
                         h5("wait       = customer's estimate of waiting time at terminal for plane, train and bus (minutes); 0 for car."),
                         h5("vcost      = customer's estimate of variable cost (dollars)."),
                         h5("gcost      = customer's estimate of generalized cost, a measure inclusive of travel time savings (dollars)."),
                         br(),
                      p('Note on Multinomial Logit - If you do not have alternative-specific variables (that is no data on alternatives that were not chosen), use discriminant analysis or bayes classifier (check classification app).',style="color:red"),
                      #h4(tags$a(href= 'https://isb-iids.shinyapps.io/classification/',"Click here to open classification app")),
                      #p('Discriminant analysis is like a regression, for categorical outcome variable. 
                      #  Data input required is also similar to regression input file 
                      #  (unlike multinomial-logit data input file.)'),br(),br(),
                #          p("Please note that download will not work with RStudio interface. 
                #            Download will work only in web-browsers. So open this app in a web-browser and then download the example file.")
                ), 
                          
                # 
                # tabPanel("Summary Stats", verbatimTextOutput("summary")),
                # tabPanel("Correlation", verbatimTextOutput("correlation"),plotOutput("heatmap")),
                 tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected X Variables"),verbatimTextOutput("summary"),h4("Missing Data Rows"),verbatimTextOutput("missing")),
                tabPanel("Model Output",br(),
                         #h4("Select Base Alternative"),
                         #br(),htmlOutput("BaseAlternativeselect"),
                         (p('Step1: Ensure you have selected the correct available/possible alternatives variable under "Data Selection" on the left panel.',style="color:black")),
                         verbatimTextOutput("basealtprint"),
                         (p('Step2: Make sure you have selected correct choice/outcome variable. Make sure every individual/observation has one row for each alternative, and only one of those alternatives is marked as "1" and rest are marked as "0" in the choice/outcome column.',style="color:black")),
                         
                         (p('Step3: Look at "Data Summary" and select alternative-specific X variables and individual-specific X variables appropriately.',style="color:black")),
                         h4("Model Summary"),
                         p("Note: coefficient (estimate) for the base alternative are set to zero.",style="color:red" ),
                         verbatimTextOutput("olssummary"),
                         p("Interpretation - one unit increase in variable increases the probability of outcome, over base alternative, by multiple of exponent(Coefficients:Estimate) units."),
                         br() ),
              #  tabPanel("Correlation",h4("Correlation Table"), verbatimTextOutput("correlation"),h4("Correlation"),plotOutput("corplot")),
                 tabPanel("Prediction Probablities", 
                          h4("Predicted probablities input data"),
                          dataTableOutput("probablities"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                          
                          h4(p("Download prediction probabilities for input data")),
                          downloadButton('downloadData1', 'Download predicted probabilities for input data'),
                          br(),br()
                          ),
                 tabPanel("Confusion Matrix", 
                          h4("Confusion Matrix Summary"),verbatimTextOutput("confusionmatrix"),
                         # (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),verbatimTextOutput("mscount"),
                          h4("ROC Curve Summary"),plotOutput("ROC"),br(),br()
                          )

                # tabPanel("Residuals Plot",h4("Fitted Values vs Residuals"),
                #          plotOutput("resplot2"),h4("Fitted Values vs Y"),
                #          plotOutput("resplot3"),h4("Residuals plot"),
                #          plotOutput("resplot1")),
                # tabPanel("Data with predicted Y",tableOutput("datatable"))
              
              # tabPanel("Prediction New Data",br(),
              #          h4("Upload data for prediction should be in the same format as input data (csv file with header) "),
              #         fileInput("filep", ""),
              #         h4("Predicted probablities new data"),
              #         dataTableOutput("probablitiespred"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
              #         h4(p("download predictions for new data")),
              #          downloadButton('downloadData2', 'download prediction probabilities for new data'))
              

    ) #type tabs
  ) #main panel
) #page with side bar
)#ui