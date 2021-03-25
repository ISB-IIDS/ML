####################################################
#      Summary & BAsic Stats                        #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
 # headerPanel("Regression App"),
  headerPanel(title=div(img(src="isb.png",align = "right"), h2("Basic Data Stats App", style="bold")), windowTitle	='Basic Stats'),
  
  # Input in sidepanel:
  sidebarPanel(

    h4(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    h4(p("Data Selection")),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect"),
    htmlOutput("imputemiss"),
    #submitButton(text = "Apply Changes", icon("refresh")),br(),
    ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4(p("How to use this shiny application")),
                         p("This shiny application require one data input from the user. To do so, click on the 'Browse' (in the panel on the left) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                         p("Once csv file is uploaded successfully, all variables in the data file will reflect in the 'Data Selection' panel on the left. 
                            If you want to drop any variable from the analysis, just uncheck that variable and it will be dropped from the analysis."),
                          p("Next you can select factor (categorical) variables just by selecting that variable in the 'Select factor (categorical) variables' list.",align="justify"),
                        # h4(p("Download sample input file")),
                         downloadButton('downloadData', 'download dample data'),
                         br(),
                         br()
                        # p("*Please note that download will not work with RStudio interface. Download will work only in web-browsers."),
                         ),
                tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected Variables"),verbatimTextOutput("summary"),
                         h4("Missing Data"),verbatimTextOutput("missing2"),
                         br()),
                tabPanel("Missing Data",#h4("Impute Missing Values"), 
                         h4("Missing Data for Selected Variables"),verbatimTextOutput("mscount"),
                         dataTableOutput("missing1"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         br()),
                tabPanel("Correlation",
                          h4("Correlation Table"), verbatimTextOutput("correlation"),
                         h4("Correlation Visulization"),
                         (p('Remove missing data variable(s) if any - check  "Missing Data" tab',style="color:red")),
                         p(plotOutput("corplot"),align="left"),
                         h4("Visulizing Correlation Matrix"),plotOutput("corplot1",width = 500)),
                          
                tabPanel("Box Plot", h4("Box Plots"),plotOutput("bplot"),br(),br(),
                         h4("Box Plots of Scaled Data (mean=0, vairance=1)"),plotOutput("sbplot")),
                tabPanel("Bi-Variate Statistics",br(),
                         htmlOutput("samsel"),
                         #h4("Randomly select 500 rows from the input data - if less than 500 rows in a dataset, select all rows"),
                         plotOutput("heatmap1")),
                tabPanel("Outliers",
                         h4("Select variable for Rosner's outlier test"),
                         htmlOutput("outselect"),plotOutput("hist"),
                         verbatimTextOutput("outlier")),
                tabPanel("Download Data with Dummy Variables",br(),
                         htmlOutput("imout"),
                         h4(p("Download input data with dummy variable columns added for factor (categorical) variables")),
                         downloadButton('downloadDatanew', 'download input data with dummy variables'), br(), br(),
                         #h5("First 10 rows of input data with dummy variable coding for factor/categorical variables"),
                         dataTableOutput("dummydata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),br()
                         )
                )
      ) 
    ) 
  )
