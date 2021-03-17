####################################################
#      Cluster Analysis                     #
####################################################

library("shiny")
#library("foreign")

shinyUI(fluidPage(
  # Header:
  #headerPanel("Segmentation Analysis"),
  titlePanel(title=div(img(src="logo.png",align='right'),h2("Cluster Analysis",style="bold")), windowTitle	='Cluster Analysis'),
  
  # Input in sidepanel:
  sidebarPanel(
    h4(p("Data Input")),
    fileInput("file", "Upload data, first column must be observation id (csv file with header)"),
    selectInput("select", "Choose cluster algorithm", 
                       c("K-Means","Hierarchical"), selected = "K-Means"),
    numericInput("Clust", "Choose number of clusters", 2),
    h4(p("Data Selection")),
    htmlOutput("xvarselect"),
    br(),
    #submitButton(text = "Apply Changes", icon("refresh"))
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                tabPanel("Overview",
                         h4(p("Cluster Analysis")), 
                         p("Cluster analysis or clustering is the task of grouping a set of objects in such a way that 
                           objects in the same group (called a cluster) are more similar (in some sense) to each other 
                           than to those in other groups (clusters). It is a main task of exploratory data mining, and 
                           a common technique for statistical data analysis, used in many fields, including pattern 
                           recognition, image analysis, information retrieval, bioinformatics, data compression, 
                           computer graphics and machine learning.",
                           align="justify"),
                         tags$a(href="https://en.wikipedia.org/wiki/Cluster_analysis", "-Wikipedia"),
                         h4(p("Data input")),
                         p("First column of the input data must be an obervation id.",style="color:red"),
                         p("This application requires input data from the user in csv format. To upload data, click on the 'Browse' (in the panel on the the left) 
                         and upload the Segmentation data input file.
                            Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                            and then proceed. Make sure you have top row as variable names and first column as id in csv file"
                           ,align="justify"),
                         
                         p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 2 segments. 
                           In the panel on the left you can change the segmentation algorithm and number of segments. Click on 'refresh' after making any changes in the input. 
                           Accordingly results will be updated in all the tabs",
                           align="justify"),
                        # h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'download sample input file'),
                         br(),br(),
                         #p("Cluster analysis or clustering is the task of grouping a set of objects in such a way that objects in the same group (called a cluster) are more similar (in some sense) to each other than to those in other groups (clusters). It is a main task of exploratory data mining, and a common technique for statistical data analysis, used in many fields, including pattern recognition, image analysis, information retrieval, bioinformatics, data compression, computer graphics and machine learning."),
                         
                          ),
                tabPanel("Data Summary",
                         #h4("select only numerical variables with no missing values in X and click 'Refresh' "), 
                         #verbatimTextOutput("tail"),
                         br(), (p('First column of the input data must be an observation id',style="color:red")),
                         h4("Selected Variables"), verbatimTextOutput("head"),
                         (p("Click refresh after loading data and every time you make changes",style="color:red")),
                         submitButton(text = "refresh", icon("refresh")),
                         
                         h4("Data Summary of Selected X"),verbatimTextOutput("summ"),
                         h4("Missing data rows"),verbatimTextOutput("missing")),
                
                #tabPanel("Data",h3(textOutput("caption"),tableOutput("table"))),
                
                tabPanel("Cluster Analysis",br(),
                         p("Input data is standardized before running cluster analysis.", style="color:red"),
                         h4("Summary Standardize Data"),
                         verbatimTextOutput("scldt"),
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         
                         p("Select X data with only numerical varaibles"),
                         p("Choose cluster algorithm (default clusters = 2)"),
                         p("Click 'refresh' and wait for algorithm to finish estimation"),
                         submitButton(text = "refresh", icon("refresh")),
                         plotOutput("plotpca",height = 400),
                         h4(textOutput("caption1")),verbatimTextOutput("summary"), br(), br()),
                
                tabPanel("Cluster Plot",br(), 
                         (p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         verbatimTextOutput("mscount"),
                         p("Select X data with only numerical varaibles"),
                         p("Choose cluster algorithm (default clusters = 2)"),
                         p("Click 'refresh' and wait for algorithm to finish estimation"),
                         submitButton(text = "refresh", icon("refresh")),
                         plotOutput("plot"), br()  ),   #,width = 400)),
                tabPanel("Download Cluster Membership Data",
                         h4("Download Cluster Membership Data"),
                         downloadButton('downloadData4', 'download membership data'), br(),
                         h4("Click 'refresh' and wait for algorithm to finish estimation"),
                         submitButton(text = "refresh", icon("refresh")),br(),
                         dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                        br(),br()  )
                
        )         
      ) 
    ) 
  )

