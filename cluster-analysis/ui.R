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
    p("First column of the input data must be an obervation id.",style="color:red"),
    fileInput("file", "Upload data (csv file with observation ID and header)"),
    #submitButton(text = "refresh", icon("refresh")),
    #(p("Click refresh after loading data and every time you make changes",style="color:red")),
    #actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
    #submitButton(text = "refresh", icon("refresh")),br(),
    #htmlOutput("Clust"),
    #numericInput("Clust", "Choose number of clusters",3),
    #submitButton(text = "refresh", icon("refresh")),
    #(p("Click refresh after loading data and every time you make changes",style="color:red")),
    #actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),
    h4(p("Data Selection")),
    htmlOutput("xvarselect"),
    selectInput("select", "Choose cluster algorithm", c("K-Means","Hierarchical","Spectral"), selected = "K-Means"),  
    htmlOutput("Clust"),
    #selectInput("Clust", "Choose number of clusters", c(2,3,4,5,6,7,8,9), selected = 3),
    htmlOutput("samsel"),
    #selectInput("obs", "Select sub sample", c("quick run, 10,000 obs", "full dataset"), selected = "quick run, 10,000 obs"),
    selectInput("scale", "Standardize input data (usually yes)",c("yes","no"), selected = "yes"),
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
                         p("Note: Cluster analysis can be performed only on the numercial data.",style="color:black"),
                         tags$a(href="https://en.wikipedia.org/wiki/Cluster_analysis", "-Wikipedia"),
                         h4(p("Data input")),
                         #p("First column of the input data must be an obervation id.",style="color:red"),
                         p("This application requires input data from the user in csv format. To upload data, click on the 'Browse' (in the panel on the the left) 
                         and upload the Segmentation data input file.
                            Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                            and then proceed. Make sure you have top row as variable names and first column as id in csv file"
                           ,align="justify"),
                         
                         p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 3 segments. 
                           In the panel on the left you can change the segmentation algorithm and number of segments. Accordingly results will be updated in all the tabs",
                           align="justify"),
                        # h4(p("Download Sample Input File")),
                         downloadButton('downloadData', 'download sample input file'),
                         br(),br(),
                         #p("Cluster analysis or clustering is the task of grouping a set of objects in such a way that objects in the same group (called a cluster) are more similar (in some sense) to each other than to those in other groups (clusters). It is a main task of exploratory data mining, and a common technique for statistical data analysis, used in many fields, including pattern recognition, image analysis, information retrieval, bioinformatics, data compression, computer graphics and machine learning."),
                         
                          ),
                tabPanel("Data Summary",
                         #h4("select only numerical variables with no missing values in X and click 'Refresh' "), 
                         #verbatimTextOutput("tail"),h4("Selected Variables"), verbatimTextOutput("head"),
                         #br(), (p('First column of the input data must be an observation id',style="color:red")),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         #(p("Click refresh after loading data and every time you make changes",style="color:black")),
                         #submitButton(text = "refresh", icon("refresh")),
                         #actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),
                         h4("Data Summary of Selected X"),verbatimTextOutput("summ"),br(),
                         h4("Missing Data"),verbatimTextOutput("missing"),
                         h4(p("Correlation")),
                         (p('Remove missing data variable(s) if any',style="color:red")),
                         (plotOutput("corplot",height = 850, width = 850))
                         ),

                # tabPanel("Missing Data",verbatimTextOutput("mscount"),
                #          (p('Remove missing data variable(s), if any, by unchecking it in the "Data Selection" panel on the left, and click refresh.',style="color:red")),
                #          submitButton(text = "refresh",icon("refresh")),
                #          h4("Missing Data"),verbatimTextOutput("missing")
                #          ),
                
                tabPanel("Cluster Analysis",br(),
                         #p("Step1: Choose cluster algorithm"),
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters"),
                         #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                         p("Step4: Wait for algorithm to finish estimation, it takes a while. Hierarchical algorithm takes more time.",style="color:black"),
                         #submitButton(text = "refresh", icon("refresh")),
                         actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         #plotOutput("plotpca",height = 400, width = 850),
                         h4("Missing Data Rows Count"),verbatimTextOutput("mscount"),
                         #(p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         h4(textOutput("caption")),verbatimTextOutput("summary"), 
                         plotOutput("plot3",height = 600),
                         br(),
                         htmlOutput("stnote"),
                         #p("Note: Input data is standardized before running cluster analysis.", style="color:black"),
                         #h4("Summary Standardize Data"),
                         #verbatimTextOutput("scldt"),
                         br()),
                
                tabPanel("Cluster Plot",br(),
                         
                         #p("Step1: Choose cluster algorithm"),
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters"),
                         #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                         p("Step4: Wait for algorithm to finish estimation, it takes a while. Hierarchical algorithm takes more time.",style="color:black"),
                         #submitButton(text = "refresh", icon("refresh")),
                         actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         h4(textOutput("caption2")),verbatimTextOutput("summary1"),
                         h4("Projecting input data onto the first two principal components dimensions."),
                         plotOutput("plot", height=850),  
                         #plotOutput("plot2",height = 400, width = 850),
                         br(),br()),
                tabPanel("Download Cluster Membership Data", br(), 
                         
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters"),
                         p("Step4: Wait for algorithm to finish estimation, hierarchical algorithm takes more time.",style="color:black"),
                         #submitButton(text = "refresh", icon("refresh")),
                         actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         h4(textOutput("caption1")),
                         htmlOutput("colList"),
                         #h4("Download Cluster Membership Data"),
                         
                         downloadButton('downloadData4', 'download membership data'), br(),br(),
                         #submitButton(text = "refresh", icon("refresh")),
                         #(p('Examine  "Missing Data" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left',style="color:red")),
                         #h4("Click 'refresh' and wait for algorithm to finish estimation"),
                         dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                        br(),br()),  
                        tabPanel("Class Visualization", br(),
                                 p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                                 (p('Step2: Examine  "Data Summary" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left.',style="color:black")),
                                 p("Step3: Choose cluster algorithm and number of clusters"),
                                 #p("Step4: Wait for algorithm to finish estimation, hierarchical algorithm takes more time.",style="color:black"),
                                 (p('Note: projecting input data onto three dimensions (t-SNE) takes a while, be patient.',style="color:red")),
                                 tags$a(href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding", "-Wikipedia"),br(),
                                 br(),
                                 selectInput("perp", "Set Perplexity Parameter", c(10,25,50,100), selected = 25),
                                 selectInput("iter", "Set Max Iterations", c(500,1000,5000), selected = 500),
                                 #numericInput("perp", "Set Perplexity Parameter",25 ,min=5,max=95, step=10),
                                 #numericInput("iter", "Set Max Iterations",500, min=500, max=5000, step=500),
                                 actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                                 h4(textOutput("caption3")),
                                 plotOutput("resplot4",height = 800),
                                 br(),br())

        )         
      ) 
    ) 
  )

