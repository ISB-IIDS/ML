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
    selectInput("select", "Choose cluster algorithm", c("K-Means","Hierarchical","Spectral","HDBSCAN"), selected = "K-Means"),  
    htmlOutput("Clust"),
    htmlOutput("outcl"),
    htmlOutput("Clusth"),
    #selectInput("Clust", "Choose number of clusters", c(2,3,4,5,6,7,8,9), selected = 3),
    htmlOutput("samsel"),
    #selectInput("obs", "Select sub sample", c("quick run, 10,000 obs", "full dataset"), selected = "quick run, 10,000 obs"),
    #selectInput("scale", "Standardize input data (usually yes)",c("yes","no"), selected = "yes"),
    htmlOutput("scale"),
    htmlOutput("imputemiss"),
    htmlOutput("winsor"),
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
                         h4("Data Summary of Selected X"),htmlOutput("imout"),verbatimTextOutput("summ"),br(),
                         h4("Missing Data"),verbatimTextOutput("missing"),br(),
                         h4("Correlation Table - Input data"), verbatimTextOutput("correlation"),
                         #h4(p("Correlation")),
                         #(p('remove missing data variable(s) if any, or impute or drop rows - check options in the panel on the left',style="color:black")),
                         #(plotOutput("corplot",height = 850, width = 850))
                         br()),

                tabPanel("Data Visulization",br(),
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
                
                # tabPanel("Missing Data",verbatimTextOutput("mscount"),
                #          (p('Remove missing data variable(s), if any, by unchecking it in the "Data Selection" panel on the left, and click refresh.',style="color:red")),
                #          submitButton(text = "refresh",icon("refresh")),
                #          h4("Missing Data"),verbatimTextOutput("missing")
                #          ),
                
                tabPanel("Cluster Analysis",br(),
                         #p("Step1: Choose cluster algorithm"),
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters."),
                         #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                         p("Note: Wait for algorithm to finish estimation, it takes a while. Hierarchical & spectral algorithms takes more time.",style="color:black"),
                         #submitButton(text = "refresh", icon("refresh")),
                         actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         #plotOutput("plotpca",height = 400, width = 850),
                         h4("Check Missing Data Rows Count"),verbatimTextOutput("mscount"),
                         #(p('Remove missing data variable(s) if any - check  "Data Summary" tab',style="color:red")),
                         h4(textOutput("caption")),verbatimTextOutput("summary"), 
                         plotOutput("plot3",height = 600),
                         br(),br(),
                         htmlOutput("scldt1note"),
                         htmlOutput("stnote"),
                         # h4("Scaled Data Boxplot (if 'Standardize data' option = yes)"),
                         plotOutput("boxx2"),
                         plotOutput("sbplot"),
                         #p("Note: Input data is standardized before running cluster analysis.", style="color:black"),
                         #h4("Summary Standardize Data"),
                         #verbatimTextOutput("scldt"),
                         br()),
                
                # tabPanel("Cluster Plot",br(),
                #          
                #          #p("Step1: Choose cluster algorithm"),
                #          p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                #          (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                #          p("Step3: Choose cluster algorithm and number of clusters."),
                #          #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                #          p("Step4: Wait for algorithm to finish estimation, it takes a while. Hierarchical ans spectral algorithms takes more time.",style="color:black"),
                #          #submitButton(text = "refresh", icon("refresh")),
                #          actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                #         # h4(textOutput("caption2")),verbatimTextOutput("summary1"),
                #         # h4("Projecting input data onto the first two principal components dimensions."),
                #         # plotOutput("plot", height=850), 
                #          plotOutput("plotcor"),
                #          #plotOutput("plot2",height = 400, width = 850),
                #          br(),br()),
                
                tabPanel("PCA Plot",br(),
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters."),
                         #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                         p("Note: Projecting input data onto top 2 principal component dimensions (PCA) takes time, be patient.",style="color:black"),
                         tags$a(href="https://en.wikipedia.org/wiki/Principal_component_analysis", "-Wikipedia"),br(),
                        # br(),
                        # actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         h4(textOutput("caption2")),verbatimTextOutput("summary1"),
                         h4("Visulaizing Clusters in 2D (top two principal components)."),
                         plotOutput("plot", height=850), 
                         #h4("Selected Variables Boxplot"),#plotOutput("boxx"),
                         #plotOutput("boxx2"),

                        # plotOutput("plotcor"),
                         #plotOutput("plot2",height = 400, width = 850),
                         br(),br()),
                
                tabPanel("Download Cluster Membership Data", br(), 
                         
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters."),
                         p("Note: Wait for algorithm to finish estimation, hierarchical and spectral algorithms takes more time.",style="color:black"),
                         #submitButton(text = "refresh", icon("refresh")),
                        # actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         h4(textOutput("caption1")),
                         htmlOutput("colList"),
                         #h4("Download Cluster Membership Data"),
                         
                         downloadButton('downloadData4', 'download cluster solution'), br(),br(),
                         #submitButton(text = "refresh", icon("refresh")),
                         #(p('Examine  "Missing Data" tab and uncheck missing data variable(s), if any, in the "Data Selection" panel on the left',style="color:red")),
                         #h4("Click 'refresh' and wait for algorithm to finish estimation"),
                         dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}")),
                        br(),br()),  
                        tabPanel("t-SNE Plot", br(),
                                 p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                                 (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                                 p("Step3: Choose cluster algorithm and number of clusters."),
                                 #p("Step4: Wait for algorithm to finish estimation, hierarchical algorithm takes more time.",style="color:black"),
                                 (p('Note: Projecting input data onto three dimensions (t-SNE) takes a while, be patient.',style="color:black")),
                                 tags$a(href="https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding", "-Wikipedia"),br(),
                                 br(),
                                 #selectInput("dupp", "Drop duplicate values", c("Yes","No"), selected = "No"),
                                 selectInput("perp", "Set perplexity parameter", c(5, 25, 50, 100, 500), selected = 25),
                                 #selectInput("iter", "Set max iterations", c(500,1000), selected = 500),
                                 #numericInput("iter", "Set max iterations",500, min=500, max=5000, step=500),
                                # actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                                 h4(textOutput("caption3")),
                                 #plotOutput("plotumap"),
                                 plotOutput("plotrtsne",height = 800),
                                 br(),br()),
                  tabPanel("UMAP Visualization", br(),
                         p("Step1: Select only numerical X variables in the 'Data Selection' panel on the left."),
                         (p('Step2: Examine  "Data Summary" tab and check missing data variable(s), if any.',style="color:black")),
                         p("Step3: Choose cluster algorithm and number of clusters."),
                         #p("Step4: Click 'refresh' and wait for algorithm to finish estimation.",style="color:black"),
                         p("Note: Wait for algorithm to finish estimation, it takes a while.",style="color:black"),
                         tags$a(href="https://en.wikipedia.org/wiki/Nonlinear_dimensionality_reduction", "-Wikipedia"),br(),
                         #submitButton(text = "refresh", icon("refresh")),
                         #actionButton(inputId = "apply",label = "refresh", icon("refresh")),br(),br(),
                         h4(textOutput("caption4")),
                         plotOutput("plotumap", height=800),
                         br())

        )         
      ) 
    ) 
  )

