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
    fileInput("file", "upload data (csv file with header) first column must be observation id"),
    selectInput("select", "choose cluster algorithm", 
                       c("K-Means","Hierarchical"), selected = "K-Means"),
    numericInput("Clust", "choose number of clusters", 3),
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
                         p("One useful application of cluster analysis is in marketing. Market segementaion is a startegy which involves dividing a broad target market into subsets of consumers, businesses, or countries who have, or are perceived to have, common needs, interests, and priorities, and then designing and implementing strategies to target them. Market segmentation strategies are generally used to identify and further define the target customers, and provide supporting data for marketing plan elements such as positioning to achieve certain marketing plan objectives. Businesses may develop product differentiation strategies, or an undifferentiated approach, involving specific products or product lines depending on the specific demand and attributes of the target segment.",
                           align="justify"),
                         h4(p("Data input")),
                         p("This application requires input data from the user. To do so, click on the Browse (in left side-bar panel) and upload the Segmentation data input file.
                            Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                            and then proceed. Make sure you have top row as variable names and first column as id in csv file"
                           ,align="justify"),
                         
                         p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 3 segments. In left-side bar panel you can change the segmentation algorithm and number of segments. Click on Apply changes after making any change in the inputs. Accordingly results will be updates in all the tabs",
                           align="justify"),
                          ),
                tabPanel("Summary Stats",
                         #h4("select only numerical variables with no missing values in X and click 'Refresh' "), 
                         #verbatimTextOutput("head"),
                         #verbatimTextOutput("tail"),
                         h4("data summary"),verbatimTextOutput("summ"),
                         submitButton(text = "refresh - apply changes", icon("refresh")),br(),
                         verbatimTextOutput("mscount"),
                         h4("missing data rows"),verbatimTextOutput("missing")),
                
                #tabPanel("Data",h3(textOutput("caption"),tableOutput("table"))),
                
                tabPanel("Cluster Analysis",
                         h4("standardized data is used for clustering"),
                         verbatimTextOutput("scldt"),br(),
                         h4("select columns with only numerical varaibles"),
                         h4("check data has no missing values"),
                         h4("choose cluster algorithm"),
                         h4("click 'refresh' and wait for algorithm to finish estimation"),
                         submitButton(text = "refresh", icon("refresh")),
                         plotOutput("plotpca",height = 400, width = 500),
                         h4(textOutput("caption1")),verbatimTextOutput("summary")),
                
                tabPanel("Cluster Plot",br(), 
                         h4("select columns with only numerical varaibles"),
                         h4("check data has no missing values"),
                         h4("choose cluster algorithm"),
                         h4("click 'refresh' and wait for algorithm to finish estimation"),
                         submitButton(text = "refresh", icon("refresh")),
                         plotOutput("plot",height = 700, width = 840)),
                tabPanel("Download Cluster Membership Data",br(),
                         downloadButton('downloadData4', 'Download output file (Works only in browser)'), br(),br(),
                         dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}")))
                
                )
      ) 
    ) 
  )
# tabPanel("PCA Variance Plot",plotOutput("plot1", width = "100%")),
# tabPanel("JSM Plot",plotOutput("plot", height = 800, width = 840)),
