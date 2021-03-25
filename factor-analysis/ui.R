#################################################
#               Factor Analysis                 #
#################################################

library("shiny")
library("pastecs")
library("nFactors")
library("qgraph")
library("corrplot")
library("dplyr")
library("DT")
library("Hmisc")
#library("foreign")

shinyUI(fluidPage(
  #titlePanel(title=div(img(src="logo.png",align='right'),"Factor analysis")),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Factor analysis App", style="bold")), windowTitle	='Factor analysis'),
  # Input in sidepanel:
  sidebarPanel(
    # Upload data:
    h4(p("Data Input")),
    p("First column of the input data must be an obervation id.",style="color:red"),
    fileInput("file", "Upload input data (csv file with observation ID and header)"),  
    uiOutput("colList"),
    #numericInput("fselect", "Number of Factors:", 2, min=2),
    htmlOutput("fselect"),
    textInput('fname',label = "Enter Factor Name (seperated by comma)"),
    sliderInput("cutoff", "Cut-off for factor loadings(for Plotting only)", min = 0,  max = 1, value = 0.25),
    htmlOutput("xaxis"),
    htmlOutput("yaxis"),
    
    sliderInput("cutoffcorr", "Cut-off for Correlation in Factors vs Variable", min = 0,  max = 1, value = 0.25),
    br()
     # submitButton(text = "Apply Changes", icon("refresh"))
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #

tabPanel("Overview",
# h5(p("Factor Analysis")), 
# p("Factor analysis is a statistical method used to describe variability among observed, correlated variables in terms of a potentially lower 
# number of unobserved variables called factors. For example, it is possible that variations in four observed variables mainly reflect the variations 
# in two unobserved variables. Factor analysis searches for such joint variations in response to unobserved latent variables. The observed variables 
# are modelled as linear combinations of the potential factors, plus error terms. The information gained about the interdependencies between 
# observed variables can be used later to reduce the set of variables in a dataset. Computationally this technique is equivalent to low rank 
# approximation of the matrix of observed variables. Factor analysis originated in psychometrics, and is used in behavioral sciences, social sciences,
# marketing, product management, operations research, and other applied sciences that deal with large quantities of data.",align="justify"),
# a(href="http://en.wikipedia.org/wiki/Factor_analysis","- Wikipedia"),
h4(p("How to use this shiny application")),
p("This shiny application requires one data input from the user. To do so, click on 'Browse' (in left side-bar panel) and upload the csv data input file.
  Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
  and then proceed."),

p("Once csv file is uploaded successfully, application will fit a factor model with optimal factors from parallel Analysis and various 
results will be showed in the above tabs. In the panel on the left, you can change the parameters' value and correspondingly new results.",align="justify"),
p("Note: Factor analysis can be performed only on the numercial data.",style="color:black"),
tags$a(href="https://en.wikipedia.org/wiki/Factor_analysis", "-Wikipedia"),
br(),br(),
#h4(p("Download Sample Input File")),
downloadButton('downloadData', 'download sample data'),
#p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
#img(src = "example1.png") #, height = 280, width = 400

),
    
                tabPanel("Data Summary",#h4("Selected Variables"), verbatimTextOutput("head"),#verbatimTextOutput("tail"),
                         h4("Uploaded Data"), 
                         dataTableOutput("readdata"),tags$head(tags$style("tfoot {display: table-header-group;}")),br(),
                         h4("Data Summary of Selected Varaibles"),verbatimTextOutput("summary"),h4("Missing Data"),
                         verbatimTextOutput("missing"),
                         h4(p("Correlation")),
                         (p("Remove missing data variable(s) if any",style="color:red")),
                         (plotOutput("corplot",height = 850, width = 850))
                          ),
                tabPanel("Model Summary",         
                         (h4(p("Test Summary"))),(textOutput("text1")),(textOutput("text2")),(textOutput("text3")),
                         (h4(p("Factors Loadings Summary"))),
                          (verbatimTextOutput("mat")),
                          br(),
                         (h4(p("Uniqueness table"))),
                         (dataTableOutput("uni")),br(),br(),
#                          (textOutput("text4")),
                         plotOutput("plot1",height = 600, width = 850)
                          # h4(p("Correlation")),
                          # (p("Remove missing data variable(s) if any, check 'Data Summary' tab", style="color:red")),
                          # (plotOutput("corplot",height = 850, width = 850))
                          ),
                tabPanel("Loadings",br(),dataTableOutput("loadings"),br(),br()),
                
#                tabPanel("Scores",tableOutput("scores")),   # origi code
                # my edits 16-9-2017 below:
                tabPanel("Scores", 	# tab name
	                      (verbatimTextOutput("mscount")),
	                      (p('Remove missing data variable(s) if any- check "Data Summary" tab',style="color:red")),
                        downloadButton('downloadDataX', 
		                        'download factor scores file'), 
	                      br(),br(),
	                      dataTableOutput("scores")),
                
                tabPanel("Factor vs Variables",plotOutput("plot20",height = 850, width = 850)),
                tabPanel("Factor vs Variables 2",plotOutput("plot2",height = 850, width = 850)),
                tabPanel("Factor vs Users",plotOutput("plot3",height = 850, width = 850))
                #tabPanel("Data",br(),dataTableOutput("table"),br(),br()) 
    )
  ) 
) 
)
