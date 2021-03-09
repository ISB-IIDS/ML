#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
#  titlePanel("RFM Analysis"),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("RFM Analysis App", style="bold")), windowTitle	='RFM Analysis'),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
       fileInput("data","Upload RFM data"),
       numericInput("bins","Number of bins",5,min=3,max=7),
       br(),
       fileInput("segmentData","Upload segment name data"),
       width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Overview",
                           h3("RFM Analysis"),
                           p("RFM is a method used for analyzing customer value. It is commonly used in database marketing and direct marketing and has received particular attention in retail and professional services industries."),
                           p("RFM stands for the three dimensions:"),
                           p("Recency - How recently did the customer purchase?"),
                           p("Frequency - How often do they purchase?"),
                           p("Monetary Value - How much do they spend?"),
                           a(href="https://en.wikipedia.org/wiki/RFM_(customer_value)","RFM (customer value)"),
                           h3("How to use this app?"),
                           h4("Example dataset"),
                           p("There are two sets of example datasets provided with this app (RFM-Data and Segment-Names)"),
                           p("The input for segment name descriptors can also be generated using the app. After uploading the data you can download the Segement description data from the 'Segmentation' tab of the app and can further use that as second input. "),
                           h5("Custom dataset"),
                           p("You can also use the app to analyse your custom input data. However the input file has to be formatted according to specific guidelines"),
                           tags$ul(
                             tags$b("RFM data"),
                             tags$li("The input file has be in csv format."),
                             tags$li("The first column has to be unique customer id."),
                             tags$li("The subsequent columns should be recency, frequency and revenue (in order)."),
                             tags$li("In case of confusion refer to the example datasets."),
                             br(),
                             tags$b("Segment name data"),
                             tags$li("The input file has be in csv format."),
                             tags$li("For convienience default segment names have been provided. You can always change the names and re-upload for updated results."),
                             tags$li("You may only modify the segment names. No other modifications should be made to the downloaded segment descriptions file. ")
                           ),  
                           br(),
                           p(downloadButton('downloadData1','Download Example RFM-Data (works only in browser)')),
                           p(downloadButton('downloadData2','Download Example Segment-Name (works only in browser)')),
                          # p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                            ),
                  
                  tabPanel("Score metrics",
                           tags$style(type = "text/css", 
                                      "#freqDist {height: calc(100vh - 150px) !important;}"),
                           plotOutput("rfreqDist"),br(),
                           plotOutput("ffreqDist"),br(),
                           plotOutput("mfreqDist"),
                           #plotOutput("scoreHist")
                           ),
                  
                  tabPanel("RFM output",
                            fluidRow("",
                                     splitLayout(
                                       cellWidths = c("50%","50%"),
                                       plotOutput("graph1"),
                                       plotOutput("graph4")),
                                     br(),
                                     plotOutput("graph2"),br(),
                                   #  plotOutput("graph3"),br(),
                                   plotOutput("graph5"),br(),
                                   plotOutput("graph6"),br(),
                                   plotOutput("graph7"),br(),
                                   )
                           ),
                  
                  tabPanel("Segment Level Analyis",
                           fluidRow("",
                                    #downloadButton('downloadSegmentData','Download Segmention data'),
                                    h4("Upload segment name data file"),
                                    plotOutput("segment1",width = "70%"),br(),
                                    plotOutput("segment3",width = "70%"),br(),
                                    plotOutput("segment2",width = "70%")
                                    )
                           
                          )
      )
    )
  )
))
