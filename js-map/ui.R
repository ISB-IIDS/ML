#################################################
#               Joint Space Map                 #
#################################################


library("shiny")
#library("foreign")
 
shinyUI(fluidPage(
  # Header:
 # titlePanel(title=div(img(src="logo.png",align='right'),"Joint Space Map")),
  headerPanel(title=div(img(src="logo.png",align = "right"), h2("Joint Space Map App", style="bold")), windowTitle	='Joint Space Map'),
  #titlePanel("Joint Space Map"),
  # Input in sidepanel:
  sidebarPanel(

    # Upload data:
    h4(p("Perceptual Data Input")),
    fileInput("file", "Upload Perceptual data"),
    # upload secon data
    h4(p("Preference Data Input")),
    fileInput("file1", "Upload Preference Data"),
    # Variable selection:
    numericInput("k0","Scaling Factor Entities",1.5),
    numericInput("k1","Scaling Factor User",1.5),
    h5(p("Data Selection (Optional)")),
    h6(p("A -  Perceptual (Attributes)")),
    htmlOutput("varselect"),
    h6(p("B -  Perceptual (Firms - only for Spider Chart)")),
    htmlOutput("varselect2"),
    
    h6(p("C -  Preference")),
    # upload secon data
    htmlOutput("varselect1"),
    br()
  ),
  # Main:
  mainPanel(
    
    tabsetPanel(type = "tabs",
                #
                
tabPanel("Overview",
         h4(p("Perceptual mapping")),
         p("Perceptual Mapping is the use of graphs to identify the positioning of products/brands that consumers have, and find their preference. The graphs layout an X and Y axis with variables and ranges from the most desirable to least desirable. For instance, the far right may be listed as 'Upper class' while the left side will be 'Lower Class'. This allows for the placement of business names to help find the position that consumers place these businesses in relation to the variables listed."
           ,align="justify"),
         a(href="https://en.wikipedia.org/wiki/Perceptual_mapping","- Wikipedia")
         
         ),
tabPanel("Data Input Format",h4(p("Data input")),
         p("To plot joint space map, this app needs two inputs from the user. In the panel on the left, click on 'Browse' and upload 
the perceptual data. Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, 
first convert your data in csv format and then proceed. Make sure you have top row as variable names and first column as respondent id/name 
in csv file. As soon as upload is complete, this app will read the data and all the variables in perceptual file will reflect in the panel on
the left. Now click on second 'Browse' link and upload the preference data in csv format. As soon as upload is complete, respondents in preference
data file will reflect in the panel on the left. Now you can navigate across different tab viz. 'PCA Variance Plot' tab, 'JSM plot' tab and 'Data' tab. Sample perceptual file and preference file is shown 
below",align="justify"),
         p('Perceptual Sample File'),
         img(src = "perceptual.png", height = 180, width = 400),
         br(),br(),
         p('Preference Sample File'),
         img(src = "preference.png", height = 180, width = 400),
         br(),br(),
         #, height = 280, width = 400
         h4(p("Download sample perceptual & preference file")), 
         #downloadButton('downloadData1', 'Download Perceptual file example 1'),br(),br(),
         #          downloadButton('downloadData2', 'Download Preference file example 1'),br(),br(),
         #          
         downloadButton('downloadData3', 'download perceptual sample data'),br(),
         downloadButton('downloadData4', 'download preference sample data'),br(),
         h4(p("Data Selection")),
         p("This app gives users additional functionality to modify the data. By default, all the variables in perceptual file are 
         selected for PCA but users can deselect/select variables in perceptual data in the panel on the left as per their requirement. 
         Similarly, users can also deselect/select respondents from preference data
  in the panel on the left. As soon as users make a change in data selection, all the tabs will be updated.",
           align="justify")),
# tabPanel("Example dataset", 
#          p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
#          img(src = "example1.png")  ),

                tabPanel("PCA Variance Plot",plotOutput("plot1", width = "100%")),
                tabPanel("JSM Plot",plotOutput("plot", height = 800, width = 840)),
                tabPanel("Spider Chart",plotOutput("spiderplot", height = 800, width = 840)),
                tabPanel("Data",h5(p("Perceptual Data")),tableOutput("table"),h5(p("Preference Data")),tableOutput("table1"))
#     tableOutput("table"),  
#     tableOutput("table1"),
#     plotOutput("plot", width = "100%"),
#     plotOutput("plot1")
  )
))
)
