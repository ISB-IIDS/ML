#################################################
#      Summary & OLS App                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")} #for stat.desc
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if (!require("Hmisc")){install.packages("Hmisc")} # for describe
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("hydroGOF")) {install.packages("hydroGOF")}
#if (!require("PerformanceAnalytics")) {install.packages("PerformanceAnalytics")}
if (!require("mice")) {install.packages("mice")}

library(shiny)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(hydroGOF)
library(mice)
#library(PerformanceAnalytics)

# library(gplot)

shinyServer(function(input, output,session) {
  
Datasetf <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})

output$samsel <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    selectInput("obs", "Select sub sample", c("quick run, 1,000 obs", "10,000 obs", "full dataset"), selected = "quick run, 1,000 obs")
  }
})

Datasetf1 <- reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  if (input$obs=="full dataset") { return(Datasetf()) }
  else if(input$obs=="10,000 obs") 
  {
    if (nrow(Datasetf())>10000){
      set.seed(1234)
      testsample= sample(1:nrow(Datasetf()), 10000 )
      Dataset1=Datasetf()[testsample,]
      return(Dataset1)}
    else {return(Datasetf())}
  }
  else (input$obs=="1,000 obs")
  {
    if (nrow(Datasetf())>1000){
      set.seed(1234)
      testsample= sample(1:nrow(Datasetf()), 1000 )
      Dataset1=Datasetf()[testsample,]
      return(Dataset1)}
    else {return(Datasetf())}
  }  }
})

output$imputemiss <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    #if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
    if (1==0) {p("error")}
    else {
      selectInput("imputemiss", "Impute missing vaulues or drop missing value rows", 
                  c("do not impute or drop rows", "impute missing values", "drop missing value rows"), selected = "drop missing value rows")
    }}
})

output$imout <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  if (input$imputemiss == "do not impute or drop rows") {return(NULL)}
  else if ((input$imputemiss == "impute missing values")) {
    p("Note: missing values imputed - check options in the panel on the left",style="color:black")
  }
  else { p("Note: missing value rows dropped - check options in the panel on the left",style="color:black") }
})

Dataset = reactive({
  if (input$imputemiss == "do not impute or drop rows") 
  { mydataimp=Datasetf1() }
  else if (input$imputemiss == "impute missing values") 
  { mydata = Datasetf1()
  mice_mod = mice(mydata, printFlag=FALSE)
  mydataimp <- complete(mice_mod) }
  else # (input$imputemiss == "drop missing value rows") 
  { mydata = Datasetf1()
  mydataimp = na.omit(mydata)  }
  return(mydataimp)
})


output$readdata <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    Datasetf()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

Datasetp <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

output$readdatap <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    Datasetp()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

nu.Dataset = reactive({
  data = Datasetf1()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = class(data[,i])
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric","integer"))
  nu.data = data[,nu] 
  return(nu.data)
})

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  selectInput("yAttr", "Select Y variable",
                     colnames(nu.Dataset()), colnames(nu.Dataset()[,-1])[1])
  }
})

output$yout <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Y is",input$yAttr,style="color:red")
})

output$yout1 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Predicted Y is",input$yAttr,style="color:red")
})

output$yout2 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Y is",input$yAttr,style="color:red")
})

output$yout3 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Predicted Y is",input$yAttr,style="color:red")
})

output$xvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Datasetf1()),input$yAttr), setdiff(colnames(Datasetf1()[,-1]),input$yAttr))
  }
})

Dataset.temp = reactive({
  mydata = Dataset()
})

nu1.Dataset = reactive({
  data = Dataset.temp()
  Class = NULL
  for (i in 1:ncol(data)){
    c1 = class(data[,i])
    Class = c(Class, c1)
  }
  nu = which(Class %in% c("numeric","integer"))
  nu.data = data[,nu] 
  return(nu.data)
})

output$fxvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables in X",
                     #setdiff(colnames(Dataset.temp()[,c(input$yAttr,input$xAttr)]),input$yAttr),setdiff(colnames(Dataset.temp()),c(input$yAttr,colnames(nu1.Dataset()))) )
                      colnames(Dataset.temp()[,c(input$xAttr)]),setdiff(colnames(Dataset.temp()),c(colnames(nu1.Dataset()))) )
      }
})

mydata = reactive({
  mydata1 = Dataset()[,c(input$yAttr,input$xAttr)]
  if (!is.null(input$fxAttr)){
  for (j in 1:length(input$fxAttr)){
      mydata1[,input$fxAttr[j]] = factor(mydata1[,input$fxAttr[j]])
                                    }}
  return(mydata1)
})

pred.readdata = reactive({
  mydata = Datasetp()
  if (!is.null(input$fxAttr)){
    for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
    }
  }
  return(mydata)
  
})

Dataset.Predict <- reactive({
  fxc = setdiff(input$fxAttr, input$yAttr)
  
  if ((input$yAttr %in%  colnames(pred.readdata()) ) ) {
  mydata = pred.readdata()[,c(input$yAttr,input$xAttr)]}
  else {mydata = pred.readdata()[,c(input$xAttr)]}
  
  if (!is.null(input$fxAttr)){
    for (j in 1:length(fxc)){
      mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
    }
  }
  return(mydata)
})

out = reactive({
data = mydata()
Missing1=(data[!complete.cases(data),])
Missing=head(Missing1)
mscount=nrow(Missing1)
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","integer"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j, Head=Head,MissingDataRows=Missing,missing.data.rows.count=mscount)
return(out)
})

testsample =  reactive({
  set.seed(5898)
  sample(1:nrow(mydata()), round(nrow(mydata())*((input$sample)/100)))
})

train_data = reactive({
  mydata() #[-testsample(),]
})

test_data = reactive({
  mydata() #[testsample(),]
})

output$trainobs = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dim( train_data())
  }
})

output$testobs = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dim( test_data())
  }
})

output$head = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[8]
  }
})

output$tail = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[3]
  }
})

output$missing = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[9]
  }
})

output$mscount = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[10]
  }
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})

# output$scatterplots <- renderUI({
#   if (is.null(input$file)) {return(NULL)}
#   else {
#     
#     plot_output_list <- lapply(1:out()[[7]], function(i) {
#       plotname <- paste("plot", i, sep="")
#       plotOutput(plotname, height = 700, width = 700)
#     })
#     # Convert the list to a tagList - this is necessary for the list of items
#     # to display properly.
#     do.call(tagList, plot_output_list)
#   }
# })
# 
# # Call renderPlot for each one. Plots are only actually generated when they
# # are visible on the web page.
# max_plots = 50
# 
# for (i in 1:max_plots) {
#   # Need local so that each item gets its own number. Without it, the value
#   # of i in the renderPlot() will be the same across all instances, because
#   # of when the expression is evaluated.
#   local({
#     
#     my_i <- i 
#     plotname <- paste("plot", my_i, sep="")
#     
#     output[[plotname]] <- renderPlot({
#       out1 = out()
#       a = out1[[6]]
#       j = my_i
#       if (ncol(out1[[5]]) == a[j] + 1){
#         a1 = a[j]+1
#         a2 = a[j]-1
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#         }
#       
#       else if ( ncol(out1[[5]]) < a[j + 1]){
#         a1 = a[j]+1
#         a2 = ncol(out1[[5]])
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       else if(ncol(out1[[5]]) > a[j + 1]){
#         a1 = a[j]+1
#         a2 = a[j + 1]
#         dai = out1[[5]][,a1:a2]
#         plot(dai)
#       }
#       
#       mtext(paste("Scater plot " ,my_i), side = 3, line = 2, cex=2)
#         })
#   })
# }

output$hist = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    hist(mydata())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$dens = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    datadensity(out()[[5]])#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$heatmap = renderPlot({ 
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
})

plotsample =  reactive({
  sample(1:nrow(mydata()), round( if (nrow(mydata()>100)) {100} else {nrow(mydata())}  ))
})

plot_data = reactive({
  my_data = out()[[5]]
  my_data[plotsample(),]
})


output$heatmap1 = renderPlot({ 
  chart.Correlation(plot_data(),hitogram=TRUE)
})

output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  round(cor(out()[[5]], use = "pairwise.complete.obs"),4)
  }
  })

output$corplot = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    #pairs(out()[[5]])
    pairs(out()[[5]],pch=20, col="grey")
  }
})

output$corplot1 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
  my_data = out()[[5]]
  cor.mat <- round(cor(my_data),2)
  corrplot(cor.mat, 
           type = "upper",    # upper triangular form
           order = "hclust",  # ordered by hclust groups
           tl.col = "black",  # text label color
           tl.srt = 45)  
  }
})

ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    formula= as.formula(paste(input$yAttr,"~", rhs , sep=""))
    ols = lm(formula, data = test_data())
  return(ols)
})

ols2 = reactive({
  
  drop = which(input$yAttr == colnames(out()[[5]]))
               
  x0 = out()[[5]][,-drop]
  x01 = scale(x0, center = T, scale = T)
  
  y1 = out()[[5]][,drop]
  y  = scale(y1,center = T, scale =T)
  
  dstd = data.frame(y,x01)
  colnames(dstd) = c(input$yAttr,colnames(x01))
  
  if (ncol(data.frame(out()[[4]])) == 1) {
    fdata = data.frame(out()[[4]])
    colnames(fdata) = input$fxAttr
    dstd = data.frame(dstd,fdata)
  }
  
  else if (ncol(data.frame(out()[[4]])) > 1) {
    fdata = data.frame(out()[[4]])
    dstd = data.frame(dstd,fdata)
  }
  Dimensions = dim(dstd)
  Class = NULL
  for (i in 1:ncol(dstd)){
    c1 = class(dstd[,i])
    Class = c(Class, c1)
  }
  
  nu = which(Class %in% c("numeric","integer"))
  fa = which(Class %in% c("factor","character"))
  nu.data = dstd[,nu] 
  fa.data = dstd[,fa] 
  Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
  
  rhs = paste(input$xAttr, collapse = "+")
  formula= as.formula(paste(input$yAttr,"~", rhs , sep=""))
  olsstd = lm(formula, data = dstd)
  ols2=list(Dimensions = Dimensions,Summary =Summary, ols=olsstd)
  return(ols2)

  })

output$summarystd = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    ols2()[1:2]
  }
})



output$resplot1 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
  plot(ols()$residuals, ylab="Residuals")
  }
})

output$resplot2 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
  plot(ols()$residuals,ols()$fitted.values, xlab="Predicted Y", ylab="Residuals") #
  }
})

output$resplot3 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
  plot(test_data()[,input$yAttr],predict(ols(),test_data()), xlab="Actual Y", ylab="Predicted Y")
  abline(0,1)  
  }
})


output$olssummary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  summary(ols())
  }
  })

output$olssummarystd = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  summary(ols2()[[3]])
  }
})

output$validation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
    dft = data.frame(scale(data.frame(y = test_data()[,input$yAttr], yhat = predict(ols(),test_data())  )))
    mse.y = mse(dft$y,dft$yhat)
    out = list(Number_of_Observations = nrow(test_data()),Mean_Square_Error_of_Standardized_Response = mse.y)
    out
})

output$validationtr = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  dft = data.frame(scale(data.frame(y = train_data()[,input$yAttr], yhat = predict(ols(),train_data())  )))
  mse.y = mse(dft$y,dft$yhat)
  out = list(Number_of_Observations = nrow(train_data()),Mean_Square_Error_of_Standardized_Response_Training_Data = mse.y)
  out
})

output$validationnew = renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  if ((input$yAttr %in%  colnames(Dataset.Predict()) ) ) {
  dft = data.frame(scale(data.frame(y = Dataset.Predict()[,input$yAttr], yhat = predict(ols(),Dataset.Predict())  )))
  mse.y = mse(dft$y,dft$yhat)
  out = list(Number_of_Observations = nrow(Dataset.Predict()),Mean_Square_Error_of_Standardized_Response_Training_Data = mse.y)
  out}
 #  p("Number_of_Observations -", nrow(Dataset.Predict()),": Mean_Square_Error_of_Standardized_Response_New_Data -", mse.y)}
  else {return(NULL)}
})

prediction = reactive({
  val = predict(ols(),Dataset.Predict())
  out = data.frame(Yhat = val, Dataset.Predict())
  names(out)[1] = paste0("Pred.",input$yAttr)
  return(out)
})
output$prediction =  renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  head(prediction(),10)
})

output$prediction <- renderDataTable({
  if (is.null(input$filep)) {return(NULL)}
  else {
    prediction()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))

inputprediction = reactive({
  val = predict(ols(),train_data())
  out = data.frame(Yhat = val, train_data())
  names(out)[1] = paste0("Pred.",input$yAttr)
  return(out)
})

output$inputprediction =  renderPrint({
  if (is.null(input$file)) {return(NULL)}
  head(inputprediction(),10)
})

output$inputprediction <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    inputprediction()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))



#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Datasetp(), '') || identical(Datasetp(),data.frame())) return(NULL)
    write.csv(prediction(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "regcalifhouse.csv" },
  content = function(file) {
    write.csv(read.csv("data/regcalifhouse.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { "Input Data With Prediction.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(inputprediction(), file, row.names=F, col.names=F)
  }
)

})

