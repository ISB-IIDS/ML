#################################################
#      Classification Analysis App                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")} #for stat.desc
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("hydroGOF")) {install.packages("hydroGOF")}
if (!require("MASS")) {install.packages("MASS")}
if (!require("caret")) {install.packages("caret")}
#if (!require("Rtsne")) {install.packages("Rtsne")}
if (!require("e1071")) {install.packages("e1071")}

library(shiny)
library(pastecs)
library(RColorBrewer)
library(ggplot2)
library(Hmisc)
library(reshape2)
library(corrplot)
library(hydroGOF)
library(MASS)
library(e1071)
library(caret)

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

Dataset <- reactive({
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
  } } 
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

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  selectInput("yAttr", "Select Y variable (must be factor/categorical)",
                     colnames(Dataset()), setdiff(colnames(Dataset()),colnames(nu1.Dataset())))
  }
})


output$xvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Dataset()),input$yAttr), setdiff(colnames(Dataset()),input$yAttr))
  }
})


output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables",
                     colnames(Dataset.temp()[,c(input$yAttr,input$xAttr)]),
                     setdiff(colnames(Dataset.temp()[,c(input$yAttr,input$xAttr)]),c(colnames(nu1.Dataset()))) )
  }
})

mydata = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]
  #mydata[,input$yAttr] = factor(mydata[,input$yAttr])
  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})

pred.readdata = reactive({
  mydata = Datasetp()
  if (length(input$fxAttr) >= 1){
    for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
    }
  }
  return(mydata)
})

out = reactive({
data = mydata()
Missing1=(data[!complete.cases(data),])
Missing=(Missing1)
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

output$missing1 = renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[[9]]
  }
}, options = list(lengthMenu = c(10, 30, 50,100), pageLength = 10))

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


output$meanstd1 = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    dtsd=out()[2]$Summary$Numeric.data
    Mean=dtsd["mean",]
    rownames(Mean)=NULL
    Stdev=dtsd["std.dev",]
    rownames(Stdev)=NULL
    meanstd = list(Mean=Mean,Stdev=Stdev) 
    return(meanstd)
  }
})

meanstd = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    dtsd=out()[2]$Summary$Numeric.data
    Mean=dtsd["mean",]
    rownames(Mean)=NULL
    Stdev=dtsd["std.dev",]
    rownames(Stdev)=NULL
    meanstd = list(Mean=Mean,Stdev=Stdev) 
    return(meanstd)
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


output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  round(cor(out()[[5]], use = "pairwise.complete.obs"),4)
  }
  })

output$corplot = renderPlot({
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
    data = mydata()#/meanstd()[[2]]-meanstd()[[1]]
    rhs = paste(input$xAttr, collapse = "+")
    formula= as.formula(paste(input$yAttr,"~", rhs , sep=""))
    if (input$select=="Linear Discriminant Analysis") { ols = lda(formula, data = data) }
    else if (input$select=="Quadratic Discriminant Analysis") {ols = qda(formula, data = data)}
    else if (input$select=="Naive Bayes Classifier") {ols = naiveBayes(formula, data = data)}
    else {ols = rda(formula, data = data)}
  return(ols)
})

output$olssummary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    (ols())
  }
})

output$confusion = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    data = mydata()#/meanstd()[[2]]-meanstd()[[1]]  
    predictions <- predict(ols(),data)
    if (input$select=="Naive Bayes Classifier") {Predicted.Class=predictions}
    else {Predicted.Class = predictions$class}
    Actual.Class=mydata()[,input$yAttr]
    confusion_matrix = table(Predicted.Class,Actual.Class)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out=confusionMatrix(confusion_matrix)
    #out = list(Confusion_matrix = confusion_matrix, Accuracy_of_Validation = accuracy)
    return(out)
    }
})

predmydata =  reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  cbind(mydata(), predict(ols())$x)
  }
})

output$resplot1 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (input$select=="Linear") { 
    #lda.data <- cbind(mydata(), predict(ols())$x)
    lda.data = predmydata() 
    col=paste("color","=",input$yAttr)
    if (length(lda.data$LD2) >0){
    ggplot(lda.data, aes(LD1, LD2)) +
      geom_point(aes(colour = mydata()[,input$yAttr]), size = 3)}
    }
  }
})

output$resplot2 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (input$select=="Linear") { 
    #lda.data <- cbind(mydata(), predict(ols())$x)
      lda.data = predmydata()
    col=paste("color","=",input$yAttr)
    if (length(lda.data$LD3) >0){
    ggplot(lda.data, aes(LD1, LD3)) +
      geom_point(aes(colour = mydata()[,input$yAttr]), size = 3)}
    }
  }
})

output$resplot3 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (input$select=="Linear") { 
   #lda.data <- cbind(mydata(), predict(ols())$x)
      lda.data = predmydata()
   col=paste("color","=",input$yAttr)
   if (length(lda.data$LD3) >0){
   ggplot(lda.data, aes(LD2, LD3)) +
     geom_point(aes(colour = mydata()[,input$yAttr]), size = 3)} 
    }
  }
})

output$datatable <- renderDataTable({
  if (is.null(input$file)) {return(NULL)}
  else {
    datatable()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))

datatable = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
  data = mydata()#/meanstd()[[2]]-meanstd()[[1]]
  predictions <- predict(ols(),data) 
  if (input$select=="Naive Bayes Classifier") {Class=predictions
  dt=data.frame(Pred.Class=Class,mydata())}
  else {Class = predictions$class
  Prob = predictions$posterior
  dt=data.frame(Pred.Class=Class,Prob.Class=Prob,mydata())}
  return(dt)
  }
})


output$datatablep <- renderDataTable({
  if (is.null(input$filep)) {return(NULL)}
  else {
    datatablep()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))

datatablep = reactive({
  if (is.null(input$filep)) {return(NULL)}
  else {
  data = pred.readdata()#/meanstd()[[2]]-meanstd()[[1]]
  predictions <- predict(ols(),data) 
  if (input$select=="Naive Bayes Classifier") {Class=predictions
  dt=data.frame(Pred.Class=Class,mydata())}
  else {Class = predictions$class
  Prob = predictions$posterior
  dt=data.frame(Pred.Class=Class,Prob.Class=Prob,mydata())}
  return(dt)
}
})

output$downloadData <- downloadHandler(
  filename = function() { "wholesalecustomerdata.csv" },
  content = function(file) {
    write.csv(read.csv("data/wholesalecustomerdata.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData1 <- downloadHandler(
  filename = function() { "New Data With Prediction.csv" },
  content = function(file) {
    if (identical(Datasetp(), '') || identical(Datasetp(),data.frame())) return(NULL)
    write.csv(prediction(), file, row.names=F, col.names=F)
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

