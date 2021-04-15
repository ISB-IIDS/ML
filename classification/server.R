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
if (!require("mice")) {install.packages("mice")}
if (!require("ggfortify")) {install.packages("ggfortify")}

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
library(mice)
library(ggfortify)

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
  else 
    {
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
  } } 
})

output$imputemiss <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    #if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
    if (1==0) {p("error")}
    else {
      selectInput("imputemiss", "Impute missing values or drop missing value rows", 
                  c("do not impute or drop rows", "impute missing values", "drop missing value rows"), selected = "drop missing value rows")
    }}
})

output$imout <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  if (input$imputemiss == "do not impute or drop rows") {return(NULL)}
  else if ((input$imputemiss == "impute missing values")) {
    p("Note: missing values imputed, check options in the panel on the left",style="color:red")
  }
  else { p("Note: missing value rows dropped, check options in the panel on the left",style="color:red") }
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

Dataset.temp = reactive({
  mydata = Datasetf1()
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
                     colnames(Datasetf1()), setdiff(colnames(Datasetf1()),colnames(nu1.Dataset()))[1])
  }
})

output$yout <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Y is",input$yAttr,style="color:red")
})

output$yout1 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Prediction Class: ",input$yAttr,style="color:red")
})

output$yout3 <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  p("Predicted Class: ",input$yAttr,style="color:red")
})


   output$warning <- renderUI({
     if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
     if (class(Datasetf1()[,input$yAttr])=="numeric") {
     (p('Y must be factor (categorical) variable',style="color:red"))
     }
     else return(NULL)
   })
     
output$xvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("xAttr", "Select X variables",
                     setdiff(colnames(Datasetf1()),input$yAttr), setdiff(colnames(Datasetf1()[,-1]),input$yAttr))
  }
})


output$fxvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables in X",
                     colnames(Dataset.temp()[,c(input$xAttr)]),
                     setdiff(colnames(Dataset.temp()[,c(input$xAttr)]),c(colnames(nu1.Dataset()))) )
  }
})

output$fyvarselect <- renderUI({
  if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
    checkboxGroupInput("fyAttr", "Select Y as factor (categorical) variable",
                       input$yAttr,
                       setdiff(colnames(Dataset.temp()),c(colnames(nu1.Dataset()))) )
  }
})

mydatay = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]
    if (length(input$fyAttr) >= 1){
      mydata[,input$yAttr] = factor(mydata[,input$yAttr])
  }
  return(mydata)
})

mydata = reactive({
  mydata = mydatay()[,c(input$yAttr,input$xAttr)]
  #mydata[,input$yAttr] = factor(mydata[,input$yAttr])
  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
})

pred.readdata = reactive({
  fvar=setdiff(input$fxAttr,input$yAttr)
  
  if ((input$yAttr %in%  colnames(Datasetp()) ) ) {
    mydata = Datasetp()[,c(input$yAttr,input$xAttr)]}
  else {mydata = Datasetp()[,c(input$xAttr)]}
  
  if (length(fvar) >= 1){
    for (j in 1:length(fvar)){
      mydata[,fvar[j]] = factor(mydata[,fvar[j]])
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
    #pairs(mydata())
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
    data = mydata()#/meanstd()[[2]]-meanstd()[[1]]
    rhs = paste(input$xAttr, collapse = "+")
    formula= as.formula(paste(input$yAttr,"~", rhs , sep=""))
    if (input$select=="Linear Discriminant Classification") { ols = lda(formula, data = data) }
    else if (input$select=="Quadratic Discriminant Classification") {ols = qda(formula, data = data)}
    else if (input$select=="Naive Bayes Classification") {ols = naiveBayes(formula, data = data)}
    else if (input$select=="Support Vector Machine") {ols = svm(formula, data = data)}
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
    predictions <- predict(ols(),newdata  = data)
    if (input$select=="Naive Bayes Classification") {Predicted.Class=predictions}
    else if (input$select=="Support Vector Machine") {Predicted.Class=predictions}
    else {Predicted.Class = predictions$class}
    Actual.Class=mydata()[,input$yAttr]
    confusion_matrix = table(Predicted.Class,Actual.Class)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out=confusionMatrix(confusion_matrix)
    #out = list(Confusion_matrix = confusion_matrix, Accuracy_of_Validation = accuracy)
    return(out)
    }
})

output$confusion1 = renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  if ((input$yAttr %in%  colnames(pred.readdata()) ) ) {
    data = pred.readdata()#/meanstd()[[2]]-meanstd()[[1]]  
    predictions <- predict(ols(),newdata  = data)
    if (input$select=="Naive Bayes Classification") {Predicted.Class=predictions}
    else if (input$select=="Support Vector Machine") {Predicted.Class=predictions}
    else {Predicted.Class = predictions$class}
    Actual.Class=pred.readdata()[,input$yAttr]
    confusion_matrix = table(Predicted.Class,Actual.Class)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out=confusionMatrix(confusion_matrix)
    #out = list(Confusion_matrix = confusion_matrix, Accuracy_of_Validation = accuracy)
    return(out)
  }
})

datatable = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    data = mydata()#/meanstd()[[2]]-meanstd()[[1]]
    predictions <- predict(ols(),newdata = data ) 
    if (input$select=="Naive Bayes Classification") {
      Class=predictions
      #Prob <- predict(ols(),newdata  = data, type="raw")
      dt=data.frame(Pred.Class=Class,data)}
    else if (input$select=="Support Vector Machine") {
      Class=predictions
      #Prob <- predict(ols(),newdata  = data, type="raw")
      dt=data.frame(Pred.Class=Class,data)}
    else {Class = predictions$class
    Prob = predictions$posterior
    dt=data.frame(Pred.Class=Class,Prob=Prob,data)}
    return(dt)
  }
})

datatablep = reactive({
  if (is.null(input$filep)) {return(NULL)}
  else {
    data = pred.readdata()#/meanstd()[[2]]-meanstd()[[1]]
    predictions <- predict(ols(),newdata = data) 
    if (input$select=="Naive Bayes Classification") {
      Class=predictions
      #Prob <- predict(ols(),newdata  = data, type="raw")
      dt=data.frame(Pred.Class=Class,data)}
    else if (input$select=="Support Vector Machine") {
      Class=predictions
      #Prob <- predict(ols(),newdata  = data, type="raw")
      dt=data.frame(Pred.Class=Class,data)}
    else {
      Class = predictions$class
      Prob = predictions$posterior
      dt=data.frame(Pred.Class=Class,Prob=Prob,data)}
    return(dt)
  }
})

output$pcaplot = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  colnames
  #data=out()[[5]]
  pca_res = prcomp(out()[[5]], scale. = TRUE)
  autoplot(pca_res, data = mydata(), colour = input$yAttr )
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


output$datatablep <- renderDataTable({
  if (is.null(input$filep)) {return(NULL)}
  else {
    datatablep()
  }
}, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))

output$downloadData <- downloadHandler(
  filename = function() { "iris.csv" },
  content = function(file) {
    write.csv(read.csv("data/iris.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadDatap <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Datasetp(), '') || identical(Datasetp(),data.frame())) return(NULL)
    write.csv(datatablep(), file, row.names=F, col.names=F)
  }
)

output$downloadData1 <- downloadHandler(
  filename = function() { "Input Data With Prediction.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(datatable(), file, row.names=F, col.names=F)
  }
)

})

