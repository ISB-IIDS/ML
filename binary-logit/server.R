#################################################
#      Summary & Binary App                      #
#################################################
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("ROCR")) {install.packages("ROCR")}
if (!require("caret")) {install.packages("caret")}
if (!require("Rfast")) {install.packages("Rfast")}
if (!require("e1071")) {install.packages("e1071")}
if (!require("Rtsne")) {install.packages("Rtsne")}
if (!require("fastDummies")) {install.packages("fastDummies")}

library(shiny)
library(e1071)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(corrplot)
library(ROCR)
library(caret)
library(Rfast)
library(Rtsne)
library(fastDummies)

# library(gplot)

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})


pred.readdata <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  selectInput("yAttr", "Select Y variable (must be binary)",
                     colnames(Dataset()), colnames(Dataset())[1])
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

Dataset.temp = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]
})

nu.Dataset = reactive({
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
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  if (is.null(input$file)) {return(NULL)}
  else {
  checkboxGroupInput("fxAttr", "Select factor (categorical) variables in X",
                #     setdiff(colnames(Dataset.temp()),input$yAttr),"" )
                setdiff(colnames(Dataset.temp()),input$yAttr),    setdiff(colnames(Dataset.temp()),c(input$yAttr,colnames(nu.Dataset()))) )
  }
})

mydata = reactive({
  ydata = Dataset()[,c(input$yAttr)]
  Y=(dummy_cols(ydata))[,3]
  mydata=cbind(Y, Dataset()[,c(input$yAttr,input$xAttr)])
  mydata[,input$yAttr] = factor(mydata[,input$yAttr])
  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})


Dataset.Predict <- reactive({
  fxc = setdiff(input$fxAttr, input$yAttr)
  mydata = pred.readdata()[,c(input$xAttr)]
  
  if (length(fxc) >= 1){
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
nu.data = data[,nu]; nu.data = nu.data[,-1]
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions, Summary =Summary, Tail=Tail, fa.data, nu.data, a, j, Head=Head,MissingDataRows=Missing,missing.data.rows.count=mscount)
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

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
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


ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    formula= as.formula(paste("Y","~", rhs , sep=""))
    ols = glm(formula, data = mydata(), family=binomial)
  return(ols)
})

output$olssummary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  summary(ols())
  }
  })

output$mscount = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[10]
  }
})

output$correlation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    cor(out()[[5]], use = "pairwise.complete.obs")
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

output$datatable = renderTable({
  if (is.null(input$file)) {return(NULL)}
  else {
  Y.Prob = ols()$fitted.values
  data.frame(Y.Prob,mydata())
  }
}, options = list(lengthMenu = c(10, 20, 50), pageLength = 10)  )

output$validation = renderPrint({
  if (is.null(input$file)) {return(NULL)}
    y_actual = mydata()[,"Y"]
    y_predicted = as.integer(ols()$fitted.values>0.5)
    confusion_matrix = table(y_predicted,y_actual)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out = list(Confusion_matrix = confusion_matrix, Accuracy_Hit_Rate = accuracy)
    out
})

output$confusionmatrix = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
  data.fit = as.integer(ols()$fitted.values>input$cutoff)
  data.act = (mydata()[,"Y"])
  Confusion_Matrix = confusionMatrix(as.factor(data.fit),as.factor(data.act))
  #out=list(Sum_Specificity_Senstivity=Confusion_Matrix['Senstivity']+Confusion_Matrix['Specificity'], Confusion_Matrix=Confusion_Matrix)
  Confusion_Matrix  
  }
})

output$roc = renderPlot({ 
  if (is.null(input$file)) {return(NULL)}
  else {
  pred.val = predict(ols(),mydata(),type="response")
  pred.lm = ROCR::prediction(pred.val, mydata()[,"Y"])
  perf.lm = performance(pred.lm,"tpr", "fpr")
  #roc_graph<-cbind(perf.lm@x.values[[1]],perf.lm@y.values[[1]],perf.lm@alpha.values[[1]]);
  #write.csv(roc_graph, file="roc_graph1.csv")
  auc_ROCR = performance(pred.lm, measure = "auc")
  plot(perf.lm, main = c("AUC", auc_ROCR@y.values[[1]]), 
       xlab="False Positive Rate (1-Specificity)", ylab="True Positive Rate (Sensitivity)" )
  #lines(x = c(0,1), y = c(0,1))
  abline(a=0,b=1)
 #abline(a=1,b=-1)
  }
})


prediction = reactive({
  val = predict(ols(),Dataset.Predict(), type='response')
  out = data.frame(Y.Prob = val, Dataset.Predict())
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
  val = predict(ols(),mydata(), type='response')
  out = data.frame(Y.Prob = val, mydata())
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

output$resplot3 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    plot(ols()$fitted.values,mydata()[,"Y"],main="Predicted vs. Actual Y", 
         xlab="Predicted Y", ylab="Actual Y", col=round(ols()$fitted.values>input$cutoff,0)+1  ) 
    #abline(0,1)  
  }
})

dup = reactive({
  dup = which(duplicated(out()[[5]]))
  return(dup)
})

output$dup =  renderPrint({
  length(dup())
})

tsne_df = reactive({
  if (is.null(input$file)) {return(NULL)}
  else {
    if (length(dup())==0) {
      data = out()[[5]]
      tsne_object = Rtsne(as.matrix(data), perplexity = input$perp, num_threads=0, max_iter=input$iter)
    }
    else{
    dup = dup()
    data = out()[[5]]
    tsne_object = Rtsne(as.matrix(data[-dup,]), perplexity = input$perp, num_threads=0, max_iter=input$iter)
    }
    tsne_df1 = as.data.frame(tsne_object$Y) 
    tsne_df = setNames(tsne_df1,c("Dim.1", "Dim.2"))
    return(tsne_df)
  }
  
})

output$resplot4 = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    dup = dup()
    tsne_df=tsne_df()
    if (length(dup())==0) {
    Y_var=mydata()[,input$yAttr]  
    ggplot(aes(x = Dim.1, y = Dim.2), data = tsne_df) +
      geom_point(aes(colour = Y_var), size = 3)
    }
    else{
      Y_var=mydata()[-dup,input$yAttr]
      ggplot(aes(x = Dim.1, y = Dim.2), data = tsne_df) +
        geom_point(aes(colour = Y_var), size = 3)
    }
  }
})

#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(predicted(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "mTitanicAll.csv" },
  content = function(file) {
    write.csv(read.csv("data/mTitanicAll.csv"), file, row.names=F, col.names=F)
  }
)
output$downloadData2 <- downloadHandler(
  filename = function() { "Input Data With Prediction.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(inputpredicted(), file, row.names=F, col.names=F)
  }
)

})

