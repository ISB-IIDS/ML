#################################################
#                 ISB MLogit                    #
#################################################

#Required Packages
if(!require("shiny")) {install.packages("shiny")}
if(!require("pastecs")){install.packages("pastecs")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("Hmisc")){install.packages("Hmisc")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("reshape2")){install.packages("reshape2")}
if(!require("e1071")){install.packages("e1071")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("caret")) {install.packages("caret")}
if (!require("Rfast")) {install.packages("Rfast")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("multiROC")) {install.packages("multiROC")}

library(shiny)
library(pastecs)
library(Hmisc)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(mlogit)
library(reshape2)
library(dplyr)
library(caret)
library(Rfast)
library(nnet)
library(dummies)
library(multiROC)
library(e1071)
# library(gplot)


  shinyServer(function(input, output,session) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(Dataset)
    }
  })
  
  output$readdata <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      Dataset()
    }
  }, options = list(lengthMenu = c(10, 50, 100), pageLength = 10))
  
  pred.Dataset <- reactive({
    if (is.null(input$filep)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })
  
  # Select variables:
  output$Individualvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) { return(NULL) }
    else{
    selectInput("IndividualAttr", "Select individual/observation ID variable",
                colnames(Dataset()), colnames(Dataset())[1])
    }
    })
  
  #select alternatives column:
  output$Alternativesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) { return(NULL) }
    else{
    selectInput("AlternativesAttr", "Select available/potential alternatives variable",
                       setdiff(colnames(Dataset()),c(input$IndividualAttr)), 
                      setdiff(colnames(Dataset()),c(input$IndividualAttr))[1])
    }
  })
  
  output$Choicevarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) { return(NULL) }
    else{
      selectInput("ChoiceAttr", "Select choice/outcome (0/1) variable",
                  # colnames(Dataset()), colnames(Dataset())[1])
                  setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr)), 
                  setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr))[1])
    }
  })
  
  basealt <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
    basealt = (as.data.frame(t(as.matrix(table(Dataset()[,input$AlternativesAttr])))))
    }
    return(basealt)
  })
  
  #select base alternative
  output$BaseAlternativeselect <- renderUI({
    if (is.null(input$file)) { return(NULL) }
    else{
    #if (identical(base(), '') || identical(base(),data.frame())) return(NULL)
    #y=t(as.matrix(table(Dataset()$AlternativesAttr))) 
    selectInput("BaseAlternative", "Select base (from avaialble alternatives)",
                colnames(basealt()), colnames(basealt())[1] )
    }
  })

  output$Alternativefeaturesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) { return(NULL) }
    else{
      checkboxGroupInput("AlternativefeaturesAttr", "Select alternative-specific X variables",
                         setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr,input$ChoiceAttr)),
                         ""   )
    }
  })
  
  output$Individualfeaturesvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    if (is.null(input$file)) { return(NULL) }
    else{
      checkboxGroupInput("IndividualfeaturesAttr", "Select individual specific X variables",
                         setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr,input$ChoiceAttr,input$AlternativefeaturesAttr)),
                         ""
                        # setdiff(colnames(Dataset()),c(input$IndividualAttr,input$AlternativesAttr,input$ChoiceAttr)) 
                          )
    }
  })
  

  mydata = reactive({
    mydata = Dataset()[,c(input$IndividualAttr,input$AlternativesAttr,input$ChoiceAttr,input$IndividualfeaturesAttr,input$AlternativefeaturesAttr)]
    return(mydata)
  })
  
  pred.mydata = reactive({
    mydata = pred.Dataset()[,c(input$IndividualAttr,input$AlternativesAttr,input$IndividualfeaturesAttr,input$AlternativefeaturesAttr)]
    return(mydata)
  })
  
  out = reactive({
    data = mydata()
    #data = dataforsummary()
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
  
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
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

  
rhs=reactive({
      if (length(input$IndividualfeaturesAttr)>=1){
ind.features=paste(input$IndividualfeaturesAttr,collapse = "+")
          if (length(input$AlternativefeaturesAttr)>=1){
         alt.features=(paste(input$AlternativefeaturesAttr,collapse = "+"))
          } else {alt.features=0}       
         rhs=paste(alt.features,"|",ind.features,sep = "")
      }else{
          #rhs=paste(input$AlternativefeaturesAttr,collapse = "+")
        if (length(input$AlternativefeaturesAttr)>=1){
          rhs=paste(input$AlternativefeaturesAttr,collapse = "+")
        } else {rhs=paste("0|1",sep="")}
          }
      
      return(rhs)
      })
  
  
  ols = reactive({
    #rhs=paste(input$xAttr,collapse = "+")
   # reg.form= paste(input$yAttr,"~",rhs,collapse = "")
   #  mlogit.reg=mlogit(reg.form,H)
    #mlogit.reg = mlogit(paste(input$yAttr,"~", rhs() , sep=""), data = mydata2())
    #mlogit.reg = mlogit(formula=as.formula(paste(input$yAttr,"~", rhs() ,sep="")),data=mydata2())
    formula=as.formula(paste(input$ChoiceAttr,"~",rhs(),sep = ""))
    #DCE_data<- mlogit.data(data=Dataset(), choice = input$ChoiceAttr, shape = "long", alt.var = input$AlternativesAttr,id.var = input$IndividualAttr) 
    a <- mlogit(formula=formula,data=mydata(),alt.var = input$AlternativesAttr,id.var = input$IndividualAttr,
                choice = input$ChoiceAttr) # reflevel=input$BaseAlternative # output needs to be reorganized
    return(a)
  })
  
  output$basealtprint = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      paste("Base Alternative - ", colnames(basealt())[1])
    }
  })
  
  ols.pred = reactive({
    pred.mydata.idx= mlogit.data(pred.mydata()) #, alt.levels = input$AlternativesAttr, id.var = input$IndividualAttr)
    b <- predict(ols(), newdata=pred.mydata.idx)
    return(b)
  })

  output$ols.pred = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      (ols.pred())
    }})
    
  
  output$olssummary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
  summary(ols())
    }
   #print(ols())
  })
  output$confusionmatrix = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
    data.fit=(fitted(object = ols(), outcome=FALSE))
    trial=Rfast::rowMaxs(data.fit,value = FALSE)
    data.fit=as.data.frame(data.fit)
    data.fit$predict=as.factor(colnames(data.fit)[trial])
    
    choice.col=(as.vector(mydata()[,input$ChoiceAttr]))
    data.fit$actual=as.factor(mydata()[which(choice.col==1),input$AlternativesAttr])
    data.try=(data.fit)
    
    caret::confusionMatrix(as.factor(data.try$predict),as.factor(data.try$actual))
    }
  })
  
  output$probablities = renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
     data.fit=(fitted(object = ols(), outcome=FALSE))
     #trial=Rfast::rowMaxs(data.fit,value = FALSE)
     #data.fit$predicted=colnames(data.fit)[trial]
   
  #  choice.col=(as.vector(mydata()[,input$ChoiceAttr]))
  #  data.fit$actual=as.vector(mydata()[which(choice.col==1),input$AlternativesAttr])
  # data.fit$obs_ID=as.vector(mydata()[which(choice.col==1),input$IndividualAttr])
  # #data.try=as.data.frame(data.fit)

     data.fit1=as.data.frame(as.vector(t(data.fit)))
     colnames(data.fit1)="predicted.prob"
     data.try=as.data.frame( cbind(data.fit1, mydata() ))
     data.try
    }
  }, options = list(lengthMenu = c(10, 50, 100), pageLength = 10))
  
  output$ROC = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {  
    data.fit=(fitted(object = ols(), outcome=FALSE))
    trial=Rfast::rowMaxs(data.fit,value = FALSE)
    data.fit=as.data.frame(data.fit)
    #data.fit$predict.choice=colnames(data.fit)[trial]
    
    choice.col=(as.vector(mydata()[,input$ChoiceAttr]))
    #label_true=as.vector(Dataset()[which(choice.col==1),input$AlternativesAttr])
    
    
    colnames(data.fit) <- paste(colnames(data.fit), "_pred_MN")
    #data.try=as.data.frame(data.fit)
    
    #test_df=data[data$choice==1,]
    test_df=mydata()[which(choice.col==1),input$AlternativesAttr]
    true_label <- dummies::dummy(test_df, sep = ".")
    
    colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
     colnames(true_label) <- paste(colnames(true_label), "_true")
     final_df <- cbind(true_label, data.fit)
    roc_res <- multi_roc(final_df, force_diag=T)
    pr_res <- multi_pr(final_df, force_diag=T)

    plot_roc_df <- plot_roc_data(roc_res)
    plot_pr_df <- plot_pr_data(pr_res)


    ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
      geom_path(aes(color = Group, linetype=Method), size=1.5) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                   colour='grey', linetype = 'dotdash') +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.justification=c(1, 0), legend.position=c(.95, .05),
            legend.title=element_blank(),
            legend.background = element_rect(fill=NULL, size=0.5,
                                           linetype="solid", colour ="black"))
  }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "Travel.csv" },
    content = function(file) {
      write.csv(read.csv("data/Travel.csv"), file, row.names=F, col.names=F)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Probabilities.csv" },
    content = function(file) {
      data.fit=(fitted(object = ols(), outcome=FALSE))
      # trial=Rfast::rowMaxs(data.fit,value = FALSE)
      # data.fit=as.data.frame(data.fit)
      # data.fit$predict=colnames(data.fit)[trial]
      # choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
      # data.fit$actual=as.vector(Dataset()[which(choice.col==1),c(input$AlternativesAttr)])
      # data.fit$obs_ID=as.vector(Dataset()[which(choice.col==1),c(input$IndividualAttr)])

      data.fit1=as.data.frame(as.vector(t(data.fit)))
      colnames(data.fit1)="predicted.prob"
      data.try=as.data.frame( cbind(data.fit1, mydata() ))
      write.csv(data.try, file, row.names=F, col.names=F)
    }
  )  
  
  output$probablitiespred = renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      data.fit=(fitted(object = ols.pred(), outcome=FALSE))
      # trial=Rfast::rowMaxs(data.fit,value = FALSE)
      # data.fit=as.data.frame(data.fit)
      # data.fit$predicted=colnames(data.fit)[trial]
      # 
      # choice.col=(as.vector(pred.Dataset()[,input$ChoiceAttr]))
      # data.fit$actual=as.vector(pred.Dataset()[which(choice.col==1),input$AlternativesAttr])
      # data.fit$obs_ID=as.vector(pred.Dataset()[which(choice.col==1),input$IndividualAttr])
      # #data.try=as.data.frame(data.fit)
      
      data.fit1=as.data.frame(as.vector(t(data.fit)))
      colnames(data.fit1)="predicted.prob"
      data.try=as.data.frame( cbind(data.fit1, pred.mydata() ))
      data.fit
    }
  }, options = list(lengthMenu = c(10, 50, 100), pageLength = 10))
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "Prediction New Data.csv" },
    content = function(file) {
      if (is.null(input$filep)) {return(NULL)}
      else { 
        data.fit=(fitted(object = ols.pred(), outcome=FALSE))
        # trial=Rfast::rowMaxs(data.fit,value = FALSE)
        # data.fit=as.data.frame(data.fit)
        # data.fit$predict=colnames(data.fit)[trial]
        # choice.col=(as.vector(Dataset()[,input$ChoiceAttr]))
        # data.fit$actual=as.vector(Dataset()[which(choice.col==1),c(input$AlternativesAttr)])
        # data.fit$obs_ID=as.vector(Dataset()[which(choice.col==1),c(input$IndividualAttr)])
        # data.try=as.data.frame(c(data.fit))
        
        data.fit1=as.data.frame(as.vector(t(data.fit)))
        colnames(data.fit1)="predicted.prob"
        data.try=as.data.frame( cbind(data.fit1, pred.mydata() ))
        write.csv(data.try, file, row.names=F, col.names=F)
      }
                              }
  ) 
  
  # output$resplot2 = renderPlot({
  #    plot(ols()$residuals,ols()$fitted.values)
  #  })

  # output$resplot3 = renderPlot({
  #   plot(mydata()[,input$yAttr],ols()$fitted.values)#
  # })
  #
  })
