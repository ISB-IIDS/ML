###########################################################
#  Decision Tree App (server)        #
###########################################################
  try(require("shiny")||install.packages("shiny"))
  try(require("pastecs")||install.packages("pastecs"))
  try(require("rpart")||install.packages("rpart"))
  try(require("rpart.plot")||install.packages("rpart.plot"))
  try(require("dplyr")||install.packages("dplyr"))
  try(require("Hmisc")||install.packages("Hmisc"))
  try(require("hydroGOF")||install.packages("hydroGOF"))
  try(require("party")||install.packages("party"))
  try(require("partykit")||install.packages("partykit"))
  try(require("caret")||install.packages("caret"))
  try(require("visNetwork")||install.packages("visNetwork"))
  try(require("sparkline")||install.packages("sparkline"))
try(require("mice")||install.packages("mice"))

library("shiny")
library("rpart")
library("rpart.plot")
library("pastecs")
library("dplyr")
library("Hmisc")
library("hydroGOF")
library("party")
library("partykit")
library("caret")
library("visNetwork")
library("sparkline")
library("mice")


shinyServer(function(input, output,session) {
  
  #------------------------------------------------#
  
  readdataf <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })
  
  output$samsel <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      selectInput("obs", "Select sub sample", c("quick run, 1,000 obs", "10,000 obs", "full dataset"), selected = "quick run, 1,000 obs")
    }
  })
  
  readdataf1 <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
    if (input$obs=="full dataset") { return(readdataf()) }
    else if(input$obs=="10,000 obs") 
    {
      if (nrow(readdataf())>10000){
        set.seed(1234)
        testsample= sample(1:nrow(readdataf()), 10000 )
        Dataset1=readdataf()[testsample,]
        return(Dataset1)}
      else {return(readdataf())}
    }
    else (input$obs=="1,000 obs")
    {
      if (nrow(readdataf())>1000){
        set.seed(1234)
        testsample= sample(1:nrow(readdataf()), 1000 )
        Dataset1=readdataf()[testsample,]
        return(Dataset1)}
      else {return(readdataf())}
    }  }
  })
  
  output$imputemiss <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      #if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
      if (1==0) {p("error")}
      else {
        selectInput("imputemiss", "Impute missing values or drop missing value rows", 
                    c("do not impute or drop rows", "impute missing values", "drop missing value rows"), 
                    selected = "do not impute or drop rows")
      }}
  })
  
  output$imout <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    if (input$imputemiss == "do not impute or drop rows") {return(NULL)}
    else if ((input$imputemiss == "impute missing values")) {
      p("Note: missing values imputed",style="color:red")
    }
    else { p("Note: missing value rows dropped",style="color:red") }
  })
  
  readdata = reactive({
    if (input$imputemiss == "do not impute or drop rows") 
    { mydataimp=readdataf1() }
    else if (input$imputemiss == "impute missing values") 
    { mydata = readdataf1()
    mice_mod = mice(mydata, printFlag=FALSE)
    mydataimp <- complete(mice_mod) }
    else # (input$imputemiss == "drop missing value rows") 
    { mydata = readdataf1()
    mydataimp = na.omit(mydata)  }
    return(mydataimp)
  })
  
  
  output$readdata <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      readdataf()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))
  
  readdata.temp = reactive({
    mydata = readdataf1()
  })
  
  nu.Dataset = reactive({
    data = readdata.temp()
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
    if (identical(readdataf(), '') || identical(readdataf(),data.frame())) return(NULL)
    if (is.null(input$file)) {return(NULL)}
    else {
    selectInput("yAttr", "Select Y variable (Is it is a categorical variable? mark it as factor variable)",
                colnames(readdataf1()), colnames(readdataf1()[,-1])[1])
               # setdiff(colnames(readdataf1()), colnames(nu.Dataset()))[1] )
    }
  })
  
  output$yout <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    p("Y predicted is",input$yAttr,style="color:red")
  })
  
  output$yout1 <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    p("Y predicted is",input$yAttr,style="color:red")
  })
  
  output$yout3 <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    p("Y predicted is",input$yAttr,style="color:red")
  })
    
  
  output$xvarselect <- renderUI({
    if (identical(readdataf(), '') || identical(readdataf(),data.frame())) return(NULL)
    if (is.null(input$file)) {return(NULL)}
    else {
    checkboxGroupInput("xAttr", "Select X variables",
                       setdiff(colnames(readdataf1()),input$yAttr), setdiff(colnames(readdataf1()[,-1]),input$yAttr))
        }
      })

   output$fyvarselect <- renderUI({
    if (identical(readdataf(), '') || identical(readdataf(),data.frame())) return(NULL)
    if (is.null(input$file)) {return(NULL)}
    else {
      checkboxGroupInput("fyAttr", "Select if Y is a factor (categorical) variable",
                         input$yAttr,
                         #setdiff(colnames(readdataf1()[,c(input$xAttr,input$yAttr)]),c(input$xAttr)),
                         setdiff(colnames(readdataf1()),c(colnames(nu.Dataset()))))
    }
  })
    
  output$fxvarselect <- renderUI({
    if (identical(readdataf(), '') || identical(readdataf(),data.frame())) return(NULL)
    if (is.null(input$file)) {return(NULL)}
    else {
    checkboxGroupInput("fxAttr", "Select factor (categorical) X variables",
                     colnames(readdata.temp()[,c(input$xAttr)]),
                     #setdiff(colnames(readdata.temp()[,c(input$xAttr,input$yAttr)]),input$yAttr),
                     setdiff(colnames(readdata.temp()[,c(input$xAttr)]),c(colnames(nu.Dataset()))) )
                    #  setdiff(colnames(Dataset.temp()),input$yAttr),setdiff(colnames(Dataset.temp()),c(input$yAttr,colnames(nu.Dataset()))) )
    }
  })
  
  
  Dataset = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
    if (length(input$yAttr) == length(input$fyAttr)) { mydata[,input$yAttr] = as.factor(mydata[,input$yAttr]) }
   # if (is.factor(readdata()[,c(input$yAttr)])) { mydata[,input$yAttr] = as.factor(mydata[,input$yAttr]) }
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = as.factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
    
  })
  
  
  # a = c('a','b','c')
  # b = ('c')
  # setdiff(a,b)
    #------------------------------------------------#
  
  out = reactive({
    data = Dataset()
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
    Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,3), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j, Head=Head, MissingDataRows=Missing,missing.data.rows.count=mscount)
    return(out)
  })
  
  output$summarydata = renderPrint({
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
  
  output$hist = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      hist(Dataset())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
    }
  })
  
  output$dens = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      datadensity(out()[[5]])#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
    }
  })
  
  output$corplot = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      #pairs(Dataset())
      pairs(out()[[5]],pch=20, col="grey")
    }
  })
  
  chheight = renderPrint({
    input$height
      }) 
  
  testsample =  reactive({
  set.seed(5898)
  sample(1:nrow(Dataset()), round(nrow(Dataset())*((input$sample)/100)))
         })

  train_data = reactive({
      Dataset()[-testsample(),]
  })
  
  test_data = reactive({
    Dataset()[testsample(),]
  })
  
  pred.readdata <- reactive({
    if (is.null(input$filep)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })
  
  output$readdatap <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      pred.readdata()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))
  
  Dataset.Predict <- reactive({
    fxc = setdiff(input$fxAttr, input$yAttr)
    
    if ((input$yAttr %in%  colnames(pred.readdata()) ) ) {
      mydata = pred.readdata()[,c(input$yAttr,input$xAttr)]}
    else {mydata = pred.readdata()[,c(input$xAttr)]}
    
    if (length(fxc) >= 1){
      for (j in 1:length(fxc)){
        mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
      }
    }
    return(mydata)
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
  
  output$inputobs = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      dim(Dataset())
    }
  })
  
  fcptable = reactive({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    x = setdiff(colnames(Dataset()), input$Attr)
    y = input$yAttr
    
    fcptable <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                      cp = 0, data=Dataset())
  })
  
  output$fcptable = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    printcp(fcptable())
  })
  
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$yAttr
  # formula1 = 
  ## mean predictions
  
  if (class(train_data()[,c(input$yAttr)]) == "factor"){
    fcptable = NULL
  # fcptable <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
  #                   cp = 0,
  #                 #  method="class",   # use "class" for classification trees
  #                   data=train_data())$cptable
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  #method="class",   # use "class" for classification trees
                data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
  
  val = predict(pr, newdata = train_data()[,c(input$xAttr)],type="response")
  val1 = predict(pr, newdata = test_data()[,c(input$xAttr)],type="response")
  val2 = predict(pr, newdata = Dataset()[,c(input$xAttr)],type="response")
  imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  
  } else {
    fcptable = NULL
  # fcptable <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
  #                   cp = 0,
  #                   #  method="anova",   # use "class" for classification trees
  #                   data=train_data())$cptable
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                #  method="anova",   # use "class" for classification trees
                  data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
   val1 = predict(pr, newdata = test_data()[,c(input$xAttr)],type="response")
   val = predict(pr, newdata = train_data()[,c(input$xAttr)],type="response")
   val2 = predict(pr, newdata = Dataset()[,c(input$xAttr)],type="response")
   imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  }
  
  out = list(model = fit.rt, validation = val, imp = imp, validation1=val1, fcptable=fcptable, validation2=val2)
    })
  
#-------------------------------------
  
  prediction = reactive({
    
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset.Predict()[,input$xAttr], type="response")

    out = data.frame(Y.predicted = val3, Dataset.Predict())
    names(out)[1] = paste0("Pred.",input$yAttr)
    return(out)    
  })
  
  predictionorg = reactive({
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val3 = predict(pr, newdata = Dataset()[,input$xAttr], type="response")
      out = data.frame(Y.predicted = val3, Dataset())
    return(out)    
  })
  
  output$prediction =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(prediction(),10)
  })
  output$predictionorg =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(predictionorg(),10)
  }) 
  
  
  
  output$prediction <- renderDataTable({
    if (is.null(input$filep)) {return(NULL)}
    else {
      prediction()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
  
  output$predictionorg <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      predictionorg()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
  
  
  
  
#---------------------------------------------------------------  
  
  output$cpselect = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      bigtree <- fit.rt()$fcptable
      min.x <- which.min(bigtree[, 4]) #column 4 is xerror
      msl=bigtree[min.x, 4] + bigtree[min.x, 5]
      #ssl=bigtree[min.x, 4] - bigtree[min.x, 5]
      kk=nrow(bigtree)
      for(i in 1:min.x) { 
        if (bigtree[i, 4] <= msl) {mcp=bigtree[i,1];i=kk}
        #if (bigtree[i, 4] > ssl) {scp=bigtree[i,1]}
        }
      return(list(largest_cp=mcp)) #column 5: xstd, column 1: cp
    }
  })
  

  output$validation1 = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    Y.predicted = fit.rt()$validation1
    if (class(test_data()[,c(input$yAttr)]) == "factor"){
      y.actual = test_data()[,input$yAttr]
    confusion_matrix = table(Y.predicted,y.actual)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
    out = list(Confusion_matrix_of_Test_Data = confusion_matrix, Model_Accuracy = accuracy)
    } else {
    dft = data.frame(scale(data.frame(y = test_data()[,input$yAttr], Y.predicted = Y.predicted)))
    mse.y = mse(dft$y,dft$Y.predicted)
    out = list(Standardized_Mean_Square_Error = mse.y)
    } 
    out
       })
  
  output$validation = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    Y.predicted = fit.rt()$validation
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      y.actual = train_data()[,input$yAttr]
      confusion_matrix = table(Y.predicted,y.actual)
      accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
      out = list(Confusion_matrix_of_Training_Data = confusion_matrix, Model_Accuracy = accuracy)
    } else {
      dft = data.frame(scale(data.frame(y = train_data()[,input$yAttr], Y.predicted = Y.predicted )))
      mse.y = mse(dft$y,dft$Y.predicted)
      out = list(Standardized_Mean_Square_Error = mse.y)
    } 
    out
  })
  
  output$validation2 = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    Y.predicted = fit.rt()$validation2
    if (class(Dataset()[,c(input$yAttr)]) == "factor"){
      y.actual = Dataset()[,input$yAttr]
      confusion_matrix = table(Y.predicted,y.actual)
      accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
      out=confusionMatrix(confusion_matrix)
      #out = list(Confusion_matrix_of_Input_Data = confusion_matrix, Model_Accuracy = accuracy)
    } else {
      dft = data.frame(scale(data.frame(y = Dataset()[,input$yAttr], Y.predicted = Y.predicted )))
      mse.y = mse(dft$y,dft$Y.predicted)
      out = list(Standardized_Mean_Square_Error = mse.y)
    } 
    out
  })
  
  output$validation3 = renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    if ((input$yAttr %in%  colnames(Dataset.Predict()) ) ) {
      fit.rt1 <- fit.rt()$model
      pr <- as.party(fit.rt1)  
      Y.predicted = predict(pr, newdata = Dataset.Predict()[,input$xAttr], type="response")
    #if (class(Dataset.Predict()[,c(input$yAttr)]) == "factor"){
      if (class(Dataset()[,c(input$yAttr)]) == "factor"){
      y.actual = Dataset.Predict()[,input$yAttr]
      confusion_matrix = table(Y.predicted,y.actual)
      accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))
      out=caret::confusionMatrix(confusion_matrix)
      #out = list(Confusion_matrix_of_Input_Data = confusion_matrix, Model_Accuracy = accuracy)
    } else {
      dft = data.frame(scale(data.frame(y = Dataset.Predict()[,input$yAttr], Y.predicted = (Y.predicted))))
      mse.y = mse(dft$y,dft$Y.predicted)
      out = list(Standardized_Mean_Square_Error = mse.y)
    } }
    out
  })
  

  #------------------------------------------------#
  output$results = renderPrint({
    if (is.null(input$file)) {return(NULL)}
     printcp(fit.rt()$model) # display the results
    # formula.mod()
  })
  
  
  #------------------------------------------------#
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    summary(fit.rt()$model) # detailed summary of splits  
  })
  
  
  #------------------------------------------------#
  output$imp = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    fit.rt()$imp
  })
  
  #------------------------------------------------#
  output$plot1 = renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    
    plotcp(fit.rt()$model) # visualize cross-validation results   
  })
  
  
  #------------------------------------------------#
  output$plot2 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Decision Nodes for", input$yAttr)
    
    fit.rt1 = fit.rt()$model
    fit.rt1$frame$yval = as.numeric(rownames(fit.rt()$model$frame))
    
    # create attractive postcript plot of tree 
    post(fit.rt1, 
         #file = "tree2.ps", 
         filename = "",   # will print to console
         use.n = FALSE,
         compress = TRUE,
         title = title1
        ) 
    
  })
  
  output$plot3 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("decision tree for", input$yAttr, "(set higher complexity parameter to reduce tree length)")
   # title1 = paste("set higher complexity parameter to reduce tree depth")
    
  post((fit.rt()$model), 
       # file = "tree2.ps", 
       filename = "",   # will print to console
       use.n = TRUE,
       compress = TRUE,
       title = title1
      ) 
  })
  
  output$mod_sum <- renderPrint({
    if (is.null(input$file)) {return(NULL)}
    as.party(fit.rt()$model)
  })
  
  output$plot33 = renderVisNetwork({
    if (is.null(input$file)) {return(NULL)}
    visTree(fit.rt()$model, main = paste("decision tree for", input$yAttr), width = "100%")
  })
  
  
  #------------------------------------------------#
  nodes1 =  reactive({
    
  tree_nodes = as.data.frame(fit.rt()$model$where)
  colnames(tree_nodes) <- "node_number"
  # tree_nodes %>% head()
    
  a0 = as.numeric(rownames(fit.rt()$model$frame)); a0
  a1 = seq(1:nrow(fit.rt()$model$frame)); a1 
  a2 = as.vector(fit.rt()$model$where)
  node_num = a2
  for (i1 in 1:nrow(tree_nodes)){
    node_num[i1] = a0[a2[i1]]
  }
  
  tree_nodes1 <- fit.rt()$model$where %>% as.data.frame() %>% 
  cbind(node_num) %>% dplyr::select("node_num")
  tree_nodes1
  
  })

  output$nodesout1 = renderPrint({
    head(nodes1(),15)
  })
  
  output$nodesout <- renderDataTable({  	
    data.frame(nodes1(), train_data())
  }, options = list(lengthMenu = c(10, 20, 50), pageLength = 10))  # my edits here
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(row_numer = row.names(nodes1()), nodes1(), train_data());   # data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(row_numer = row.names(nodes1()), nodes1(), train_data());   # data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )  


  #------------------------------------------------#
  output$downloadData0 <- downloadHandler(
    filename = function() { "Input Data with Predictions.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(predictionorg(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Data.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(prediction(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() { "mTitanicAll.csv" },
    content = function(file) {
      write.csv(read.csv("data/mTitanicAll.csv"), file, row.names=F, col.names=F)
    }
  )

  
  })