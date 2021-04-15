####################################################
#      Cluster Analysis                         #
####################################################
if (!require("shiny")) {install.packages("shiny")}
if (!require("ggplot2")) {install.packages("ggplot2")}
if (!require("plyr")) {install.packages("plyr")}
if (!require("grid")) {install.packages("grid")}
if (!require("scales")) {install.packages("scales")}
if (!require("cluster")){install.packages("cluster")}
if (!require("mclust")){install.packages("mclust")}
if (!require("MASS")){install.packages("MASS")}
if (!require("Hmisc")){install.packages("Hmisc")} # for describe
if (!require("pastecs")){install.packages("pastecs")} #for stat.desc
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("nFactors")) {install.packages("nFactors")}
if (!require("factoextra")) {install.packages("factoextra")}
if (!require("dplyr")) {install.packages("dplyr")}
if(!require("tidyr")) {install.packages("tidyr")}
if (!require("Spectrum")) {install.packages("Spectrum")}
if (!require("Rtsne")) {install.packages("Rtsne")}
if (!require("dbscan")) {install.packages("dbscan")}
if (!require("plot3D")) {install.packages("plot3D")}
#if (!require("kohonen")) {install.packages("kohonen")}
if (!require("mice")) {install.packages("mice")}
if (!require("DescTools")) {install.packages("DescTools")}
#devtools::install_github("ropenscilabs/umapr")
if (!require("umap")) {install.packages("umap")}

source("ggbiplot.r")
source("tsne.R")
#source("umap.R")
source("pca.R")

library("umap")
library('shiny')
library('pastecs')
library('cluster')
library('mclust')
library('MASS')
library('Hmisc')
library('corrplot')
library('nFactors')
library("factoextra")
library("dplyr")
library("tidyr")
library("Spectrum")
library("Rtsne")
library("dbscan")
library("plot3D")
#library("kohonen")
library("mice")
library("DescTools")

library(ggplot2)
library(plyr)
library(scales)
library(grid)

shinyServer(function(input, output){
 
  
  Dataset0 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset0 <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(Dataset0)
    }
  })
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset1 = Dataset0()
      rownames(Dataset1) = Dataset1[,1]
      Dataset = Dataset1[,2:ncol(Dataset1)]
      #Dataset = t(Dataset)
      return(Dataset)
    }
  })
  
  output$readdata <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      Dataset0()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))
  
  nu.Dataset = reactive({
    data = Dataset()[,1:ncol(Dataset())]
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    nu = which(Class %in% c("numeric","integer"))
    nu.data = data[,nu] 
    return(nu.data)
  })
  
  output$xvarselect <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    checkboxGroupInput("xAttr", "Select only numerical X variables",
                       colnames(Dataset()), colnames(nu.Dataset()) )
    }
  })
  
  output$xvarsom <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      #if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      selectInput("xSom", "Select variable",
                         c("all",colnames(Dataset()[,input$xAttr])), c("all") )
    }
  })
  
  output$Clust <- renderUI({
    if (input$select %in% c("Spectral", "HDBSCAN")) { return(NULL) }
    else{
      numericInput("Clust", "Choose number of clusters:", 3, min=2)  # unlist((nS())[1])[3])
    }
  })
  
  output$Clusth <- renderUI({
    if (input$select %in% c("K-Means","Hierarchical","Spectral")) { return(NULL) }
    else{
      numericInput("Clusth", "Choose minimum number of samples", 5, min=3)  # unlist((nS())[1])[3])
    }
  })
  output$outcl <- renderUI({
    if (input$select %in% c("K-Means","Hierarchical","Spectral")) { return(NULL) }
    else{
      p("larger is the minimum number of samples selceted, denser will be clusters and more data points will be assigned as noise")
    }
  })
  #"max 10,000 obs"
  
  output$samsel <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      selectInput("obs", "Select sub sample", c("quick run, 1,000 obs", "10,000 obs", "full dataset"), selected = "quick run, 1,000 obs")
    }
  })
  
  Datasetf1 <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
    if (input$obs=="full dataset") { return(Dataset()) }
    else if(input$obs=="10,000 obs") 
                      {
                      if (nrow(Dataset())>10000){
                      set.seed(1234)
                      testsample= sample(1:nrow(Dataset()), 10000 )
                      Dataset1=Dataset()[testsample,]
                      return(Dataset1)}
                      else {return(Dataset())}
                      }
    else (input$obs=="1,000 obs")
                      {
                      if (nrow(Dataset())>1000){
                      set.seed(1234)
                      testsample= sample(1:nrow(Dataset()), 1000 )
                      Dataset1=Dataset()[testsample,]
                      return(Dataset1)}
                      else {return(Dataset())}
                      }}  
  })
  
  output$imputemiss <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
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
      p("Note: missing values imputed, if any - check options in the panel on the left",style="color:black")
    }
    else { p("Note: missing value rows dropped, if any - check options in the panel on the left",style="color:black") }
  })
  
  Datasetf2 = reactive({
    if (input$imputemiss == "do not impute or drop rows") 
    {mydataimp=Datasetf1()}
    else if (input$imputemiss == "impute missing values") 
    { mydata = Datasetf1()
    mice_mod = mice(mydata, printFlag=FALSE)
    mydataimp <- complete(mice_mod) }
    else # (input$imputemiss == "drop missing value rows") 
    { mydata = Datasetf1()
    mydataimp = na.omit(mydata)  }
    return(mydataimp)
  })
  
  output$winsor <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      selectInput("winsor", "Winsorize extreme values below 0.5% and above 99.5%", 
                  c("Yes", "No"), selected = "Yes")
    }
  })
  
  
  Datasetf = reactive({
    if (input$winsor=="No") {return(Datasetf2())}
    else (input$winsor=="Yes") 
    {
      mydata = Datasetf2()
      mydata[,colnames(nu.Dataset())] <- lapply(mydata[,colnames(nu.Dataset())], function(x) Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.005, 0.995), na.rm = TRUE)) #probs = c(0.01, 0.99)
      }
    return(mydata)
  })
  
  output$colList <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      varSelectInput("selVar",label = "Select variables for download data",
                     data = Dataset(),multiple = TRUE,selectize = TRUE, selected = ""  )
    }
  })
  
  output$scale <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      selectInput("scale", "Standardize data (usually yes)",c("Yes","No"), selected = "Yes")
    }
  })
  
  out = reactive({
    data = Datasetf()[,c(input$xAttr)]
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

    out = list(Dimensions = Dimensions, Summary =Summary ,Tail=Tail, Head=Head, MissingDataRows=Missing, num.data=nu.data, factr.data=fa.data,missing.data.rows.count=mscount)
    return(out)
  })
  
  Dataset2 = reactive({
    x00 = out()[[6]]
    if (input$scale == "Yes"){
    x0 = x00
    x01 = scale(x0, scale = T)
    dstd = data.frame(x01)}
    else {dstd=data.frame(x00)}
    #colnames(dstd) = c(colnames(x01))
    return(dstd)
  })
  
  mydata = reactive({
    #x00 = Datasetf()[,c(input$xAttr)]
    x00 = out()[[6]]
    x0 = x00
    return(x0)
  })
  
  output$head = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[4]
    }
  })
  
  output$stnote <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      if (input$scale=="Yes"){ verbatimTextOutput("scldt") }
    }
  })
  
  output$scldt = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      data=Dataset2()
      Class = NULL
      for (i in 1:ncol(data)){
        c1 = class(data[,i])
        Class = c(Class, c1)
      }
      
      nu = which(Class %in% c("numeric","integer"))
      #fa = which(Class %in% c("factor","character"))
      nu.data = data[,nu] 
      #fa.data = data[,fa] 
      list(Summary.Standardize.Data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,5))
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
      out()[5]
    }
  })
  
  output$mscount = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[8]
    }
  })
  
  output$summ = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })
  
  output$correlation = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      #data=Datasetf()[,c(input$xAttr)]
      round(cor(out()[[6]], use = "pairwise.complete.obs"),4)
    }
  })
  
  output$corplot = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      #data=Datasetf()[,c(input$xAttr)]
      pairs(out()[[6]],pch=20, col="grey")
    }
  })
  
  output$corplot1 = renderPlot({
    if (is.null(input$file)) { return(NULL) }
    else{
      my_data = mydata()[,c(input$xAttr)]
      cor.mat <- round(cor(my_data),2)
      corrplot(cor.mat, 
               type = "upper",    # upper triangular form
               order = "hclust",  # ordered by hclust groups
               tl.col = "black",  # text label color
               tl.srt = 45)  
      
    }
  })

  t0 = reactive({
    set.seed(12345)
      if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = kmeans(Dataset2(),input$Clust)
        Cluster.Membership = as.character(fit$cluster)
        clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
        Summary = list(Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm=fit )
        return(Summary)
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        distm <- dist(Dataset2(), method = "euclidean") # distance matrix
        fith <- hclust(distm, method="ward.D") 
        Cluster.Membership =  as.character(cutree(fith, k=input$Clust))
        clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
        Summary = list(Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm = fith )
        return(Summary)
      }
    })
      
      else if (input$select == "Spectral") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          if (nrow(Dataset2())>=20000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/100), silent=TRUE, showres= FALSE )}
          else if (nrow(Dataset2())>=10000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/50), silent=TRUE, showres= FALSE )}
          else if (nrow(Dataset2())>=5000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/20), silent=TRUE, showres= FALSE )} 
          else if (nrow(Dataset2())>=1000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/5), silent=TRUE, showres= FALSE )}
          else { fits = Spectrum(t(Dataset2()) , FASP=TRUE, FASPk=round(nrow(Dataset2())/2), silent=TRUE, showres= FALSE ) }
          Cluster.Membership = as.character(fits$allsample_assignments)
          clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
          Summary = list(Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm = fits )
          return(Summary)
        }
      })
    
    else if (input$select == "HDBSCAN") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        fitd = hdbscan(Dataset2(),minPts=input$Clusth)
        Cluster.Membership = as.character(fitd$cluster)
        clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
        Summary = list(Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm=fitd )
        return(Summary)
      }
    })
    
  })
  
  outdataset <- reactive({if (is.null(input$file)) { return(NULL) }
    else{
      outdataset <- Datasetf2() %>% dplyr::select(!!!input$selVar) # not winsorized
      return(outdataset)
    }})
  
  dataouttable = reactive({
    if (input$select == "K-Means") {
      fit=t0()[[3]]
      cluster = as.character(fit$cluster)
          }
    else if (input$select == "Hierarchical") {
      fith=t0()[[3]]
      cluster =  as.character(cutree(fith, k=input$Clust))
    }
    else if (input$select == "Spectral") {
      fits=t0()[[3]]
      cluster = as.character(fits$allsample_assignments)
    }
    else if (input$select == "HDBSCAN") {
      fitd=t0()[[3]]
      cluster = as.character(fitd$cluster)
    }
    
    d = data.frame(obs.id = row.names(outdataset()),cluster=cluster,outdataset())
    return(d)
  })  
  
  output$hist = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      hist(Datasetf())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
    }
  })
  
  output$dens = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
      datadensity(Datasetf())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
    }
  })
  
  output$sbplot = renderPlot({ 
    if (is.null(input$file)) {return(NULL)}
    else {
        if (input$scale=="Yes")  {
      data_wide= as.data.frame(scale(Dataset2(), center = T, scale = T))
      data_wide %>%
        gather(key="MesureType", value="Val") %>%
        ggplot( aes(x = factor(MesureType, levels = input$xAttr), y=Val)) + #fill=MesureType
        #geom_violin(trim = FALSE) +
        geom_boxplot() + ggtitle("Standardize variable boxplots") #width=0.5
        }
      else {
        data_wide= as.data.frame(mydata())
        data_wide %>%
          gather(key="MesureType", value="Val") %>%
          ggplot( aes(x = factor(MesureType, levels = input$xAttr), y=Val)) + #fill=MesureType
          #geom_violin(trim = FALSE) +
          geom_boxplot() + ggtitle("Variable boxplots")
      } 
    }
  })
  
  output$scldt1note = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      if (input$scale=="Yes"){ h4("Data Summary and Boxplots of Standardize Variables (mean=0 and var=1) - 
                                  check 'standardize data' option in the panel on the left") }
        #p("check 'standardize data' option on the left panel")}
      else { p("Note: standardize variables (to mean = 0 and variance = 1) 
               if scale is very different across selected X variables - check 'standardize data' option in the panel on the left",style="color:red") }
    }
  })
  
  output$boxx2 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    else {
    if (input$scale=="Yes")  {
        dt = data.frame(cluster=dataouttable()[,2],Dataset2())
        dt %>% pivot_longer(., cols = input$xAttr, names_to = "Var", values_to = "Val") %>%
          ggplot(aes(x = factor(Var, levels = input$xAttr), y = Val, fill = cluster)) +
          geom_boxplot() + ggtitle("Standardized variable boxplots (by clusters)")  }
    else { 
      dt = data.frame(cluster=dataouttable()[,2],mydata())
      dt %>% pivot_longer(., cols = input$xAttr, names_to = "Var", values_to = "Val") %>%
        ggplot(aes(x = factor(Var, levels = input$xAttr), y = Val, fill = cluster)) +
        #geom_violin(trim=FALSE)
        geom_boxplot() + ggtitle("Variable boxplots (by clusters)")
      } }
    
  })
  
  dataouttableprint <- reactive({
    data = dataouttable()
    data[,2] = as.character(paste0("cluster.",data[,2]),sep="")
    return(data)
  })
  
    output$table <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      else {
        dataouttableprint()
      }
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption <- renderText({
      if (input$select == "Spectral") return ("Spectral Cluster -  Summary")
      else if (input$select == "K-Means") return ("K-Means Cluster -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Cluster -  Summary")
      else if (input$select == "HDBSCAN") return ("HDBSCAN -  Summary")
      else return (NULL)
    })
    
    output$summary <- renderPrint({
      if (is.null(input$file)) {return(data.frame())}
      else {
        if (input$select == "Spectral") return ( list(t0()[2],t0()[1] ) )
        else if (input$select == "K-Means") return ( list(t0()[2],t0()[1]) )
        else if (input$select == "Hierarchical") return ( list(t0()[2],t0()[1]) )
        else if (input$select == "HDBSCAN") return ( list(t0()[2],t0()[1]) )
        else return (NULL)
           }
    })
    
    output$caption1 <- renderText({
      if (input$select == "Spectral") return ("Download Spectral Cluster Membership Data")
      else if (input$select == "K-Means") return ("Download K-Means Cluster Membership Data")
      else if (input$select == "Hierarchical") return ("Download Hierarchical Cluster Membership Data")
      else if (input$select == "HDBSCAN") return ("Download HDBSCAN Cluster Membership Data")
      else return (NULL)
    })
    
    output$caption2 <- renderText({
      if (input$select == "Spectral") return ("Spectral Cluster -  Centroid")
      else if (input$select == "K-Means") return ("K-Means Cluster - Centroid")
      else if (input$select == "Hierarchical") return ("Hierarchical Cluster -  Centroid")
      else if (input$select == "HDBSCAN") return ("HDBSCAN Cluster -  Centroid")
      else return (NULL)
    })
    
    output$summary1 <- renderPrint({
      if (is.null(input$file)) {return(data.frame())}
      else {
        if (input$select == "Spectral") return ( (t0()[1]) )
        else if (input$select == "K-Means") return ( (t0()[1]) )
        else if (input$select == "Hierarchical") return ( (t0()[1]) )
        else if (input$select == "HDBSCAN") return ( (t0()[1]) )
        else return (NULL)
      }
    })
       
    output$plotpca = renderPlot({ 
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        data.pca <- prcomp(Dataset2(),center = TRUE,scale. = TRUE)
        plot(data.pca, type = "l"); abline(h=1)    
      }
    })

    output$plot3 = renderPlot({
      if (is.null(input$file)) { NULL}
      else {
      if (input$select == "Hierarchical") { 
        fith = t0()[[3]]
        plot(fith) } # display dindogram
      else if (input$select == "K-Means") {
        set.seed(123)
        fviz_nbclust(Dataset2(), kmeans, method = "silhouette", k.max = 7)
      }
      else if (input$select == "Spectral") {
        fits = t0()[[3]]
        plot(fits$eigenvector_analysis)
      }
        else if (input$select == "HDBSCAN") {
          set.seed(123)
          fitd = t0()[[3]]
          plot(fitd,show_flat=TRUE)
        }
      }
    })
    
    output$plot = renderPlot({  
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        if (is.null(input$file)) {return(data.frame())}
        fit=t0()[[3]]
        classif1 = as.character(fit$cluster)
            })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) { return(data.frame())  }
        fith = t0()[[3]]
        classif1 =  as.character(cutree(fith, k=input$Clust))
      })
      else if (input$select == "Spectral") ({
        if (is.null(input$file)) {return(data.frame())}
        fits=t0()[[3]]
        #pca(fits$similarity_matrix,labels=fits$allsample_assignments,axistextsize=8,legendtextsize=8,dotsize=2)
        classif1 =  as.character(fits$allsample_assignments)
      })
      else if (input$select == "HDBSCAN") ({
        if (is.null(input$file)) {return(data.frame())}
        fitd=t0()[[3]]
        classif1 = as.character(fitd$cluster)
      })
      
      data.pca <- prcomp(Dataset2(),
                         center = TRUE,
                         scale = TRUE)
      g <- ggbiplot(data.pca,
                    obs.scale = 1,
                    var.scale = 1,
                    groups = classif1,
                    ellipse = TRUE,
                    circle = FALSE)
      
      g <- g + scale_color_discrete(name = '')
      g <- g + theme(legend.direction = 'vertical',
                     legend.position = 'right')
      print(g)
    })
    
    dup = reactive({
      dup = which(duplicated(Dataset2()))
      return(dup)
    })
    
    output$dup =  renderPrint({
      length(dup())
    })
    
    output$plotrtsne = renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        if (input$select == "K-Means") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        else if (input$select == "Hierarchical") ({
          fith = t0()[[3]]
          classif1 =  as.integer(cutree(fith, k=input$Clust))
          classif2 =  as.factor(cutree(fith, k=input$Clust))
        })
        else if (input$select == "Spectral") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$allsample_assignments)
          classif2 = as.factor(fit$allsample_assignments)
        })
        else if (input$select == "HDBSCAN") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        
 #       if (input$dupp=="No") { data = Dataset2(); class=classif1 }
  #      else{
          #dup = dup()
          data1 = cbind.data.frame(classif1, Dataset2())
          dup = !(duplicated(Dataset2()))
          data = data1[dup,-1]
          class= data1[dup,1]
  #      }
        set.seed(12345)
        y = Rtsne::Rtsne(as.matrix(data), num_threads=0, dim=3, perplexity = as.integer(input$perp)  )#max_iter=as.integer(input$iter),
        scatter3D(y$Y[,1],y$Y[,2],y$Y[,3], phi = 0, #bty = "g", ex = 2,
                  ticktype = "detailed", colvar = class,
                   pch=20, # col = gg.col(100), #cex=1, groups = classif2, # # type = 'h',
                  xlab='Dim1', ylab='Dim2', zlab='Dim3')
      }
    })

    output$plotumap = renderPlot({
      if (is.null(input$file)) {return(NULL)}
      else {
        if (input$select == "K-Means") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        else if (input$select == "Hierarchical") ({
          fith = t0()[[3]]
          classif1 =  as.integer(cutree(fith, k=input$Clust))
          classif2 =  as.factor(cutree(fith, k=input$Clust))
        })
        else if (input$select == "Spectral") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$allsample_assignments)
          classif2 = as.factor(fit$allsample_assignments)
        })
        else if (input$select == "HDBSCAN") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        
        #       if (input$dupp=="No") { data = Dataset2(); class=classif1 }
        #      else{
        #dup = dup()
        data1 = cbind.data.frame(classif2, Dataset2())
        dup = !(duplicated(Dataset2()))
        data = data1[dup,-1]
        cluster = data1[dup,1]
        #      }
        #set.seed(12345)
        umap.out=(umap(data))
        umap.cor=as.data.frame(umap.out$layout)
        colnames(umap.cor) = c("UMAP1", "UMAP2")
        umap.cor %>% 
        mutate(cluster = cluster) %>%
        ggplot(aes(UMAP1, UMAP2, color = cluster)) + geom_point()
      }
    })    

    output$caption3 <- renderText({
      if (input$select == "Spectral") return ("t-SNE Visulization - Spectral Clusters")
      else if (input$select == "K-Means") return ("t-SNE Visulization - K-Means Clusters")
      else if (input$select == "Hierarchical") return ("t-SNE Visulization - Hierarchical Clusters")
      else if (input$select == "HDBSCAN") return ("t-SNE Visulization - HDBSCAN Clusters")
      else return (NULL)
    })

    output$caption4 <- renderText({
      if (input$select == "Spectral") return ("UMAP Visulization - Spectral Clusters")
      else if (input$select == "K-Means") return ("UMAP Visulization - K-Means Clusters")
      else if (input$select == "Hierarchical") return ("UMAP Visulization - Hierarchical Clusters")
      else if (input$select == "HDBSCAN") return ("UMAP Visulization - HDBSCAN Clusters")
      else return (NULL)
    })
        
    output$plotsom = renderPlot({ 
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        if (input$select == "K-Means") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        else if (input$select == "Hierarchical") ({
          fith = t0()[[3]]
          classif1 =  as.integer(cutree(fith, k=input$Clust))
          classif2 =  as.factor(cutree(fith, k=input$Clust))
        })
        else if (input$select == "Spectral") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$allsample_assignments)
          classif2 = as.factor(fit$allsample_assignments)
        })
        else if (input$select == "HDBSCAN") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        
        data_train <- as.matrix(scale(Dataset2()))
        som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")
        som.model <- som(data_train, grid=som_grid)
        #som.cluster <- cutree(hclust(dist(getCodes(som.model))), input$Clust)
        #class <- xyf(data_train, classvec2classmat(classif2), som_grid)
        #class.cluster <- cutree(hclust(dist(getCodes(class))), input$Clust)
        #plot(class, type = "codes", palette.name=rainbow) #bgcol=rainbow(input$Clust) )
        #plot(som.model, type = "codes", palette.name=rainbow) #, bgcol=pretty_palette(input$Clust) )
        if (input$xSom == "all"){plot(som.model, type = "codes", palette.name=rainbow)}
        else {plot(som.model, type = "property", property = getCodes(som.model)[,input$xSom],
                   main=input$xSom, palette.name=rainbow )}
       # plot(class, type = "property", property = getCodes(som.model)[,1],main=colnames(getCodes(som.model))[1], bgcol=rainbow(input$Clust))
        
        add.cluster.boundaries(som.model, classif2)
        #var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som.model$unit.classif), FUN=mean, simplify=TRUE)[,2]
        #plot(som.model, type = "property", property=var_unscaled,main=colnames(getCodes(som_model))[var], palette.name=rainbow)
        
      }
    })
    
    
    output$plotcor = renderPlot({
      if (is.null(input$file)) { return(NULL) }
      else{
        mammals_df = mydata()[,c(input$xAttr)]
       
        if (input$select == "K-Means") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        else if (input$select == "Hierarchical") ({
          fith = t0()[[3]]
          classif1 =  as.integer(cutree(fith, k=input$Clust))
          classif2 =  as.factor(cutree(fith, k=input$Clust))
        })
        else if (input$select == "Spectral") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$allsample_assignments)
          classif2 = as.factor(fit$allsample_assignments)
        })
        else if (input$select == "HDBSCAN") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$cluster)
          classif2 = as.factor(fit$cluster)
        })
        
        mammals_df$cluster <- classif2
        par(mfrow=c(2,1))
        ggpairs(mammals_df, 1:5, mapping = ggplot2::aes(color = cluster, alpha = 0.5), 
                diag = list(continuous = wrap("densityDiag")), 
                lower=list(continuous = wrap("points", alpha=0.9)))  
        ggpairs(mammals_df, 6:10, mapping = ggplot2::aes(color = cluster, alpha = 0.5), 
                diag = list(continuous = wrap("densityDiag")), 
                lower=list(continuous = wrap("points", alpha=0.9)))  
      }
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() { "iris.csv" },
      content = function(file) {
        write.csv(read.csv("data/iris.csv"), file, row.names=F, col.names=F)
      }
    )
    
    output$downloadData4 <- downloadHandler(
      filename = function() { "cluster_solution.csv" },
      content = function(file) {
      write.csv(dataouttableprint(), file, row.names=F)
      }
    )
    
    
    

})
  
