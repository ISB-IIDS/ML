####################################################
#      Cluster Analysis                         #
####################################################
if (!require("shiny")) {install.packages("shiny")}
if (!require("cluster")){install.packages("cluster")}
if (!require("ggbiplot")){install.packages("ggbiplot")}
if (!require("mclust")){install.packages("mclust")}
if (!require("MASS")){install.packages("MASS")}
if (!require("Hmisc")){install.packages("Hmisc")} # for describe
if (!require("pastecs")){install.packages("pastecs")} #for stat.desc
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("nFactors")) {install.packages("nFactors")}
if (!require("factoextra")) {install.packages("factoextra")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("Spectrum")) {install.packages("Spectrum")}
if (!require("Rtsne")) {install.packages("Rtsne")}
if (!require("plot3D")) {install.packages("plot3D")}

library('shiny')
library('pastecs')
library('cluster')
library('ggbiplot')
library('mclust')
library('MASS')
library('Hmisc')
library('corrplot')
library('nFactors')
library("factoextra")
library("dplyr")
library("Spectrum")
library("Rtsne")
library("plot3D")

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
  
  output$Clust <- renderUI({
    if (input$select == "Spectral") { return(NULL) }
    else{
      numericInput("Clust", "Choose number of clusters:", 3)  # unlist((nS())[1])[3])
    }
  })
  #"max 10,000 obs"
  
  output$samsel <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      selectInput("obs", "Select sub sample", c("quick run, 1,000 obs", "10,000 obs", "full dataset"), selected = "quick run, 1,000 obs")
    }
  })
  
  Datasetf <- reactive({
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
  
  output$colList <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      varSelectInput("selVar",label = "Select variables for download data",
                     data = Dataset(),multiple = TRUE,selectize = TRUE,selected = colnames(Dataset()))
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
  
  outdataset <- reactive({if (is.null(input$file)) { return(NULL) }
    else{
      outdataset <- Datasetf() %>% dplyr::select(!!!input$selVar)
      return(outdataset)
    }})
  
  Dataset2 = reactive({
    x00 = Datasetf()[,c(input$xAttr)]
    if (input$scale == "yes"){
    x0 = x00
    x01 = scale(x0, scale = T)
    dstd = data.frame(x01)}
    else {dstd=data.frame(x00)}
    #colnames(dstd) = c(colnames(x01))
    return(dstd)
  })
  
  mydata = reactive({
    x00 = Datasetf()[,c(input$xAttr)]
    x0 = x00
    return(x0)
  })
  
  output$head = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[4]
    }
  })
  
  output$scldt = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      data=Dataset2()[,c(input$xAttr)]
      Class = NULL
      for (i in 1:ncol(data)){
        c1 = class(data[,i])
        Class = c(Class, c1)
      }
      
      nu = which(Class %in% c("numeric","integer"))
      #fa = which(Class %in% c("factor","character"))
      nu.data = data[,nu] 
      #fa.data = data[,fa] 
      list(Note = "Usually, it is a good practice to standardize the data before running cluster analysis (mean 0 and variance 1)", Summary.Standardize.Data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,5))
    }
  })
  
  output$stnote <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    else {
      if (input$scale=="yes"){ verbatimTextOutput("scldt") }
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
  
  output$corplot = renderPlot({
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
          if (nrow(Dataset2())>=20000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/50), silent=TRUE, showres= FALSE )}
          else if (nrow(Dataset2())>=10000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/20), silent=TRUE, showres= FALSE )}
          else if (nrow(Dataset2())>=5000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/10), silent=TRUE, showres= FALSE )} 
          else if (nrow(Dataset2())>=1000) {fits = Spectrum(t(Dataset2()), FASP=TRUE, FASPk=round(nrow(Dataset2())/2), silent=TRUE, showres= FALSE )}
          else { fits = Spectrum(t(Dataset2()) )  }
          Cluster.Membership = as.character(fits$allsample_assignments)
          clustmeans = aggregate(mydata(),by = list(Cluster.Membership), FUN = mean)
          Summary = list(Cluster.Means =clustmeans, Count = table(Cluster.Membership), ModelSumm = fits )
          return(Summary)
        }
      })
  })
  
  dataouttable = reactive({
    if (input$select == "K-Means") {
      fit=t0()[[3]]
      cluster = as.character(fit$cluster)
          }
    else if (input$select == "Hierarchical") {
      fith=t0()[[3]]
      cluster =  cutree(fith, k=input$Clust)
    }
    else if (input$select == "Spectral") {
      fits=t0()[[3]]
      cluster = as.character(fits$allsample_assignments)
    }
    
    d = data.frame(obs.id = row.names(outdataset()),cluster,outdataset())
    return(d)
  })   
    
    output$table <- renderDataTable({
      if (is.null(input$file)) {return(NULL)}
      else {
        dataouttable()
      }
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption <- renderText({
      if (input$select == "Spectral") return ("Spectral Cluster -  Summary")
      else if (input$select == "K-Means") return ("K-Means Cluster -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Cluster -  Summary")
      else return (NULL)
    })
    
    output$summary <- renderPrint({
      if (is.null(input$file)) {return(data.frame())}
      else {
        if (input$select == "Spectral") return ( list(t0()[2],t0()[1] ) )
        else if (input$select == "K-Means") return ( list(t0()[2],t0()[1]) )
        else if (input$select == "Hierarchical") return ( list(t0()[2],t0()[1]) )
        else return (NULL)
           }
    })
    
    output$caption1 <- renderText({
      if (input$select == "Spectral") return ("Download Spectral Cluster Membership Data")
      else if (input$select == "K-Means") return ("Download K-Means Cluster Membership Data")
      else if (input$select == "Hierarchical") return ("Download Hierarchical Cluster Membership Data")
      else return (NULL)
    })
    
    output$caption2 <- renderText({
      if (input$select == "Spectral") return ("Spectral Cluster -  Centroid")
      else if (input$select == "K-Means") return ("K-Means Cluster - Centroid")
      else if (input$select == "Hierarchical") return ("Hierarchical Cluster -  Centroid")
      else return (NULL)
    })
    
    output$caption3 <- renderText({
      if (input$select == "Spectral") return ("Spectral Cluster -  Visulization")
      else if (input$select == "K-Means") return ("K-Means Cluster - Visulization")
      else if (input$select == "Hierarchical") return ("Hierarchical Cluster -  Visulization")
      else return (NULL)
    })
    
    output$summary1 <- renderPrint({
      if (is.null(input$file)) {return(data.frame())}
      else {
        if (input$select == "Spectral") return ( (t0()[1]) )
        else if (input$select == "K-Means") return ( (t0()[1]) )
        else if (input$select == "Hierarchical") return ( (t0()[1]) )
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
      }
    })
    
    output$plot = renderPlot({  
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        if (is.null(input$file)) {return(data.frame())}
        fit=t0()[[3]]
        classif1 = as.character(fit$cluster)
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
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) { return(data.frame())  }
        fith = t0()[[3]]
        classif1 =  as.character(cutree(fith, k=input$Clust))
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
      else if (input$select == "Spectral") ({
        if (is.null(input$file)) {return(data.frame())}
        fits=t0()[[3]]
        #pca(fits$similarity_matrix,labels=fits$allsample_assignments,axistextsize=8,legendtextsize=8,dotsize=2)
        classif1 =  as.character(fits$allsample_assignments)
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
      
    })
    
    dup = reactive({
      dup = which(duplicated(Dataset2()))
      return(dup)
    })
    
    output$dup =  renderPrint({
      length(dup())
    })
    
    output$resplot4 = renderPlot({
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
        if (input$select == "Spectral") ({
          fit=t0()[[3]]
          classif1 = as.integer(fit$allsample_assignments)
          classif2 = as.factor(fit$allsample_assignments)
        })
        
        if (length(dup())==0) { data = Dataset2() }
        else{
          data1 = Dataset2()
          dup = dup()
          data = data1[-dup,]
        }
        
        y = Rtsne::Rtsne(as.matrix(data), perplexity = as.integer(input$perp), num_threads=0, max_iter=as.integer(input$iter), dim=3)
        scatter3D(y$Y[,1],y$Y[,2],y$Y[,3], phi = 0, #bty = "g", ex = 2,
                  ticktype = "detailed", 
                  colvar = classif1, pch=20, cex=1, # #col = gg.col(100), type = 'h',
                  xlab='Dim1', ylab='Dim2', zlab='Dim3')
      }
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { "UniversalBank.csv" },
      content = function(file) {
        write.csv(read.csv("data/UniversalBank.csv"), file, row.names=F, col.names=F)
      }
    )
    
    output$downloadData4 <- downloadHandler(
      filename = function() { "cluster_output.csv" },
      content = function(file) {
      write.csv(dataouttable(), file, row.names=F)
      }
      
    )

})
  
