####################################################
#      Cluster Analysis                         #
####################################################

library('shiny')
library('cluster')
library('ggbiplot')
library('mclust')
library('MASS')






shinyServer(function(input, output){
  
  
  Dataset11 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      Dataset1 = as.data.frame(scale(Dataset1, center = T, scale = T))
      return(Dataset1)
    }
  })
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      return(Dataset1)
    }
  })
  
  output$xvarselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    checkboxGroupInput("xAttr", "Select variables to be used in segmentation analysis",
                       setdiff(colnames(Dataset()),''), setdiff(colnames(Dataset()),''))
    
  })
  
  Dataset2 = reactive({
    mydata = Dataset()[,c(input$xAttr)]
    mydata1 = as.data.frame(scale(mydata, center = T, scale = T))
    return(mydata1)
  })
  
  Dataset1 = reactive({
    mydata = Dataset()[,c(input$xAttr)]
    return(mydata)
  })
  
  t0 = reactive({
    set.seed(12345)
    if (input$select == ""){} else {
      if (input$select == "K-Means") ({
      
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      
      else {
        fit = kmeans(Dataset2(),input$Clust)
        Segment.Membership =  fit$cluster
        d = data.frame(r.name = row.names(Dataset1()),Segment.Membership,Dataset2())
        return(d)
      }
    })
    
    else if (input$select == "Hierarchical") ({
      if (is.null(input$file)) {
        # User has not uploaded a file yet
        return(data.frame())
      }
      else {
        distm <- dist(Dataset2(), method = "euclidean") # distance matrix
        fit <- hclust(distm, method="ward") 
        Segment.Membership =  cutree(fit, k=input$Clust)
        d = data.frame(r.name = row.names(Dataset1()),Segment.Membership,Dataset2())
        return(d)
      }
    })
    }
  })
    
    output$table <- renderDataTable({
      t0()
    }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 30))
    
    output$caption1 <- renderText({
      if (input$select == "") return ("Choose cluster algorithm and click ''Apply Changes'' button")
      #else if (input$select == "Model Based") return ("Model Based Segmentation -  Summary")
      else if (input$select == "K-Means") return ("K-Means Segmentation -  Summary")
      else if (input$select == "Hierarchical") return ("Hierarchical Segmentation -  Summary")
      else return (NULL)
    })
    
    output$summary1 <- renderText({
      
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          # 
          # fit = kmeans(Dataset(),input$Clust)
          # Segment.Membership = as.character(fit$cluster)
          # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
          # Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
          # 
          # 
          Summary<-t0()[-1] %>% 
            #mutate(Group = as.factor(Segment.Membership)) %>%
            group_by(Segment.Membership) %>%
            summarize_all(.funs = list(mean)) %>%
            arrange(Segment.Membership) %>%
            round(2)%>%
            mutate_if(is.numeric, function(x) {
              cell_spec(x, bold = T, 
                        color = spec_color(x, end = 0.9),
                        font_size = spec_font_size(x,begin = 14,end = 20))
            }) %>%
            kable(escape = F, align = "c") %>%
            kable_styling(c("striped", "condensed"), full_width = F)%>%
          footnote(general = "Mean value of all variables within each cluster. ")
          return(Summary)
        }
      })  
      else if (input$select == "Hierarchical") ({
            if (is.null(input$file)) {
              # User has not uploaded a file yet
              return(data.frame())
            }
            else {
              # d <- dist(Dataset(), method = "euclidean") # distance matrix
              # fit <- hclust(d, method="ward")
              # Segment.Membership =  as.character(cutree(fit, k=input$Clust))
              # clustmeans = aggregate(Dataset2(),by = list(Segment.Membership), FUN = mean)
              # Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
              # Summary
              Summary<-t0()[-1] %>% 
               # mutate(Group = as.factor(Segment.Membership)) %>%
                group_by(Segment.Membership) %>%
                summarize_all(.funs = list(mean)) %>%
                arrange(Segment.Membership) %>%
                round(2)%>%
                mutate_if(is.numeric, function(x) {
                  cell_spec(x, bold = T, 
                            color = spec_color(x, end = 0.9),
                            font_size = spec_font_size(x,begin = 14,end = 20))
                }) %>%
                kable(escape = F, align = "c") %>%
                kable_styling(c("striped", "condensed"), full_width = F)%>%
                footnote(general = "Mean value of all variables within each cluster. ")
              return(Summary)
            }
          })
    })
    
    output$summary <- renderPrint({
      if (input$select == ""){} else {
      
      if (input$select == "K-Means") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          fit = kmeans(Dataset2(),input$Clust)
          Segment.Membership = as.character(fit$cluster)
          clustmeans = aggregate(Dataset1(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership) )
          Summary
        }
      })
      
      else  if (input$select == "Model Based") ({
        
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        else {
          fit = Mclust(Dataset2(),input$Clust)
          Segment.Membership = as.character(fit$classification)
          clustmeans = aggregate(Dataset1(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans,ModelSumm = summary(fit) )
          Summary
        }
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        else {
          d <- dist(Dataset2(), method = "euclidean") # distance matrix
          fit <- hclust(d, method="ward") 
          Segment.Membership =  as.character(cutree(fit, k=input$Clust))
          clustmeans = aggregate(Dataset1(),by = list(Segment.Membership), FUN = mean)
          Summary = list(Segment.Membership = Segment.Membership, SegMeans =clustmeans, Count = table(Segment.Membership), ModelSumm = fit )
          Summary
        }
      })
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
    
    
    output$plot = renderPlot({  
      set.seed(12345)
      
      if (input$select == "K-Means") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        fit = kmeans(Dataset2(),input$Clust)
        
        classif1 = as.character(fit$cluster)
        data.pca <- prcomp(Dataset2(),
                           center = TRUE,
                           scale. = TRUE)
        
        # plot(data.pca, type = "l"); abline(h=1)    
        
        g <- ggbiplot(data.pca,
                      obs.scale = 1,
                      var.scale = 1,
                      groups = classif1,
                      ellipse = TRUE,
                      circle = TRUE)
        
        g <- g + scale_color_discrete(name = '')
        g <- g + theme(legend.direction = 'horizontal',
                       legend.position = 'top')
        print(g)
        
      })
      
      else if (input$select == "Hierarchical") ({
        if (is.null(input$file)) {
          # User has not uploaded a file yet
          return(data.frame())
        }
        
        d <- dist(Dataset2(), method = "euclidean") # distance matrix
        fit <- hclust(d, method="ward") 
        plot(fit) # display dendogram
        groups <- cutree(fit, k=input$Clust) # cut tree into 5 clusters
        # draw dendogram with red borders around the 5 clusters
        rect.hclust(fit, k=input$Clust, border="red") 
      })
    })
    
    output$downloadData4 <- downloadHandler(
      filename = function() { "segmentation.csv" },
      content = function(file) {
        write.csv(t0(), file, row.names=F)
      }
    )

})
  
