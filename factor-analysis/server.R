#################################################
#               Factor Analysis                 #
#################################################
if (!require("shiny")) {install.packages("shiny")}
if (!require("pastecs")){install.packages("pastecs")} #for stat.desc
if (!require("Hmisc")){install.packages("Hmisc")} # for describe
if (!require("nFactors")) {install.packages("nFactors")}
if (!require("qgraph")) {install.packages("qgraph")}
if (!require("corrplot")) {install.packages("corrplot")}
if (!require("dplyr")) {install.packages("dplyr")}
if (!require("DT")) {install.packages("DT")}
if (!require("mice")) {install.packages("mice")}

if (!require("tidyr")) {install.packages("tidyr")} # gather()
library("tidyr")

library("shiny")
library("pastecs")
library("nFactors")
library("qgraph")
library("corrplot")
library("dplyr")
library("DT")
library("Hmisc")
library("mice")



shinyServer(function(input, output) {

  Dataset0 <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
    Dataset0 <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset0)
    }
  })
  
  Datasetf <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset1 = Dataset0()
      rownames(Dataset1) = Dataset1[,1]
      Dataset = Dataset1[,2:ncol(Dataset1)]
      #Dataset = t(Dataset)
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
        selectInput("imputemiss", "Impute missing values or drop missing value rows", 
                    c("do not impute or drop rows", "impute missing values", "drop missing value rows"), selected = "drop missing value rows")
      }}
  })
  
  output$imout <- renderUI({
    if (identical(Datasetf(), '') || identical(Datasetf(),data.frame())) return(NULL)
    if (input$imputemiss == "do not impute or drop rows") {return(NULL)}
    else if ((input$imputemiss == "impute missing values")) {
      p("Note: missing values imputed",style="color:red")
    }
    else { p("Note: missing value rows dropped",style="color:red") }
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
  
  
  output$readdata <- renderDataTable({
    if (is.null(input$file)) {return(NULL)}
    else {
      Dataset0()
    }
  }, options = list(lengthMenu = c(5, 30, 50,100), pageLength = 5))

fname <- reactive({
  if(length(strsplit(input$fname,',')[[1]])==0){return(NULL)}
  else{
    return(strsplit(input$fname,',')[[1]])
  }
})

output$colList <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
  varSelectInput("selVar",label = "Select variables",data = Datasetf1(),multiple = TRUE,selectize = TRUE,selected = colnames(nu.Dataset()))
  }
    })

filtered_dataset1 <- reactive({if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- Dataset() %>% dplyr::select(!!!input$selVar)
    return(Dataset)
  }})


# output$table22 <- renderTable ({ 
#   round(cor(Dataset()),2) 
#                                       })

out = reactive({
  data = filtered_dataset1()
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
  out = list(Dimensions=Dimensions, Summary=Summary, Tail=Tail, fa.data, nu.data, a, j, Head=Head, MissingDataRows=Missing, missing.data.rows.count=mscount)
  return(out)
})

# norm_dataset = reactive({
#   x00 = nu.Dataset()
#   if (input$scale == "yes"){
#     x0 = x00
#     x01 = scale(x0, scale = T)
#     dstd = data.frame(x01)}
#   else {dstd=data.frame(x00)}
#   #colnames(dstd) = c(colnames(x01))
#   return(dstd)
# })
# 
# filtered_dataset = reactive({
#   x00 = norm_dataset() %>% dplyr::select(!!!input$selVar)
#   return(x00)
# })
# 
# 
# output$downList <- renderUI({
#   if (is.null(input$file)) {return(NULL)}
#   else {
#     varSelectInput("downVar",label = "Select variables for download data",
#                    data = nu.Dataset(),multiple = TRUE,selectize = TRUE,selected = colnames(nu.Dataset()))
#   }
# })
# 
# outdataset <- reactive({if (is.null(input$file)) { return(NULL) }
#   else{
#     outdataset <- norm_dataset() %>% dplyr::select(!!!input$downVar)
#     return(outdataset)
#   }})
# 
# output$scldt = renderPrint({
#   if (is.null(input$file)) {return(NULL)}
#   else {
#     nu.data = outdataset() 
#     #fa.data = data[,fa] 
#     list(Note = "Usually, it is a good practice to standardize the data before running cluster analysis (mean 0 and variance 1)", Summary.Standardize.Data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,5))
#   }
# })

filtered_dataset <- reactive({if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- out()[[5]]
    return(Dataset)
  }})

output$hist = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    hist(filtered_dataset1())#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$dens = renderPlot({
  if (is.null(input$file)) {return(NULL)}
  else {
    datadensity(out()[[5]])#[,input$xAttr],main=paste("Histogram of" ,input$xAttr), xlab=input$xAttr)
  }
})

output$missing = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[9]
  }
})

output$head = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[8]
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
  if (is.null(input$file)) { return(NULL) }
    else{
      my_data = filtered_dataset()
      cor.mat <- round(cor(my_data),2)
      corrplot(cor.mat, 
               type = "upper",    # upper triangular form
               order = "hclust",  # ordered by hclust groups
               tl.col = "black",  # text label color
               tl.srt = 45)  
      
    }
  })



output$table <- renderDataTable({ Dataset() },options = list(pageLength=25))
  
nS = reactive ({    
    if (is.null(input$file)) { return(NULL) }
    else{
      ev = eigen(cor(filtered_dataset(), use = 'pairwise.complete.obs'))  # get eigenvalues
      ap = parallel(subject=nrow((filtered_dataset())),var=ncol((filtered_dataset())),rep=100,cent=.05)
      nS = nScree(ev$values, aparallel= ap$eigen$qevpea)
}
})

output$fselect <- renderUI({
  if (is.null(input$file)) { return(NULL) }
  else{
    if (unlist((nS())[1])[1]==1){ nfac= unlist((nS())[1])[3]} else {nfac = unlist((nS())[1])[1]}
  numericInput("fselect", "Number of factors", nfac)
  }
  })

fselect = reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    
  fselect=input$fselect
  return(fselect)
  }
})

fit = reactive ({ 
  if (is.null(input$file)) { return(NULL) }
  else{
    
  fit = factanal(na.omit(filtered_dataset()), fselect() , scores="Bartlett", rotation="varimax");
  return (fit)
  }
  }) 


output$xaxis <- renderUI({
  
  if (is.null(input$file)) { return(NULL) }
  else {
  if(is.null(fname())){
    n =(fselect())
    list = character(0)
    for (i in 1:n) { 
      temp = paste("Factor",i)
      list = c(list, temp)
    }
    
    selectInput("xaxis", "Choose factor for plotting on X axis",
                list, selected = "Factor 1")
  }else{
      temp = fname()
       selectInput("xaxis", "Choose factor for plotting on X axis",
                temp, selected = temp[1])
    
  }
    
    
  }
  
  
})

output$yaxis <- renderUI({
  
  if (is.null(input$file)) { return(NULL) }
  else {
    if(is.null(fname())){
      n =(fselect())
      list = character(0)
      for (i in 1:n) { 
        temp = paste("Factor",i)
        list = c(list, temp)
      }
      list2 = setdiff(list,input$xaxis)
      selectInput("yaxis", "Choose Factor for plotting on Y axis",
                  list2, selected = "Factor 2")
    }else{
      temp = fname()
      temp2 = setdiff(temp,input$xaxis)
      selectInput("yaxis", "Choose Factor for plotting on Y axis",
                  temp2, selected = temp2[1])
      
    }
      }
  
})

f1 = reactive({
  if(is.null(fname())){
    f = input$xaxis
    s <- strsplit(f, "[^[:digit:]]")
    solution <- as.numeric(unlist(s))
    solution <- unique(solution[!is.na(solution)])
    return(solution)
  }else{
index <- match(input$xaxis,fname())
return(index)
}
  
})

f2 = reactive({
  if(is.null(fname())){
    f = input$yaxis
    s <- strsplit(f, "[^[:digit:]]")
    solution <- as.numeric(unlist(s))
    solution <- unique(solution[!is.na(solution)])
    return(solution)
  }else{
    index<-match(input$yaxis,fname()) 
    return(index)
    
   }
  
  
})

output$text1 <- renderText({    
  if (is.null(input$file)) { return(NULL) }
else {
      return(paste("Test of the hypothesis that",(fit())$factors,"factors are sufficient."))}
})

output$text2 <- renderText({
  if (is.null(input$file)) { return(NULL) }
 else{
     return(paste("The chi square statistic is",round((fit())$STATISTIC,3),"on",(fit())$dof," degrees of freedom.")) }
                                   })

output$text3 <- renderText({
  if (is.null(input$file)) { return(NULL) }
 else{
   return(paste("The p-value is",round((fit())$PVAL,3)))
 }
  })
#output$text4 <- renderText({ return(paste("Note - Optimal factors from are:",unlist((nS())[1])[3])) })

output$plot1 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
  plotnScree(nS())
  }
  })


output$plot20 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
a = unclass((fit())$loadings)
grp = NULL
for (i in 1:nrow(a)){
  max = max(abs(a[i,]))
  temp0 =  which(abs(a[i,]) == max)
  temp = temp0[1]
  grp = c(grp,temp)
}
grp = matrix(c(grp,seq(1:length(grp))),,2)
rownames(grp) = colnames(filtered_dataset())

gr = vector("list", length = length(table(grp[,1])))
for (i in 1:length(table(grp[,1]))) {
  l1  = grp[(grp[,1] == as.numeric(names(table(grp[,1])[i]))),2]
  gr[[i]][1:length(l1)] = c(l1)   
}

qgraph(cor(filtered_dataset(), use= 'complete.obs'),layout="spring", groups = gr, labels=names(filtered_dataset()), label.scale=F, label.cex = 1, minimum=input$cutoffcorr)
}
})

output$plot2 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
  a0 = (fit())$loadings
  a1 = a0
  for (i in 1:ncol(a1)){  a1[,i] = a0[,i]*(abs(a0[,i]) > input$cutoff)}
  k2 = f1()
  k3 = f2()
  
  factor.plot = function(a0, a1, k2, k3){
    
    load = a0[((a1[, k2] != 0)|(a1[, k3] != 0)), c(k2, k3)]
    
    par(col="black") #black lines in plots
    
    plot(load,type="p",pch=19,col="red", xlim=c(-1, 1), ylim=c(-1, 1),xlab=input$xaxis,ylab = input$yaxis) # set up plot
    
    abline(h=0);abline(v=0)#draw axes
    
    arrows(0,0, x1=load[,1], y1=load[,2], col="blaCK", lwd=1.5);
    
    text(load,labels = rownames(load),cex=1,pos=1)
    
  } # factor.plot() func ends
  
  factor.plot(a0, a1, k2, k3)
  }
})

output$plot3 = renderPlot({
  if (is.null(input$file)) { return(NULL) }
  else{
    
plot(x=(fit())$scores[,(f1())], y=(fit())$scores[,(f2())], type="p", pch=19, col="red",
    xlab = paste0(input$xaxis), ylab = paste0(input$yaxis))   # added this line in edit

text(x=(fit())$scores[,(f1())],y=(fit())$scores[,(f2())],labels=rownames(Dataset()), pos = 2, col="blue", cex=0.8)

abline(h=0); abline(v=0)
}
})

output$loadings <- renderDataTable({ 
  if (is.null(input$file)) { return(NULL) } else{
  # rownames((fit())$loadings) = colnames(Dataset())  # edit 2
  b2 <- unclass((fit())$loadings); rownames(b2) <- NULL;  
  b1 <- data.frame(colnames(filtered_dataset()), round(b2,2));
  names(b1)[1] <- "Variable"# [2:ncol(Dataset())];rownames(b1) <- colnames(Dataset())  # edit 2  
  #-------#
  if(is.null(fname())){return(b1)}
  else{
    names(b1)[c(-1)]<-fname()
    return(b1)
  }
  
  #-------#
  return(b1) # unclass((fit())$loadings)
  }
  })

mat = reactive({
  fact = (fit())
# SS.loadings= colSums(fact$loading*fact$loading)
# Proportion.Var = colSums(fact$loading*fact$loading)/dim(fact$loading)[1]
# Cumulative.Var= cumsum(colSums(fact$loading*fact$loading)/dim(fact$loading)[1])
# mat = rbind(SS.loadings,Proportion.Var,Cumulative.Var)
laodings = print(fact$loadings, digits=2, cutoff=.25, sort = TRUE)
# out = list(Stat = mat, loadings=laodings)
# return(laodings)

})

output$mat <- renderPrint({ 
  if (is.null(input$file)) { return(NULL) }
  else{ mat() }
  
  })

# uni = reactive({ 
# a = matrix(fit()$uniqueness,1,)
# colnames(a) = rownames(as.matrix(fit()$uniqueness))
# rownames(a) = "Uniqueness"
# return(a)
#  })

output$uni <- renderDataTable({ 
  if (is.null(input$file)) { return(NULL) }
  else{ 
    # n = ceiling(length(uni())/3)
    # matrix(uni(), ncol = 1)
    data.frame(Variable = rownames(as.matrix(fit()$uniqueness)), Uniqueness = round(fit()$uniqueness,2))
    }
},options = list(pageLength=10))


output$scores <- renderDataTable({     
  if (is.null(input$file)) { return(NULL) } else{
      # rownames((fit())$scores) = rownames(Dataset()) # edit 3 i made.
      # b0 <- (fit())$scores;   rownames(b0) <- rownames(Dataset()); 
      b2 <- unclass((fit())$scores); rownames(b2) <- NULL; 
      b0 <- data.frame(rownames(filtered_dataset()), round(b2,2)) # else ends    
      names(b0)[1] <- "Variable"
  if(is.null(fname())){return(b0)}
  else{
    names(b0)[c(-1)]<-fname()
    return(b0)
  }
  }
      # unclass((fit())$scores)
      #                             }
})  
  
# my edits 16-sept-2017 below
  output$downloadDataX <- downloadHandler(
    filename = function() { "Fac_scores.csv" },
    content = function(file) {
      write.csv(data.frame(rownames(filtered_dataset()), (fit())$scores), file, row.names = F)
   				 }
	)

output$downloadData <- downloadHandler(
  filename = function() { "Big_five_Survey_data.csv" },
  content = function(file) {
    write.csv(read.csv("data/Big_five_Survey_data.csv"), file, row.names=F, col.names=F)
  }
)
  
})

