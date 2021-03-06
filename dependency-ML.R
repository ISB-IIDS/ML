try(require("shiny") || install.packages("shiny"))
try(require("devtools")||install.packages("devtools"))
try(require("magick")||install.packages("magick"))
try(require("text2vec") || install.packages("text2vec"))
try(require("tm") || install.packages("tm"))
try(require("tokenizers") || install.packages("tokenizers"))
try(require("wordcloud") || install.packages("wordcloud"))
try(require("slam") || install.packages("slam"))
try(require("stringi") || install.packages("stringi"))
try(require("tidytext") || install.packages("tidytext"))
try(require("tidyr") || install.packages("tidyr"))
try(require("igraph")|| install.packages("igraph"))
try(require("visNetwork")|| install.packages('visNetwork'))
try(require("SnowballC")||install.packages("SnowballC"))
try(require("psych")||install.packages("psych"))
try(require("DBI")||install.packages("DBI"))
try(require("assertthat")||install.packages("assertthat"))
try(require("Rcpp")||install.packages("Rcpp"))
try(require("mnormt")||install.packages("mnormt"))
try(require("ggplot2")||install.packages("ggplot2"))
try(require("DT")||install.packages("DT"))
try(require("reshape2")||install.packages("reshape2"))
try(require("wordcloud")||install.packages("wordcloud"))
try(require("plotly")||install.packages("plotly"))
try(require("magrittr")||install.packages("magrittr"))
try(require("caret")||install.packages("caret"))
try(require("rpart")||install.packages("rpart"))
try(require("rpart.plot")||install.packages("rpart.plot"))
try(require("randomForest")||install.packages("randomForest"))
try(require("hydroGOF")||install.packages("hydroGOF"))
try(require("pastecs")||install.packages("pastecs"))
try(require("dplyr")||install.packages("dplyr"))
try(require("Hmisc")||install.packages("Hmisc"))
try(require("party")||install.packages("party"))
try(require("partykit")||install.packages("partykit"))
try(require("fmsb")||install.packages("fmsb"))
try(require("PerformanceAnalytics")||install.packages("PerformanceAnalytics"))
try(require("sparkline")||install.packages("sparkline"))

if(!require("plot3D")) {install.packages("plot3D")}
if(!require("ROCR")) {install.packages("ROCR")}
if(!require("Spectrum")) {install.packages("Spectrum")}
if(!require("textrank")) {install.packages("textrank")}
if(!require("Rtsne")) {install.packages("Rtsne")}
if(!require("randomcoloR")) {install.packages("randomcoloR")}
if(!require("shifnyWidgets")) {install.packages("shinyWidgets")}
if(!require("stringr")) {install.packages("stringr")}
if(!require("e1071")) {install.packages("e1071")}
if(!require("EnvStats")) {install.packages("EnvStats")}
if(!require("fastDummies")) {install.packages("fastDummies")}
if(!require("maptpx")) {install.packages("maptpx")}
if(!require("nFactors")) {install.packages("nFactors")}
if(!require("qgraph")) {install.packages("qgraph")}
if(!require("corrplot")) {install.packages("corrplot")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("rfm")) {install.packages("rfm")}
if(!require("lubridate")) {install.packages("lubridate")}
#if(!require("ggbiplot")) {install_github("vqv/ggbiplot")}
if(!require("plyr")) {install.packages("plyr")}
if(!require("scales")) {install.packages("scales")}
if(!require("grid")) {install.packages("grid")}
if(!require("ggpubr")) {install.packages("ggpubr")}
if(!require("cluster")) {install.packages("cluster")}
if(!require("mclust")) {install.packages("mclust")}
if(!require("MASS")) {install.packages("MASS")}
if(!require("mda")) {install.packages("mda")}
#if(!require("klaR")) {install.packages("klaR")}
if (!require("plyr")) {install.packages("plyr")}
if (!require("grid")) {install.packages("grid")}
if (!require("scales")) {install.packages("scales")}
if(!require("gridExtra")) {install.packages("gridExtra")}
if(!require("tesseract")) {install.packages("tesseract")}


warnings()