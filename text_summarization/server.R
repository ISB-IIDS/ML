if (!require(shiny)) {install.packages("shiny")}
if (!require(dplyr)) {install.packages("dplyr")}
if (!require(tidytext)) {install.packages("tidytext")}
if (!require(textrank)) {install.packages("textrank")}
if (!require(ggplot2)) {install.packages("ggplot2")}
if (!require(pdftools)) {install.packages("pdftools")}
if (!require(stringr)) {install.packages("stringr")}

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(pdftools)



# Define server logic required to draw a histogram
shinyServer( function(input, output) {
    
    pdftotxt <- reactive({
        if (is.null(input$filep)) {return(NULL)}
        txt_output <- pdftools::pdf_text(input$filep$datapath) %>%
            paste(sep = " ") %>%
            stringr::str_replace_all(fixed("\n"), " ") %>%
            stringr::str_replace_all(fixed("\r"), " ") %>%
            stringr::str_replace_all(fixed("\t"), " ") %>%
            stringr::str_replace_all(fixed("\""), " ") %>%
            paste(sep = " ", collapse = " ") %>%
            stringr::str_squish() %>%
            stringr::str_replace_all("- ", "") 
        # inspect
        return(txt_output)  
    })

    dataset <- reactive({ 
    if (is.null(input$file)){data=pdftotxt()}
    else (data=readLines(input$file$datapath))
    return(data)
        })
    
    #num_sents <- reactive({ if (is.null(input$num)) {return(5)} })
    #num_sents <- input$num
    
        require(dplyr)
        require(magrittr)
        require(tidytext)
        require(ggplot2)
        
    ## load data into tibble
    article_sentences <- reactive({
            article_sentences = tibble(text = dataset()) %>%
            unnest_tokens(sentence, text, token = "sentences", to_lower=FALSE) %>%    # sentence-tokenizing the article   
            mutate(sentence_id = row_number()) %>%    # insert setence_id
            select(sentence_id, sentence)  # drop frivolous stuff

        return(article_sentences)
        })
    
    output$article_sentences <- renderTable({ 
        if (is.null(input$filep)) {if (is.null(input$file)) {return(NULL)}}
        article_sentences() })
    
    output$num_sent <- renderText({ 
        if (is.null(input$filep)) {if (is.null(input$file)) {return(NULL)}}
        str1 = as.character(summary(article_sentences())[[7]])
        str1   })
    
   
     article_summary <- reactive({ 
                article_words = article_sentences() %>%
                unnest_tokens(word, sentence) %>%
                anti_join(stop_words, by = "word")
                textrank_sentences(data = article_sentences(), terminology = article_words)
                }) 

    output$output1 <- renderTable({
        if (is.null(input$filep)) {if (is.null(input$file)) {return(NULL)}}
        # ## word-tokenize too. for IDing keywords
        # article_words = article_sentences() %>%
        #     unnest_tokens(word, sentence) %>%
        #     # drop stopwords
        #     anti_join(stop_words, by = "word")
        # ## print summary
        # article_summary <- textrank_sentences(data = article_sentences(), 
        #                                       terminology = article_words)
            
#        a0 = data.frame(article_summary$sentences)
#        a1 = order(a0$textrank, decreasing=TRUE)
#        summ_sents = a0$sentence[a1[1:input$num]] # %>% tibble()
        
        summ_sents = article_summary()[["sentences"]] %>%
            arrange(desc(textrank)) %>% 
            slice(1:input$num) %>%  # dplyr::slice() chooses rows by their ordinal position in the tbl
            pull(sentence) %>% tibble()
        
        return(summ_sents)
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function() { "exampletext.txt" },
        content = function(file) {
            writeLines(readLines("data/exampletext.txt"), file)
        }
       )
    
    output$downloadData1 <- downloadHandler(
        filename = function() { "textfile.txt" },
        content = function(file) {
            writeLines(dataset(), file)
        }
    )
    
    output$output2 <- renderPlot({  
        if (is.null(input$filep)) {if (is.null(input$file)) {return(NULL)}}
        # ## word-tokenize too. for IDing keywords
        # article_words = article_sentences() %>%
        #     unnest_tokens(word, sentence) %>%
        #     # drop stopwords
        #     anti_join(stop_words, by = "word")
        # article_summary <- textrank_sentences(data = article_sentences(), 
        #                                       terminology = article_words) 
    
#        output$output1 <- renderTable({ output1() })
            
        # top k sentences ka plot
#        output2 <- reactive({
        
#        output$output2 <- renderPlot({            
            
            article_summary()[["sentences"]] %>%
            
            ggplot(aes(textrank_id, textrank, fill = textrank_id)) +
            
            geom_col() +
            theme_minimal() +
            scale_fill_viridis_c() +
            guides(fill = "none") +
            
            labs(x = "Sentence",
                 y = "TextRank score",
                 title = "Where do the most informative sentences appear in the article")
        
        })
        

}) # shinyServer func ends
