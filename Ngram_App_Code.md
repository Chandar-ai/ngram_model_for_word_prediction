---
title: "Ngram_App_Code"
author: "Chandar"
date: "7/4/2021"
output: html_document
---

## Shiny App  
#' This R script will create a Shiny App in which a word or phrase can be given as an input in a text box
#' and a predicted next word will appear as an output. 


```{r}
library(shiny)
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})
```

### Source of ngram matching function for ShinyApp
```{r}
source("./ngram_model_for_word_prediction/Ngram_Word_Prediction_Function_For_ShinyApp.R")
```

### Define UI for application that takes a word or phrase as an input and shows the predicted next word as an output
```{r}
ui <- fluidPage(
  
  # Application title
  titlePanel("Word Prediction Model"),
  p("This app will read an input phrase or incomplete sentense that we type in a text box and will show the predicted next word as an output."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h2("Instructions:"), 
      h5("1. Enter one or more than one word in the given text box"),
      h5("2. Immediately, the next word predicted by the model will appear below in red color"),
      h5("3. If there is no prediction or misspelling of input words then #No Prediction# will appear"),
      h5("4. Additional tabs display barplots of the top ngrams (Frequency Distribution of Words) in the dataset"),
      br(),
      a("Source Code", href = "https://github.com/Chandar-ai/ngram_model_for_word_prediction")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel( 
      tabsetPanel(
        tabPanel("Prediction of next word",
                 textInput("user_input", h3("Type your words in the below box:"), 
                           value = ""),
                 h3("Predicted Next Word:"),
                 h4(em(span(textOutput("ngram_output"), style="color:red")))),
        
        tabPanel("top quadgrams",
                 # br(),
                 imageOutput("barplot4")),
        
        tabPanel("top trigrams",
                 # br(),
                 imageOutput("barplot3")),
        
        tabPanel("top bigrams",
                 # br(),
                 imageOutput("barplot2"))
        
      )   
    )
  )
)
```


### Define Server logic to predict next word and display barplots for each ngram word distribution 
```{r}
toptable4 = head(na.omit(readRDS("./ngram_model_for_word_prediction/ngram_app_data/four_words_fast.rds")))
toptable3 = head(na.omit(readRDS("./ngram_model_for_word_prediction/ngram_app_data/three_words_fast.rds")))
toptable2 = head(na.omit(readRDS("./ngram_model_for_word_prediction/ngram_app_data/two_words_fast.rds")))
```


```{r}
server <- function(input, output, session) {
  
  output$ngram_output <- renderText({
    ngrams(input$user_input)
  })
  
  output$barplot4 <- renderImage({
    outfile4 <- tempfile(fileext = '.png')
    png(outfile4, width = 800, height = 500)
    table4 = toptable4
    table4$xvar = paste(toptable4$word1, toptable4$word2, toptable4$word3, toptable4$word4)
    barplot(table4$n, names=(table4$xvar), col='darkgreen')
    dev.off()
    
    list(src = outfile4)
    # alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$barplot3 <- renderImage({
    outfile3 <- tempfile(fileext = '.png')
    png(outfile3, width = 800, height = 500)
    table3 = toptable3
    table3$xvar = paste(toptable3$word1, toptable3$word2, toptable3$word3)
    barplot(table3$n, names=(table3$xvar), col='brown')
    dev.off()
    
    list(src = outfile3,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$barplot2 <- renderImage({
    outfile2 <- tempfile(fileext = '.png')
    png(outfile2, width = 800, height = 500)
    table2 = toptable2
    table2$xvar = paste(toptable2$word1, toptable2$word2)
    barplot(table2$n, names=(table2$xvar), col='pink')
    dev.off()
    
    list(src = outfile2,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
}
```

### Run the application 
```{r}
shinyApp(ui = ui, server = server)

```

