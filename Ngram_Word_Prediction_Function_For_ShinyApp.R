

# Reading Required Libraries 

library(ngram)
library(NLP)
library(tm)
library(corpus)
library(ggplot2)
library(stringi)
library(slam)
# install.packages("remotes")
library(remotes)
remotes::install_github("news-r/textblob")

library(textblob)
library(RWeka)
library(LSD) 
library(dplyr)
library(stringr)

library(tidytext)
library(tidyverse)
library(knitr)
library(png)

library(dplyr)


# Load Training Data, which alread created 

n2_words <- readRDS("https://github.com/Chandar-ai/ngram_model_for_word_prediction/tree/main/ngram_app_data/two_words_fast.rds")
n3_words  <- readRDS("https://github.com/Chandar-ai/ngram_model_for_word_prediction/tree/main/ngram_app_data/three_words_fast.rds")
n4_words <- readRDS("https://github.com/Chandar-ai/ngram_model_for_word_prediction/tree/main/ngram_app_data/four_words_fast.rds")


# Creation of Ngram Matching Functions

# BIGRAM Matching Function 
# Finding the frequency distribution of pairs of words, using which, the 2nd word with high frequency is selected. 
bigram <- function(input_words){
  n2_words = na.omit(n2_words)
  num <- length(input_words)
  w1 = n2_words[n2_words$word1 %in% input_words[num],]
  top = top_n(w1, 1, n)
  top1 = filter(top, row_number() == 1L) 
  next_word = select(top1, num_range("word", 2))
  out = as.character(next_word)
  res = ifelse(out =="character(0)", "#No Prediction#", out)
  return(res)
}


# TRIGRAM Matching Function 
# Finding the frequency distribution of three set of words (or triplet), using which, the 3rd word with high frequency is selected 
trigram <- function(input_words){
  n3_words = na.omit(n3_words)
  num <- wordcount(input_words)
  w1 = n3_words[n3_words$word1 %in% word(input_words,num-1),]
  w2 = w1[w1$word2 %in% word(input_words,num),]
  top = top_n(w2, 1, n)
  top1 = filter(top, row_number() == 1L) 
  next_word = select(top1, num_range("word", 3))
  out = as.character(next_word)
  res = ifelse(out =="character(0)", bigram(input_words), out)
  return(res)
}


# QUADGRAM Matching Function 
# Finding the frequency distribution of four set of words (or quads), using which, the 4th word with high frequency is selected.
quadgram <- function(input_words){
  n4_words = na.omit(n4_words)
  num <- wordcount(input_words)
  w1 = n4_words[n4_words$word1 %in% word(input_words,num-2),]
  w2 = w1[w1$word2 %in% word(input_words,num-1),]
  w3 = w2[w2$word3 %in% word(input_words,num),]
  
  top = top_n(w3, 1, n)
  top1 = filter(top, row_number() == 1L) 
  next_word = select(top1, num_range("word", 4))
  out = as.character(next_word)
  res = ifelse(out =="character(0)", trigram(input_words), out)
  return(res)
}


# Creatiion of User Input and Data Cleaning Function
# This 'ngrams' function will call the matching functions which we have defined above.

ngrams <- function(input){
  # Creation of a dataframe
  input <- data_frame(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Finding word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Calling the matching functions
  out <- ifelse(input_count == 0, "#No Prediction#",
                ifelse(input_count == 3, quadgram(input_words),
                       ifelse(input_count == 2, trigram(input_words), bigram(input_words))))
  # Output
  return(out)
}

