
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


# Link to download the SwiftKey dataset: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

# Downloading the SwiftKey dataset from the given URL 

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url, "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip", exdir = "Coursera-SwiftKey")
}



### Reading three text files (blogs, news and twitter) and calculate basic summary statistics


path <- "C:/Users/admin/Documents/final/en_US/"

# Blogs summary
con1 <- file(paste(path, "en_US.blogs.txt", sep=""), open="r")
blogs <- readLines(con1, warn=FALSE)
close(con1)

# Twitter summary
con2 <- file(paste(path, "en_US.twitter.txt", sep=""), open="r")
twitter <- readLines(con2, warn=FALSE)
close(con2)

# News summary
con3 <- file(paste(path, "en_US.news.txt", sep=""), open="r")
news <- readLines(con3, warn = FALSE)
close(con3)



#' Create dataframes
blogs   <- data.frame(text = blogs)
news    <- data.frame(text = news)
twitter <- data.frame(text = twitter)


## Sample the data
## DataSampling
set.seed(1234)
sample_pct <- 0.15


blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)

news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)

twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)


# Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

# 
# mycorpus <- VCorpus(VectorSource(repo_sample))
# mycorpus <- tm_map(mycorpus, removePunctuation)
# mycorpus <- tm_map(mycorpus, removeNumbers)
# mycorpus <- tm_map(mycorpus, stripWhitespace)
# mycorpus <- tm_map(mycorpus, content_transformer(tolower))
# mycorpus <- tm_map(mycorpus, PlainTextDocument)
# mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
# 
# 
# mycorpus_df<-data.frame(text=unlist(sapply(mycorpus, `[`, "content")), 
#                         stringsAsFactors=F)
# 

# Clean up
rm(list = c("blogs", "blogs_sample","news",     
            "news_sample", "sample_pct", "twitter", "twitter_sample"))

# Clean the sample data
# Create filters: non-alphanumeric's, url's, repeated letters(+3x)
# Data Cleaning
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b" 



# Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
# and ngrams.

# clean_sample <-  mycorpus_df %>%
  
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

# clean_sample <- gsub("?", "", clean_sample)



rm(list = c("repo_sample"))


## Create all n-grams
# Ngrams 
#Bigrams
twogram_repo <- clean_sample  %>%
  unnest_tokens(twogram, text, token = "ngrams", n = 2)

# Trigrams
threegram_repo <- clean_sample  %>%
  unnest_tokens(threegram, text, token = "ngrams", n = 3)

# Quadgrams
fourgram_repo <- clean_sample  %>%
  unnest_tokens(fourgram, text, token = "ngrams", n = 4)

# Quintgrams
fivegram_repo <- clean_sample  %>%
  unnest_tokens(fivegram, text, token = "ngrams", n = 5)

# Sixtgrams
sixgram_repo <- clean_sample  %>%
  unnest_tokens(sixgram, text, token = "ngrams", n = 6)



## Reduce n-grams files
# ReduceNgrams 
# twograms
twogram_cover <- twogram_repo %>%
  count(twogram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("twogram_repo"))

# threegrams
threegram_cover <- threegram_repo %>%
  count(threegram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("threegram_repo"))

# fourgrams
fourgram_cover <- fourgram_repo %>%
  count(fourgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("fourgram_repo"))

# fivegrams
fivegram_cover <- fivegram_repo %>%
  count(fivegram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("fivegram_repo"))

# sixgrams
sixgram_cover <- sixgram_repo %>%
  count(sixgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("sixgram_repo"))



## What does the distribution on ngrams look like?
#+ DistyPlot
disty <- data_frame(ngram = c(rep("twograms",  nrow(twogram_cover)),
                              rep("threegrams",  nrow(threegram_cover)),
                              rep("fourgrams", nrow(fourgram_cover)),
                              rep("fivegrams", nrow(fivegram_cover)),
                              rep("sixgrams",  nrow(sixgram_cover))),
                    number = c(twogram_cover$n,  threegram_cover$n, 
                               fourgram_cover$n, fivegram_cover$n,
                               sixgram_cover$n))
disty

disty$ngram <- as.factor(disty$ngram)
ggplot(data = disty, aes(y = number, x = reorder(ngram, -number), fill = "darkblue")) +
  geom_boxplot(outlier.color = "blue") + scale_y_log10() +
  xlab("ngram")
ggsave("./ngram_match/www/ngrams.png")

sixgram_cover %>%
  top_n(15, n) %>%
  mutate(sixgram = reorder(sixgram, n)) %>%
  ggplot(aes(sixgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("sixgrams")
ggsave("./ngram_match/www/sixgrams.png")

fivegram_cover %>%
  top_n(15, n) %>%
  mutate(fivegram = reorder(fivegram, n)) %>%
  ggplot(aes(fivegram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("fivegrams")
ggsave("./ngram_match/www/fivegrams.png")

fourgram_cover %>%
  top_n(15, n) %>%
  mutate(fourgram = reorder(fourgram, n)) %>%
  ggplot(aes(fourgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("fourgrams")
ggsave("./ngram_match/www/fourgrams.png")

threegram_cover %>%
  top_n(15, n) %>%
  mutate(threegram = reorder(threegram, n)) %>%
  ggplot(aes(threegram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("threegrams")
ggsave("./ngram_match/www/threegrams.png")

twogram_cover %>%
  top_n(15, n) %>%
  mutate(twogram = reorder(twogram, n)) %>%
  ggplot(aes(twogram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("twograms")
ggsave("./ngram_match/www/twograms.png")


#' ## Separate words
#+ NgramWords 
two_words <- twogram_cover %>%
  separate(twogram, c("word1", "word2"), sep = " ")
head(two_words)

three_words <- threegram_cover %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ")
head(three_words)

four_words <- fourgram_cover %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ")
head(four_words)

five_words <- fivegram_cover %>%
  separate(fivegram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
head(five_words)

six_words <- sixgram_cover %>%
  separate(sixgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
head(six_words)


#' Save data for the Shiny App
saveRDS(two_words, "E:/Coursera/Coursera-SwiftKey/two_words_fast.rds")
saveRDS(three_words, "E:/Coursera/Coursera-SwiftKey/three_words_fast.rds")
saveRDS(four_words,"E:/Coursera/Coursera-SwiftKey/four_words_fast.rds")
saveRDS(five_words,"E:/Coursera/Coursera-SwiftKey/five_words_fast.rds")
saveRDS(six_words,"E:/Coursera/Coursera-SwiftKey/six_words_fast.rds")


#################


n2_words <- readRDS("E:/Coursera/Coursera-SwiftKey/two_words_fast.rds")
n3_words  <- readRDS("E:/Coursera/Coursera-SwiftKey/three_words_fast.rds")
n4_words <- readRDS("E:/Coursera/Coursera-SwiftKey/four_words_fast.rds")
n5_words <- readRDS("E:/Coursera/Coursera-SwiftKey/five_words_fast.rds")
n6_words <- readRDS("E:/Coursera/Coursera-SwiftKey/six_words_fast.rds")


# Create Ngram Matching Functions

bigram <- function(input_words){
  num <- length(input_words)
  filter(n2_words, 
         word1==input_words[num]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(n3_words, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(n4_words, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}
# 
# 
# quintgram <- function(input_words){
#   num <- length(input_words)
#   filter(n5_words,
#          word1==input_words[num-3],
#          word2==input_words[num-2],
#          word3==input_words[num-1],
#          word4==input_words[num])  %>%
#     top_n(1, n) %>%
#     filter(row_number() == 1L) %>%
#     select(num_range("word", 5)) %>%
#     as.character() -> out
#   ifelse(out=="character(0)", quadgram(input_words), return(out))
# }
# 
# 

# sextgram <- function(input_words){
#   num <- length(input_words)
#   filter(n6_words, 
#          word1==input_words[num-4], 
#          word2==input_words[num-3], 
#          word3==input_words[num-2],
#          word4==input_words[num-1],
#          word5==input_words[num])  %>% 
#     top_n(1, n) %>%
#     filter(row_number() == 1L) %>%
#     select(num_range("word", 6)) %>%
#     as.character() -> out
#   ifelse(out=="character(0)", quintgram(input_words), return(out))
# }


# Create User Input and Data Cleaning Function; Calls the matching functions

ngrams <- function(input){
  # Create a dataframe
  input <- data_frame(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count == 1, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
  # Output
  return(out)
}
