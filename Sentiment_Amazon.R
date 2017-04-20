library(data.table)
setwd("C:/Data Science/Spring Semester-2017/Advanced BA/R-Codes")

data = fread("amazon_phone_filtered.csv",strip.white=T, sep=",", header=T, na.strings=c(""," ", "NA","nan", "NaN", "nannan"))
data$review_id <- seq.int(nrow(data))
head(data, n=5)

#text <- c("Because I could not stop for Death -",
   #       "He kindly stopped for me -",
  #        "The Carriage held but just Ourselves -",
  #        "and Immortality")
#text

library(dplyr)
#text_df <- data_frame(line = 1:4, text = text)
#head(text_df)
#idytext's unnest_tokens() function
library(tidytext)
#text_df %>%
  #unnest_tokens(word, text) # This means that in data frame text_df, tokenize column "text" by each word (standard tokenization).
#we can start processing our original Twitter data:
tidy_text <- data %>%
  unnest_tokens(word, Reviews_text)
tidy_text[1:20]

#remove stop-words from our data. We can do this using anti_join function
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

#dplyr's count() to find the most common words 
tidy_text %>%
  count(word, sort = TRUE)

#create a visualization of the most common words
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#tf-idf

review_words <- data %>%
  unnest_tokens(word, Reviews_text) %>%
  count(Brand_Name, word, sort = TRUE) %>% # It will count the number of words per Brand Name
  ungroup()
head(review_words)
total_words <- review_words %>% 
  group_by(Brand_Name) %>% 
  summarize(total = sum(n)) # Counts the total number of words used by each Brand
head(total_words)


review_words <- left_join(review_words, total_words) 
head(review_words)

review_words <- review_words %>%
  bind_tf_idf(word, Brand_Name, n)
head(review_words)

#Sentiment Analysis:
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#'s use the NRC lexicon and filter() for the joy words
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_text %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
head(sentiment)

#sentiment per tweet, we need to filter our data using the index
sentiment <- tidy_text %>%
  filter(review_id == 2) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
head(sentiment)

#We can also filter by each Brand_Name:

sentiment <- tidy_text %>%
  filter(Brand_Name == "apple") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 
head(sentiment)

#As there are 50k tweets, we can use package "parallel" to perform the sentiment analysis faster
library("syuzhet")
library("parallel")
#tweets = head(data, n= 1000)
(no_cores <- detectCores() - 1) # Calculate the number of cores in your machine

cl <- makeCluster(no_cores) # Initiate the cluster
clusterEvalQ(cl, library("syuzhet")) # Load library "syuzhet" on the cluster
clusterEvalQ(cl, library("plyr")) # Load library "plyr" on the cluster

data$syuzhet = parLapply(cl, data$Reviews_text, function(x) get_sentiment(toString(x), method="syuzhet")) # Use syuzhet lexicon for sentiment analysis
data$syuzhet <- as.numeric(unlist(data$syuzhet)) # Convert the sentiment scores to numeric
data$bing = parLapply(cl, data$Reviews_text, function(x) get_sentiment(toString(x), method="bing")) # Use bing lexicon for sentiment analysis
data$bing <- as.numeric(unlist(data$bing))
data$nrc = parLapply(cl, data$Reviews_text, function(x) get_sentiment(toString(x), method="nrc"))
data$nrc <- as.numeric(unlist(data$nrc))
data$afinn = parLapply(cl, data$Reviews_text, function(x) get_sentiment(toString(x), method="afinn"))
data$afinn <- as.numeric(unlist(data$afinn))
data$mean_sentiment <- rowMeans(subset(data, select = c(syuzhet, bing, nrc, afinn)), na.rm = TRUE) # Take the average of all four methods as the final sentiment score
data <- review[order(data$mean_sentiment),] # Order teh data based on lowest average sentiment to the highest
data$x <- rescale_x_2(data$mean_sentiment)$x # Re-scale the sentiment scores to range from 0 to 1. 
stopCluster(cl) # Shutdown the cluster
head(data)

###WRITE TO CSV

write.csv(data, file = "data with sentiments.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
