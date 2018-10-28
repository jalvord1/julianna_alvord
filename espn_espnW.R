library(twitteR)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(splitstackshape)

consumer_key <- 'XXX'
consumer_secret <- 'XXX'
access_token <- 'XXX'
access_secret <- 'XXX'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#getting list of latest 1000 tweets from ESPN twitter
espn_tweets <- userTimeline("espn", n = 1000)

#creating df from list of tweets
espn_tweets_df <- twListToDF(espn_tweets)

#getting df of individual words from tweets
espn_tweet_words <- espn_tweets_df %>% select(id, text) %>% unnest_tokens(word,text)


#getting rid of stop words
my_stop_words <- stop_words %>% 
  select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt")))

#removing stop words from list of ESPN words
espn_tweet_words_interesting <- espn_tweet_words %>% 
  anti_join(my_stop_words)

#top words
espn_tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")


##----------------------------------------------------------------
#now doing the same for ESPNW
espnW_tweets <- userTimeline("espnW", n = 1000)

#creating df from list of tweets
espnW_tweets_df <- twListToDF(espnW_tweets)

#getting df of individual words from tweets
espnW_tweet_words <- espnW_tweets_df %>% 
  select(id, text) %>% 
  unnest_tokens(word,text)

#removing stop words from list of ESPN words
espnW_tweet_words_interesting <- espnW_tweet_words %>% 
  anti_join(my_stop_words)

#top words
espnW_tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")



#putting together the interesting words df to do analyses

par(mfrow=c(2,2))


espnW_tweet_words_interesting <- espnW_tweet_words_interesting %>%
  mutate(gender = "espnW")

full_tweet_words_interesting <- espn_tweet_words_interesting %>%
  mutate(gender = "espn") %>%
  bind_rows(espnW_tweet_words_interesting)

full_tweet_words_interesting2 <- full_tweet_words_interesting %>%
  group_by(gender) %>%
  summarise(n_word = tally(word))

words <- data.frame(table(full_tweet_words_interesting$word, full_tweet_words_interesting$gender))

#plot of top words
full_tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + facet_wrap('gender', ncol = 2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")



#boy v girl language
boy <- espn_tweet_words %>%
  mutate(boy = ifelse(word %in% c("boy", "Boy"), 1, 0))

girl <- espnW_tweet_words %>%
  mutate(girl = ifelse(word %in% c("girl", "Girl"), 1, 0))

