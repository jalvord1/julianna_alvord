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

###running example code
fn_twitter <- searchTwitter("#Houston",n=1000,lang="en")

fn_twitter_df <- twListToDF(fn_twitter) # Convert to data frame


tweet_words <- fn_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)
#EACH ROW IS DIFFERENT WORD

#PLOTTING MOST COMMON WORDS
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                      hjust = 1)) + xlab("")
#adding in sentiment analysis info
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)
#REMOVE STOP WORDS

#GRAPHING ONLY INTERESTING WORDS
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

#looking at sentiment of words 
bing_lex <- get_sentiments("nrc")

fn_sentiment <- tweet_words_interesting %>% left_join(bing_lex)

fn_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

###-------------------------------------------------------------------------------------





###trying my own now  -- NFL draft
draft_twitter <- searchTwitter("NFLDraft",n=5000,lang="en")

draft_twitter_df <- twListToDF(draft_twitter)

tweet_words <- draft_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

#plotting top words
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))


#top words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)

tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))


#mentions of Barkley/mayfield?
# tweet_words <- tweet_words %>%
#   mutate(barkley = ifelse(word == "saquon" | word == "barkley" | word == "Saquon" | word == "Barkley" 
#                          | word == "saquonbarkley" | word == "SaquonBarkley", 1, 0),
#          mayfield = ifelse(word == "baker" | word == "mayfield" | word == "Baker" | word == "Mayfield" |
#                              word == "bakermayfield" | word == "BakerMayfield", 1, 0))

# draft_twitter_df <- draft_twitter_df %>%
#   mutate(barkley = ifelse(text %in% "saquon" | text %in% "barkley" | text %in% "Saquon" | text %in% "Barkley" 
#                           | text %in% "saquonbarkley" | text %in% "SaquonBarkley", 1, 0),
#          mayfield = ifelse(text %in% "baker" | text %in% "mayfield" | text %in% "Baker" | text %in% "Mayfield" |
#                              text %in% "bakermayfield" | text %in% "BakerMayfield", 1, 0))

draft_twit_sep<- cSplit(draft_twitter_df, "text", sep = " ")

draft_twitter_df <- draft_twitter_df %>%
  mutate(barkley = ifelse(grepl("Barkley", text, fixed = TRUE)|grepl("Saquon", text, fixed = TRUE)|
                            grepl("barkley", text, fixed = TRUE)|grepl("saquon", text, fixed = TRUE)|
                            grepl("SaquonBarkley", text, fixed = TRUE)|grepl("saquonbarkley", text, fixed = TRUE), 1, 0),
         mayfield = ifelse(grepl("Mayfield", text, fixed = TRUE)|grepl("Baker", text, fixed = TRUE)|
                             grepl("mayfield", text, fixed = TRUE)|grepl("baker", text, fixed = TRUE)|
                             grepl("BakerMayfield", text, fixed = TRUE)|grepl("bakermayfield", text, fixed = TRUE), 1, 0))

barkley_or_mayfield <- draft_twitter_df %>%
  filter(barkley == 1|mayfield == 1)
#NOT ENOUGH INFO (only 24 tweets)


###------------------------------------------------------------------------------------------------------


#trying for saquon or mayfield
###SAQUON
saquon_twitter <- searchTwitter("@saquon",n=10000,lang="en")

saquon_twitter_df <- twListToDF(saquon_twitter)

tweet_words_barkley <- saquon_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

#plotting top words
tweet_words_barkley %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

#top words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting_barkley <- tweet_words_barkley %>% anti_join(my_stop_words)

tweet_words_interesting_barkley %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

bing_lex <- get_sentiments("nrc")

fn_sentiment_barkley <- tweet_words_interesting_barkley %>% left_join(bing_lex)

#how many of each sentiment?
sentiment_barkley_df <- fn_sentiment_barkley %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

sentiment_barkley_df <- sentiment_barkley_df %>%
  mutate(player = "Barkley")

###MAYFIELD
#trying for saquon or mayfield
baker_twitter <- searchTwitter("@bakermayfield",n=5000,lang="en")

baker_twitter_df <- twListToDF(baker_twitter)

tweet_words_mayfield <- baker_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

#plotting top words
tweet_words_mayfield %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

#top words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting_mayfield <- tweet_words_mayfield %>% anti_join(my_stop_words)

tweet_words_interesting_mayfield %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

bing_lex <- get_sentiments("nrc")

fn_sentiment_mayfield <- tweet_words_interesting_mayfield %>% left_join(bing_lex)

sentiment_mayfield <- fn_sentiment_mayfield %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

sentiment_mayfield <- sentiment_mayfield %>%
  mutate(player = "Mayfield")


##comparing sentiment of barkley v mayfield

#sentiment_bind <- bind_cols(sentiment_barkley_df, sentiment_mayfield)

sentiment <- sentiment_barkley_df %>%
  full_join(sentiment_mayfield, by = "sentiment")

sentiment <- sentiment %>%
  gather(c(player.x, player.y, n.x, n.y), key = "key", value = "value") %>%
  filter(!(key == "player.x"|key == "player.y")) %>%
  mutate(player = ifelse(key == "n.x", "barkley", "mayfield"))

sentiment$value <- as.numeric(sentiment$value)

ggplot(sentiment, aes(x = sentiment, y = value, fill = player)) + geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 60))

#not what I expected, but he's also performing better (see code below)

#bringing in pbp data
pbp_2018 <- read.csv("pbp-2018.csv")

#filtering for mayfield or barkley
pbp_2018_filtered <- pbp_2018 %>%
  mutate(barkley = ifelse(grepl("BARKLEY", Description, fixed = TRUE), 1, 0),
         mayfield = ifelse(grepl("MAYFIELD", Description, fixed = TRUE), 1, 0)) %>%
  filter(barkley == 1|mayfield == 1) %>%
  mutate(player = ifelse(barkley == 1, "barkley",
                         ifelse(mayfield == 1, "mayfield", "both")))


#how successful are they?
pbp_yards <- pbp_2018_filtered %>%
  group_by(player) %>%
  summarise(mean_yards = mean(Yards),
            median_yards = median(Yards),
            min_yards = min(Yards),
            max_yards = max(Yards))

View(pbp_yards)

#plotting distribution of yards
ggplot(pbp_2018_filtered, aes(Yards, fill = player)) + geom_density(alpha = 0.6)
#similar distributions



### LAMAR JACKSON
jackson_twitter <- searchTwitter("@Lj_era8",n=10000,lang="en")

jackson_twitter_df <- twListToDF(jackson_twitter)

tweet_words_jackson <- jackson_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

#plotting top words
tweet_words_jackson %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

#top words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting_jackson <- tweet_words_jackson %>% anti_join(my_stop_words)

tweet_words_interesting_jackson %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

bing_lex <- get_sentiments("nrc")

fn_sentiment_jackson <- tweet_words_interesting_jackson %>% left_join(bing_lex)

sentiment_jackson <- fn_sentiment_jackson %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

sentiment_jackson <- sentiment_jackson %>%
  mutate(player = "jackson")


#sentiment of jackson v mayfield
sentiment <- sentiment_jackson %>%
  full_join(sentiment_mayfield, by = "sentiment")

sentiment <- sentiment %>%
  gather(c(player.x, player.y, n.x, n.y), key = "key", value = "value") %>%
  filter(!(key == "player.x"|key == "player.y")) %>%
  mutate(player = ifelse(key == "n.x", "jackson", "mayfield"))

sentiment$value <- as.numeric(sentiment$value)

ggplot(sentiment, aes(x = sentiment, y = value, fill = player)) + geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 60))



#how are they performing? Jackson v mayfield
#bringing in pbp data
pbp_2018 <- read.csv("pbp-2018.csv")

#filtering for mayfield or jackson
pbp_2018_filtered <- pbp_2018 %>%
  mutate(jackson = ifelse(grepl("JACKSON", Description, fixed = TRUE), 1, 0),
         mayfield = ifelse(grepl("MAYFIELD", Description, fixed = TRUE), 1, 0)) %>%
  filter(jackson == 1|mayfield == 1) %>%
  mutate(player = ifelse(jackson == 1, "jackson",
                         ifelse(mayfield == 1, "mayfield", "both")))


#how successful are they?
pbp_yards <- pbp_2018_filtered %>%
  group_by(player) %>%
  summarise(mean_yards = mean(Yards),
            median_yards = median(Yards),
            min_yards = min(Yards),
            max_yards = max(Yards),
            n_plays = n())

View(pbp_yards)

#plotting distribution of yards
ggplot(pbp_2018_filtered, aes(Yards, fill = player)) + geom_density(alpha = 0.6)
#similar distributions

#does this say anything of play of falcons v browns?



###Christian Mccaffrey
mccaffrey_twitter <- searchTwitter("@run__cmc",n=10000,lang="en")

mccaffrey_twitter_df <- twListToDF(mccaffrey_twitter)

tweet_words_mccaffrey <- mccaffrey_twitter_df %>% select(id, text) %>% unnest_tokens(word,text)

#top words
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("https", "t.co", "rt", "amp","4yig9gzh5t","fyy2ceydhi","78","fakenews")))

tweet_words_interesting_mccaffrey <- tweet_words_mccaffrey %>% anti_join(my_stop_words)

tweet_words_interesting_mccaffrey %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60))

bing_lex <- get_sentiments("nrc")

fn_sentiment_mccaffrey <- tweet_words_interesting_mccaffrey %>% left_join(bing_lex)

sentiment_mccaffrey <- fn_sentiment_mccaffrey %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

sentiment_mccaffrey <- sentiment_mccaffrey %>%
  mutate(player = "jackson")
