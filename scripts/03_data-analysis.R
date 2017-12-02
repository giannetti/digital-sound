# Data analysis for digital sound article

## load packages
library(tidyverse)

## read data file
tweets.df <- read.delim("all_tweets.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

## Top 10 tweeters
tweets.df %>% 
  select(from_user) %>% 
  group_by(from_user) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% View

## Retweets by date
rt <- tweets.df %>% 
  select(text, converted_date) %>% 
  mutate(rt.count = str_count(text, regex("^RT"))) %>% 
  group_by(rt.count, converted_date)

ggplot(rt, aes(x=converted_date, y=rt.count)) +
  geom_smooth(span=0.05, color="gray", se=FALSE) +
  theme_minimal()

## Total retweets? 12,143
sum(rt$rt.count)

## clean text of tweets
## code adapted from Zhao, Y. "Twitter Data Analysis with R – Text Mining and Social Network Analysis." http://www.rdatamining.com/docs/twitter-analysis-with-r

library(tm)
library(SnowballC)
library(stringi)

tweetsCorpus <- Corpus(VectorSource(tweets.df$text))
# tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(stringi::stri_trans_tolower)) 
tweetsCorpus <- tm_map(tweetsCorpus, removePunctuation)

## before removing numbers, consider if you want to keep "mp3", dates mentioned in text, etc. intact
# tweetsCorpus <- tm_map(tweetsCorpus, removeNumbers)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweetsCorpus <- tm_map(tweetsCorpus, removeURL)

## remove emoji
tweetsCorpus <- tm_map(tweetsCorpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

## add extra stop words
myStopwords <- c(stopwords("english"), stopwords("french"), stopwords("spanish"), stopwords("german"), "soundarchive", "pennsound", "librarycongress", "eu_sounds", "internetarchive", "ubuweb", "eusound", "amp", "via", "eusounds", "britishlibrary", "rt")
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, myStopwords)
tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)

## stem words (only to produce list of most frequent words overall)
tweetsCorpus <- tm_map(tweetsCorpus, PlainTextDocument)
tweetsCorpus <- tm_map(tweetsCorpus, stemDocument)

## turn corpus into a word list
tweetsCorpus <- as.character(tweetsCorpus)
tweetWords.l <- strsplit(tweetsCorpus, "\\W")
tweetWords.v <- unlist(tweetWords.l, recursive = TRUE, use.names = FALSE)

## get rid of spaces and blanks
not.blanks.v <- which(tweetWords.v != "")
tweetWords.v <- tweetWords.v[not.blanks.v]

## find frequency of various terms
length(tweetWords.v[which(tweetWords.v=="new")])

## copy words to csv, no header row
write.table(tweetWords.v, file = "tweet-words.csv", row.names=FALSE, na="", col.names=FALSE, sep=",")

## create table of word types and corresponding frequencies
tweetWordFreqs.t <- table(tweetWords.v)
sorted.tweetWordFreqs.t <- sort(tweetWordFreqs.t, decreasing=TRUE)

## convert raw counts into relative frequencies
sorted.tweetWord.relFreqs.t <- 100*(sorted.tweetWordFreqs.t/sum(sorted.tweetWordFreqs.t))
plot(sorted.tweetWord.relFreqs.t[1:10], type="l", xlab="Top Ten Words", 
     ylab="Relative Frequency", xaxt = "n")
axis(1,1:10, labels=names(sorted.tweetWord.relFreqs.t[1:10]))

## convert table to dataframe and print top ten words
library(knitr)
freqs <- as.data.frame(sorted.tweetWord.relFreqs.t)
names(freqs) <- c("word", "relative_frequency")
freqs %>% 
  slice(1:10) %>%
  kable()

## write to file
write.csv(freqs, file = "words_rel_freqs.csv", fileEncoding = "utf-8")

## visualize words in positions 7-9. the preceding words are more expected, e.g. "sound", "record", "music", "archive", "audio", and "read", as in "author reading her poem entitled..." (PennSound in particular uses this construction)
## spike in "collect" has to do with BLS #saveoursounds and directory campaigns
frequencies <- tweets.df %>% 
  select(text, converted_date) %>% 
  mutate(new.count = str_count(text, regex("\\b(new)")),
         listen.count = str_count(text, regex("\\b(listen)")),
         collect.count = str_count(text, regex("\\b(collect)")),
         total.count = str_count(text, regex("\\W+")),
         my = floor_date(converted_date, "month")) %>% 
  group_by(my) %>% 
  summarise(new = sum(new.count, na.rm = TRUE)/sum(total.count, na.rm = TRUE)*100,
            listen = sum(listen.count, na.rm = TRUE)/sum(total.count, na.rm = TRUE)*100,
            collect = sum(collect.count, na.rm = TRUE)/sum(total.count, na.rm = TRUE)*100) %>% 
  gather(word, rel_freq, -my)

p1 <- ggplot(frequencies, aes(x=my, y=rel_freq, color=word)) + 
  geom_line(size=1) +
  theme_minimal() +
  scale_color_grey(start = 0.9, end = 0) +
  labs(y="relative frequency", x="")

ggsave(paste0("fig1.png"), 
       p1, width = 7, height = 5)


## kwic (key words in context) for checking out the conversation on keywords like copyright, rare, etc. 
## code adapated from ch. 8 of Jockers, M. *Text Analysis with R for Students of Literature*.

tweetWords.v <- scan("tweet-words.csv", what="character", encoding = "UTF-8")
twitterCorpus.l <- as.list(tweetWords.v)
positions.v <- which(twitterCorpus.l[] == "music")
context <- 5
for(i in 1:length(positions.v)){
  start <- positions.v[i]-context
  end <- positions.v[i]+context
  before <- unlist(twitterCorpus.l[start:(start+context-1)])
  after <- unlist(twitterCorpus.l[(start+context+1):end])
  keyword <- unlist(twitterCorpus.l[start+context])
  cat("---------------------", i, "---------------------", "\n")
  cat(before, "[", keyword, "]", after, "\n")
}

## print corpus for later reuse by mallet. note: corpus has had punctuation and urls removed, but is otherwise unmodified (e.g. not stemmed).
library(readr)
all_tweets <- data.frame(text=sapply(tweetsCorpus, identity), stringsAsFactors=F)
write_delim(all_tweets, "all-tweets.txt", delim = '\n', col_names = FALSE)

## regex to insert newline every 9 lines in text file to create 2936 documents
## find: (.*\r?\n){9}\K
## replace: \n

## topic modeling using the mallet for R package
## code adapted from https://github.com/shawngraham/R/blob/master/topicmodel.R and http://rci.rutgers.edu/~ag978/litdata/hw10/

library(mallet)
# devtools::install_github("agoldst/litdata")
library(litdata)

## read in documents of tweets
alltweets <- readLines("all-tweets.txt")
alltweets_id <- as.character(seq_along(alltweets))
token_regex <- "\\w['\\w]*\\w|\\w"
mallet.instances <- mallet.import(id.array = alltweets_id, text.array = alltweets, stoplist.file = "stopwords.txt", preserve.case = F, token.regexp = token_regex)
n.topics <- 25
topic.model <- MalletLDA(n.topics)

## save instance of topic model
mallet.instances.file <- "tweets.mallet"
write_mallet_instances(mallet.instances)

## random number seed
seed <- 41
topic.model$model$setRandomSeed(as.integer(seed))

topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(500)

topic.model$maximize(10)

## save topic model state file
tweets_state <- "tweets_state5.gz"
write_mallet_state(topic.model, tweets_state)

## When reading in a saved state of topic model...
tweets_model_frm <- read.table(gzfile(tweets_state),
                               header=F, quote="", as.is=T, comment.char="#",
                               col.names=c("doc", "skip2", "skip3", "skip4", "word", "topic"),
                               colClasses=c("integer", "NULL", "NULL", "NULL", "character", "integer")
)
tweets_model_frm <- tweets_model_frm %>% 
  mutate(doc=alltweets_id[doc+1],
         topic=topic +1)

tweets_model_frm <- read_mallet_state(tweets_state, alltweets_id)

topic_words <- tweets_model_frm %>%
  group_by(topic, word) %>%
  summarize(count=n())

topic_top_words <- topic_words %>% group_by(topic) %>%
  arrange(desc(count)) %>%
  top_n(10, count) 

topic_labels <- topic_top_words %>%
  filter(count >= 3) %>%
  summarize(label=str_c(word, collapse=" "))

topic_labels %>%
  print_tabular()

## valentine to sherwood anderson topic
p2 <- topic_words %>%
  filter(topic == 2) %>%
  top_n(10, count) %>%
  arrange(count) %>%
  mutate(word=factor(word, levels=word, ordered=T)) %>% 
  ggplot(aes(word, count)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  coord_flip() +
  ggtitle(topic_labels$label[2])

ggsave(paste0("fig2.png"), 
       p2, width = 7.5, height = 3)


n_topics <- max(topic_words$topic) 
topic_words %>%
  mutate(term_freq=count) %>%
  group_by(word) %>%
  mutate(score=1 + term_freq * log(n_topics / n())) %>%
  group_by(topic) %>%
  top_n(10, score) %>% 
  arrange(desc(score)) %>% 
  summarize(label=str_c(word, collapse=" ")) %>%
  print_tabular()

doc_topics <- tweets_model_frm %>%
  group_by(doc, topic) %>%
  summarize(count=n()) 

labeled_top_docs <- doc_topics %>%
  inner_join(topic_labels, by="topic") %>%
  group_by(topic) %>%
  top_n(3, count) 

doc_topics$doc <- as.numeric(doc_topics$doc)

topic_series_plot <- doc_topics %>% group_by(doc) %>%
  mutate(topic_proportion=count / sum(count)) %>% 
  ggplot(aes(doc, topic_proportion)) +
  facet_wrap(~ topic, ncol=5) +
  xlab("") +
  ylab("Topic proportion")

## plot topic model
## fig not used b/c too event-y and confusing
p3.1 <- topic_series_plot +
  geom_bar(stat="identity", width=0.5, color="gray50") +
  geom_smooth(group=1, method="loess", se=F, color="black") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## having a look more than 15 words out into topic
library(wordcloud)
wc <- topic_words %>% 
  filter(topic == 11 & count > 3)
wordcloud(wc$word, wc$count)

## valentine to sherwood anderson topic
p2 <- topic_words %>%
  filter(topic == 2) %>%
  top_n(10, count) %>% 
  arrange(count) %>%
  mutate(word=factor(word, levels=word, ordered=T)) %>% 
  ggplot(aes(word, count)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  coord_flip() +
  ggtitle(paste0("topic 2: ", topic_labels$label[2]))

ggsave(paste0("fig2.png"), 
       p2, width = 8, height = 3)

## TIFF
tiff("fig2.tiff", width = 9, height = 3, units = 'in', res = 600)
p2
dev.off()
