# Data analysis for digital sound article

# load packages

library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

# read data file
total.tweets <- read.delim("all_tweets.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

# type conversions
total.tweets$converted_date <- as.Date(total.tweets$converted_date, format="%Y-%m-%d")

# Who is doing all this tweeting? Mostly the sound archives.
total.tweets %>% 
  select(from_user) %>% 
  group_by(from_user) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  top_n(10, n) %>% View

# How many RTs are there by date?
rt <- total.tweets %>% 
  select(text, converted_date) %>% 
  mutate(rt.count = str_count(text, regex("\\brt*\\b",
                                          ignore_case=TRUE))) %>% 
  group_by(rt.count, converted_date)

ggplot(rt, aes(x=converted_date, y=rt.count)) +
  geom_smooth(span=0.05, color="gray", se=FALSE) +
  theme_minimal()

# how many retweets in total? 10,288
sum(rt$rt.count)

# count of 'new', in EN, FR, IT, GER, & SP, ignoring 'new york'
new <- total.tweets %>% 
  select(text, converted_date) %>% 
  mutate(new.count = str_count(text, regex("\\bnew(er|est|ly)*\\b\\s(?!york)|nouve(au|l|le)|nuov(o|a)|\\bneu(es|esten)\\b|nuev(a|o)\\b", 
                                           ignore_case=TRUE,
                                           multiline=TRUE)), 
         my = floor_date(converted_date, "month")) %>% 
  group_by(my) %>% 
  summarize(new = sum(new.count, na.rm = TRUE)) 

# plot of 'new', dates binned by month
p1 <- ggplot(new, aes(x=my, y=new)) + 
  geom_point(alpha = 0.7, shape = 1, na.rm = TRUE) +
  geom_smooth(span=0.3, color="gray25", se=FALSE) +
  theme_minimal() +
  labs(title="Plot of 'new' over time", y="count", x="date") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# PNG
ggsave(paste0("fig1.png"), 
       p1, width = 7, height = 5)

# TIFF
tiff("fig1.tiff", width = 7, height = 5, units = 'in', res = 600)
p1
dev.off()

# How many 'new' in total? 1,228
sum(new$new.count)

# clean text of tweets
# code adapted from Zhao, Y. "Twitter Data Analysis with R – Text Mining and Social Network Analysis." http://www.rdatamining.com/docs/twitter-analysis-with-r

library(tm)
library(SnowballC)

tweetsCorpus <- Corpus(VectorSource(total.tweets$text))
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))
tweetsCorpus <- tm_map(tweetsCorpus, removePunctuation)

# before removing numbers, consider if you want to keep "mp3", dates mentioned in text, etc. intact
# tweetsCorpus <- tm_map(tweetsCorpus, removeNumbers)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweetsCorpus <- tm_map(tweetsCorpus, removeURL)

# add extra stop words
myStopwords <- c(stopwords("english"), stopwords("french"), stopwords("spanish"), stopwords("german"), "soundarchive", "pennsound", "librarycongress", "eu_sounds", "internetarchive", "ubuweb", "eusound", "amp", "via", "eusounds", "britishlibrary")
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, myStopwords)
tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)

# turn corpus into a word list
tweetsCorpus <- as.character(tweetsCorpus)
tweetWords.l <- strsplit(tweetsCorpus, "\\W")
tweetWords.v <- unlist(tweetWords.l, recursive = TRUE, use.names = FALSE)

# get rid of spaces and blanks
not.blanks.v <- which(tweetWords.v != "")
tweetWords.v <- tweetWords.v[not.blanks.v]

# find frequency of various terms
length(tweetWords.v[which(tweetWords.v=="new")])

# copy words to csv, no header row
write.table(tweetWords.v, file = "tweet-words.csv", row.names=FALSE, na="", col.names=FALSE, sep=",")

# kwic (key words in context) for checking out the conversation on keywords like copyright, rare, etc. 
# code adapated from ch. 8 of Jockers, M. *Text Analysis with R for Students of Literature*.

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

# topic modeling using the mallet for R package
# code adapted from https://github.com/shawngraham/R/blob/master/topicmodel.R and http://rci.rutgers.edu/~ag978/litdata/hw10/

library(mallet)
devtools::install_github("agoldst/litdata")
library(litdata)

# make one giant corpus document out of all tweets
alltweets <- readLines("all-tweets.txt")
alltweets_id <- as.character(seq_along(alltweets))
token_regex <- "\\w['\\w]*\\w|\\w"
mallet.instances <- mallet.import(id.array = alltweets_id, text.array = alltweets, stoplist.file = "stopwords.txt", preserve.case = F, token.regexp = token_regex)
n.topics <- 25
topic.model <- MalletLDA(n.topics)

# save instance of topic model
write_mallet_instances(mallet.instances)

topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(500)

topic.model$maximize(10)

tweets_state <- "tweets_state.gz"

# If you are reading in the saved state of topic model...
tweets_model_frm <- read.table(gzfile(tweets_state),
                               header=F, quote="", as.is=T, comment.char="#",
                               col.names=c("doc", "skip2", "skip3", "skip4", "word", "topic"),
                               colClasses=c("integer", "NULL", "NULL", "NULL", "character", "integer")
)
tweets_model_frm <- tweets_model_frm %>% 
  mutate(doc=alltweets_id[doc+1],
         topic=topic +1)

topic.model$model$printState(new(J("java.io.File"), tweets_state))
write_mallet_state(topic.model, tweets_state)

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

# valentine to sherwood anderson topic
p2 <- topic_words %>%
  filter(topic == 4) %>%
  top_n(10, count) %>%
  arrange(count) %>%
  mutate(word=factor(word, levels=word, ordered=T)) %>% 
  ggplot(aes(word, count)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  coord_flip() +
  ggtitle(topic_labels$label[4])

ggsave(paste0("fig4.png"), 
       p2, width = 7.5, height = 3)

# TIFF
tiff("fig4.tiff", width = 7.5, height = 3, units = 'in', res = 600)
p2
dev.off()

n_topics <- max(topic_words$topic) # long way to say 12
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
  # proportions, recall
  mutate(topic_proportion=count / sum(count)) %>% 
  ggplot(aes(doc, topic_proportion)) +
  facet_wrap(~ topic, ncol=5) +
  xlab("") +
  ylab("Topic proportion")

# plot topic model
p3 <- topic_series_plot +
  geom_bar(stat="identity", width=0.5, color="gray50") +
  geom_smooth(group=1, method="loess", se=F, color="black") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# all-tweets Gertrude Stein topic
topic_4 <- doc_topics %>% 
  filter(topic == 4) %>% 
  group_by(doc) %>% 
  ggplot(aes(x=doc, y=count)) + geom_bar(stat="identity") +
  geom_smooth(method = "loess", se=F, color="black") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# IA mopidy topic
topic_5 <- doc_topics %>%
  inner_join(topic_labels, by="topic") %>% 
  filter(topic == 5) %>% 
  group_by(doc) %>% 
  ggplot(aes(x=doc, y=count)) + geom_bar(stat="identity") +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle(topic_labels$label[5]) 

ggsave(paste0("fig4b.png"), 
       topic_5, width = 9, height = 3)

# TIFF
tiff("fig4b.tiff", width = 9, height = 3, units = 'in', res = 600)
topic_5
dev.off()

doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "mallet-topic-docs.csv")
mallet.top.words(topic.model, topic.words[7,])

word.freqs %>% 
  filter(words == "copyright") %>% View

topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
topics.labels

topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents_id
plot(hclust(dist(topic.words)), labels=topics.labels)