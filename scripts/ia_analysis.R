# internetarchive tweets analysis

# read in files
internetarchive <- read.delim("internetarchive.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
internetarchive_old <- read.delim("internetarchive_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

# drop columns that aren't shared between new and old TAGS archives
drop <- c("retweet_count", "user_utc_offset")
internetarchive.new <- internetarchive[,!(names(internetarchive) %in% drop)]
internetarchive.old <- internetarchive_old[,!(names(internetarchive_old) %in% drop)]

# bind them together and deduplicate 
internetarchive.tot <- rbind(internetarchive.new, internetarchive.old)
internetarchive.tot.dedup <- subset(internetarchive.tot, !duplicated(internetarchive.tot$id_str))

# sort them by time
ia.tot.dedup.sorted <- internetarchive.tot.dedup[order(as.POSIXlt(internetarchive.tot.dedup$time, format= "%d/%m/%Y %H:%M:%S")), ]

# create version of IA dataset w/o RTs for topic modeling (n=1270)
library(dplyr)
library(stringr)
ia_no_rts <- ia.tot.dedup.sorted %>% 
  select(text) %>% 
  filter(!str_detect(text, "^RT")) # remove retweets

# clean text of tweets
library(tm)
library(SnowballC)
iaCorpus <- Corpus(VectorSource(ia_no_rts))
iaCorpus <- tm_map(iaCorpus, content_transformer(tolower))
iaCorpus <- tm_map(iaCorpus, removePunctuation)
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
iaCorpus <- tm_map(iaCorpus, removeURL)

# print corpus for later reuse by mallet. note: corpus has been lowercased, has had punctuation and urls removed, but is otherwise unmodified.
library(readr)
ia_tweets <- data.frame(text=sapply(iaCorpus, identity), stringsAsFactors=F)
write_delim(ia_tweets, "ia_no_rts2.txt", delim = '\n', col_names = FALSE)

# regex to insert newline every 100 words in text file to create 30 documents
# find: (\w+\W+){100}
# replace: $1\n

# topic modeling
library(mallet)
# devtools::install_github("agoldst/litdata")
library(litdata)

# read in 30 docs of tweets
alltweets <- readLines("ia_no_rts.txt") 
alltweets_id <- as.character(seq_along(alltweets))
token_regex <- "\\w['\\w]*\\w|\\w"
mallet.instances <- mallet.import(id.array = alltweets_id, text.array = alltweets, stoplist.file = "stopwords.txt", preserve.case = F, token.regexp = token_regex)
n.topics <- 6
topic.model <- MalletLDA(n.topics)

# random number seed
seed <- 41
topic.model$model$setRandomSeed(as.integer(seed))

topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(500)

topic.model$maximize(10)

# save topic model state file
tweets_state <- "ia_state2.gz"
write_mallet_state(topic.model, tweets_state)

# If you are reading in the saved state of topic model...
tweets_model_frm <- read.table(gzfile(tweets_state),
                               header=F, quote="", as.is=T, comment.char="#",
                               col.names=c("doc", "skip2", "skip3", "skip4", "word", "topic"),
                               colClasses=c("integer", "NULL", "NULL", "NULL", "character", "integer")
)
tweets_model_frm <- tweets_model_frm %>% 
  mutate(doc=alltweets_id[doc+1],
         topic=topic +1)

topic_words <- tweets_model_frm %>%
  group_by(topic, word) %>%
  summarize(count=n())

topic_top_words <- topic_words %>% 
  group_by(topic) %>%
  arrange(desc(count)) %>%
  top_n(11, count) 

topic_labels <- topic_top_words %>%
  filter(count >=4) %>%
  summarize(label=str_c(word, collapse=" "))

library(knitr)
n_topics <- max(topic_words$topic) # in other words, 6
topic_words %>%
  mutate(term_freq=count) %>% 
  group_by(word) %>% 
  mutate(score=1 + term_freq * log(n_topics / n())) %>% 
  group_by(topic) %>%
  top_n(10, score) %>% 
  arrange(desc(score)) %>% 
  summarize(label=str_c(word, collapse=" ")) %>% 
  kable()

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
  facet_wrap(~ topic, ncol=1) +
  xlab("") +
  ylab("Topic proportion")

# plot topic model
p3 <- topic_series_plot +
  geom_bar(stat="identity", width=0.5, fill="gray80") +
  geom_smooth(method="loess", se=FALSE, color="black", fill="grey60", span=0.5) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())

# save figures
ggsave(paste0("fig3.png"), 
       p3, width = 8, height = 5)

tiff("fig3.tiff", width = 8, height = 5, units = 'in', res = 600)
p3
dev.off()

# IA Mopidy, Grateful Dead, and @ANBOLIVIA topics
ia_topic_plot <- doc_topics %>% 
  group_by(doc) %>%
  mutate(topic_proportion=count / sum(count))

mopidy_plot <- ggplot(data=subset(ia_topic_plot,topic==5), aes(doc, topic_proportion)) +
  xlab("") +
  ylab("Topic proportion")

dead_plot <- ggplot(data=subset(ia_topic_plot, topic==6), aes(doc, topic_proportion)) +
  xlab("") +
  ylab("Topic proportion")

anbolivia_plot <- ggplot(data=subset(ia_topic_plot, topic==2), aes(doc, topic_proportion)) +
  xlab("") +
  ylab("Topic proportion")

p4 <- mopidy_plot +
  geom_bar(stat="identity", width=0.5, fill="gray80") +
  geom_smooth(method="loess", se=FALSE, color="black", fill="grey60", span=0.5) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  ggtitle(topic_labels$label[5])

p4 <- p4 + scale_x_continuous(limits=c(0, 30))

# save figures
ggsave(paste0("fig4b.png"), 
       p4, width = 8, height = 4)

tiff("fig4b.tiff", width = 8, height = 4, units = 'in', res = 600)
p4
dev.off()

p5 <- dead_plot +
  geom_bar(stat="identity", width=0.5, fill="gray80") +
  geom_smooth(method="loess", se=FALSE, color="black", fill="grey60", span=0.5) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  ggtitle(topic_labels$label[6])

# save figures
ggsave(paste0("fig5.png"), 
       p5, width = 8, height = 4)

tiff("fig5.tiff", width = 8, height = 4, units = 'in', res = 600)
p5
dev.off()


p6 <- anbolivia_plot +
  geom_bar(stat="identity", width=0.5, fill="gray80") +
  geom_smooth(method="loess", se=FALSE, color="black", fill="grey60", span=0.5) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank()) +
  ggtitle(topic_labels$label[2])

p6 <- p6 + scale_x_continuous(limits=c(0, 30))

library(grid)
library(gridExtra)
p <- rbind(ggplotGrob(p6), ggplotGrob(p4), ggplotGrob(p5), size = "last")
grid.newpage() 
grid.draw(p)

# save figs
ggsave(paste0("fig6.png"), plot = grid.draw(p8), width = 8, height = 6)
tiff("fig6.tiff", width = 8, height = 6, units = 'in', res = 600)
grid.draw(p8)
dev.off()

# having a look more than 10 words out into topic
library(wordcloud)
wc <- topic_words %>% 
  filter(topic == 2 & count > 1)
wordcloud(wc$word, wc$count)
