# Data preparation for digital sound article

# join new and old TAGS archives into one file, remove duplicate tweets

# read files into data frames
eu_sounds <- read.delim("eu_sounds.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
eu_sounds_old <- read.delim("eu_sounds_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

internetarchive <- read.delim("internetarchive.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
internetarchive_old <- read.delim("internetarchive_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

librarycongress <- read.delim("librarycongress.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
librarycongress_old <- read.delim("librarycongress_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

pennsound <- read.delim("pennsound.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
pennsound_old <- read.delim("pennsound_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

soundarchive <- read.delim("soundarchive.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
soundarchive_old <- read.delim("soundarchive_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

ubuweb <- read.delim("ubuweb.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
ubuweb_old <- read.delim("ubuweb_old.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)

# get rid of extraneous columns
drop <- c("retweet_count", "user_utc_offset")
eu_sounds.new <- eu_sounds[,!(names(eu_sounds) %in% drop)]
eu_sounds.old <- eu_sounds_old[,!(names(eu_sounds_old) %in% drop)]

internetarchive.new <- internetarchive[,!(names(internetarchive) %in% drop)]
internetarchive.old <- internetarchive_old[,!(names(internetarchive_old) %in% drop)]

librarycongress.new <- librarycongress[,!(names(librarycongress) %in% drop)]
librarycongress.old <- librarycongress_old[,!(names(librarycongress_old) %in% drop)]

pennsound.new <- pennsound[,!(names(pennsound) %in% drop)]
pennsound.old <- pennsound_old[,!(names(pennsound_old) %in% drop)]

soundarchive.new <- soundarchive[,!(names(soundarchive) %in% drop)]
soundarchive.old <- soundarchive_old[,!(names(soundarchive_old) %in% drop)]

ubuweb.new <- ubuweb[,!(names(ubuweb) %in% drop)]
ubuweb.old <- ubuweb_old[,!(names(ubuweb_old) %in% drop)]

# join and remove duplicate tweets
eu_sounds.tot <- rbind(eu_sounds.new, eu_sounds.old)
eu_sounds.tot.dedup <- subset(eu_sounds.tot, !duplicated(eu_sounds.tot$id_str))

internetarchive.tot <- rbind(internetarchive.new, internetarchive.old)
internetarchive.tot.dedup <- subset(internetarchive.tot, !duplicated(internetarchive.tot$id_str))

librarycongress.tot <- rbind(librarycongress.new, librarycongress.old)
librarycongress.tot.dedup <- subset(librarycongress.tot, !duplicated(librarycongress.tot$id_str))

pennsound.tot <- rbind(pennsound.new, pennsound.old)
pennsound.tot.dedup <- subset(pennsound.tot, !duplicated(pennsound.tot$id_str))

soundarchive.tot <- rbind(soundarchive.new, soundarchive.old)
soundarchive.tot.dedup <- subset(soundarchive.tot, !duplicated(soundarchive.tot$id_str))

ubuweb.tot <- rbind(ubuweb.new, ubuweb.old)
ubuweb.tot.dedup <- subset(ubuweb.tot, !duplicated(ubuweb.tot$id_str))

# stack them all together, add a formatted date column, sort by time
tweets.df <- rbind(eu_sounds.tot.dedup, internetarchive.tot.dedup, librarycongress.tot.dedup, pennsound.tot.dedup, soundarchive.tot.dedup, ubuweb.tot.dedup, header = TRUE)
tweets.df$converted_date <- as.Date(tweets.df$time, format= "%d/%m/%Y %H:%M:%S")
tweets.sorted.df <- tweets.df[order(as.POSIXlt(tweets.df$time, format= "%d/%m/%Y %H:%M:%S")), ]

# print out modified dataset for entire corpus
write.table(tweets.sorted.df, "all_tweets.tsv", sep = "\t", quote=FALSE, row.names = FALSE)

# create dataset for public archive
# per Twitter Developer Agreement:
# "If you provide Content to third parties, including downloadable datasets of Content or an API that returns Content, you will only distribute or allow download of Tweet IDs and/or User IDs."
data_deposit <- read.delim("all_tweets.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T)
data_deposit <- format(data_deposit$id_str, scientific = FALSE)
write(data_deposit, file="data_deposit.txt", ncolumns = 1, sep = "\n")
