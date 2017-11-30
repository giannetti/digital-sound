# Data preparation for digital sound article

## Create function to read in new and old datasets (from different versions of TAGS tool), drop columns that aren't shared, and then join them together into one data frame

tsvJoin <- function(new_data, old_data) {
  stopifnot(endsWith(new_data, ".tsv"), endsWith(old_data, "_old.tsv"))
  tsvNew <- read.delim(paste0("data/", new_data), encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
  tsvOld <- read.delim(paste0("data/", old_data), encoding = "UTF-8", sep = "\t", quote = "", header = T, stringsAsFactors = F)
  drop <- c("retweet_count", "user_utc_offset")
  rlyNew_data <- tsvNew[,!(names(tsvNew) %in% drop)]
  newOld_data <- tsvOld[,!(names(tsvOld) %in% drop)]
  joinedTsv <- rbind(rlyNew_data, newOld_data)
}

## Join TSVs
eu_sounds <- tsvJoin("eu_sounds.tsv", "eu_sounds_old.tsv")
internetarchive <- tsvJoin("internetarchive.tsv", "internetarchive_old.tsv")
pennsound <- tsvJoin("pennsound.tsv", "pennsound_old.tsv")
soundarchive <- tsvJoin("soundarchive.tsv", "soundarchive_old.tsv")
ubuweb <- tsvJoin("ubuweb.tsv", "ubuweb_old.tsv")

## Remove duplicate tweets
eu_sounds.dedup <- subset(eu_sounds, !duplicated(eu_sounds$id_str))
internetarchive.dedup <- subset(internetarchive, !duplicated(internetarchive$id_str))
pennsound.dedup <- subset(pennsound, !duplicated(pennsound$id_str))
soundarchive.dedup <- subset(soundarchive, !duplicated(soundarchive$id_str))
ubuweb.dedup <- subset(ubuweb, !duplicated(ubuweb$id_str))

## Stack them all together, add a formatted date column, and sort by time
tweets.df <- rbind(eu_sounds.dedup, internetarchive.dedup, librarycongress.dedup, pennsound.dedup, soundarchive.dedup, ubuweb.dedup, header = TRUE)
tweets.df$converted_date <- as.Date(tweets.df$time, format= "%d/%m/%Y %H:%M:%S")
tweets.sorted.df <- tweets.df[order(as.POSIXlt(tweets.df$time, format= "%d/%m/%Y %H:%M:%S")), ]

## Print out modified dataset for entire corpus
write.table(tweets.sorted.df, "all_tweets.tsv", sep = "\t", quote=FALSE, row.names = FALSE)

## Create dataset for public archive
## per Twitter Developer Agreement:
## "If you provide Content to third parties, including downloadable datasets of Content or an API that returns Content, you will only distribute or allow download of Tweet IDs and/or User IDs."
data_deposit <- read.delim("all_tweets.tsv", encoding = "UTF-8", sep = "\t", quote = "", header = T)
data_deposit <- format(data_deposit$id_str, scientific = FALSE)
write(data_deposit, file="data_deposit.txt", ncolumns = 1, sep = "\n")
