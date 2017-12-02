# Descriptive statistics for all Twitter archives

fileName <- c("eu_sounds", "internetarchive", "pennsound", "soundarchive", "ubuweb")

## loop through filenames to read in data and create header row
for (f in fileName) {
  newDF <- read.csv(paste0("data/quoted_", f, ".csv"), encoding = "UTF-8", stringsAsFactors = FALSE, header = FALSE)
  names(newDF) <- c("id", "screen_name", "text", "quoted_tweet", "quoted_screen_name")
  newname <- paste0(f, "_quoted")
  assign(newname,newDF)
}

## Create function to count the number of quoted tweets
quote_counts <- function(f) {
    a <- get(paste0(f, "_quoted"))
    a %>% 
      filter(quoted_tweet != "") %>% 
      mutate(dataset = f) %>% 
      group_by(dataset) %>% 
      summarize(quoted_counts = n())
}

eu_sounds_quotes <- quote_counts("eu_sounds")
internetarchive_quotes <- quote_counts("internetarchive")
pennsound_quotes <- quote_counts("pennsound")
soundarchive_quotes <- quote_counts("soundarchive")
ubuweb_quotes <- quote_counts("ubuweb")

all_quotes <- rbind(eu_sounds_quotes, internetarchive_quotes, pennsound_quotes, soundarchive_quotes, ubuweb_quotes)

## Create function to calculate descriptive statistics on each archive
createStats <- function(f) {
  a <- get(paste0(f, ".dedup"))
  a %>% select(text, from_user) %>% 
    mutate(rt.count = str_count(text, regex("^RT")),
           dr.count = str_count(text, regex("^@"))) %>% 
    summarize(dataset = f,
              total = nrow(a),
              retweets = sum(rt.count),
              replies = sum(dr.count),
              accounts = length(unique(from_user))) %>% 
    left_join(all_quotes, by = "dataset") %>% 
    mutate(original = total - retweets - replies - quoted_counts)
}

eu_sounds_stats <- createStats("eu_sounds")
internetarchive_stats <- createStats("internetarchive")
pennsound_stats <- createStats("pennsound")
soundarchive_stats <- createStats("soundarchive")
ubuweb_stats <- createStats("ubuweb")

all_stats <- rbind(eu_sounds_stats, internetarchive_stats, pennsound_stats, soundarchive_stats, ubuweb_stats)
