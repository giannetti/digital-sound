# digital-sound
Data and scripts used for the article "A Twitter Case Study for Assessing Digital Sound."

Data include only the tweet id values. One can "hydrate" these ids (retrieve the corresponding JSON data) using Ed Summers's excellent [twarc](https://github.com/DocNow/twarc) Python library. Upon request, I will also supply the complete dataset.

-----

## data

"Raw" and derived datasets used in the article. In keeping with Twitter's [developer guidelines](https://developer.twitter.com/en/developer-terms/policy), only tweet ids are included in the .txt files ("If you provide Content to third parties, including downloadable datasets of Content or an API that returns Content, you will only distribute or allow download of Tweet IDs, Direct Message IDs, and/or User IDs"). `all_tweets.txt` is an aggregated, chronologically sorted list of tweet ids collected for British Library Sounds, Europeana Sounds, the Internet Archive Audio Archive, PennSound, and Ubuweb. Tweets collected for the individual sound archives have their Twitter handles as filenames. `words_rel_freqs.csv` is a stemmed list of word frequencies for the aggregate corpus. The `_state.gz` files are the saved instantiations of the topic models analyzed in the article. 

-----

## scripts
Scripts used for preparing data, creating a table of descriptive statistics, word frequency analysis, and topic modeling. There's also a `smoke-test.Rmd` to evaluate the impact of some missing data on the analysis.
