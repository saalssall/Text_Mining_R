# Load qdap
library(qdap)
# Load tm
library(tm)
# Load wordcloud package
library(wordcloud)
library(tidyverse)
install.packages("plotrix")  # run once
library(plotrix)

text <- "Text mining, text data mining (TDM) or text analytics is the process of deriving high-quality information from text. It involves 'the discovery by computer of new, previously unknown information, by automatically extracting information from different written resources.'[1] Written resources may include websites, books, emails, reviews, and articles.[2] High-quality information is typically obtained by devising patterns and trends by means such as statistical pattern learning. According to Hotho et al. (2005), there are three perspectives of text mining: information extraction, data mining, and knowledge discovery in databases (KDD).[3] Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interest. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities)."
# Print new_text to the console
text
# Find the 10 most frequent terms: term_count
term_count <- freq_terms(text, 10)
# Plot term_count
plot(term_count)

# Import text data from CSV, no factors
tweet_data <- read.csv("twitter_data.csv", stringsAsFactors = FALSE)
# View the structure of the data
str(tweet_data)
# Isolate the text from the tweets
tech_tweets <- tweet_data$tweet_text
head(tech_tweets)

# Make a vector source from tech_tweets
tech_source <- VectorSource(tech_tweets)
# Make a volatile corpus from tech_source
tech_corpus <- VCorpus(tech_source)
# Print out tech_corpus
print(tech_corpus)
# Print the 15th tweet in tech_corpus
print(tech_corpus[[15]])
# Print the contents of the 15th tweet in tech_corpus
print(content(tech_corpus[[15]]))
# Now use content to review the plain text of the 10th tweet
print(content(tech_corpus[[10]]))

tweets_df <- data.frame(
  doc_id = seq_len(nrow(tweet_data)),
  text   = tweet_data$tweet_text,
  stringsAsFactors = FALSE
)

# Corpus from the data frame
df_source <- DataframeSource(tweets_df)
df_corpus <- VCorpus(df_source)

# Corpus from a plain vector of text
vec_source <- VectorSource(tweet_data$tweet_text)
vec_corpus <- VCorpus(vec_source)

# Compare the number of documents
df_corpus
vec_corpus

# Compare corpus-level metadata
meta(df_corpus)
meta(vec_corpus)

clean_text <- function(x) {
  # 1. Remove markup and bracketed text while the brackets still exist
  x <- gsub("<[^>]+>", "", x)         # HTML tags, keeping the words between them
  x <- bracketX(x)                    # (text in brackets) removed entirely
  
  # 2. Replace things that rely on punctuation, symbols or digits
  x <- replace_abbreviation(x)        # needs periods: "Sr." -> "Senior"
  x <- replace_contraction(x)         # needs apostrophes: "It's" -> "it is"
  x <- replace_symbol(x)              # "%" -> "percent", "$" -> "dollar"
  x <- replace_number(x)              # "6" -> "six", "10" -> "ten"
  
  # 3. Standardise what is left
  x <- tolower(x)                     # after replacements, which may add capitals
  x <- removePunctuation(x)           # remaining punctuation (!, commas, periods)
  x <- stripWhitespace(x)             # last, so gaps left by earlier steps collapse
  x
}

text <- "<b>She</b> woke up at       6 A.M. It\'s so early!  She was only 10% awake and began drinking coffee in front of her computer. But drinking too much coffee seems to be not good (bad) idea, Said Sr. Sofrware Engineer, Hamid (only in dreams lols!). Drinking too much coffee cost too much $"

clean_text(text)

# List standard English stop words
stopwords("en")
# Print text without standard stop words
removeWords(text, stopwords("en"))
# Add "coffee" and "bean" to the list: new_stops
new_stops <- c("coffee", "bean", stopwords("en"))
# Remove stop words from text
removeWords(text, new_stops)

# Create complicate
complicate <- c("complicated", "complication", "complicatedly")
# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)
# Create the completion dictionary: comp_dict
comp_dict <- "complicate"
# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)
# Print complete_text
complete_text

text_data <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly."
# Dictionary of full words used to complete the stems
comp_dict <- c("In", "a", "complicate", "haste", "Tom",
               "rush", "to", "fix", "new", "too")

word_stemming <- function(x) {
  # Remove punctuation: rm_punc
  rm_punc <- removePunctuation(x)
  # Create character vector: n_char_vec
  n_char_vec <- unlist(strsplit(rm_punc, split = " "))
  # Perform word stemming: stem_doc
  stem_doc <- stemDocument(n_char_vec)
  # Re-complete stemmed document: complete_doc
  complete_doc <- stemCompletion(stem_doc, comp_dict)
  print(complete_doc)
  
}

word_stemming(text_data)

clean_corpus <- function(corpus) {
  # Lowercase first, so the stop word list (all lowercase) matches
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove URLs before punctuation is stripped, or they turn into junk like "httpstcoabc"
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("http\\S+|www\\S+", "", x)))
  # Remove stop words plus custom ones for this dataset
  corpus <- tm_map(corpus, removeWords,
                   words = c(stopwords("en"), "rt", "link", "mention", "sxsw"))
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Apply the function to your corpus
clean_corp <- clean_corpus(vec_corpus)
# A cleaned tweet
content(clean_corp[[220]])
# The same tweet in its original form
tweet_data$tweet_text[220]

# Create the document-term matrix from the corpus
tweet_dtm <- DocumentTermMatrix(clean_corp)
# Print out tweet_dtm data
tweet_dtm
# Convert tweet_dtm to a matrix
tweet_m <- as.matrix(tweet_dtm)
# Print the dimensions of tweet_m
dim(tweet_m)
# Review a portion of the matrix to get some Starbucks
tweet_m[25:35, c("ipad", "google")]

# Create a term-document matrix from the corpus
tweet_tdm <- TermDocumentMatrix(clean_corp)
# Print tweet_tdm data
tweet_tdm
# Convert tweet_tdm to a matrix
tweet_m <- as.matrix(tweet_tdm)
# Print the dimensions of the matrix
dim(tweet_m)
# Review a portion of the matrix
tweet_m[c("ipad", "google"), 25:35]

# Convert tweet_tdm to a matrix
tweet_m <- as.matrix(tweet_tdm)
# Calculate the row sums of tweet_m
term_frequency <- rowSums(tweet_m)
# Sort term_frequency in decreasing order
term_frequency <- sort(term_frequency, decreasing = TRUE)
# View the top 10 most common words
term_frequency[1:10]
# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)

# Create frequency using freq_terms from qdap
frequency <- freq_terms(
  tweet_data$tweet_text,
  top = 10,
  at.least = 3,
  stopwords = Top200Words
)

plot(frequency)

# Create frequency
frequency <- freq_terms(
  tweet_data$tweet_text,
  top = 10,
  at.least = 3,
  stopwords = stopwords("english")
)
# Make a frequency bar chart
plot(frequency)

# Print the first 10 entries in term_frequency
term_frequency[1:10]
# Vector of terms
terms_vec <- names(term_frequency)
# Create a word cloud for the values in word_freqs
wordcloud(terms_vec, term_frequency, max.words = 50, colors = "red")

# Word frequencies: total count of each term across all tweets
word_freqs <- rowSums(as.matrix(tweet_tdm))      # tweet_tdm is a TDM (terms = rows)
# Sort in descending order
sorted_words <- sort(word_freqs, decreasing = TRUE)
# Print the 6 most frequent terms
head(sorted_words, 6)
# Get a terms vector
terms_vec <- names(sorted_words)
# Create a word cloud
wordcloud(terms_vec, sorted_words,
          max.words = 50, colors = "red")


# Data frame with one row per term
term_frequency <- data.frame(term = names(word_freqs),
                             num  = word_freqs)

# Word cloud with the specified colours
wordcloud(term_frequency$term, term_frequency$num,
          max.words = 100,
          colors = c("grey80", "darkgoldenrod1", "tomato"))

library(viridisLite)
# Select 5 colors 
color_pal <- cividis(5)
# Examine the palette output
color_pal
# Create a word cloud with the selected palette
wordcloud(term_frequency$term, term_frequency$num, max.words = 100, colors = color_pal)

# Check the label column and the exact group names (names(tweet_data) shows the columns)
label_col <- "is_there_an_emotion_directed_at_a_brand_or_product"
table(tweet_data[[label_col]])

# Two groups to compare 
positive_tweets <- tweet_data[which(tweet_data[[label_col]] == "Positive emotion"), ]
negative_tweets <- tweet_data[which(tweet_data[[label_col]] == "Negative emotion"), ]

# Create all_positive
all_positive <- paste(positive_tweets$tweet_text, collapse = " ")
# Create all_negative
all_negative <- paste(negative_tweets$tweet_text, collapse = " ")
# Create all_tweets: a vector of two long strings
all_tweets <- c(all_positive, all_negative)
# Convert to a vector source
all_tweets <- VectorSource(all_tweets)
# Create all_corpus
all_corpus <- VCorpus(all_tweets)
length(all_corpus)  

# Clean the corpus
all_clean <- clean_corpus(all_corpus)
# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)
# Create all_m
all_m <- as.matrix(all_tdm)
# Print a commonality cloud
commonality.cloud(all_m, colors = "steelblue1", max.words = 100)

# Clean the corpus
all_clean <- clean_corpus(all_corpus)
# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean)
# Give the columns distinct names
colnames(all_tdm) <- c("coffee", "chardonnay")
# Create all_m
all_m <- as.matrix(all_tdm)
# Create comparison cloud
comparison.cloud(all_m, colors = c("orange", "blue"), max.words = 50)

# Create all_tdm_m: rows = words, columns = groups
all_tdm_m <- as.matrix(all_tdm)

top25_df <- all_tdm_m %>%
  # Convert to data frame
  as_tibble(rownames = "word") %>% 
  # Keep rows where word appears everywhere
  filter(if_all(everything(), ~. > 0)) %>% 
  # Get difference in counts
  mutate(difference = chardonnay - coffee) %>% 
  # Keep rows with biggest difference
  slice_max(difference,  n = 25) %>% 
  # Arrange by descending difference
  arrange(desc(difference))

pyramid.plot(
  # Chardonnay counts
  top25_df$chardonnay, 
  # Coffee counts
  top25_df$coffee, 
  # Words
  labels = top25_df$word, 
  top.labels = c("Chardonnay", "Words", "Coffee"), 
  main = "Words in Common", 
  unit = NULL,
  gap = 8,
)