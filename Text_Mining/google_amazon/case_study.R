# 1. Problem definition:
# 2. Identifying the text sources:
library(qdap)
library(tm)
library(RWeka)
library(wordcloud)
amzn <- read_csv("Datasets/amazon_reviews.csv")
goog <- read_csv("Datasets/google_glassdoor_reviews.csv")

# Print the structure of amzn
str(amzn)
# Create amzn_pros
amzn_pros <- amzn$pros
# Create amzn_cons
amzn_cons <- amzn$cons
# Print the structure of goog
str(goog)
# Create goog_pros
goog_pros <- goog$pros
# Create goog_cons
goog_cons <- goog$cons


# qdap cleaning function
qdap_clean <- function(x){  
  x <- replace_abbreviation(x)  
  x <- replace_contraction(x)  
  x <- replace_number(x)  
  x <- replace_ordinal(x)  
  x <- replace_symbol(x) 
  x <- na.omit(x)
  x <- tolower(x)
    return(x)
}

# tm_cleaning function
tm_clean <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "Google", "Amazon", "company"))
  return(corpus)
}

# a tokenizer function
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

# 1. Cleaning the amazon dataset by applying the cleaning functions
# qdap_clean the text
qdap_cleaned_amzn_pros <- qdap_clean(amzn_pros)
# Source and create the corpus
amzn_p_corp <- VCorpus(VectorSource(qdap_cleaned_amzn_pros))
# tm_clean the corpus
amzn_pros_corp <- tm_clean(amzn_p_corp)

# qdap_clean the text
qdap_cleaned_amzn_cons <- qdap_clean(amzn_cons)
# Source and create the corpus
amzn_c_corp <- VCorpus(VectorSource(qdap_cleaned_amzn_cons))
# tm_clean the corpus
amzn_cons_corp <- tm_clean(amzn_c_corp)

# 2. Cleaning the google dataset by applying the cleaning functions

# qdap_clean the text
qdap_cleaned_goog_pros <- qdap_clean(goog_pros)
# Source and create the corpus
goog_p_corp <-VCorpus(VectorSource(qdap_cleaned_goog_pros))
# tm_clean the corpus
goog_pros_corp <- tm_clean(goog_p_corp)

# qdap clean the text
qdap_cleaned_goog_cons <- qdap_clean(goog_cons)
# Source and create the corpus
goog_c_corp <- VCorpus(VectorSource(qdap_cleaned_goog_cons))
# tm clean the corpus
goog_cons_corp <- tm_clean(goog_c_corp)

# 3. Feature extraction and Analysis 
# Create amzn_p_tdm
amzn_p_tdm <- TermDocumentMatrix(
  amzn_pros_corp,
  control = list(tokenize = tokenizer)
)

# Create amzn_p_tdm_m
amzn_p_tdm_m <- as.matrix(amzn_p_tdm)
# Create amzn_p_freq
amzn_p_freq <- rowSums(amzn_p_tdm_m)
# Plot a word cloud using amzn_p_freq values
wordcloud(words = names(amzn_p_freq), freq = amzn_p_freq, max.words = 25, color = "blue" )

# Create amzn_c_tdm
amzn_c_tdm <- TermDocumentMatrix(
  amzn_cons_corp,
  control = list(tokenize = tokenizer)
)

# Create amzn_c_tdm_m
amzn_c_tdm_m <- as.matrix(amzn_c_tdm)
# Create amzn_c_freq
amzn_c_freq <- rowSums(amzn_c_tdm_m)
# Plot a word cloud of negative Amazon bigrams
wordcloud(words = names(amzn_c_freq), freq = amzn_c_freq, max.words = 25, color = "red")

# Create amzn_c_tdm
amzn_c_tdm <- TermDocumentMatrix(
  amzn_cons_corp,
  control = list(tokenize = tokenizer)
)

# Print amzn_c_tdm to the console
print(amzn_c_tdm)
# Create amzn_c_tdm2 by removing sparse terms 
amzn_c_tdm2 <- removeSparseTerms(amzn_c_tdm, sparse = 0.993)
# Create hc as a cluster of distance values
hc <- hclust(dist(amzn_c_tdm2),
             method = "complete")
# Produce a plot of hc
plot(hc)


