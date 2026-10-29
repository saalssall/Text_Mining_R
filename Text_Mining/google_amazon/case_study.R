# 1. Problem definition:
# 2. Identifying the text sources:
library(qdap)
library(tm)
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