# 1. Problem definition:
# 2. Identifying the text sources:

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