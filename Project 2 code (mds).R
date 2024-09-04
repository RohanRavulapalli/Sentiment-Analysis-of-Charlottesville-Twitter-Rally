setwd("C:\\Users\\rravu\\OneDrive\\Desktop\\CMDA 2014 Rohan\\Project 2")
library(rio)
Dat <- import("aug15_sample.xlsx", which = 1)

Dat$id <- as.character(Dat$id)

Dat$user_id <- as.character(Dat$user_id)

library(dplyr)

Dat <- Dat %>%
  mutate(first4id = substr(user_id, 1, 4))

# Set a seed for reproducibility
set.seed(123)

# Get the number of rows in the dataset
num_rows <- nrow(Dat)

# Randomly select 500 row indices
sample_indices <- sample(num_rows, 1000)

# Create the sample dataset
sample_t <- data.frame(ID = Dat$first4id[sample_indices],
                             Full_text = Dat$full_text[sample_indices],
                             stringsAsFactors = FALSE)

library(tm)
library(wordcloud)
corpT <- Corpus(VectorSource(sample_t$Full_text))

inspect(corpT[1:2])

# first two documents of corpus

corpT <- tm_map(corpT, content_transformer(tolower))

corpT <- tm_map(corpT, removeNumbers)

corpT <- tm_map(corpT, removeWords, stopwords("english"))

corpT <- tm_map(corpT, removePunctuation)

corpT <- tm_map(corpT, stripWhitespace)

remove_https_terms <- content_transformer(function(x) {
  x <- gsub("\\bhttps\\S*\\b", "", x)
  x <- removePunctuation(x)
  x <- removeNumbers(x)
  x <- stripWhitespace(x)
  return(x)
})

corpT <- tm_map(corpT, remove_https_terms)

corpT <- tm_map(corpT, removeWords, c("@", "Charlottesville"))

corpT <- tm_map(corpT, removeWords, c("charlottesvill"))

corpT <- tm_map(corpT, removeWords, c("#"))

corpT <- tm_map(corpT, stemDocument)

corpT <- tm_map(corpT, removeWords, c("impeachtrump", "realdonaldtrump"))

corpT <- tm_map(corpT, removeWords, c("rightmakesright"))

TDMt <- TermDocumentMatrix(corpT)

TDMtm <- as.matrix(TDMt)

# shows the first 4 rows (terms) and their frequencies for the first 4
# documents in the term document matrix

dim(TDMtm)

# dimensions are 2103 rows x 500 columns. In TDM words, 2103 terms x 500 documents

tfidf <- TermDocumentMatrix(corpT, control = list(weighting = function(x) {
  weightTfIdf(x)}))

tfidf <- as.matrix(tfidf)

tfidftot <- rowSums(tfidf)

# calculates total tfidfs for each term across the 500 sampled documents

ord <- order(tfidftot, decreasing = TRUE)

# creates object ord, that orders tfidf totals by decreasing order

tfidforder <- tfidf[ord, ]

# since the rows are the ones being summed up, we must include ord in the
# row portion

rownames(tfidforder)[1:10]

# showcases the top 10 words that are most relevant across the 50 documents

tfidfTr <- t(tfidforder)
# transposes the tfidf order matrix by making the rows columns

#MDS plot

library(MASS)

Point_names <- sample_t$ID

distTFIDF <- dist(tfidfTr)

fit <- cmdscale(distTFIDF, k = 2)

# Perform k-means clustering
k <- 6  # Number of clusters (adjust as needed)
kmeans_result <- kmeans(fit, centers = k)

# Get cluster assignments
cluster_labels <- kmeans_result$cluster

# Define colors for each cluster
cluster_colors <- c("red", "turquoise", "green", "orange", "pink","purple")  # Adjust colors as needed

# Plot MDS with colored clusters
plot(fit, xlab = "Coordinate 1", ylab = "Coordinate 2", pch = 16, col = cluster_colors[cluster_labels])
legend("topright", legend = paste("Cluster", 1:k), col = cluster_colors, pch = 19)
text(fit + 0.16, labels = Point_names, cex = 0.62)

tfidfTr[, 2]
orig_dists = as.vector(upper.tri(distTFIDF))
config_dists = as.vector(upper.tri(fit))
diff = (orig_dists - config_dists)^2
stress = sum(diff)
stress_value = stress/sum(orig_dists^2)

library(factoextra)
table(kmeans_result$cluster)

print(stress_value)

findFunc("stress")

# Accesses 2nd column (2nd term) of tfidf transposed matrix