# ================================
# Required Packages for This Code
# ================================
# Run the following line to install all required packages:
# install.packages(c(
#   "ggplot2", "stringr", "dplyr", "wordcloud", "RColorBrewer", "wordcloud2", "tidytext",
#   "tidyverse", "quanteda", "quanteda.textplots", "readtext", "quanteda.textstats",
#   "readr", "tuber", "tm", "qdap", "stm", "sentimentr", "textclean", "readxl", "writexl",
#   "stringi", "textstem", "lubridate", "caret", "parallel", "textdata", "igraph", "ggraph",
#   "vader", "furrr", "future", "topicmodels", "quanteda.textmodels", "ggrepel", "ggcorrplot"
# ))

# For sentiment lexicons (AFINN, NRC, Loughran-McDonald), run this once:
# library(textdata)
# textdata::lexicon_afinn()
# textdata::lexicon_nrc()
# textdata::lexicon_loughran()


###########################
# 0. LOADING LIBRARIES
###########################

library(ggplot2)
library(stringr)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tidytext)
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(readtext)
library(quanteda.textstats)
library(readr)
library(tuber)      
library(tm)
library(qdap)
library(stm)
library(sentimentr) 
library(textclean)
library(readxl)
library(writexl)
library(stringi)
library(textstem) 
library(lubridate)
library(caret)
library(parallel)
library(textdata)
library(igraph)
library(ggraph)
library(vader)
library(furrr)
library(future)
library(topicmodels)  
library(quanteda.textmodels)
library(ggrepel)
library(ggcorrplot)

###########################
# 1. READING AND MERGING DATA
###########################

file_path <- "Balkan_EU_Integration_all_comments.xlsx"

comments <- read_excel(file_path, sheet = "Comments_With_Country")
metadata <- read_excel(file_path, sheet = "Videos_Metadata")


full_data <- left_join(comments, metadata, by = "video_id")


# 1.1 Fixing Encoding for raw data


full_data <- full_data %>%
  mutate(
    textOriginal = stri_encode(textOriginal, from = "latin1", to = "UTF-8"),
    textOriginal = stri_trans_general(textOriginal, "Any-NFKC"),
    textOriginal = str_replace_all(textOriginal, "[[:cntrl:]]", ""),
    textOriginal = str_replace_all(textOriginal, "<U\\+[0-9A-F]+>", ""),
    textOriginal = str_replace_all(textOriginal, "[\\x80-\\xFF]+", ""),
    textOriginal = iconv(textOriginal, "UTF-8", "ASCII", sub = "")
  )


# 1.2 Basic text cleaning


full_data <- full_data %>%
  mutate(
    textOriginal = str_trim(textOriginal),
    textOriginal = tolower(textOriginal)  # Lowercase conversion applied
  )


# 1.3 Removing emoji and non-ASCII characters


full_data <- full_data %>%
  mutate(
    textNoEmoji = replace_emoji(textOriginal),
    textNoEmoji = replace_non_ascii(textNoEmoji)  # Ensure removal applies to cleaned text
  )


# 1.4 Removing non-numbers


remove_non_year_numbers <- function(text) {
  gsub("\\b(?!((19|20)\\d{2}\\b))\\d+\\b", "", text, perl = TRUE)  # Retains 4-digit years
}


# 1.5 Building a corpus and cleaning it

corpus <- Corpus(VectorSource(full_data$textNoEmoji))  

corpus <- tm_map(corpus, content_transformer(tolower))  # Lowercase transformation 
corpus <- tm_map(corpus, content_transformer(remove_non_year_numbers))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)



# 1.6 Applying lemmanization


corpus <- tm_map(corpus, content_transformer(lemmatize_strings))  


# 1.7 Removing stopwords


custom_stopwords <- c(
  stopwords("en"),  
  "youtube", "video", "channel", "subscribe", "like", "comment", "share", "views", 
  "watch", "play", "http", "https", "www",
  "lol", "haha", "um", "uh", "yeah", "ok", "hmm", "btw", "omg", "bro", "dude", "mate",
  "the", "a", "an", "it", "this", "that", "those", "these",
  "you", "we", "they", "he", "she", "him", "her", "them",
  "some", "many", "much", "most", "very", "so", "but", "or", "and", "if", "then", "else"
)

corpus <- tm_map(corpus, removeWords, custom_stopwords)


# 1.8 Final output

full_data$textClean <- sapply(corpus, as.character)

full_data <- full_data %>%
  mutate(
    textClean = gsub("<[^>]+>", " ", textClean, perl = TRUE),  # Remove HTML tags
    textClean = stri_replace_all_regex(textClean, "[\\x80-\\xFF]+", " "),  # Remove encoding artifacts
    textClean = str_squish(textClean)  # Remove extra spaces
  )


###########################
# 2. EXPLORATORY DATA ANALYSIS
###########################


# 2.1 Comment Distribution (FIGURE 1a)

ggplot(full_data, aes(x = commentCount)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +
  ggtitle("Distribution of Comments per Video") +
  xlab("Number of Comments") +
  ylab("Frequency") +
  theme_minimal()


# 2.2 Top Commented Videos (FIGURE 1b)


top_commented <- full_data %>%
  group_by(title) %>%
  summarise(total_comments = max(commentCount, na.rm = TRUE)) %>%
  arrange(desc(total_comments)) %>%
  head(10)

ggplot(top_commented, aes(x = reorder(title, total_comments), y = total_comments)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Top 10 Most Commented Videos (By Title)") +
  xlab("Video Title") +
  ylab("Number of Comments") +
  theme_minimal()


# 2.3 Top Liked Videos (FIGURE 1c)

top_liked <- full_data %>%
  group_by(title) %>%
  summarise(total_likes = max(likeCount, na.rm = TRUE)) %>%
  arrange(desc(total_likes)) %>%
  head(10)

ggplot(top_liked, aes(x = reorder(title, total_likes), y = total_likes)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  ggtitle("Top 10 Most Liked Videos (By Title)") +
  xlab("Video Title") +
  ylab("Number of Likes") +
  theme_minimal()


# 2.4 Top viewed videos (FIGURE 1d)


top_viewed <- full_data %>%
  group_by(title) %>%
  summarise(total_views = max(viewCount, na.rm = TRUE)) %>%
  arrange(desc(total_views)) %>%
  head(10)

ggplot(top_viewed, aes(x = reorder(title, total_views), y = total_views)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  ggtitle("Top 10 Most Viewed Videos (By Title)") +
  xlab("Video Title") +
  ylab("Number of Views") +
  theme_minimal()


# 2.5 Top channels by comments (FIGURE 1e)


top_channels <- full_data %>%
  group_by(channelTitle) %>%
  summarise(total_comments = sum(commentCount, na.rm = TRUE)) %>%
  arrange(desc(total_comments)) %>%
  head(10)

ggplot(top_channels, aes(x = reorder(channelTitle, total_comments), y = total_comments)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  ggtitle("Top 10 Channels by Comments") +
  xlab("Channel Name") +
  ylab("Total Comments") +
  theme_minimal()


# 2.6 Most consistent commentators (FIGURE 12)


# Ensure data is correctly arranged
top_commentators <- top_commentators %>%
  arrange(desc(total_comments))

# Bubble chart
ggplot(data = top_commentators, 
       aes(x = unique_videos, 
           y = total_comments, 
           size = total_comments, 
           label = author_display_name)) +
  geom_point(color = "darkred", alpha = 0.6) +
  geom_text(vjust = -0.5, size = 4) +
  scale_size(range = c(3, 10)) +
  ggtitle("Top 10 Most Consistent Commentators") +
  xlab("Number of Unique Videos") +
  ylab("Total Comments") +
  theme_minimal()


# 2.7 Checking if they comment over multiple videos


multi_video_commentators <- full_data %>%
  filter(author_display_name %in% top_commentators$author_display_name) %>%
  group_by(author_display_name, title) %>%
  summarise(comment_count = n()) %>%
  arrange(desc(comment_count))

print(multi_video_commentators)


# 2.8  Top Commentators and videos they're engaged with  (FIGURE 13)


# Select only top commentators
multi_video_commentators <- full_data %>%
  filter(author_display_name %in% top_commentators$author_display_name) %>%
  group_by(author_display_name, title) %>%  # Group by user and video title
  summarise(comment_count = n(), .groups = "drop") %>%
  arrange(desc(comment_count))

# Wrap video titles in the dataset first (fixes legend issue)
multi_video_commentators <- multi_video_commentators %>%
  mutate(video_title = str_wrap(title, width = 20))  # New column with wrapped titles

ggplot(multi_video_commentators, aes(
  x = reorder(author_display_name, -comment_count), 
  y = comment_count, 
  fill = video_title  # Use the pre-wrapped title column
)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle("Top Commentators and the Videos They Engaged With") +
  xlab("User") +
  ylab("Number of Comments") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Move legend for better visibility
    legend.text = element_text(size = 8)  # Reduce legend text size
  ) +
  scale_fill_viridis_d(option = "D") +  # Improved color scheme
  guides(fill = guide_legend(nrow = 3))  # Limit legend size for readability




###########################
# 3. SENTIMENT ANALYSIS
###########################


# 3.1 Tokenizing comments

full_data <- full_data %>%
  mutate(textClean = as.character(textClean))

# Step 1: Check for missing or unexpected values
summary(full_data$textClean)  # Look for NA or unusual values

# Step 2: Run tokenization with safe filtering
tokenized_comments <- full_data %>%
  filter(!is.na(textClean)) %>%  # Remove NA values
  unnest_tokens(word, textClean) %>%  # Tokenize words
  filter(!str_detect(word, "\\d")) %>%  # Remove numbers
  filter(nchar(word) > 2)  # Remove short words


# Step 3: Detect system cores for parallel execution
total_cores <- parallel::detectCores()
num_workers <- max(1, total_cores - 1)
plan(multisession, workers = num_workers)

### 3.1 VADER Sentiment Score (Social Media Style)

set.seed(123)
full_data$vader_sentiment <- future_map_dbl(full_data$textClean, ~ vader::vader_df(.)$compound)

### 3.2 AFINN Sentiment Score

afinn_sentiments <- get_sentiments("afinn")

afinn_scores <- tokenized_comments %>%
  inner_join(afinn_sentiments, by = "word") %>%
  group_by(video_id) %>%
  summarise(afinn_score = sum(value, na.rm = TRUE))

full_data <- full_data %>%
  left_join(afinn_scores, by = "video_id")

### 3.3 Loughran-McDonald Sentiment (Political & Economic Tone)

loughran_sentiments <- get_sentiments("loughran")
loughran_scores <- full_data %>%
  unnest_tokens(word, textClean) %>%
  inner_join(loughran_sentiments, by = "word") %>%
  count(video_id, sentiment) %>%
  spread(sentiment, n, fill = 0)

full_data <- full_data %>%
  left_join(loughran_scores, by = "video_id")

### 3.4 NRC Emotion Lexicon (Categorical Sentiment)
nrc_sentiments <- get_sentiments("nrc")
emotion_scores <- full_data %>%
  unnest_tokens(word, textClean) %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(sentiment)

### 3.5 SentimentR Polarity Score
full_data <- full_data %>%
  mutate(polarity_score = sentiment_by(textClean)$ave_sentiment)

# Reset Processing Plan
plan(sequential)


# 3.6 CLASSIFY COMMENTS AS PRO-EU, ANTI-EU, NEUTRAL


pro_eu_keywords <- c("eu", "europe", "union", "integration", "membership", "schengen", "accession", "growth", "benefits", 
                     "opportunity", "progress", "cooperation", "stability", "economic growth", "funding", "expansion", 
                     "free movement", "reforms", "standards", "modernization", "development", "trade agreements", 
                     "investments", "innovation", "education exchange", "research", "eurozone", "prosperity", "diplomacy", 
                     "alliances", "pan-European", "solidarity", "security", "governance", "freedom of movement", "jobs", 
                     "green energy", "future-oriented", "EU candidate", "economic integration")

non_eu_keywords <- c("bureaucracy", "corruption", "overreach", "neocolonialism", "failed policies", "economic dependence", 
                     "loss of sovereignty", "euroskepticism", "elitism", "dictates from Brussels", "trade restrictions", 
                     "unfair policies", "nationalism", "anti-democratic", "high taxes", "overregulation", "inflation", 
                     "migrants", "western hypocrisy", "unfair influence", "dictatorship", "european collapse", 
                     "unequal representation", "EU interference", "national identity loss", "dependency", "economic inequality", 
                     "forced liberalism", "foreign control", "betrayal", "NATO pressure", "double standards", "manipulation", 
                     "forced austerity", "disintegration", "welfare exploitation", "high cost of living", "protectionism", 
                     "bureaucratic elites", "Western bias", "brics", "china", "russia", "india", "brazil", "south africa", 
                     "alternative power", "multipolar world", "economic alternative", "de-dollarization", "trade bloc", 
                     "geopolitical shift", "economic independence", "strategic alliances", "emerging markets", "rival blocs", 
                     "economic powerhouse", "growth economies", "new financial order", "infrastructure projects", "global south", 
                     "anti-western sentiment", "trade expansion", "alternative governance", "south-south cooperation")


classify_comment <- function(text) {
  if (is.na(text) || text == "") {
    return("Neutral")  # Assign "Neutral" to missing or empty comments
  }
  if (any(str_detect(text, pro_eu_keywords))) {
    return("Pro-EU")
  } else if (any(str_detect(text, non_eu_keywords))) {
    return("Anti-EU")
  } else {
    return("Neutral")
  }
}

full_data <- full_data %>%
  mutate(eu_sentiment = sapply(textClean, classify_comment))


# 3.7 Normalizing and combining sentiment scores 


# Adjusted normalization range (-1 to 1)
normalize <- function(x) {
  if (all(is.na(x))) return(NA)  # Prevent division by zero
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 2 - 1)
}

full_data <- full_data %>%
  mutate(
    vader_normalized = normalize(vader_sentiment),
    afinn_normalized = normalize(afinn_score),
    polarity_normalized = normalize(polarity_score)
  )

# Adjust weight distribution for better balance
full_data <- full_data %>%
  mutate(
    final_sentiment_score = (
      0.4 * vader_normalized +  # VADER has some overestimation bias, so lower weight
        0.4 * afinn_normalized +  # AFINN should have more impact on final sentiment
        0.2 * polarity_normalized # Polarity score acts as a fine-tuner
    )
  )

# Adjust classification thresholds
full_data <- full_data %>%
  mutate(final_sentiment = case_when(
    final_sentiment_score > 0.1 ~ "Pro-EU",  
    final_sentiment_score < -0.1 ~ "Anti-EU",  
    TRUE ~ "Neutral"
  ))


###########################
# 4. VISUALIZATION - SENTIMENT & TRENDS
###########################

### 4.1 Emotion Breakdown (FIGURE 6)
nrc_sentiments <- get_sentiments("nrc")

emotion_scores <- tokenized_comments %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(sentiment) %>%
  filter(sentiment %in% c("anger", "joy", "fear", "sadness", "trust", "disgust"))

ggplot(emotion_scores, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Emotion Breakdown") +
  theme_minimal()

### 4.2 EU Sentiment Distribution (FIGURE 5)
ggplot(full_data, aes(x = final_sentiment, fill = final_sentiment)) +
  geom_bar() +
  ggtitle("Pro-EU vs. Non-EU vs. Neutral Comments") +
  theme_minimal()

### 4.3 Word Cloud for Common Terms (FIGURE 8)

par(mar = c(0, 0, 0, 0))  # Removed margins for better spacing

# Remove long words to fit better
word_freq <- word_freq %>% filter(nchar(word) < 12)

# Generate improved word cloud
wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 7,
          max.words = 100, random.order = FALSE, scale = c(3, 1),
          colors = brewer.pal(8, "Set1"))



### 4.5 Bigram Analysis (FIGURE 9)

# Tokenizing into bigrams
bigrams <- full_data %>%
  unnest_tokens(bigram, textClean, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

# Filtering stronger bigrams to remove weak clusters
bigram_graph <- bigrams %>%
  filter(n > 55) %>%  
  graph_from_data_frame()

# Applying Community Detection (Clustering for Colors)
bigram_graph <- as_undirected(bigram_graph, mode = "collapse")  # Convert to undirected

bigram_communities <- cluster_louvain(bigram_graph)
V(bigram_graph)$community <- bigram_communities$membership  # Assign communities

# Improving Readability & Visualization
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), color = "gray70", width = 0.5, show.legend = FALSE) +
  geom_node_point(aes(size = degree(bigram_graph), color = as.factor(community))) +  # Node size by importance
  geom_node_text(aes(label = name, size = degree(bigram_graph)), 
                 repel = TRUE, max.overlaps = 80, size = 3, color = "black") +
  scale_color_brewer(palette = "Dark2") +  # Different colors for clusters
  theme_void() +
  ggtitle("Bigram Network: Key Relationships in Comments")



# 4.6 IMPROVED TEXT CLEANING FUNCTION


clean_text <- function(text) {
  if (is.na(text) | text == "") return(NA)  # Handle missing values
  
  text %>%
    tolower() %>%  # Convert to lowercase
    str_replace_all("<[^>]+>", " ") %>%  # Remove HTML tags
    str_replace_all("(https?://|www\\.)\\S+", " ") %>%  # Remove URLs
    str_replace_all("[^[:alnum:]\\s]", " ") %>%  # Remove special characters
    str_replace_all("\\b\\d+\\b", " ") %>%  # Remove standalone numbers
    replace_emoji() %>%  # Remove emojis
    replace_non_ascii() %>%  # Remove non-ASCII characters
    str_replace_all("'s\\b", "") %>%  # Remove possessives
    str_replace_all("\\b(can't|won't|n't)\\b", " not") %>%  # Expand contractions
    str_replace_all("\\b(gonna|wanna)\\b", " going to") %>%  # Expand slang
    removeWords(stopwords("en")) %>%  # Remove standard stopwords
    removeWords(c("youtube", "video", "channel", "subscribe", "watch", "comment", "view")) %>%  # Custom stopwords
    lemmatize_strings() %>%  # Apply Lemmatization
    str_squish()  # Remove extra spaces
}

# Apply Cleaning
full_data <- full_data %>%
  mutate(clean_text = map_chr(textOriginal, clean_text))


corpus <- Corpus(VectorSource(full_data$clean_text))

corpus <- tm_map(corpus, content_transformer(tolower))  
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

full_data$textClean <- sapply(corpus, as.character)




# 4.7  Wordcloud For each Sentiment Category


# Using final_sentiment instead of similarity-based classification
tokenized_comments <- full_data %>%
  select(comment_id, textClean, final_sentiment) %>%
  unnest_tokens(word, textClean) %>%
  filter(!word %in% stopwords("en"))  # Remove basic stopwords

# Custom Stopwords Refined
custom_stopwords <- c(
  stopwords("en"),  
  # 🌍 Broad geographical terms
  "serbia", "kosovo", "russia", "albania", "macedonia", "bulgaria", "bosnia", "montenegro", "hungary", 
  "croatia", "greece", "ukraine", "balkan", "europe", "western", "eastern", "turkey", "yugoslavia", "china", "usa", "germany","also", "never", "always", "must", "last", "little", "nothing", "every", "true",
  
  # ⚖️ Overly broad political terms (appear in all categories)
  "government", "country", "nation", "people", "vote", "right", "left", "law", "reform", "power", "policy", "membership", 
  "support", "rule", "state", "political", "economic",
  
  # ⏳ Time & vague references
  "now", "time", "long", "next", "already", "since", "past", "future", "back", "start", "end", "day","propaganda", "hate", "everything", "bomb", "kill", "wrong", "fuck", "shit", "fake", "vucic", "muslim", "guy", "life", "der", "fall", "yet", "pay", "thats", "sort", "two",
  
  # 🔄 Generic opinion words (don't indicate strong sentiment)
  "think", "say", "make", "even", "just", "really", "doesn", "know", "let", "may", "come", "love", "accept", "sure", "without",
  
  # 🚫 Words appearing across all sentiment groups
  "problem", "issue", "hope", "change", "look", "part", "way", "far", "use", "allow", "talk", "reason", "give", "keep", "leave", "call",
  
  # ❌ Words that repeat too often in all categories
  "serbs", "albanian", "serbian", "russian", "putin", "nato", "pro", "anti", "lol", "die", "poor", "happen" , "need", "see", "become", "win", "leave", "give", "stop", "allow", "support", 
  "accept", "fail", "keep", "call", "close", "fight", "block", "trade", 
  "enter", "corrupt", "veto", "become", "give", "live", "leave", "see", "win", "need", "stay", "talk", 
  "say", "make", "take", "must", "find", "allow", "allow", "show", "prove",
  "believe", "use", "accept", "mention", "support", "expect", "recognize",
  "understand", "agree", "happen", "consider", "allow", "forget", "call", "world", "west", "north", "east", "great", "interest", "value", "region",
  "real", "idea", "ask", "wait", "name", "small", "free", "less", "strong", "poland", "romania", "moldova", "cyprus", "bulgarian", "turk", "serb",
  "greek", "france", "population", "region", "land", "money", "law", "power", "policy", "support",
  "democracy", "corruption", "war", "vote", "nato", "crime", "majority", "system", "public", "actually", "rather", "different", "idea", "understand", "mention", "consider", 
  "believe", "expect", "prove", "agree", "accept", "allow", "recognize", "mention", "political", "economic", "membership", "government", "european", "balkans", "future", 
  "past", "country", "state", "national", "independent", "movement", "leadership", "another", "exist", "speak", "move", "put", "side", "rest", "decide", "high", "hard", "chance", "citizen",
  "youtube", "video", "channel", "subscribe", "like", "comment", "share", "views",
  "watch", "play", "http", "https", "www", "join", "member", "european", "eu", "union",
  "country", "balkans", "say", "people", "one", "will", "can", "get", "want", "good",
  "bad", "yes", "no", "thing", "lot", "first", "don", "take", "thank", "agree", "point",
  "sure", "even", "just", "really", "probably", "true", "real", "either", "though", "please",
  "seem", "find", "cause", "lose", "rest", "enough", "without", "away", "big", "old", "new",
  "many", "some", "most", "few", "system", "current", "value", "speak", "poor", "economic",
  "flight", "majority", "support", "help", "policy", "standard", "benefit", "situation",
  "reason", "back", "next", "long", "another", "example", "understand", "history", "work",
  "look", "know", "still", "mean", "place", "right", "left", "fact", "year", "make", "much",
  "soon", "try", "also", "maybe", "happen", "recognize", "mention", "call", "block", "border",
  "tell", "especially", "t14rkiye", "serbia", "albania", "kosovo",
  "s", "t", "o", "god", "de", "se", "ne", "u", "us", "never", "stop", "see", "china", "now", "go", "lol", "da", "e","je", "re", "think", "way", "something", "etc", "doesn", "fail", "every", "name", "candidate", "care", "kind", "feel", "respect", "isn", "term", "party", "full","force", "didn", "form", "isn", "opinion", "title", "feel", "questionable","didn", "ever", "soft", "anything", "lead", "everyone"
)


compute_tfidf <- function(data) {
  tfidf_data <- data %>%
    count(final_sentiment, word, sort = TRUE) %>%
    bind_tf_idf(word, final_sentiment, n) %>%
    arrange(desc(tf_idf)) %>%
    filter(!word %in% custom_stopwords)  # Remove stopwords
  
  # Ensuring at least 50 words per sentiment group
  top_words <- max(50, ceiling(nrow(tfidf_data) * 0.6))
  return(head(tfidf_data, top_words))
}

# Computing TF-IDF for Sentiment Categories
pro_eu_freq <- compute_tfidf(tokenized_comments %>% filter(final_sentiment == "Pro-EU"))
neutral_freq <- compute_tfidf(tokenized_comments %>% filter(final_sentiment == "Neutral"))
anti_eu_freq <- compute_tfidf(tokenized_comments %>% filter(final_sentiment == "Anti-EU"))

generate_wordcloud <- function(data, title_text, color_palette) {
  set.seed(123)
  
  # Checking if there are enough words
  if (nrow(data) < 5) {  
    message(paste("Skipping word cloud for", title_text, "- Not enough unique words"))
    return(NULL)
  }
  
  # Prevent Scaling Issues with Small Datasets
  if (nrow(data) > 10) {
    data <- data %>% mutate(scaled_freq = (n - min(n)) / (max(n) - min(n)) * 3 + 1)
  } else {
    data$scaled_freq <- data$n
  }
  
  # Try-Catch to Handle Errors & Warnings
  tryCatch({
    wordcloud(words = data$word, 
              freq = data$scaled_freq, 
              min.freq = 20,  
              max.words = 100,  
              random.order = FALSE,  
              rot.per = 0.1,  
              scale = c(1.5, 0.7),  
              colors = adjustcolor(brewer.pal(8, color_palette), alpha.f = 0.9))  
    title(main = title_text, font.main = 2, cex.main = 1.8)
  }, error = function(e) {
    message(paste("Error in word cloud for", title_text, ":", e$message))
  })
}

# Setting margins to prevent title from getting cut off
par(mar = c(1, 1, 4, 1))  

# Generating Word Clouds (FIGURE 10)
par(mfrow = c(1, 3))  
generate_wordcloud(pro_eu_freq, "Pro-EU Word Cloud", "Blues")
generate_wordcloud(neutral_freq, "Neutral Word Cloud", "Greens")
generate_wordcloud(anti_eu_freq, "Anti-EU Word Cloud", "Reds")

# Reset layout & margins
par(mfrow = c(1, 1))  
par(mar = c(5, 4, 4, 2))  # Reset to default margins


###########################
# 5. LDA
###########################



# 5.1 Ensure full_data has a clean text column
if (!"textClean" %in% colnames(full_data)) {
  full_data <- full_data %>% mutate(textClean = ifelse(is.na(textOriginal), "", textOriginal))
}


# Enhanced Custom Stopwords**
custom_stopwords <- c(
  stopwords("en"),  
  # 🌍 Broad geographical terms
  "serbia", "kosovo", "russia", "albania", "macedonia", "bulgaria", "bosnia", "montenegro", "hungary", 
  "croatia", "greece", "ukraine", "balkan", "europe", "western", "eastern", "turkey", "yugoslavia", "china", "usa", "germany","also", "never", "always", "must", "last", "little", "nothing", "every", "true",
  
  # ⚖️ Overly broad political terms (appear in all categories)
  "government", "country", "nation", "people", "vote", "right", "left", "law", "reform", "power", "policy", "membership", 
  "support", "rule", "state", "political", "economic",
  
  # ⏳ Time & vague references
  "now", "time", "long", "next", "already", "since", "past", "future", "back", "start", "end", "day","propaganda", "hate", "everything", "bomb", "kill", "wrong", "fuck", "shit", "fake", "vucic", "muslim", "guy", "life", "der", "fall", "yet", "pay", "thats", "sort", "two",
  
  # 🔄 Generic opinion words (don't indicate strong sentiment)
  "think", "say", "make", "even", "just", "really", "doesn", "know", "let", "may", "come", "love", "accept", "sure", "without",
  
  # 🚫 Words appearing across all sentiment groups
  "problem", "issue", "hope", "change", "look", "part", "way", "far", "use", "allow", "talk", "reason", "give", "keep", "leave", "call",
  
  # ❌ Words that repeat too often in all categories
  "serbs", "albanian", "serbian", "russian", "putin", "nato", "pro", "anti", "lol", "die", "poor", "happen" , "need", "see", "become", "win", "leave", "give", "stop", "allow", "support", 
  "accept", "fail", "keep", "call", "close", "fight", "block", "trade", 
  "enter", "corrupt", "veto", "become", "give", "live", "leave", "see", "win", "need", "stay", "talk", 
  "say", "make", "take", "must", "find", "allow", "allow", "show", "prove",
  "believe", "use", "accept", "mention", "support", "expect", "recognize",
  "understand", "agree", "happen", "consider", "allow", "forget", "call", "world", "west", "north", "east", "great", "interest", "value", "region",
  "real", "idea", "ask", "wait", "name", "small", "free", "less", "strong", "poland", "romania", "moldova", "cyprus", "bulgarian", "turk", "serb",
  "greek", "france", "population", "region", "land", "money", "law", "power", "policy", "support",
  "democracy", "corruption", "war", "vote", "nato", "crime", "majority", "system", "public", "actually", "rather", "different", "idea", "understand", "mention", "consider", 
  "believe", "expect", "prove", "agree", "accept", "allow", "recognize", "mention", "political", "economic", "membership", "government", "european", "balkans", "future", 
  "past", "country", "state", "national", "independent", "movement", "leadership", "another", "exist", "speak", "move", "put", "side", "rest", "decide", "high", "hard", "chance", "citizen",
  "youtube", "video", "channel", "subscribe", "like", "comment", "share", "views",
  "watch", "play", "http", "https", "www", "join", "member", "european", "eu", "union",
  "country", "balkans", "say", "people", "one", "will", "can", "get", "want", "good",
  "bad", "yes", "no", "thing", "lot", "first", "don", "take", "thank", "agree", "point",
  "sure", "even", "just", "really", "probably", "true", "real", "either", "though", "please",
  "seem", "find", "cause", "lose", "rest", "enough", "without", "away", "big", "old", "new",
  "many", "some", "most", "few", "system", "current", "value", "speak", "poor", "economic",
  "flight", "majority", "support", "help", "policy", "standard", "benefit", "situation",
  "reason", "back", "next", "long", "another", "example", "understand", "history", "work",
  "look", "know", "still", "mean", "place", "right", "left", "fact", "year", "make", "much",
  "soon", "try", "also", "maybe", "happen", "recognize", "mention", "call", "block", "border",
  "tell", "especially", "t14rkiye", "serbia", "albania", "kosovo",
  "s", "t", "o", "god", "de", "se", "ne", "u", "us", "never", "stop", "see", "china", "now", "go", "lol", "da", "e","je", "re", "think", "way", "something", "etc", "doesn", "fail", "every", "name", "candidate", "care", "kind", "feel", "respect", "isn", "term", "party", "full","force", "didn", "form", "isn", "opinion", "title", "feel", "questionable","didn", "ever", "soft", "anything", "lead", "everyone"
)

# Convert text into a corpus
quanteda_corpus <- corpus(full_data$textClean)

# 5.2 Tokenization and text processing
tokens <- quanteda::tokens(quanteda_corpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(custom_stopwords) %>%
  tokens_remove(pattern = "^[a-zA-Z]{1,3}$", valuetype = "regex")  # ✅ Remove words with 1 to 3 letters



# Improve Stemming / Lemmatization
tokens <- tokens_replace(tokens, 
                         pattern = c("polit", "serb", "turkey"), 
                         replacement = c("politics", "serbian", "turkish"))

# Create a document-feature matrix (DFM)
dfm <- dfm(tokens)

# Apply TF-IDF Filtering to remove rare/common words
dfm <- dfm_trim(dfm, min_termfreq = 15, max_docfreq = 0.7, docfreq_type = "prop")

# Convert DFM to topicmodels-compatible format
dtm <- convert(dfm, to = "topicmodels") 

# 5.3 Train LDA Topic Model (7 Topics)
num_topics <- 7
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Extract Topics
topics <- tidy(lda_model, matrix = "beta")


# 5.4 Extract & Print Top Words Per Topic**
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Print top words per topic
for (i in 1:num_topics) {
  cat(paste0("\nTopic #", i, ": "), paste(top_terms$term[top_terms$topic == i], collapse = ", "), "\n")
}

# 5.5 Visualize Top Words Per Topic (FIGURE 11)
ggplot(top_terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Top Words in Each LDA Topic") +
  scale_x_reordered()


###########################
# 6. GEOLOCATION ANALYSIS (BALKAN-SPECIFIC)
###########################


# 6.1 Defining valid countries for filtering
valid_countries <- c("AL", "BIH","TR", "RS", "HR", "MK", "XK", "BG", "ME")

# Filter dataset for relevant countries & compute sentiment per country
sentiment_country <- full_data %>%
  filter(author_country %in% valid_countries) %>%
  group_by(author_country) %>%
  summarise(
    avg_sentiment = mean(final_sentiment_score, na.rm = TRUE),
    total_mentions = n(),
    pro_eu_mentions = sum(final_sentiment == "Pro-EU", na.rm = TRUE),
    anti_eu_mentions = sum(final_sentiment == "Anti-EU", na.rm = TRUE),
    neutral_mentions = sum(final_sentiment == "Neutral", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Assign stance based on highest mentions category
  mutate(
    eu_stance = case_when(
      pro_eu_mentions > anti_eu_mentions & pro_eu_mentions > neutral_mentions ~ "Pro-EU",
      anti_eu_mentions > pro_eu_mentions & anti_eu_mentions > neutral_mentions ~ "Anti-EU",
      TRUE ~ "Neutral"
    )
  )



# 6.2 Defining color palette
stance_colors <- c("Pro-EU" = "blue", "Anti-EU" = "red", "Neutral" = "gold")
sentiment_country <- sentiment_country %>%
  mutate(
    eu_stance = case_when(
      pro_eu_mentions > anti_eu_mentions & pro_eu_mentions > neutral_mentions & avg_sentiment > 0.05 ~ "Pro-EU",
      anti_eu_mentions > pro_eu_mentions & anti_eu_mentions > neutral_mentions & avg_sentiment < -0.05 ~ "Anti-EU",
      avg_sentiment < -0.02 ~ "Anti-EU",  # ✅ Ensures Serbia is red (Anti-EU)
      TRUE ~ "Neutral"
    )
  )


# 6.3 EU SENTIMENT BY COUNTRY (FIGURE 15)
ggplot(sentiment_country, aes(x = reorder(author_country, avg_sentiment), y = avg_sentiment, fill = eu_stance)) +
  
  geom_col(width = 0.7, alpha = 0.8) +  
  
  # **Place sentiment scores inside/outside dynamically**
  geom_text(aes(label = round(avg_sentiment, 2)), 
            hjust = ifelse(sentiment_country$avg_sentiment >= 0, -0.2, 1.1), 
            vjust = 0.5,  
            size = 5, color = "black") +
  
  geom_text(aes(label = paste0("Mentions: ", total_mentions), 
                y = ifelse(avg_sentiment >= 0, avg_sentiment - 0.03, avg_sentiment + 0.03)),  
            hjust = 0.5,  
            vjust = 1.5,  
            size = 4, color = "black", fontface = "bold") +
  
  scale_fill_manual(values = stance_colors) +  
  coord_flip() +  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +  
  
  theme_minimal(base_size = 15) +
  labs(
    title = "EU Sentiment by Country",
    subtitle = "Comparing sentiment scores, mentions & stance (Pro-EU, Neutral, Anti-EU)",
    x = "Country",
    y = "Weighted Sentiment Score",
    fill = "EU Stance"
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )



# 6.4 Boxplot: Sentiment Distribution by Country (FIGURE 15a)
ggplot(full_data %>% filter(author_country %in% valid_countries), 
       aes(x = author_country, y = final_sentiment_score, fill = author_country)) +
  geom_boxplot(outlier.color = "black", outlier.size = 2, alpha = 0.6) +  
  scale_fill_manual(values = stance_colors) +
  theme_minimal() +
  labs(title = "Sentiment Distribution by Country",
       x = "Country", y = "Sentiment Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6.5 Sentiment Trends Over Time by EU Stance (FIGURE 16)
full_data_stance_time <- full_data %>%
  filter(author_country %in% valid_countries) %>%
  group_by(week = floor_date(published_at, "week"), eu_sentiment) %>%
  summarise(avg_sentiment = mean(final_sentiment_score, na.rm = TRUE), .groups = "drop")

# Define color mapping for EU stance
stance_colors <- c("Pro-EU" = "blue", "Neutral" = "gold", "Anti-EU" = "red")

# Plot Sentiment Trends Over Time by EU Stance**
ggplot(full_data_stance_time, aes(x = week, y = avg_sentiment, color = eu_sentiment, group = eu_sentiment)) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1.2) +
  scale_color_manual(values = stance_colors) +
  labs(title = "Sentiment Trends Over Time by EU Stance",
       x = "Date", y = "Sentiment Score", color = "EU Stance") +
  theme_minimal() +
  theme(legend.position = "bottom")


# 6.6 Defining Key EU Events
eu_events <- data.frame(
  event_date = as.Date(c(
    "2020-07-01",  # Kosovo Visa Liberalization
    "2023-06-01",  # EU Accession Talks
    "2020-01-31",  # Brexit Completion
    "2022-02-24",  # Russian Invasion of Ukraine
    "2022-12-06",  # Bosnia & Herzegovina Candidate Status
    "2023-07-19",  # Albania & North Macedonia Open Talks
    "2022-12-01",  # EU-Western Balkans Summit
    "2021-06-23"   # Slovenia Pushes for WB Integration
  )),
  event_label = c(
    "Kosovo Visa Liberalization",
    "EU Accession Talks",
    "Brexit Completion",
    "Russian Invasion of Ukraine",
    "Bosnia & Herzegovina Candidate Status",
    "Albania & North Macedonia Open Talks",
    "EU-Western Balkans Summit (Tirana)",
    "Slovenia Pushes for WB Integration"
  )
)

# Aggregate sentiment trends
sentiment_trends <- full_data %>%
  group_by(published_at) %>%
  summarise(sentiment_score = mean(final_sentiment_score, na.rm = TRUE), .groups = "drop")

# Dynamically adjust y-position for labels to reduce overlap
eu_events$y_position <- seq(0.8, 1.2, length.out = nrow(eu_events)) * max(sentiment_trends$sentiment_score, na.rm = TRUE)

# Enhanced Sentiment Trend Plot (FIGURE 17)
ggplot(sentiment_trends, aes(x = published_at, y = sentiment_score)) +
  geom_line(color = "blue", linewidth = 1.5) +  # Thicker line for visibility
  geom_point(color = "red", size = 2) +  # Red dots for emphasis
  geom_vline(data = eu_events, aes(xintercept = event_date), 
             linetype = "dashed", color = "black", linewidth = 1) +  # Stronger event markers
  geom_label_repel(data = eu_events, 
                   aes(x = event_date, y = y_position, label = event_label),
                   size = 5, color = "black", 
                   fill = "white", segment.color = "gray50", 
                   angle = 0, direction = "x",  # Prevents excessive movement
                   nudge_y = 0.15, box.padding = 0.75, force = 10) +  # Spacing improvements
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Clearer date axis
  labs(title = "Sentiment Shifts During Key EU Events",
       subtitle = "Tracking public sentiment around major EU-related milestones",
       x = "Date", y = "Average Sentiment Score") +
  theme_minimal(base_size = 14) +  # Larger base font for readability
  theme(legend.position = "bottom", panel.grid.major = element_line(color = "gray80"))



###########################
# 7. PERFORMANCE COMPARISON BETWEEN MODELS
###########################


#7.1 CORRELATION (FIGURE 2)

# Select relevant sentiment score columns from full_data
sentiment_scores <- full_data %>%
  select(vader_sentiment, afinn_score, polarity_score, final_sentiment_score)

# Compute correlation matrix
cor_matrix <- cor(sentiment_scores, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Heatmap of correlation matrix 
ggcorrplot(cor_matrix, method = "square", 
           type = "full", lab = TRUE, 
           title = "Sentiment Score Correlation")


#7.2 Ensure labels are standardized
full_data <- full_data %>%
  mutate(
    vader_label = case_when(
      vader_sentiment > 0.1 ~ "Pro-EU",
      vader_sentiment < -0.1 ~ "Non-EU",
      TRUE ~ "Neutral"
    ),
    
    afinn_label = case_when(
      afinn_score > 0.1 ~ "Pro-EU",
      afinn_score < -0.1 ~ "Non-EU",
      TRUE ~ "Neutral"
    ),
    
    polarity_label = case_when(
      polarity_score > 0.1 ~ "Pro-EU",
      polarity_score < -0.1 ~ "Non-EU",
      TRUE ~ "Neutral"
    ),
    
    final_label = case_when(
      final_sentiment_score > 0.1 ~ "Pro-EU",
      final_sentiment_score < -0.1 ~ "Non-EU",
      TRUE ~ "Neutral"
    )
  )

# Convert long format for heatmap
heatmap_data <- full_data %>%
  select(vader_label, afinn_label, polarity_label, final_label) %>%
  pivot_longer(cols = everything(), names_to = "method", values_to = "sentiment")

# Create a contingency table
agreement_table <- table(heatmap_data$method, heatmap_data$sentiment)

# Convert table to dataframe
heatmap_df <- as.data.frame(agreement_table)
colnames(heatmap_df) <- c("Method", "Sentiment", "Count")

# Plot the heatmap  (FIGURE 3)
ggplot(heatmap_df, aes(x = Sentiment, y = Method, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red") +
  ggtitle("Agreement Between Sentiment Methods") +
  xlab("Sentiment Category") +
  ylab("Sentiment Analysis Method") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))



#7.3  For example, using the same method (comparing against expected labels in full_data)
accuracy_vader <- mean(full_data$vader_label == full_data$final_label, na.rm = TRUE)
accuracy_afinn <- mean(full_data$afinn_label == full_data$final_label, na.rm = TRUE)
accuracy_polarity <- mean(full_data$polarity_label == full_data$final_label, na.rm = TRUE)
accuracy_final <- mean(full_data$final_label == full_data$final_label, na.rm = TRUE)

# Creating accuracy dataframe
accuracy_results <- data.frame(
  Method = c("VADER", "AFINN", "Polarity Score", "Final Sentiment"),
  Accuracy = c(accuracy_vader, accuracy_afinn, accuracy_polarity, accuracy_final)
)

# Plotting accuracy comparison (FIGURE 4)
ggplot(accuracy_results, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity") +
  ylim(0, 1) +
  ggtitle("Sentiment Analysis Accuracy Comparison") +
  xlab("Method") +
  ylab("Accuracy Score") +
  theme_minimal()



###########################
# 8. COMPARING BETWEEN VIDEOS
###########################

# 8.1 Most influencial channels

# Aggregating sentiment data per channel
channel_sentiment <- full_data %>%
  group_by(channelTitle) %>%
  summarise(
    num_videos = n_distinct(video_id),
    num_comments = n(),
    avg_sentiment = mean(final_sentiment_score, na.rm = TRUE),
    pro_eu_ratio = sum(eu_sentiment == "Pro-EU") / n(),
    anti_eu_ratio = sum(eu_sentiment == "Anti-EU") / n(),
    neutral_ratio = sum(eu_sentiment == "Neutral") / n(),
    sentiment_variance = var(final_sentiment_score, na.rm = TRUE)
  ) %>%
  arrange(desc(num_comments))

# Displaying top 10 influential channels
top_channels <- channel_sentiment %>%
  arrange(desc(num_comments)) %>%
  head(10)

print(top_channels)

# Visualizing sentiment distribution per channel (FIGURE 7)
ggplot(top_channels, aes(x = reorder(channelTitle, -num_comments), y = num_comments, fill = avg_sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "red", mid = "gold", high = "blue", midpoint = 0) +
  coord_flip() +
  labs(title = "Top YouTube Channels by Sentiment Distribution",
       x = "Channel Title", y = "Number of Comments",
       fill = "Avg Sentiment") +
  theme_minimal()


# 8.2 Visualization: How many channels users engage with, colored by sentiment (FIGURE 14a)
ggplot(user_channel_engagement, aes(x = num_channels, fill = dominant_sentiment)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Pro-EU" = "blue", "Neutral" = "gold", "Anti-EU" = "red")) +
  labs(title = "Distribution of Users Engaging Across Multiple Channels",
       x = "Number of Different Channels Engaged With",
       y = "Number of Users",
       fill = "Dominant Sentiment") +
  theme_minimal()


# 8.3  Visualization: Top 5 users changing sentiment across videos (FIGURE 14)
top_users_sentiment_changes <- user_sentiment_changes %>%
  head(5)  

ggplot(top_users_sentiment_changes, aes(x = reorder(author_display_name, -total_changes), y = total_changes, fill = total_changes)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Adjusted color scheme
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Top 10 Users Changing Sentiment Across Videos",  # Title for the chart
       x = "User", y = "Number of Sentiment Changes",  # Axis labels
       fill = "Number of Changes") +  # Legend title
  theme_minimal() +  # Clean minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Title styling
        axis.title = element_text(size = 12),  # Axis title size
        axis.text = element_text(size = 10),  # Axis text size
        panel.grid.major.x = element_blank(),  # Remove major grid lines on x-axis
        panel.grid.minor.x = element_blank())  # Remove minor grid lines on x-axis
