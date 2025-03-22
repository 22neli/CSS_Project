# Balkan-EU Sentiment Analysis in YouTube Comments 🇪🇺

This project analyzes public sentiment on **EU integration in the Balkans**, using YouTube comment data. The script performs **text cleaning, sentiment analysis (VADER, AFINN, Loughran, NRC, SentimentR)**, **topic modeling (LDA)**, and **visualization** of sentiment patterns over time, per country, and per user.

---

## 📁 Required Files

Ensure that the following Excel file is downloaded and saved locally:

- `Balkan_EU_Integration_all_comments.xlsx`  
  - Contains two sheets:  
    - `Comments_With_Country`  
    - `Videos_Metadata`

⚠️ **Important**:  
In the R script, the file path to read the data is currently set to the absolute path in my machine. For code to run, you need to replace it to you one.

📦 Required R Packages
To run this project, the following packages must be installed. You can install them all at once using:

install.packages(c(
  "ggplot2", "stringr", "dplyr", "wordcloud", "RColorBrewer", "wordcloud2", "tidytext",
  "tidyverse", "quanteda", "quanteda.textplots", "readtext", "quanteda.textstats",
  "readr", "tuber", "tm", "qdap", "stm", "sentimentr", "textclean", "readxl", "writexl",
  "stringi", "textstem", "lubridate", "caret", "parallel", "textdata", "igraph", "ggraph",
  "vader", "furrr", "future", "topicmodels", "quanteda.textmodels", "ggrepel", "ggcorrplot"
))

Also, install sentiment lexicons (only needed once):
library(textdata)
textdata::lexicon_afinn()
textdata::lexicon_nrc()
textdata::lexicon_loughran()


📊 Output Summary
Descriptive Analysis: Top videos, channels, commenters

Sentiment Analysis: Using 5 methods + hybrid score

Topic Modeling: LDA analysis with 7 topics

Trends: Time-based sentiment shifts & EU events

Geographic Analysis: Sentiment by Balkan countries

User Behavior: Consistency & sentiment shifts across videos
