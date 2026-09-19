# Script Topic Analysis
# Anlysis Disconnected Agendas? Youth Priorities, Social Media, and Candidate Agendas in the 2024 European Elections 
# Federico Taddei

# ============================================================
# ARTICLE: Anlysis Disconnected Agendas? Youth Priorities, Social Media, and Candidate Agendas in the 2024 European Elections
# SUPERVISED TOPIC CLASSIFICATION WITH RANDOM FOREST
# ============================================================

rm(list = ls())

# 1. PACKAGES ============================================================

required_packages <- c(
  "tidyverse",
  "readxl",
  "readr",
  "stringi",
  "tm",
  "text2vec",
  "randomForest",
  "caret",
  "ggplot2",
  "syuzhet"
)

for (package_name in required_packages) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  library(package_name, character.only = TRUE)
}

# 2. FILE PATHS ============================================================

test_path <- paste0(
  "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)",
  "\\ARTICOLI\\Articoli Accademici",
  "\\File di Alice Sanarico - Articolo youth, EU elections and news",
  "\\ANALISI\\DB_ElezioniEuropee_Politici_Profili Influenti.xlsx"
)

training_path <- paste0(
  "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)",
  "\\ARTICOLI\\Articoli Accademici",
  "\\File di Alice Sanarico - Articolo youth, EU elections and news",
  "\\ANALISI\\Training set\\FINAL TS 800.csv"
)

output_directory <- paste0(
  "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)",
  "\\ARTICOLI\\Articoli Accademici",
  "\\File di Alice Sanarico - Articolo youth, EU elections and news",
  "\\ANALISI"
)

# 3. IMPORT DATA ============================================================

test_set <- read_excel(test_path)

# The first row contains the column names
colnames(test_set) <- as.character(unlist(test_set[1, ]))
test_set <- test_set[-1, ]

training_set <- read_csv2(
  training_path,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Make sure the text variable is character
test_set$Text <- as.character(test_set$Text)
training_set$Text <- as.character(training_set$Text)

# 4. TEXT PREPROCESSING ============================================================

remove_emoji <- function(text) {
  
  text <- stringi::stri_replace_all_regex(
    text,
    "[\\p{So}\\p{Cn}\\p{Sk}\\p{Lm}\\p{Ps}\\p{Pe}\\p{Cf}]",
    "",
    vectorize_all = FALSE
  )
  
  text <- stringi::stri_replace_all_regex(
    text,
    "[\\x{1F600}-\\x{1F64F}]|
     [\\x{1F300}-\\x{1F5FF}]|
     [\\x{1F680}-\\x{1F6FF}]|
     [\\x{2600}-\\x{26FF}]|
     [\\x{2700}-\\x{27BF}]",
    "",
    vectorize_all = FALSE
  )
  
  return(text)
}

preprocess_text <- function(text) {
  
  text[is.na(text)] <- ""
  
  text <- stringi::stri_trans_tolower(text)
  text <- remove_emoji(text)
  text <- stringi::stri_trans_general(text, "Latin-ASCII")
  text <- stringi::stri_replace_all_regex(text, "[[:punct:]]", " ")
  text <- stringi::stri_replace_all_regex(text, "[0-9]", " ")
  text <- tm::removeWords(text, tm::stopwords("it"))
  text <- stringi::stri_replace_all_regex(text, "\\s+", " ")
  text <- stringi::stri_trim_both(text)
  
  return(text)
}

test_set$clean_text <- preprocess_text(test_set$Text)
training_set$clean_text <- preprocess_text(training_set$Text)

# 5. TOPIC LABELS ============================================================

# Category 9, "Connecting European Youth", was excluded
# from the final classification analysis.

topic_labels <- c(
  "1"  = "Environment & Climate",
  "2"  = "Youth Employment",
  "3"  = "Poverty & Inequality",
  "4"  = "Health",
  "5"  = "Peace & Security",
  "6"  = "Democracy & Human Rights",
  "7"  = "Digitalisation",
  "8"  = "Inclusion & Equality",
  "10" = "Other",
  "11" = "Don't Know / Uncodable"
)

active_topic_codes <- names(topic_labels)

# 6. CREATE TOPIC VARIABLE FROM ONE-HOT COLUMNS ============================================================

# The topic columns are columns 2:12 in the original training set
topic_columns <- names(training_set)[2:12]

# Convert one-hot coding into a single topic code
one_hot_matrix <- as.matrix(training_set[, topic_columns])

# Make sure values are numeric
one_hot_matrix <- apply(
  one_hot_matrix,
  2,
  function(x) as.numeric(as.character(x))
)

# Identify the position of the value 1 in each row
topic_position <- max.col(
  one_hot_matrix,
  ties.method = "first"
)

# Convert position into the original topic code
# Positions correspond to topic codes 1:11
training_set$topic_code <- topic_position

# Exclude category 9
training_set <- training_set %>%
  filter(topic_code %in% as.numeric(active_topic_codes))

# Convert topic into a factor with the desired category levels
training_set$topic <- factor(
  training_set$topic_code,
  levels = as.numeric(active_topic_codes)
)

# Check topic distribution
cat("\nTopic distribution after excluding category 9:\n")
print(table(training_set$topic))

# Check that every active category has at least two observations
topic_counts <- table(training_set$topic)

if (any(topic_counts < 2)) {
  warning(
    "At least one category has fewer than two observations. ",
    "Stratified validation may be unreliable."
  )
}

# 7. VALIDATION - Stratified 80/20 held-out validation ============================================================

set.seed(12345)

validation_index <- caret::createDataPartition(
  y = training_set$topic,
  p = 0.80,
  list = FALSE
)

train_validation <- training_set[validation_index, ]
test_validation <- training_set[-validation_index, ]

train_validation$topic <- factor(
  train_validation$topic,
  levels = as.numeric(active_topic_codes)
)

test_validation$topic <- factor(
  test_validation$topic,
  levels = as.numeric(active_topic_codes)
)

cat("\nTraining partition:\n")
print(table(train_validation$topic))

cat("\nValidation partition:\n")
print(table(test_validation$topic))

# 8. DTM FOR VALIDATION - Vocabulary is learned only from the training partition ============================================================

it_train_validation <- text2vec::itoken(
  train_validation$clean_text,
  progressbar = FALSE
)

vocabulary_validation <- text2vec::create_vocabulary(
  it_train_validation
)

vectorizer_validation <- text2vec::vocab_vectorizer(
  vocabulary_validation
)

train_dtm_validation <- text2vec::create_dtm(
  it_train_validation,
  vectorizer_validation
)

it_test_validation <- text2vec::itoken(
  test_validation$clean_text,
  progressbar = FALSE
)

test_dtm_validation <- text2vec::create_dtm(
  it_test_validation,
  vectorizer_validation
)

# Ensure identical columns in the two matrices
test_dtm_validation <- test_dtm_validation[
  ,
  colnames(train_dtm_validation),
  drop = FALSE
]

# 9. RANDOM FOREST FOR VALIDATION ============================================================

set.seed(12345)

rf_validation <- randomForest::randomForest(
  x = as.matrix(train_dtm_validation),
  y = train_validation$topic,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(train_dtm_validation)))
  ),
  importance = TRUE
)

# 10. VALIDATION PREDICTIONS ============================================================

predicted_validation <- predict(
  rf_validation,
  newdata = as.matrix(test_dtm_validation)
)

predicted_validation <- factor(
  predicted_validation,
  levels = as.numeric(active_topic_codes)
)

observed_validation <- factor(
  test_validation$topic,
  levels = as.numeric(active_topic_codes)
)

# 11. CONFUSION MATRIX ============================================================

confusion_matrix <- caret::confusionMatrix(
  data = predicted_validation,
  reference = observed_validation,
  mode = "everything"
)

print(confusion_matrix)

# Extract confusion matrix
cm_matrix <- as.matrix(confusion_matrix$table)

# Make sure rows and columns follow the same topic order
class_codes <- active_topic_codes

cm_matrix <- cm_matrix[
  class_codes,
  class_codes,
  drop = FALSE
]

# Rows = predicted classes
# Columns = actual/reference classes

true_positive <- diag(cm_matrix)
predicted_total <- rowSums(cm_matrix)
actual_total <- colSums(cm_matrix)

# Precision
precision <- true_positive / predicted_total

# Recall
recall <- true_positive / actual_total

# Avoid undefined values
precision[is.na(precision)] <- 0
recall[is.na(recall)] <- 0

# F1
f1 <- ifelse(
  precision + recall == 0,
  0,
  2 * precision * recall / (precision + recall)
)

# Support
support <- actual_total

# 12. TABLE C5: CLASSIFIER PERFORMANCE BY CATEGORY ============================================================

table_C5 <- data.frame(
  Category = unname(topic_labels[class_codes]),
  Precision = round(precision, 3),
  Recall = round(recall, 3),
  F1 = round(f1, 3),
  Support = as.numeric(support),
  row.names = NULL
)

print(table_C5)

# 13. OVERALL ACCURACY, MACRO F1 AND WEIGHTED F1 ============================================================

overall_accuracy <- sum(true_positive) / sum(cm_matrix)

macro_f1 <- mean(f1, na.rm = TRUE)

weighted_f1 <- sum(
  f1 * support,
  na.rm = TRUE
) / sum(support)

validation_summary <- data.frame(
  Metric = c(
    "Overall accuracy",
    "Macro F1",
    "Weighted F1"
  ),
  Value = round(
    c(
      overall_accuracy,
      macro_f1,
      weighted_f1
    ),
    3
  )
)

print(validation_summary)

# Majority-class baseline
majority_baseline <- max(support) / sum(support)

cat(
  "\nMajority-class baseline:",
  round(majority_baseline, 3),
  "\n"
)

# 14. EXPORT VALIDATION RESULTS ============================================================

write.csv(
  table_C5,
  file.path(
    output_directory,
    "Table_C5_classifier_performance_by_category.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  validation_summary,
  file.path(
    output_directory,
    "classifier_validation_summary.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 15. FIGURE C4: CONFUSION MATRIX ============================================================

cm_plot_data <- as.data.frame(confusion_matrix$table)

names(cm_plot_data) <- c(
  "Predicted",
  "Actual",
  "Frequency"
)

cm_plot_data$Predicted <- factor(
  cm_plot_data$Predicted,
  levels = class_codes,
  labels = unname(topic_labels[class_codes])
)

cm_plot_data$Actual <- factor(
  cm_plot_data$Actual,
  levels = class_codes,
  labels = unname(topic_labels[class_codes])
)

figure_C4 <- ggplot(
  cm_plot_data,
  aes(
    x = Actual,
    y = Predicted,
    fill = Frequency
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = Frequency),
    color = "black",
    size = 3
  ) +
  scale_fill_gradient(
    low = "white",
    high = "steelblue"
  ) +
  labs(
    title = "Figure C4. Confusion matrix",
    x = "Actual category",
    y = "Predicted category",
    fill = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(
      size = 9
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

print(figure_C4)

ggsave(
  filename = file.path(
    output_directory,
    "Figure_C4_confusion_matrix.png"
  ),
  plot = figure_C4,
  width = 12,
  height = 10,
  dpi = 300
)

# 16. FINAL MODEL ON THE COMPLETE TRAINING SET ============================================================

# Build the vocabulary only on the complete training set
it_training_final <- text2vec::itoken(
  training_set$clean_text,
  progressbar = FALSE
)

vocabulary_final <- text2vec::create_vocabulary(
  it_training_final
)

vectorizer_final <- text2vec::vocab_vectorizer(
  vocabulary_final
)

train_dtm_final <- text2vec::create_dtm(
  it_training_final,
  vectorizer_final
)

# Apply the training vocabulary to the test set
it_test_final <- text2vec::itoken(
  test_set$clean_text,
  progressbar = FALSE
)

test_dtm_final <- text2vec::create_dtm(
  it_test_final,
  vectorizer_final
)

# Ensure the same variables are present in both DTMs
test_dtm_final <- test_dtm_final[
  ,
  colnames(train_dtm_final),
  drop = FALSE
]

# 17. TRAIN FINAL RANDOM FOREST ============================================================

set.seed(12345)

rf_final <- randomForest::randomForest(
  x = as.matrix(train_dtm_final),
  y = training_set$topic,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(train_dtm_final)))
  ),
  importance = TRUE
)

# 18. CLASSIFY TEST SET ============================================================

test_set$predicted_topic_code <- as.character(
  predict(
    rf_final,
    newdata = as.matrix(test_dtm_final)
  )
)

test_set$predicted_topic_code <- as.numeric(
  test_set$predicted_topic_code
)

test_set$predicted_topic_label <- unname(
  topic_labels[
    as.character(test_set$predicted_topic_code)
  ]
)

# Check predicted topic distribution
cat("\nPredicted topic distribution in test set:\n")
print(table(test_set$predicted_topic_code))

# Number of unique texts
cat(
  "\nNumber of unique cleaned texts:",
  length(unique(test_set$clean_text)),
  "\n"
)

# 19. SENTIMENT ANALYSIS ============================================================

test_set$sentiment_text <- syuzhet::get_sentiment(
  test_set$Text,
  method = "afinn",
  language = "italian"
)

test_set$sentiment_clean_text <- syuzhet::get_sentiment(
  test_set$clean_text,
  method = "afinn",
  language = "italian"
)

# Sentiment direction
test_set$sentiment_direction_text <- sign(
  test_set$sentiment_text
)

test_set$sentiment_direction_clean_text <- sign(
  test_set$sentiment_clean_text
)

cat("\nSentiment distribution based on original text:\n")
print(table(test_set$sentiment_direction_text))

cat("\nSentiment distribution based on cleaned text:\n")
print(table(test_set$sentiment_direction_clean_text))

# 20. EXPORT FINAL DATASET ============================================================

final_dataset_path <- file.path(
  output_directory,
  "FINAL_DF.csv"
)

readr::write_csv2(
  test_set,
  final_dataset_path
)

cat(
  "\nFinal dataset saved to:\n",
  final_dataset_path,
  "\n"
)