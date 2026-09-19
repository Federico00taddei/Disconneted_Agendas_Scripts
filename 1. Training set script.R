# CREATING TRAINING SET FOR ARTICLE YOUTH AND DEMOCRACY EU 2024

#load the xlsx file, first row  are the names of the column
library(readxl)
data <- read_excel("C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI\\DB_ElezioniEuropee_Politici_Profili Influenti.xlsx")

#put the first row as column names
colnames(data) <- data[1,]
data <- data[-1,]

unique(data$voice_type)
#[1] "GIORNALISTI/OL" "POLITICI"   

# 800 text training set --------------------------------------------------------
# So now we take 400 casual row for each voice type, and we create a dataset with only the random selection of text
# so we have 800 total row with only text casually extracted from the two category of account

library(dplyr)
set.seed(123) # for reproducibility
# Sample 400 rows for each voice type
sampled_data <- data %>%
  group_by(voice_type) %>%
  sample_n(400)
# Now we have a dataset with 800 rows, we can select only the text column
training_set <- sampled_data %>%
  select(Text)
# Now we have a training set with 800 rows and only the text column, we can save it as a csv file
write.csv(training_set, "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI\\Training set\\training_set.csv", row.names = FALSE)

# FIRST PILOT - 50 text training set --------------------------------------------------------
# So now we take 25 casual row for each voice type, and we create a dataset with only the random selection of text
# so we have 50 total row with only text casually extracted from the two category of account
# this is for the pilot training set and to adjust the coding scheme in case

library(dplyr)
set.seed(123) # for reproducibility
# Sample 400 rows for each voice type
sampled_data50 <- data %>%
  group_by(voice_type) %>%
  sample_n(25)
# Now we have a dataset with 800 rows, we can select only the text column
training_set50 <- sampled_data50 %>%
  select(Text)
# Now we have a training set with 800 rows and only the text column, we can save it as a csv file
write.csv(training_set50, "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI\\Training set\\training_set_50.csv", row.names = FALSE)

# SECOND PILOT - 50 text training set --------------------------------------------------------
# So now we take 25 casual row for each voice type, and we create a dataset with only the random selection of text
# so we have 50 total row with only text casually extracted from the two category of account
# this is for the pilot training set and to adjust the coding scheme in case

library(dplyr)
set.seed(987) # for reproducibility
# Sample 400 rows for each voice type
sampled_data50a <- data %>%
  group_by(voice_type) %>%
  sample_n(25)
# Now we have a dataset with 800 rows, we can select only the text column
training_set50a <- sampled_data50a %>%
  select(Text)
# Now we have a training set with 800 rows and only the text column, we can save it as a csv file
write.csv(training_set50a, "C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI\\Training set\\training_set_50a.csv", row.names = FALSE)



# INTERCODER RELIABILITY ANALYSIS ----------------------------------------------------
# Fleiss' Kappa & Krippendorff's Alpha for 4 Coders

# Set working directory
setwd("C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI\\Training set\\TS 50\\Second Pilot")

# Load required packages
library(readxl)
library(dplyr)
library(irr)

# LOAD DATA 
cod1 <- read.csv2("ver2_training_set_50_fede.csv")
cod2 <- read.csv2("ver2_training_set_50_ali.csv")
cod3 <- read_excel("ver2_training_set_50_fra.xlsx")
cod4 <- read.csv2("ver2_training_set_50_chia.csv")

# Standardize column names (use cod3 as reference)
colnames(cod1) <- colnames(cod3)
colnames(cod2) <- colnames(cod3)
colnames(cod4) <- colnames(cod3)

# Define the 11 category columns exactly as in cod3
categories <- c(
  "Climate Change & Environment",
  "Job Opportunities for Youth",
  "Fighting Poverty & Inequality",
  "Health Challenges",
  "Peace & International Security",
  "Human Rights & Democracy",
  "Digitalisation of Society",
  "Inclusion & Gender Equality",
  "Connecting European Youth",
  "Other",
  "Don't Know / Uncodable"
)

# Helper: interpret reliability values (Landis & Koch, 1977)
interpret_reliability <- function(value) {
  if (is.na(value)) return("NA")
  if (value < 0) return("Poor")
  if (value < 0.21) return("Slight")
  if (value < 0.41) return("Fair")
  if (value < 0.61) return("Moderate")
  if (value < 0.81) return("Substantial")
  "Almost Perfect"
}

# Ensure all categories exist in all coder data frames
stopifnot(all(categories %in% colnames(cod1)))
stopifnot(all(categories %in% colnames(cod2)))
stopifnot(all(categories %in% colnames(cod3)))
stopifnot(all(categories %in% colnames(cod4)))

# Fleiss' Kappa (4 coders) ---------------------------------------------------

fleiss_results <- data.frame(
  Category = character(),
  Fleiss_Kappa = numeric(),
  SE = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  Agreement_Level = character(),
  stringsAsFactors = FALSE
)

safe1 <- function(x) {
  # Return a single numeric value or NA if length is 0 or NULL
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)[1]
}

for (cat in categories) {
  # Build ratings data frame: rows = texts, cols = coders
  ratings_df <- data.frame(
    Fede   = as.numeric(as.character(cod1[[cat]])),
    Ali    = as.numeric(as.character(cod2[[cat]])),
    Fra    = as.numeric(as.character(cod3[[cat]])),
    Chiara = as.numeric(as.character(cod4[[cat]]))
  )
  
  # Drop rows with any NA for this category
  ratings_df <- ratings_df[complete.cases(ratings_df), ]
  
  # If too few items remain, record NA and skip computation
  if (nrow(ratings_df) < 2) {
    fleiss_results <- rbind(
      fleiss_results,
      data.frame(
        Category = cat,
        Fleiss_Kappa = NA_real_,
        SE = NA_real_,
        z_value = NA_real_,
        p_value = NA_real_,
        Agreement_Level = "NA",
        stringsAsFactors = FALSE
      )
    )
    next
  }
  
  # Ensure values are treated as nominal categories (0/1 as factors)
  ratings_df <- as.data.frame(lapply(ratings_df, function(x) factor(x, levels = c(0, 1))))
  
  # Compute Fleiss' Kappa with safety
  fk <- tryCatch(irr::kappam.fleiss(ratings_df), error = function(e) NULL)
  
  # If computation failed, record NA; otherwise extract safely
  if (is.null(fk)) {
    fleiss_results <- rbind(
      fleiss_results,
      data.frame(
        Category = cat,
        Fleiss_Kappa = NA_real_,
        SE = NA_real_,
        z_value = NA_real_,
        p_value = NA_real_,
        Agreement_Level = "NA",
        stringsAsFactors = FALSE
      )
    )
  } else {
    fk_value <- safe1(fk$value)
    fk_se <- safe1(fk$se)
    fk_z <- safe1(fk$statistic)
    fk_p <- safe1(fk$p.value)
    
    fleiss_results <- rbind(
      fleiss_results,
      data.frame(
        Category = cat,
        Fleiss_Kappa = fk_value,
        SE = fk_se,
        z_value = fk_z,
        p_value = fk_p,
        Agreement_Level = interpret_reliability(fk_value),
        stringsAsFactors = FALSE
      )
    )
  }
}

# Krippendorff's Alpha (nominal) ----------------------------------------------
krippendorff_results <- data.frame(
  Category = character(),
  Krippendorff_Alpha = numeric(),
  Agreement_Level = character(),
  stringsAsFactors = FALSE
)

for (cat in categories) {
  # Matrix format required: rows = raters, cols = items
  ratings_mat <- rbind(
    Fede   = as.numeric(as.character(cod1[[cat]])),
    Ali    = as.numeric(as.character(cod2[[cat]])),
    Fra    = as.numeric(as.character(cod3[[cat]])),
    Chiara = as.numeric(as.character(cod4[[cat]]))
  )
  
  # Compute Krippendorff's Alpha (nominal)
  ka <- irr::kripp.alpha(ratings_mat, method = "nominal")
  
  krippendorff_results <- rbind(
    krippendorff_results,
    data.frame(
      Category = cat,
      Krippendorff_Alpha = ka$value,
      Agreement_Level = interpret_reliability(ka$value),
      stringsAsFactors = FALSE
    )
  )
}

# Combined summary --------------------------------------------------------------
combined_results <- merge(
  fleiss_results,
  krippendorff_results,
  by = "Category"
)

# Print summaries
print(fleiss_results, row.names = FALSE)
print(krippendorff_results, row.names = FALSE)
print(combined_results[order(combined_results$Category), ], row.names = FALSE)

#save combined results as xlxs file
library(writexl)
write_xlsx(combined_results, "results.xlsx")

# Overall averages
overall_fleiss <- mean(fleiss_results$Fleiss_Kappa, na.rm = TRUE)
overall_kripp  <- mean(krippendorff_results$Krippendorff_Alpha, na.rm = TRUE)

# Plot comparing Fleiss' κ vs. Krippendorff's α per category
# Load plotting libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data (handle NaN for κ)
plot_df <- combined_results %>%
  transmute(
    Category,
    Fleiss_Kappa = ifelse(is.nan(Fleiss_Kappa), NA_real_, Fleiss_Kappa),
    Krippendorff_Alpha
  ) %>%
  pivot_longer(
    cols = c(Fleiss_Kappa, Krippendorff_Alpha),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = dplyr::recode(Metric,
                           Fleiss_Kappa = "Fleiss' κ",
                           Krippendorff_Alpha = "Krippendorff's α"),
    Category = factor(Category, levels = categories)
  )

# Define threshold lines
thresholds <- data.frame(
  y = c(0.800, 0.667, 0.810, 0.610),
  type = c("Alpha thresholds", "Alpha thresholds", "Kappa thresholds", "Kappa thresholds")
)

alpha_color <- "#8B0000"   # dark red
kappa_color <- "#1f77b4"   # blue (matches κ bars)

# Create plot with threshold lines
p <- ggplot(plot_df, aes(x = Category, y = Value, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, na.rm = TRUE) +
  # Dotted threshold lines
  geom_hline(
    data = thresholds %>% filter(type == "Alpha thresholds"),
    aes(yintercept = y, color = type),
    linetype = "dashed", size = 0.7
  ) +
  geom_hline(
    data = thresholds %>% filter(type == "Kappa thresholds"),
    aes(yintercept = y, color = type),
    linetype = "dashed", size = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.1, 1.0), breaks = seq(0, 1.0, by = 0.1)) +
  scale_fill_manual(values = c("Fleiss' κ" = "#1f77b4", "Krippendorff's α" = "#d62728")) +
  scale_color_manual(
    values = c("Alpha thresholds" = alpha_color, "Kappa thresholds" = kappa_color),
    name = "Threshold lines"
  ) +
  labs(
    title = "Intercoder Reliability by Category",
    subtitle = "Comparison of Fleiss' κ and Krippendorff's α with dotted threshold lines",
    x = "Category",
    y = "Reliability",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.y = element_blank()
  )

# Display the plot
print(p)

# Agreement and disagreement on the df -----------------------------------------

# now we create a df with the agreement and disagreement between the coders,
# so we can see how many times they agreed and disagreed on the coding of the text for each category
# to do so we want to sum the value in every cell of the 4 datasets (since they have the same structures)
# plus presenrving all the Notes column and the Text column, so we can see the text and the notes of the coders for each text
# we can do this by using the rowSums function, we are sure that the value in the cells are 0 or 1, so we can sum them and see how many times they agreed (4) and how many times they disagreed (0,1,2,3)

# Create per-text, per-category agreement counts across 4 coders,
# while preserving the Text and all coders' Notes columns.

# Assumes cod1, cod2, cod3, cod4 and 'categories' are already loaded and column names standardized.

library(dplyr)
library(tidyr)

# Compute per-row sums (0..4) for each category across the 4 coders
sum_df <- as.data.frame(
  sapply(
    categories,
    function(cat) {
      rowSums(
        cbind(
          as.numeric(as.character(cod1[[cat]])),
          as.numeric(as.character(cod2[[cat]])),
          as.numeric(as.character(cod3[[cat]])),
          as.numeric(as.character(cod4[[cat]]))
        ),
        na.rm = TRUE
      )
    }
  ),
  check.names = FALSE
)

# Build output with Text and Notes per coder
out_df <- cbind(
  data.frame(Text = cod3$`Text`, check.names = FALSE),
  sum_df,
  data.frame(
    Notes_Fede   = cod1$`Notes`,
    Notes_Ali    = cod2$`Notes`,
    Notes_Fra    = cod3$`Notes`,
    Notes_Chiara = cod4$`Notes`,
    check.names = FALSE
  )
)

# Preview and save
print(head(out_df, 10))
#write.csv(out_df, "ver2_agreement_df_with_notes.csv", row.names = FALSE)
