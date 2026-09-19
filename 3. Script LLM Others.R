# LLM MODELING ON OTHER CATEGORY

setwd("C:\\Users\\39333\\OneDrive - Università degli Studi di Milano (1)\\ARTICOLI\\Articoli Accademici\\File di Alice Sanarico - Articolo youth, EU elections and news\\ANALISI")
df = read.csv2("FINAL_DF.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")

# keep only those rows in which predicted_topic is 10
df <- df[df$predicted_topic == 10, ]
unique(df$predicted_topic)

# select and show 10 random text form "Text" variable
set.seed(123)
sample_texts <- sample(df$Text, 10)
cat("Sample texts:\n")
for (i in seq_along(sample_texts)) {
  cat(sprintf("[%d] %s\n", i, sample_texts[i]))
}


# Check ollama
library(httr2)
request("http://localhost:11434/api/tags") |> req_perform()
# if 200 is OK

# Topic Discovery con Ollama in RStudio =========================================================

needed <- c("httr2", "jsonlite", "readr", "dplyr", "purrr", "stringr", "tibble")
to_install <- needed[!needed %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(httr2)
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# Config -----------------------------
input_csv  <- df
output_csv <- "df_topics_discovered.csv"
text_col   <- "Text"
id_col     <- "id"

ollama_url   <- "http://localhost:11434/api/generate"
ollama_model <- "qwen2.5:7b"

# soglie
min_topic_freq <- 20     # topic con meno di 20 post = candidati merge
coverage_target <- 0.93  # quanti post vogliamo coprire coi topic principali

# Utility ---------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

to_utf8_safe <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # converte encoding locale/ignoto -> UTF-8
  y <- iconv(x, from = "", to = "UTF-8", sub = " ")
  y[is.na(y)] <- ""
  y
}

clean_text <- function(x) {
  x <- to_utf8_safe(x)
  x <- enc2utf8(x)
  # rimuove caratteri di controllo ASCII senza usare \u0000
  x <- gsub("[[:cntrl:]]", " ", x, perl = TRUE)
  x <- stringr::str_squish(x)
  x
}

safe_parse_json <- function(x) {
  x <- to_utf8_safe(x)
  out <- tryCatch(fromJSON(x), error = function(e) NULL)
  if (!is.null(out)) return(out)
  
  # prova a estrarre il primo blocco {...}
  m <- str_extract(x, "\\{[\\s\\S]*\\}")
  if (is.na(m)) return(NULL)
  tryCatch(fromJSON(m), error = function(e) NULL)
}

normalize_topic <- function(x) {
  x %>%
    to_utf8_safe() %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]àèéìòù ]", " ") %>%
    str_squish() %>%
    str_trim()
}

# Check Ollama  ---------------------------------------------------------

check_ollama <- function() {
  ok <- TRUE
  tryCatch({
    r <- request("http://localhost:11434/api/tags") |> req_perform()
    if (resp_status(r) != 200) ok <- FALSE
  }, error = function(e) ok <<- FALSE)
  ok
}

if (!check_ollama()) {
  stop("Ollama non raggiungibile su localhost:11434. Avvia: `ollama serve`")
}

# Load data (try UTF-8, fallback Latin1)  ---------------------------------------------------------

df <- tryCatch(
  read_csv(input_csv, locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
  error = function(e) {
    message("Lettura UTF-8 fallita, provo Latin1...")
    read_csv(input_csv, locale = locale(encoding = "Latin1"), show_col_types = FALSE)
  }
)

if (!text_col %in% names(df)) stop(sprintf("Colonna '%s' non trovata.", text_col))
if (!id_col %in% names(df)) df <- df %>% mutate(!!id_col := row_number())

df[[text_col]] <- clean_text(df[[text_col]])


# Step 1: topic libero per post -----------------------------
build_prompt_discovery <- function(post_text) {
  paste0(
    "Analizza il seguente post social in italiano di giovani candidati alle elzioni europee e di pagine di informazioni sui social media usate dai giovani.\n",
    "Assegna UNA SOLA etichetta topic breve (2-4 parole) che descriva il tema principale.\n",
    "Nessuna lista predefinita: scegli liberamente.\n\n",
    "Rispondi SOLO in JSON valido minificato con schema esatto:\n",
    "{\"topic\":\"<stringa>\",\"confidence\":<numero 0-1>}\n\n",
    "Regole:\n",
    "- topic corto, specifico ma generale (non troppo dettagliato)\n",
    "- niente hashtag\n",
    "- niente testo extra\n\n",
    "Post:\n", post_text
  )
}

call_ollama_json <- function(prompt, model, url, retries = 3) {
  last_err <- NULL
  
  for (i in seq_len(retries)) {
    res <- tryCatch({
      resp <- request(url) |>
        req_timeout(120) |>
        req_body_json(list(
          model = model,
          prompt = prompt,
          stream = FALSE,
          options = list(temperature = 0)
        )) |>
        req_perform()
      
      body <- resp_body_json(resp)
      raw <- body$response %||% ""
      raw <- to_utf8_safe(raw)
      
      parsed <- safe_parse_json(raw)
      if (is.null(parsed)) return(NULL)
      
      topic <- parsed$topic %||% "altro"
      conf  <- suppressWarnings(as.numeric(parsed$confidence %||% 0))
      if (is.na(conf)) conf <- 0
      conf <- max(min(conf, 1), 0)
      
      tibble(topic_raw = clean_text(as.character(topic)[1]), confidence = conf)
    }, error = function(e) {
      last_err <<- e$message
      NULL
    })
    
    if (!is.null(res)) return(res)
    Sys.sleep(i)
  }
  
  message("Warning: classificazione fallita per un post. Errore: ", last_err)
  tibble(topic_raw = "altro", confidence = 0)
}


# Step 1: topic per ogni post  ---------------------------------------------------------

n <- nrow(df)
res1 <- vector("list", n)

cat(sprintf("Step 1/3: topic discovery su %d post...\n", n))

for (i in seq_len(n)) {
  txt <- df[[text_col]][i]
  txt <- clean_text(txt)
  
  if (nchar(txt) == 0) {
    res1[[i]] <- tibble(topic_raw = "vuoto", confidence = 0)
  } else {
    p <- build_prompt_discovery(txt)
    res1[[i]] <- call_ollama_json(p, ollama_model, ollama_url)
  }
  
  if (i %% 50 == 0 || i == n) cat(sprintf("  progresso: %d/%d\n", i, n))
}

topics_df <- bind_rows(res1) %>%
  mutate(topic_norm = normalize_topic(topic_raw))

df1 <- bind_cols(df, topics_df)

#Save this dataset because it took 16 hours!!!

write_csv2(df1, "llm_topics.csv")



# Step 2: consolidamento topic  ---------------------------------------------------------

freq <- df1 %>% count(topic_norm, sort = TRUE)

major_topics <- freq %>%
  filter(n >= min_topic_freq, topic_norm != "", topic_norm != "vuoto") %>%
  pull(topic_norm)

if (length(major_topics) == 0) {
  major_topics <- freq %>%
    filter(topic_norm != "", topic_norm != "vuoto") %>%
    slice_head(n = min(10, n())) %>%
    pull(topic_norm)
}

build_prompt_merge <- function(topic, major_topics_vec) {
  majors <- toJSON(major_topics_vec, auto_unbox = TRUE)
  paste0(
    "Dato il topic: \"", topic, "\"\n",
    "Scegli il topic più vicino semanticamente tra questa lista:\n",
    majors, "\n\n",
    "Rispondi SOLO JSON minificato:\n",
    "{\"mapped_topic\":\"<uno_della_lista>\",\"confidence\":<numero tra 0 e 1>}\n"
  )
}

map_topic_to_major <- function(topic, majors) {
  if (topic %in% majors) return(tibble(mapped_topic = topic, map_confidence = 1))
  
  prompt <- build_prompt_merge(topic, majors)
  out <- call_ollama_json(prompt, ollama_model, ollama_url)
  
  mapped <- normalize_topic(out$topic_raw[1] %||% "altro")
  conf <- out$confidence[1] %||% 0
  
  if (!mapped %in% majors) mapped <- "altro"
  tibble(mapped_topic = mapped, map_confidence = conf)
}

unique_topics <- sort(unique(df1$topic_norm))
map_list <- vector("list", length(unique_topics))

cat("Step 2/3: consolidamento topic...\n")
for (i in seq_along(unique_topics)) {
  map_list[[i]] <- tibble(topic_norm = unique_topics[i]) %>%
    bind_cols(map_topic_to_major(unique_topics[i], major_topics))
}

# ABout at least 6h???

map_table <- bind_rows(map_list)

df2 <- df1 %>%
  left_join(map_table, by = "topic_norm") %>%
  mutate(final_topic = if_else(is.na(mapped_topic) | mapped_topic == "", "altro", mapped_topic))

# Step 3: topic minimi per coverage target  ---------------------------------------------------------

final_counts <- df2 %>%
  count(final_topic, sort = TRUE) %>%
  mutate(
    cum_posts = cumsum(n),
    cum_cov = cum_posts / sum(n),
    rank = row_number()
  )

k_min <- final_counts %>%
  filter(cum_cov >= coverage_target) %>%
  slice(1) %>%
  pull(rank)

if (length(k_min) == 0) k_min <- nrow(final_counts)

cat("\n========== RISULTATI ==========\n")
cat(sprintf("Numero topic finali trovati: %d\n", nrow(final_counts)))
cat(sprintf("Topic minimi per coprire %.0f%% dei post: %d\n", coverage_target * 100, k_min))
cat("\nDistribuzione topic finali:\n")
print(final_counts)

# Save output  ---------------------------------------------------------
write_csv2(df2, "DF_OTHER_LLM.csv")
