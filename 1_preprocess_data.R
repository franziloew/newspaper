# This script reads in raw text data from the PostgreSQL server and 
# 1. combines the data sources -> model_df
# 2. runs the STM model -> output: covariates, news_df_sparse, stmModel
# 3. calculates the cosine similarity -> output: cosine_dist_{source}

# Load the required packages
library(RPostgreSQL)
library(tidytext)
library(dplyr)
library(tidyr)
library(stm)

# Source functions
source("func/functions.R")

# Set global variables
color_palette <-c('#d2fbd4','#a5dbc2','#7bbcb0','#559c9e','#3a7c89','#235d72','#123f5a')
max_date <- as.Date('2018-02-13')

# --- 1. Download text data from PostgreSQL database
# load the PostgreSQL driver & connect to server
pg <- dbDriver("PostgreSQL")
pw <- {
  "Up627R&F6"
}
con <- dbConnect(pg,
                 user = "Franzi",
                 password = pw,
                 host = "news-paper.cildx1grqozu.eu-west-2.rds.amazonaws.com",
                 port = 5432,
                 dbname = "postgres"
)

# check if table exists
dbExistsTable(con, "news_cleaned")

# get news articles
news_df <- dbGetQuery(con, "SELECT * from news_cleaned")
# get press releases
press_df <- dbGetQuery(con, "SELECT * from press_releases")

# combine both
model_df <- news_df %>%
  dplyr::mutate(
    date = as.Date(date),
    type = "news",
    source = medium
  ) %>%
  select(date, title, title_text, text_length, text_cleaned, type, source) %>%
  bind_rows(press_df %>%
              select(title, date, title_text, type, source, text_length, text_cleaned)) %>%
  filter(date <= max_date) %>% 
  dplyr::mutate(
    doc_index = as.numeric(rownames(.)),
    year_week = lubridate::floor_date(date, "1 week")
  )

# --- 2. Calculate STM
tidy_news_df <- model_df %>%
  select(source, year_week, text_cleaned, doc_index) %>%
  unnest_tokens(word, text_cleaned) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

news_df_sparse <- tidy_news_df %>%
  count(doc_index, word) %>%
  cast_sparse(doc_index, word, n)

## notice that I am scrambling the order of the rows
## the covariate data is in random order now
covariates <- tidy_news_df %>%
  sample_frac() %>%
  distinct(doc_index, source, year_week)

stmModel <- stm(
  documents = news_df_sparse,
  K = 40, prevalence = ~ source + s(year_week),
  data = covariates, init.type = "Spectral"
)

save(stmModel, covariates, model_df, tidy_news_df, news_df_sparse, 
     file = "output/model40_weekly_source.Rda")

# --- 3. Calculate Cosine Sim
# load("output/models/model40_weekly_source.Rda")

model_df <- model_df %>%
  dplyr::mutate(
    doc_index = as.numeric(rownames(.)),
    election_dummy = as.factor(ifelse(date <= as.Date("24Sep2017", "%d%b%Y"), "pre", "post")),
    year_week = lubridate::floor_date(date, "1 week")
  )

## Create matrix of topic distribution
td_gamma <- tidy(stmModel,
                 matrix = "gamma",
                 document_names = rownames(news_df_sparse)) %>% 
  mutate(doc_index = as.integer(document)) %>%
  select(-document)

# convert to "wide" data frame
td_gamma_wide <- td_gamma %>%
  spread(doc_index, gamma) # one row per topic, one column per document

# convert to matrix
gamma <- as.matrix(td_gamma_wide[, -1])

# For each source, create a dataframe of the cosine similarity
# DIE WELT
target_source <- "DIE WELT"
welt <- run_cos_sim_loop(target_source)
save(welt, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# SPIEGEL ONLINE
target_source <- "SPIEGEL ONLINE"
spiegel <- run_cos_sim_loop(target_source)
save(spiegel, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# stern.de
target_source <- "stern.de"
stern <- run_cos_sim_loop(target_source)
save(stern, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# ZEIT ONLINE
target_source <- "ZEIT ONLINE"
zeit <- run_cos_sim_loop(target_source)
save(zeit, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# Handelsblatt
target_source <- "Handelsblatt"
handelsblatt <- run_cos_sim_loop(target_source)
save(handelsblatt, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# FOCUS Online
target_source <- "FOCUS Online"
focus <- run_cos_sim_loop(target_source)
save(focus, file = paste0("output/cosine_dist_", target_source, ".Rda"))

# Bild.de
target_source <- "Bild.de"
bild <- run_cos_sim_loop(target_source)
save(bild, file = paste0("output/cosine_dist_", target_source, ".Rda"))
