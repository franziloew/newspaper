
# 1. STM ----
###########################################
## Plot Topic distribution of a document ##
###########################################
plot_topic_distr_document <- function(doc_id){
  title <- theta %>% filter(doc_index == doc_id) %>% select(title, source)
  theta %>% 
    filter(doc_index == doc_id) %>% 
    ggplot(aes(reorder(topic,topic), theta)) +
    geom_col() +
    #coord_flip() +
    ylim(c(0,1)) +
    labs(x = NULL, y = "Topic proportion",
         caption = paste0(title$title," (",title$source,")")) +
    theme(axis.title.y = element_text(angle = 90))
}

# 2. Cosine similarity ----
#################################
## Calculate cosine similarity ##
#################################
cosine_sim <- function(a, b) crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))

###################################
## Apply cosine_sim on documents ##
###################################
calc_cos_sim <- function(doc_code, gamma_mat = gamma) {
  # find the doc
  doc_col_index <- which(colnames(gamma_mat) == as.character(doc_code))
  # get the date of the doc
  date_target <- filter(model_df, doc_index == doc_code)$date
  # get all doc_codes +/- 4 days
  date_seq <- seq(from=date_target-6,by="day",length.out = 7)
  doc_code_pairs <- filter(model_df, date %in% date_seq)$doc_index
  # calculate cosine similarity for each document based on gamma
  # apply(..., 2) iterates over the columns of a matrix
  cos_sims <- apply(gamma_mat[, doc_code_pairs], 2,
                    FUN = function(y) cosine_sim(gamma_mat[, doc_col_index], y)
  )
  
  # return results as data frame
  data_frame(
    doc_code1 = doc_code,
    doc_code2 = as.numeric(names(cos_sims)),
    cos_sim = unname(cos_sims)
  ) %>%
    filter(doc_code2 != doc_code) # remove self reference
}

# 3. Regression ----
#####################################
## Prepare df for regression model ##
#####################################
df_prep_ols <- function(cosine_df) {
  cosine_df %>%
    left_join(., model_df %>%
                transmute(
                  doc_code1 = doc_index,
                  source1 = source, 
                  type1 = type,
                  date1 = date
                ),
              by = "doc_code1"
    ) %>%
    left_join(., model_df %>%
                transmute(
                  doc_code2 = doc_index,
                  source2 = source, 
                  type2 = type,
                  date2 = date
                ),
              by = "doc_code2"
    ) %>%
    mutate(
      election1 = ifelse(date1 <= election_date, "pre", "post"),
      election2 = ifelse(date2 <= election_date, "pre", "post"),
      election_dummy = as.factor(ifelse(date1 <= election_date, "pre", "post"))
      ) %>% 
    filter(election1 == election2) %>% 
    select(-election1, election2)
}

#####################
### Calculate OLS ###
#####################
calc_ols_dummy <- function(dataframe) { 
temp_df <- dataframe %>%
  filter(type2 == "press")

model_outcome <- temp_df %$%
  lm(log(cos_sim) ~ source2)
}

##########################################
### Calculate regression discontinuity ###
##########################################
### Dummy
calc_rd_dummy<- function(dataframe, target_source) { 
  temp_df <- dataframe %>%
    filter(type2 == "press") %>%
    mutate(
      X_centered = I(date1 - election_date),
      treatedTRUE = ifelse(date1 >= election_date, 1, 0))
  
  model_outcome <- temp_df %$%
    lm(log(cos_sim) ~ treatedTRUE + source2 + X_centered)
}

### Dummy & interaction term
calc_rd_dummy_interaction <- function(dataframe, target_source) { 
  temp_df <- dataframe %>%
    filter(type2 == "press") %>%
    mutate(
      X_centered = I(date1 - election_date),
      treatedTRUE = ifelse(date1 >= election_date, 1, 0))
  
  model_outcome <- temp_df %$%
    lm(log(cos_sim) ~ treatedTRUE * source2 + X_centered)
}
