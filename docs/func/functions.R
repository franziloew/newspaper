
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

#######################
## Cosine similarity ##
#######################
cosine_sim <- function(a, b) crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))

#######################################
## Calculate cosine sim of documents ##
#######################################
calc_cos_sim <- function(doc_code,
                         gamma_mat = gamma) {
  # find the doc
  doc_col_index <- which(colnames(gamma_mat) == as.character(doc_code))
  # get the week of the doc
  date_target <- filter(model_df, doc_index == doc_code)$date
  # get all doc_codes +/- 4 days
  date_seq <- seq(from=date_target-4,by="day",length.out = 9)
  doc_code_pairs <- filter(model_df, year_week %in% date_seq)$doc_index
  # calculate cosine similarity for each document based on gamma
  # apply(..., 2) iterates over the columns of a matrix
  cos_sims <- apply(gamma_mat[, doc_code_pairs], 2,
                    FUN = function(y) cosine_sim(gamma_mat[, doc_col_index], y)
  )
  
  # return results as data frame
  data_frame(
    doc_code1 = doc_code,
    doc_code2 = as.numeric(names(cos_sims)),
    cos_sim = cos_sims
  ) %>%
    filter(doc_code2 != doc_code) # remove self reference
}

#####################################
## Prepare df for regression model ##
#####################################
df_prep_ols <- function(cosine_df) {
  cosine_df %>%
    left_join(., model_df %>%
                transmute(
                  doc_code1 = doc_index,
                  source1 = source, type1 = type,
                  date1 = date
                ),
              by = "doc_code1"
    ) %>%
    left_join(., model_df %>%
                transmute(
                  doc_code2 = doc_index,
                  source2 = source, type2 = type,
                  date2 = date
                ),
              by = "doc_code2"
    ) %>%
    group_by(date1, source2, type2) %>%
    summarise(cos_sim = mean(cos_sim, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(election_dummy = as.factor(ifelse(date1 <= election_date, "pre", "post")))
}
