
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
    filter(type2 == "press") %>% 
    select(-election1, election2) %>% 
    group_by(date1, source1, source2, election_dummy) %>% 
    summarise(cos_sim = mean(cos_sim, na.rm = T)) %>% 
    ungroup()
}

###############
## Plot data ##
###############
plot_cosine_sim_ols <- function(df, target) {
  ggplot(cosine_distances_df, 
         aes(date1, log(cos_sim))) +
    geom_point(size = 0.2, alpha = 0.5) +
    geom_smooth(method = lm, 
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = "ln(cosine similarity)",
         caption = paste0(target)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 3),
      axis.title.y = element_text(size = 3, angle = 90)) 
}

plot_cosine_sim_rd <- function(df, target) {
  ggplot(cosine_distances_df, 
         aes(date1, log(cos_sim),
             group = election_dummy)) +
    geom_vline(xintercept = as.Date('2017-09-24'), size = 0.3, linetype = 'dashed') +
    geom_point(size = 0.2, alpha = 0.5) +
    geom_smooth(method = lm, 
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = "ln(cosine similarity)",
         caption = paste0(target)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 3),
      axis.title.y = element_text(size = 3, angle = 90))  
}

#####################
### Calculate OLS ###
#####################
calc_ols_dummy <- function(dataframe) { 
model_outcome <- dataframe %$%
  lm(log(cos_sim) ~ source2)
}

##################################
### Calculate OLS coefficients ###
##################################
transform_coeff_ols <- function(coeff) {
  exp(coeff)-1
}

##########################################
### Calculate regression discontinuity ###
##########################################
### Dummy
calc_rd_dummy<- function(dataframe, target_source) { 
  temp_df <- dataframe %>%
    mutate(
      X_centered = I(date1 - election_date),
      treated = date1 >= election_date)
  
  model_outcome <- temp_df %$%
    lm(log(cos_sim) ~ treated + X_centered + source2 )
}

### Dummy & interaction term
calc_rd_dummy_interaction <- function(dataframe, target_source) { 
  temp_df <- dataframe %>%
    mutate(
      X_centered = I(date1 - election_date),
      treated = date1 >= election_date)
  
  model_outcome <- temp_df %$%
    lm(log(cos_sim) ~ treated + X_centered + source2 + treated:source2)
}
