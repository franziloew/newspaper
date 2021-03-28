
# 1. STM ----
###########################################
## Plot Topic distribution of a document ##
###########################################
plot_topic_distr_document <- function(doc_id){
  title <- theta %>% filter(doc_index == doc_id) %>% select(title, source)
  temp_df <- theta %>% filter(doc_index == doc_id) 
  
  temp_df %>% 
    ggplot(aes(reorder(topic,topic), theta)) +
    geom_col() +
    geom_text(aes(label=ifelse(theta > 0.02, round(theta,2), '')),
              size = 2, color = 'red', vjust = 0
    ) +
    ylim(c(0,1)) +
    labs(x = "Topic", y = "Topic proportion",
         title = paste0(title$title," (",title$source,")")) +
    theme(
      axis.title = element_text(size=4),
      axis.title.y = element_text(angle = 90))
}

#######################################
## Plot Topic distribution over time ##
#######################################
plot_topic_distr_time <- function(topic_filter, source_filter_array) {
  topic_label <-theta[theta$topic == topic_filter, ]$topic_label[1]
  
  theta %>% 
    filter(topic == topic_filter) %>% 
    filter(source %in% source_filter_array) %>% 
    group_by(source, year_week, type) %>% 
    summarise(theta_mean = mean(theta, na.rm = T)) %>% 
    ggplot(aes(year_week, theta_mean,
               group = source, color = source)) + 
    ylim(c(0,0.35)) +
    geom_line(size = 0.3) +
    geom_vline(xintercept = election_date, linetype = 'dashed', size = 0.3) +
    scale_colour_hc()+
    scale_x_date(date_breaks = "1 month", labels = date_format("%m-%Y")) +
    labs(x = NULL, y = 'Topic proportion', 
         title = 'Mean topic proportion',
         subtitle = topic_label) +
    theme(
      axis.title = element_text(size=4),
      axis.title.y = element_text(angle = 90),
      legend.title = element_blank()
    )
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
  # get all doc_codes within -7 days
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
      election_dummy = as.factor(ifelse(date1 <= election_date, "pre", "post"))
      ) %>% 
    filter(type2 == "press") %>% 
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
                color='red',
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    scale_y_continuous(limits = c(-5,-1)) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = NULL,
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
                color='red',
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = NULL,
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
