
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
    labs(x = "Topic", y = NULL,
         title = title$source,
         subtitle = title$title) +
    theme(
      axis.text.x = element_text(size = 8),
      axis.title.y = element_text(angle = 90))
}

#######################################
## Plot Topic distribution over time ##
#######################################
plot_topic_distr_time <- function(topic_filter, source_filter_array) {
  topic_label <-theta[theta$topic == topic_filter, ]$topic_label[1]
  
  theta %>% 
    filter(date <= max_date) %>% 
    filter(topic == topic_filter) %>% 
    filter(source %in% source_filter_array) %>% 
    group_by(source, year_week, type) %>% 
    summarise(theta_mean = mean(theta, na.rm = T)) %>% 
    ggplot(aes(year_week, theta_mean,
               group = source, color = source)) + 
    ylim(c(0,0.35)) +
    geom_line(size = 0.3) +
    geom_vline(xintercept = election_date, linetype = 'dashed', size = 0.3) +
    scale_color_manual(values = custom_colors[source_filter]) +
    scale_x_date(date_breaks = "1 month", labels = date_format("%m-%Y")) +
    labs(x = NULL, y = 'Topic proportion', 
         title = NULL,
         subtitle = topic_label) +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(size=6),
      axis.title = element_text(size=5),
      axis.text = element_text(size=5),
      axis.title.y = element_text(angle = 90),
      legend.title = element_blank()
    )
}

# 2. Cosine similarity ----
#################################
## Calculate cosine similarity ##
#################################
cosine_sim <- function(a, b) crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))


#########################################
## Calculate cosine similarity in loop ##
#########################################
run_cos_sim_loop <- function(target) {
  small_df <- model_df %>% filter(source == target)
  doc_indices <- small_df$doc_index
  
  print(paste("Number of documents:", length(doc_indices)))
  
  rm(cosine_distances)
  i <- 1
  for (x in doc_indices) {
    print(i)
    temp <- calc_cos_sim(x, gamma)
    
    # append the results to a data frame
    cosine_distances <- if (!exists("cosine_distances")) temp else rbind(cosine_distances, temp)
    
    i <- i + 1
  }
  
  cosine_distances
}


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
  df %>% 
    filter(election_dummy == "pre") %>% 
    ggplot(aes(date1, cos_sim)) +
    geom_point(size = 0.2, alpha = 0.5) +
    geom_smooth(method = lm, 
                color='red',
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    #scale_y_continuous(limits = c(-4,-1)) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = 'CS',
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
         aes(date1, cos_sim,
             group = election_dummy)) +
    geom_vline(xintercept = as.Date('2017-09-24'), size = 0.3, linetype = 'dashed') +
    geom_point(size = 0.2, alpha = 0.5) +
    geom_smooth(method = lm, 
                color='red',
                formula = y ~ x,
                se=FALSE, size = 0.3) +
    #scale_y_continuous(limits = c(-4,-1)) +
    scale_x_date(date_breaks = "2 month", labels = date_format("%m-%Y")) +
    facet_wrap(~source2, nrow = 1) +
    labs(x=NULL, y = 'CS',
         caption = paste0(target)) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 3),
      axis.title.y = element_text(size = 3, angle = 90))  
}

# quick and dirty workaround for removing text from covariates  
remove_text = function(text){
  text = gsub("source2", "", text, perl = TRUE)
  text
}

#####################
### Calculate OLS ###
#####################
calc_ols_dummy <- function(dataframe) {
  truncated_df <- dataframe %>% 
    filter(election_dummy == "pre")
  
  model_outcome <- truncated_df %$%
    lm(cos_sim ~ source2)
}

##################################
### Calculate OLS coefficients ###
##################################
transform_coeff_ols <- function(coeff) {
  coeff
  #exp(coeff)-1
}

##########################################
### Calculate regression discontinuity ###
##########################################
### Simple RDD (no dummy)
calc_rd <- function(dataframe) {
  temp_df <- dataframe %>%
    mutate(
      W = I(date1 - election_date),
      `T` = date1 >= election_date) %>% 
    filter(between(W,-115,115))
  
  model_outcome <- temp_df %$%
    lm(cos_sim ~ `T` * W)
}

### Dummy
calc_rd_dummy<- function(dataframe) { 
  temp_df <- dataframe %>%
    mutate(
      W = I(date1 - election_date),
      `T` = date1 >= election_date) %>% 
    filter(between(W,-115,115))
  
  model_outcome <- temp_df %$%
    lm(cos_sim ~ `T` * W + source2 )
}

### Dummy & interaction term
calc_rd_dummy_interaction <- function(dataframe) { 
  temp_df <- dataframe %>%
    mutate(
      W = I(date1 - election_date),
      `T` = date1 >= election_date) %>% 
    filter(between(W,-115,115))
  
  model_outcome <- temp_df %$%
    lm(cos_sim ~ `T` * W + `T` * source2)
}
