## Plot text length

# news articles
maxval1 <- 4000
dd <- news_df %>% filter(text_length > maxval1) %>% 
  group_by(source) %>% 
  summarise(outlier_txt= paste(n()))

p1 <- news_df %>%
  filter(text_length < maxval1) %>%
  ggplot(aes(x=source, y=text_length)) +
  geom_boxplot() +
  geom_text(data=dd,aes(y=maxval1+400,label= outlier_txt),
            size=2, color = "red") +
  geom_segment(data=dd, 
               aes(y=maxval1+20,yend=maxval1+250,xend=source),
               color = "red", arrow = arrow(length = unit(0.1,"cm"))) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="blue", fill="blue") +
  labs(x=NULL,  y=NULL) +
  theme(axis.text.x = element_text(angle = 45))

# press releases
maxval2 <- 600
dd <- press_df %>% filter(text_length > maxval2) %>% 
  group_by(source) %>% 
  summarise(outlier_txt= paste(n()))

p2 <- press_df %>%
  filter(text_length < maxval2) %>%
  ggplot(aes(x=source, y=text_length)) +
  geom_boxplot() +
  geom_text(data=dd,aes(y=maxval2+60,label= outlier_txt),
            size=2, color = "red") +
  geom_segment(data=dd, 
               aes(y=maxval2+10,yend=maxval2+40,xend=source),
               color = "red", arrow = arrow(length = unit(0.1,"cm"))) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=1, color="blue", fill="blue") +
  labs(x=NULL,  y=NULL) +
  theme(axis.text.x = element_text(angle = 45))

p1 + p2 + plot_layout(widths = c(2,1))