# Viswijzer beoordelingsmethodiek

library(tidyverse)

df <- data.frame(
  cat1 = seq(10,50,10),
  cat2 = seq(10,100,10),
  cat3 = seq(10,100,10)
  ) %>% 
  expand(cat1, cat2, cat3) %>% 
  mutate(score = (2*cat1 + 2.5*cat2 + 1*cat3)/5.5 ) %>% 
  mutate(colour = cut(score, breaks=c(0,30,45,65,80,100), labels=c("red","pink","yellow","lightgreen","green")))

cols <- c("red" = "red", "pink" = "pink", "yellow" = "yellow", "lightgreen" = "lightgreen","green"="green")

ggplot(data=df, aes(x=cat2, y=cat3, fill=colour)) +
  theme_bw() +
  geom_point(size=5, shape=21) +
  scale_fill_manual(values=cols) +
  labs(fill="score") +
  scale_x_continuous(breaks=seq(0,100,10)) + 
  scale_y_continuous(breaks=seq(0,100,10)) + 
  facet_wrap(~paste("cat1 =",cat1), nrow=1)

