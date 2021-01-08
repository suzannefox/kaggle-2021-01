library(tidyverse)
library(data.table)
library(glue)

df.train <- data.table::fread('train.csv', data.table = FALSE) %>% mutate(sample='train')
df.test <- data.table::fread('test.csv', data.table = FALSE) %>% mutate(sample='test')
df.data <- bind_rows(df.train, df.test) 

df.data %>% 
  filter(sample=='train') %>% 
  ggplot() +
  geom_density(aes(target)) +
  geom_boxplot(aes(target), alpha = 0.4)

for (var in colnames(df.data) %>% str_subset('cont')) {
  print(df.data %>% 
    ggplot() +
    geom_density(aes(.data[[var]])) +
    geom_boxplot(aes(.data[[var]])) +
    facet_wrap(~sample, ncol=2) +
    ggtitle(glue('{var} : Compare Distribution of test and train samples'))
  )
}

# -------------------------------

median(df.train$target)
mean(df.train$target)

df.train.test <- df.train %>% 
  mutate(split = if_else(target < 7.9,'lower','upper'))

for (var in colnames(df.train.test) %>% str_subset('target')) {
  print(df.train.test %>% 
          ggplot() +
          geom_density(aes(.data[[var]])) +
          geom_boxplot(aes(.data[[var]])) +
          facet_wrap(~split, ncol=2) +
          ggtitle(glue('{var}'))
  )
}

df.train.test %>% 
  group_by(split) %>% 
  summarise(mean(target))
