
library(tidyverse)

#read data
diamonds2 <- readRDS("src/ch2/diamonds2.rds")
diamonds2 %>% head (n=5)

#Rearranging data

#method 1
diamonds2 <- diamonds2 %>%
  pivot_longer(!cut, names_to = "year", values_to = "price")

#method 2
diamonds2 %>% pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>%  head(n = 5)

#linear regression mdel
model <- lm(formula= price ~ ., data= diamonds2)


diamonds3 <- readRDS("../ch2/diamonds3.rds")
diamonds3 %>% head (n=5)
diamonds3 <- pivot_wider(diamonds3, names_from = "dimension", values_from = "measurement")

diamonds4 <- readRDS("../ch2/diamonds4.rds")
diamonds4 <- separate(diamonds4, col=4, into = c("x", "y", "z"), sep="/", convert = TRUE)

diamonds5 <- readRDS("../ch2/diamonds5.rds")
diamonds5 <- unite (diamonds5, col= "dim", c("x", "y", "z"), sep = "/", remove=T )
diamonds5 <- diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')


library(ggplot2) #load diamond dataset
library(dplyr)
diamonds %>% filter (cut =='Ideal' | cut == 'Premium', carat >= 0.23) %>% head(n=5)
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)


diamonds %>% 
  arrange( carat, desc(price), cut)


diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)

diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

diamonds %>% 
  select(x:z, everything()) %>% 
  head(n = 5)

diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

diamonds %>% 
  group_by(cut)

diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

glimpse(diamonds)

typeof(diamonds)

library(lubridate)
ymd(20101215)
mdy("4/1/17")
bday <- dmy("14/10/1979")
month(bday)
year(bday)
