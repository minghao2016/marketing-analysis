library(car)
library(lattice)
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(dplyr)

#options(dplyr.print_max = 1e9)
source("utils.R")

df <- read.csv("data/rintro-chapter5.csv", stringsAsFactors = FALSE)
df <- convert_to_factor(df)

df %>% group_by(Segment) %>% summarise(income = mean(income))
df %>% group_by(Segment, subscribe) %>% summarise(income = mean(income))

seg_income  <- df %>% group_by(Segment) %>% 
  summarise(seg_income = mean(income))
df <- df %>% inner_join(seg_income, by = "Segment")
df$seg_income <- NULL

aggregate(income ~ Segment + ownHome, data = df, FUN = mean)
df %>% group_by(Segment, ownHome) %>% summarise(income = mean(income))

with(df, table(Segment, ownHome))
dcast(data = df, formula = Segment ~ ownHome, fun.aggregate = length)
dcast(data = df, kids ~ Segment, fun.aggregate = length)

xtabs(kids ~ Segment, df)
df %>% group_by(Segment) %>% summarise(num_kids = sum(kids))

## visualization by group
histogram(~ subscribe | Segment, df)
histogram(~ subscribe | Segment, df, type = "count", layout = c(4,1), col = c("burlywood", "darkolivegreen"))

df %>% group_by(Segment, subscribe) %>% summarise(count = n()) %>%
  ggplot(aes(x = subscribe, y = count, fill = Segment)) + 
  geom_bar(position = "dodge", stat = "identity")

histogram(~ subscribe | Segment + ownHome, df)


gg1 <- df %>% group_by(Segment, subscribe, ownHome) %>% summarise(count = n()) %>%
  filter(ownHome == "ownYes") %>%
  ggplot(aes(x = subscribe, y = count, fill = Segment)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Own Home: Yes")

gg2 <- df %>% group_by(Segment, subscribe, ownHome) %>% summarise(count = n()) %>%
  filter(ownHome == "ownNo") %>%
  ggplot(aes(x = subscribe, y = count, fill = Segment)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Own Home: No")

grid.arrange(gg1, gg2, ncol = 1)

prop.table(with(df, table(subscribe, Segment)), margin = 2)[2,] %>% unlist() %>% as.data.frame()

dcast(df, subscribe ~ Segment, fun.aggregate = length) %>%
  melt(id.vars = "subscribe", variable.name = "Segment", value.name = "count") %>% 
  group_by(Segment, subscribe) %>% summarise(n = sum(count)) %>% mutate(freq = n / sum(n)) %>%
  filter(subscribe == "subYes")
















