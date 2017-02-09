source("data/simulate_data.R")
df <- simulate_cust_df()

library(ggplot2)
library(grid)
library(gridExtra)
# http://www.cookbook-r.com/
# http://rstudio-pubs-static.s3.amazonaws.com/2852_379274d7c5734f979e106dcf019ec46c.html

## relationship between age and credit score
gg1 <- ggplot(data = df, aes(x = age, y = credit_score)) + 
  geom_point() +
  xlab("Age") + ylab("Credit Score")

gg2 <- ggplot(data = df, aes(x = age, y = credit_score)) + 
  geom_point() +
  geom_hline(yintercept = mean(df$credit_score)) +
  geom_vline(xintercept = mean(df$age)) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Age") + ylab("Credit Score")

grid.arrange(gg1, gg2, ncol = 2, top = "Age vs Credit Score")

## relationship between online and in-store sales
gg1 <- ggplot(data = df, aes(x = store_spend, y = online_spend, color = email)) +
  geom_point() + 
  xlab("Prior 12 months in-store sales ($)") + ylab("Prior 12 months online sales ($)")

gg2 <- ggplot(data = df, aes(x = log(store_spend), y = log(online_spend), color = email)) +
  geom_point() +  geom_smooth(method = lm, se = FALSE) +
  xlab("Log of prior 12 months in-store sales ($)") + ylab("Log of prior 12 months online sales ($)")

grid.arrange(gg1, gg2, ncol = 2, top = "Customers as of June 2014")

ggplot(df, aes(x = online_spend)) + 
  geom_histogram(binwidth = 30) + 
  xlab("Prior 12 months online sales ($)") + ylab("Count of Customers") + 
  ggtitle("Customers as of June 2014")

## distance vs spend
gg1 <- ggplot(data = df, aes(x = as.numeric(distance), y = store_spend)) +
  geom_point() + 
  xlab("Distance to store") + ylab("Prior 12 months in-store sales ($)") +
  ggtitle("In-Store")

gg2 <- ggplot(data = df, aes(x = as.numeric(distance), y = online_spend)) +
  geom_point() + 
  xlab("Distance to store") + ylab("Prior 12 months online sales ($)") +
  ggtitle("Online")

gg3 <- ggplot(data = df, aes(x = log(as.numeric(distance)), y = log(store_spend))) +
  geom_point() + 
  xlab("Distance to store") + ylab("Prior 12 months in-store sales ($)")

gg4 <- ggplot(data = df, aes(x = log(as.numeric(distance)), y = log(online_spend))) +
  geom_point() + 
  xlab("Distance to store") + ylab("Prior 12 months online sales ($)")

grid.arrange(gg1, gg2, gg3, gg4, ncol = 2, top = "Customers as of June 2014")

## scatter plot matrices
pairs(formula = ~ age + credit_score + email + distance + online_visits + online_trans + online_spend + store_trans + store_spend, data = df)


library(car)
scatterplotMatrix(formula = ~ age + credit_score + email + distance + online_visits + online_trans + online_spend + store_trans + store_spend,
                  data = df, diagonal = "histogram", main = "Scatter Plot Matrices of Selected Variables")
title(main = "Scatter Plot Matrices of Selected Variables")

library(gpairs)
gpairs(df[2:10])

library(car, lib.loc = .libPaths())

