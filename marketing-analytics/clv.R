library(sqldf)
library(magrittr)
library(dplyr)

data <- read.delim(file = "purchases.txt", header = FALSE, sep = "\t", dec = ".", stringsAsFactors = FALSE)
names(data) <- c("customer_id", "purchase_amount", "date_of_purchase")

data <- data %>% mutate(date_of_purchase = as.Date(date_of_purchase, "%Y-%m-%d"),
                        year_of_purchase = as.numeric(format(date_of_purchase, "%Y")),
                        days_since = difftime(time1 = "2016-01-01", time2 = date_of_purchase, units = "days"))


#sqldf("SELECT customer_id, MIN(days_since) AS 'recency', MAX(days_since) AS 'first_purchase', COUNT(*) AS 'frequency', AVG(purchase_amount) AS 'amount' FROM data GROUP BY 1")

customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency = as.numeric(min(days_since)), first_purchase = as.numeric(max(days_since)), frequency = n(), amount = mean(purchase_amount))

customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))
customers_2014 <- data %>% 
  filter(days_since > 365) %>%
  group_by(customer_id) %>% 
  summarise(recency = as.numeric(min(days_since)) - 365, first_purchase = as.numeric(max(days_since)) - 365, frequency = n(), amount = mean(purchase_amount))

customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

### transition matrix
new_data <- customers_2014 %>% inner_join(customers_2015, by = "customer_id")
transition <- table(new_data$segment.x, new_data$segment.y)
transition <- transition / rowSums(transition)

### make prediction
segments <- matrix(nrow = 8, ncol = 11)
segments[, 1] <- table(customers_2015$segment)
colnames(segments) <- 2015:2025
rownames(segments) <- levels(customers_2015$segment)

lapply(2:11, function(i) {
  segments[, i] <<- segments[, i-1] %*% transition
  ""
})

# new active can't be added given db by definition


### compute CLV
## assume revenue per segment remains constant
## discount reflects risk

# merge 2015 customers and 2015 revenue - see module 2, lines 160-161
revenue_2015 <- data %>% filter(year_of_purchase == 2015) %>% 
  group_by(customer_id) %>% 
  summarise(revenue_2015 = sum(purchase_amount))

yearly_revenue <- customers_2015 %>% left_join(revenue_2015, by = "customer_id") %>%
  mutate(revenue_2015 = ifelse(is.na(revenue_2015), 0, revenue_2015)) %>%
  group_by(segment) %>% 
  summarise(average_per_segment = mean(revenue_2015)) %>% 
  select(average_per_segment) %>% unlist() %>% unname()

revenue_per_segments <- yearly_revenue * segments
yearly_revenue <- colSums(revenue_per_segments)
cumulated_revenue <- cumsum(yearly_revenue)

discount_rate <- 0.1
discount <- 1 / ((1 + discount_rate) ^ (0:10))

disc_yearly_revenue <- yearly_revenue * discount
disc_cumulated_revenue <- cumsum(disc_yearly_revenue)

cumulated_revenue
barplot(disc_cumulated_revenue)

# how much the database worth?
disc_cumulated_revenue[11] - yearly_revenue[1]
sum(disc_yearly_revenue) - disc_yearly_revenue[1]







