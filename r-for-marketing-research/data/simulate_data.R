## simulated data for Ch 03
simulate_store_df <- function() {
  stores <- 20
  weeks <- 104 # 2 years
  store_num <- 101:(100 + stores)
  city <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2), rep("JP", 4), rep("AU", 1), rep("CN", 2))
  
  df <- data.frame(matrix(NA, ncol = 10, nrow = stores * weeks))
  col_names <- c("store_num", "year", "week", "p1_sales", "p2_sales", "p1_price", "p2_price", "p1_prom", "p2_prom", "country")
  names(df) <- col_names
  
  df$store_num <- factor(rep(store_num, each = weeks))
  df$country <- factor(rep(city, each = weeks))
  df$week <- rep(1:52, times = stores * 2)
  df$year <- rep(rep(1:2, each = weeks / 2), times = stores)
  
  set.seed(98250)
  df$p1_prom <- rbinom(nrow(df), 1, 0.1)
  df$p2_prom <- rbinom(nrow(df), 1, 0.15)
  df$p1_price <- sample(c(2.19, 2.29, 2.49, 2.79, 2.99), nrow(df), replace = TRUE)
  df$p2_price <- sample(c(2.29, 2.49, 2.59, 2.99, 3.19), nrow(df), replace = TRUE)
  df$p1_sales <- {
    tmp_sales <- rpois(nrow(df), 120)
    tmp_sales <- tmp_sales * log(df$p2_price) / log(df$p1_price)
    floor(tmp_sales * (1 + df$p1_prom * 0.3))
  }
  df$p2_sales <- {
    tmp_sales <- rpois(nrow(df), 100)
    tmp_sales <- tmp_sales * log(df$p1_price) / log(df$p2_price)
    floor(tmp_sales * (1 + df$p2_prom * 0.4))
  }
  
  df
}

## simulated data for Ch 04
simulate_cust_df <- function(seed = 21821) {
  set.seed(seed)
  ncust <- 1000
  df <- data.frame(id = as.factor(1:ncust))
  df$age <- rnorm(n = ncust, mean = 35, sd = 5)
  df$credit_score <- rnorm(n = ncust, mean = 3 * df$age + 620, sd = 50)
  df$email <- factor(sample(c("yes", "no"), size = ncust, replace = TRUE, prob = c(0.8, 0.2)))
  df$distance <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
  df$online_visits <- rnbinom(n = ncust, size = 0.3,
                              mu = 15 + ifelse(df$email == "yes", 15, 0) - 0.7 * (df$age - median(df$age)))
  df$online_trans <- rbinom(n = ncust, size = df$online_visits, prob = 0.3)
  df$online_spend <- exp(rnorm(n = ncust, mean = 3, sd = 0.1)) * df$online_trans
  df$store_trans <- rnbinom(n = ncust, size = 5, mu = 3 / sqrt(df$distance))
  df$store_spend <- exp(rnorm(n = ncust, mean = 3.5, sd = 0.4)) * df$store_trans
  
  sat_overall <- rnorm(n = ncust, mean = 3.1, sd = 0.7)
  
  sat_service <- floor(sat_overall + rnorm(n = ncust, mean = 0.5, sd = 0.4))
  sat_selection <- floor(sat_overall + rnorm(n = ncust, mean = -0.2, sd = 0.6))
  sat_service[sat_service > 5] <- 5
  sat_service[sat_service < 1] <- 1
  sat_selection[sat_selection > 5] <- 5
  sat_selection[sat_selection < 1] <- 1
  
  no_response <- as.logical(rbinom(ncust, size = 1, prob = df$age/100))
  sat_selection[no_response] <- NA
  sat_service[no_response] <- NA

  df$sat_service <- sat_service
  df$sat_selection <- sat_selection

  df
}










