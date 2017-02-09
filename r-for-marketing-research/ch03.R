source("data/simulate_data.R")
df <- simulate_store_df()

library(car)
some(df, 10)

## discrete variables
p1_table <- table(df$p1_price)
plot(p1_table)

p1_table <- table(df$p1_price, df$p1_prom)
p1_table[,2] / apply(p1_table, 1, sum)

## continuous variables
get_summary <- function(x) {
  funs <- list(min = min, max = max, mean = mean, median = median, var = var, sd = sd, IQR = IQR, mad = mad)
  unlist(lapply(names(funs), function(name) {
    f <- funs[[name]]
    out <- f(x, na.rm = TRUE)
    names(out) <- name
    out
  }))
}

do.call(rbind, lapply(df[, c("p1_sales", "p2_sales")], get_summary))

library(psych)
describe(df)

## single value visualization
library(ggplot2)
library(grid)
library(gridExtra)
# http://www.cookbook-r.com/
# http://rstudio-pubs-static.s3.amazonaws.com/2852_379274d7c5734f979e106dcf019ec46c.html

# histogram + density
ggplot(df, aes(x = p1_sales)) + 
  geom_histogram(binwidth = 2, aes(y=..density..)) + 
  geom_density(alpha = 0.2) + 
  xlab("Product 1 Sales (Units)") + ylab("Relative Frequency") + 
  ggtitle("Product 1 Weekly Sales Frequencies. All Stores")

# box plot
ggplot(df, aes(x = store_num, y = p2_sales, fill = store_num)) + 
  geom_boxplot() + coord_flip() + guides(fill = FALSE) +
  xlab("Store") + ylab("Weekly Unit Sales") + 
  ggtitle("Weekly Sales of P2 by Store")

ggplot(df, aes(x = as.factor(p2_prom), y = p2_sales, fill = p2_prom)) + 
  geom_boxplot() + coord_flip() + guides(fill = FALSE) +
  scale_x_discrete(breaks = c("1", "0"), labels = c("Yes", "No")) + 
  xlab("P2 Promoted in Store?") + ylab("Weekly Unit Sales") + 
  ggtitle("Weekly Sales of P2 by Store")

# Q-Q plot
get_line_params <- function(data) {
  y <- quantile(data[!is.na(data)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  intercept <- y[1] - slope * x[1]
  list(slope = slope, intercept = intercept)  
}
params1 <- get_line_params(df$p1_sales)
params2 <- get_line_params(log(df$p1_sales))

qq1 <- ggplot(df, aes(sample = p1_sales)) + 
  stat_qq(distribution = qnorm) + geom_abline(slope = params1$slope, intercept = params1$intercept) + 
  xlab("Theoretical Quantile") + ylab("Sample Quantile") + 
  ggtitle("Normal Q-Q Plot - Product 1")

qq2 <- ggplot(df, aes(sample = log(p1_sales))) + 
  stat_qq(distribution = qnorm) + geom_abline(slope = params2$slope, intercept = params2$intercept) + 
  xlab("Theoretical Quantile") + ylab("Sample Quantile") + 
  ggtitle("Normal Q-Q Plot - Log Product 1")

grid.arrange(qq1, qq2, ncol = 2)

# cumulative dist
plot(ecdf(df$p1_sales), yaxt = "n")
axis(side=2, at=seq(0, 1, by = 0.1), las=1, labels=paste(seq(0,100,by=10), "%", sep=""))
abline(h=0.9, lty=3)
abline(v=quantile(df$p1_sales, pr=0.9), lty=3)

ggplot(df, aes(x = p1_sales)) + 
  geom_step(aes(y=..y..), stat = "ecdf", size = 2) +
  geom_abline(slope = 0, intercept = 1) + geom_abline(slope = 0, intercept = 0) +
  xlab("P1 Weekly Sales, All Stores") + ylab("Cumulative Proportion") + 
  ggtitle("Cumulative Distribution of P1 Weekly Sales")

ggplot(df, aes(x = p1_sales)) + 
  geom_histogram(aes(y = cumsum(..count..)), binwidth = 2) + 
  stat_bin(aes(y = cumsum(..count..)), geom = "line", binwidth = 2) +
  xlab("P1 Weekly Sales, All Stores") + ylab("Cumulative Counts") + 
  ggtitle("Cumulative Counts of P1 Weekly Sales")

# aggregation and map
# http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# http://www.unomaha.edu/mahbubulmajumder/data-science/fall-2014/
library(ggmap)
sales_by_country <- aggregate(df$p1_sales, by = list(country = df$country), FUN = sum)
sales_by_country$country <- as.character(sales_by_country$country)
sales_by_country <- cbind(sales_by_country, geocode(sales_by_country$country))

ggplot(data = sales_by_country) +
  borders(database = "world", colour = "gray50", fill = "gray50") +
  geom_polygon(aes(x = lon, y = lat, fill = country, group = x), color = "white") +
  coord_fixed(1.3) + guides(fill=FALSE)






