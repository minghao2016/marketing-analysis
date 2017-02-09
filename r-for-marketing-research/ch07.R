library(car)
library(gpairs)
library(corrplot)
library(reshape2)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggfortify)

#options(dplyr.print_max = 1e9)
source("utils.R")

df <- read.csv("data/rintro-chapter7.csv", stringsAsFactors = FALSE)
df <- convert_to_factor(df)

scatterplotMatrix(df, diagonal = "histogram")
gpairs(df)
# ggpairs(df)

# individual looks symetric?
df$log_dist <- log(df$distance)
gpairs(df)

# pairwise not correlated?
cor_mat <- cor(df[, !names(df) %in% c("weekend", "distance")])
corrplot.mixed(cor_mat, upper = "ellipse")

ggplot(df, aes(x = rides, y = overall)) +
  geom_point() + geom_smooth(method = lm)

m1 <- lm(overall ~ rides, data = df)
summary(m1)
confint(m1)

# relationship is linear?
# residuals are normally distributed?
set.seed(1237)
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy_model <- lm(y ~ x)
plot(y ~ x)
abline(toy_model)
plot(toy_model$fitted.values, toy_model$residuals)
plot(toy_model)
autoplot(toy_model)
gpairs(data.frame(x = x, y = y))

autoplot(m1, which = 1:6)
# high residuals - different pattern, high leverage - undue influence
# cook's distance - how much predicted values would change if model were re-estimated with that point eliminated
df[c(57, 129, 295), ]

## multivariate regression
m2 <- lm(overall ~ rides + games + wait + clean, data = df)
summary(m2)
autoplot(m2, which = 1:6)
df[c(59, 441, 475), ]

ggcoef(m2, exclude_intercept = TRUE, color = "blue")

# comparing models - adj R squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

plot_df <- data.frame(overall = df$overall, m1 = m1$fitted.values, m2 = m2$fitted.values) %>%
  melt(id = "overall", variable.name = "model", value.name = "fitted")
ggplot(data = plot_df, aes(x = overall, y = fitted, colour = model)) + 
  geom_point()

anova(m1, m2)

# predict
coef(m2) %*% c(1, rep(100, 4))
predict(m2, newdata = df[1:10, names(m2$model)])

# standardize predictors to compare coefficients - after transformation, before model fitting
df_std <- df[, !names(df) %in% c("distance")]
df_std[, !names(df_std) %in% c("weekend", "num.child")] <- scale(df_std[, !names(df_std) %in% c("weekend", "num.child")])
summary(df_std)

# factors as predictors
m3 <- lm(overall ~ rides + games + wait + clean + weekend + log_dist + num.child, data = df_std)

df_std$num.child.factor <- as.factor(df_std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + weekend + log_dist + num.child.factor, data = df_std)
# coefficients of num.child.factor are about the same - use single dummy
# also remove weekend as not significant
df_std$has_child <- factor(df_std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + log_dist + has_child, data = df_std)

# interaction terms
# when adding interaction effect, good to add main effect (i.e. weekend) with standardized predictors 
m6 <- lm(overall ~ rides + games + wait + clean + weekend + log_dist + has_child +
           rides:has_child + games:has_child + wait:has_child + clean:has_child + 
           rides:weekend + games:weekend + wait:weekend + clean:weekend, data = df_std)
m6_ <- lm(overall ~ rides*has_child + rides*weekend + games*has_child + games*weekend +
            wait*has_child + wait*weekend + clean*has_child + clean*weekend, data = df_std)
# only wait:has_child is significant

m7 <- lm(overall ~ rides + games + wait + clean + log_dist + has_child + wait:has_child, data = df_std)
m8 <- lm(overall ~ rides + games + wait + clean + has_child + wait:has_child, data = df_std)
anova(m8, m7)

autoplot(m7, which = 1:6)
ggcoef(m7, exclude_intercept = TRUE, color = "blue")



