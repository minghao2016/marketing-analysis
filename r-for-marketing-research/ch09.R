library(gpairs)
library(car)
library(forecast)
library(vcd)
library(vcdExtra)
library(lme4)
library(dplyr)

#### highly correlated variables
df <- read.csv("data/rintro-chapter4.csv", stringsAsFactors = TRUE)
gpairs(df)

## which variables are most predictive of online spending
## which factors to consider so as to increase online spending

spend_m1 <- lm(online.spend ~ . - cust.id, data = df, subset = online.spend > 0)
# positive online.trans but negative online.visits
# too high adjusted r squared

## loglik method doesn't work so that powerTransform, preProcess don't work
auto_transform <- function(x) {
  lambda <- BoxCox.lambda(x)
  message(paste("lambda", lambda))
  scale(BoxCox(x, lambda))
}

df_bc <- df[complete.cases(df), -1] %>% filter(online.spend > 0)
is_non_email <- names(df_bc)[!names(df_bc) %in% "email"]
df_bc[, is_non_email] <- lapply(df_bc[, is_non_email], auto_transform)

gpairs(df_bc)

spend_m2 <- lm(online.spend ~ ., data = df_bc)
spend_m3 <- lm(online.spend ~ online.trans, data = df_bc)

anova(spend_m3, spend_m2)

## remedy collinearity
vif(spend_m2)
## need to mitigate collinearity if VIF > 5
# omit highly correlated variables
# eliminate correlation by PCA or EFA
# use method that's robust to collinearity (eg random forest)
# data specific <- transform variables

spend_m4 <- lm(online.spend ~ . - online.trans - store.trans, data = df_bc)

pc_online <- prcomp(df_bc[, c("online.visits", "online.trans")])
df_bc$online <- pc_online$x[, 1]
pc_store <- prcomp(df_bc[, c("store.trans", "store.spend")])
df_bc$store <- pc_store$x[, 1]

spend_m5 <- lm(online.spend ~ . - online.trans - online.visits - store.trans - store.spend, data = df_bc)
# sign of coefficient is meaningless
vif(spend_m5)

#### binary outcomes
# logistic p(y) = exp(Vx) / (exp(Vx) + 1)
# logit: Vx = log(p(y) / (1 - p(y)))

# pass_df <- read.csv("data/rintro-chapter9.csv")
# pass_df$Promo <- factor(pass_df$Promo, levels = c("NoBundle", "Bundle"))

pass_tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass_tab) <- c(3, 2, 2)
class(pass_tab) <- "table"
dimnames(pass_tab) <- list(Channel = c("Mail", "Park", "Email"),
                           Promo = c("Bundle", "NoBundle"),
                           Pass = c("YesPass", "NoPass"))

pass_df <- expand.dft(pass_tab)
pass_df$Promo <- factor(pass_df$Promo, levels = c("NoBundle", "Bundle"))

#common feature of all GLM models is that they relate normally distributed predictors to a non-normal outcome using a function known as a link
gpairs(pass_df)

pass_m1 <- glm(Pass ~ Promo, data = pass_df, family = binomial)
summary(pass_m1)

# odd ration of Promo - Bundle to Promo - NoBundle
# percentage increase of purchase likelihood
plogis(0.3888) / (1 - plogis(0.3888))
exp(0.3888)
exp(coef(pass_m1))

exp(confint(pass_m1))

## Simpson's paradox
# Channel park dominates and purchase is most successful at park regardless of bundle
table(pass_df$Pass, pass_df$Channel)
doubledecker(table(pass_df))

pass_m2 <- glm(Pass ~ Promo + Channel, data = pass_df, family = binomial)
summary(pass_m2)

pass_m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, data = pass_df, family = binomial)
summary(pass_m3)

exp(confint(pass_m3))

##### HLM
### estimate both population and individual level effect
## individual effects follow a distribution across the population
## fit overall model to all data and attempts to determine best fit for each individual within that overall estimate
## vs factor
# factor variable add a single term that adjusts model up or down while HLM can estimate every coefficient for each individual

## individual level (eg customer) estimation or estimation by group (eg geographic region, store, salesperson, product or promotion campaign)

## fixed effect (global) and random effect (adjustment for individual or group)
# why random? estimated as random variables that follow a distribution around the fixed estimates

## hierarchical model, multilevel model, mixed effect model, nested model

### rating based conjoint analysis
df <- read.csv("data/rintro-chapter9conjoint.csv", stringsAsFactors = FALSE)
df$speed <- as.factor(df$speed)
df$height <- as.factor(df$height)

df %>% group_by(height) %>% summarize(mean(rating))

ride_lm <- lm(rating ~ speed + height + const + theme, data = df)
summary(ride_lm)

## intercept only HLM
# (random | group)
ride_hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id), data = df)
summary(ride_hlm1)

fixef(ride_hlm1)
ranef(ride_hlm1)$resp.id %>% head()
coef(ride_hlm1)$resp.id %>% head()

## complete HLM
ride_hlm2 <- lmer(rating ~ speed + height + const + theme +
                    (speed + height + const + theme | resp.id),
                  data = df, control = lmerControl(optCtrl = list(maxfun = 100000)))
# slow and convergence can be an issue - increase maxfun
# check whether maximum absolute value of gradient in opt function < 0.001 - if > 0.01, increase iteration
# consider different optimization function
# more data













# library(cluster)
# set.seed(1237)
# xfac <- sample(letters[1:6], 100, replace = TRUE)
# yfac <- xfac
# yfac[sample(1:length(yfac), 50)] <- sample(letters[1:6], 50, replace = TRUE)
# df <- data.frame(xfac = as.factor(xfac), yfac = as.factor(yfac))
# daisy(df, metric = "gower")
# 
# 
# library(FactoMineR)
# survey <- read.table("http://factominer.free.fr/classical-methods/datasets/women_work.txt", header=TRUE, row.names=1, sep="\t")
# survey <- survey[, 1:3]
# rownames(survey) <- c("both", "man_more", "man_only")
# colnames(survey) <- c("stay_home", "part_time", "full_time")
# 
# res <- CA(survey)
