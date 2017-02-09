library(car)
library(psych)
library(gpairs)
library(reshape2)
library(dplyr)

#options(dplyr.print_max = 1e9)
source("utils.R")

df <- read.csv("data/rintro-chapter5.csv", stringsAsFactors = FALSE)
df <- convert_to_factor(df)

scatterplotMatrix(df, diagonal = "histogram", main = "Scatter Plot Matrices of Selected Variables")
gpairs(df)

## testing group frequencies
# one dimension - H0: values are equally distributed
# multiple - H0: factors are unrelated
#   Pearson's chi-squared test is performed of the null hypothesis that 
#   the joint distribution of the cell counts in a 2-dimensional contingency table 
#   is the product of the row and column marginals.
get_table <- function(vals = c(1:4), times = c(25, 25, 25, 20)) {
  table(rep(vals, times = times))
}

return_chisq <- function(tbl) {
  out <- chisq.test(tbl)
  list(p_value = out$p.value, expected = out$expected, observed = out$observed)
}

return_chisq(get_table())
return_chisq(get_table(times = c(25, 25, 25, 10)))
# significance test is sensitive to both the observed difference and the sample size
# generally significance test is sensitive to both the real effect and the sample size
return_chisq(get_table(times = c(25, 25, 25, 10)/5))

# are segments equally distributed?
seq_tbl <- table(df$Segment)
return_chisq(seq_tbl)
# is segment independent from home ownership?
sub_home_tbl <- table(df$subscribe, df$ownHome)
return_chisq(sub_home_tbl) # home ownership is independent of subscription status

chisq.test(sub_home_tbl) # Yates' correction
chisq.test(sub_home_tbl, correct = FALSE) # match to calculation by hand

chisq.test(sub_home_tbl, simulate.p.value = TRUE, B = 10000)

## testing observed proportions
library(binom)
# CI can be overly conservative (wide)
binom.test(12, 20, p = 0.5)
binom.test(120, 200, p = 0.5)

sum(dbinom(8:12, 20, .5))
sum(dbinom(80:120, 200, .5))

binom.confint(12, 20)
binom.confint(0, 20)

## testing group means
library(lattice)
bwplot(~ income | ownHome, df, horizontal = TRUE, xtab = "income")
hist(df$income)
histogram(~ income | ownHome, df)

# H0: same income by ownership
t.test(income ~ ownHome, data = df)
t.test(income ~ ownHome, data = df[df$Segment == "Travelers",])

## testing multiple group means: ANOVA
# testing for difference among multiple means, assuming the groups have similar variance
aov_income_ownHome <- aov(income ~ ownHome, df)
anova(aov_income_ownHome) # significant
anova(aov(income ~ Segment, df)) # significant
# segment is significant but not own home
# segment and own home are not independent and effect is captured by segment alone
anova(aov(income ~ Segment + ownHome, df))

# formula: + main, : interaction, * both main and interaction
anova(aov(income ~ Segment + ownHome, df))
anova(aov(income ~ Segment : ownHome, df))
anova(aov(income ~ Segment * ownHome, df))

# see if model 2 is significantly different from model 1
# note both model should be nested - LR
# income ~ Segment and income ~ Segment + ownHome nested
# income ~ Segment and income ~ subscribe + ownHome not nested
# comparing non-nested models, check AIC and BIC
anova(aov(income ~ Segment, df), aov(income ~ Segment + ownHome, df))

library(multcomp)
glht(aov(income ~ Segment, df)) # with intercept, relative
glht(aov(income ~ -1 + Segment, df)) # without intercept, absolute

plot(glht(aov(income ~ -1 + Segment, df)))

## variable selection in ANOVA
# backward
aov_step <- step(aov(income ~ ., df))
anova(aov_step)
# when there are many variables, variable selection is better informed by lasso or random forest

## Basysian ANOVA
library(BayesFactor)
set.seed(96761)
seg_bf1 <- lmBF(income ~ Segment, df)
seg_bf2 <- lmBF(income ~ Segment + ownHome, df)
seg_bf1 / seg_bf2

seg_bf_chain <- posterior(seg_bf1, 1, iterations = 1000)
plot(seg_bf_chain)
plot(seg_bf_chain[, 1:6])

summary(seg_bf_chain)






