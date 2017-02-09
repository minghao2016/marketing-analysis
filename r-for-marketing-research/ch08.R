## PCA for perceptual maps
library(nFactors)
library(GPArotation)
library(semPlot)
library(cluster)
library(gpairs)
library(corrplot)
library(gplots)
library(RColorBrewer)
library(reshape2)
library(dplyr)

source("utils.R")

df <- read.csv("data/rintro-chapter8.csv", stringsAsFactors = TRUE)
#gpairs(df)

df_sc <- df
df_sc[, !names(df) %in% "brand"] <- scale(df_sc[, !names(df) %in% "brand"])
#gpairs(df_sc)

corrplot(cor(df_sc[, !names(df) %in% "brand"]), order = "hclust")

# average mean ratings by brand
aggregate(. ~ brand, data = df_sc, FUN = mean)
brand_mean <- df_sc %>% group_by(brand) %>% summarise_each(funs(mean)) %>%
  reset_rownames(col = "brand") %>% as.matrix()

heatmap.2(brand_mean, col = brewer.pal(9, "GnBu"), trace = "none",
          key = FALSE, dendrogram = "none", main = "\n\n\n\n\nBrand attributes")

### PCA
set.seed(98286)
xvar <- sample(1:10, 100, replace = TRUE)
yvar <- xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace = TRUE)
zvar <- yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace = TRUE)
my_vars <- cbind(xvar, yvar, zvar)

plot(yvar ~ xvar, data = jitter(my_vars))
cor(my_vars)

# expectation
# component 1 for common variance
# component 2 for uniqueness of yvar

my_pca <- prcomp(my_vars)
summary(my_pca)

cor(my_pca$x)

biplot(my_pca)

## PCA for brand rating
brand_pc <- prcomp(df_sc[, !names(df) %in% "brand"])
summary(brand_pc)

plot(brand_pc, type = "l")
biplot(brand_pc)

brand_mean_pc <- prcomp(brand_mean, scale. = TRUE)
summary(brand_mean_pc)

# perceptual map
# with mean - well differentiated sets of adjectives and brands
biplot(brand_mean_pc, main = "Brand positioning", cex = c(1.5, 1))

# e is not well differentiated and want to move in direction of c
brand_mean["c", ] - brand_mean["e", ]

# how to position value-leader?
colMeans(brand_mean[c("b", "c", "g", "f"), ]) - brand_mean["e", ]

### cautions
## carefully choose level/type of aggregation - median (ordinal) or mode (categorical) if necessary
# check dimensioins are similar for full and aggregated data
## outcome can be sensitive - repeat different times with subset or dropping adjective
## strength of a brand on a single adjective cannot be read directly from the chart
## adjective positions are relative

##### EFA
## determine # factors
# scree plot and eigenvalue > 1
scree <- nScree(df_sc[, !names(df_sc) %in% "brand"])
scree

plotnScree(scree)

eigen(cor(df_sc[, !names(df_sc) %in% "brand"]))

factanal(df_sc[, !names(df_sc) %in% "brand"], factors = 2)
# factor 1 - bargain, value
# factor 2 - leader, serious

brand_fa <- factanal(df_sc[, !names(df_sc) %in% "brand"], factors = 3)
# factor 1 - bargain, value
# factor 2 - leader, serious
# factor 3 - latest, trendy

## allow factors to be correlated or not
# value and leader may be negatively correlated
brand_fa_ob <- factanal(df_sc[, !names(df_sc) %in% "brand"], factors = 3, rotation = "oblimin")
brand_fa_ob

heatmap.2(brand_fa_ob$loadings, col = brewer.pal(9, "Greens"), trace = "none", key = FALSE, dend = "none",
          Colv = FALSE, cexCol = 1, main = "\n\n\n\n\nFactor loading for brand adjectives")

semPaths(brand_fa_ob, what = "est", residuals = FALSE, cut = 0.3, posCol = c("white", "darkgreen"),
         negCol = c("white", "red"), edge.label.cex = 0.75, nCharNodes = 7)

brand_fa_ob <- factanal(df_sc[, !names(df_sc) %in% "brand"], factors = 3, rotation = "oblimin", scores = "Bartlett")
brand_scores <- data.frame(brand_fa_ob$scores)
brand_scores$brand <- df_sc$brand
head(brand_scores)

aggregate(. ~ brand, data = brand_scores, FUN = mean)
brand_fa_mean <- brand_scores %>% group_by(brand) %>% summarise_each(funs(mean)) %>%
  reset_rownames(col = "brand") %>% as.matrix()

heatmap.2(brand_fa_mean, col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE, dendrogram = "none",
          cexCol = 1.2, main = "\n\n\n\n\nMean factor score by brand")

#### MDS
brand_dist <- dist(brand_mean)
brand_mds <- cmdscale(brand_dist)

plot(brand_mds, type = "n")
text(brand_mds, rownames(brand_mds), cex = 2)

brand_rank <- as.data.frame(brand_mean)
brand_rank[] <- lapply(brand_rank[], function(x) ordered(rank(x)))

brand_dist_r <- daisy(brand_rank, metric = "gower")
brand_mds_r <- isoMDS(brand_dist_r)

plot(brand_mds_r$points, type = "n")
text(brand_mds_r$points, rownames(brand_mds_r$points), cex = 2)
