#### finding actionable business outcomes
#### ensuring outcome is meaningful for a particular business need

library(cluster)
library(mclust)
library(poLCA)
library(magrittr)
library(dplyr)

df <- read.csv("data/rintro-chapter5.csv", stringsAsFactors = TRUE)
seg_df <- df %>% select(-Segment)

#### segmentation by clustering
## distance-based - heuristic (hclust()) and kmeans (kmeans())
## model-based - Mclust() and poLCA()
# data as mixture of groups sampled from different distributions  and group membership has been lost

seg_sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}
## are there obvious differences in groups?
## does differentiation point to some underlying story to tell?
## are immediate odd results such as equal mean?

d <- dist(seg_df[, c("age", "income", "kids")])
seg_dist <- daisy(seg_df)
as.matrix(seg_dist)[1:5, 1:5]

seg_hc <- hclust(seg_dist, method = "complete")
plot(seg_hc)
plot(cut(as.dendrogram(seg_hc), h = 0.5)$lower[[1]])

seg_df[c(101, 107),]
seg_df[c(278, 294),]

seg_df[c(183, 141),]

# goodness of fit - cophenetic correlation coefficient (CPCC)
cor(cophenetic(seg_hc), seg_dist)

plot(seg_hc)
rect.hclust(seg_hc, k = 4, border = "red")

seg_hc_segment <- cutree(seg_hc, k = 4)
table(seg_hc_segment)

seg_sum(seg_df, seg_hc_segment)

plot(jitter(as.numeric(seg_df$gender)) ~ jitter(as.numeric(seg_df$subscribe)),
     col = seg_hc_segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at = c(1, 2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at = c(1, 2), labels = levels(seg_df$gender))

library(ggplot2)
gg_df <- seg_df
ggplot(gg_df, aes(subscribe, gender)) + 
  geom_point(position = position_jitter(w = 0.5, h = 0.5), alpha = 0.5, color = "firebrick")

## uninteresting outcomes
# two subscribe = NO groups that are differentiated only by gender
# daisy() rescale variables [0, 1] and makes two-category factors more influential
# should try several methods and interate in order to find something useful

### K-means clustering
seg_df_num <- seg_df %>% mutate(gender = ifelse(gender == "Male", 0, 1),
                                ownHome = ifelse(ownHome == "ownNo", 0, 1),
                                subscribe = ifelse(subscribe == "subNo", 0, 1))
summary(seg_df_num)

set.seed(96743)
seg_k <- kmeans(seg_df_num, centers = 4)

seg_sum(seg_df_num, seg_k$cluster)

boxplot(seg_df_num$income ~ seg_k$cluster, ylab = "Income", xlab = "Cluster")
boxplot(seg_df_num$age ~ seg_k$cluster, ylab = "Age", xlab = "Cluster")
clusplot(seg_df, seg_k$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means cluster plot")

### Model-based clustering
# observations come from groups with different statistical distributions (eg different mean and variance)
# mcluster models clusters as being drawn from a mixture of normal
seg_mc <- Mclust(seg_df_num)
summary(seg_mc)

seg_mc_3 <- Mclust(seg_df_num, G = 3)
summary(seg_mc_3)

seg_mc_4 <- Mclust(seg_df_num, G = 4)
summary(seg_mc_4)

BIC(seg_mc, seg_mc_4)
BIC(seg_mc, seg_mc_3)
diff(BIC(seg_mc, seg_mc_3)$BIC)

# but best model doesn't necessarily mean good business insight 
seg_sum(seg_df_num, seg_mc_3$classification)
seg_sum(seg_df_num, seg_mc_4$classification)

clusplot(seg_df, seg_mc_3$classification, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Model-based cluster plot")
clusplot(seg_df, seg_mc_4$classification, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "Model-based cluster plot")

### latent class analysis: poLCA()
seg_df_cut <- seg_df %>% mutate(age = factor(ifelse(age < median(age), 1, 2)),
                                income = factor(ifelse(income < median(income), 1, 2)),
                                kids = factor(ifelse(kids < median(kids), 1, 2)))

seg_f <- with(seg_df_cut, cbind(age, gender, income, kids, ownHome, subscribe) ~ 1)

set.seed(02807)
seg_LCA3 <- poLCA(seg_f, data = seg_df_cut, nclass = 3)
seg_LCA4 <- poLCA(seg_f, data = seg_df_cut, nclass = 4)

seg_LCA3$bic
seg_LCA4$bic

table(seg_LCA3$predclass)
table(seg_LCA4$predclass)

seg_sum(seg_df, seg_LCA3$predclass)
seg_sum(seg_df, seg_LCA4$predclass)

par(mfrow=c(1,2))
clusplot(seg_df, seg_LCA3$predclass, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "LCA plot (K=3)")
clusplot(seg_df, seg_LCA4$predclass, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "LCA plot (K=4)")

par(mfrow=c(1,1))

#### comparing cluster solutions
table(seg_LCA3$predclass, seg_LCA4$predclass)

mapClass(seg_LCA3$predclass, seg_LCA4$predclass)
adjustedRandIndex(seg_LCA3$predclass, seg_LCA4$predclass)

set.seed(11021)
random_data <- sample(4, length(seg_LCA4$predclass), replace = TRUE)
adjustedRandIndex(random_data, seg_LCA4$predclass)

table(df$Segment, seg_LCA4$predclass)
adjustedRandIndex(df$Segment, seg_LCA4$predclass)

############ classification



