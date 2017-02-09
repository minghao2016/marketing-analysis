# events occur together more often than individuals, they are interesting
# association - co-occurrence
# rule - incidence across transactions of one set of items as a condition of another set of items {A} => {B}

## metrics
# support - marginal proportion of {X, Y}, doesn't matter if another item exists eg {X, Y, Z}
# confidence - confidence(X => Y) = support(X and Y) / support(X), divide support of left only
# lift - lift(X => Y) = support(X and Y) / (support(X)*support(Y)), how much more co-occurrence than expected if two are independent

# exceed a minimum threshold on each in order to find item sets that occur relatively frequently in transactions (support)
# that show strong conditional relationships (confidence) and that are more common than chance (lift)

library(car)
library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)

inspect(head(Groceries, 3))

groc_rules <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3, target = "rules"))
# number of items in rules - sorting and recoding items ... [88 item(s)] done [0.00s]
# number of rules - writing ... [125 rule(s)] done [0.00s]

inspect(subset(groc_rules, lift > 3))

## supermarket data
retail_raw <- readLines("data/retail.dat")
retail_list <- strsplit(retail_raw, " ")
names(retail_list) <- paste("Trans", 1:length(retail_list), sep = "")

str(retail_list)
some(retail_list)

retail_trans <- as(retail_list, "transactions")
summary(retail_trans)

rm(retail_list)

retail_rules <- apriori(retail_trans, parameter = list(supp = 0.001, conf = 0.4))
plot(retail_rules)
plot(retail_rules, interactive = TRUE)

inspect(subset(retail_rules, lift > 205 & confidence > 0.994))
#            lhs           rhs     support     confidence lift    
#2706 {16431,48} => {16430} 0.001973639 0.9942857  205.7705

# {16431,48} occurs in about 0.2% and when it occurs it almost always includes {16430} and
# the combination occurs 205 times more often than {16431,48} and {16430} considered separately

retail_hi <- head(sort(retail_rules, by = "lift"), 50)
inspect(retail_hi)

plot(retail_hi, method = "graph", control = list(type = "items"))

retail_item_names <- sort(unique(unlist(as(retail_trans, "list"))))
set.seed(03870)
retail_margin <- data.frame(margin = rnorm(length(retail_item_names), mean = 0.3, sd = 0.3))
rownames(retail_margin) <- retail_item_names

quantile(retail_margin$margin)
some(retail_margin)

retail_margin[c("39", "48"),]
sum(retail_margin[c("39", "48"),])

basket_items <- as(retail_trans[3], "list")[[1]]
retail_margin[basket_items,]
sum(retail_margin[basket_items,])

retail_margsum <- function(items, item_margins) {
  if(class(items) == "rules") {
    tmp_items <- as(items(items), "list")
  } else if(class(items) == "transactions") {
    tmp_items <- as(items, "list")
  } else if(class(items) == "list") {
    tmp_items <- items
  } else if(class(items) == "character") {
    tmp_items <- list(items)
  } else {
    stop("Don't know how to handle margin for class ", class(items))
  }

  good_items <- unlist(lapply(tmp_items, function(x) {
    all(unlist(x) %in% rownames(item_margins))
  }))
  
  if(!all(good_items)) {
    warning("Some items not found in rownames of item margins. ",
            "Lookup failed for element(s): \n",
            paste(tmp_items[!good_items], collapse = "\n"), "\nReturning only good values")
    tmp_items <- tmp_items[good_items]
  }
  
  return(unlist(lapply(tmp_items, function(x) sum(item_margins[x, ]))))
}

retail_margsum(c("39", "48"), retail_margin)
retail_margsum(list(t1 = c("39", "45"), t2 = c("31", "32")), retail_margin)
retail_margsum(retail_trans[101:103], retail_margin)
retail_margsum(retail_hi, retail_margin)
retail_margsum(c("hello", "world"), retail_margin)
retail_margsum(list(t1 = c("39", "45"), t2 = c("31", "32"), t3 = c("hello", "world")), retail_margin)



