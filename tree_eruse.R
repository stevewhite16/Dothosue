# hospitalization_trade.R - uses Decision Trees to predict hospitalization
# with the goal of making easy-to-interpret but accurate predictions of risk

library(tree)

# prepare the data
setwd("~/GitHub/Dothouse")
data <- read.csv("data/dothouse_high_risk.csv")

risk_vars <- c(-1,-3,-4,-5,-42,-43,-44,-53,-54,-55)
predictors <- data[,risk_vars]
NAs <- is.na(predictors)
predictors[NAs] <- 0


novisit <- data[,4]
data <- data.frame(novisit,predictors)
n = nrow(data)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

tree <- tree(formula = novisit ~ ., data = data, subset = train)
plot(tree)
text(tree, pretty=0)

high_er_use <- (novisit >= 10)
data <- data.frame(high_er_use, predictors)

tree <- tree(formula = high_er_use ~ ., data = data, subset = train)
plot(tree)
text(tree, pretty=0)

# set.seed(1)
# cv.hosp.tree <- cv.tree(hosp.tree, FUN=prune.misclass)
# best_size <- cv.hosp.tree$size[which.min(cv.hosp.tree$dev)]
# hosp.tree.pruned <- prune.misclass(hosp.tree, best=best_size)
# 
# predictions <- predict(hosp.tree.pruned, data_for_tree[test, ], type="class")
# table(predictions, data_for_tree$hosp[test])
# 
# plot(hosp.tree.pruned)
# text(hosp.tree.pruned, pretty=0)