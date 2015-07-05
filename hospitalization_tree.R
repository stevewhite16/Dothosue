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

set.seed(5)
NA_y <- is.na(data$hospitalization)
data$hospitalization[NA_y] <- 0
hosp <- data$hospitalization
data_for_tree <- data.frame(hosp, predictors)
n = nrow(data_for_tree)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

n_train <- length(train)
overfit <- tree.control(nobs=n_train, minsize=2, mindev=0)
hosp.tree <- tree(formula = hosp ~ ., data = data_for_tree, subset = train)
plot(hosp.tree)
text(hosp.tree, pretty=0)

set.seed(1)
cv.hosp.tree <- cv.tree(hosp.tree, FUN=prune.tree)
best_size <- cv.hosp.tree$size[which.min(cv.hosp.tree$dev)]
hosp.tree.pruned <- prune.tree(hosp.tree, best=best_size)

# this code doesn't work since it isn't for a factor y variable anymore
# predictions <- predict(hosp.tree.pruned, data_for_tree[test, ], type="class")
# table(predictions, data_for_tree$hosp[test])
  
plot(hosp.tree.pruned)
text(hosp.tree.pruned, pretty=0)