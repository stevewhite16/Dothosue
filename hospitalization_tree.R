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

set.seed(1)
NA_y <- is.na(data$hospitalization)
data$hospitalization[NA_y] <- 0
hosp <- data$hospitalization
data_for_tree <- data.frame(hosp, predictors)
n = nrow(x)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

hosp.tree <- tree(formula = hosp ~ ., data = data_for_tree, subset = train)

set.seed(1)
cv.hosp.tree <- cv.tree(hosp.tree, FUN=prune.misclass)
best_size <- cv.hosp.tree$size[which.min(cv.hosp.tree$dev)]
hosp.tree.pruned <- prune.misclass(hosp.tree, best=best_size)

predictions <- predict(hosp.tree.pruned, data_for_tree[test, ], type="class")
table(predictions, data_for_tree$hosp[test])
  
plot(hosp.tree.pruned)
text(hosp.tree.pruned, pretty=0)