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
n <- nrow(data)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

n_train <- length(train)
overfit <- tree.control(nobs=n_train, mincut=1, minsize=2, mindev=0.001)
er_tree <- tree(formula = novisit ~ ., data = data, subset = train, control = overfit)
plot(er_tree)
text(er_tree, pretty=0)

set.seed(1)
cv.er_tree <- cv.tree(er_tree, FUN=prune.tree)
best_size <- cv.er_tree$size[which.min(cv.er_tree$dev)]
er_tree.pruned <- prune.tree(er_tree, best=best_size)

# try a classification tree for high_er_use
cutoff <- 10
high_er_use <- (novisit >= cutoff)
high_er_use <- as.factor(high_er_use)
levels(high_er_use) <- c("l","h")
data <- data.frame(high_er_use, predictors)
n <- nrow(data)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

n_train <- length(train)
overfit <- tree.control(nobs=n_train, mincut=1, minsize=2, mindev=0.001)
er_tree <- tree(formula = high_er_use ~ ., data = data, subset = train, control = overfit)
plot(er_tree)
text(er_tree, pretty=0)

set.seed(1)
cv.er_tree <- cv.tree(er_tree, FUN=prune.misclass)
best_size <- cv.er_tree$size[which.min(cv.er_tree$dev)]
er_tree.pruned <- prune.misclass(er_tree, best=best_size)

predictions <- predict(er_tree.pruned, data[test, ], type="class")
table(predictions, data$high_er_use[test])