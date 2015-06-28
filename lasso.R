# LASSO experimental script - not final version

# prepare the data
setwd("~/GitHub/Dothouse")
data <- read.csv("data/dothouse_high_risk.csv")

risk_vars <- c(-1,-3,-4,-5,-42,-43,-44,-53,-54,-55)
predictors <- data[,risk_vars]
NAs <- is.na(predictors)
predictors[NAs] <- 0

# regsubsets(formula, data=, nvmax=)

library(glmnet)
set.seed(1)
x <- model.matrix(~., predictors)
y <- data[,4]
n = nrow(x)
train <- sample(1:n, n/2, replace=FALSE) # pick a training set
test <- (-train)

sensitivty <- 3
grid <- 10^(seq(0, -5, length=10^sensitivty))

lasso.model <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid, standardize=FALSE)
cv.lasso <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 20)
plot(cv.lasso)
lambda_min <- cv.lasso$lambda.min

lasso.model <- glmnet(x, y, alpha = 1, lambda = grid, standardize=FALSE)
coefs <- predict(lasso.model, type="coefficients", s=lambda_min)

lasso_novisit <- predict(lasso.model, s=lambda_min, newx=x)
risk_score <- data[, 5]
novisit <- data[,4]

lm.risk_score <- lm(novisit ~ risk_score, data = data)
rs_novisit <- predict(lm.risk_score)

cor(lasso_novisit, novisit)