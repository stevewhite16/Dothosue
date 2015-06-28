# Principle Components of the Risk Scores
library(nsprcomp)

# setwd("GitHub/Dothouse")
data <- read.csv("data/dothouse_high_risk.csv")

risk_vars <- c(-1,-2,-3,-4,-5,-42,-43,-44,-53,-54,-55)
risk_scores <- data[,risk_vars]
NAs <- is.na(risk_scores)
risk_scores[NAs] <- 0

pc <- nsprcomp(x = risk_scores, ncomp = 5, scale. = TRUE)
summary(pc)

