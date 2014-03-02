universe <- read.csv(file="universe.csv", head=T, sep=",")
universe[is.na(universe)] <- 0

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(universe$target, p = 0.7, list = FALSE, times = 1)
train <- universe[trainIndex, ]
test  <- universe[-trainIndex, ]

rho <- cor(train[,1:ncol(train)])
rho <- as.matrix(sort(abs(rho[,1]), decreasing=T))
rownames(rho)

fit <- glm(train$target ~ 
             train$var1 + train$var2
R2 <- cor(train$target, predict(fit))^2

testMargin <- coef(fit)[1] + 
  coef(fit)[2] * test$var1 + 
  coef(fit)[3] * test$var2

RMSE <- sqrt(mean((testMargin - test$target)^2))
