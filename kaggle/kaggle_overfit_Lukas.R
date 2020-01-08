train <- read.csv("kaggle/train.csv")
test <- read.csv("kaggle/test.csv")

library(glmnet)
# formula <- as.formula(paste0("target ~ ",paste0("s(X",0:299,")",collapse="+")))
model <- glmnet::cv.glmnet(as.matrix(train[,-c(1, 2)]), train$target, family = 'binomial')

pred <- predict(model, newx = as.matrix(test[,-c(1)]), type = "class")

P <- data.frame(test[, c(1)], pred)
colnames(P) <- c("id", "target")

write.csv(P, file = "sub.csv", row.names = FALSE)
