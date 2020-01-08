# minimial example

train = read.csv("kaggle/train.csv")
test = read.csv("kaggle/test.csv")

model = glm(target~., data = train[,-1], family = binomial)

pred = predict(model, test[,-1], type = "response")
pred = ifelse(pred < 0.5, 0, 1)

pred = cbind(as.numeric(test$id), pred)
colnames(pred) = c("id", "target")

write.csv(pred,file = "sub.csv",row.names = FALSE)


# xgboost, keras, random forest, elastic net