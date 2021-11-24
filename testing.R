library(randomForest)
library(caret)
set.seed(42)

npf <- read.csv("npf_train2.csv")
npfm <- read.csv("npf_train2mean.csv")

rownames(npf) <- npf[,"date"]
npf <- npf[,-1]
rownames(npfm) <- npfm[,"date"]
npfm <- npfm[,-1]

idx <- sample.int(nrow(npf),92)

train_all <- npf[ idx,]
test_all <- npf[-idx,]

train_mean <- npfm[ idx,]
test_mean <- npfm[-idx,]

rF_mean_only <- randomForest(formula = factor(class4) ~ ., data=train_mean, mtry=20, ntree=10000)
rf_all <- randomForest(formula = factor(class4) ~ ., data=train_all, mtry=20, ntree=10000)

rF_mean_only
plot(rF_mean_only)
rf_all
rF_mean_tuned <-tuneRF(
  x=train_mean[,], #define predictor variables
  y=factor(train_mean$class4), #define response variable
  ntreeTry=1000,
  mtryStart=9, 
  stepFactor=1.5,
  improve=T,
  trace=FALSE #don't show real-time progress
)
rF_mean_tuned 
pred1 <- predict(rF_mean_only, newdata = test_mean)
confusionMatrix(factor(test_mean$class4), pred1)

pred2 <- predict(rf_all, newdata = test_all)
confusionMatrix(factor(test_all$class4), pred2)

