setwd('~/IMLproject2021')
library(randomForest)
library(caret)
set.seed(1)

npf <- read.csv("npf_Train2.csv")
npfm  <- read.csv("npf_train_means_height.csv")
npfm2 <- read.csv("npf_train2mean_low.csv")
npfm3 <- read.csv("npf_train_means_mid_height.csv")
npfm4 <- read.csv("npf_Train2mean.csv")
rownames(npf) <- npf[,"date"]
npf <- npf[,-1]
rownames(npfm) <- npfm[,"date"]
npfm <- npfm[,-1]

rownames(npfm2) <- npfm2[,"date"]
npfm2 <- npfm2[,-1]
rownames(npfm3) <- npfm3[,"date"]
npfm3 <- npfm3[,-1]

rownames(npfm4) <- npfm4[,"date"]
npfm4 <- npfm4[,-1]
idx <- sample.int(nrow(npfm),300)

#rain_all <- npf[ idx,]
#test_all <- npf[-idx,]

train_mean <- npfm[ idx,]
test_mean <- npfm[-idx,]

train_mean2 <- npfm2[ idx,]
test_mean2 <- npfm2[-idx,]
train_mean3 <- npfm3[ idx,]
test_mean3 <- npfm3[-idx,]

train_mean4 <- npfm4[ idx,]
test_mean4 <- npfm4[-idx,]

# mtry by default: square of the number of columns
rF_mean <- randomForest(formula = factor(class4) ~ ., 
                             data=train_mean,
                             ntree=1000, importance=T,
                             proximity=T)

rF_mean2 <- randomForest(formula = factor(class4) ~ ., 
                             data=train_mean2,
                             ntree=1000, importance=T,
                             proximity=T)
rF_mean3 <- randomForest(formula = factor(class4) ~ ., 
                                data=train_mean3,
                             ntree=1000, importance=T,
                             proximity=T)
rF_mean4 <- randomForest(formula = factor(class4) ~ ., 
                             data=train_mean4,
                             ntree=1000, importance=T,
                             proximity=T)

rF_mean
rF_mean2
rF_mean3
rF_mean4
plot(rF_mean)
plot(rF_mean2)
plot(rF_mean3)
plot(rF_mean4)
#rF_mean_tuned <-tuneRF(
#  x=train_mean[,], #define predictor variables
#  y=factor(train_mean$class4), #define response variable
#  ntreeTry=5,
#  mtryStart=9, 
#  stepFactor=1.5,
#  improve=T,
#  trace=FALSE #don't show real-time progress
#)
#rF_mean_tuned 
pred1 <- predict(rF_mean, newdata = test_mean)
confusionMatrix(factor(test_mean$class4), pred1)

pred2 <- predict(rF_mean2, newdata = test_mean2)
confusionMatrix(factor(test_mean2$class4), pred2)

pred3 <- predict(rF_mean3, newdata = test_mean3)
confusionMatrix(factor(test_mean3$class4), pred3)

pred4 <- predict(rF_mean4, newdata = test_mean4)
confusionMatrix(factor(test_mean4$class4), pred4)

rF_mean$confusion
rF_mean2$confusion
rF_mean3$confusion
rF_mean4$confusion
