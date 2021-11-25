# SVM correlation
library(e1071) #svm naive-bayes
library(caret) 
set.seed(42)
# standard deviationeilla ja ilman npftrain2 nptrain2mean

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

svm_mean_only <- svm(factor(class4) ~ ., data=train_mean, gamma=0.2, cost=15)
svm_all <- svm(formula = factor(class4) ~ ., data=train_all, gamma=0.1, cost=10)

summary(svm_mean_only)

summary(svm_all)

train_mean
dev.off()
plot(svm_mean_only, train_mean, CS.mean ~ RHIRGA42.mean)

pred1 <- predict(svm_mean_only, newdata = test_mean)
table(factor(test_mean$class4), pred1)
#accuracy
confusionMatrix(factor(test_mean$class4), pred1)
tm <- confusionMatrix(factor(test_mean$class4), pred1)$overall
tm[1] # acc: 0.6448087

pred2 <- predict(svm_all, newdata = test_all)
table(factor(test_all$class4), pred2)
confusionMatrix(factor(test_all$class4), pred2)
ta <- confusionMatrix(factor(test_all$class4), pred2)$overall
ta[1] # acc: 0.6038251

#data(iris)
#iris$Species -  setosa, versicolor, virginica
