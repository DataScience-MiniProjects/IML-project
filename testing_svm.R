# SVM correlation
library(e1071) #svm naive-bayes
library(caret) 
library(dplyr)
set.seed(42)

npf <- read.csv("/home/omistaja/IML-project/IML-project/npf_train2.csv")
npfm <- read.csv("/home/omistaja/IML-project/IML-project/npf_train2mean.csv")
npfl <- read.csv("/home/omistaja/IML-project/IML-project/npf_train2mean_low.csv")
npfh <- read.csv("/home/omistaja/IML-project/IML-project/npf_train__high.csv")
npfm2 <- read.csv("/home/omistaja/IML-project/IML-project/npf_train_means_mid_height.csv")

rownames(npf) <- npf[,"date"]
npf <- npf[,-1]
rownames(npfm) <- npfm[,"date"]
npfm <- npfm[,-1]
rownames(npfl) <- npfl[,"date"]
npfl <- npfl[,-1]
rownames(npfh) <- npfh[,"date"]
npfh <- npfh[,-1]
rownames(npfm2) <- npfm2[,"date"]
npfm2 <- npfm2[,-1]

idx <- sample.int(nrow(npf),92)

train_all <- npf[ idx,]
test_all <- npf[-idx,]

train_mean <- npfm[ idx,]
test_mean <- npfm[-idx,]

train_low_mean <- npfl[idx,]
test_low_mean <- npfl[-idx, ]

train_high_mean <- npfh[idx,]
test_high_mean <- npfh[-idx, ]

train_m2_mean <- npfm2[idx,]
test_m2_mean <- npfm2[-idx, ]

# PCA svm
# https://rpubs.com/markloessi/505498
dim(train_mean)
head(train_mean[2:26])
train_mean_pca = train_mean
test_mean_pca = test_mean
train_mean_pca[2:26] <- scale(train_mean_pca[2:26]) # training set
test_mean_pca[2:26] <- scale(test_mean_pca[2:26]) # test set

pca = preProcess(x = train_mean_pca[2:26], method = 'pca', pcaComp = 2)
train_mean_pca <- predict(pca, train_mean_pca)
train_mean_pca = train_mean_pca[c(2,3,1)]

test_mean_pca <- predict(pca, test_mean_pca)
test_mean_pca <- test_mean_pca[c(2,3,1)]
head(train_mean_pca)
head(test_mean_pca)

svm_pca <- svm(factor(class4) ~., data=train_mean_pca, type = 'C-classification', kernel="radial", gamma=0.2, cost=15)
pred_pca <- predict(svm_pca, newdata = test_mean_pca[-3])
pred_pca
#accuracy
confusionMatrix(factor(test_mean_pca[,3]), pred_pca)
tpca <- confusionMatrix(factor(test_mean_pca[,3]), pred_pca)$overall
tpca[1] 
cm = table(test_mean_pca[, 3], pred_pca)
cm
svm_pca %>% plot(test_mean_pca)
# end PCA
# mean_only: acc: 0.6338798
# all: acc: 0.6038251
# mean_low: acc: 0.636612
# mean_high: acc: 0.6229508
# m2: acc: 0.6174863

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

svm_mean_only <- svm(factor(class4) ~ ., trControl= train_control, kernel="radial",data=train_mean, gamma=0.2, cost=15)
svm_all <- svm(formula = factor(class4) ~ .,trControl= train_control, kernel="radial", data=train_all, gamma=0.1, cost=10)
svm_low <- svm(factor(class4) ~ ., trControl= train_control, kernel="radial", data=train_low_mean, gamma=0.2, cost=15)
svm_high <- svm(factor(class4) ~ ., trControl= train_control, kernel="radial", data=train_high_mean, gamma=0.2, cost=15)
svm_m2 <- svm(factor(class4) ~ ., trControl= train_control, kernel="radial", data=train_m2_mean, gamma=0.2, cost=15)

pred1 <- predict(svm_mean_only, newdata = test_mean)
#accuracy
confusionMatrix(factor(test_mean$class4), pred1)
tm <- confusionMatrix(factor(test_mean$class4), pred1)$overall
tm[1] # acc: 0.6448087

tuned = tune.svm(class4 ~., data=train_mean, gamma = 0.2, cost=15, tune.control(cross=10))
tuned$performances   
#gamma cost     error dispersion
# 0.2   15  0.4688889  0.1845768


pred2 <- predict(svm_all, newdata = test_all)
confusionMatrix(factor(test_all$class4), pred2)
ta <- confusionMatrix(factor(test_all$class4), pred2)$overall
ta[1] # acc: 0.6038251

tuned2 = tune.svm(class4 ~., data=train_all, gamma = 0.1, cost=10, tune.control(cross=10))
tuned2$performances   
#gamma cost  error dispersion
#0.1   10    0.51  0.1213154

pred3 <- predict(svm_low, newdata = test_low_mean)
confusionMatrix(factor(test_low_mean$class4), pred3)
t3 <- confusionMatrix(factor(test_low_mean$class4), pred3)$overall
t3[1] # 0.6311475

tuned3 = tune.svm(class4 ~., data=train_low_mean, gamma = 0.2, cost=15, tune.control(cross=10))
tuned3$performances   
#gamma cost  error dispersion
# 0.2   15   0.49  0.1914818

pred4 <- predict(svm_high, newdata = test_high_mean)
confusionMatrix(factor(test_low_mean$class4), pred4)
t4 <- confusionMatrix(factor(test_low_mean$class4), pred4)$overall
t4[1] # 0.6338798

tuned4 = tune.svm(class4 ~., data=train_high_mean, gamma = 0.2, cost=15, tune.control(cross=10))
tuned4$performances   
#gamma cost  error     dispersion
# 0.2   15   0.4455556  0.1304682

pred5 <- predict(svm_m2, newdata = test_m2_mean)
confusionMatrix(factor(test_low_mean$class4), pred5)
t5 <- confusionMatrix(factor(test_low_mean$class4), pred5)$overall
t5[1] # 0.6284153

tuned5 = tune.svm(class4 ~., data=train_m2_mean, gamma = 0.2, cost=15, tune.control(cross=10))
tuned5$performances   
#gamma cost  error     dispersion
# 0.2   15   0.4566667  0.1614789



