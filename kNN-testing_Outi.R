
## Testing the caret package nearest neighbor classifier on the project data

library(caret)

set.seed(42)
npf <- read.csv("npf_Train2.csv")
npfm <- read.csv("npf_Train2mean.csv")

rownames(npf) <- npf[,"date"]
npf <- npf[,-1]
rownames(npfm) <- npfm[,"date"]
npfm <- npfm[,-1]

idx <- sample.int(nrow(npf),92)

train_all <- npf[ idx,]
test_all <- npf[-idx,]

train_mean <- npfm[ idx,]
test_mean <- npfm[-idx,]


# Now [,"class4"] gives y
# and [,-1] should give X

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_all <- train(class4 ~., data = train_all, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_mean <- train(class4 ~., data = train_mean, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

all_pred <- predict(knn_all, newdata = test_all)
mean_pred <- predict(knn_mean, newdata = test_mean)

conf_all <- confusionMatrix(all_pred, factor(test_all$class4))
conf_mean <- confusionMatrix(mean_pred, factor(test_mean$class4))
conf_mean
#rmse <- function(yhat,y) sqrt(mean((y-yhat)**2))

#knn_all_rmse <- rmse(all_pred, test_all$class4)
#knn_mean_rmse <- rmse(mean_pred, test_all$class4)

#knn_all <- knnreg(class4 ~ .,data=train_all, k=5)
#knn_mean <- knnreg(class4 ~ .,data=train_mean, k=5)

#knn_all_y <- predict(knn_all, newdata = test_all)
#knn_mean_y <- predict(knn_mean, newdata = test_mean)

#knn_all_rmse <- rmse(knn_all_y, test_all$class4)
#knn_mean_rmse <- rmse(knn_mean_y, test_all$class4)



