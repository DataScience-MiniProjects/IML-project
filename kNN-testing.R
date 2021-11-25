
## Testing the caret package nearest neighbor classifier on the project data

setwd("C:\\Users\\fanni\\Documents/IntroToML/Project")
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


# Now [,"class4"] gives y
# and [,-1] should give X

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs=T)
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

all_probs <- predict(knn_all, newdata = test_all, type = "prob")
mean_probs <- predict(knn_mean, newdata = test_mean, type = "prob")

conf_all <- confusionMatrix(all_pred, test_all$class4)
conf_mean <- confusionMatrix(mean_pred, test_mean$class4)

