setwd('~/IMLproject2021')
library(randomForest)
library(caret)
set.seed(42)

npf <- read.csv("npf_Train2mean.csv")
npf$class2 <- factor("event",levels=c("nonevent","event"))
npf$class2[npf$class4=="nonevent"] <- "nonevent"
rownames(npf) <- npf[,"date"]
npf <- npf[,-1]

idx <- sample.int(nrow(npf),92)

training_set <- npf[ idx,]
validation_set <- npf[-idx,]

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     classProbs = TRUE,
                     savePredictions = "final")
rFClass4 <- train(factor(class4) ~ .,
                    method="rf",
                    data=training_set,
                    trControl=ctrl,
                    preProc=c("pca","center", "scale"))
rFClass4

pred4 <- predict(rFClass4, newdata = validation_set)

probs4 <- predict(rFClass4, newdata = validation_set, type = "prob")

confusionMatrix(factor(validation_set$class4), pred4)

testClass4 <-function(p) {
  ia <- 0
  ib <- 0
  ii <- 0
  nonevent <- 0
  for (i in 1:length(dataset$class2)) {
    if (p$Ia[i] >=0.5) {
      ia = ia + 1
    }
    if (p$Ib[i] >=0.5) {
      ib = ib + 1
    }
    if (p$II[i] >=0.5) {
      ii = ii + 1
    }
    if (p$nonevent[i] >0.5) {
      nonevent = nonevent + 1
    }
  }
  return(list(ia, ib, ii, nonevent))
}

accurracy4 <- accClass4(probs4, validation_set)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 10,
                     classProbs = TRUE,
                     savePredictions = "final")
rFClass2 <- train(factor(class2) ~ .,
            method="rf",
            data=training_set,
            trControl=ctrl,
            preProc=c("pca","center", "scale"))
rFClass2

pred2 <- predict(rFClass2, newdata = validation_set)

probs2 <- predict(rFClass2, newdata = validation_set, type = "prob")

confusionMatrix(factor(validation_set$class2), pred2)

accuracy2 <- accClass2(probs2, validation_set)
accuracy2

