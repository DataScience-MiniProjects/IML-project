library(randomForest)
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

rF_mean_only <- randomForest(Class4 ~ . train_mean)
rf_all <- randomForest(Class4 ~ ., train_all)
