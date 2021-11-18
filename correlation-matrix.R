setwd("C:Your/path/here")

npf <- read.csv("npf_train.csv")
# fix(npf)
# View(npf)

rownames(npf) <- npf[,"date"]
# View(npf)

npf <- npf[,-(1:2)]
# View(npf)

npf <- npf[,-2]
npf <- npf[,-1]

npf.cor = cor(npf)