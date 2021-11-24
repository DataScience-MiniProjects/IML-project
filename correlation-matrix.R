
setwd(<your path>)
npf <- read.csv("npf_traincopy.csv")
fix(npf)
# View(npf)

rownames(npf) <- npf[,"date"]
# View(npf)

npf <- npf[,-(1:2)]
# View(npf)

npf <- npf[,-2]
npf <- npf[,-1]

npf.cor = cor(npf)
npf.cor

