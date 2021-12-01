
#setwd("path/to/folder")

set.seed(42)

npf <- read.csv("npf_train.csv")

rownames(npf) <- npf[,"date"]
npf <- npf[,c(-1,-4)]

npf <- npf[, c("date",	"class4", "CO242.mean",	"CO2504.mean",	"Glob.mean",	
                "H2O42.mean",	"H2O504.mean", "NET.mean",	"NO42.mean",	"NO504.mean",	
                "NOx42.mean",	"NOx504.mean", "O342.mean",	"O3504.mean",	"Pamb0.mean",	
                "PAR.mean",	"PTG.mean", "RGlob.mean",	"RHIRGA42.mean",	"RHIRGA504.mean",	
                "RPAR.mean", "SO2168.mean",	"SWS.mean",	"T42.mean",	"T504.mean",	"UV_A.mean",	
                "CS.mean")]

npf$class2 <- factor("event",levels=c("nonevent","event"))
npf$class2[npf$class4=="nonevent"] <- "nonevent"
npf <- npf[,-1]