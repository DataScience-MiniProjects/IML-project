
# Code to produce the answer-csv
# The first column "class4" in the answers.csv file is our 
# prediction for the day, where class4 is Ia, Ib, II, or nonevent.
# The second column "p" is our prediction for probability Pr(class2=event)


# Creates the csv and adds the first line
# Change the string here to our guess of the accuracy
write.table(0.75,
             file="./answers.csv",
             append = F,
             sep=',',
             row.names=F,
             col.names=F)

# Write column names to the file
write.table(data.frame("class4","p"),
            file="./answers.csv",
            append = T,
            sep=',',
            row.names=F,
            col.names=F)

# testing testing
#setwd()
#probs_test4 <- data.frame(c(0.1,0.2,0.5,0.1,0.5),c(0.6,0.05,0.1,0.2,0.05),c(0.1,0.7,0.2,0.2,0.15),c(0.2,0.05,0.2,0.5,0.3))
#colnames(probs_test4)<- c("Ia","Ib","II","nonevent")

# Assume the class probabilities for each row are in probs_test4
classes_test4 <- colnames(probs_test4)[max.col(probs_test4, ties.method = "first")]

# Write the class predictions and probabilities
write.table(data.frame(classes_test4, (probs_test4$Ia+probs_test4$Ib+probs_test4$II)),
            file="./answers.csv",
            append = T,
            sep=',',
            row.names=F,
            col.names=F)


