
perp <- function(probs, y=npf_test$y) {
  exp(-mean(log(ifelse(y==1, probs, 1-probs))))
}