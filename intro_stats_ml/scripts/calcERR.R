calcERR <- function(prob,
                    trueClass,
                    thresh = seq(0, 1, length.out = 100)) {
  m <- length(thresh)
  e <- vector(length = m)
  edef <- vector(length = m)
  enondef <- vector(length = m)
  
  for(i in 1:m) {
    pred <- factor(ifelse(prob < thresh[i], "No", "Yes"),
                   levels = c("No", "Yes"))
    tab <- table(trueClass, pred)
    e[i] <- (tab[1,2] + tab[2,1]) / sum(tab)
    edef[i] <- tab[2,1] / sum(tab[2,])
    enondef[i] <- tab[1,2] / sum(tab[1,])                      
  }
  return(tibble(thresh = thresh,
                error = e,
                type1 = edef,
                type2 = enondef))
}