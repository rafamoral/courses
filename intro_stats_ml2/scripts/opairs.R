suppressMessages(library(gclus))
suppressMessages(library(DendSer))

opairs <- function(dat, method = "pearson", ...) {
  dcorr <- abs(cor(dat, method = method))
  cols <- dmat.color(dcorr)
  ord <- dser(- as.dist(dcorr), method = "average")
  cpairs(dat,
         ord,
         panel.colors=cols,
         gap=.5,...
  )
}