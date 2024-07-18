screeplot <- function(p) {
  e <- p$sdev ^ 2
  e <- e / sum(e)
  plot(
    1:length(e),
    e,
    xlab = "Component number",
    pch = 20,
    ylab = "Variance proportion",
    main = "Scree plot",
    axes = F,
    ylim = c(0, max(e)*1.04)
  )
  lines(1:length(e), e)
  axis(1, at = 1:length(e))
  axis(2)
}