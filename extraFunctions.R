aster <- function(x) {
  out <- rep("", length(x))
  out[which(x<0.1)] <- "."
  out[which(x<0.05)] <- "*"
  out[which(x<0.01)] <- "**"
  out[which(x<0.001)] <- "***"
  out
}