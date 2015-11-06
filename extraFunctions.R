aster <- function(x) {
  out <- rep("", length(x))
  out[which(x<0.1)] <- "."
  out[which(x<0.05)] <- "*"
  out[which(x<0.01)] <- "**"
  out[which(x<0.001)] <- "***"
  out
}

capwords <- function(s, strict = FALSE, sep = "_") {
  s <- gsub(sep, " ", s)
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

commas <- function(x){
  format(x, digits = 0, big.mark = ",")
}