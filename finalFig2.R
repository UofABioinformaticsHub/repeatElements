library(grid)
library(ggplot2)
library(readr)

# Exported from the file Fig2.Rnw
p1 <- readRDS("p1.rds")
p2 <- readRDS("p2.rds")

fs <- 11
p1$theme$strip.text$size <-fs +1
p1$theme$axis.text.y$size <- fs
p1$theme$axis.title.y$size <- fs + 1
p1$theme$axis.title.y$angle <- 90
p1$labels$y <- "Probability Estimate"
p1$theme$legend.position <- "top"
p1$theme$legend.text$size <- fs
p1$theme$legend.title$size <- fs

p2$theme$strip.text$size <- fs + 1
p2$theme$axis.text.y$size <- fs - 1
p2$theme$axis.text.x$size <- fs
p2$theme$axis.title.y$size <- fs + 1
p2$theme$axis.title.x$size <- fs + 1
p2$labels$x <- "Difference in logit(p)"
p2$labels$y <- c()

pdf("Figure2.pdf", 10, 10)
grid.newpage()
vp1 <- viewport(x=0.095, y=0.48, w=0.905, h=0.52, just=c("left","bottom"))
vp2 <- viewport(x= 0, y=0, w=0.97, h=0.48, just=c("left","bottom"))
#pushViewport(vp1)
print(p1, vp=vp1)
print(p2, vp=vp2)
grid.text("A", x = 0.03, y = 0.96, default.units = "npc", gp = gpar(fontsize = fs + 4))
grid.text("B", x = 0.03, y = 0.46, default.units = "npc", gp = gpar(fontsize = fs + 4))
dev.off()

