library(grid)
library(ggplot2)
library(readr)

# Exported from the file Fig2.Rnw
p1 <- readRDS("p1.rds")
p2 <- readRDS("p2.rds")

fs <- 8
p1$theme$strip.text$size <-fs - 1
p1$theme$axis.text.y$size <- fs - 1
p1$theme$axis.title.y$size <- fs - 1
p1$theme$axis.title.y$angle <- 90
p1$labels$y <- "Probability Estimate"
p1$theme$legend.position <- "top"
p1$theme$legend.text$size <- fs - 2
p1$theme$legend.title$size <- fs - 1
p1$theme$legend.key.size <- unit(4, "mm")

p2$theme$strip.text$size <- fs - 1 
p2$theme$axis.text.y$size <- fs - 2
p2$theme$axis.text.x$size <- fs - 1
p2$theme$axis.title.y$size <- fs 
p2$theme$axis.title.x$size <- fs 
p2$theme$panel.margin <- unit(0.004,"npc")
p2$labels$x <- "Difference in logit(p)"
p2$labels$y <- c()

png("Figure2.png", 14.5, 14.5, units = "cm", res = 300)
grid.newpage()
vp1 <- viewport(x=0.085, y=0.45, w=0.915, h=0.57, just=c("left","bottom"))
vp2 <- viewport(x= 0, y=0, w=0.955, h=0.47, just=c("left","bottom"))
#pushViewport(vp1)
print(p1, vp=vp1)
print(p2, vp=vp2)
grid.text("A", x = 0.03, y = 0.96, default.units = "npc", gp = gpar(fontsize = fs + 4))
grid.text("B", x = 0.03, y = 0.43, default.units = "npc", gp = gpar(fontsize = fs + 4))
dev.off()

