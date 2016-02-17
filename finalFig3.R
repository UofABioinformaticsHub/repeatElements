library(grid)
library(ggplot2)
library(readr)

# Exported from the file Fig3.Rnw
p3a <- readRDS("p3a.rds")
p3b <- readRDS("p3b.rds")

fs <- 8
p3a$theme$strip.text$size <-fs - 1
p3a$theme$axis.text.y$size <- fs - 1
p3a$theme$axis.title.y$size <- fs - 1
p3a$theme$legend.text$size <- fs - 2
p3a$theme$legend.title$size <- fs - 1
p3a$theme$legend.key.size <- unit(4, "mm")
 
p3b$theme$strip.text$size <- fs - 1 
p3b$theme$axis.text.y$size <- fs - 2
p3b$theme$axis.text.x$size <- fs - 1
p3b$theme$axis.title.y$size <- fs 
p3b$theme$axis.title.x$size <- fs 
p3b$theme$panel.margin <- unit(0.004,"npc")

png("Figure3.png", 14.5, 14.5, units = "cm", res = 300)
grid.newpage()
vp1 <- viewport(x=0.085, y=0.55, w=0.915, h=0.48, just=c("left","bottom"))
vp2 <- viewport(x= 0, y=0, w=1, h=0.57, just=c("left","bottom"))
print(p3b, vp=vp2)
print(p3a, vp=vp1)
grid.text("A", x = 0.02, y = 0.96, default.units = "npc", gp = gpar(fontsize = fs + 4))
grid.text("B", x = 0.02, y = 0.54, default.units = "npc", gp = gpar(fontsize = fs + 4))
dev.off()

