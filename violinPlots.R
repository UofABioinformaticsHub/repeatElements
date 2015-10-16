library(ggplot2)
library(MASS)
library(matrixStats)
library(vioplot)

violData <- read.delim("vioplot_data")
head(violData)

## Plot gene length vs FPKM
plot(violData$gene_length, violData$adipose, cex=0.8, log="x", xlab="Gene Length", ylab="log2(FPKM)")
title(main="Adipose Tissue: FPKM Vs Length")
mtext("Length is plotted on the log scale", font=3)
## There are clearly two populations here. The main Expressed genes in the top cluster & the non expressed

# Firstly, check if there is a relationship between gene length & the repeat elements
m1 <- glm.nb(gene_length~(L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs)^3, data=violData)
m2 <- update(m1, .~. - L1_utr5_IDs:alu_utr5_IDs:LTR_utr5_IDs)
m3 <- update(m2, .~. - L1_utr5_IDs:alu_utr5_IDs - alu_utr5_IDs:LTR_utr5_IDs - L1_utr5_IDs:LTR_utr5_IDs)
m4 <- glm(gene_length~(L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs)^3, family="poisson", data=violData)
pchisq(2 * (logLik(m1) - logLik(m4)), df = 1, lower.tail = FALSE)
# Looks like m1 is the go
nArg <- c("\n\nNone", 
          "\n\nL1_utr5_IDs", "\n\nalu_utr5_IDs", "\nL1_utr5_IDs:\nalu_utr5_IDs", "\n\nLTR_utr5_IDs", 
          "\nL1_utr5_IDs:\nLTR_utr5_IDs", "\nalu_utr5_IDs:\nLTR_utr5_IDs", "L1_utr5_IDs:\nalu_utr5_IDs:\nLTR_utr5_IDs")
boxplot(gene_length~(L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs)^3, data=violData, log="y", ylab="Gene Length", xlab="RE structure", names=nArg)
# So clearly having more than one repeat element present increases the length of the gene


# Just look at the adipose tissue as a test for the methods.
# Initially just check the genes with no REs vs those with any RE
adipose <- data.frame(FPKM=violData$adipose_expression, length=violData$gene_length)
rpts <- rep("N", nrow(adipose))
rpts[which(rowSums(violData[,c("L1_utr5_IDs","alu_utr5_IDs", "LTR_utr5_IDs")])!=0)] <- "Y"
adipose <- data.frame(adipose, RE=rpts)

## Have a look at the raw data, separated on whether there is a RE in the gene
ggplot(adipose, aes(x=RE, y=FPKM)) + 
  geom_violin(fill="darkred") +
  geom_boxplot(width=0.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=5) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background=element_rect("white"))

# Cluster into DE & non-DE using k-means
adKmeans <- kmeans(cbind(log(adipose$length), adipose$FPKM), 2)
ex <- which(adKmeans$centers[,2] == max(adKmeans$centers[,2])) # The highest expressed cluster
expG <- rep(0, nrow(adipose))
plot(adipose$length, adipose$FPKM, cex=0.8, log="x", xlab="Gene Length", ylab="log2(FPKM)", col="grey")
expG[which(adKmeans$cluster==ex)] <- 1
points(adipose$length[which(adKmeans$cluster==ex)], adipose$FPKM[which(adKmeans$cluster==ex)], cex=0.8, col="black")
title(main="Adipose Tissue: FPKM Vs Length after K-means clustering")
mtext("Length is plotted on the log scale", font=3)

# Add the cluster info & the detailed Repeat Element from the info
adipose <- data.frame(adipose, Exp=expG, violData[,c("L1_utr5_IDs","alu_utr5_IDs", "LTR_utr5_IDs")])
# See if having a repeat element affected the chance of being called notDE
summary(glm(Exp~L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs, family=binomial(link = "logit"), data=adipose))
# So it looks like having an LTR decreases your chance of being called expressed after k-means,
# whereas an alu increases your chances of being called expressed

## The confounding between L1 & alu is a problem, do a series of Fisher tests
fisher.test(rbind(table(subset(adipose, Exp==0)$L1_utr5_IDs), table(subset(adipose, Exp==1)$L1_utr5_IDs)))
fisher.test(rbind(table(subset(adipose, Exp==0)$alu_utr5_IDs), table(subset(adipose, Exp==1)$alu_utr5_IDs)))
fisher.test(rbind(table(subset(adipose, Exp==0)$LTR_utr5_IDs), table(subset(adipose, Exp==1)$LTR_utr5_IDs)))


## Now explore the effect on actual expression levels. No interactions are expected, so don't include them
adipose.lm <- lm(FPKM~log(length)+L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs, subset=Exp==1, data=adipose)
par(mfrow=c(2,2))
plot(adipose.lm)
summary(adipose.lm)
# Likewise, the L1s have no impact on expression, whereas the alu & LTRs are both showing reduced expression
# There is a problem with confounding here, as 95% of the L1s also have an alu, whereas only 69% have an LTR.
# Only 55% of alus have an LTR as well, so it's just the confounding between L1 & alu...

# To do a violinplot, we need an extra column for "None"
adipose <- data.frame(adipose, None=0)
adipose$None[which(rowMaxs(as.matrix(adipose[,c("L1_utr5_IDs", "alu_utr5_IDs", "LTR_utr5_IDs")]))==0)] <- 1

vioplot(adipose$FPKM[which(adipose$None==1)], 
        adipose$FPKM[which(adipose$L1_utr5_IDs==1)], 
        adipose$FPKM[which(adipose$alu_utr5_IDs==1)], 
        adipose$FPKM[which(adipose$LTR_utr5_IDs==1)],
        col="darkred", names=c("None", "L1_utr5_IDs", "alu_utr5_IDs", "LTR_utr5_IDs"))
# They are inaccurate as those tails are completely artificial

# Can they be done using ggplot2
## Have a look at the raw data, separated on whether there is a RE in the gene
#ggplot(adipose, aes(x=RE, y=FPKM)) + 
  #geom_violin(fill="darkred") +
  #geom_boxplot(width=0.1, fill="black", outlier.colour=NA) +
  #stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=5) +
  #scale_y_continuous(expand=c(0,0)) +
  #theme(panel.background=element_rect("white"))

###################################
###################################

## Due to the serious confounding, maybe a set of paired regressions may be a better option.
## Obviously the results for L1 & alu repeats should be very similar
adipose.L1.lm <- lm(FPKM~log(length)+L1_utr5_IDs, data=adipose, subset=Exp==1)
par(mfrow=c(2,2))
plot(adipose.L1.lm)
summary(adipose.L1.lm)

## alu
adipose.alu.lm <- lm(FPKM~log(length)+alu_utr5_IDs, data=adipose, subset=Exp==1)
par(mfrow=c(2,2))
plot(adipose.alu.lm)
summary(adipose.alu.lm)

## LTR
adipose.LTR.lm <- lm(FPKM~log(length)+LTR_utr5_IDs, data=adipose, subset=Exp==1)
par(mfrow=c(2,2))
plot(adipose.LTR.lm)
summary(adipose.LTR.lm)


##################
## BRAIN TISSUE ##
##################
# Just look at the brain tissue as a test for the methods.
# Initially just check the genes with no REs vs those with any RE
brain <- data.frame(FPKM=violData$brain_expression, length=violData$gene_length)
rpts <- rep("N", nrow(brain))
rpts[which(rowSums(violData[,c("L1_utr5_IDs","alu_utr5_IDs", "LTR_utr5_IDs")])!=0)] <- "Y"
brain <- data.frame(brain, RE=rpts)

## Have a look at the raw data, separated on whether there is a RE in the gene
ggplot(brain, aes(x=RE, y=FPKM)) + 
  geom_violin(fill="darkred") +
  geom_boxplot(width=0.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=5) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background=element_rect("white"))

# Cluster into DE & non-DE using k-means
brnKmeans <- kmeans(cbind(log(brain$length), brain$FPKM), 2)
ex <- which(brnKmeans$centers[,2] == max(brnKmeans$centers[,2])) # The highest expressed cluster
expG <- rep(0, nrow(brain))
plot(brain$length, brain$FPKM, cex=0.8, log="x", xlab="Gene Length", ylab="log2(FPKM)", col="grey")
expG[which(brnKmeans$cluster==ex)] <- 1
points(brain$length[which(brnKmeans$cluster==ex)], brain$FPKM[which(brnKmeans$cluster==ex)], cex=0.8, col="black")
title(main="brain Tissue: FPKM Vs Length after K-means clustering")
mtext("Length is plotted on the log scale", font=3)

# Add the cluster info & the detailed Repeat Element from the info
brain <- data.frame(brain, Exp=expG, violData[,c("L1_utr5_IDs","alu_utr5_IDs", "LTR_utr5_IDs")])
# See if having a repeat element affected the chance of being called notDE
summary(glm(Exp~L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs, family=binomial(link = "logit"), data=brain))
# So it looks like having an LTR decreases your chance of being called expressed after k-means,
# whereas the other have no impact

## Now explore the effect on actual expression levels. No interactions are expected, so don't include them
brain.lm <- lm(FPKM~log(length)+L1_utr5_IDs+alu_utr5_IDs+LTR_utr5_IDs, subset=Exp==1, data=brain)
par(mfrow=c(2,2))
plot(brain.lm)
summary(brain.lm)
# Likewise, the L1s have no impact on expression, whereas the alu & LTRs are both showing reduced expression

# To do a violinplot, we need an extra column for "None"
brain <- data.frame(brain, None=0)
brain$None[which(rowMaxs(as.matrix(brain[,c("L1_utr5_IDs", "alu_utr5_IDs", "LTR_utr5_IDs")]))==0)] <- 1

vioplot(brain$FPKM[which(brain$None==1)], 
        brain$FPKM[which(brain$L1_utr5_IDs==1)], 
        brain$FPKM[which(brain$alu_utr5_IDs==1)], 
        brain$FPKM[which(brain$LTR_utr5_IDs==1)],
        col="darkred", names=c("None", "L1_utr5_IDs", "alu_utr5_IDs", "LTR_utr5_IDs"))
# They are inaccurate as those tails are completely artificial