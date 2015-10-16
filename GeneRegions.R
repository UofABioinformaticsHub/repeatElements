###################################################################################################
###################################################################################################

fig2 <- read.csv("Figure2.csv", header=FALSE, stringsAsFactor=FALSE)
colnames(fig2) <- c("Region", "RE", "Tissue", "bpRETE", "bpRE")
fig2$Region <- factor(fig2$Region, levels=c("5'UTR", "Start", "CDS-exon", "CDS-intron", "Stop", "3'UTR", "Intergenic"))
fig2$RE <- factor(fig2$RE, levels=c("Active_promoter","Weak_promoter", "Strong_enhancer", "Weak_enhancer","Repressor", "Insulator"))
levels(fig2$RE)[1:4] <- c("Active Promoter","Weak Promoter", "Strong Enhancer", "Weak Enhancer")
fig2$Tissue <- as.factor(fig2$Tissue)
fig2[,4:5] <- fig2[,4:5]/100 # Convert back to proportions
boxplot(bpRETE~RE, data=fig2, log="y", subset=Region=="5'UTR", las=2)

## Maybe the smartest thing to do would be to ask?
## 1 - Are the REs enriched specifically in any regions
## 2 - Are the RETE's enriched specifically within any regions

## Collect the mean & sd for each combination of 
n <- length(levels(fig2$RE))*length(levels(fig2$Region))
fig2stats <- data.frame(Region = factor(rep(levels(fig2$Region), each=length(levels(fig2$RE))), levels=levels(fig2$Region)),
                       RE = factor(rep(levels(fig2$RE), times=length(levels(fig2$Region))), levels=levels(fig2$RE)),
                       Class = rep(c("BG", "RETE"), each=n))
temp <- matrix(0, nrow=n*2, ncol=3, dimnames=list(c(),c("mean","lower","upper")))
for (i in 1:n){
  x <- logit(fig2$bpRE[intersect(which(fig2$Region==fig2stats$Region[i]), which(fig2$RE==fig2stats$RE[i]))])
  temp[i, 1] <- inv.logit(mean(x))
  temp[i, 2] <- inv.logit(mean(x)-sd(x))
  temp[i, 3] <- inv.logit(mean(x)+sd(x))
  y <- logit(fig2$bpRETE[intersect(which(fig2$Region==fig2stats$Region[i]), which(fig2$RE==fig2stats$RE[i]))])
  temp[i + n, 1] <- inv.logit(mean(y))
  temp[i + n, 2] <- inv.logit(mean(y)-sd(y))
  temp[i + n, 3] <- inv.logit(mean(y)+sd(y))
}
fig2stats <- data.frame(fig2stats, 100*temp)

## Change the lebelling for better reading on facets
levels(fig2stats$Class)[levels(fig2stats$Class)=="BG"] <- "RE in Genomic Region"
levels(fig2stats$Class)[levels(fig2stats$Class)=="RETE"] <- "TE in RE"


## Instead of looking at the data in facets for each region, look using REs instead
y_ranges <- data.frame(Region=rep("5'UTR",2), RE=rep("Active Promoter", 2), Class=c("RE in Genomic Region","TE in RE"), mean=c(26, 3.7))
ggplot(fig2stats, aes(x=Region, y=mean, fill=Region)) +
  facet_grid(Class ~ RE, scales="free_y") +
  geom_bar(stat = "identity", position="dodge") + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  ylab("Percentage of Base Pairs") + 
  xlab("") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_y_continuous(expand=c(0,0)) + 
  geom_blank(data=y_ranges, aes(x=Region, y=mean))
ggsave("Fig2a.pdf", height=150, width=250, units="mm")

##############
## Analysis ##
##############

# Run a series of pairwise comparisons


