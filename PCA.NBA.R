library(dplyr)
library(corrplot)

#read player data from web and convert name from factor to text
#nba = read.csv("/Temp/14-15RD.txt", sep = "", header = TRUE)
nba = read.csv("http://www.dougstats.com/14-15RD.txt", sep = "", header = TRUE)
nba$Player = as.character(nba$Player)

#use dplyr to group by position, sort my minutes played, and take top 20 per group
n = 20
topmins = data.frame(ungroup(slice( arrange(group_by(nba, PS), desc(Min)), 1:n)))
nba.numeric = dplyr::select(topmins, -c(GP, Player, Team, PS, Min, DQ, TC, EJ, FF, Sta, X..., PF))

#add a few shooting percent features, adjust for players who haven't shot any 3s...
nba.numeric$FGPCT = nba.numeric$FGM/nba.numeric$FGA
nba.numeric$FTPCT = nba.numeric$FTM/nba.numeric$FTA
nba.numeric$X3PCT = nba.numeric$X3M/nba.numeric$X3A
nba.numeric$X3PCT[is.nan(nba.numeric$X3PCT)] = 0

#attach player names as row names for plotting
rownames(nba.numeric) = topmins$Player

#correlation plot ordered by cluster
corrplot(cor(nba.numeric), order="hclust")

#compute PCA on nba data.  Note that feature centering and scaling is performed
pca = prcomp(nba.numeric, scale=TRUE, center=TRUE)

#print summary of PCA
summary(pca)

#biplot of PCA results
biplot(pca, cex=0.5)

#illustrate % of variance retained by each component
pctOfVar = (pca$sdev^2)/sum(pca$sdev^2) * 100
barplot(pctOfVar, main="% of Variance Retained by PC")

#compute correlation of source data with first two PCs
corFeatWithPCs = data.frame(PC1=cor(nba.numeric, pca$x[,1]), PC2=cor(nba.numeric, pca$x[,2]))