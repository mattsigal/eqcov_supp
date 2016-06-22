#' ---
#' title: "betadisper examples for EqCov paper"
#' author: "Michael Friendly"
#' date: "21 Jun 2016"
#' ---

#' This script reproduces all of the analysis and graphs for betadisper examples 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages and the data
#' NB: This example requires vegan >= 2.4-0
library(vegan)
data(iris)

#' Calculate distances and run `betadisper`
dst <- dist(iris[,1:4])
iris.bd <- betadisper(dst, iris$Species)
iris.bd
anova(iris.bd)
permutest(iris.bd)

labs <- paste("Dimension", 1:4, "(", 
              round(100*iris.bd$eig / sum(iris.bd$eig), 2), "%)")

plot(iris.bd, cex=1, pch=15:17,
     main="Iris data: MDS coordinates", cex.lab=1.25,
     xlab=labs[1], ylab=labs[2],
     hull=FALSE, ellipse=TRUE, conf=0.68, lwd=2)


#' Boxplot of dispersions
boxplot(iris.bd, xlab="Species", notch=TRUE, col=c("gray", "red", "green"))

#' ## Skulls data
#' 
data(Skulls, package="heplots")
dst <- dist(Skulls[, -1])
skulls.bd <- betadisper(dst, Skulls$epoch)
skulls.bd
anova(skulls.bd)
permutest(skulls.bd)

plot(skulls.bd, main="Skulls data: MDS coordinates", 
     hull=FALSE, ellipse=TRUE, conf=0.68)
boxplot(skulls.bd, notch=TRUE, col="lightblue", xlab="Epoch")



#' ## Wine data
data(Wine, package="candisc")
dst <- dist(Wine[, -1])
wine.bd <- betadisper(dst, Wine$Cultivar)
wine.bd
anova(wine.bd)
permutest(wine.bd)

#plot(wine.bd, hull=FALSE, ellipse=TRUE, conf=0.68)
boxplot(wine.bd, xlab="Cultivar", notch=TRUE, col=c("red", "green", "blue"))


