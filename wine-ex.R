#' ---
#' title: "Wine data examples for EqCov paper"
#' author: "Michael Friendly and Matthew Sigal"
#' date: "21 Jun 2016"
#' ---


#' This script reproduces all of the analysis and graphs for the MANOVA of the `Wine` data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages and the data
library(heplots)
library(car)
library(candisc)
data(Wine, package="candisc")

#' ## Fit the MANOVA model
Wine.mod <- lm(as.matrix(Wine[, -1]) ~ Cultivar, data=Wine)
Anova(Wine.mod)

#' ## HE plots
heplot(Wine.mod, fill=TRUE, fill.alpha=.1)
#pairs(Wine.mod, fill=TRUE, fill.alpha=.1)
pairs(Wine.mod, fill=TRUE, fill.alpha=.1, variables=1:5)


#' ## univariate F tests

#' There is no simple way to get a compact summary of the univariate F-tests from a MLM.
#' This code extracts the result of `summary()` and makes them into a data frame.
#' 
ss <- summary(Wine.mod)
UniStats <- as.data.frame(matrix(0, nrow=length(ss), 5))
for (i in 1:length(ss)) {
	UniStats[i,1] <- ss[[i]]$r.squared
	f <- ss[[i]]$fstatistic
	UniStats[i,2:4] <- f
	UniStats[i,5] <- pf(f[1], f[2], f[3], lower.tail=FALSE)
}

rownames(UniStats) <- sub("Response ", "", names(ss))
UniStats$stars <- c(gtools:::stars.pval(UniStats[,5]))
UniStats[,5] <- format.pval(UniStats[,5], eps=0.001)
colnames(UniStats) <- c("R^2", "F", "df1", "df2", "Pr (>F)", "")
UniStats


#' ## view in canonical space

Wine.can <- candisc(Wine.mod)
Wine.can
plot(Wine.can, ellipse=TRUE, var.lwd=2)

heplot(Wine.can, var.lwd=2)

# need to use extremely small p-value to preserve resolution
heplot(Wine.can, var.lwd=2, alpha=0.00001, fill=TRUE, fill.alpha=.1, var.col="black")


#' ## effect ordered pairs.mlm plot

#' The HE pairs plot is confusing with so many responses, but can be simplified by
#' rearranging the variables in the order derived from the canonical discriminant plot.
struc <- Wine.can$structure
angles <- ifelse( struc[,1] > 0, 
                  atan(struc[,2]/struc[,1]), 
                  atan(struc[,2]/struc[,1]) + pi)
#angles[order(angles)]
colnames(Wine[,-1])[order(angles)]

#' This computation is now done by the function `varOrder()` in the candisc package.
(ord <- varOrder(Wine.mod))
varOrder(Wine.mod, names=TRUE)

ord <- varOrder(Wine.mod)
pairs(Wine.mod, variables=ord, fill=TRUE, fill.alpha=.1, var.cex=1.5)

#' ## Box's M test
wine.boxm <- boxM(Wine.mod)
wine.boxm

#' More details are given by the `summary` method
summary(wine.boxm)

plot(wine.boxm, bias.adj=FALSE)

#' ## Examine eigenvalue distributions
eigs <- summary(wine.boxm, quiet=TRUE)$eigs

#' large eigenvalues don't differ that much; small one do!
op <- par(mfrow = c(1,2), mar=c(5,4,1,1)+.1)
matplot(1:6, log(eigs)[1:6,], type='b', 
	lwd=c(rep(1,3), 3), pch=c(1:3, 'p'),
	lty=c(rep(3,3), 1), 
	cex=1.2, cex.lab=1.25,
	xlab="Dimension", ylab="log Eigenvalue")
matplot(7:13, log(eigs)[7:13,], type='b', 
	lwd=c(rep(1,3), 3), pch=c(1:3, 'p'),
	lty=c(rep(3,3), 1), 
	cex=1.2, cex.lab=1.25,
	xlab="Dimension", ylab="log Eigenvalue")
par(op)

#' ## Plot other eigenvalue measures

 op <- par(mfrow=c(2,2), mar=c(5,4,1,1))
 plot(wine.boxm, which="product", gplabel="Cultivar")
 plot(wine.boxm, which="sum", gplabel="Cultivar")
 plot(wine.boxm, which="precision", gplabel="Cultivar")
 plot(wine.boxm, which="max", gplabel="Cultivar")
 par(op)

#' ## univariate Bartlett tests

bartlettTests(Wine[,-1], Wine$Cultivar)

#' ## univariate Levene tests

leveneTests(Wine[,-1], Wine$Cultivar)


#' ## multivariate analog of Levene's test

winedev <- abs( colDevs(Wine[,-1], Wine$Cultivar, median) )
winedev <- data.frame( winedev, Cultivar=Wine$Cultivar)
winedev.mod <-lm( as.matrix(winedev[,1:13]) ~ Cultivar, data=winedev)
Anova(winedev.mod)

# variables that do not show any effects
pairs(winedev.mod, variables=1:5, fill=TRUE, fill.alpha=.1)
# variables that do show effects
pairs(winedev.mod, variables=c(7,10:12), fill=TRUE, fill.alpha=.1)

#' ## Canonical view
winedev.can <- candisc(winedev.mod)
winedev.can

plot(winedev.can, which=1)
plot(winedev.can, ellipse=TRUE)





