#' ---
#' title: "Skulls data examples for EqCov paper"
#' author: "Michael Friendly"
#' date: "12 Apr 2016"
#' ---


#' This script reproduces all of the analysis and graphs for the MANOVA of the `Skulls` data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages and the data
library(heplots)
library(car)
library(candisc)

data(Skulls, package="heplots")
# make shorter labels for epochs
Skulls$epoch <- ordered(Skulls$epoch, labels=sub("c","",levels(Skulls$epoch)))
# make longervariable labels
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")

#' ##  fit manova model
skulls.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=Skulls)

Manova(skulls.mod)

# test trends over epochs
linearHypothesis(skulls.mod, "epoch.L") # linear component
#linearHypothesis(skulls.mod, "epoch.Q") # quadratic component
# test all nonlinear terms
print(linearHypothesis(skulls.mod, c("epoch.Q", "epoch.C", "epoch^4")), SSP=FALSE)

heplot(skulls.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), 
       xlab=vlab[1], ylab=vlab[2])

heplot(skulls.mod, 
	hypotheses=list(Lin="epoch.L", NonLin=c("epoch.Q", "epoch.C", "epoch^4") ), 
	xlab=vlab[1], ylab=vlab[2])

pairs(skulls.mod, var.labels=vlab, hypotheses=list(Lin="epoch.L"))


#' ## Visualizing covariance matrices

covEllipses(Skulls[,-1], Skulls$epoch, 
            fill=c(rep(FALSE, 5), TRUE), label.pos=1:4,
            xlab=vlab[1], ylab=vlab[2])

covEllipses(Skulls[,-1], Skulls$epoch, 
            fill=c(rep(FALSE, 5), TRUE), label.pos=1:4,
            xlab=vlab[1], ylab=vlab[2], center=TRUE)

		
#' All pairwise plots
covEllipses(Skulls[,-1], Skulls$epoch, variables=1:4, vlabels=vlab,
            fill=c(rep(FALSE, 5), TRUE), label.pos=1:4,
            center=TRUE, fill.alpha=.1)

covEllipses(Skulls[,-1], Skulls$epoch, variables=1:4, vlabels=vlab,
            fill=c(rep(FALSE, 5), TRUE), label.pos=1:4)

#' ## Box's M test

skulls.boxm <- boxM(skulls.mod)
skulls.boxm
summary(skulls.boxm)

plot(skulls.boxm, gplabel="Epoch")


#' ## plot other eigenvalue measures

skulls.stats <- summary(skulls.boxm, quiet=TRUE)
eigs <- skulls.stats$eigs
eigstats <- skulls.stats$eigstats

matplot(1:4, log(skulls.stats$eigs), type="b",
	lwd=c(rep(1,5), 3), pch=c(1:5, 'p'),
	lty=c(rep(5,5), 1),
	xlab = "Dimension", ylab="log Eigenvalue")


 op <- par(mfrow=c(2,2), mar=c(5,4,1,1))
 plot(skulls.boxm, which="product", gplabel="Epoch", xlim=c(10,14))
 plot(skulls.boxm, which="sum", gplabel="Epoch")
 plot(skulls.boxm, which="precision", gplabel="Epoch")
 plot(skulls.boxm, which="max", gplabel="Epoch")
 par(op)
