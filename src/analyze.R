library(plyr)
library(reshape2)
library(ggplot2)

prefix <- "de"
name <- function(x) { paste(prefix,x,sep="_") }
filename <- name("pfeg-stats.csv")

pfeg <- read.csv(filename, header=F, sep="\t")
colnames(pfeg) <- c("itemID", "leftC", "target", "rightC", "pattern", "isCorrect", "candidate", "hits", "queryTime")

Coverage <- function(df) {
	df$anyhits <- 0
	df[ df$hits > 0, ]$anyhits <- 1
	cov <- dcast(df, pattern ~ ., function(x) { sum(x)/length(x) }, value.var = "anyhits")
	colnames(cov) <- c("pattern","coverage")
	return(cov[with(cov, order(pattern)),])
}

Accuracy <- function(df) {
	acc <- ddply(dcast(df[df$hits > 0,], pattern ~ isCorrect, length, value.var = "isCorrect"), .(Correct/(Correct + Incorrect)))
	colnames(acc) <- c("accuracy", "pattern", "Correct", "Incorrect")
	return(acc[with(acc, order(pattern)),])
}

# Compute accuracy and coverage per pattern
pfeg.stats <- Accuracy(pfeg)
pfeg.stats$coverage <- Coverage(pfeg)$coverage

# Compute precision for candidate predictions
targets <- dcast(dcast(pfeg, itemID + target ~ .)[,c(1,2)], target ~ .)
colnames(targets) <- c("target","count")
baseline = max(targets$count) / sum(targets$count)

# Accuracy/Coverage graph
acccov <- ggplot(pfeg.stats,aes(accuracy,coverage,label=pattern)) + geom_text(hjust=0,vjust=0,size=8) + geom_point() +
	labs(x="Accuracy", y="Coverage") + expand_limits(x=c(0.2,1),y=c(0.2,1)) +
	geom_smooth(method="lm",formula=y ~ poly(x,3,raw=T)) +
	annotate("segment", x=baseline,xend=baseline,y=0.1,yend=1,colour="red") +
	annotate("text", x=baseline+0.06,y=0.4,label="baseline", colour="red",size=10)

theme1 <- theme_bw() + theme(axis.text = element_text(size=14),text = element_text(size=26),legend.position="bottom")

png(name("acc-cov.png"), height=1000,width=1000)
print(acccov + theme1)
dev.off()

writeLines("Accuracy and coverage for patterns:\n============================")
print(pfeg.stats)
writeLines("Accuracy for prediction candidates:\n============================")
print(ddply(dcast(pfeg[pfeg$hits > 0,], candidate ~ isCorrect,value.var="hits"), .(Correct/(Correct + Incorrect))))

makeConvergence <- function(stepsize=100) {
	meanCoverage <- function(maxID) { mean(Coverage(pfeg[pfeg$itemID < maxID,])$coverage) }
	meanAccuracy <- function(maxID) { mean(Accuracy(pfeg[pfeg$itemID < maxID,])$accuracy) }

        items <- seq(stepsize,max(pfeg$itemID),stepsize)

        writeLines("Generating convergence statistic...")
        # dynamic programming my ass. Brute force all the way. Incidentally, this computation is slow.
	convg <- data.frame(mCov=mapply(meanCoverage,items),
                            mAcc=mapply(meanAccuracy,items),
                            trainingItems=items)

        return(convg)
}

printConvergenceGraphs <- function(convg,stepsize=100) {
        convg <- melt(convg,id.vars="trainingItems")
        cg <- ggplot(convg,aes(x=trainingItems,y=value,group=variable)) +
                geom_point(aes(colour=variable,shape=variable)) +
                labs(x="Number of items", y="Mean value") +
                scale_colour_discrete(name="Statistic: ",labels=c("Coverage","Accuracy")) +
                scale_shape(name="Statistic: ",labels=c("Coverage","Accuracy"))
        png(name("convergence.png"), height=700,width=700)
        print(cg + theme1)
        dev.off()
}

printConvergenceGraphs(makeConvergence())
