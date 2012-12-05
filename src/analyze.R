library(plyr)
library(reshape2)
library(ggplot2)

prefix <- "de"
name <- function(x) { paste(prefix,x,sep="_") }
filename <- name("pfeg-stats.csv")

pfeg <- read.csv(filename, header=F, sep="\t")
colnames(pfeg) <- c("itemID", "leftC", "target", "rightC", "pattern", "isCorrect", "candidate", "hits", "queryTime")

pfeg.totalhits <- pfeg[,c("itemID", "pattern","hits","isCorrect")]
pfeg.totalhits$anyhits <- 0
pfeg.totalhits[ pfeg.totalhits$hits > 0, ]$anyhits <- 1

pfeg.recall <- dcast(pfeg.totalhits[,c("pattern","anyhits")], pattern ~ . ,
                     function(x) { sum(x)/length(x) }, value.var ="anyhits")
colnames(pfeg.recall) <- c("pattern","recall")

pfeg.anyhits <- pfeg.totalhits[pfeg.totalhits$hits > 0,]
pfeg.precision <- ddply(dcast(pfeg.anyhits, pattern ~ isCorrect, length, value.var = "isCorrect"), .(Correct/(Correct + Incorrect)))
colnames(pfeg.precision) <- c("precision", "pattern", "Correct" , "Incorrect")

pfeg.precision <- pfeg.precision[with(pfeg.precision, order(pattern)),]
pfeg.recall <- pfeg.recall[with(pfeg.recall, order(pattern)),]

pfeg.F <- pfeg.precision
pfeg.F$recall <- pfeg.recall$recall

fmeasure <- function(beta, precision, recall) { (1+beta^2) * (precision * recall / (beta^2 * precision + recall)) }
beta = 0.1
pfeg.F <- ddply(pfeg.F, .(fmeasure(beta,precision,recall)))
colnames(pfeg.F) <- c("F_", "precision", "pattern", "Correct", "Incorrect","recall")

id <- function (x) x

png(name("f-values.png"),height=1000,width=1000)
print(ggplot(pfeg.F,aes(reorder(pattern,F_,id),F_)) + geom_point(stat="identity"))
dev.off()

png(name("prec-rec.png"), height=1000,width=1000)
print(ggplot(pfeg.F,aes(precision,recall,label=pattern)) + geom_text(hjust=0,vjust=0) + geom_point())
dev.off()

writeLines("Precision and recall for patterns:\n============================")
print(pfeg.F)
writeLines("Precision for prediction candidates:\n============================")
print(ddply(dcast(pfeg[pfeg$hits > 0,], candidate ~ isCorrect,value.var="hits"), .(Correct/(Correct + Incorrect))))
