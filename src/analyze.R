library(plyr)
library(reshape2)
library(ggplot2)

filename <- "pfeg-stats.csv"

pfeg <- read.csv(filename, header=F, sep="\t")
colnames(pfeg) <- c("itemID", "leftC", "target", "rightC", "query", "pattern", "candidate", "hits", "seconds", "correct")

pfeg.totalhits <- dcast(pfeg[,c("itemID", "pattern","hits","correct")], itemID + pattern + correct ~ . , sum, value.var = "hits")
colnames(pfeg.totalhits) <- c("itemID","pattern","correct","hitstotal")
pfeg.totalhits$anyhits <- 0
pfeg.totalhits[ pfeg.totalhits$hitstotal > 0, ]$anyhits <- 1

pfeg.recall <- dcast(pfeg.totalhits[,c("pattern","anyhits")], pattern ~ . ,
                     function(x) { sum(x)/length(x) }, value.var ="anyhits")
colnames(pfeg.recall) <- c("pattern","recall")

pfeg.anyhits <- pfeg.totalhits[pfeg.totalhits$hitstotal > 0,]
pfeg.precision <- ddply(dcast(pfeg.anyhits, pattern ~ correct, length, value.var = "correct"), .(Correct/(Correct + Incorrect)))
colnames(pfeg.precision) <- c("precision", "pattern", "Correct" , "Incorrect")

pfeg.precision <- pfeg.precision[with(pfeg.precision, order(pattern)),]
pfeg.recall <- pfeg.recall[with(pfeg.recall, order(pattern)),]

pfeg.F <- pfeg.precision
pfeg.F$recall <- pfeg.recall$recall

fmeasure <- function(beta, precision, recall) { (1+beta^2) * (precision * recall / (beta^2 * precision + recall)) }
beta = 1
pfeg.F <- ddply(pfeg.F, .(fmeasure(beta,precision,recall)))
colnames(pfeg.F) <- c("F_1", "precision", "pattern", "Correct", "Incorrect","recall")

id <- function (x) x

png("f-values.png",height=1000,width=1000)
print(ggplot(pfeg.F,aes(reorder(pattern,F_1,id),F_1)) + geom_point(stat="identity"))
dev.off()

png("prec-rec.png", height=1000,width=1000)
print(ggplot(pfeg.F,aes(precision,recall,label=pattern)) + geom_text(hjust=0,vjust=0) + geom_point())
dev.off()
