library(ggplot2)
library(reshape2)
library(plyr)

prefix <- "en"
name <- function(x) { paste(prefix,x,sep="_")}

pfeg <- read.table(name("pfeg-predict.csv"),header=F,sep="\t")
colnames(pfeg) <- c("itemID","prediction","target","pattern","hits","status")

writeLines("Confusion matrix")
print(dcast(pfeg,target ~ prediction,fun.aggregate=length,value.var="prediction"))

writeLines(paste("Accuracy:",length(pfeg[pfeg$status=="Correct",]$status) / length(pfeg$status)))
writeLines(paste("Coverage:",length(pfeg[pfeg$pattern!="Baseline",]$pattern) / length(pfeg$pattern)))

# 'correct' is the cumulative sum of the amount of predictions
correct <- cumsum(mapply(function(x) { if (x=="Correct") return(1) else return(0)},pfeg$status))
# 'pfeg$accuracy' is the cumulative accuracy score.
pfeg$accuracy <- mapply(function(x,y) return(x/y), correct, seq(1,length(correct)))

theme1 <- theme_bw() + theme(axis.text = element_text(size=14),text = element_text(size=26),legend.position="bottom")

# make cumulative accuracy score graph.
ggplot(pfeg,aes(itemID,accuracy)) + geom_point() + theme1 + labs(x="item count")
ggsave(filename=name("accuracy.png"))

# individual pattern accuracy scores
patternScores <- ddply(dcast(pfeg,pattern ~ status), .(Correct / (Correct + Incorrect)))
colnames(patternScores) <- c("Accuracy","pattern","Correct","Incorrect")
print(patternScores)

writeLines(paste("Mean accurate/inaccurate prediction hit counts:",
                 mean(pfeg[pfeg$status=="Correct",]$hits),"/",
                 mean(pfeg[pfeg$status=="Incorrect",]$hits)))

writeLines("Individual preposition precision scores:")
print(ddply(dcast(pfeg,target ~ status),.(Correct/(Correct + Incorrect)))[,c(1,2)])