#
# ----------------------------------- TIME AND ENGAGEMENT
# code by Eugene Ch'ng (www.complexity.io)
# http://www.github.com/drecuk
#
# The code is linked to the two articles below:
# Ch’ng E., Cai S., F.T. Leow, Zhang T. (forthcoming) The Adoption and Use of Emerging Cultural Technologies in China’s Museums, Journal of Cultural Heritage
# Ch’ng E., Cai S., F.T. Leow, Zhang T. (forthcoming) Datasets on the evaluation of the adoption and use of digital technologies in China Museums, Data in Brief
#

rm(list=ls()) #will remove ALL objects

#------------------------------------ LOAD FILE
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

d= read.table("workingData.csv", header=TRUE, skip=0, sep=",")
d
names(d)
summary(d)
sapply(d)
dev.new(width=10.25,height=10.25,units="in",res=1200, bg="white")

m <- matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE)
#       [,1] [,2] [,3] [,4]
# [1,]   1    1     2    2
# [2,]   0    3     3    0

nf <- layout(m)
layout.show(nf)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))
par(mgp=c(1.2,0.4,0))	 # label and axis spaces c(label, axes, title)	

par(mar=c(5,5,2,3))

#  --------------- BOXPLOT - compares time at exhibit and exhibit length
boxplot(d$ExhibitLength, d$TimeAtExhibit, ylab="Time (minutes)", xlab="")
axis(1, at=seq(1, 2, by=1), labels=c("Length", "Actual")) 
title(main="Exhibit Length and Actual Visitor Access Time", cex.main=1)

cat("------------------------ BoxPlot - Exhibit Length and time at exhibit")
boxplot(d$TimeAtExhibit~d$ExhibitLength)
title(main="Visitor Access Time", sub="", ylab="Actual Visitor Time Spent (minutes)", xlab="Length of Exhibit (minutes)", cex.main=1)

cat("------------------------ BoxPlot - engagement and time at exhibit")
boxplot(d$Engagement~d$TimeAtExhibit, col="lightgrey")
title(main="Observed Visitor Engagement \n(Various Length of Time Spent on Exhibit)", sub="", xlab="Time at Exhibit", ylab="Level of Engagement", cex.main=1)

dev.print(png,'03-TimeEngagement.png', width=10.25,height=10.25, units="in",res=1200, bg="white") 
dev.off()
###### END 2ND SET

