#
# ----------------------------------- Quality of Digital Systems
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

d=read.table("workingData.csv", header=TRUE, skip=0, sep=",")


dev.new(width=10.25,height=5.25,units="in",res=1200, bg="white")

#  ---------------------- group size (NumberOfPeople) and discussions ********
data <- c(length(d$NumberOfPeople[d$NumberOfPeople == 1]), 
          length(d$NumberOfPeople[d$NumberOfPeople == 2]),
          length(d$NumberOfPeople[d$NumberOfPeople == 3]),
          length(d$NumberOfPeople[d$NumberOfPeople == 4]),
          length(d$NumberOfPeople[d$NumberOfPeople == 5]),
          length(d$NumberOfPeople[d$NumberOfPeople == 6]),
          length(d$NumberOfPeople[d$NumberOfPeople == 7]),
          length(d$NumberOfPeople[d$NumberOfPeople == 8]),
          length(d$NumberOfPeople[d$NumberOfPeople == 9]),
          length(d$NumberOfPeople[d$NumberOfPeople == 10]),
          length(d$NumberOfPeople[d$NumberOfPeople == 11]),
          length(d$NumberOfPeople[d$NumberOfPeople == 12]),
          length(d$NumberOfPeople[d$NumberOfPeople == 13]),
          length(d$NumberOfPeople[d$NumberOfPeople == 14]),
          length(d$NumberOfPeople[d$NumberOfPeople == 15])
          )
plot(data, xaxt='n', xlab="", ylab="")
# 
# bins <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
# barplot(d$NumberOfPeople)
# 
# hist(d$NumberOfPeople, breaks=seq(1,15,by=1),
#      col="orange",main="Histogram",
#      xlab="x",ylab="f(x)",yaxs="i",xaxs="i")

# hist.data = hist(d$NumberOfPeople, plot=F)
# # hist.data$counts = log(hist.data$counts, 2)
# plot(hist.data$counts[hist.data$counts != 'NaN'], xaxt='n', xlab="", ylab="")
axis(1, at=c(1:15), label=c(1:15), las=0)
title(main="Observed User Group Sizes (1 to 15 in a group)", sub="", xlab="Group Size", ylab="Population", cex.main=0.8)
x = c(1:15)
y = data
smoothingSpline = smooth.spline(x, y, spar=0.05)
text(x, y, round(y, 2), cex=0.8, pos=4, offset=0.8, adj=0.2)
lines(smoothingSpline)
points(data,pch=20, col='red')

# lo <- loess(y~x)
# xl <- seq(min(x),max(x), (max(x) - min(x))/100)
# lines(xl, predict(lo,xl), col='red', lwd=1)

dev.print(png,'00-GroupSize.png', width=8.25,height=5.25, units="in",res=1200, bg="white") 
dev.off()


#  ---------------------- Test plots and binning for log plots
# max(y)
# buckets <- c(1:15)
# d$NumberOfPeople[d$NumberOfPeople == 0] <- NA
# dhist <- hist(d$NumberOfPeople, breaks=buckets, plot="FALSE")
# names(dhist)
# bp <- barplot(dhist$counts, log="y", col="white", names.arg=buckets)
# text(bp, mydata_hist$counts, labels=mydata_hist$counts, pos=1)
# 
# 
# plot(d$NumberOfPeople, log = "xy")
# x = c(1:nrow(d))
# y = d$NumberOfPeople
# plot(y, log = "x", main = "Log-log Plot")
# length(x)
# length(y)
# plot(1:100,1:100, log="xy")
# abline(v=seq(0,807,10), lty=3)
# abline(h=seq(0,807,10), lty=3)