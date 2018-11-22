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

d= read.table("workingData.csv", header=TRUE, skip=0, sep=",")
d
names(d)
summary(d)

#
# ----------------------------------- METADATA START
#

dev.new(width=10.25,height=5.25,units="in",res=1200, bg="white")

m <- matrix(c(1:2), nrow = 1, ncol = 2, byrow = TRUE)
#       [,1] [,2] [,3] [,4]
# [1,]   1    1     2    2
# [2,]   0    3     3    0

nf <- layout(m)
layout.show(nf)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))
par(mgp=c(1.2,0.4,0))	 # label and axis spaces c(label, axes, title)	

par(mar=c(2,2,2,2))

# --------------------------- Exhibit Type PIE CHART
# counting number of unique systems
numSys <- length(unique(d$Exhibit))

# counting number of each unique system in the digital systems category
numAR <- length(unique(d$Exhibit[d$ExhibitType == "AR"]))
numVR <- length(unique(d$Exhibit[d$ExhibitType == "VR"]))
numProj <- length(unique(d$Exhibit[d$ExhibitType == "Projection"]))
numM2D <- length(unique(d$Exhibit[d$ExhibitType == "Multitouch2D"]))
numi2D <- length(unique(d$Exhibit[d$ExhibitType == "i2D"]))
numi3D <- length(unique(d$Exhibit[d$ExhibitType == "i3D"]))

numSys
numAR
numVR
numProj
numM2D
numi2D
numi3D

piepercent<- round(100*c(numAR,numVR,numProj,numM2D,numi2D,numi3D)/numSys, 1)
#piepercent<- round(100*mytable/sum(mytable), 1)
pie(c(numAR,numVR,numProj,numM2D,numi2D,numi3D), labels=piepercent, main="Digital System Types", col= c("red", "yellow", "green", "violet","orange", "blue", "pink", "cyan"), radius=1.5)
legend("topleft", title="Type", legend=sort(unique(d$ExhibitType)), fill=c("red", "orange", "violet", "blue", "green", "yellow"), box.lty=0, cex=0.7)

# --------------------------- Quality of Exhibits Rated
# hist(d$QualityOfExhibit, main="Quality of Digital Exhibit", yaxt="n", xlab="Quality Ratings", ylab="Number of Digital Systems", col="blue", ylim=c(0,400))
# axis(2, 1:608, c(1:36), col.axis = "blue")

unique(d$ExhibitType)

quality <- c(length(unique(d$Exhibit[d$QualityOfExhibit == 1])),
  length(unique(d$Exhibit[d$QualityOfExhibit == 2])),
  length(unique(d$Exhibit[d$QualityOfExhibit == 3])),
  length(unique(d$Exhibit[d$QualityOfExhibit == 4])),
  length(unique(d$Exhibit[d$QualityOfExhibit == 5]))
)

par(mar=c(4,4,2,3))

barplot(quality, xaxt='n', xlab="", ylab="", yaxt='n', col = "cyan", width=0.5, space=1)
axis(1, at=c(1:5), label=c(1:5), las=0)
# axis(2, at=c(1:22), label=c(1:22), las=0)
title(main="Quality of Exhibit", sub="", xlab="Quality Ranking (1 to 5)", ylab="Number of Exhibits", cex.main=0.8)
x = c(1:5)
y = quality
text(x, y, labels=quality, cex=0.8, pos=3, offset=0.5, adj = c(0.5, 1))


dev.print(png,'01-DigitalSystems.png', width=10.25,height=5.25, units="in",res=1200, bg="white") 
dev.off()

###### END 1ST SET
