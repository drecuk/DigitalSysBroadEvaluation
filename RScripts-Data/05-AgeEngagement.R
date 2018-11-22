#
# ----------------------------------- AGE and Engagement
# code by Eugene Ch'ng (www.complexity.io)
# http://www.github.com/drecuk
#
# The code is linked to the two articles below:
# Ch’ng E., Cai S., F.T. Leow, Zhang T. (forthcoming) The Adoption and Use of Emerging Cultural Technologies in China’s Museums, Journal of Cultural Heritage
# Ch’ng E., Cai S., F.T. Leow, Zhang T. (forthcoming) Datasets on the evaluation of the adoption and use of digital technologies in China Museums, Data in Brief
#

rm(list=ls()) #will remove ALL objects

# --------------------------- COLOUR RAMP
colfunc<-colorRampPalette(c("yellow","red"))
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)


#------------------------------------ LOAD FILE
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

dAE2 = read.table("AgeEngagement.csv", header=TRUE, skip=0, sep=",")
df = subset(dAE2, select=c(2:7))
names(df)
summary(df)

dev.new(width=10.25,height=5.25,units="in",res=1200, bg="white")

boxplot(df, ylab="Age", xlab="Engagement (0 = no engagement, 1 = low, 5 = high)", xaxt='n', yaxt='n')
axis(1, at=seq(1, 6, by=1), labels=c("0", "1", "2", "3", "4", "5")) 
axis(2, at=seq(0, 90, by=10)) 
title(main="Age Groups and Engagement", cex.main=1)

dev.print(png,'05-AgeEngagement.png', width=10.25,height=5.25, units="in",res=1200, bg="white") 
dev.off()


# --------- APPENDIX
# drawing age data out from the original raw file.
d = read.table("workingData.csv", header=TRUE, skip=0, sep=",")
# converting age "75-90" to 90 as numeric
df$Age2 <- as.character(df$Age)
df$Age2[df$Age2 == "75-90"] <- 90
df$Age <- as.numeric(df$Age2)


sum(((as.numeric(d$Age) >= 24) | (as.numeric(d$Age) <= 90)) & (d$Engagement == 5), na.rm = TRUE) # child and 5 engagement
sum(((d$Age >= 17) | (d$Age == 12)) & (d$Engagement == 4), na.rm = TRUE) # child and 4 engagement
sum(((d$Age == 17) | (d$Age == 12)) & (d$Engagement == 3), na.rm = TRUE) # child and 3 engagement
sum(((d$Age == 17) | (d$Age == 12)) & (d$Engagement == 2), na.rm = TRUE) # child and 2 engagement
sum(((d$Age == 17) | (d$Age == 12)) & (d$Engagement == 1), na.rm = TRUE) # child and 1 engagement
sum(((d$Age == 17) | (d$Age == 12)) & (d$Engagement == 0), na.rm = TRUE) # child and 1 engagement

sum((d$Age == 24) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 24) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 24) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 24) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 24) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 24) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 34) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 34) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 34) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 34) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 34) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 34) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 44) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 44) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 44) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 44) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 44) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 44) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 54) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 54) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 54) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 54) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 54) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 54) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 64) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 64) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 64) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 64) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 64) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 64) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 74) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 74) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 74) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 74) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 74) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 74) & (d$Engagement == 0), na.rm = TRUE)

sum((d$Age == 90) & (d$Engagement == 5), na.rm = TRUE)
sum((d$Age == 90) & (d$Engagement == 4), na.rm = TRUE)
sum((d$Age == 90) & (d$Engagement == 3), na.rm = TRUE)
sum((d$Age == 90) & (d$Engagement == 2), na.rm = TRUE)
sum((d$Age == 90) & (d$Engagement == 1), na.rm = TRUE)
sum((d$Age == 90) & (d$Engagement == 0), na.rm = TRUE)

#Age 44: 8 ranked 5, 11 ranked 4, 11 ranked 3, 9 ranked 2, 17 ranked 1 
#Age 54: 8 ranked 5, 3 ranked 4, 5 ranked 3, 10 ranked 2, 14 ranked 1 
#Age 34: 14 ranked 5, 37 ranked 4, 61 ranked 3, 39 ranked 2, 60 ranked 1 

sum(((d$Age == 17) | (d$Age == 12)))

