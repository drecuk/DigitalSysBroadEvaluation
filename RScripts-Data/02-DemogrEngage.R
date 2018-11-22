#
# ----------------------------------- Demographics and Engagement
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

d= read.table("workingData.csv", header=TRUE, skip=0, sep=",")
d
names(d)
summary(d)

# converting age "75-90" to 90 as numeric
d$Age <- as.character(d$Age)
d$Age[d$Age == "75-90"] <- 90
d$Age <- as.numeric(d$Age)


#
# ----------------------------------- METADATA START
#

dev.new(width=8.25,height=6.25,units="in",res=1200, bg="white")

m <- matrix(c(1:4), nrow = 2, ncol = 2, byrow = TRUE)
#       [,1] [,2] [,3] [,4]
# [1,]   1    1     2    2
# [2,]   0    3     3    0

nf <- layout(m)
layout.show(nf)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))
par(mgp=c(1.2,0.4,0))	 # label and axis spaces c(label, axes, title)	

par(mar=c(5,5,2,3))


# --------------------------- Age of Exhibits Rated
table1 = table(d$Age, d$Sex, dnn=c("Number of People", "Engagement")) # Creates a contingency table
addmargins(table1) #Displays the table (Not necessary)
barplot(table1, ylab="Population Distribution", xlab="", main="Age Groups and Sex", 
        col=(colfunc(9)), beside=TRUE, width=.3, names=c("Female", "Male"))
legend("topright", title="Age Groups", legend=sort(unique(paste("≤", d$Age))), fill =colfunc(9), box.lty=0, cex=0.6)

# --------------------------- Age AND Engagement
colfunc<-colorRampPalette(c("yellow","green"))
counts <- table(d$Age, d$Engagement)
barplot(counts, main="Age and Engagement",
        xlab="Age and Level of Engagement", ylab="Population Sample",col=(colfunc(9)))
legend("topright", title="Age", legend=sort(unique(paste("≤", d$Age))), fill=colfunc(9), box.lty=0, cex=0.6)

# --------------------------- Engagement AND QUALITY OF EXHIBIT
colfunc<-colorRampPalette(c("yellow","red"))
counts <- table(d$Engagement, d$QualityOfExhibit)
barplot(counts, main="Quality of Exhibit",
        xlab="Engagement", ylab="Population Sample", col=(colfunc(6)))
legend("topright", title="Quality", legend= sort(unique(d$QualityOfExhibit)), fill=colfunc(6), box.lty=0, cex=0.6)

# --------------------------- Number of People and Engagement
table1 = table(d$NumberOfPeople, d$Engagement, dnn=c("Number of People", "Engagement")) # Creates a contingency table
gray = gray.colors(15, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)
addmargins(table1) #Displays the table (Not necessary)
barplot(table1, ylab="Population Sample", xlab="Level of Engagement", main="Engagement and No. of people", beside=TRUE, width=.3)
legend("topright", title="No. of people", legend=sort(unique(d$NumberOfPeople)), fill=gray, box.lty=0, cex=0.6)


dev.print(png,'02-DemogrEngage.png', width=8.25,height=6.25, units="in",res=1200, bg="white") 
dev.off()