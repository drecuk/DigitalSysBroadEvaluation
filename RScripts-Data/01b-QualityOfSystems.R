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

dev.new(width=10.25,height=5.25,units="in",res=300, bg="white")

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
d <- c(7, 18, 11)
types <- c("Games (19.4%)","Information (50%)","Narrative (31%)")
pie(d, labels = types, radius=1.2, main="Distribution of Content Types", col=c('red', 'green', 'blue'), cex=0.6, cex.main=0.8)

par(mar=c(5,5,2,3))
# Grouped Bar Plot
dat <- read.table(text="Game	Information	Narrative
1	0	0	0
2	0	5	2
3	5	8	4
4	1	4	3
5	1	1	2
", header = TRUE)

heatcols <- heat.colors(5)
barplot(as.matrix(dat), main="Quality of Exhibits",
      xlab="Quality Ranking for Exhibits (1 to 5)", 
        legend = c(1:5), col=heatcols, beside=TRUE, cex.names=0.8, cex.main=0.6)

dev.print(png,'01b-QualityOfSystems.png', width=10.25,height=5.25, units="in",res=300, bg="white") 
dev.off()

###### END 1ST SET
