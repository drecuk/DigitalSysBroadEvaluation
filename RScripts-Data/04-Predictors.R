#
# ----------------------------------- PREDICTIVE START
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

d= read.table("workingData.OLD.csv", header=TRUE, skip=0, sep=",")
d
names(d)
summary(d)

# converting age "75-90" to 90 as numeric
d$Age <- as.character(d$Age)
d$Age[d$Age == "75-90"] <- 90
d$Age <- as.numeric(d$Age)

dev.new(width=10.25,height=10.25,units="in",res=1200, bg="white")
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
max(d$Adult)
#  (Adult TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs1 = glm(Adult ~ TimeAtExhibit, data=d, family=binomial, na.action=na.omit)
x <- c(0:vmax)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Adult, factor=0.5)~d$TimeAtExhibit, ylab="P(Adult = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs1, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs1),pch=20, col='red')
title(main="Predictors of Adult Visitors and Access Time", cex.main=0.7)

# generate confidence intervals for the regression coefficients
summary(rs1)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    2.97082    0.17328  17.145   <2e-16 ***
#   TimeAtExhibit -0.05101    0.02136  -2.388   0.0169 * 
# for every one unit change in TimeAtExhibit, there is a log odds of
# of the user not being an adult by -0.05101

# put the coefficients and confidence intervals onto a useful scale
exp(rs1$coefficients)
# (Intercept) TimeAtExhibit 
# 19.5078558     0.9502669 
# for a one unit increase in TimeAtExhibit, the odds of not being an adult
# increase by a factor of 0.95. 
#exp(confint(rs1))


#  (Family TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs2 = glm(Family ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs2$coefficients[1] # intercept
m = rs2$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Family, factor=0.5)~d$TimeAtExhibit, ylab="P(Family = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs2, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs2),pch=20, col='red')
title(main="Predictors of Family Visitors and Access Time", cex.main=0.7)

# generate confidence intervals for the regression coefficients
summary(rs2)
#confint(rs2)
exp(rs2$coefficients)
#exp(confint(rs2))

#  ---------------------- (Discussion TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs3 = glm(Discussion ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs3$coefficients[1] # intercept
m = rs3$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Discussion, factor=0.5)~d$TimeAtExhibit, ylab="P(Discussion = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs3, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs3),pch=20, col='red')
title(main="Predictors of Discussions and Access Time", cex.main=0.7)

x = 5 # if TimeAtExhibit is at 5, what is the probability of queued
b = rs3$coefficients[1] # intercept
m = rs3$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
as.numeric(y)
# generate confidence intervals for the regression coefficients
summary(rs3)
#confint(rs3)
exp(rs3$coefficients)
#exp(confint(rs3))

#  ---------------------- (Discussion ~ Number of People) ****
vmax = max(d$NumberOfPeople)
# plot the predicted values using the sigmoid function
rs4 = glm(Discussion ~ NumberOfPeople, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs4$coefficients[1] # intercept
m = rs4$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Discussion, factor=0.5)~d$NumberOfPeople, ylab="P(Discussion = 1)", xlab="Number of People (Group)", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
NumberOfPeople = d$NumberOfPeople
curve(predict(rs4, data.frame(NumberOfPeople = x), type = "resp"), add = TRUE)
points(d$NumberOfPeople,fitted(rs4),pch=20, col='red')
title(main="Predictors of Discussions and Number Of People (Group)", cex.main=0.7)
x = 5 # if TimeAtExhibit is at 5, what is the probability of queued
b = rs4$coefficients[1] # intercept
m = rs4$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
as.numeric(y)
# generate confidence intervals for the regression coefficients
summary(rs4)
#confint(rs4)
exp(rs4$coefficients)
#exp(confint(rs4))


dev.print(png,'04-Predictors.png', width=10.25,height=10.25, units="in",res=1200, bg="white") 
dev.off()



#  --------------- END SET OF USEFUL GRAPHS




#  --------------- (Sex TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs1 = glm(Sex ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Sex, factor=0.5)~d$TimeAtExhibit, ylab="P(Sex = Male)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs1, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs1),pch=20, col='red')
title(main="Predictors of Sex and Access Time")


#  --------------- (Iscrowded TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs1 = glm(Iscrowded ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Iscrowded, factor=0.5)~d$TimeAtExhibit, ylab="P(IsCrowded = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs1, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs1),pch=20, col='red')
title(main="Time Spent on Exhibit if Crowded")

#  --------------- (Attracts Queue TimeSpent)
vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs1 = glm(AttractsQueue ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$AttractsQueue, factor=0.5)~d$TimeAtExhibit, ylab="P(AttractsQueue = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs1, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs1),pch=20, col='red')
title(main="Time Spent on Exhibit if it Attracts Queue")


#  --------------- (Queued TimeSpent)

vmax = max(d$TimeAtExhibit)
# plot the predicted values using the sigmoid function
rs1 = glm(Queued ~ TimeAtExhibit, data = d, family = binomial, na.action = na.omit)
x <- c(0:vmax)
b = rs1$coefficients[1] # intercept
m = rs1$coefficients[2] # slope
y <- exp((b + m*x)) / (1 + exp((b + m*x)))
plot(jitter(d$Queued, factor=0.5)~d$TimeAtExhibit, ylab="P(Queued = 1)", xlab="Time At Exhibit", ylim=c(0,1), xlim=c(0,vmax))
# draw a curve based on prediction from logistic regression model
TimeAtExhibit = d$TimeAtExhibit
curve(predict(rs1, data.frame(TimeAtExhibit = x), type = "resp"), add = TRUE)
points(d$TimeAtExhibit,fitted(rs1),pch=20, col='red')
title(main="Predictors of Queueing Visitors and Access Time")


###### END 3rd SET