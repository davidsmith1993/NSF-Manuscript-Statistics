#######################################################################%
####                   E3 create data sets                          ####
#######################################################################%
rm(list = ls ())
## LIBRARIES

library(ggplot2)
library(haven)
library(plyr)
library(Rmisc)
library(wesanderson)
#library(psych)
library(tidyr)
library(QuantPsyc)
library(cocor)
library(mvoutlier)
library(nlme)
#detach("package:psych", unload=TRUE)
#install.packages("mvoutlier", repos="http://cran.rstudio.com/", dependencies=TRUE)

#######################################################################%
####                   LOAD IN THE DATA                             ####
#######################################################################%
#######################################################################%
####                   E3 Experiment 1                              ####
#######################################################################%
##Load in the data set for AB Training

Exp1AB <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1AB.csv",sep = ",")

#Create blocks instead of splitting by clusters
Exp1AB$Block1 <- (Exp1AB$A1 +Exp1AB$B1+Exp1AB$C1+Exp1AB$D1)/4
Exp1AB$Block2 <- (Exp1AB$A2 +Exp1AB$B2+Exp1AB$C2+Exp1AB$D2)/4
Exp1AB$Block3 <- (Exp1AB$A3 +Exp1AB$B3+Exp1AB$C3+Exp1AB$D3)/4
Exp1AB$Block4 <- (Exp1AB$A4 +Exp1AB$B4+Exp1AB$C4+Exp1AB$D4)/4
#Long Form
Exp1AB_long <- gather(Exp1AB, block, accuracy, B1:B4, factor_key = FALSE)
#Make Block a factor
Exp1AB_long$block <- factor(Exp1AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))
Exp1AB_long$accuracy <- (Exp1AB_long$accuracy)*100
#Graph a line graph
Exp1AB_long$block <- factor(Exp1AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))

####Load in the data set for Inf Training
Exp1inf <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1inf.csv",sep = ",")

#Make category A and B from Clusters

Exp1inf$CatA <- (Exp1inf$A + Exp1inf$C)/2
Exp1inf$CatB <- (Exp1inf$B + Exp1inf$D)/2

#Long Form
Exp1inflong <- gather(Exp1inf, Category, Correlation, CatA:CatB, factor_key = FALSE)


####Exp1Test####
Exp1test <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1test.csv",sep = ",")
Exp1test$CatA <- (Exp1test$A + Exp1test$C)/2
Exp1test$CatB <- (Exp1test$B + Exp1test$D)/2
#Long Form
Exp1testlong <- gather(Exp1test, Category, Correlation, CatA:CatB, factor_key = FALSE)
Exp1testlong$Cond <- factor(Exp1testlong$Cond, labels = c("A/B", "INF"))

#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp1testAB <- data.frame(subset(Exp1test, Cond == 1))
Exp1testAB$trainlearn <- (Exp1AB$Block4 - Exp1AB$Block1)
#Add an average correlation variable 
Exp1testAB$TotalCorr <- (Exp1testAB$CatA + (-1 * (Exp1testAB$CatB)))/2
Exp1infwide <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1infwide.csv",sep = ",")


Exp1testinf <- data.frame(subset(Exp1test, Cond == 2))
Exp1testinf$trainlearn <- Exp1infwide$TotalLearn
#Add an average correlation variable 
Exp1testinf$TotalCorr <- (Exp1testinf$CatA + (-1 * (Exp1testinf$CatB)))/2

#######################################################################%
####                   E3 Experiment 2                              ####
#######################################################################%

Exp2AB <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2AB.csv",sep = ",")
Exp2AB_long <- gather(Exp2AB, block, accuracy, B1:B4, factor_key = FALSE)
#Make Block a factor
Exp2AB_long$block <- factor(Exp2AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))

####Exp2 2 INF####
####Load in the data set for Inf Training
Exp2inf <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2inf.csv",sep = ",")

#Make category A and B from Clusters
Exp2inf$CatA <- (Exp2inf$A + Exp2inf$C)/2
Exp2inf$CatB <- (Exp2inf$B + Exp2inf$D)/2
#Long Form
Exp2inflong <- gather(Exp2inf, Category, Correlation, CatA:CatB, factor_key = FALSE)

#Create a b4-b1 correlation for INF####Exp2Test####
Exp2AB <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2AB.csv",sep = ",")
Exp2test <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2test.csv",sep = ",")
Exp2test$CatA <- (Exp2test$A + Exp2test$C)/2
Exp2test$CatB <- (Exp2test$B + Exp2test$D)/2
#Long Form
Exp2testlong <- gather(Exp2test, Category, Correlation, CatA:CatB, factor_key = FALSE)
Exp2testlong$Cond <- factor(Exp2testlong$Cond, labels = c("A/B", "INF"))

#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp2testAB <- data.frame(subset(Exp2test, Cond == 1))
Exp2testAB$trainlearn <- (Exp2AB$B4 - Exp2AB$B1)
#Add an average correlation variable 
Exp2testAB$TotalCorr <- (Exp2testAB$CatB + (-1 * (Exp2testAB$CatA)))/2
#Now we plot the scatterplot
#Load in infwide
Exp2infwide <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2infwide.csv",sep = ",")
Exp2test <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2test.csv",sep = ",")
Exp2test$CatA <- (Exp2test$A + Exp2test$C)/2
Exp2test$CatB <- (Exp2test$B + Exp2test$D)/2
Exp2testinf <- data.frame(subset(Exp2test, Cond == 2))
Exp2testinf$trainlearn <- Exp2infwide$TotalLearn
#Add an average correlation variable 
Exp2testinf$TotalCorr <- (Exp2testinf$CatB + (-1 * (Exp2testinf$CatA)))/2

#######################################################################%
####                   E3 Experiment 3                              ####
#######################################################################%
##Load in the data set for AB Training

Exp3AB <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3AB.csv",sep = ",")

#Create blocks instead of splitting by clusters
Exp3AB$Block1 <- (Exp3AB$A1 +Exp3AB$B1+Exp3AB$C1+Exp3AB$D1)/4
Exp3AB$Block2 <- (Exp3AB$A2 +Exp3AB$B2+Exp3AB$C2+Exp3AB$D2)/4
Exp3AB$Block3 <- (Exp3AB$A3 +Exp3AB$B3+Exp3AB$C3+Exp3AB$D3)/4
Exp3AB$Block4 <- (Exp3AB$A4 +Exp3AB$B4+Exp3AB$C4+Exp3AB$D4)/4
#Long Form
Exp3AB_long <- gather(Exp3AB, block, accuracy, B1:B4, factor_key = FALSE)
#Make Block a factor
Exp3AB_long$block <- factor(Exp3AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))
Exp3AB_long$accuracy <- (Exp3AB_long$accuracy)*100
#Graph a line graph
Exp3AB_long$block <- factor(Exp3AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))

####e3 INF FC#####
Exp3FC <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3FC.csv",sep = ",")

#Create blocks instead of splitting by clusters
Exp3FC$Block1 <- (Exp3FC$A1 +Exp3FC$B1+Exp3FC$C1+Exp3FC$D1)/4
Exp3FC$Block2 <- (Exp3FC$A2 +Exp3FC$B2+Exp3FC$C2+Exp3FC$D2)/4
Exp3FC$Block3 <- (Exp3FC$A3 +Exp3FC$B3+Exp3FC$C3+Exp3FC$D3)/4
Exp3FC$Block4 <- (Exp3FC$A4 +Exp3FC$B4+Exp3FC$C4+Exp3FC$D4)/4
#Long Form
Exp3FC_long <- gather(Exp3FC, block, accuracy, B1:B4, factor_key = FALSE)
#Make Block a factor
Exp3FC_long$block <- factor(Exp3FC_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))
Exp3FC_long$accuracy <- (Exp3FC_long$accuracy)*100
#Graph a line graph
Exp3FC_long$block <- factor(Exp3FC_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))


###EXP 3 Test####
#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp3test <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3test.csv",sep = ",")
Exp3test$CatA <- (Exp3test$A + Exp3test$C)/2
Exp3test$CatB <- (Exp3test$B + Exp3test$D)/2

Exp3testAB <- data.frame(subset(Exp3test, Cond == 1))
Exp3testAB$trainlearn <- (Exp3AB$Block4 - Exp3AB$Block1)
#Add an average correlation variable 
Exp3testAB$TotalCorr <- (Exp3testAB$CatA + (-1 * (Exp3testAB$CatB)))/2

#Now we do it with FC training
Exp3test <- read.delim(file="C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3test.csv",sep = ",")
Exp3test$CatA <- (Exp3test$A + Exp3test$C)/2
Exp3test$CatB <- (Exp3test$B + Exp3test$D)/2

Exp3testFC <- data.frame(subset(Exp3test, Cond == 2))
Exp3testFC$trainlearn <- (Exp3FC$Block4 - Exp3FC$Block1)
#Add an average correlation variable 
Exp3testFC$TotalCorr <- (Exp3testFC$CatA + (-1 * (Exp3testFC$CatB)))/2
#####mvoutlier#####
Exp3testFC <- na.omit(Exp3testFC)
mv1 <- cbind(Exp3testFC$trainlearn,Exp3testFC$TotalCorr)
outliers <- aq.plot(mv1)
outliers <- unlist(outliers, use.names = FALSE)
outliers <- t(outliers)
outliers <- t(outliers)
Exp3testFC$outliers <- outliers
Exp3testFCoutlierremoved <- Exp3testFC[!(Exp3testFC$outliers==TRUE),]


Exp1testAB <- na.omit(Exp1testAB)
mv1 <- cbind(Exp1testAB$trainlearn,Exp1testAB$TotalCorr)
outliers <- aq.plot(mv1)
outliers <- unlist(outliers, use.names = FALSE)
outliers <- t(outliers)
outliers <- t(outliers)
Exp1testAB$outliers <- outliers
Exp1testABoutlierremoved <- Exp1testAB[!(Exp1testAB$outliers==TRUE),]

Exp1testinf <- na.omit(Exp1testinf)
mv1 <- cbind(Exp1testinf$trainlearn,Exp1testinf$TotalCorr)
outliers <- aq.plot(mv1)
outliers <- unlist(outliers, use.names = FALSE)
outliers <- t(outliers)
outliers <- t(outliers)
Exp1testinf$outliers <- outliers
Exp1testinfoutlierremoved <- Exp1testinf[!(Exp1testinf$outliers==TRUE),]
##################SAVE THE DATA#######################
write.csv(Exp1testAB, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1testAB.csv")
write.csv(Exp2testAB, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2testAB.csv")
write.csv(Exp1testinf, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1testinf.csv")
write.csv(Exp2testinf, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2testinf.csv")
write.csv(Exp1inflong, "C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1inflong.csv")
write.csv(Exp2AB_long, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2AB_long.csv")
write.csv(Exp2testAB, "C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2testAB.csv")
write.csv(Exp2testinf, "C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2testinf.csv")
write.csv(Exp2inf, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2inf.csv")
write.csv(Exp1AB_long, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1AB_long.csv")
write.csv(Exp1inf, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1inf.csv")
write.csv(Exp1testlong, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp1testlong.csv")
write.csv(Exp2testlong, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp2testlong.csv")
write.csv(Exp3AB_long, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3AB_long.csv")
write.csv(Exp3FC_long, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3FC_long.csv")
write.csv(Exp3testAB, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3testAB.csv")
write.csv(Exp3testFCoutlierremoved, "C:/Users/David/Documents/NSF e3 Data Analysis/r/datasheets/Exp3testFC.csv")
