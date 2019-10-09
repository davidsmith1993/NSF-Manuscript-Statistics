#######################################################################%
####                   E3 Analyses                                  ####
#######################################################################%
rm(list = ls ())
## LIBRARIES

library(ggplot2)
library(haven)
library(plyr)
library(Rmisc)
library(wesanderson)
library(psych)
library(tidyr)
library(QuantPsyc)
library(cocor)
library(mvoutlier)
library(nlme)
library(lme4)
library(emmeans)
library(dplyr)
library(sjstats)
library(heplots)
library(ez)
library(multcomp)
library(outliers)
#detach("package:outliers", unload=TRUE)
#library(effsize)
#detach("package:psych", unload=TRUE)
#detach("package:effsize", unload=TRUE)
library(lsr)
library(Bolstad)
#######################################################################%
####                   LOAD IN THE DATA                             ####
#######################################################################%
Exp2AB_long <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2AB_long.csv",sep = ",")
Exp2inf <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2inf.csv",sep = ",")
Exp1AB_long <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1AB_long.csv",sep = ",")
Exp1inf <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1inf.csv",sep = ",")
Exp1testlong <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1testlong.csv",sep = ",")
Exp2testlong <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2testlong.csv",sep = ",")
Exp3AB_long <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3AB_long.csv",sep = ",")
Exp3FC_long <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3FC_long.csv",sep = ",")
Exp3testAB <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3testAB.csv",sep = ",")
Exp3testFC <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3testFC.csv",sep = ",")
Exp1testAB <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1testAB.csv",sep = ",")
Exp2testAB <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2testAB.csv",sep = ",")
Exp1testinf <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1testinf.csv",sep = ",")
Exp2testinf <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2testinf.csv",sep = ",")
Exp1Training <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp1Training.csv",sep = ",")
Exp2Training <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2Training.csv",sep = ",")
Exp3Training <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3Training.csv",sep = ",")
Exp3Test <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3test.csv",sep = ",")
Exp2AB <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp2AB.csv",sep = ",")

#######################################################################%
####                   E3 Experiment 1                              ####
#######################################################################%
####Exp1 Training
#For inf see if clusters are different in block 4
Exp1Training$Clusters <- factor(Exp1Training$Clusters)
Exp1traininginf <- Exp1Training[Exp1Training$Cond==2,]
Exp1traininginf <- na.omit(Exp1traininginf)
clustersinf<-lme(b4clusterscorrected ~ Clusters, data=Exp1traininginf, random = ~1|SubID)
anova(clustersinf)
summary(glht(clustersinf, linfct=mcp(Clusters = "Tukey")), test = adjusted(type = "bonferroni"))
confint(glht(clustersinf, linfct=mcp(Clusters = "Tukey")), test = adjusted(type = "bonferroni"))
ezANOVA(Exp1traininginf, b4clusterscorrected, wid = SubID, within = Clusters, detailed = TRUE)

Exp1trainingAB <- Exp1Training[Exp1Training$Cond==1,]
Exp1trainingAB <- Exp1trainingAB[Exp1trainingAB$Block==4,]
keep <- c("SubID", "Clusters", "B4clusters")
Exp1trainingAB = Exp1trainingAB[,keep]
Exp1trainingAB$Clusters <- factor(Exp1trainingAB$Clusters)
clustersAB<-lme(B4clusters ~ Clusters, Exp1trainingAB, random = ~1|SubID)
anova(clustersAB)
summary(glht(clustersAB, linfct=mcp(Clusters = "Tukey")), test = adjusted(type = "bonferroni"))
confint(glht(clustersAB, linfct=mcp(Clusters = "Tukey")), test = adjusted(type = "bonferroni"))
aggregate(Exp1trainingAB, list(Exp1trainingAB$Clusters), mean)
sum = summarySE(Exp1trainingAB, 
                measurevar="B4clusters", 
                groupvars=c("Clusters"))
sum


pairwise.t.test(Exp1Training[Exp1Training$Cond==2,]$b4clusterscorrected, Exp1Training[Exp1Training$Cond==2,]$Clusters, paired = TRUE, p.adjust.method = "bonferroni")

#2 t tests to show learning in each block
#AB
ttestexp1AB <- Exp1Training[Exp1Training$Cond==1,]
ttestexp1AB <- ttestexp1AB[which(ttestexp1AB$Block==1|ttestexp1AB$Block==4),]
t.test(performance ~ Block, paired = TRUE, data=ttestexp1AB)
ttestexp1AB$Block <- factor(ttestexp1AB$Block)
cohensD(performance ~ Block, data = ttestexp1AB)

#INF
ttestexp1Inf <- Exp1Training[Exp1Training$Cond==2,]
ttestexp1Inf <- ttestexp1Inf[which(ttestexp1Inf$Block==1|ttestexp1Inf$Block==4),]
t.test(performance ~ Block, paired = TRUE, data=ttestexp1Inf)
ttestexp1Inf$Block <- factor(ttestexp1Inf$Block)
cohensD(performance ~ Block, data = ttestexp1Inf)
#These were my mixed anovas
#Exp1Training$Block <- factor(Exp1Training$Block)
#Exp1TrainModel<-(lme(performance ~ Cond*Block, random=list(SubID=pdBlocked(list(~1, pdIdent(~Cond-1), pdIdent(~Block-1)))), method="ML", data=Exp1Training))


#Find what blocks are different (I dont need this with the t tests above)
require(multcomp)
Exp1Training$Clusters <- factor(Exp1Training$Clusters)
Exp1Training$Block <- factor(Exp1Training$Block)
Exp1TrainModelinf<-lme(performance ~ Block, data=Exp1Training[Exp1Training$Cond==2,], random = ~1|SubID)
summary(glht(Exp1TrainModelinf, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))
Exp1TrainModelAB<-lme(performance ~ Block, data=Exp1Training[Exp1Training$Cond==1,], random = ~1|SubID)
summary(glht(Exp1TrainModelAB, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))




####Exp 1 Test
#correlations for each condition between Training (b4-b1) and test
cor.test(Exp1testAB$TotalCorr,Exp1testAB$trainlearn,use = "complete.obs" )
cor.test(Exp1testinf$TotalCorr,Exp1testinf$trainlearn,use = "complete.obs" )
paired.r(0.5694225, 0.6168135, n=54, n2=51, twotailed = TRUE)
#average correlation learned during test
r.test(length(Exp1testAB$TotalCorr), mean(Exp1testAB$TotalCorr, na.rm = TRUE), twotailed = TRUE)
r.test(length(Exp1testinf$TotalCorr), mean(Exp1testinf$TotalCorr, na.rm = TRUE), twotailed = TRUE)
paired.r(mean(Exp1testAB$TotalCorr, na.rm = TRUE), mean(Exp1testinf$TotalCorr, na.rm = TRUE), n=length(Exp1testAB$TotalCorr), n2=length(Exp1testinf$TotalCorr), twotailed = TRUE)


#average correlation learned during test split by category
#This is not correct DBS 7-13-18
#I need to do some one sample t tests instead!
#AB cat A
r.test(length(Exp1testAB$CatA), mean(Exp1testAB$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp1testAB, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#AB cat B
r.test(length(Exp1testAB$CatB), mean(Exp1testAB$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp1testAB, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)
#Inf catA
r.test(length(Exp1testinf$CatA), mean(Exp1testinf$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp1testinf, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#Inf catB
r.test(length(Exp1testinf$CatB), mean(Exp1testinf$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp1testinf, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)

paired.r(mean(Exp1testinf$CatB, na.rm = TRUE), mean(Exp1testAB$CatB, na.rm = TRUE), n=length(Exp1testinf$CatB), n2=length(Exp1testAB$CatB), twotailed = TRUE)

#These are the one sample t tests to check if the correlation learned is different than 0
t.test(Exp1testAB$TotalCorr,mu = 0)
t.test(Exp1testinf$TotalCorr,mu = 0)
cohensD(Exp1testAB$TotalCorr)
cohensD(Exp1testinf$TotalCorr)

t.test(Exp1testAB$TotalCorr, Exp1testinf$TotalCorr, var.equal = TRUE)
cohensD(Exp1testAB$TotalCorr, Exp1testinf$TotalCorr)



t.test(Exp2testAB$TotalCorr,mu = 0)
t.test(Exp2testinf$TotalCorr,mu = 0)
cohensD(Exp2testAB$TotalCorr)
cohensD(Exp2testinf$TotalCorr)

t.test(Exp2testAB$TotalCorr, Exp2testinf$TotalCorr, var.equal = TRUE)
cohensD(Exp2testAB$TotalCorr, Exp2testinf$TotalCorr)

t.test(Exp3testAB$TotalCorr,mu = 0)
t.test(Exp3testFC$TotalCorr,mu = 0)
cohensD(Exp3testAB$TotalCorr)
cohensD(Exp3testFC$TotalCorr)

t.test(Exp3testAB$TotalCorr, Exp3testFC$TotalCorr, var.equal = TRUE)
cohensD(Exp3testAB$TotalCorr, Exp3testFC$TotalCorr)


#######################################################################%
####                   E3 Experiment 2                              ####
#######################################################################%
####Exp2 Training
#Rotated Stimuli
#2 t tests to show learning in each block
#AB
ttestexp2AB <- Exp2Training[Exp2Training$Cond==1,]
ttestexp2AB <- ttestexp2AB[which(ttestexp2AB$Block==1|ttestexp2AB$Block==4),]
t.test(performance ~ Block, paired = TRUE,  data=ttestexp2AB)
cohensD(performance ~ Block, data = ttestexp2AB)
#INF
ttestexp2Inf <- Exp2Training[Exp2Training$Cond==2,]
ttestexp2Inf <- ttestexp2Inf[which(ttestexp2Inf$Block==1|ttestexp2Inf$Block==4),]
t.test(performance ~ Block, paired = TRUE, data=ttestexp2Inf)
cohensD(performance ~ Block, data = ttestexp2Inf)
#Find what blocks are different
require(multcomp)
Exp2TrainModelinf<-lme(performance ~ Block, data=Exp2Training[Exp2Training$Cond==2,], random = ~1|SubID)
summary(glht(Exp2TrainModelinf, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))
Exp2TrainModelAB<-lme(performance ~ Block, data=Exp2Training[Exp2Training$Cond==1,], random = ~1|SubID)
summary(glht(Exp2TrainModelAB, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))

####Exp 2 Test
#correlations for each condition between Training (b4-b1) and test
cor.test(Exp2testAB$TotalCorr,Exp2testAB$trainlearn,use = "complete.obs" )
cor.test(Exp2testinf$TotalCorr,Exp2testinf$trainlearn,use = "complete.obs" )
paired.r(0.2509768, 0.1639384, n=34, n2=39, twotailed = TRUE)
#average correlation learned during test
r.test(length(Exp2testAB$TotalCorr), mean(Exp2testAB$TotalCorr), twotailed = TRUE)
r.test(length(Exp2testinf$TotalCorr), mean(Exp2testinf$TotalCorr, na.rm = TRUE), twotailed = TRUE)
paired.r(mean(Exp2testAB$TotalCorr, na.rm = TRUE), mean(Exp2testinf$TotalCorr, na.rm = TRUE), n=length(Exp2testAB$TotalCorr), n2=length(Exp2testinf$TotalCorr), twotailed = TRUE)



#average correlation learned during test split by category
#AB cat A
r.test(length(Exp2testAB$CatA), mean(Exp2testAB$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp2testAB, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#AB cat B
r.test(length(Exp2testAB$CatB), mean(Exp2testAB$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp2testAB, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)
#Inf catA
r.test(length(Exp2testinf$CatA), mean(Exp2testinf$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp2testinf, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#Inf catB
r.test(length(Exp2testinf$CatB), mean(Exp2testinf$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp2testinf, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)



#one sample t's for each quadrant for the test phase
#Start with Classification, create the arrays to be tested for each quadrant
Q1class = Exp2testAB$A
Q2class = Exp2testAB$B
Q3class = Exp2testAB$C
Q4class = Exp2testAB$D
#Now run the one sample t tests
t.test(Q1class,mu = 0)
cohensD(Q1class)

t.test(Q2class,mu = 0)
cohensD(Q2class)

t.test(Q3class,mu = 0)
cohensD(Q3class)

t.test(Q4class,mu = 0)
cohensD(Q4class)

#now lets do the same for inference!
Q1inf = Exp2testinf$A
Q2inf = Exp2testinf$B
Q3inf = Exp2testinf$C
Q4inf = Exp2testinf$D
#Now run the one sample t tests
t.test(Q1inf,mu = 0)
cohensD(Q1inf)

t.test(Q2inf,mu = 0)
cohensD(Q2inf)

t.test(Q3inf,mu = 0)
cohensD(Q3inf)

t.test(Q4inf,mu = 0)
cohensD(Q4inf)
#######################################################################%
####                   E3 Experiment 3                              ####
#######################################################################%
####Exp 3 Training

#2 t tests to show learning in each block (to keep consistent with exp 1 and 2)
#AB
ttestexp3AB <- Exp3Training[Exp3Training$Cond==1,]
ttestexp3AB <- ttestexp3AB[which(ttestexp3AB$Block==1|ttestexp3AB$Block==4),]
t.test(performance ~ Block, paired = TRUE, data=ttestexp3AB)
ttestexp3AB$Block <- factor(ttestexp3AB$Block)
cohensD(performance ~ Block, data = ttestexp3AB)

#INF
ttestexp3Inf <- Exp3Training[Exp3Training$Cond==2,]
ttestexp3Inf <- ttestexp3Inf[which(ttestexp3Inf$Block==1|ttestexp3Inf$Block==4),]
t.test(performance ~ Block, paired = TRUE, data=ttestexp3Inf)
ttestexp3Inf$Block <- factor(ttestexp3Inf$Block)
cohensD(performance ~ Block, data = ttestexp3Inf)

#Forced Choice
#2 condition by 4 block mixed anova (i can only do the mixed anova for exp 3)
Exp3Training$Block <- factor(Exp3Training$Block)
Exp3Training$Cond <- factor(Exp3Training$Cond)

sum = summarySE(Exp3Training, 
                measurevar="performance", 
                groupvars=c("Cond", "Block"))
sum
m1 <- ezANOVA(data=Exp3Training, dv = performance, wid = SubID, within = .(Block), between= .(Cond), type = 3, detailed = TRUE)
m1$ANOVA
fit <- aov(performance ~ (Block) + Error(SubID),
           data=Exp3Training)
summary(fit)
glht(fit, linfct = mcp(Block = "Tukey"))
 

exp3trainlong<- reshape(Exp3Training, idvar = "SubID", timevar = "Block", direction = "wide")
write.csv(exp3trainlong, "C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3Traininglong.csv")
#Find what blocks are different
require(multcomp)
Exp3TrainModelFC<-lme(performance ~ Block, data=Exp3Training[Exp3Training$Cond==2,], random = ~1|SubID)
summary(glht(Exp3TrainModelFC, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))
Exp3TrainModelAB<-lme(performance ~ Block, data=Exp3Training[Exp3Training$Cond==1,], random = ~1|SubID)
summary(glht(Exp3TrainModelAB, linfct=mcp(Block = "Tukey")), test = adjusted(type = "bonferroni"))

#Checking to see if there is a differnec in AB and FC training in EXP3 for only block 3
ttestexp3block4 <- Exp3Training[Exp3Training$Block==4,]
ttestexp3block4 <- ttestexp2AB[which(ttestexp2AB$Block==1|ttestexp2AB$Block==4),]
t.test(performance ~ Cond, paired = FALSE, var.equal=TRUE, data=ttestexp3block4)
cohensD(performance ~ Cond, data = ttestexp3block4)


####Exp 3 Test
#correlations for each condition between Training (b4-b1) and test
cor.test(Exp3testAB$TotalCorr,Exp3testAB$trainlearn,use = "complete.obs" )
cor.test(Exp3testFC$TotalCorr,Exp3testFC$trainlearn,use = "complete.obs" )
paired.r(.4735, .474, n=37, n2=27, twotailed = TRUE)
#average correlation learned during test
r.test(length(Exp3testAB$TotalCorr), mean(Exp3testAB$TotalCorr), twotailed = TRUE)
r.test(length(Exp3testFC$TotalCorr), mean(Exp3testFC$TotalCorr, na.rm = TRUE), twotailed = TRUE)
paired.r(mean(Exp3testAB$TotalCorr, na.rm = TRUE), mean(Exp3testFC$TotalCorr, na.rm = TRUE), n=length(Exp3testAB$TotalCorr), n2=length(Exp3testFC$TotalCorr), twotailed = TRUE)

#average correlation learned during test split by category
#AB cat A
r.test(length(Exp3testAB$CatA), mean(Exp3testAB$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp3testAB, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#AB cat B
r.test(length(Exp3testAB$CatB), mean(Exp3testAB$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp3testAB, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)
#Inf catA
r.test(length(Exp3testFC$CatA), mean(Exp3testFC$CatA, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp3testFC, na.rm = TRUE,  
                measurevar="CatA")
print(sum$CatA/sum$sd)
#Inf catB
r.test(length(Exp3testFC$CatB), mean(Exp3testFC$CatB, na.rm = TRUE), twotailed = TRUE)
sum = summarySE(Exp3testFC, na.rm = TRUE,  
                measurevar="CatB")
print(sum$CatB/sum$sd)

####EXP 3 only learners#### 
Exp3testABlearners <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3testABlearners.csv",sep = ",")
Exp3testFClearners <- read.delim("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/datasheets/Exp3testFClearners.csv",sep = ",")


t.test(Exp3testABlearners$TotalCorr,mu = 0)
t.test(Exp3testFClearners$TotalCorr,mu = 0)
cohensD(Exp3testAB$TotalCorr)
cohensD(Exp3testFC$TotalCorr)

t.test(Exp3testAB$TotalCorr, Exp3testFC$TotalCorr, var.equal = TRUE)
cohensD(Exp3testAB$TotalCorr, Exp3testFC$TotalCorr)


####EXP 1 only learners#### 
Exp1testABlearners <- read.delim("C:/Users/Pablo/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1testABlearners.csv",sep = ",")
Exp1testinflearners <- read.delim("C:/Users/Pablo/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1testinflearners.csv",sep = ",")


t.test(Exp1testABlearners$TotalCorr,mu = 0)
t.test(Exp1testinflearners$TotalCorr,mu = 0)
cohensD(Exp1testABlearners$TotalCorr)
cohensD(Exp1testinflearners$TotalCorr)

t.test(Exp1testABlearners$TotalCorr, Exp1testinflearners$TotalCorr, var.equal = TRUE)
cohensD(Exp1testABlearners$TotalCorr, Exp1testinflearners$TotalCorr)






outlier_values <- boxplot.stats(Exp2testinf$D)$out  # outlier values.
boxplot(Exp2testinf$D, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)




#####Bayes Stats E1####
#So, we would need bayes factors for the 2 one-sample tests as well as the class vs. inf test, in each experiment
#Bayes factor to look at 1 sample t test for correlation at test compared to 0
#inference
bayes.t.test(Exp1testinf$TotalCorr, mu=0)
#classification
bayes.t.test(Exp1testAB$TotalCorr, mu=0)