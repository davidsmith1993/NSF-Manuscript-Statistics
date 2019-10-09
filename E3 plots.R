#######################################################################%
####                   E3 Analyses and graphs                       ####
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
theme_update(plot.title = element_text(hjust = 0.5, size = 16))
#######################################################################%
####                   E3 Experiment 1                              ####
#######################################################################%
##Load in the data set for AB Training

Exp1AB <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1AB.csv",sep = ",")

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
sum = summarySE(Exp1AB_long, na.rm = TRUE,  
                measurevar="accuracy", 
                groupvars=c("block"))


sum$block <- as.integer(sum$block)

ggplot(sum, aes(x=block, 
                y=accuracy)) + 
  geom_errorbar(aes(ymin=accuracy-se, 
                    ymax=accuracy+se), 
                width=.2, size=0.7) +
  ylim(50, 100) + 
  geom_line() + 
  geom_point() +
  theme_minimal() +
  theme(axis.text =element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Accuracy (+/-SEM)") 
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp1ABTrain.png", width = 5, height = 5)


#exp1 abtrain splitcat
Exp1ablong <- gather(Exp1AB_long, Category, Performance, CatA:CatB, factor_key = FALSE)

sum = summarySE(Exp1ablong, na.rm = TRUE,  
                measurevar="Performance", 
                groupvars=c("block", "Category"))

ggplot(sum, aes(x=block, 
                y=Performance, 
                color=Category,
                group = Category)) + 
  geom_errorbar(aes(ymin=Performance-se, 
                    ymax=Performance+se), 
                width=.2, size=0.7) +
  ylim(.5, 1) + 
  geom_line() +
  geom_point() +
  theme_grey()  +
  labs(x="Block", y="Proportion Correct (+/-SEM)") +
  scale_color_manual(values = c("black", "darkgrey")) +   theme_minimal() +ggtitle("Classification")+theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 20)) + theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold"))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp1ABTrainsplit.png", width = 5, height = 5)
####Load in the data set for Inf Training
Exp1inf <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1inf.csv",sep = ",")

#Make category A and B from Clusters

Exp1inf$CatA <- (Exp1inf$A + Exp1inf$C)/2
Exp1inf$CatB <- (Exp1inf$B + Exp1inf$D)/2

#Long Form
  Exp1inflong <- gather(Exp1inf, Category, Correlation, CatA:CatB, factor_key = FALSE)
  Exp1inflong$Block <- factor(Exp1inflong$Block, labels = c("Block1", "Block2", "Block3", "Block4"))
  
  
  ggplot(sum, aes(x=Block, 
                  y=Correlation, 
                  color=Category,
                  group = Category)) + 
    geom_errorbar(aes(ymin=Correlation-se, 
                      ymax=Correlation+se), 
                  width=.2, size=0.7) +
    ylim(-1, 1) + 
    geom_line() +
    geom_point() +
    theme_grey() +
    theme_minimal() +
    theme(
      axis.text =element_text(size=14),
      legend.title=element_text(size=14),
      legend.text=element_text(size=14),
      axis.title.y = element_text(vjust= 1.8, size = 16),
      axis.title.x = element_text(vjust= -0.5, size = 16),
      axis.title = element_text(face = "bold")) +
    labs(x="Block", y="Correlation (+/-SEM)") +
    scale_color_manual(values = c("black", "darkgrey")) +ggtitle("Inference")+ theme(plot.title = element_text(hjust = 0.5, size = 20))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp1infTrain.png", width = 5, height = 5)


####Exp1Test####
Exp1test <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1test.csv",sep = ",")
Exp1test$CatA <- (Exp1test$A + Exp1test$C)/2
Exp1test$CatB <- (Exp1test$B + Exp1test$D)/2
#Long Form
Exp1testlong <- gather(Exp1test, Category, Correlation, CatA:CatB, factor_key = FALSE)
Exp1testlong$Cond <- factor(Exp1testlong$Cond, labels = c("A/B", "INF"))



sum = summarySE(Exp1inflong, na.rm = TRUE,  
                measurevar="Correlation", 
                groupvars=c("Category", "Cond"))

is.factor(sum$Cond)

ggplot(sum, aes(x=Category, 
                y=Correlation, 
                color=Cond,
                group = Cond)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Correlation-se, 
                    ymax=Correlation+se), 
                width=.2, size=0.7) +
  ylim(-1, 1) +
  geom_point() +
  theme_grey() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Correlation (+/-SEM)") +
  scale_color_manual(values = wes_palette("Cavalcanti")) 


#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp1testAB <- data.frame(subset(Exp1test, Cond == 1))
Exp1testAB$trainlearn <- (Exp1AB$Block4 - Exp1AB$Block1)
#Add an average correlation variable 
Exp1testAB$TotalCorr <- (Exp1testAB$CatA + (-1 * (Exp1testAB$CatB)))/2
#Now we plot the scatterplot
ggplot(Exp1testAB, aes(x=trainlearn, 
                y=TotalCorr)) + geom_jitter() + geom_smooth(method= "lm") +theme_minimal() +theme(
                  axis.text =element_text(size=14),
                  legend.title=element_text(size=14),
                  legend.text=element_text(size=14),
                  axis.title.y = element_text(vjust= 1.8, size = 16),
                  axis.title.x = element_text(vjust= -0.5, size = 16),
                  axis.title = element_text(face = "bold")) + ylim(-1,1) +
  labs(x="Change in Performance", y="Correlation") + ggtitle("Classification")  + theme(plot.title = element_text(hjust = 0.5, size = 20))
  
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp1ABscatter.png", width = 5, height = 5)





#Create a b4-b1 correlation for INF
#Load in infwide
Exp1infwide <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp1infwide.csv",sep = ",")

Exp1testinf <- data.frame(subset(Exp1test, Cond == 2))
Exp1testinf$trainlearn <- Exp1infwide$TotalLearn
#Add an average correlation variable 
Exp1testinf$TotalCorr <- (Exp1testinf$CatA + (-1 * (Exp1testinf$CatB)))/2
#Now we plot it!

ggplot(Exp1testinf, aes(x=trainlearn, 
                       y=TotalCorr)) + geom_jitter() + geom_smooth(method= "lm") + theme_minimal() + theme(
                         axis.text =element_text(size=14),
                         legend.title=element_text(size=14),
                         legend.text=element_text(size=14),
                         axis.title.y = element_text(vjust= 1.8, size = 16),
                         axis.title.x = element_text(vjust= -0.5, size = 16),
                         axis.title = element_text(face = "bold")) + ylim(-1,1) + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(x="Change in Performance", y="Correlation") + ggtitle("Inference")

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp1inftestscatter.png", width = 5, height = 5)

cor.test(Exp1testinf$TotalCorr,Exp1testinf$trainlearn,use = "complete.obs" )
summary(m2)


#exp1test bar graphs ####
sum = summarySE(Exp1testlong, na.rm = TRUE,  
                measurevar="Correlation", 
                groupvars=c("Cond", "Category"))
sum$Cond <- factor(sum$Cond, labels = c("A/B","INF"))
sum$Category <- factor(sum$Category, labels = c("A","B"))
ggplot(sum, aes(x=Cond, 
                y=Correlation, 
                fill=Category,
                group = Category)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position = position_dodge(.9),aes(ymin=Correlation-se, 
                    ymax=Correlation+se), colour="black",
                width=.2, size=0.7) +
  ylim(-1, 1) + 
  theme_grey() +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Training Methodology", y="Correlation (+/-SEM)") +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) + ggtitle("Test Phase Performance") + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("A" = "grey17", "B" = "grey"))

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/exp1inftestbarsplit.png", width = 5, height = 5)

#not split by cat
sum = summarySE(Exp1testlong, na.rm = TRUE,  
                measurevar="performance", 
                groupvars=c("Cond"))
sum$Cond <- factor(sum$Cond, labels = c("Classification","Inference"))
ggplot(sum, aes(x=Cond, 
                y=performance, 
                fill=Cond,
                group = Cond)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position = position_dodge(.9),aes(ymin=performance-se, 
                                                  ymax=performance+se), colour="black",
                width=.2, size=0.7) +
  ylim(-1, 1) + 
  theme_grey() + theme_minimal() +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Training Methodology", y="Correlation (+/-SEM)") +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) + ggtitle("Test Phase Performance") + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Classification" = "grey17", "Inference" = "grey")) 

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/exp1inftestbar.png", width = 5, height = 5)

#######################################################################%
####                   E3 Experiment 2                              ####
#######################################################################%
##Load in the data set for AB Training
Exp2AB <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2AB.csv",sep = ",")
Exp2AB_long <- gather(Exp2AB, block, accuracy, B1:B4, factor_key = FALSE)
#Make Block a factor
Exp2AB_long$block <- factor(Exp2AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))

#Graph a line graph
Exp2AB_long$block <- factor(Exp2AB_long$block, labels = c("Block1", "Block2", "Block3", "Block4"))
sum = summarySE(Exp2AB_long, na.rm = TRUE,  
                measurevar="accuracy", 
                groupvars=c("block"))


sum$block <- as.integer(sum$block)

ggplot(sum, aes(x=block, 
                y=accuracy)) + 
  geom_errorbar(aes(ymin=accuracy-se, 
                    ymax=accuracy+se), 
                width=.2, size=0.7) +
  ylim(50, 100) + 
  geom_line() + 
  geom_point()+
  theme_minimal() +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Accuracy (+/-SEM)") +
  scale_color_manual(values = c("green", "blue")) 

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp2ABTrain.png", width = 5, height = 5)


#exp2 abtrain splitcat
Exp2AB_long <- gather(Exp2AB_long, Category, Performance, CatA:CatB, factor_key = FALSE)

sum = summarySE(Exp2AB_long, na.rm = TRUE,  
                measurevar="Performance", 
                groupvars=c("block", "Category"))

ggplot(sum, aes(x=block, 
                y=Performance, 
                color=Category,
                group = Category)) + 
  geom_errorbar(aes(ymin=Performance-se, 
                    ymax=Performance+se), 
                width=.2, size=0.7) +
  ylim(.5, 1) + 
  geom_line() +
  geom_point() +
  theme_grey()  +
  labs(x="Block", y="Proportion Correct (+/-SEM)") +
  scale_color_manual(values = c("black", "darkgrey")) +   theme_minimal() +ggtitle("Classification")+theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 20)) + theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold"))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp2ABTrainsplit.png", width = 5, height = 5)



####Load in the data set for Inf Training
####Exp2 2 INF####
####Load in the data set for Inf Training
Exp2inf <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2inf.csv",sep = ",")

#Make category A and B from Clusters

Exp2inf$CatA <- (Exp2inf$A + Exp2inf$C)/2
Exp2inf$CatB <- (Exp2inf$B + Exp2inf$D)/2

#Long Form
Exp2inflong <- gather(Exp2inf, Category, Correlation, CatA:CatB, factor_key = FALSE)


sum = summarySE(Exp2inflong, na.rm = TRUE,  
                measurevar="Correlation", 
                groupvars=c("Block", "Category"))

ggplot(sum, aes(x=Block, 
                y=Correlation, 
                color=Category,
                group = Category)) + 
  geom_errorbar(aes(ymin=Correlation-se, 
                    ymax=Correlation+se), 
                width=.2, size=0.7) +
  ylim(-1, 1) + 
  geom_line() +
  geom_point() +
  theme_grey()+   theme_minimal() +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Correlation (+/-SEM)") +
  scale_color_manual(values = c("black", "darkgrey"))  +ggtitle("Inference")+ theme(plot.title = element_text(hjust = 0.5, size = 20))


ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp2INFTrain.png", width = 5, height = 5)



ggplot(Exp2testAB, aes(x=.01*trainlearn, 
                       y=-1*TotalCorr)) + geom_jitter() + geom_smooth(method= "lm")+theme_minimal() + theme(
                         axis.text =element_text(size=14),
                         legend.title=element_text(size=14),
                         legend.text=element_text(size=14),
                         axis.title.y = element_text(vjust= 1.8, size = 16),
                         axis.title.x = element_text(vjust= -0.5, size = 16),
                         axis.title = element_text(face = "bold")) + ylim(-1,1) +
  labs(x="Change in Performance", y="Correlation") + ggtitle("Classification") + theme(plot.title = element_text(hjust = 0.5, size = 20))


ggsave("C:/Users/Pablo/Desktop/NSF e3 Data Analysis/r/plots/Exp2ABscatter.png", width = 5, height = 5)

m1 <- cor(Exp2testAB$trainlearn,Exp2testAB$TotalCorr,use = "complete.obs" )
summary




#Create a b4-b1 correlation for INF####Exp2Test####
Exp2AB <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2AB.csv",sep = ",")
Exp2test <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2test.csv",sep = ",")
Exp2test$CatA <- (Exp2test$A + Exp2test$C)/2
Exp2test$CatB <- (Exp2test$B + Exp2test$D)/2
#Long Form
Exp2testlong <- gather(Exp2test, Category, Correlation, CatA:CatB, factor_key = FALSE)
Exp2testlong$Cond <- factor(Exp2testlong$Cond, labels = c("A/B", "INF"))



sum = summarySE(Exp1inflong, na.rm = TRUE,  
                measurevar="Correlation", 
                groupvars=c("Category", "Cond"))

is.factor(sum$Cond)

ggplot(sum, aes(x=Category, 
                y=Correlation, 
                color=Cond,
                group = Cond)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Correlation-se, 
                    ymax=Correlation+se), 
                width=.2, size=0.7) +
  ylim(-1, 1) +
  geom_point() +
  theme_grey() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Correlation (+/-SEM)") +
  scale_color_manual(values = wes_palette("Cavalcanti")) 


#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp2testAB <- data.frame(subset(Exp2test, Cond == 1))
Exp2testAB$trainlearn <- (Exp2AB$B4 - Exp2AB$B1)
#Add an average correlation variable 
Exp2testAB$TotalCorr <- (Exp2testAB$CatA + (-1 * (Exp2testAB$CatB)))/2
#Now we plot the scatterplot
#Load in infwide
Exp2infwide <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2infwide.csv",sep = ",")
Exp2test <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp2test.csv",sep = ",")
Exp2test$CatA <- (Exp2test$A + Exp2test$C)/2
Exp2test$CatB <- (Exp2test$B + Exp2test$D)/2
Exp2testinf <- data.frame(subset(Exp2test, Cond == 2))
Exp2testinf$trainlearn <- Exp2infwide$TotalLearn
#Add an average correlation variable 
Exp2testinf$TotalCorr <- (Exp2testinf$CatA + (-1 * (Exp2testinf$CatB)))/2
#Now we plot it!

ggplot(Exp2testinf, aes(x=trainlearn, 
                        y=-1*TotalCorr)) + geom_jitter() + geom_smooth(method= "lm")+theme_minimal()  + theme(
                          axis.text =element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.text=element_text(size=14),
                          axis.title.y = element_text(vjust= 1.8, size = 16),
                          axis.title.x = element_text(vjust= -0.5, size = 16),
                          axis.title = element_text(face = "bold")) + ylim(-1,1) +
  labs(x="Change in Performance", y="Correlation") + ggtitle("Inference")+ theme(plot.title = element_text(hjust = 0.5, size = 20))


ggsave("C:/Users/Pablo/Desktop/NSF e3 Data Analysis/r/plots/Exp2inftestscatter.png", width = 5, height = 5)

m2 <- cor(Exp2testinf$trainlearn,Exp2testinf$TotalCorr,use = "complete.obs" )
summary(m2)
cor.test(Exp2testinf$TotalCorr,Exp2testinf$trainlearn,use = "complete.obs" )
cor.test(Exp2testAB$TotalCorr,Exp2testAB$trainlearn,use = "complete.obs" )

r.test(53, m1, m2,  pooled=TRUE, twotailed = TRUE)


#Exp 2 test bar graph
sum = summarySE(Exp2testlong, na.rm = TRUE,  
                measurevar="performance", 
                groupvars=c("Cond"))
sum$Cond <- factor(sum$Cond, labels = c("Classification","Inference"))
ggplot(sum, aes(x=Cond, 
                y=-1*performance, 
                fill=Cond,
                group = Cond)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position = position_dodge(.9),aes(ymin=-1*performance-se, 
                                                  ymax=-1*performance+se), colour="black",
                width=.2, size=0.7) +
  ylim(-1, 1) + 
  theme_grey() + theme_minimal() +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Training Methodology", y="Correlation (+/-SEM)") +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) + ggtitle("Test Phase Performance") + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Classification" = "grey17", "Inference" = "grey")) 


ggsave("C:/Users/Pablo/Desktop/NSF e3 Data Analysis/r/plots/exp2inftestbar.png", width = 5, height = 5)


#######################################################################%
####                   E3 Experiment 3                              ####
#######################################################################%
##Load in the data set for AB Training

Exp3AB <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp3AB.csv",sep = ",")

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
sum = summarySE(Exp3AB_long, na.rm = TRUE,  
                measurevar="accuracy", 
                groupvars=c("block"))


sum$block <- as.integer(sum$block)

ggplot(sum, aes(x=block, 
                y=accuracy)) + 
  geom_errorbar(aes(ymin=accuracy-se, 
                    ymax=accuracy+se), 
                width=.2, size=0.7) +
  ylim(40, 100) + 
  geom_line() + 
  geom_point()+
  theme_minimal()  +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Accuracy (+/-SEM)") +
  scale_color_manual(values = wes_palette("Cavalcanti")) 
ggsave("C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/plots/Exp3ABTrain.png", width = 5, height = 5)

#######################################################################%
####                   E3 Experiment 3                              ####
#######################################################################%
##Load in the data set for AB Training

Exp3FC <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp3FC.csv",sep = ",")

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
sum = summarySE(Exp3FC_long, na.rm = TRUE,  
                measurevar="accuracy", 
                groupvars=c("block"))


sum$block <- as.integer(sum$block)

ggplot(sum, aes(x=block, 
                y=accuracy)) + 
  geom_errorbar(aes(ymin=accuracy-se, 
                    ymax=accuracy+se), 
                width=.2, size=0.7) +
  ylim(40, 100) + 
  geom_line() + 
  geom_point()+
  theme_minimal()  +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Block", y="Accuracy (+/-SEM)") +
  scale_color_manual(values = wes_palette("Cavalcanti")) + theme(plot.title = element_text(hjust = 0.5, size = 20))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp3FCTrain.png", width = 5, height = 5)


#exp3 abtrain splitcat
Exp3AB_long <- gather(Exp3AB_long, Category, Performance, CatA:CatB, factor_key = FALSE)

sum = summarySE(Exp3AB_long, na.rm = TRUE,  
                measurevar="Performance", 
                groupvars=c("block", "Category"))

ggplot(sum, aes(x=block, 
                y=Performance, 
                color=Category,
                group = Category)) + 
  geom_errorbar(aes(ymin=Performance-se, 
                    ymax=Performance+se), 
                width=.2, size=0.7) +
  ylim(.5, 1) + 
  geom_line() +
  geom_point() +
  theme_grey()  +
  labs(x="Block", y="Proportion Correct (+/-SEM)") +
  scale_color_manual(values = c("black", "darkgrey")) +   theme_minimal() +ggtitle("Classification")+theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold"))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp3ABTrainsplit.png", width = 5, height = 5)

#exp3 FCtrain splitcat
Exp3FC_long <- gather(Exp3FC_long, Category, Performance, CatA:CatB, factor_key = FALSE)

sum = summarySE(Exp3FC_long, na.rm = TRUE,  
                measurevar="Performance", 
                groupvars=c("block", "Category"))

ggplot(sum, aes(x=block, 
                y=Performance, 
                color=Category,
                group = Category)) + 
  geom_errorbar(aes(ymin=Performance-se, 
                    ymax=Performance+se), 
                width=.2, size=0.7) +
  ylim(.5, 1) + 
  geom_line() +
  geom_point() +
  theme_grey()  +
  labs(x="Block", y="Proportion Correct (+/-SEM)") +
  scale_color_manual(values = c("black", "darkgrey")) +   theme_minimal() +ggtitle("Two-Choice Inference")+theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold"))
ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp3FCTrainsplit.png", width = 5, height = 5)


###EXP 3 Test####
#####Relationship between learning during training and the test phase stimuli correlation####
#First we need to create a b4-b1 accuracy variable for AB
Exp3test <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp3test.csv",sep = ",")
Exp3test$CatA <- (Exp3test$A + Exp3test$C)/2
Exp3test$CatB <- (Exp3test$B + Exp3test$D)/2

Exp3testAB <- data.frame(subset(Exp3test, Cond == 1))
Exp3testAB$trainlearn <- (Exp3AB$Block4 - Exp3AB$Block1)
#Add an average correlation variable 
Exp3testAB$TotalCorr <- (Exp3testAB$CatA + (-1 * (Exp3testAB$CatB)))/2
#Now we plot the scatterplot
ggplot(Exp3testAB, aes(x=trainlearn, 
                       y=TotalCorr)) + geom_jitter() +theme_minimal()+ geom_smooth(method= "lm") +
                        theme(
                          axis.text =element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.text=element_text(size=14),
                          axis.title.y = element_text(vjust= 1.8, size = 16),
                          axis.title.x = element_text(vjust= -0.5, size = 16),
                          axis.title = element_text(face = "bold")) + ylim(-1,1) + 
  labs(x="Change in Performance", y="Correlation") + ggtitle("Classification") + theme(plot.title = element_text(hjust = 0.5, size = 20))

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp3ABtest.png", width = 5, height = 5)



cor.test(Exp3testAB$TotalCorr,Exp3testAB$trainlearn,use = "complete.obs" )


#Now we do it with FC training
Exp3test <- read.delim(file="C:/Users/smit1_000/Desktop/NSF e3 Data Analysis/r/datasheets/Exp3test.csv",sep = ",")
Exp3test$CatA <- (Exp3test$A + Exp3test$C)/2
Exp3test$CatB <- (Exp3test$B + Exp3test$D)/2

Exp3testFC <- data.frame(subset(Exp3test, Cond == 2))
Exp3testFC$trainlearn <- (Exp3FC$Block4 - Exp3FC$Block1)
#Add an average correlation variable 
Exp3testFC$TotalCorr <- (Exp3testFC$CatA + (-1 * (Exp3testFC$CatB)))/2
#Now we plot the scatterplot
ggplot(Exp3testFC, aes(x=trainlearn, 
                       y=TotalCorr)) + geom_jitter()+theme_minimal()  + geom_smooth(method= "lm") +
                        theme(
                          axis.text =element_text(size=14),
                          legend.title=element_text(size=14),
                          legend.text=element_text(size=14),
                          axis.title.y = element_text(vjust= 1.8, size = 16),
                          axis.title.x = element_text(vjust= -0.5, size = 16),
                          axis.title = element_text(face = "bold")) + ylim(-1,1) +
  labs(x="Change in Performance", y="Correlation") + ggtitle("Two-Choice Inference")+ theme(plot.title = element_text(hjust = 0.5, size = 20))

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/Exp3FCtest.png", width = 5, height = 5)

cor.test(Exp3testFC$TotalCorr,Exp3testFC$trainlearn,use = "complete.obs" )


#exp3test bar graph

sum = summarySE(Exp3Test, na.rm = TRUE,  
                measurevar="performance", 
                groupvars=c("Cond"))
sum$Cond <- factor(sum$Cond, labels = c("Classification","Inference"))
ggplot(sum, aes(x=Cond, 
                y=performance, 
                fill=Cond,
                group = Cond)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(position = position_dodge(.9),aes(ymin=performance-se, 
                                                  ymax=performance+se), colour="black",
                width=.2, size=0.7) +
  ylim(-1, 1) + 
  theme_grey() + theme_minimal()+
  theme(
    axis.text =element_text(size=14),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    axis.title.y = element_text(vjust= 1.8, size = 16),
    axis.title.x = element_text(vjust= -0.5, size = 16),
    axis.title = element_text(face = "bold")) +
  labs(x="Training Methodology", y="Correlation (+/-SEM)") +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1)) + ggtitle("Test Phase Performance")  + theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Classification" = "grey17", "Inference" = "grey")) 

ggsave("C:/Users/David/Documents/Exp 3 Manuscript/NSF e3 Data Analysis/r/plots/exp3inftestbar.png", width = 5, height = 5)






