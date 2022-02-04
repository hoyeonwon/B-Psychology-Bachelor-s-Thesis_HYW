

# DEMOGRAPHICS ------------------------------------------------------------
library(plyr)
dpath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/demographics"
data_demo <- ldply(list.files(path=dpath, full.names=TRUE), read.csv, header=TRUE)
View(data_demo)

data_demo$ID[which(data_demo$Age==0)] #participants who wrote their age as 0
#20211215173641 20220119204911 
MQpath <-"C:/Users/ASUS/Documents/3rd Year/Thesis Writing/MQ" 
data_MQ <- ldply(list.files(path=MQpath, full.names=TRUE), read.csv, header=TRUE)
data_MQ<- data_MQ[which(!data_MQ$ID ==20211215173641),]
data_MQ<- data_MQ[which(!data_MQ$ID ==20220119204911),]
data_demo<-data_demo[which((data_demo$ID %in% data_MQ$ID)==TRUE),] #extract participants who didn't complete the experiment by finding mismatch

summary(data_demo[,-1])
sapply(data_demo[,-1],mean)
sapply(data_demo[,-1],sd)
View(data_demo)

#By Gender
Femaledemo<-data_demo[which(data_demo$Gender==1),names(data_demo) %in% c(colnames(data_demo))]
Maledemo<-data_demo[which(data_demo$Gender==2),names(data_demo) %in% c(colnames(data_demo))]
Otherdemo<-data_demo[which(data_demo$Gender==3),names(data_demo) %in% c(colnames(data_demo))] #the only participant has been extracted
View(Femaledemo)
f<-function(x){
  list(mean(x),sd(x),length(x))
}

Femaled<-sapply(Femaledemo[,-1],f)
Maled<-sapply(Maledemo[,-1],f)
list(Femaled,Maled)
length(which(Maledemo$Occupation==1))

# EXPLORATION -------------------------------------------------------------

exppath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/exploration" 
data_exp <- ldply(list.files(path=exppath, full.names=TRUE), read.csv, header=TRUE)
data_exp<-data_exp[which((data_exp$ID %in% data_MQ$ID)==TRUE),] #extract participants who didn't complete the experiment

#creating a data frame with means of exploration per participant per block of trials
explo <- data.frame()
index <- list(a = seq(from = 4, to = 52, by = 2), b = seq(from = 54, to = 102, by = 2), c = seq(from = 104, to = 152, by = 2), d = seq(from = 154, to = 202, by = 2))

for(i in 1:nrow(data_exp)) { 
  
  for(j in 1:4){ 
    explo[i, j] <- mean(as.numeric(data_exp[i, index[[j]]]))
  } 
  
}
colnames(explo) <- c(paste("Explo Block", 1:4))
data_exp<- cbind(data_exp, explo)



#computing mean and standard deviation of exploration rate per condition per block of trials
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")


# for the reward frequency condition p = 0.1
means_p_0.1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_0.1[j, i] <- mean(data_exp[which(data_exp[, 2] == ps[1] & data_exp[, 3] == cond[j]), 204+i])
  }
} 

colnames(means_p_0.1) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.1) <- c(paste("Control", ps[1]), paste("Yoked", ps[1]))
View(means_p_0.1)


# for the reward frequency condition p = 0.2
means_p_0.2 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_0.2[j, i] <- mean(data_exp[which(data_exp[, 2] == ps[2] & data_exp[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_0.2) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.2) <- c(paste("Control", ps[2]), paste("Yoked", ps[2]))


# for the reward frequency condition p = 0.4
means_p_0.4 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_0.4[j, i] <- mean(data_exp[which(data_exp[, 2] == ps[3] & data_exp[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_0.4) <- c(paste("Explo Block", 1:4))
rownames(means_p_0.4) <- c(paste("Control", ps[3]), paste("Yoked", ps[3]))


# for the reward frequency condition p = 1
means_p_1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    means_p_1[j, i] <- mean(data_exp[which(data_exp[, 2] == ps[4] & data_exp[, 3] == cond[j]), 204+i])
  }
} 
colnames(means_p_1) <- c(paste("Explo Block", 1:4))
rownames(means_p_1) <- c(paste("Control", ps[4]), paste("Yoked", ps[4]))

means_p_0.1
means_p_0.2
means_p_0.4
means_p_1

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Exploration_plots.pdf")
par(mfrow = c(2, 2))

# p = 0.1
plot(x = 1:4, y = means_p_0.1[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0.3, 1), main = "p reward = 0.1", lwd = 2)
lines(x = 1:4, y = means_p_0.1[1, ], col = "green", lwd = 2)
legend(x = "bottomright", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.2
plot(x = 1:4, y = means_p_0.2[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0.3, 1), main = "p reward = 0.2", lwd = 2)
lines(x = 1:4, y = means_p_0.2[1, ], col = "green", lwd = 2)
legend(x = "bottomright", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.4
plot(x = 1:4, y = means_p_0.4[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0.3, 1), main = "p reward = 0.4", lwd = 2)
lines(x = 1:4, y = means_p_0.4[1, ], col = "green", lwd = 2)
legend(x = "bottomright", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

# p = 1
plot(x = 1:4, y = means_p_1[2, ], type = "l", xlab = "Block of trials", ylab = "Exploration rate", col = "red", ylim = c(0.3, 1), main = "p reward = 1", lwd = 2)
lines(x = 1:4, y = means_p_1[1, ], col = "green", lwd = 2)
legend(x = "bottomright", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

dev.off()


# DIRECT MEASURE OF PERCEIVED CONTROL -------------------------------------

DirectPCpath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/DirectPC"
data_dir_pc <- ldply(list.files(path=DirectPCpath, full.names=TRUE), read.csv, header=TRUE)
data_dir_pc<-data_dir_pc[which((data_dir_pc$ID %in% data_MQ$ID)==TRUE),] #extract participants who didn't complete the experiment by finding mismatch

# recode Strongly Disagree to 1 and Strongly Agree to 10

for(j in 1:ncol(data_dir_pc)) {
  for(i in 1:nrow(data_dir_pc)) {
    if(data_dir_pc[i, j] == "Strongly Disagree") {
      data_dir_pc[i, j] <- 1
    } else if (data_dir_pc[i, j] == "Strongly Agree") {
      data_dir_pc[i, j] <- 10
    }
  }
}


# covert to numeric
for(i in 1:4) {
  data_dir_pc[, 1+i] <- as.numeric(data_dir_pc[, 1+i])
}

# merging the exploration data with the direct measure of PC data
exp_dirpc <- cbind(data_exp, data_dir_pc[ ,-1])
head(data_exp[, 1]) == head(data_dir_pc[, 1])

# knowing how many people from each condition participated in the study
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")
sample <- data.frame()

for(j in 1:4) {
  for(i in 1:2) {
    sample[i, j] <- sum(exp_dirpc[, 2] == ps[j] & exp_dirpc[, 3] == cond[i])
    
  }
}
colnames(sample) <- c(ps)
rownames(sample) <- c(cond)
sumCTRL<-sum(sample[1,])
sumYOK<-sum(sample[2,])
Sum<-rbind(sumCTRL,sumYOK)
tcolsum<-colSums(sample)
sample<-cbind(sample,Sum)
sample<-rbind(sample,Colsum=tcolsum)
View(sample)

# mean of direct measure of perceived controllability per condition per block of trials

# for the reward frequency condition p = 0.1
dir_p_0.1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.1[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[1] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.1[i, j] <- mean(as.numeric(dir_p_0.1[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.1 <- dir_p_0.1[1:2, 1:2]
colnames(dir_p_0.1) <- c(paste("Half", 1:2))
rownames(dir_p_0.1) <- c(paste("Control", ps[1]), paste("Yoked", ps[1]))



# for the reward frequency condition p = 0.2
dir_p_0.2 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.2[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[2] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.2[i, j] <- mean(as.numeric(dir_p_0.2[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.2 <- dir_p_0.2[1:2, 1:2]
colnames(dir_p_0.2) <- c(paste("Half", 1:2))
rownames(dir_p_0.2) <- c(paste("Control", ps[2]), paste("Yoked", ps[2]))


# for the reward frequency condition p = 0.4
dir_p_0.4 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_0.4[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[3] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_0.4[i, j] <- mean(as.numeric(dir_p_0.4[i, (2*j-1):(2*j)]))
  }
}
dir_p_0.4 <- dir_p_0.4[1:2, 1:2]
colnames(dir_p_0.4) <- c(paste("Half", 1:2))
rownames(dir_p_0.4) <- c(paste("Control", ps[3]), paste("Yoked", ps[3]))




# for the reward frequency condition p = 1
dir_p_1 <- data.frame()

for(i in 1:4) { # looping over the block
  for(j in 1:2) { # looping over control condition (control/yoked)
    dir_p_1[j, i] <- mean(exp_dirpc[which(exp_dirpc[, 2] == ps[4] & exp_dirpc[, 3] == cond[j]), 208+i])
  }
} 

for(i in 1:2) {
  for(j in 1:2) {
    dir_p_1[i, j] <- mean(as.numeric(dir_p_1[i, (2*j-1):(2*j)]))
  }
}
dir_p_1 <- dir_p_1[1:2, 1:2]
colnames(dir_p_1) <- c(paste("Half", 1:2))
rownames(dir_p_1) <- c(paste("Control", ps[4]), paste("Yoked", ps[4]))


dir_p_0.1
dir_p_0.2
dir_p_0.4
dir_p_1

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/DirectPC_plots.pdf")
par(mfrow = c(2, 2))

# p = 0.1
plot(x = 1:2, y = dir_p_0.1[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.1", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.1[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.1[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.1[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.2
plot(x = 1:2, y = dir_p_0.2[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.2", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.2[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.2[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.2[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)


# p = 0.4
plot(x = 1:2, y = dir_p_0.4[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 0.4", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_0.4[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_0.4[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_0.4[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)

# p = 1
plot(x = 1:2, y = dir_p_1[2, ], type = "l", xlab = "Half", ylab = "Explicit PC", col = "red", ylim = c(0, 10), main = "p reward = 1", lwd = 1.5, xaxt = "n")
lines(x = 1:2, y = dir_p_1[2, ], col = "red", lwd = 2, type = "p")
axis(side=1, at=seq(1,2,1))
lines(x = 1:2, y = dir_p_1[1, ], col = "green", lwd = 1.5, type = "l")
lines(x = 1:2, y = dir_p_1[1, ], col = "green", lwd = 2, type = "p")
legend(x = "topleft", legend = c("Control", "Yoked"), col = c("green", "red"), pch = 15)
dev.off()


# PERCEIVED CONTROLLABLIITY -----------------------------------------------
pc1path <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/perceived_control1" 
data_pc1 <- ldply(list.files(path=pc1path, full.names=TRUE), read.csv, header=TRUE)
rewardfreq<-(data_pc1$X.10guess+data_pc1$X.11guess)/100 #the total expected frequency of reward
data_pc1<-cbind(data_pc1,rewardfreq) #combine
data_pc1<- data_pc1[which((data_pc1$ID %in% data_MQ$ID)==TRUE),] #remove participantsexppath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/exploration" 
length(unique(data_pc1$ID)) #check whether the participant lists are complete

Conditions<-rep(data_exp$Control.Condition,each=16) #for each unique ID
Reward_Frequency <- rep(data_exp$Reward.Frequency,each=16)
data_pc1<-cbind(data_pc1,Conditions,Reward_Frequency) #add conditions of participants to data frame

#Calculate variance between 4 sequential trials
varbtw<- numeric()
for (i in 1:(nrow(data_pc1)/4)){
  varbtw[i]<- var(data_pc1[(4*i-3):(4*i),9])
}

condit<-numeric()
for(i in 1:(nrow(data_pc1)/4)){
  condit[i]<-data_pc1[4*i,10]
}

prob<-numeric()
for(i in 1:(nrow(data_pc1)/4)){
  prob[i]<-data_pc1[4*i,11]
}

length(unique(data_pc1$ID))
Trials<-rep(1:4,times=98) #Trial numbers

Vardata <- data.frame(var_btw_reward=varbtw,Control=condit,Reward=prob,Trials)
#divide by blocks
Block1<- data.frame(Vardata[which(Vardata$Trials=='1'),])
Block1<-Block1[,c(2,3,4,1)]
Block2<- data.frame(Block2_var=Vardata$var_btw_reward[Vardata$Trials=='2'])
Block3<- data.frame(Block3_var=Vardata$var_btw_reward[Vardata$Trials=='3'])
Block4<- data.frame(Block4_var=Vardata$var_btw_reward[Vardata$Trials=='4'])
Vardata2<- data.frame(Block1[-3],Block2,Block3,Block4)
names(Vardata2)<- c("Control","Reward","Block1_var","Block2_var","Block3_var","Block4_var")

#means of PC1 by condition & reward frequency & Block
ps<- c(0.1,0.2,0.4,1)
cond <- c('CTRL','YOK')

means_PC1 <- data.frame()
for(m in 1:2) {
  for(j in 1:4) {
    for(i in 1:4) {
      
      means_PC1[j+(m-1)*4, i] <- mean(Vardata2[which(Vardata2[2] == ps[j] & Vardata2[1] == cond[m]), 2+i])
      
    }
  }
}
colnames(means_PC1) <- c(paste("Block", 1:4))
rownames(means_PC1) <- c(paste("Control", ps), paste("YOK", ps))
View(means_PC1)
View(Vardata2)

Half1<-rowMeans(means_PC1[,c('Block 1', 'Block 2')], na.rm=TRUE)
Half2<-rowMeans(means_PC1[,c('Block 3','Block 4')],na.rm=TRUE)
meanshalf_PC1<-cbind(means_PC1,Half1,Half2)
#each blocks + half
View(meanshalf_PC1)

Half1_0.1 <- c(meanshalf_PC1[1,5],meanshalf_PC1[5,5])
Half1_0.2 <- c(meanshalf_PC1[2,5],meanshalf_PC1[6,5])
Half1_0.4 <- c(meanshalf_PC1[3,5],meanshalf_PC1[7,5])
Half1_1 <- c(meanshalf_PC1[4,5],meanshalf_PC1[8,5])

Half2_0.1 <- c(meanshalf_PC1[1,6],meanshalf_PC1[5,6])
Half2_0.2 <- c(meanshalf_PC1[2,6],meanshalf_PC1[6,6])
Half2_0.4 <- c(meanshalf_PC1[3,6],meanshalf_PC1[7,6])
Half2_1 <- c(meanshalf_PC1[4,6],meanshalf_PC1[8,6])

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/PC1_plots.pdf")
par(mfrow = c(2,2))

#Plot for p(reward)=0.1 Perceived Controllabliity
Low_PC1plot<- data.frame(Half1_0.1,Half2_0.1)
names(Low_PC1plot) <- c("Half 1","Half 2")
barplot(height=as.matrix(Low_PC1plot), ylim=c(0,0.2), main="p(reward) = .1",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))
#Plot for p(reward)=0.1 Perceived Controllabliity
Mod_PC1plot<- data.frame(Half1_0.2,Half2_0.2)
names(Mod_PC1plot) <- c("Half 1","Half 2")
barplot(height=as.matrix(Mod_PC1plot), ylim=c(0,0.2),main="p(reward) = .2",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))
#Plot for p(reward)=0.4 Perceived Controllabliity
High_PC1plot<- data.frame(Half1_0.4,Half2_0.4)
names(High_PC1plot) <- c("Half 1","Half 2")
barplot(height=as.matrix(High_PC1plot), ylim=c(0,0.2),main="p(reward) = .4",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))
#Plot for p(reward)=1 Perceived Controllabliity
EHigh_PC1plot<- data.frame(Half1_1,Half2_1)
names(EHigh_PC1plot) <- c("Half 1","Half 2")
barplot(height=as.matrix(EHigh_PC1plot), ylim=c(0,0.2),main="p(reward) = 1",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))

dev.off()

# ACHIEVEMENT MOTIVATION ----------------------------------------------------
#Recode responses to numeric values
data_MQ[data_MQ=="Strongly Disagree"] <- 1
data_MQ[data_MQ=="Disagree"] <- 2
data_MQ[data_MQ=="Agree"] <- 3
data_MQ[data_MQ=="Strongly Agree"] <- 4

for(i in 1:10) {
  data_MQ[, 1+i] <- as.numeric(data_MQ[, 1+i])
} #convert to numeric

MQmeans<-rowMeans(data_MQ[,2:11])
data_MQ<-cbind(data_MQ,MQmeans)

#All data combined
alldata<-cbind(exp_dirpc,Vardata2[,-(1:2)],data_MQ[,-1])
View(alldata)


# Exploration Analysis ----------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
library(rstatix)
library(broom)

final_exp <- alldata[ , c(1:3 , 205:208,227)]
final_exp <- final_exp %>% gather(key = "Block", value = "explore", `Explo Block 1`, `Explo Block 2`, `Explo Block 3`, `Explo Block 4`) %>% convert_as_factor(ID,Block)
final_exp %>% sample_n_by(Control.Condition,Reward.Frequency,Block,size=1)

#convert IV to character
final_exp$Reward.Frequency<-as.character(final_exp$Reward.Frequency)
final_exp$Block <-as.character(final_exp$Block)

#summary statistics
#Group the data by reward frequency, control conditions, and block of trials: compute the exploration scores
summary <- final_exp %>%
  group_by(Reward.Frequency, Control.Condition, Block) %>%
  get_summary_stats(explore, type = "mean_sd")
View(summary)

###Assumption Check
#Homogeneity of regression slopes
final_exp %>%
  unite(col="group",Control.Condition,Reward.Frequency,Block) %>%
  anova_test(explore ~ group*MQmeans)

#Normality of residuals
summary(aov(explore~ MQmeans + Control.Condition*Reward.Frequency*Block,data=final_exp))
model_1 <- lm(explore~ MQmeans + Control.Condition*Reward.Frequency*Block,data=final_exp)
shapiro_test(resid(model_1)) #can't assume normality of residuals

#Computation
res.aov <- final_exp %>% 
  anova_test(explore ~ MQmeans + Control.Condition*Reward.Frequency*Block)
get_anova_table(res.aov)

#After adjustment for MQmeans, there wasn't a statistically significant three-way interaction. 


### post-hoc test
## Simple main effect for Reward Frequency
final_exp %>%
  group_by(Control.Condition,Block) %>%
  anova_test(explore ~ MQmeans+Reward.Frequency)

## Simple main effect for Control Condition
final_exp %>%
  group_by(Reward.Frequency,Block) %>%
  anova_test(explore ~ MQmeans+Control.Condition)

## Simple main effect for Block
final_exp %>%
  group_by(Reward.Frequency,Control.Condition) %>%
  anova_test(explore ~ MQmeans+Block)

# Indirect PC Analysis ------------------------------------------------------------
Half1var<-rowMeans(alldata[,213:214])
Half2var<-rowMeans(alldata[,215:216])
alldata<-cbind(alldata,Half1var,Half2var)
final_exp2 <- alldata[ , c(1:3,227:229)]
View(final_exp2)

final_exp2 <- final_exp2 %>% gather(key = "Half", value = "PCvar", `Half1var`, `Half2var`)%>% convert_as_factor(ID,Half)
final_exp2 %>% sample_n_by(Control.Condition,Reward.Frequency, Half,size=1)

#convert IV to character
final_exp2$Reward.Frequency<-as.character(final_exp2$Reward.Frequency)
final_exp2$Half <-as.character(final_exp2$Half)


#summary statistics
#Group the data by reward frequency, control conditions, and half of the task: compute the variance scores
summary2 <- final_exp2 %>%
  group_by(Reward.Frequency, Control.Condition, Half) %>%
  get_summary_stats(PCvar, type = "mean_sd")
View(summary2)

###Assumption Check
#Homogeneity of regression slopes
final_exp2 %>%
  unite(col="group",Control.Condition,Reward.Frequency,Half) %>%
  anova_test(PCvar ~ group*MQmeans)

#Normality of residuals
model_2 <- lm(PCvar~ MQmeans + Control.Condition*Reward.Frequency*Half,data=final_exp2)
shapiro_test(resid(model_2)) #can't assume normality of residuals

#Computation
res.aov2 <- final_exp2 %>% 
  anova_test(PCvar ~ MQmeans + Control.Condition*Reward.Frequency*Half)
get_anova_table(res.aov2)

#After adjustment for MQmeans, there wasn't a statistically significant three-way interaction between reward frequency, control condition, and half on the PCvar score, F(3, 101) = 2.292, p = 0.064. 

### post-hoc test
## Simple main effect for Reward Frequency
final_exp2 %>%
  group_by(Control.Condition,Half) %>%
  anova_test(PCvar ~ MQmeans+Reward.Frequency)

# Pairwise comparisons
pwc2 <- final_exp2 %>% 
  group_by(Control.Condition,Half) %>%
  emmeans_test(
    PCvar ~ Reward.Frequency, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2 %>% filter(Control.Condition == "CTRL" & Half == "Half2var")
pwc2 %>% filter(Control.Condition == "YOK" & Half == "Half2var")

## Simple main effect for Control Condition
final_exp2 %>%
  group_by(Reward.Frequency,Half) %>%
  anova_test(PCvar ~ MQmeans+Control.Condition)

# Pairwise comparisons
pwc2_1 <- final_exp2 %>% 
  group_by(Reward.Frequency,Half) %>%
  emmeans_test(
    PCvar ~ Control.Condition, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2_1 %>% filter(Reward.Frequency == "1" & Half == "Half2var")

## Simple main effect for Half
final_exp2 %>%
  group_by(Reward.Frequency,Control.Condition) %>%
  anova_test(PCvar ~ MQmeans+Half)

# Pairwise comparisons
pwc2_2 <- final_exp2 %>% 
  group_by(Reward.Frequency,Control.Condition) %>%
  emmeans_test(
    PCvar ~ Half, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2_2 %>% filter(Reward.Frequency == "1" & Control.Condition == "CTRL")


# Direct PC Analysis -------------------------------------------------------
Half1Direct<-rowMeans(alldata[,209:210])
Half2Direct<-rowMeans(alldata[,211:212])
alldata<-cbind(alldata,Half1Direct,Half2Direct)
final_exp3 <- alldata[ , c(1:3,227,230:231)]

final_exp3 <- final_exp3 %>% gather(key = "Half", value = "DP", `Half1Direct`, `Half2Direct`)%>% convert_as_factor(ID,Half)
final_exp3 %>% sample_n_by(Control.Condition,Reward.Frequency,Half,size=1)

#convert IV to character
final_exp3$Reward.Frequency<-as.character(final_exp3$Reward.Frequency)
final_exp3$Half <-as.character(final_exp3$Half)

#summary statistics
#Group the data by reward frequency, control conditions, and half of the task: compute the variance scores
summary3 <- final_exp3 %>%
  group_by(Reward.Frequency, Control.Condition, Half) %>%
  get_summary_stats(DP, type = "mean_sd")
View(summary3)

#Assumption Checks
#Homogeneity of regression slopes
final_exp3 %>%
  unite(col="group",Control.Condition,Reward.Frequency,Half) %>%
  anova_test(DP ~ group*MQmeans)

#Normality of residuals
model_3 <- lm(DP~ MQmeans + Control.Condition*Reward.Frequency*Half,data=final_exp3)
shapiro_test(resid(model_3)) 

#Computation
res.aov3 <- final_exp3 %>% 
  anova_test(DP ~ MQmeans + Control.Condition*Reward.Frequency*Half)
get_anova_table(res.aov3)

#After adjustment for MQmeans, there was a statistically significant three-way interaction between reward frequency, control condition, and half on the DP score, F(1, 109) = 3.814, p = 0.034. 

### post-hoc test
## Simple main effect for Reward Frequency
final_exp3 %>%
  group_by(Control.Condition,Half) %>%
  anova_test(DP ~ MQmeans+Reward.Frequency)

# Pairwise comparisons
pwc3 <- final_exp3 %>% 
  group_by(Control.Condition,Half) %>%
  emmeans_test(
    DP ~ Reward.Frequency, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3 %>% filter(Control.Condition == "CTRL" & Half == "Half1Direct")
pwc3 %>% filter(Control.Condition == "CTRL" & Half == "Half2Direct")
pwc3 %>% filter(Control.Condition == "YOK" & Half == "Half1Direct")
pwc3 %>% filter(Control.Condition == "YOK" & Half == "Half2Direct")

## Simple main effect for Control Condition
final_exp3 %>%
  group_by(Reward.Frequency,Half) %>%
  anova_test(DP ~ MQmeans+Control.Condition)

# Pairwise comparisons
pwc3_1 <- final_exp3 %>% 
  group_by(Reward.Frequency,Half) %>%
  emmeans_test(
    DP ~ Control.Condition, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3_1 %>% filter(Reward.Frequency == "1" & Half == "Half2Direct")

## Simple main effect for Half
final_exp3 %>%
  group_by(Reward.Frequency,Control.Condition) %>%
  anova_test(DP ~ MQmeans+Half)

# Pairwise comparisons
pwc3_2 <- final_exp3 %>% 
  group_by(Reward.Frequency,Control.Condition) %>%
  emmeans_test(
    DP ~ Half, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3_2 %>% filter(Reward.Frequency == "0.4" & Control.Condition == "CTRL")
pwc3_2 %>% filter(Reward.Frequency == "1" & Control.Condition == "CTRL")


# Correlations between DP and PC ------------------------------------------

library("ggpubr")
#Half1
ggscatter(alldata, x="Half1var", y="Half1Direct",
          add="reg.line",conf.int=TRUE,
          cor.coef=TRUE,cor.method="spearman",
          xlab="Half1 variances", ylab="Half1 DP scores")
#Half2
ggscatter(alldata, x="Half2var", y="Half2Direct",
          add="reg.line",conf.int=TRUE,
          cor.coef=TRUE,cor.method="spearman",
          xlab="Half2 variances", ylab="Half2 DP scores")

cor.test(alldata$Half1var,alldata$Half1Direct, method=c("pearson", "kendall", "spearman"))
cor.test(alldata$Half2var,alldata$Half2Direct, method=c("pearson", "kendall", "spearman"))


# Visualization -----------------------------------------------------------

library(ggplot2)
library(grid)
library(scales)
library(plotrix)

View(alldata)
#exploration rate plots

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/Exploration_plots_withSE.pdf",onefile = TRUE)

text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  i <- i + 1
  
  yoked_means <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,mean)
  yoked_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,std.error)
  
  ctrl_means <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,mean)
  ctrl_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('Yoked',4))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
  lineplot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(
    ggplot(lineplot_df,mapping = aes(y = means,x = block,group=group)) +
      geom_line(aes(colour=group)) +
      geom_point() + 
      theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
            legend.key.height = unit(0.8, 'mm'), #change legend key height
            legend.key.width = unit(3, 'mm'), #change legend key width
            legend.title = element_text(size=10), #change legend title font size
            legend.text = element_text(size=10)) + #change legend text font size 
      geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.2) +
      scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,1)) + 
      theme_classic() +
      scale_color_manual(values = c('darkgrey','darkred')) +
      labs(y = 'Exploration Rate', x = 'Block') + 
      geom_vline(xintercept=2.5,linetype=11) +
      ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
      theme(plot.title = element_text(hjust = 0.5))
  )
} 

dev.off()

#1.Low motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/Exploration_plots_Lmotivation.pdf",onefile = TRUE)

text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  i <- i + 1
  
  yoked_means <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5 ),],2,mean)
  yoked_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,std.error)
  
  ctrl_means <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,mean)
  ctrl_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('Yoked',4))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
  lineplot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
 print(
    ggplot(lineplot_df,mapping = aes(y = means,x = block,group=group)) +
      geom_line(aes(colour=group)) +
      geom_point() + 
      theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
            legend.key.height = unit(0.8, 'mm'), #change legend key height
            legend.key.width = unit(3, 'mm'), #change legend key width
            legend.title = element_text(size=10), #change legend title font size
            legend.text = element_text(size=10)) + #change legend text font size 
      geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.2) +
      scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,1)) + 
      theme_classic() +
      scale_color_manual(values = c('darkgrey','darkred')) +
      labs(y = 'Exploration Rate', x = 'Block') + 
      geom_vline(xintercept=2.5,linetype=11) +
      ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
      theme(plot.title = element_text(hjust = 0.5))
  )
} 
dev.off()



#2.High motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/Exploration_plots_Hmotivation.pdf",onefile = TRUE)
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  i <- i + 1
  
  yoked_means <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5 ),],2,mean)
  yoked_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  ctrl_means <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,mean)
  ctrl_errors <- apply(alldata[205:208][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('Yoked',4))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
  lineplot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(
    ggplot(lineplot_df,mapping = aes(y = means,x = block,group=group)) +
      geom_line(aes(colour=group)) +
      geom_point() + 
      theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
            legend.key.height = unit(0.8, 'mm'), #change legend key height
            legend.key.width = unit(3, 'mm'), #change legend key width
            legend.title = element_text(size=10), #change legend title font size
            legend.text = element_text(size=10)) + #change legend text font size 
      geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.2) +
      scale_y_continuous(labels = percent_format(accuracy = 1),limits = c(0,1)) + 
      theme_classic() +
      scale_color_manual(values = c('darkgrey','darkred')) +
      labs(y = 'Exploration Rate', x = 'Block') + 
      geom_vline(xintercept=2.5,linetype=11) +
      ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
      theme(plot.title = element_text(hjust = 0.5))
  )
} 
dev.off()

### bar plot direct ###

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/DirectPC_plots_withSE.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,mean)
  yoked_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,std.error)
  
  ctrl_means <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,mean)
  ctrl_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,std.error)

  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  plot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(plot_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          labs(y = 'Rating', x = 'Half') +
          ylim(0,10) +
          scale_x_continuous(breaks=1:2)+
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()

#1. Low motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/DirectPC_plots_Lmotivation.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5 ),],2,mean)
  yoked_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5 ),],2,std.error)
  
  ctrl_means <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5 ),],2,mean)
  ctrl_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5 ),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  plot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(plot_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          labs(y = 'Rating', x = 'Half') +
          ylim(0,10) +
          scale_x_continuous(breaks=1:2)+
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()

#2. High motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/DirectPC_plots_Hmotivation.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5 ),],2,mean)
  yoked_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  ctrl_means <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,mean)
  ctrl_errors <- apply(alldata[230:231][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  plot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(plot_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          labs(y = 'Rating', x = 'Half') +
          ylim(0,10) +
          scale_x_continuous(breaks=1:2)+
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()

### bar plot pc1 ###

pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/PC1_plots_.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,mean)
  yoked_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p),],2,std.error)
  
  ctrl_means <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,mean)
  ctrl_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  indirect_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(indirect_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          labs(y = 'Variance', x = 'Half') +
          ylim(0,0.3)+
          scale_x_continuous(breaks=1:2)+
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()

#1. Low motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/PC1_plots_Lmotivation.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,mean)
  yoked_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,std.error)
  
  ctrl_means <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,mean)
  ctrl_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans < 2.5),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  indirect_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(indirect_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          labs(y = 'Variance', x = 'Half') +
          ylim(0,0.3)+
          scale_x_continuous(breaks=1:2)+
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()

#2. High motivation
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/PC1_plots_Hmotivation.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0

for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,mean)
  yoked_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="YOK" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  ctrl_means <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,mean)
  ctrl_errors <- apply(alldata[228:229][which(alldata$Control.Condition=="CTRL" & alldata$Reward.Frequency ==p & alldata$MQmeans > 2.5),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:2,'group'=rep('Yoked',2))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:2,'group'=rep('With control',2))
  indirect_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(ggplot(indirect_df,aes(x=block,y=means,fill=group)) +
          geom_bar(stat="identity",position=position_dodge(),colour='black') +
          theme_classic() + 
          theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
                legend.key.height = unit(0.8, 'mm'), #change legend key height
                legend.key.width = unit(3, 'mm'), #change legend key width
                legend.title = element_text(size=10), #change legend title font size
                legend.text = element_text(size=10)) + #change legend text font size 
          labs(y = 'Variance', x = 'Half') +
          ylim(0,0.3)+
          scale_x_continuous(breaks=1:2)+
          geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.3, position = position_dodge(0.9)) +
          scale_fill_manual(values = c('darkgrey','darkred')) +
          ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
          theme(plot.title = element_text(hjust = 0.5))
  )  
}
dev.off()


# Time_clicks -------------------------------------------------------------

timepath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/times_clicks" 
data_time <- ldply(list.files(path=timepath, full.names=TRUE), read.csv, header=TRUE)
data_time<- data_time[which((data_time$ID %in% data_MQ$ID)==TRUE),] #remove participantsexppath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/exploration" 
View(data_time)

timeclicks <- data.frame()
indexing <- list(a = seq(from=4,to=27,by=1), b = seq(from=28,to=52), c = seq(from=53,to=77), d = seq(from=78,to=102))
for(i in 1:nrow(data_time)) { 
  
  for(j in 1:4){ 
    timeclicks[i, j] <- mean(as.numeric(data_time[i, indexing[[j]]]))
  } 
  
}
colnames(timeclicks) <- c(paste("Block", 1:4))
data_time<- cbind(data_time, timeclicks)


#Plots
pdf(file = "/Users/ASUS/Documents/3rd Year/Thesis Writing/Plots/Time_clicks.pdf")
text <- c('Extremely Low','Moderate','High','Extremely high'); i <- 0
for(p in c(0.1,0.2,0.4,1)){
  
  i <- i + 1
  
  yoked_means <- apply(data_time[103:106][which(data_time$Control.Condition=="YOK" & data_time$Reward.Frequency ==p),],2,mean)
  yoked_errors <- apply(data_time[103:106][which(data_time$Control.Condition=="YOK" & data_time$Reward.Frequency ==p),],2,std.error)
  
  ctrl_means <- apply(data_time[103:106][which(data_time$Control.Condition=="CTRL" & data_time$Reward.Frequency ==p),],2,mean)
  ctrl_errors <- apply(data_time[103:106][which(data_time$Control.Condition=="CTRL" & data_time$Reward.Frequency ==p),],2,std.error)
  
  yoked <- data.frame('means'=yoked_means,'errors'=yoked_errors,'block'=1:4,'group'=rep('Yoked',4))
  control <- data.frame('means'=ctrl_means,'errors'=ctrl_errors,'block'=1:4,'group'=rep('With control',4))
  lineplot_df <- rbind(yoked,control,make.row.names=F) ; rm(yoked_errors,yoked_means,ctrl_errors,ctrl_means,yoked,control)
  
  print(
    ggplot(lineplot_df,mapping = aes(y = means,x = block,group=group)) +
      geom_line(aes(colour=group)) +
      geom_point() + 
      theme(legend.key.size = unit(0.8, 'mm'), #change legend key size
            legend.key.height = unit(0.8, 'mm'), #change legend key height
            legend.key.width = unit(3, 'mm'), #change legend key width
            legend.title = element_text(size=10), #change legend title font size
            legend.text = element_text(size=10)) + #change legend text font size 
      geom_errorbar(aes(ymax=means+errors,ymin=means-errors),width=0.2) +
      theme_classic() +
      scale_color_manual(values = c('darkgrey','darkred')) +
      labs(y = 'Time_Clicks', x = 'Block') + 
      ylim(0,3.0) +
      geom_vline(xintercept=2.5,linetype=11) +
      ggtitle(paste0(text[i],' \n P(reward) = ',p)) +
      theme(plot.title = element_text(hjust = 0.5))
  )
} 
dev.off()


# Descriptive for motivation ---------------------------------------------------------------------

#Low motivation
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")

Lowmotiv <- data.frame()

for(j in 1:4) {
  for(i in 1:2) {
    Lowmotiv[i, j] <- sum(alldata[, 2] == ps[j] & alldata[, 3] == cond[i] & alldata$MQmeans < 2.5)
    
  }
}
colnames(Lowmotiv) <- c(ps)
rownames(Lowmotiv) <- c(cond)
sumCTRL_l<-sum(Lowmotiv[1,])
sumYOK_l<-sum(Lowmotiv[2,])
Sum_l<-rbind(sumCTRL_l,sumYOK_l)
tcolsum_l<-colSums(Lowmotiv)
Lowmotiv<-cbind(Lowmotiv,Sum_l)
Lowmotiv<-rbind(Lowmotiv,Colsum=tcolsum_l)
View(Lowmotiv)

mean(alldata$MQmeans[alldata$MQmeans<2.5])
sd(alldata$MQmeans[alldata$MQmeans<2.5])

#High motivation
ps <- c(0.1, 0.2, 0.4, 1)
cond <- c("CTRL", "YOK")

Highmotiv <- data.frame()

for(j in 1:4) {
  for(i in 1:2) {
    Highmotiv[i, j] <- sum(alldata[, 2] == ps[j] & alldata[, 3] == cond[i] & alldata$MQmeans > 2.5)
    
  }
}
colnames(Highmotiv) <- c(ps)
rownames(Highmotiv) <- c(cond)
sumCTRL_H<-sum(Highmotiv[1,])
sumYOK_H<-sum(Highmotiv[2,])
Sum_H<-rbind(sumCTRL_H,sumYOK_H)
tcolsum_H<-colSums(Highmotiv)
Highmotiv<-cbind(Highmotiv,Sum_H)
Highmotiv<-rbind(Highmotiv,Colsum=tcolsum_H)
View(Highmotiv)

mean(alldata$MQmeans[alldata$MQmeans>2.5])
sd(alldata$MQmeans[alldata$MQmeans>2.5])