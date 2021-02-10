library(readxl)
library(plyr)
library(ggplot2)
library(ggthemes)
library(ggRandomForests)
library(randomForestSRC)
library(ggsci)
setwd("")
rt <- read.table(".txt", header = T)
set.seed(2020)
trainIndex <- sample(1:nrow(rt), nrow(rt)*7/10)
Train <- rt[trainIndex, ]
###Randomforest Survival Model###
#Train group
data1 <- Train[,-1]
data1$Group <- 1
#Fit Randomforest Survival Model
trees <- rfsrc(Surv(time, recurrence)~., data=data1, ntree=1000)
#Assign 24 months of routine follow-up duration
times <- c(1:24) 
#Get covariate-adjusted randomforest survival probability stratified by train group
getRFSprob1 <- function(x){
  n <- plot.variable(trees, surv.type="surv", time=x, show.plots=F)
  yhat <- n$yhat
  yhat[is.na(yhat)] <- 100
  m <- cbind(n$xvar$Group, yhat)
  colnames(m) <- c("Group", "yhat")
  sur <- aggregate(yhat~Group, data=m, FUN="mean")
  return(sur)
}
surv.prob1 <- adply(times, 1, getRFSprob1)
colnames(surv.prob1) <- c("Month", "Group", "Survival")
surv.prob1 <- surv.prob1[, c("Survival", "Month", "Group")]
surv.prob1$Month <- as.numeric(surv.prob1$Month)
surv.prob1 <- surv.prob1[order(surv.prob1[, "Group"]), ]
surv.prob1[, "Group"] <- as.factor(surv.prob1[, "Group"])
write.csv(surv.prob1, "\\surv.prob.Train.csv")
#######Validation group#################
data2 <- Validation[,-1]
data2$Group <- 2
#Fit Randomforest Survival Model
trees <- rfsrc(Surv(time, recurrence)~., data=data2, ntree=1000)
#Assign 24 months of routine follow-up duration
times <- c(1:24) 
#Get covariate-adjusted randomforest survival probability stratified by validation group
getRFSprob2 <- function(x){
  n <- plot.variable(trees, surv.type="surv", time=x, show.plots=F)
  yhat <- n$yhat
  yhat[is.na(yhat)] <- 100
  m <- cbind(n$xvar$Group, yhat)
  colnames(m) <- c("Group", "yhat")
  sur <- aggregate(yhat~Group, data=m, FUN="mean")
  return(sur)
}
surv.prob2 <- adply(times, 1, getRFSprob2)
colnames(surv.prob2) <- c("Month", "Group", "Survival")
surv.prob2 <- surv.prob2[, c("Survival", "Month", "Group")]
surv.prob2$Month <- as.numeric(surv.prob2$Month)
surv.prob2 <- surv.prob2[order(surv.prob2[, "Group"]), ]
surv.prob2[, "Group"] <- as.factor(surv.prob2[, "Group"])
write.csv(surv.prob2, "\\surv.prob.Validation.csv")
#######test group#################
test <- read.table(".txt", header = T)
data3 <- test[,c(-1,-2)]
data3$Group <- 3
#Fit Randomforest Survival Model
trees <- rfsrc(Surv(time, recurrence)~., data=data3, ntree=1000)
#Assign 24 months of routine follow-up duration
times <- c(1:24) 
#Get covariate-adjusted randomforest survival probability stratified by test group
getRFSprob3 <- function(x){
  n <- plot.variable(trees, surv.type="surv", time=x, show.plots=F)
  yhat <- n$yhat
  yhat[is.na(yhat)] <- 100
  m <- cbind(n$xvar$Group, yhat)
  colnames(m) <- c("Group", "yhat")
  sur <- aggregate(yhat~Group, data=m, FUN="mean")
  return(sur)
}
surv.prob3 <- adply(times, 1, getRFSprob3)
colnames(surv.prob3) <- c("Month", "Group", "Survival")
surv.prob3 <- surv.prob3[, c("Survival", "Month", "Group")]
surv.prob3$Month <- as.numeric(surv.prob3$Month)
surv.prob3 <- surv.prob3[order(surv.prob3[, "Group"]), ]
surv.prob3[, "Group"] <- as.factor(surv.prob3[, "Group"])
write.csv(surv.prob3, "\\surv.prob.Test.csv")
#Plot cumulative risk curves
graph1 <- ggplot(data=surv.prob, aes(x=Month, y=100-Survival))+
  geom_line(aes(colour=Group), size=1.3)+
  scale_x_continuous(expand=c(0, 0), breaks=seq(0,24,3),labels=seq(0,24,3))+
  xlab("Month")+ylab("Cumulative risk (%)")+
  theme_bw()+
  theme(plot.title=element_text(size=22, face="bold"), 
        panel.grid.major.y=element_line(linetype=1), 
        panel.grid.minor=element_line(linetype=2), 
        panel.border=element_blank(), 
        axis.line=element_line(color="black", size=1), 
        axis.text=element_text(size=14, face="bold"), 
        axis.title=element_text(size=16, face="bold"), 
        legend.text=element_text(size=12, face="bold"), 
        legend.title=(element_text(size=14, face="bold")))+
  theme(legend.position = 'top')
graph1+ggsci::scale_color_jco()
F1<- graph1+ggsci::scale_color_jco()
#Get monthly probabilities of interested events 
getMonthlyRisk <- function(x){
  prob <- subset(surv.prob, Group==x)
  latitude <- data.frame(Month=c(1:24), Rate=c(1:24))
  for(i in 1:23){
    latitude[1, 2] <-100-prob[1, 1]
    a <- prob[which(prob$`Month`==i), 1]
    b <- prob[which(prob$`Month`==i+1), 1]
    latitude[i+1, 2] <- a-b
  }
  return(latitude)
}
MonthlyRisk <- adply(as.numeric(unique(surv.prob$Group)), 1, getMonthlyRisk)
colnames(MonthlyRisk) <- c("Group", "Month", "Rate")
write.csv(MonthlyRisk, "\\Monthlyrisk.csv")
#Plot monthly probabilities of interested events
names <- list(     
  '1'="Train", 
  '2'="Validation",
  '3'="Test")        
labeller <- function(variable, value){return(names[value])}
graph2 <- ggplot(MonthlyRisk, aes(x=Month, y=Rate))+
  geom_point(size=1.2, color="#EFC000FF")+
  geom_line(alpha=1, color="#0073C2FF", size=1)+
  geom_line(stat="smooth", method="loess", se=FALSE, color="#CD534CFF", alpha=0.8, size=1.2)+
  scale_x_continuous(breaks=seq(0,24,3))+
  facet_wrap(~Group, ncol=1, labeller=labeller)+
  ylab("Risk Probability (%)")+
  theme_bw()+
  theme(panel.grid.major.y=element_line(linetype=2), panel.grid.minor=element_blank(), panel.border=element_blank(), axis.line=element_line(color="black", size=1), axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14, face="bold"), legend.text=element_text(size=12, face="bold"), legend.title=element_text(size=14, face="bold"), strip.text.x=element_text(size=18, face="bold"), strip.background=element_blank())
graph2+ggsci::scale_color_jco()
F2 <- graph2+ggsci::scale_color_jco()
#Distributed follow-up times to each month based on monthly probabilities of interested events
followup <- data.frame(matrix(NA, 24, 6))
for(number.of.follow.up in 4:12){
  for(i in as.numeric(unique(MonthlyRisk$Group))){
    rate <- subset(MonthlyRisk, Group==i)
    followup[, 3*(i-1)+1] <- rate$Rate/sum(rate$Rate, na.rm=TRUE)
    followup[, 3*(i-1)+2] <- followup[, 3*(i-1)+1]*number.of.follow.up
    for(j in 1:24){
      followup[j, 3*(i-1)+3] <- sum(followup[1:j, 3*(i-1)+2], na.rm=TRUE)
    }
    colnames(followup) <- c("Group Train probability per month",
                            "Group Train follow-up times per month",
                            "Group Train follow-up cumulative times",
                            "Group Validation probability per month",
                            "Group Validation follow-up times per month",
                            "Group Validation follow-up cumulative times")
    }
  write.csv(followup, file=paste0("number.of.follow.up=", number.of.follow.up, ".csv"))
}

