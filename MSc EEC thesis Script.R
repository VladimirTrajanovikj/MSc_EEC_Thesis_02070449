
############Setting the working directory#######################################
rm(list=ls())
setwd("/Users/vladimirtrajanovikj/Desktop/Masters 2021-2022/MSc Project/data")
########Getting the necessary packages########################################
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggiraphExtra)
library(ggeffects)
library(lme4)
library(moments) 
library(hrbrthemes)
library(cowplot)
library(ggpubr)
library(data.table)



#####Importing the data from Claudia Wyer for replicates 1 and 2################

cd<-fread("R1and2.txt",header=T,stringsAsFactors = T)

#Fathers are  denoted as P sons are F###########################################

  
#####Importing the data for replicate 3 of sons and fathers####################
  
  R3<-fread("R3.txt",header = T)
  
#######Deleting unnecessary columns#############################################

  R3SF<-subset(R3, select = -c(V10,female_5,V12,female_6,V14,female_7,
                               V16,female_8,V18,female_1,V4,female_3,V8,female_4,female_2,V6) )

  
#######Deleting unnecessary rows###############################################

  R3SF<-R3SF[-c(1,18,21), ]
#########Renaming##############################################################

  names(R3SF)[names(R3SF)=="Nofemales"]<-"P_Females"
  names(R3SF)[names(R3SF)=="F1_Females"]<-"F_Females"

#####Checking for data structure################################################  
  str(R3SF)
  
#Removing redundant columns#####################################################
  
  R3FS<-R3SF %>% 
    select(unique(colnames(.)))
 
#####Checking for data structure################################################   
  str(R3FS)
  
#Converting the column of females from character to integer#####################
  R3FS$P_Females=as.integer(R3FS$P_Females)

#########Importing the data for replicate 4 of sons and fathers##################

R4<-fread("R4.txt",header = T)
R_4<-R4[-c(1)]  

  

#####Concatenating all of the separate data sets for replicates 1,2,3,4 into one data set#
  
  H<-rbind(cd,R3FS,R_4)

###Data Structure check#
  str(H)
  
 
  
####Renaming some columns #####################################################
  
names(H)[names(H)=="P1_scores"]<-"P_scores"
names(H)[names(H)=="P1_proportions"]<-"P_proportions"
names(H)[names(H)=="F1_scores"]<-"F_scores"
names(H)[names(H)=="F1_proportions"]<-"F_proportions"
names(H)[names(H)=="P1_status"]<-"P_status"
names(H)[names(H)=="F1_status"]<-"F_status"


#########Checking for data structure and changing the female column to integer##
str(H)

H$P_Females=as.integer(H$P_Females)



###Calculating proportions in a  better way in R and adding them in a data frame 
h3<-mutate(H,F_proportions=F_scores/F_Females)
h3$P_scores=as.integer(h3$P_scores)
h4<-mutate(h3,P_proportions=P_scores/P_Females)

#Data structure check####

str(h4)

####FIRST COMPONENT OF ANALYSIS OF MATING DISTRIBUTIONS for all replicates######

###Importing the data file that I arranged to be easily analysed for the Mann-Whitney#

mw<-fread("MannWhitney_PF.txt",header=T,stringsAsFactors = T)

####Mann Whitney test to compare the fathers and sons mating proportion scores##

pf<-wilcox.test(mw$MatingScore~mw$Generation,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,na.rm=T)



##Checking how many unmated fathers and sons there are#########################
sum(h4$F_proportions==0,na.rm = T)
sum(h4$P_proportions==0,na.rm=T)

######Checking how many fathers and sons mated only once########################
sum(h4$P_proportions==0.125,na.rm=T)
sum(h4$F_proportions==0.125,na.rm=T)

######Checking how many fathers and sons mated more than once###################

sum(h4$F_proportions>0.125,na.rm=T)
sum(h4$P_proportions>0.125,na.rm=T)
######Checking how many fathers and sons scored 0.5###################

sum(h4$P_proportions==0.5,na.rm=T)
sum(h4$F_proportions==0.5,na.rm=T)

######Plotting mating distributions of fathers and sons #########################
  

b<-ggplot(h4, aes(x=P_proportions)) +
  geom_histogram(position="dodge",bins=10)+theme_classic()+scale_x_continuous(breaks=seq(0,0.6,0.1))+labs(x="Fathers(P)proportional mating scores")

b2<-ggplot(h4, aes(x=F_proportions)) +
  geom_histogram(position="dodge",bins=10)+theme_classic()+scale_x_continuous(breaks = seq(0, 0.6, 0.1))+labs(x="Sons(F) proportional mating scores")


plot_grid(b,b2,labels='AUTO')
  
  
#######Kolmogorov-Smirnov Test to check for normality of the mating distribution#

#Fathers#  
p<-ks.test(h4$P_proportions,"pnorm")
#Sons#
f<-ks.test(h4$F_proportions,"pnorm")



#####Checking for Skewness for the mating distribution of fathers and sons######
  
skewness(h4$F_proportions,na.rm=T)
skewness(h4$P_proportions,na.rm=T)


#########Summary statistics for the distributions##############################
  
pm<-mean(h4$P_proportions,na.rm=T)
fm<-mean(h4$F_proportions,na.rm=T)

 
mp<-median(h4$P_proportions,na.rm=T)
mf<-median(h4$F_proportions,na.rm=T)


######Simulations for the random and reduced competitive ability of the Thai##

#Adding weights that show the probability of mating where the Thai has 12.5%#
my_prob=c(0.125,0.4,0.4,0.4,0.4,0.4,0.4,0.4)

#Parameters for the simulation
ncage <- 1000
female<-8

#Empty vector to store the outputs
male <- c()
female_id <- c()
cage_id <- c()

##############################Simulation#######################################

###NOTE: if you want to simulate the mating distribution of random chance 
###remove the prob=my_prob from the code. Otherwise for the reduced competitive
##ability keep it in the code######

for(cage in 1:ncage){
  for(female in 1:female){
    male_temp <- sample(x=c("wt","red","red","red","red","red","red","red"),replace = F, size = 2,prob=my_prob)
    male <- c(male, male_temp)
    cage_id <- c(cage_id, cage)
    female_id <- c(female_id,female)
  }
}
#Merge the results into a data frame
simulated_matings <- data.frame(cage_id = cage_id,
                                female_id = female_id,
                                successful_male = male)

cage_no <- c()
score_wt <- c()
for(cage in 1:ncage){
  temp_df <- subset(simulated_matings, cage_id == cage)
  count_wt <- length(which(temp_df$successful_male == "wt"))
  
  cage_no <- c(cage_no, cage)
  score_wt <- c(score_wt, count_wt)
  
}

scores_simulation <- data.frame(cage_id = cage_no,
                                score_wt = score_wt)

scores_simulation$prop <- scores_simulation$score_wt/female

######Plotting the distribution from the simulation##########################################

hist_1<- ggplot(scores_simulation,aes(x=prop))+
  geom_histogram(bins=15)+theme_classic()+xlab("Thai Proportional mating score")
print(hist_1)


####Importing the File containing the  distribution of 12.5% chance of thai mating#

#### NOTE: I previously exported the simulated results from the above simulation
###and made a new file in excel and saved it as txt where I also  placed the score from the experimental data.
##This makes it easier in terms of structure for analysis for the Mann-Whitney test##############

sd125<-fread("SD125.txt",header = T,stringsAsFactors = T)


####Importing the File containing the  distribution of 25% (random) chance of thai mating##

#### NOTE: I previously exported the simulated results from the above simulation
###and made a new file in excel and saved it as txt where I also  placed the score from the experimental data.
##This makes it easier in terms of structure for analysis for the Mann-Whitney test##############
sd25<-fread("SD25.txt",header = T,stringsAsFactors = T)



######Mann-Whitney for 25% chance of mating####################################
wt25<-wilcox.test(sd25$Wt_prop~sd25$Data_type,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,na.rm=T)


######Mann-Whitney for 12.5% chance of mating####################################

wt125<-wilcox.test(sd125$Wt_prop~sd125$Data_type,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,na.rm=T)


#######################################################################

####Calculating how many unmated in the simulated and experimental###

###For 12.5% chance####

nrow(filter(sd125,Data_type == "Simulated" & Wt_prop == "0"))
nrow(filter(sd125,Data_type == "Experimental" & Wt_prop == "0"))

###For 25% chance####

nrow(filter(sd25,Data_type == "Simulated" & Wt_proportion == "0"))
nrow(filter(sd25,Data_type == "Experimental" & Wt_proportion == "0"))

###Removing NAs  to check that in total there should be 285 for
##experimental and 1000 for simulated######

sd125_2<-na.omit(sd125)
sd25_2<-na.omit(sd25)
sd125_2%>%group_by(Data_type)%>%tally()
sd25_2%>%group_by(Data_type)%>%tally()

#####Heritability Component Analysis###########################################

##########THE BINOMIAL GLMM############

  
#Binomial generalized linear model to see if the  fathers score predicts the sons score#

mm2<-glmer(F_proportions~P_proportions+(1|replicate),data=h4,weights=F_Females,family="binomial")
summary(mm2)

###########BINARY GLMM############

#Whether the  father' mating status predicts sons mating status 

#Here I convert the mated/unmated to 0 and 1 for easier analysis##############
##0 is unmated and 1 is mated###

H1<-h4%>%mutate(F_status = ifelse(F_status=="mated", 1, 0))
H2<-H1%>%mutate(P_status = ifelse(P_status=="mated", 1, 0))
H3<-na.omit(H2)
str(H2)


####Changing the reference category from unmated to mated for interpretation###
h4$P_status=relevel(factor(h4$P_status),ref="unmated")
str(h4)  

bm<-glmer(as.factor(F_status)~as.factor(P_status)+(1|replicate),data=H3,family="binomial")

summary(bm)

##Plotting the model to analyse it#####
plot(ggpredict(bm))


############Plots for heritability #################################################

####Plot  For Binary Data #######

bp<-ggplot(h4, aes(x=P_status, y=F_proportions, fill=P_status)) + 
  geom_boxplot()

bp+labs(
  x = "P status",
  y = "F proportional mating scores"
) +
  theme_classic()

#Removing the NA's in the data for easier plotting of the second plot for 
#heritability

h5<-na.omit(h4)
str(h5)


###Plotting the Linear plot for heritability##############

plot1<-ggplot(h5, aes(x=P_proportions,y=F_proportions)) +
  geom_point(position=position_jitter(h=0.1, w=0.1),
             shape = 21, size = 1,col="brown2",fill="black") +
  theme_classic()+geom_smooth(method="lm")+stat_regline_equation(label.x = 0.4,label.y=0.5)+
  labs(x="Fathers(P)proportional mating scores",y="Sons(F)proportional mating scores")









