# Project 2 

library(dplyr)
library(ggplot2)
library(plyr)
library(scales)

# Here is the code used to filter the dataset:


setwd("C:/Users/Palma/Desktop/MS STATS/Year2 Sem1/STA 660 Data Practicum")

sdata <- read.csv("Climate Raw Data Coded Perp.csv")
sdata <- as.data.frame(sdata)
sdata <- sdata[sdata$Q189==1 & !is.na(sdata$Q189),] #only Oxford Campus
sdata <- sdata[sdata$Year<6 & !is.na(sdata$Year),] #only include undergraduates
sdata <- sdata[sdata$studentstatus==1 & !is.na(sdata$studentstatus),] #fulltime student
sdata <- sdata[sdata$Gender==0 | sdata$Gender==1 ,]#Only include Males and Females
sdata <- sdata[!is.na(sdata$Gender),]  #remove two blanks

#Remove Observations that were Non-Victims but left questions blank
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_1_SV_V1_1),0,sdata$Victim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_1_SV_V1_2),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_1_SV_V1_3),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_1_SV_V1_4),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_1_SV_V1_5),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_2_SV_V2_1),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_2_SV_V2_2),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_2_SV_V2_3),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_2_SV_V2_4),0,sdata$NonVictim)
sdata$NonVictim <- ifelse(sdata$Victim==1 & is.na(sdata$SV_V_2_SV_V2_5),0,sdata$NonVictim)
sdata <- sdata[sdata$NonVictim>0,]

  
length(sdata$Victim) #number of remaining obs
  
sdata$Gender <- as.character(ifelse(sdata$Gender==0, "Female", "Male"))
table(sdata$Gender) 
  
100*table(sdata$Victim)/length(sdata$Victim) 
100*table(sdata$Victim[sdata$Gender=="Female"])/length(sdata$Victim[sdata$Gender=="Female"])
100*table(sdata$Victim[sdata$Gender=="Male"])/length(sdata$Victim[sdata$Gender=="Male"])



# Subsetting data to only variables of interest.
subset <- sdata %>% select(Q189, Gender, SO, ethnicity, Race, Year, 
                           studentstatus, Age, Q207, V_SR_B1,SV_V_gend, SV_V_rel,
                           SV_V_alc1, SV_V_alc2, Q194, FSH_Status, SSH_stat1, StklV_gend,
                           SV_P_gen, SV_P_rel, SV_P_alc1, SV_P_alc2, SV_Perp1_SV_Perp1_1,
                           SV_Perp1_SV_Perp1_2, SV_Perp1_SV_Perp1_3, SV_Perp1_SV_Perp1_4, 
                           SV_Perp1_SV_Perp1_5, Victim, NonVictim)


#################################################
## Renaming factors from numeric to string ######
#################################################
subset$Gender <-factor(subset$Gender)
subset$Q189 <- factor(subset$Q189,levels = c(1,2,3),labels = c("Oxford", "Hamilton", "Midtown"))
subset$SO <-factor(subset$SO)
subset$SO <- factor(subset$SO,levels = c(1,2,3,4,5,6,7,11,12),
                    labels = c("Gay", "Lesbian", "Bisexual","Asexual", "Straight", "Queer",
                               "Questioning", "Not Listed", "Prefer not to answer"))
subset$SO.Coll <- subset$SO
# This code collapses groups
levels(subset$SO.Coll) <- list(Straight="Straight", 
                            Other=c("Gay", "Lesbian", "Bisexual","Asexual", "Queer",
                                    "Questioning", "Not Listed", "Prefer not to answer"))
subset$ethnicity <-factor(subset$ethnicity)
subset$ethnicity <- factor(subset$ethnicity,levels = c(1,2),
                    labels = c("Hispanic", "Not Hispanic"))
subset$Race <-factor(subset$Race)
subset$Race <- factor(subset$Race,levels = c(1,2,3,4,5,7),
                    labels = c("Black", "White", "Asian","Pacific", 
                               "Native American","Not Listed"))
subset$Race.Coll <- subset$Race
# This code collapses groups
levels(subset$Race.Coll) <- list(White="White", 
                            Other=c("Black", "Asian", "Pacific", "Native American","Not Listed"))
subset$Year <-factor(subset$Year)
subset$Year <- factor(subset$Year,levels = c(1,2,3,4,5,6,7,8),
                      labels = c("Freshman", "Sophomore", "Junior","Senior", 
                                 "Super Senior","Grad Student","Non-Matriculated", "Other" ))
# This code collapses groups
subset$Year.Coll <- subset$Year
levels(subset$Year.Coll) <- list(Freshman="Freshman", Sophomore="Sophomore", Junior="Junior",Senior="Senior",
                            Other=c("Super Senior","Grad Student","Non-Matriculated"))
subset$studentstatus <-factor(subset$studentstatus)
subset$studentstatus <- factor(subset$studentstatus,levels = c(1,2,3),
                           labels = c("Full-Time", "Part-Time","Not taking classes"))
subset$Q207 <-factor(subset$Q207)
subset$Q207 <- factor(subset$Q207,levels = c(1,3),
                               labels = c("Yes", "No"))
subset$SV_V_gend <-factor(subset$SV_V_gend)
subset$SV_V_gend <- factor(subset$SV_V_gend,levels = c(0,1,2),
                      labels = c("Woman", "Man", "Other"))
subset$SV_V_rel <-factor(subset$SV_V_rel)
subset$SV_V_rel <- factor(subset$SV_V_rel,levels = c(1,2,3,4,5,6,7),
                           labels = c("Stranger", "Acquaintance", "Friend", "Rom. Partner",
                                      "Ex Rom. Partner", "Relative", "Faculty"))
subset$SV_P_rel <-factor(subset$SV_P_rel)
subset$SV_P_rel <- factor(subset$SV_P_rel,levels = c(1,2,3,4,5,6,7),
                          labels = c("Stranger", "Acquaintance", "Friend", "Rom. Partner",
                                     "Ex Rom. Partner", "Relative", "Faculty"))
subset$Victim <-factor(subset$Victim)
subset$Victim <- factor(subset$Victim,levels = c(1,2,3,4,5,6),
                          labels = c("Non-Victim", "Unw. Touching", "Att. Coercion",
                                     "Coercion", "Att. Rape", "Rape"))

subset$Victim.Group <- subset$Victim
levels(subset$Victim.Group) <- list( "Non-Victim" ="Non-Victim", Rape="Rape", 
                                    "Non-Rape Victim"=c("Unw. Touching", "Att. Coercion",
                                    "Coercion", "Att. Rape"))


#############################################################
#######################  Graphs  ############################
#############################################################

################ GENDER ####################
# All victim groups
gen.vict <- as.data.frame(table(subset$Gender, subset$Victim))
gen.vict$Rel.Freq <- gen.vict$Freq/length(subset$Gender)
ggplot()+
  geom_bar(aes(x=Var2, y=Rel.Freq, fill=Var1), stat="identity", position="dodge",data=gen.vict) +
  xlab("Descriptor") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Gender") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) +
  scale_fill_manual(values=c("#F0B1EE", "lightblue")) +
  theme(legend.position=c(.8, .85)) +
  scale_y_continuous(labels=percent)


qplot(Victim, data=subset, geom="bar", fill=Gender)
ggplot(subset, aes(Victim, fill=Gender)) + geom_bar(position="dodge") 

# Collapse victim groups
gen.vict2 <- as.data.frame(table(subset$Gender, subset$Victim.Group))
gen.vict2$Rel.Freq <- gen.vict2$Freq/length(subset$Gender)
ggplot()+
  geom_bar(aes(x=Var2, y=Rel.Freq, fill=Var1), stat="identity", position="dodge", data=gen.vict2) +
  xlab("Descriptor") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Gender") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) +
  scale_fill_manual(values=c("#F0B1EE", "lightblue")) +
  theme(legend.position=c(.85, .85)) +
  scale_y_continuous(labels=percent)

# Conditional Tables & Chart
f.tot <- sum(filter(gen.vict, Var1 == "Female")$Freq)
m.tot <- sum(filter(gen.vict, Var1 == "Male")$Freq)
gen.vict$con.Freq <- ifelse(gen.vict$Var1 =="Female", gen.vict$Freq/f.tot, gen.vict$Freq/m.tot) 

ggplot()+
  geom_bar(aes(x=Var2, y=con.Freq, fill=Var1), stat="identity", position="dodge",data=gen.vict) +
  xlab("Descriptor") +
  ylab("Condiotional Frequency (%)") +
  ggtitle("Victim Descriptor by Gender") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) +
  scale_fill_manual(values=c("#F0B1EE", "lightblue")) +
  theme(legend.position=c(.8, .85)) +
  scale_y_continuous(labels=percent)


gen.vict2$con.Freq <- ifelse(gen.vict2$Var1 =="Female", gen.vict2$Freq/f.tot, gen.vict2$Freq/m.tot)
ggplot()+
  geom_bar(aes(x=Var2, y=con.Freq, fill=Var1), stat="identity", position="dodge",data=gen.vict2) +
  xlab("Descriptor") +
  ylab("Condiotional Frequency (%)") +
  ggtitle("Victim Descriptor by Gender") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Gender", title.position = "top")) +
  scale_fill_manual(values=c("#F0B1EE", "lightblue")) +
  theme(legend.position=c(.8, .85)) +
  scale_y_continuous(labels=percent)

##################### RACE ####################
# All victim groups
race.vict <- as.data.frame(table(subset$Race.Coll, subset$Victim))
race.vict$Rel.Freq <- race.vict$Freq/length(subset$Race.Coll)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position = "dodge", data=race.vict) +
  xlab("Race") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Race") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .7)) +
  scale_y_continuous(labels=percent)

# Collapsed victim groups
race.vict2 <- as.data.frame(table(subset$Race.Coll, subset$Victim.Group))
race.vict2$Rel.Freq <- race.vict2$Freq/length(subset$Race.Coll)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position = "dodge", data=race.vict2) +
  xlab("Race") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Race") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .7)) +
  scale_y_continuous(labels=percent)

## Conditional on Race
white.tot <- sum(filter(race.vict, Var1 == "White")$Freq)
otherRace.tot <- sum(filter(race.vict, Var1 == "Other")$Freq)
race.vict$con.Freq <- ifelse(race.vict$Var1 =="White", race.vict$Freq/white.tot, 
                             race.vict$Freq/otherRace.tot) 

ggplot()+
  geom_bar(aes(x=Var1, y=con.Freq, fill=Var2), stat="identity", position = "dodge", data=race.vict) +
  xlab("Race") +
  ylab("Conditional Frequency (%)") +
  ggtitle("Victim Descriptor by Race") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .7)) +
  scale_y_continuous(labels=percent)

race.vict2$con.Freq <- ifelse(race.vict2$Var1 =="White", race.vict2$Freq/white.tot, 
                             race.vict2$Freq/otherRace.tot) 

ggplot()+
  geom_bar(aes(x=Var1, y=con.Freq, fill=Var2), stat="identity", position = "dodge", data=race.vict2) +
  xlab("Race") +
  ylab("Conditional Frequency (%)") +
  ggtitle("Victim Descriptor by Race") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .7)) +
  scale_y_continuous(labels=percent)



###################### YEAR ####################
# All years
year.vict <- as.data.frame(table(subset$Year.Coll, subset$Victim))
year.vict$Rel.Freq <- year.vict$Freq/length(subset$Year.Coll)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position = "dodge", data=year.vict) +
  xlab("Descriptor") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Year") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Year", title.position = "top")) +
  theme(legend.position=c(.8, .7)) +
  scale_y_continuous(labels=percent)

# Collapsed years
year.vict2 <- as.data.frame(table(subset$Year.Coll, subset$Victim.Group))
year.vict2$Rel.Freq <- year.vict2$Freq/length(subset$Year.Coll)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=year.vict2) +
  xlab("Descriptor") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Year") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Year", title.position = "top")) +
  theme(legend.position=c(.9, .7)) +
  scale_y_continuous(labels=percent)

##### Conditional 
fresh.tot <- sum(filter(year.vict2, Var1 == "Freshman")$Freq)
soph.tot <- sum(filter(year.vict2, Var1 == "Sophomore")$Freq)
jun.tot <- sum(filter(year.vict2, Var1 == "Junior")$Freq)
sen.tot <- sum(filter(year.vict2, Var1 == "Senior")$Freq)
othYear.tot <- sum(filter(year.vict2, Var1 == "Other")$Freq)
year.vict2$con.Freq <- ifelse(year.vict2$Var1 =="Freshman", year.vict2$Freq/fresh.tot, 
                              ifelse(year.vict2$Var1 =="Sophomore", year.vict2$Freq/soph.tot,
                                     ifelse(year.vict2$Var1 =="Junior", year.vict2$Freq/jun.tot,
                                            ifelse(year.vict2$Var1 =="Senior", year.vict2$Freq/sen.tot, 
                                                   year.vict2$Freq/othYear.tot))))

ggplot()+
  geom_bar(aes(x=Var1, y=con.Freq, fill=Var2), stat="identity", position="dodge",data=year.vict2) +
  xlab("Descriptor") +
  ylab("Condtional Frequency (%)") +
  ggtitle("Victim Descriptor by Year") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5,
        axis.text=element_text(size=8)) +
  guides(fill = guide_legend(title = "Year", title.position = "top")) +
  scale_y_continuous(labels=percent)


#################### Sexual Orientation ###################
so.vict <- as.data.frame(table(subset$SO.Coll, subset$Victim))
so.vict$Rel.Freq <- so.vict$Freq/length(subset$SO)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=so.vict) +
  xlab("Sexual Orientation") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Sexual Orientation") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .60)) +
  scale_y_continuous(labels=percent)

# Colllapsed SO
so.vict2 <- as.data.frame(table(subset$SO.Coll, subset$Victim.Group))
so.vict2$Rel.Freq <- so.vict2$Freq/length(subset$SO)
ggplot()+
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=so.vict2) +
  xlab("Sexual Orientation") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Sexual Orientation") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .60)) +
  scale_y_continuous(labels=percent)

## Conditional on Race
straight.tot <- sum(filter(race.vict, Var1 == "Straight")$Freq)
otherSO.tot <- sum(filter(race.vict, Var1 == "Straight")$Freq)
rso.vict$con.Freq <- ifelse(race.vict$Var1 =="White", race.vict$Freq/white.tot, 
                             race.vict$Freq/otherSO.tot) 


################## AGE #################### - Come back to age variable needs cleaning
age.vict <- as.data.frame(table(subset$Age, subset$Victim))
age.vict$Rel.Freq <- so.vict2$Freq/length(subset$Age)
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=age.vict) +
  xlab("Sexual Orientation") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Sexual Orientation") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.8, .60)) +
  scale_y_continuous(labels=percent)

#################################################
############## SUBSET VICTIMS ONLY ##############
#################################################
victims <- subset %>% filter( Victim != "Non-Victim" & Victim.Group != "NonVictim" )
victims$Victim <-factor(victims$Victim) #Removes Non-Victim level
victims$Victim.Group <-factor(victims$Victim.Group) #Removes Non-Victim level

############### SV_V_Rel ############### (Relationship to the other person)
rel.vict <- as.data.frame(table(victims$SV_V_rel, victims$Victim))
rel.vict$Rel.Freq <- rel.vict$Freq/length(victims$SV_V_rel)
levels(rel.vict$Var1) <- gsub(" ", "\n", levels(rel.vict$Var1)) # Code to put Xaxis labels on two lines.
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=rel.vict) +
  xlab("Relationship to Other Person") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Relationship to Other Person") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

# Collapsed Rel
rel.vict2 <- as.data.frame(table(victims$SV_V_rel, victims$Victim.Group))
rel.vict2$Rel.Freq <- rel.vict2$Freq/length(victims$SV_V_rel)
levels(rel.vict2$Var1) <- gsub(" ", "\n", levels(rel.vict2$Var1))
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=rel.vict2) +
  xlab("Relationship to Other Person") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Relationship to Other Person") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

############### SV_V_Alc 1 - Was the other person using alcohol ##############
alc1.vict <- as.data.frame(table(victims$SV_V_alc1, victims$Victim))
alc1.vict$Rel.Freq <- alc1.vict$Freq/length(victims$SV_V_alc1)
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=alc1.vict) +
  xlab("Alcohol/Drug use of Perpetrator") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Perpetrator use of Drugs/Alcohol") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

# Collapsed Alc1
alc1.vict2 <- as.data.frame(table(victims$SV_V_alc1, victims$Victim.Group))
alc1.vict2$Rel.Freq <- alc1.vict2$Freq/length(victims$SV_V_alc1)
levels(rel.vict2$Var1) <- gsub(" ", "\n", levels(rel.vict2$Var1))
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=alc1.vict2) +
  xlab("Alcohol/Drug use of Perpetrator") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Perpetrator use of Drugs/Alcohol") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

############## SV_V_Alc 2 - Was the victim using alcohol ##################
alc2.vict <- as.data.frame(table(victims$SV_V_alc2, victims$Victim))
alc2.vict$Rel.Freq <- alc2.vict$Freq/length(victims$SV_V_alc2)
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=alc2.vict) +
  xlab("Alcohol/Drug use of Victim") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Victim use of Drugs/Alcohol") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

# Collapsed Alc2
alc2.vict2 <- as.data.frame(table(victims$SV_V_alc2, victims$Victim.Group))
alc2.vict2$Rel.Freq <- alc2.vict2$Freq/length(victims$SV_V_alc2)
ggplot() +
  geom_bar(aes(x=Var1, y=Rel.Freq, fill=Var2), stat="identity", position="dodge",data=alc2.vict2) +
  xlab("Alcohol/Drug use of Victim") +
  ylab("Relative Frequency (%)") +
  ggtitle("Victim Descriptor by Victim use of Drugs/Alcohol") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top")) +
  theme(legend.position=c(.85, .60)) +
  scale_y_continuous(labels=percent)

####### Perpetrator Force #########
# For this section we will be grouping incidents as 1, 2 or 3 times as 1.

subset$Lies <- ifelse(subset$SV_Perp1_SV_Perp1_1==0,0,1) # 1 represents at least one incident
subset$Displeasure <- ifelse(subset$SV_Perp1_SV_Perp1_2==0,0,1) # 1 represents at least one incident
subset$TakingAdv <- ifelse(subset$SV_Perp1_SV_Perp1_3==0,0,1) # 1 represents at least one incident
subset$Threat <- ifelse(subset$SV_Perp1_SV_Perp1_4==0,0,1) # 1 represents at least one incident
subset$Force <- ifelse(subset$SV_Perp1_SV_Perp1_5==0,0,1) # 1 represents at least one incident

perp1.vict <- as.data.frame(table(victims$Victim,victims$SV_Perp1_SV_Perp1_1))
perp1a.vict <- as.data.frame(table(victims$Victim,victims$Lies))

ggplot() +
  geom_bar(aes(x=Var1, y=Freq, fill=Var2), stat="identity", position="dodge",data=perp1a.vict) +
  xlab("# of times") +
  ylab("Frequency") +
  ggtitle("Perpetrator Lie") +
  theme_bw() +
  theme(legend.position = "top", legend.title.align = 0.5) +
  guides(fill = guide_legend(title = "Descriptor", title.position = "top"))

perp2.vict <- as.data.frame(table(victims$Victim,victims$SV_Perp1_SV_Perp1_2))
perp3.vict <- as.data.frame(table(victims$Victim,victims$SV_Perp1_SV_Perp1_3))
perp4.vict <- as.data.frame(table(victims$Victim,victims$SV_Perp1_SV_Perp1_4))
perp5.vict <- as.data.frame(table(victims$Victim,victims$SV_Perp1_SV_Perp1_5))

