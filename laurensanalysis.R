mturk <- read.csv("Downloads/laurenrusp.csv")

#Stripping out the first 15 observations, which are all survey previews or surveys taken by us
mturk <- mturk[16:nrow(mturk),]
head(mturk) #Start dates all look correct

names(mturk)
#DVs are in variables Q2.8, Q2.9, Q2.10 Q2.11, Q2.12

#Creating treatment variables
mturk$attack <- as.factor(NA)
levels(mturk$attack) <- c("Gendered attack", "No attack")
mturk$attack[mturk$PoliticalAttitudes_DO_Control..Neutral==2] <- "No attack"
mturk$attack[mturk$PoliticalAttitudes_DO_Control..Black==2] <- "No attack"
mturk$attack[mturk$PoliticalAttitudes_DO_Control..White==2] <- "No attack"
mturk$attack[mturk$PoliticalAttitudes_DO_Attack..Neutral==2] <- "Gendered attack"
mturk$attack[mturk$PoliticalAttitudes_DO_Attack..Black==2] <- "Gendered attack"
mturk$attack[mturk$PoliticalAttitudes_DO_Attack..White==2] <- "Gendered attack"
mturk$attack <- relevel(mturk$attack, "No attack")
table(mturk$attack)

mturk$candidate_race <- as.factor(NA)
levels(mturk$candidate_race) <- c("White cue", "Black cue", "No cue")
mturk$candidate_race[mturk$candidate_name=="Susan Johnson"] <- "White cue"
mturk$candidate_race[mturk$candidate_name=="Althea Johnson"] <- "Black cue"
mturk$candidate_race[mturk$candidate_name=="Michael Hepner's opponent"] <- "No cue"
mturk$candidate_race <- relevel(mturk$candidate_race, "No cue")
table(mturk$candidate_race)

#Recoding the DV into a 0-1 scale, with 1 being more favorable evaluations of the candidate
library(dplyr) #you may need to run install.packages("dplyr") to get this to run
mturk$dv1 <- recode(mturk$Q2.8, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
table(mturk$dv1)

levels(mturk$attack)


m1 <- lm(dv1 ~ attack, data=mturk)
summary(m1) 
#Great! (well, not great) Gendered attacks significantly reduce evaluations of candidates

m2 <- lm(dv1 ~ candidate_race, data=mturk)
summary(m2) 
#Okay, also cool. Giving the name seems to result in an ever so slightly (but not significantly) lower evaluation.

m3 <- lm(dv1 ~ attack*candidate_race, data=mturk)
summary(m3) 
#Okay, so this looks like there is a significant main effect, but no interaction. So gendered attacks harm candidates, but it does not look like there's a big difference between white and black candidates


#Now plotting just the means (which should be mathematically equivalent to the point estimates from the above models)
library(survey)
library(ggplot2)

mturk_survey <- svydesign(ids = ~1, data=mturk) #This just transforms the data into something the survey package understands; you can use this to assign weights to respondents

m3_survey <- svyby(~dv1, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")


jpeg("competence.jpeg")
ggplot(m3_survey, aes(x=candidate_race, y=dv1)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived competence", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("No cue", "White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()

#lauren attempts to repeat with honesty (dv2=Q2.9)

library(dplyr)
mturk$dv2 <- recode(mturk$Q2.9, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
table(mturk$dv2)

levels(mturk$attack)

m4 <- lm(dv2 ~ attack, data=mturk)
summary(m4)
#F(1, 362)= 6.713, p= 0.009958

m5 <- lm(dv2 ~ candidate_race, data=mturk)
summary(m5)
#F(2, 361)=1.617, p=0.1999

m6 <- lm(dv2 ~ attack*candidate_race, data=mturk)
summary(m6) 
#F(5, 358)=2.984, p=0.01183

library(ggplot2)

mturk_survey <- svydesign(ids = ~1, data=mturk) 

honesty_survey <- svyby(~dv2, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")


jpeg("honesty.jpeg")
ggplot(honesty_survey, aes(x=candidate_race, y=dv2)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived honesty", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("No cue", "White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()

#lauren attempts to repeat with concern (dv3=Q2.10)

library(dplyr)
mturk$dv3 <- recode(mturk$Q2.10, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
table(mturk$dv3)

levels(mturk$attack)

m7 <- lm(dv3 ~ attack, data=mturk)
summary(m7)
#F(1, 362)= 7.947, p= 0.005082

m8 <- lm(dv3 ~ candidate_race, data=mturk)
summary(m8)
#F(2, 361)=1.366 p=0.2566

m9<-lm(dv3 ~ attack*candidate_race, data=mturk)
summary(m9) 
#F(5, 358)=2.524, p=0.02905

library(ggplot2)

mturk_survey <- svydesign(ids = ~1, data=mturk) 

concern_survey <- svyby(~dv3, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")

#10/24 attempt to fix pdf export function
pdf("concern1.pdf")
ggplot(concern_survey, aes(x=candidate_race, y=dv3)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived concern", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("No cue", "White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()

#lauren attempts to repeat with reliability (dv4=Q2.11)

library(dplyr)
mturk$dv4<- recode(mturk$Q2.11, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
table(mturk$dv4)

levels(mturk$attack)

m10<- lm(dv4 ~ attack, data=mturk)
summary(m10)
#F(1, 362)= 26.81, p= 0.03.729e-07

m11<- lm(dv4 ~ candidate_race, data=mturk)
summary(m11)
#F(2, 361)=1.610 p=0.2014

m12<- lm(dv4 ~ attack*candidate_race, data=mturk)
summary(m12)
#F(5, 358)=6.325, p=1.212e-05

library(ggplot2)

mturk_survey <- svydesign(ids = ~1, data=mturk) 

reliable_survey<-svyby(~dv4, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")


pdf("reliable.pdf")
ggplot(reliable_survey, aes(x=candidate_race, y=dv4)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived reliability", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("No cue", "White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()

#lauren attempts to repeat with same values (dv5=Q2.12)

library(dplyr)
mturk$dv5<- recode(mturk$Q2.12, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
table(mturk$dv4)

levels(mturk$attack)

m13<- lm(dv5 ~ attack, data=mturk)
summary(m13)
#F(1, 362)= 5.609, p= 0.01839

m14<- lm(dv5 ~ candidate_race, data=mturk)
summary(m14)
#F(2, 361)=2.401 p=0.09207

m15<- lm(dv5 ~ attack*candidate_race, data=mturk)
summary(m15)
#F(5, 358)=2.588, p=0.0257

library(ggplot2)

mturk_survey <- svydesign(ids = ~1, data=mturk) 

values_survey<-svyby(~dv5, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")


pdf("values.pdf")
ggplot(values_survey, aes(x=candidate_race, y=dv5)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived same values", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("No cue", "White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()
