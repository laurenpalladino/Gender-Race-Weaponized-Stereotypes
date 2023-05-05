library(sjPlot)
library(dplyr)
library(ggplot2)

mturk <- read.csv("Documents/[RESEARCH] HHH/Palladino+RUSP+Survey_March+2,+2020_10.09.csv")

mturk <- mturk[16:nrow(mturk),]
names(mturk) #DVs are in variables Q2.8, Q2.9, Q2.10 Q2.11, Q2.12

#cleaning up some demographics
mturk$female<-NA
mturk$female[mturk$Q1.1=="Female"]<-1
mturk$female[mturk$Q1.1=="Male"]<-0

mturk$pid7<-NA
mturk$pid7[mturk$Q1.4=="Democrat" & mturk$Q1.5=="Strong ${q://QID44/ChoiceGroup/SelectedChoices}"]<-1
mturk$pid7[mturk$Q1.4=="Democrat" & mturk$Q1.5=="Not strong ${q://QID44/ChoiceGroup/SelectedChoices}"]<-2
mturk$pid7[mturk$Q1.4=="Independent" & mturk$Q1.6=="Closer to Democratic Party"]<-3
mturk$pid7[mturk$Q1.4=="Other" & mturk$Q1.6=="Closer to Democratic Party"]<-3
mturk$pid7[mturk$Q1.4=="Independent" & mturk$Q1.6=="Neither"]<-4
mturk$pid7[mturk$Q1.4=="Other" & mturk$Q1.6=="Neither"]<-4
mturk$pid7[mturk$Q1.4=="Independent" & mturk$Q1.5=="Closer to Republican Party}"]<-5
mturk$pid7[mturk$Q1.4=="Other" & mturk$Q1.5=="Closer to Republican Party}"]<-5
mturk$pid7[mturk$Q1.4=="Republican" & mturk$Q1.5=="Not strong ${q://QID44/ChoiceGroup/SelectedChoices}"]<-6
mturk$pid7[mturk$Q1.4=="Republican" & mturk$Q1.5=="Strong ${q://QID44/ChoiceGroup/SelectedChoices}"]<-7
table(mturk$pid7)

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
mturk$dv1 <- recode(mturk$Q2.8, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)

competentlm <- lm(dv1 ~ attack*candidate_race + pid7 + female, data=mturk)
summary(competentlm) 

#plot
plot_model(competentlm, type = "pred", terms = c("attack", "candidate_race"),
           axis.title = c("Attack Type", "Competence"),
           legend.title = "Candidate Race", title = "Predicted Competence")

#honesty
mturk$dv2 <- recode(mturk$Q2.9, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
honestlm <- lm(dv2 ~ attack*candidate_race + pid7 + female, data=mturk)
summary(honestlm) 

plot_model(honestlm, type = "pred", terms = c("attack", "candidate_race"),
           axis.title = c("Attack Type", "Honesty"),
           legend.title = "Candidate Race", title = "Predicted Honesty")

#concern
mturk$dv3 <- recode(mturk$Q2.10, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
concernlm<-lm(dv3 ~ attack*candidate_race + pid7 + female, data=mturk)
summary(concernlm) 

plot_model(concernlm, type = "pred", terms = c("attack", "candidate_race"),
           axis.title = c("Attack Type", "Concern for Constituents"),
           legend.title = "Candidate Race", title = "Predicted Concern")

#reliable
mturk$dv4<- recode(mturk$Q2.11, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
reliablelm<- lm(dv4 ~ attack*candidate_race + pid7 + female, data=mturk)
summary(reliablelm)

plot_model(reliablelm, type = "pred", terms = c("attack", "candidate_race"),
           axis.title = c("Attack Type", "Reliability"),
           legend.title = "Candidate Race", title = "Predicted Reliability")

#same values
mturk$dv5<- recode(mturk$Q2.12, "Agree Strongly"=1, "Agree"= 0.75, "Neither Agree nor Disagree" = 0.5, "Disagree"=0.25, "Disagree Strongly"=0)
valueslm<- lm(dv5 ~ attack*candidate_race + pid7 + female, data=mturk)
summary(valueslm)

plot_model(valueslm, type = "pred", terms = c("attack", "candidate_race"),
           axis.title = c("Attack Type", "Shares the Same Values as I Do"),
           legend.title = "Candidate Race", title = "Predicted Values")


#May 2023 figure revamp:
trust <- (mturk$dv1/5) + (mturk$dv2/5) + (mturk$dv3/5) + (mturk$dv4/5) + (mturk$dv5/5)

trustlm<-lm(trust ~ attack*candidate_race + pid7 + female, data=mturk)

plot_model(trustlm, type = "pred", terms = c("attack", "candidate_race"),
                axis.title = c("Attack Type", "Trust"),
                legend.title = "Candidate Race", title = "Predicted Trust")
