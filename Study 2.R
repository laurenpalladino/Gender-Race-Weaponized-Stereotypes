library(readr)
library(dplyr)
library(survey)
library(stargazer)
library(effects)
library(ggplot2)
library(sjPlot)
mturk <- read_csv("mturk.csv")
attach(mturk)

mturk$pid7 <-NA
#strong dem
mturk$pid7[PID == 1 & `PID Strength` == 1] <- 1
#weak dem
mturk$pid7[PID == 1 & `PID Strength` == 2] <- 2
#lean dem
mturk$pid7[PID == 3 & `Party Leaners` == 2] <- 3
mturk$pid7[PID == 4 & `Party Leaners` == 2] <- 3
#true ind
mturk$pid7[PID == 3 & `Party Leaners` == 3] <- 4
mturk$pid7[PID == 4 & `Party Leaners` == 3] <- 4
#lean rep
mturk$pid7[PID == 3 & `Party Leaners` == 1] <- 5
mturk$pid7[PID == 4 & `Party Leaners` == 1] <- 5
#weak rep
mturk$pid7[PID == 2 & `PID Strength` == 2]  <- 6
#strong rep
mturk$pid7[PID == 2 & `PID Strength` == 1]  <- 7
table(mturk$pid7)

mturk$female<-NA
mturk$female[mturk$Gender==16]<-0
mturk$female[mturk$Gender==17]<-1

mturk$attack <- as.factor(NA)
levels(mturk$attack) <- c("control attack", "white feminine attack", "Black feminine attack")
mturk$attack[mturk$condition=="CABN"] <- "control attack"
mturk$attack[mturk$condition=="CAWN"] <- "control attack"
mturk$attack[mturk$condition=="WABN"] <- "white feminine attack"
mturk$attack[mturk$condition=="WAWN"] <- "white feminine attack"
mturk$attack[mturk$condition=="BABN"] <- "Black feminine attack"
mturk$attack[mturk$condition=="BAWN"] <- "Black feminine attack"
table(mturk$attack)

mturk$congruent<-as.factor(NA)
levels(mturk$congruent) <- c("congruent", "incongruent")
mturk$congruent[mturk$condition=="WABN"] <- "incongruent"
mturk$congruent[mturk$condition=="WAWN"] <- "congruent"
mturk$congruent[mturk$condition=="BABN"] <- "congruent"
mturk$congruent[mturk$condition=="BAWN"] <- "incongruent"
table(mturk$congruent)

mturk$candidate_race <- as.factor(NA)
levels(mturk$candidate_race) <- c("Black cue", "white cue")
mturk$candidate_race[mturk$candidate_name=="Althea Johnson"] <- "Black cue"
mturk$candidate_race[mturk$candidate_name=="Susan Johnson"] <- "white cue"
table(mturk$candidate_race)
table(mturk$attack, mturk$candidate_race)

#hostile sexism
HS1<-as.numeric(mturk$HS1)
HS2<-as.numeric(mturk$HS2)
HS3<-as.numeric(mturk$HS3)
mturk$HS <- (HS1 + HS2 + HS3)
summary(mturk$HS)

#racial resentment
RR1<-as.numeric(mturk$RR1)
RR2<-as.numeric(mturk$RR2)
RR3<-as.numeric(mturk$RR3)
RR4<-as.numeric(mturk$RR4)
mturk$RR <- (RR1 + RR2 + RR3 + RR4)
summary(mturk$RR)

#competent
mturk$dv1 <-dplyr::recode(mturk$Q2.8, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
competent1 <- lm(dv1~attack+candidate_race + HS + RR + pid7 + female, data=mturk)
competent2 <- lm(dv1~attack*candidate_race + HS + RR + pid7 + female, data=mturk)

#honesty
mturk$dv2 <- dplyr::recode(mturk$Q2.9, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
honest1 <- lm(dv2~attack+candidate_race + HS + RR + pid7 + female, data=mturk)
honest2 <- lm(dv2~attack*candidate_race + HS + RR + pid7 + female, data=mturk)

#concern
mturk$dv3 <- dplyr::recode(mturk$Q2.10, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
concern1 <- lm(dv3~attack+candidate_race + HS + RR + pid7 + female, data=mturk)
concern2 <- lm(dv3~attack*candidate_race + HS + RR + pid7 + female, data=mturk)

#reliable
mturk$dv4 <- dplyr::recode(mturk$Q2.11, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
reliable1 <- lm(dv4~attack+candidate_race + HS + RR + pid7 + female, data=mturk)
reliable2 <- lm(dv4~attack*candidate_race + HS + RR + pid7 + female, data=mturk)

#same values
mturk$dv5 <- dplyr::recode(mturk$Q2.12, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
values1 <- lm(dv5~attack+candidate_race + HS + RR + pid7 + female, data=mturk)
values2 <- lm(dv5~attack*candidate_race + HS + RR + pid7 + female, data=mturk)

#table comparing the 5 whole-sample models
stargazer(competent1, competent2, honest1, honest2, concern1, concern2,
          reliable1, reliable2, values1, values2, type="text",star.cutoffs = c(0.05, 0.01, 0.001))

#composite model
mturk$trust <- (mturk$dv1*0.20 + mturk$dv2*0.20 + mturk$dv3*0.20 + mturk$dv4*0.20 + mturk$dv5*0.20)

trustlm <- lm(trust~candidate_race*attack + pid7 + female+ HS+ RR, data=mturk)
trustlm2 <- lm(trust~candidate_race+attack + pid7 + female+ HS+ RR, data=mturk)
stargazer(trustlm, trustlm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

t.test(trust~candidate_race, data=subset(mturk, congruent=="congruent"))
t.test(trust~candidate_race, data=subset(mturk, congruent=="incongruent"))

#graph
plot_model(trustlm, type = "pred", terms = c("attack", "candidate_race"),
                axis.title = c("Attack Type", "Composite Evaluation"),
                legend.title = "Candidate Race", title = "")+
  theme_bw() +
  scale_color_manual(name = "Candidate Race",
                     labels = c("White cue", "Black cue"),
                     values = c("grey50", "black")) +
  scale_shape_manual(name = "Candidate Race",
                     labels = c("White cue", "Black cue"),
                     values = c(17, 15)) +
  guides(color = guide_legend(override.aes = list(shape = c(17, 15)))) +
  theme(
    axis.text.x = element_text(size = 22),
    text = element_text(size = 25),
    legend.position = "bottom",
    aspect.ratio = 0.85
  ) + aes(shape = group)
  
