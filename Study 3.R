library(readr)
library(dplyr)
library(survey)
library(stargazer)
library(effects)
library(ggplot2)
library(sjPlot)

setwd("/Users/laurenpalladino/Documents/[RESEARCH] HHH/Experiment 3")
mturkdata <- read_csv("mturkdata.csv")

mturkdata$pid <-NA
mturkdata$pid[mturkdata$PID == 1 & mturkdata$PIDstrong == 1] <- 1
mturkdata$pid[mturkdata$PID == 1 & mturkdata$PIDstrong == 2] <- 2
mturkdata$pid[mturkdata$PID == 3 & mturkdata$PIDlean == 2] <- 3
mturkdata$pid[mturkdata$PID == 4 & mturkdata$PIDlean == 2] <- 3
mturkdata$pid[mturkdata$PID == 3 & mturkdata$PIDlean == 3] <- 4
mturkdata$pid[mturkdata$PID == 4 & mturkdata$PIDlean == 3] <- 4
mturkdata$pid[mturkdata$PID == 3 & mturkdata$PIDlean == 1] <- 5
mturkdata$pid[mturkdata$PID == 4 & mturkdata$PIDlean == 1] <- 5
mturkdata$pid[mturkdata$PID == 2 & mturkdata$PIDstrong == 2]  <- 6
mturkdata$pid[mturkdata$PID == 2 & mturkdata$PIDstrong == 1]  <- 7
table(mturkdata$pid)

democrats = subset(mturkdata, mturkdata$pid<=3)
republicans = subset(mturkdata, mturkdata$pid>=5)

mturkdata$attack <- as.factor(NA)
levels(mturkdata$attack) <- c("political attack", "gender attack")
mturkdata$attack[mturkdata$condition=="controlpol"] <- "political attack"
mturkdata$attack[mturkdata$condition=="inpol"] <- "political attack"
mturkdata$attack[mturkdata$condition=="outpol"] <- "political attack"
mturkdata$attack[mturkdata$condition=="controlfem"] <- "gender attack"
mturkdata$attack[mturkdata$condition=="infem"] <- "gender attack"
mturkdata$attack[mturkdata$condition=="outfem"] <- "gender attack"
table(mturkdata$attack)

mturkdata$party <- as.factor(NA)
levels(mturkdata$party) <- c("in party", "out party", "no party")
mturkdata$party[mturkdata$condition=="inpol"] <- "in party"
mturkdata$party[mturkdata$condition=="infem"] <- "in party"
mturkdata$party[mturkdata$condition=="outpol"] <- "out party"
mturkdata$party[mturkdata$condition=="outfem"] <- "out party"
mturkdata$party[mturkdata$condition=="controlpol"] <- "no party"
mturkdata$party[mturkdata$condition=="controlfem"] <- "no party"
table(mturkdata$party)

table(mturkdata$attack, mturkdata$party)

#hostile sexism
HS1<-as.numeric(mturkdata$HS1)
HS1 <-dplyr::recode(mturkdata$HS1, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
HS2<-as.numeric(mturkdata$HS2)
HS2 <-dplyr::recode(mturkdata$HS2, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
HS3<-as.numeric(mturkdata$HS3)
HS3 <-dplyr::recode(mturkdata$HS3, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
mturkdata$HS <- (HS1 + HS2 + HS3)/15
summary(mturkdata$HS)

#racial resentment
RR1<-as.numeric(mturkdata$RR1)
RR1 <-dplyr::recode(mturkdata$RR1, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
RR2<-as.numeric(mturkdata$RR2)
RR2 <-dplyr::recode(mturkdata$RR2, "1"=1, "2"=2, "4"=3, "5"=4, "6"=5)
RR3<-as.numeric(mturkdata$RR3)
RR3 <-dplyr::recode(mturkdata$RR3, "1"=1, "2"=2, "4"=3, "5"=4, "6"=5)
RR4<-as.numeric(mturkdata$RR4)
RR4 <-dplyr::recode(mturkdata$RR4, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
mturkdata$RR <- (RR1 + RR2 + RR3 + RR4)/20
summary(mturkdata$RR)

#gender
mturkdata$female<-NA
mturkdata$female[mturkdata$Gender==16]<-0
mturkdata$female[mturkdata$Gender==17]<-1

#competent
mturkdata$dv1 <-dplyr::recode(mturkdata$competent, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
competentlm1 <- lm(dv1~party+attack + pid + HS + RR + female, data=mturkdata)
competentlm2 <- lm(dv1~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(competentlm1, competentlm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#honesty
mturkdata$dv2 <-dplyr::recode(mturkdata$honest, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
honestlm1 <- lm(dv2~party+attack + pid + HS + RR + female, data=mturkdata)
honestlm2 <- lm(dv2~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(honestlm1, honestlm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#concern for constituents
mturkdata$dv3 <-dplyr::recode(mturkdata$concern, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
concernlm1 <- lm(dv3~party+attack + pid + HS + RR + female, data=mturkdata)
concernlm2 <- lm(dv3~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(concernlm1, concernlm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#reliable
mturkdata$dv4 <-dplyr::recode(mturkdata$reliable, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
reliablelm1 <- lm(dv4~party+attack + pid + HS + RR + female, data=mturkdata)
reliablelm2 <- lm(dv4~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(reliablelm1, reliablelm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#same values
mturkdata$dv5 <-dplyr::recode(mturkdata$values, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
valueslm1 <- lm(dv5~party+attack + pid + HS + RR + female, data=mturkdata)
valueslm2 <- lm(dv5~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(valueslm1, valueslm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(competentlm1, competentlm2, honestlm1, honestlm2, reliablelm1,reliablelm2,
  concernlm1, concernlm2, valueslm1, valueslm2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#how likely are you to vote for susan?
mturkdata$dvvote <-dplyr::recode(mturkdata$vote, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
votemodel1 <-lm(dvvote~party + attack + pid + HS + RR + female, data=mturkdata)
votemodel2 <-lm(dvvote~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(votemodel1, votemodel2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#who is more capable?
mturkdata$capable <-dplyr::recode(mturkdata$pref, "1"=0, "2"=1, "3"=0.5) #1=man #2=susan #3=equally capable
capable1 <- lm(capable~party+attack + pid + HS + RR + female, data=mturkdata)
capable2 <- lm(capable~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(capable1, capable2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#are the attacks against susan justified?
mturkdata$justified <-dplyr::recode(mturkdata$justify, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
#1=definitely yes #5=definitely no

justify1 <- lm(justified~party+attack + pid + HS + RR + female, data=mturkdata)
justify2 <- lm(justified~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(justify1, justify2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#feeling thermometer
mturkdata$ft<-as.numeric(mturkdata$FT_41)
ftmodel1 <- lm(ft~party+attack + pid + HS + RR + female, data=mturkdata)
ftmodel2 <- lm(ft~party*attack + pid + HS + RR + female, data=mturkdata)

stargazer(ftmodel1, ftmodel2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(votemodel1, votemodel2, capable1, capable2, justify1, justify2,
          ftmodel1, ftmodel2, type="text", star.cutoffs = c(0.05, 0.01, 0.001))

#composite measure of trust

mturkdata$trust <- (mturkdata$dv1*.2 + mturkdata$dv2*.2 + mturkdata$dv3*.2 + mturkdata$dv4*.2 + mturkdata$dv5*.2)

trustlm1 <- lm(trust~party+attack + HS+ RR + pid + female, data=mturkdata)
trustlm2 <- lm(trust~party*attack + HS+ RR + pid + female, data=mturkdata)

plot_model(trustlm, type = "pred", terms = c("attack", "party"),
           axis.title = c("Attack Type", "Trust"),
           legend.title = "Party", title = "Predicted Trust")

plot_model(trustlm2, type = "pred", terms = c("attack", "party"),
           axis.title = c("Attack Type", "Composite Evaluation"),
           legend.title = "Party", title = "")+
  theme_bw() +
  scale_color_manual(name = "Candidate Party",
                     labels = c("In Party", "Out Party", "No Party"),
                     values = c("black", "grey45", "grey80")) +
  scale_shape_manual(name = "Candidate Party",
                     labels = c("In Party", "Out Party", "No Party"),
                     values = c(16, 17, 15)) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 15)))) +
  theme(
    axis.text.x = element_text(size = 22),
    text = element_text(size = 25),
    legend.position = "bottom",
    aspect.ratio = 0.85
  ) + aes(shape = group)+
  annotate("rect", xmin = 0.965, xmax = 1, ymin = 0.642, ymax =0.642, alpha=1, colour = "black") +
  annotate("rect", xmin = 0.965, xmax = 0.965, ymin = 0.640, ymax =0.642, alpha=1,colour = "black") +
  annotate("rect", xmin = 1, xmax = 1, ymin = 0.640, ymax =0.642, alpha=1,colour = "black") +
  annotate("text", x=0.985, y=0.645, label="p<0.01", size=5)+
  annotate("rect", xmin = 1.965, xmax = 2, ymin = 0.642, ymax =0.642, alpha=1, colour = "black") +
  annotate("rect", xmin = 1.965, xmax = 1.965, ymin = 0.640, ymax =0.642, alpha=1,colour = "black") +
  annotate("rect", xmin = 2 , xmax = 2, ymin = 0.640, ymax =0.642, alpha=1,colour = "black") +
  annotate("text", x=1.985, y=0.645, label="p<0.001", size=5)

subset1<-subset(mturkdata, party=="in party" | party=="out party")
t.test(trust~party, data=subset(subset1, attack=="political attack"))
t.test(trust~party, data=subset(subset1, attack=="gender attack"))
