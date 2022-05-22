library(readr)
library(dplyr)
library(survey)
library(stargazer)
library(effects)
mturk <- read_csv("Documents/HHH/mturk.csv")
names(mturk)
attach(mturk)

pid <-NA
#strong dem
pid[PID == 1 & `PID Strength` == 1] <- 1
#weak dem
pid[PID == 1 & `PID Strength` == 2] <- 2
#lean dem
pid[PID == 3 & `Party Leaners` == 2] <- 3
pid[PID == 4 & `Party Leaners` == 2] <- 3
#true ind
pid[PID == 3 & `Party Leaners` == 3] <- 4
pid[PID == 4 & `Party Leaners` == 3] <- 4
#lean rep
pid[PID == 3 & `Party Leaners` == 1] <- 5
pid[PID == 4 & `Party Leaners` == 1] <- 5
#weak rep
pid[PID == 2 & `PID Strength` == 2]  <- 6
#strong rep
pid[PID == 2 & `PID Strength` == 1]  <- 7
table(pid)

democrats = subset(mturk, pid<=3)
republicans = subset(mturk, pid>=5)

mturk$attack <- as.factor(NA)
levels(mturk$attack) <- c("control attack", "white attack", "black attack")
mturk$attack[mturk$condition=="CAWN"] <- "control attack"
mturk$attack[mturk$condition=="CABN"] <- "control attack"
mturk$attack[mturk$condition=="WAWN"] <- "white attack"
mturk$attack[mturk$condition=="WABN"] <- "white attack"
mturk$attack[mturk$condition=="BAWN"] <- "black attack"
mturk$attack[mturk$condition=="BABN"] <- "black attack"
table(mturk$attack)

mturk$candidate_race <- as.factor(NA)
levels(mturk$candidate_race) <- c("White cue", "Black cue")
mturk$candidate_race[mturk$candidate_name=="Susan Johnson"] <- "White cue"
mturk$candidate_race[mturk$candidate_name=="Althea Johnson"] <- "Black cue"
table(mturk$candidate_race)
table(mturk$attack, mturk$candidate_race)

#respondent gender
mturk$gender <- recode(mturk$Gender, "16"=0, "17"=1)

#hostile sexism
HS1<-as.numeric(HS1)
HS2<-as.numeric(HS2)
HS3<-as.numeric(HS3)
HS <- (HS1 + HS2 + HS3)
summary(HS)

#racial resentment
RR1<-as.numeric(RR1)
RR2<-as.numeric(RR2)
RR3<-as.numeric(RR3)
RR4<-as.numeric(RR4)
RR <- (RR1 + RR2 + RR3 + RR4)
summary(RR)

#competent
dv1 <-recode(mturk$Q2.8, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
table(dv1)

model1 <-lm(dv1~candidate_race + attack, data=mturk)
summary(model1)
model2 <- lm(dv1~candidate_race*attack + HS + RR +mturk$pid, data=mturk)
summary(model2)

#attempt to split among party lines....
demcomp <- lm(dv1~candidate_race + attack, data=democrats)
summary(demcomp)
repcomp <- lm(dv1~candidate_race + attack, data=republicans)
summary(repcomp)

#making funky bad graph
m3_survey <- svyby(~dv1, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))
displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")
ggplot(m3_survey, aes(x=candidate_race, y=dv1)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived competence", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("White cue", "Black cue")) + 
  theme_bw() + displayprefs

#better graph
interact <- effect('candidate_race*attack', model2, se=TRUE)
interactdf<-as.data.frame(interact)
interactdf$attack <- factor(interactdf$attack,  
                                 level=c("white attack", "black attack", "control attack"),    
                                 labels=c("white attack", "black attack", "control attack"))   
interactdf$candidate_race <- factor(interactdf$candidate_race,
                                  level=c("White cue", "Black cue"),   
                                  labels=c("White cue", "Black cue"))
plot1<-ggplot(data=interactdf, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Competence")+
  xlab("Attack type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot1

#honesty
dv2 <- recode(mturk$Q2.9, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
table(dv2)

#baseline effects
model3 <-lm(dv2~candidate_race + attack, data=mturk)
summary(model3)
#effects, controls, interactions
model4 <- lm(dv2~candidate_race*attack + HS + RR + pid, data=mturk)
summary(model4)

#making funky graph that's hard to read
honest_survey <- svyby(~dv2, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))
displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")
ggplot(honest_survey, aes(x=candidate_race, y=dv2)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived honesty", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("White cue", "Black cue")) + 
  theme_bw() + displayprefs

#making a better graph
interact4 <- effect('candidate_race*attack', model4, se=TRUE)
interactdf4<-as.data.frame(interact4)
interactdf4$attack <- factor(interactdf4$attack,  
                            level=c("white attack", "black attack", "control attack"),    
                            labels=c("white attack", "black attack", "control attack"))   
interactdf4$candidate_race <- factor(interactdf4$candidate_race,
                                    level=c("White cue", "Black cue"),   
                                    labels=c("White cue", "Black cue"))

plot2<-ggplot(data=interactdf4, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Honesty")+
  xlab("Attack type")+
  ggtitle("Perceived honesty")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot2

#concern
dv3 <- recode(mturk$Q2.10, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
table(dv3)

lm3 <- lm(dv3~candidate_race + attack + HS + RR + HS*RR, data=mturk)
summary(lm3)

model5 <-lm(dv3~candidate_race + attack, data=mturk)
summary(model5)
model6 <- lm(dv3~candidate_race*attack + HS + RR + pid, data=mturk)
summary(model6)

stargazer(model6,type="text", 
          column.labels = c("Effects on Concern"), 
          intercept.bottom = FALSE, 
          single.row=TRUE,  
          notes.append = FALSE, 
          omit.stat=c("ser"),   
          star.cutoffs = c(0.05, 0.01, 0.001),  
          header=FALSE) 

concern_survey <- svyby(~dv3, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")

ggplot(concern_survey, aes(x=candidate_race, y=dv3)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived concern", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("White cue", "Black cue")) + 
  theme_bw() + displayprefs
dev.off()

interact6 <- effect('candidate_race*attack', model6, se=TRUE)
interactdf6<-as.data.frame(interact6)
interactdf6$attack <- factor(interactdf6$attack,  
                             level=c("white attack", "black attack", "control attack"),    
                             labels=c("white attack", "black attack", "control attack"))   
interactdf6$candidate_race <- factor(interactdf6$candidate_race,
                                     level=c("White cue", "Black cue"),   
                                     labels=c("White cue", "Black cue"))

plot3<-ggplot(data=interactdf6, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Concern")+
  xlab("Attack type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot3

#reliable
dv4 <- recode(mturk$Q2.11, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
table(dv4)

model7 <-lm(dv4~candidate_race + attack, data=mturk)
summary(model7)
model8 <- lm(dv4~candidate_race*attack + HS + RR + pid, data=mturk)
summary(model8)

stargazer(model7, model8,type="text", 
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=TRUE,  
          notes.append = FALSE, 
          omit.stat=c("ser"),   
          star.cutoffs = c(0.05, 0.01, 0.001),  
          header=FALSE) 

reliable_survey <- svyby(~dv4, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")

ggplot(reliable_survey, aes(x=candidate_race, y=dv4)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Perceived reliability", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("White cue", "Black cue")) + 
  theme_bw() + displayprefs

interact8 <- effect('candidate_race*attack', model8, se=TRUE)
interactdf8<-as.data.frame(interact8)
interactdf8$attack <- factor(interactdf8$attack,  
                             level=c("white attack", "black attack", "control attack"),    
                             labels=c("white attack", "black attack", "control attack"))   
interactdf8$candidate_race <- factor(interactdf8$candidate_race,
                                     level=c("White cue", "Black cue"),   
                                     labels=c("White cue", "Black cue"))

plot4<-ggplot(data=interactdf8, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Reliability")+
  xlab("Attack type")+
  ggtitle("Perceived Reliability")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot4

#same values
dv5 <- recode(mturk$Q2.12, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
table(dv5)

lm5 <- lm(dv5~candidate_race + attack + HS + RR + HS*RR, data=mturk)
summary(lm5)

model9 <-lm(dv5~candidate_race + attack, data=mturk)
summary(model9)
model10 <- lm(dv5~candidate_race*attack + HS + RR + pid, data=mturk)
summary(model10)

stargazer(model9, model10,type="text", 
          column.labels = c("Main Effects", "Interaction"), 
          intercept.bottom = FALSE, 
          single.row=TRUE,  
          notes.append = FALSE, 
          omit.stat=c("ser"),   
          star.cutoffs = c(0.05, 0.01, 0.001),  
          header=FALSE) 

values_survey <- svyby(~dv5, ~attack + ~candidate_race, mturk_survey, svymean, na.rm=TRUE, vartype=c("se","ci"))

displayprefs <- theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(), 
                      axis.title.x=element_blank(), 
                      plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"), legend.position="none")

ggplot(values_survey, aes(x=candidate_race, y=dv5)) +
  geom_point() +
  geom_pointrange(aes(ymin=ci_l, ymax=ci_u), size=0.75) +
  facet_grid(.~attack) +
  scale_y_continuous(name="Share the same values", limits=c(0,1)) + 
  scale_x_discrete(name="Candidate racial cue", labels=c("White cue", "Black cue")) + 
  theme_bw() + displayprefs

interact10 <- effect('candidate_race*attack', model10, se=TRUE)
interactdf10<-as.data.frame(interact10)
interactdf10$attack <- factor(interactdf10$attack,  
                             level=c("white attack", "black attack", "control attack"),    
                             labels=c("white attack", "black attack", "control attack"))   
interactdf10$candidate_race <- factor(interactdf10$candidate_race,
                                     level=c("White cue", "Black cue"),   
                                     labels=c("White cue", "Black cue"))

plot5<-ggplot(data=interactdf10, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Shares the Same Values")+
  xlab("Attack type")+
  ggtitle("Shares the Same Values")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot5

#table comparing the 5 whole-sample models
stargazer(model2, model4, model6, model8, model10)

#overalltrust model
trust <- (dv1 + dv2 + dv3 + dv4 + dv5)
table(trust)

trustlm <- lm(trust~candidate_race*attack + HS+ RR + pid, data=mturk)
summary(trustlm)

interact6 <- effect('candidate_race*attack', trustlm, se=TRUE)
interactdf6<-as.data.frame(interact6)
interactdf6$attack <- factor(interactdf6$attack,  
                               level=c("white attack", "black attack", "control attack"),    
                               labels=c("white attack", "black attack", "control attack"))   
interactdf6$candidate_race <- factor(interactdf6$candidate_race,
                             level=c("White cue", "Black cue"),   
                             labels=c("White cue", "Black cue"))
plot6<-ggplot(data=interactdf6, aes(x=attack, y=fit, group=candidate_race))+
  geom_line(size=2, aes(color=candidate_race))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=candidate_race),alpha=.2)+
  ylab("Trust")+
  xlab("Attack Type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot6
