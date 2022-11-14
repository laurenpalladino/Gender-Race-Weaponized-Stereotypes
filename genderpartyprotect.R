library(readr)
library(dplyr)
library(survey)
library(stargazer)
library(effects)
library(ggplot2)
library(margins)
library(sjPlot)
mturkdata <- read_csv("Downloads/mturkdata.csv")
names(mturkdata)
attach(mturkdata)

pid <-NA
#strong dem
pid[PID == 1 & PIDstrong == 1] <- 1
#weak dem
pid[PID == 1 & PIDstrong == 2] <- 2
#lean dem
pid[PID == 3 & PIDlean == 2] <- 3
pid[PID == 4 & PIDlean == 2] <- 3
#true ind
pid[PID == 3 & PIDlean == 3] <- 4
pid[PID == 4 & PIDlean == 3] <- 4
#lean rep
pid[PID == 3 & PIDlean == 1] <- 5
pid[PID == 4 & PIDlean == 1] <- 5
#weak rep
pid[PID == 2 & PIDstrong == 2]  <- 6
#strong rep
pid[PID == 2 & PIDstrong == 1]  <- 7
table(pid)

democrats = subset(mturkdata, pid<=3)
republicans = subset(mturkdata, pid>=5)

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
HS <- (HS1 + HS2 + HS3)
summary(HS)

#racial resentment
RR1<-as.numeric(mturkdata$RR1)
RR1 <-dplyr::recode(mturkdata$RR1, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
RR2<-as.numeric(mturkdata$RR2)
RR2 <-dplyr::recode(mturkdata$RR2, "1"=1, "2"=2, "4"=3, "5"=4, "6"=5)
RR3<-as.numeric(mturkdata$RR3)
RR3 <-dplyr::recode(mturkdata$RR3, "1"=1, "2"=2, "4"=3, "5"=4, "6"=5)
RR4<-as.numeric(mturkdata$RR4)
RR4 <-dplyr::recode(mturkdata$RR4, "1"=5, "2"=4, "4"=3, "5"=2, "6"=1)
RR <- (RR1 + RR2 + RR3 + RR4)
summary(RR)

#recode gender (1=men, 2=women, 3=non-binary)
gender <-NA
gender[Gender == 16] <- 1
gender[Gender == 17] <- 2
gender[Gender == 18] <- 3

table(Race)

#recode age
age <-NA
age[Age == 2] <- 1
age[Age == 3] <- 2
age[Age == 4] <- 3
age[Age == 5] <- 4
age[Age == 6] <- 5
age[Age == 7] <- 6
age[Age == 8] <- 7

#recode educ
educ<-NA
educ[Educ == 1] <-1
educ[Educ == 2] <-2
educ[Educ == 3] <-3
educ[Educ == 4] <-4
educ[Educ == 5] <-5

#competent
dv1 <-dplyr::recode(mturkdata$competent, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dv1)

model1 <-lm(dv1~party + attack, data=mturkdata)
summary(model1)
model2 <- lm(dv1~party*attack + pid + HS + RR + gender + age + educ, data=mturkdata)
summary(model2)

#marginal effects
m1 <- margins(model1)
summary(m1)
plot_model(model2, type="pred", terms="party")

#graph
interact <- effect('party*attack', model2, se=TRUE)
interactdf<-as.data.frame(interact)
interactdf$attack <- factor(interactdf$attack,  
                            level=c("political attack", "gender attack"),    
                            labels=c("political attack", "gender attack"))   
interactdf$party <- factor(interactdf$party,
                                    level=c("in party", "out party", "no party"),   
                                    labels=c("in party", "out party", "no party"))
plot1<-ggplot(data=interactdf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
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
dv2 <-dplyr::recode(mturkdata$honest, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dv2)

model3 <-lm(dv2~party + attack, data=mturkdata)
summary(model3)
model4 <- lm(dv2~party*attack + pid + HS + RR, data=mturkdata)
summary(model4)

#marginal effects
m2 <- margins(model3)
summary(m2)
plot_model(model4, type="pred", terms="party")

#graph
interactdv2 <- effect('party*attack', model4, se=TRUE)
interactdv2df<-as.data.frame(interactdv2)
interactdv2df$attack <- factor(interactdv2df$attack,  
                            level=c("political attack", "gender attack"),    
                            labels=c("political attack", "gender attack"))   
interactdv2df$party <- factor(interactdv2df$party,
                           level=c("in party", "out party", "no party"),   
                           labels=c("in party", "out party", "no party"))
plot2<-ggplot(data=interactdv2df, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Honesty")+
  xlab("Attack type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot2

#concern for constituents
dv3 <-dplyr::recode(mturkdata$concern, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dv3)

model5 <-lm(dv3~party + attack, data=mturkdata)
summary(model5)
model6 <- lm(dv3~party*attack + pid + HS + RR, data=mturkdata)
summary(model6)

#marginal effects
m3 <- margins(model5)
summary(m3)
plot_model(model6, type="pred", terms="party")


#graph
interactdv3 <- effect('party*attack', model6, se=TRUE)
interactdv3df<-as.data.frame(interactdv3)
interactdv3df$attack <- factor(interactdv3df$attack,  
                               level=c("political attack", "gender attack"),    
                               labels=c("political attack", "gender attack"))   
interactdv3df$party <- factor(interactdv3df$party,
                              level=c("in party", "out party", "no party"),   
                              labels=c("in party", "out party", "no party"))
plot3<-ggplot(data=interactdv3df, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
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
dv4 <-dplyr::recode(mturkdata$reliable, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dv4)

model7 <-lm(dv4~party + attack, data=mturkdata)
summary(model5)
model8 <- lm(dv4~party*attack + pid + HS + RR, data=mturkdata)
summary(model8)

#marginal effects
m4 <- margins(model7)
summary(m4)
plot_model(model8, type="pred", terms="party")

#graph
interactdv4 <- effect('party*attack', model8, se=TRUE)
interactdv4df<-as.data.frame(interactdv4)
interactdv4df$attack <- factor(interactdv4df$attack,  
                               level=c("political attack", "gender attack"),    
                               labels=c("political attack", "gender attack"))   
interactdv4df$party <- factor(interactdv4df$party,
                              level=c("in party", "out party", "no party"),   
                              labels=c("in party", "out party", "no party"))
plot4<-ggplot(data=interactdv4df, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Reliability")+
  xlab("Attack type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot4

#same values
dv5 <-dplyr::recode(mturkdata$values, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dv5)

model9 <-lm(dv5~party + attack, data=mturkdata)
summary(model9)
model10 <- lm(dv5~party*attack + pid + HS + RR, data=mturkdata)
summary(model10)

#marginal effects
m5 <- margins(model9)
summary(m5)
plot_model(model10, type="pred", terms="party")

#graph
interactdv5 <- effect('party*attack', model10, se=TRUE)
interactdv5df<-as.data.frame(interactdv5)
interactdv5df$attack <- factor(interactdv5df$attack,  
                               level=c("political attack", "gender attack"),    
                               labels=c("political attack", "gender attack"))   
interactdv5df$party <- factor(interactdv5df$party,
                              level=c("in party", "out party", "no party"),   
                              labels=c("in party", "out party", "no party"))
plot5<-ggplot(data=interactdv5df, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Shares the Same Values")+
  xlab("Attack type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot5

stargazer(model2, model4, model6, model8, model10)

#how likely are you to vote for susan?
dvvote <-dplyr::recode(mturkdata$vote, "1"=0, "2"=0.25, "3"=0.5, "4"=0.75, "5"=1)
table(dvvote)

votemodel <-lm(dvvote~party + attack, data=mturkdata)
summary(votemodel)

votemodel2 <- lm(dvvote~party*attack + pid + HS + RR, data=mturkdata)
summary(votemodel2)

stargazer(votemodel2)

#marginal effects
m6 <- margins(votemodel)
summary(m6)
plot_model(votemodel2, type="pred", terms="party")

#graph
interactvote <- effect('party*attack', votemodel2, se=TRUE)
interactvotedf<-as.data.frame(interactvote)
interactvotedf$attack <- factor(interactvotedf$attack,  
                               level=c("political attack", "gender attack"),    
                               labels=c("political attack", "gender attack"))   
interactvotedf$party <- factor(interactvotedf$party,
                              level=c("in party", "out party", "no party"),   
                              labels=c("in party", "out party", "no party"))
plot6<-ggplot(data=interactvotedf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Vote Likelihood")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot6

#who is more capable?
capable <-dplyr::recode(mturkdata$pref, "1"=0, "2"=1, "3"=0.5)
#1=man #2=susan #3=equally capable
table(capable)

capablemodel <-lm(capable~party + attack, data=mturkdata)
summary(capablemodel)

capablemodel2 <- lm(capable~party*attack + pid + HS + RR, data=mturkdata)
summary(capablemodel2)

#marginal effects
m7 <- margins(capablemodel)
summary(m7)
plot_model(capablemodel2, type="pred", terms="party")


#graph
interactcapable <- effect('party*attack', capablemodel2, se=TRUE)
interactcapabledf<-as.data.frame(interactcapable)
interactcapabledf$attack <- factor(interactcapabledf$attack,  
                                level=c("political attack", "gender attack"),    
                                labels=c("political attack", "gender attack"))   
interactcapabledf$party <- factor(interactcapabledf$party,
                               level=c("in party", "out party", "no party"),   
                               labels=c("in party", "out party", "no party"))
plot7<-ggplot(data=interactcapabledf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Views Susan as more Capable")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot7

#are the attacks against susan justified?
justified <-dplyr::recode(mturkdata$justify, "1"=1, "2"=0.75, "3"=0.5, "4"=0.25, "5"=0)
#1=definitely no #5=definitely yes
table(justified)

justifymodel <-lm(justified~party + attack, data=mturkdata)
summary(justifymodel)

justifymodel2 <- lm(justified~party*attack + pid + HS + RR, data=mturkdata)
summary(justifymodel2)

#marginal effects
m8 <- margins(justifymodel)
summary(m8)
plot_model(justifymodel2, type="pred", terms="party")

#graph
interactjustify <- effect('party*attack', justifymodel2, se=TRUE)
interactjustifydf<-as.data.frame(interactjustify)
interactjustifydf$attack <- factor(interactjustifydf$attack,  
                                level=c("political attack", "gender attack"),    
                                labels=c("political attack", "gender attack"))   
interactjustifydf$party <- factor(interactjustifydf$party,
                               level=c("in party", "out party", "no party"),   
                               labels=c("in party", "out party", "no party"))
plot8<-ggplot(data=interactjustifydf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Are the attacks justified?")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot8

#feeling thermometer
ft<-as.numeric(mturkdata$FT_41)

ftmodel <-lm(ft~party + attack, data=mturkdata) 
summary(ftmodel)

ftmodel2 <- lm(ft~party*attack + pid + HS + RR + gender + age + educ, data=mturkdata)
summary(ftmodel2)

stargazer(ftmodel2)

#marginal effects
m9 <- margins(ftmodel)
summary(m9)
plot_model(ftmodel2, type="pred", terms="party")

#graph
interactft <- effect('party*attack', ftmodel2, se=TRUE)
interactftdf<-as.data.frame(interactft)
interactftdf$attack <- factor(interactftdf$attack,  
                                   level=c("political attack", "gender attack"),    
                                   labels=c("political attack", "gender attack"))   
interactftdf$party <- factor(interactftdf$party,
                                  level=c("in party", "out party", "no party"),   
                                  labels=c("in party", "out party", "no party"))
plot9<-ggplot(data=interactftdf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Feeling Thermometer toward Susan")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
plot9

stargazer(ftmodel2, votemodel2, justifymodel2)

#composite measure of trust
trust <- (dv1*0.20 + dv2*0.20 + dv3*0.20 + dv4*0.20 + dv5*0.20)
table(trust)

trustmodel <-lm(trust~party + attack, data=mturkdata) 
trustlm <- lm(trust~party*attack + HS+ RR + pid + age + gender + educ, data=mturkdata)
summary(trustlm)

#marginal effects
m10 <- margins(trustmodel)
summary(m10)
plot_model(trustlm, type="pred", terms="party")


interacttrust <- effect('party*attack', trustlm, se=TRUE)
interacttrustdf<-as.data.frame(interacttrust)
interacttrustdf$attack <- factor(interacttrustdf$attack,  
                                 level=c("political attack", "gender attack"),    
                                 labels=c("Political attack", "Gender attack"))
interacttrustdf$party <- factor(interacttrustdf$party,
                                     level=c("in party", "out party", "no party"),   
                                     labels=c("In party", "Out party", "No party"))
trustplot<-ggplot(data=interacttrustdf, aes(x=attack, y=fit, group=party))+
  geom_line(size=2, aes(color=party))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=party),alpha=.2)+
  ylab("Trust")+
  xlab("Attack Type")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
trustplot
