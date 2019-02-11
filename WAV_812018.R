library(readstata13)
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)


xd <- read.dta13('WAV_HH_PANEL_MOD_8.02.17.dta')

xd1<- xd %>%
  filter(in_both_waves=="panel") %>%
  mutate(treatcondition = ifelse(survey_treatment==0 & in_kbk=="Non-KbK", "Control",
                                 ifelse(survey_treatment==1 & in_kbk=="Non-KbK", "Treat CV, non-KbK", 
                                        "Treat CV, KbK")))

table(xd1$treatcondition)

summary(xd1$resp_pref_risk) #answers question about how much they would bet on something

#running t-tests to determine differences between
#(1) treatmentKbK versus control conditions at baseline

xd1_base <- xd1 %>%
  filter(panel_wave==2014) %>%
  mutate(treatkbk = ifelse(in_kbk=="KbK" & survey_treatment==1, "Treatment",
                           ifelse(survey_treatment==0, "Control", NA))) %>%
  mutate(SE_ladder_1 = ifelse(resp_self_ladder_1<3, 0, 1),
         SE_ladder_2 = ifelse(resp_self_ladder_2<3, 0, 1),
         SE_ladder_3 = ifelse(resp_self_ladder_3<3, 0, 1),
         SE_ladder_4 = ifelse(resp_self_ladder_4<3, 0, 1))
table(xd1_base$treatkbk)

t.test(xd1_base$resp_pref_risk ~ xd1_base$treatkbk) #not significant at 0.05 level

#(2) between men and women at baseline

t.test(xd1_base$resp_pref_risk ~ xd1_base$resp_gender_fem) #not significant at even 0.1 level, no diff

##Endline differences

xd1_end <- xd1 %>%
  filter(panel_wave==2016) %>%
  mutate(treatkbk = ifelse(in_kbk=="KbK" & survey_treatment==1, "Treatment",
                           ifelse(survey_treatment==0, "Control", NA)))%>%
  mutate(SE_ladder_1 = ifelse(resp_self_ladder_1<3, 0, 1),
         SE_ladder_2 = ifelse(resp_self_ladder_2<3, 0, 1),
         SE_ladder_3 = ifelse(resp_self_ladder_3<3, 0, 1),
         SE_ladder_4 = ifelse(resp_self_ladder_4<3, 0, 1))
table(xd1_end$treatkbk)

t.test(xd1_end$resp_pref_risk ~ xd1_end$treatkbk) #not significant at 0.05 level

#(2) between men and women at endline

t.test(xd1_end$resp_pref_risk ~ xd1_end$resp_gender_fem)

##Intertemporal differences for groups

#treatment(KbK)

xd1_treat <- xd1 %>%
  filter(survey_treatment==1 & in_kbk=="KbK") %>%
  mutate(SE_ladder_1 = ifelse(resp_self_ladder_1<3, 0, 1),
         SE_ladder_2 = ifelse(resp_self_ladder_2<3, 0, 1),
         SE_ladder_3 = ifelse(resp_self_ladder_3<3, 0, 1),
         SE_ladder_4 = ifelse(resp_self_ladder_4<3, 0, 1))

t.test(xd1_treat$resp_pref_risk ~ xd1_treat$panel_wave)

xd1_control <- xd1 %>%
  filter(survey_treatment==0) %>%
  mutate(SE_ladder_1 = ifelse(resp_self_ladder_1<3, 0, 1),
         SE_ladder_2 = ifelse(resp_self_ladder_2<3, 0, 1),
         SE_ladder_3 = ifelse(resp_self_ladder_3<3, 0, 1),
         SE_ladder_4 = ifelse(resp_self_ladder_4<3, 0, 1))

t.test(xd1_control$resp_pref_risk ~ xd1_control$panel_wave)

###############################################################################
xd_risk <- xd1 %>%
  group_by(treatcondition, panel_wave) %>%
  summarize(N=n(),
            mean_risk = mean(resp_pref_risk, na.rm=T),
            sd_risk = sd(resp_pref_risk, na.rm=T))

write.csv(xd_risk, "riskpreference_812018.csv")

p1 <- ggplot(xd_risk, aes(x=panel_wave, y=mean_risk, color=treatcondition))+
  geom_line()+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Units wagered')+
  ggtitle("Risk preferences by treatment condition")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p1
ggsave("risk_bytreatment.png", p1)

xd_risk2 <- xd1 %>%
  group_by(treatcondition, resp_gender_fem, panel_wave) %>%
  summarize(N=n(),
            mean_risk = mean(resp_pref_risk, na.rm=T),
            sd_risk = sd(resp_pref_risk, na.rm=T)) %>%
  mutate(gender = ifelse(resp_gender_fem==0, "Male", "Female"))

write.csv(xd_risk2, "riskbygender.csv")

p2 <- ggplot(xd_risk2, aes(x=panel_wave, y=mean_risk, color=treatcondition))+
  geom_line()+
  facet_grid(.~gender)+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Units wagered')+
  ggtitle("Risk preferences by treatment condition\nand gender")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p2

ggsave('riskbyTCandgender.png', p2)

####################################################################################
####################################################################################
#Exploring respondent self-efficacy questions 1-4
####################################################################################
####################################################################################

table(xd1$treatcondition, xd1$resp_self_ladder_1)

xdse <- xd1 %>%
  mutate(SE_ladder_1 = ifelse(resp_self_ladder_1<3, 0, 1),
         SE_ladder_2 = ifelse(resp_self_ladder_2<3, 0, 1),
         SE_ladder_3 = ifelse(resp_self_ladder_3<3, 0, 1),
         SE_ladder_4 = ifelse(resp_self_ladder_4<3, 0, 1))

table(xdse$SE_ladder_1, xdse$resp_self_ladder_1)

xd_selfefficacy1 <- xd1 %>%
  group_by(treatcondition, panel_wave) %>%
  summarize(N = n(),
            mean_se1 = mean(SE_ladder_1, na.rm=T),
            sd_se1 = sd(SE_ladder_1, na.rm=T),
            mean_se2 = mean(SE_ladder_2, na.rm=T),
            sd_se2 = sd(SE_ladder_2, na.rm=T),
            mean_se3 = mean(SE_ladder_3, na.rm=T),
            sd_se3 = sd(SE_ladder_3, na.rm=T),
            mean_se4 = mean(SE_ladder_4, na.rm=T),
            sd_se4 = sd(SE_ladder_4, na.rm=T))
write.csv(xd_selfefficacy1, "selfeff_byTC.csv")


#Running t-tests and proportion difference tests to determine significant variations in agreement with Self-efficacy statements
#based on gender.

table(xdse$resp_gender_fem)

#Question 1
#Baseline (gender or treatment)

t.test(xd1_base$SE_ladder_1~xd1_base$resp_gender_fem) 
table(xd1_base$SE_ladder_1, xd1_base$resp_gender_fem)
prop.test(x=c(212,340), n=c(387,657), correct=F)

t.test(xd1_base$SE_ladder_1~xd1_base$treatkbk) 
table(xd1_base$SE_ladder_1, xd1_base$treatkbk)
prop.test(x=c(270,146), n=c(484,293), correct=F)

#Question 2
#Baseline (gender or treatment)

t.test(xd1_base$SE_ladder_2~xd1_base$resp_gender_fem) 
table(xd1_base$SE_ladder_2, xd1_base$resp_gender_fem)
prop.test(x=c(212,311), n=c(387,657), correct=F)

t.test(xd1_base$SE_ladder_2~xd1_base$treatkbk) 
table(xd1_base$SE_ladder_2, xd1_base$treatkbk)
prop.test(x=c(257,143), n=c(482,293), correct=F)

#Question 3
#Baseline (gender or treatment)

t.test(xd1_base$SE_ladder_3~xd1_base$resp_gender_fem) 
table(xd1_base$SE_ladder_3, xd1_base$resp_gender_fem)
prop.test(x=c(235,362), n=c(387,655), correct=F)

t.test(xd1_base$SE_ladder_3~xd1_base$treatkbk) 
table(xd1_base$SE_ladder_3, xd1_base$treatkbk)
prop.test(x=c(288,165), n=c(481,294), correct=F)

#Question 4 does not exist at endline!
#Baseline (gender or treatment)

##Significance tests for endline values

#Question 1

t.test(xd1_end$SE_ladder_1~xd1_end$resp_gender_fem) #signif. at  p < 0.05
table(xd1_end$SE_ladder_1, xd1_end$resp_gender_fem)
prop.test(x=c(246,377), n=c(386,660), correct=F) #signif. at  p < 0.05

t.test(xd1_end$SE_ladder_1~xd1_end$treatkbk) #NS
table(xd1_end$SE_ladder_1, xd1_end$treatkbk)
prop.test(x=c(289,189), n=c(484,293), correct=F) #NS

#Question 2

t.test(xd1_end$SE_ladder_2~xd1_end$resp_gender_fem) #NS
table(xd1_end$SE_ladder_2, xd1_end$resp_gender_fem)
prop.test(x=c(215,348), n=c(385,661), correct=F) #neither t-test nor difference of proportions tests are significant in the endline

t.test(xd1_end$SE_ladder_2~xd1_end$treatkbk) #NS
table(xd1_end$SE_ladder_2, xd1_end$treatkbk)
prop.test(x=c(258,175), n=c(484,295), correct=F) #NS

#Question 3

t.test(xd1_end$SE_ladder_3~xd1_end$resp_gender_fem) 
table(xd1_end$SE_ladder_3, xd1_end$resp_gender_fem) #significant at p < 0.05
prop.test(x=c(275,427), n=c(387,659), correct=F)

t.test(xd1_end$SE_ladder_3~xd1_end$treatkbk) 
table(xd1_end$SE_ladder_3, xd1_end$treatkbk)
prop.test(x=c(331,208), n=c(485,295), correct=F)

#Question 4

t.test(xd1_end$SE_ladder_4~xd1_end$resp_gender_fem) #significant at p < 0.0001
table(xd1_end$SE_ladder_4, xd1_end$resp_gender_fem)
prop.test(x=c(312,406), n=c(369,587), correct=F)

t.test(xd1_base$SE_ladder_4~xd1_base$treatkbk) 
table(xd1_base$SE_ladder_4, xd1_base$treatkbk)
prop.test(x=c(257,143), n=c(482,293), correct=F)

###Intertemporal differences within treatment and control groups
t.test(xd1_treat$SE_ladder_1 ~ xd1_treat$panel_wave) # p<0.001
t.test(xd1_treat$SE_ladder_2 ~ xd1_treat$panel_wave) # p<0.05
t.test(xd1_treat$SE_ladder_3 ~ xd1_treat$panel_wave) # p<0.001

t.test(xd1_control$SE_ladder_1 ~ xd1_control$panel_wave) #NS
t.test(xd1_control$SE_ladder_2 ~ xd1_control$panel_wave) #NS
t.test(xd1_control$SE_ladder_3 ~ xd1_control$panel_wave) # p < 0.01

table(xd1_treat$SE_ladder_1, xd1_treat$panel_wave)
prop.test(x = c(146,189), n=c(293,296)) 

p_se1<- ggplot(xd_selfefficacy1, aes(x=panel_wave, y=mean_se1, color=treatcondition))+
  geom_line()+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Risk preferences by treatment condition")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p_se1

p_se2<- ggplot(xd_selfefficacy1, aes(x=panel_wave, y=mean_se2, color=treatcondition))+
  geom_line()+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Risk preferences by treatment condition")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p_se2

p_se3<- ggplot(xd_selfefficacy1, aes(x=panel_wave, y=mean_se3, color=treatcondition))+
  geom_line()+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Risk preferences by treatment condition")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p_se3

#need new plot layout for the last self-efficacy ladder

xd_selfefficacy2 <- xd1 %>%
  group_by(treatcondition, resp_gender_fem, panel_wave) %>%
  summarize(N = n(),
            mean_se1 = mean(SE_ladder_1, na.rm=T),
            sd_se1 = sd(SE_ladder_1, na.rm=T),
            mean_se2 = mean(SE_ladder_2, na.rm=T),
            sd_se2 = sd(SE_ladder_2, na.rm=T),
            mean_se3 = mean(SE_ladder_3, na.rm=T),
            sd_se3 = sd(SE_ladder_3, na.rm=T),
            mean_se4 = mean(SE_ladder_4, na.rm=T),
            sd_se4 = sd(SE_ladder_4, na.rm=T)) %>%
  mutate(gender = ifelse(resp_gender_fem==0, "Male", "Female"))
write.csv(xd_selfefficacy2, "selfeff_byTCandgender.csv")  

p_se1<- ggplot(xd_selfefficacy2, aes(x=panel_wave, y=mean_se1, color=treatcondition))+
  geom_line()+
  facet_grid(.~gender)+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Self-efficacy ladder statement 1:\nI am able to achieve most goals that I set for myself.")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'none')
p_se1

p_se2<- ggplot(xd_selfefficacy2, aes(x=panel_wave, y=mean_se2, color=treatcondition))+
  geom_line()+
  facet_grid(.~gender)+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Self-efficacy statement 2:\nI believe that I can succeed at most any endeavor to\nwhich I set my mind.")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'none')
p_se2

p_se3<- ggplot(xd_selfefficacy2, aes(x=panel_wave, y=mean_se3, color=treatcondition))+
  geom_line()+
  facet_grid(.~gender)+
  guides(color=guide_legend(title="Treatment\nCondition"))+
  xlab('Year')+
  ylab('Mean proportion of respondents\nin agreement')+
  ggtitle("Self-efficacy statement 3:\nI feel comfortable speaking up in public village or subvillage meetings about plans and activities in our village")+
  scale_x_continuous(breaks=c(2014,2016))+
  theme(legend.position = 'bottom')
p_se3

#########Household decision-making differences
t.test(xd1_end$dec_crop_level_resp~xd1_end$resp_gender_fem) #NS
t.test(xd1_end$dec_crop_level_resp~xd1_end$treatkbk) #NS

table(xd1_end$dec_crop_resp, xd1_end$resp_gender_fem)
prop.test(x=c(21,37), n=c(23,44), correct=T)

t.test(xd1_end$dec_investment_level_resp~xd1_end$resp_gender_fem) #p < 0.01
t.test(xd1_base$dec_investment_level_resp~xd1_base$resp_gender_fem) #p < 0.01
t.test(xd1_treat$dec_investment_level_resp~xd1_treat$panel_wave) #NS
t.test(xd1_control$dec_investment_level_resp~xd1_control$panel_wave) #NS

t.test(xd1_end$dec_investment_level_resp~xd1_end$treatkbk) #NS
t.test(xd1_base$dec_investment_level_resp~xd1_base$treatkbk) #NS


t.test(xd1_end$dec_timeuse_level_resp~xd1_end$resp_gender_fem) # p < 0.05
t.test(xd1_base$dec_timeuse_level_resp~xd1_base$resp_gender_fem) #NS
t.test(xd1_treat$dec_timeuse_level_resp~xd1_treat$panel_wave) #NS
t.test(xd1_control$dec_timeuse_level_resp~xd1_control$panel_wave) #p < 0.05

t.test(xd1_end$dec_timeuse_level_resp~xd1_end$treatkbk) #NS
