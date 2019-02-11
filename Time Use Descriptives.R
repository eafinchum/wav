rm(list=ls())

library(dplyr)
library(ggplot2)
library(pander)
library(readstata13)
library(tidyr)

wav <- read.dta13('WAV_HH_PANEL_MOD_8.02.17.dta')
wav <- wav %>%
  filter(in_both_waves=="panel")

wav <- wav %>%
  mutate(treatment = ifelse(dataset_treatment == 1 & in_kbk=="KbK", "Treatment, KbK",
                       ifelse(dataset_treatment == 0, "Control", "Treatment Non-KbK")))

wav_desc <- wav %>% 
  filter(in_both_waves=="panel") %>%
  group_by(treatment, panel_wave) %>%
  summarize(n_obs = n(),
            farm_help_mean = mean(resp_farmwork_self, na.rm=T), #respondent helped out on farm during masika
            farm_help_sd = sd(resp_farmwork_self, na.rm=T),
            resp_health_mean = mean(resp_health, na.rm=T), #how respondent rates own health
            resp_health_sd = sd(resp_health, na.rm=T),
            resp_group_ldr = mean(resp_group_lead, na.rm=T), #whether the respondent has been a group leader
            resp_group_ldr_sd = sd(resp_group_lead, na.rm=T),
            resp_groupnum_mean = mean(resp_othergroup_active_total_num, na.rm=T), #number of groups respondent is an active member of
            resp_groupnum_sd = sd(resp_othergroup_active_total_num, na.rm=T),
            selfeff1_mean = mean(resp_self_ladder_1, na.rm=T), #perceived ability of self to achieve goals
            selfeff1_sd = sd(resp_self_ladder_1, na.rm=T)) %>%
  mutate(panel = ifelse(panel_wave==2014, "Baseline", "Endline"))

ggplot(xd_desc, aes(x=panel, y=farm_help_mean, group=treatment, fill=treatment))+
  geom_bar(stat='identity')+
  facet_wrap(~treatment)+
  xlab("Panel Wave")+
  ylab("Proportion having helped on farm during masika")

##graph suggests that control and treatment conditions are not changing that much, but individuals in 
##treatment non-kbk work on farm much less during masika

xd$treatment <- ifelse(xd$dataset_treatment == 1 & xd$in_kbk=="KbK", "Treatment, KbK",
                       ifelse(xd$dataset_treatment == 0, "Control", "Treatment Non-KbK"))
pander(table(xd$treatment,xd$panel_wave))

xd_timeuse <- xd %>%
  group_by(treatment, panel_wave, survey_month) %>%
  arrange(survey_month) %>%
  summarize(Obs=n(),
            tot_time_use_clean=mean(time_use_clean_total, na.rm=T),
            prop_time_use_clean=mean(time_use_clean_prop, na.rm=T),
            pct_time_use_clean=mean(time_use_clean_perc, na.rm=T),
            tot_time_use_care=mean(time_use_care_total, na.rm=T),
            prop_time_use_care=mean(time_use_care_prop, na.rm=T),
            pct_time_use_care=mean(time_use_care_perc, na.rm=T),
            tot_time_use_farm_cash=mean(time_use_farm_cash_total, na.rm=T),
            prop_time_use_farm_cash=mean(time_use_farm_cash_prop, na.rm=T),
            pct_time_use_farm_cash=mean(time_use_farm_cash_perc, na.rm=T),
            tot_time_use_farm_nocash=mean(time_use_farm_nocash_total, na.rm=T),
            prop_time_use_farm_nocash=mean(time_use_farm_nocash_prop, na.rm=T),
            pct_time_use_farm_nocash=mean(time_use_farm_nocash_perc, na.rm=T),
            tot_time_use_inc=mean(time_use_income_total, na.rm=T),
            prop_time_use_inc=mean(time_use_income_prop, na.rm=T),
            pct_time_use_inc=mean(time_use_income_perc, na.rm=T),
            tot_time_use_fetch=mean(time_use_fetch_total, na.rm=T),
            prop_time_use_fetch=mean(time_use_fetch_prop, na.rm=T),
            pct_time_use_fetch=mean(time_use_fetch_perc, na.rm=T),
            tot_time_use_rest=mean(time_use_rest_total, na.rm=T),
            prop_time_use_rest=mean(time_use_rest_prop, na.rm=T),
            pct_time_use_rest=mean(time_use_fetch_perc, na.rm=T))

##Need to figure out how to graph by the month in which the respondent was surveyed
table(xd$survey_month)
table(xd$survey_month, xd$panel_wave)

############################################################################################
#KbK activities descriptives by age and gender

wav1 <- read.dta13('endline_kbkdescfile_12122017.dta')

wav1$resp_act_input_resp_1 <- ifelse(wav1$resp_act_input_resp == 0,0,1)

wav_kbkact_desc1 <- wav1 %>%
  filter(in_kbk=="KbK") %>%
  group_by(resp_gender_fem, resp_age_cat) %>%
  summarize(Obs = n(),
            mean_active = mean(kbk_active_mem, na.rm=T),
            sd_active = sd(kbk_active_mem, na.rm=T),
            mean_part = mean(kbk_participate, na.rm=T),
            sd_part = sd(kbk_participate, na.rm=T),
            mean_meetfreq = mean(kbk_meet_freq, na.rm=T),
            sd_meetfreq = sd(kbk_meet_freq, na.rm=T),
            mean_meetmore = mean(kbk_freq_more, na.rm=T),
            sd_meetmore = sd(kbk_freq_more, na.rm=T),
            mean_meetless = mean(kbk_freq_less, na.rm=T),
            sd_meetless = sd(kbk_freq_less, na.rm=T))

write.csv(wav_kbkact_desc1, 'wav_kbk_participate.csv', row.names=T)

############################################################################################

wav1 <- wav1 %>%
  mutate(kbk_act_input_resp_1 = ifelse(kbk_act_input_resp == "none of the time", 0,1))

wav1 <- wav1 %>%
  mutate(kbk_satisfaction_dum = ifelse(kbk_satisfaction == 0, 0,1))

wav_kbkact_desc2 <- wav1 %>%
  filter(in_kbk=="KbK") %>%
  group_by(resp_gender_fem, resp_age_cat) %>%
  summarize(Obs = n(),
            mean_kbk_resp_input = mean(kbk_act_input_resp_1, na.rm=T),
            sd_kbk_resp_input = sd(kbk_act_input_resp_1, na.rm=T),
            mean_kbk_resp_hh = mean(kbk_act_input_hh, na.rm=T),
            sd_kbk_resp_hh = sd(kbk_act_input_hh, na.rm=T),
            mean_kbk_resp_spouse = mean(kbk_act_input_spousepartner, na.rm=T),
            sd_kbk_resp_spouse = sd(kbk_act_input_spousepartner, na.rm=T),
            mean_satisfaction = mean(kbk_satisfaction_dum, na.rm=T),
            sd_satisfaction = sd(kbk_satisfaction_dum, na.rm=T), 
            mean_savetime = mean(kbk_savetime, na.rm=T),
            sd_savetime = sd(kbk_savetime, na.rm=T))

kbk_activities_respturn <- as.data.frame(table(wav1$kbk_act_respturn))    
write.csv(kbk_activities_respturn, 'wav_kbkactivities_respturn.csv', row.names=T)
            
wav_fem <- wav1 %>%
  filter(resp_gender_fem==1)
wav_male <- wav1 %>%
  filter(resp_gender_fem==0)

kbk_activities_respturn_fem <- as.data.frame(table(wav_fem$kbk_act_respturn))   
kbk_activities_respturn_male <- as.data.frame(table(wav_male$kbk_act_respturn))    

write.csv(kbk_activities_respturn_fem, 'wav_kbkactivities_respturn_fem.csv', row.names=T)
write.csv(kbk_activities_respturn_male, 'wav_kbkactivities_respturn_male.csv', row.names=T)

write.csv(wav_kbkact_desc2, 'wav_kbkact_desc2.csv', row.names=T)

########################################################################
#Subdividing KbK level variables by CPG



#########################################################################
#Time use variables, using wav file with both waves

wav_timeuse1 <- wav %>%
  group_by(resp_gender_fem)

wav_kbkact_desc2 <- wav1 %>%
  filter(in_kbk=="KbK") %>%
  group_by(resp_gender_fem, resp_age_cat) %>%
  summarize(Obs = n(),
            mean_
