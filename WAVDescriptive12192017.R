library(foreign)
library(readstata13)
library(dplyr)
library(tidyr)
library(pander)

wav <- read.dta13('WAV_HH_PANEL_12192017.dta')

table(wav$cpg_name)
table(wav$resp_age_cat)

wav <- wav %>%
  mutate(kbk_meet_weekhours = kbk_meet_freq*kbk_meet_freq_hrs,
         kbk_input = ifelse(kbk_act_input_resp=="none of the time",0,1),
         kbk_satisfied = ifelse(kbk_satisfaction==0,0,1))

wav <- wav %>%
  mutate(resp_age_cat = ifelse(resp_age < 35, "18-34",
                               ifelse(resp_age >=35 & resp_age < 65, "35-64", "65+")))

#Descriptives on KBK Participation/Activities subdivided by age and gender
wav_kbk <- wav %>%
  filter(panel_wave==2016) %>%
  group_by(resp_age_cat, resp_gender_fem) %>%
  summarize(Obs = n(),
            mean_activekbk = mean(kbk_active_mem, na.rm=T),
            sd_activekbk = sd(kbk_active_mem, na.rm=T),
            mean_participate = mean(kbk_participate, na.rm=T),
            sd_participate = sd(kbk_participate, na.rm=T),
            mean_kbksize = mean(kbk_mem_num, na.rm=T),
            sd_kbksize = sd(kbk_mem_num, na.rm=T),
            mean_weekhours = mean(kbk_meet_weekhours, na.rm=T),
            sd_weekhours = sd(kbk_meet_freq_hrs, na.rm=T),
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_input_HH = mean(kbk_act_input_hh, na.rm=T),
            sd_input_HH = sd(kbk_act_input_hh, na.rm=T), 
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_savetime = mean(kbk_savetime, na.rm=T),
            sd_savetime = sd(kbk_savetime, na.rm=T),
            mean_smallbiz = mean(kbk_smallbus, na.rm=T),
            sd_smallbiz = sd(kbk_smallbus, na.rm=T),
            mean_part_savetime = mean(kbk_part_savetime, na.rm=T),
            sd_part_savetime = sd(kbk_part_savetime, na.rm=T),
            mean_part_taskeasy = mean(kbk_part_taskeasy, na.rm=T),
            sd_part_taskeasy = sd(kbk_part_taskeasy, na.rm=T),
            mean_overalone = mean(kbk_over_alone, na.rm=T),
            sd_overalone = sd(kbk_over_alone, na.rm=T))
write.csv(wav_kbk, file = "wav_kbk.csv")     
            
#Descriptives on KBK Participation/Activities subdivided CV
wav_kbk1 <- wav %>%
  filter(panel_wave==2016) %>%
  group_by(kbk_cv) %>%
  mutate(kbk_meet_weekhours = kbk_meet_freq*kbk_meet_freq_hrs) %>%
  summarize(Obs = n(),
            mean_activekbk = mean(kbk_active_mem, na.rm=T),
            sd_activekbk = sd(kbk_active_mem, na.rm=T),
            mean_participate = mean(kbk_participate, na.rm=T),
            sd_participate = sd(kbk_participate, na.rm=T),
            mean_kbksize = mean(kbk_mem_num, na.rm=T),
            sd_kbksize = sd(kbk_mem_num, na.rm=T),
            mean_weekhours = mean(kbk_meet_weekhours, na.rm=T),
            sd_weekhours = sd(kbk_meet_weekhours, na.rm=T),
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_input_HH = mean(kbk_act_input_hh, na.rm=T),
            sd_input_HH = sd(kbk_act_input_hh, na.rm=T), 
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_savetime = mean(kbk_savetime, na.rm=T),
            sd_savetime = sd(kbk_savetime, na.rm=T),
            mean_smallbiz = mean(kbk_smallbus, na.rm=T),
            sd_smallbiz = sd(kbk_smallbus, na.rm=T),
            mean_part_savetime = mean(kbk_part_savetime, na.rm=T),
            sd_part_savetime = sd(kbk_part_savetime, na.rm=T),
            mean_part_taskeasy = mean(kbk_part_taskeasy, na.rm=T),
            sd_part_taskeasy = sd(kbk_part_taskeasy, na.rm=T),
            mean_overalone = mean(kbk_over_alone, na.rm=T),
            sd_overalone = sd(kbk_over_alone, na.rm=T))
write.csv(wav_kbk1, file = "wav_kbk1.csv") 

#Descriptives on KBK Participation/Activities subdivided CPG
wav_kbk2 <- wav %>%
  filter(panel_wave==2016) %>%
  group_by(cpg_name) %>%
  summarize(Obs = n(),
            mean_activekbk = mean(kbk_active_mem, na.rm=T),
            sd_activekbk = sd(kbk_active_mem, na.rm=T),
            mean_participate = mean(kbk_participate, na.rm=T),
            sd_participate = sd(kbk_participate, na.rm=T),
            mean_kbksize = mean(kbk_mem_num, na.rm=T),
            sd_kbksize = sd(kbk_mem_num, na.rm=T),
            mean_weekhours = mean(kbk_meet_weekhours, na.rm=T),
            sd_weekhours = sd(kbk_meet_weekhours, na.rm=T),
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_input_HH = mean(kbk_act_input_hh, na.rm=T),
            sd_input_HH = sd(kbk_act_input_hh, na.rm=T), 
            mean_input = mean(kbk_act_input_resp, na.rm=T),
            sd_input = sd(kbk_act_input_resp, na.rm=T),
            mean_savetime = mean(kbk_savetime, na.rm=T),
            sd_savetime = sd(kbk_savetime, na.rm=T),
            mean_smallbiz = mean(kbk_smallbus, na.rm=T),
            sd_smallbiz = sd(kbk_smallbus, na.rm=T),
            mean_part_savetime = mean(kbk_part_savetime, na.rm=T),
            sd_part_savetime = sd(kbk_part_savetime, na.rm=T),
            mean_part_taskeasy = mean(kbk_part_taskeasy, na.rm=T),
            sd_part_taskeasy = sd(kbk_part_taskeasy, na.rm=T),
            mean_overalone = mean(kbk_over_alone, na.rm=T),
            sd_overalone = sd(kbk_over_alone, na.rm=T))
write.csv(wav_kbk2, file = "wav_kbk2.csv") 

#Descriptives on Time Use (by treatment condition)

wav <- wav %>%
  mutate(treatment = ifelse(dataset_treatment == 1 & in_kbk=="KbK", "Treatment, KbK",
                            ifelse(dataset_treatment == 0, "Control", "Treatment Non-KbK")))

##limited to individuals for whom yesterday was a typical day for themselves and their family members
wav_tu_bypanelwave <- wav %>%
  group_by(treatment, panel_wave) %>%
  filter(time_use_typicalyesterday==1 & time_use_typicalyesterday_o==1) %>%
  summarize(Obs = n(),
            mean_propcare = mean(time_use_care_prop, na.rm=T),
            sd_propcare = mean(time_use_care_prop, na.rm=T),
            mean_propclean = mean(time_use_clean_prop, na.rm=T),
            sd_propclean = mean(time_use_clean_prop, na.rm=T),
            mean_propfarmcash = mean(time_use_farm_cash_prop, na.rm=T),
            sd_propfarmcash = mean(time_use_farm_cash_prop, na.rm=T),
            mean_propfarmnocash = mean(time_use_farm_nocash_prop, na.rm=T),
            sd_propfarmnocash = mean(time_use_farm_nocash_prop, na.rm=T),
            mean_propfarmHH = mean(time_use_farm_prop, na.rm=T),
            sd_propfarmHH = mean(time_use_farm_prop, na.rm=T),
            mean_propfetch = mean(time_use_fetch_prop, na.rm=T),
            sd_propfetch = mean(time_use_fetch_prop, na.rm=T),
            mean_propfood = mean(time_use_foodprep_prop, na.rm=T),
            sd_propfood = mean(time_use_foodprep_prop, na.rm=T),
            mean_propincome = mean(time_use_income_prop, na.rm=T),
            sd_propincome = mean(time_use_income_prop, na.rm=T),
            mean_proprest = mean(time_use_rest_prop, na.rm=T),
            sd_proprest = mean(time_use_rest_prop, na.rm=T),
            mean_daylength = mean(day_length, na.rm=T),
            sd_daylength = mean(day_length, na.rm=T),
            mean_laborprop = mean(time_use_labor_prop, na.rm=T),
            sd_laborprop = mean(time_use_labor_prop, na.rm=T),
            mean_hhdaylength = mean(hhmem_day_length, na.rm=T),
            sd_hhdaylength = mean(hhmem_day_length, na.rm=T),
            mean_propcare_HH = mean(time_use_care_prop_o, na.rm=T),
            sd_propcare_HH = mean(time_use_care_prop_o, na.rm=T),
            mean_propclean_HH = mean(time_use_clean_prop_o, na.rm=T),
            sd_propclean_HH = mean(time_use_clean_prop_o, na.rm=T),
            mean_propfarmcash_HH = mean(time_use_farm_cash_prop_o, na.rm=T),
            sd_propfarmcash_HH = mean(time_use_farm_cash_prop_o, na.rm=T),
            mean_propfarmnocash_HH = mean(time_use_farm_nocash_prop_o, na.rm=T),
            sd_propfarmnocash_HH = mean(time_use_farm_nocash_prop_o, na.rm=T),
            mean_propfetch_HH = mean(time_use_fetch_prop_o, na.rm=T),
            sd_propfetch_HH = mean(time_use_fetch_prop_o, na.rm=T),
            mean_propfood_HH = mean(time_use_foodprep_prop_o, na.rm=T),
            sd_propfood_HH = mean(time_use_foodprep_prop_o, na.rm=T),
            mean_propincome_HH = mean(time_use_income_prop_o, na.rm=T),
            sd_propincome_HH = mean(time_use_income_prop_o, na.rm=T),
            mean_proprest_HH = mean(time_use_rest_prop_o, na.rm=T),
            sd_proprest_HH = mean(time_use_rest_prop_o, na.rm=T))

write.csv(wav_tu_bypanelwave, file="wav_desc_timeuse.csv")
  
#Descriptives based on recall by different household members
#first need to organize the household family members 

