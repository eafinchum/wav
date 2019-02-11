library(readstata13)
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)
#library(waffle)

xd <- read.dta13('WAV_HH_PANEL_MOD_8.02.17.dta')

#Baseline Pre-treatment analysis (Control versus Treatment CVs - CPG members at some point)
#Not including KbK membership at this point

xd_baseline <- xd %>%
  filter(base_or_end=="baseline" & in_both_waves=="panel")

t.test(xd_baseline$resp_gender_fem ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_age ~ xd_baseline$survey_treatment)
t.test(xd_baseline$hhmem_num ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_strength ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_farmwork_self ~ xd_baseline$survey_treatment)
t.test(xd_baseline$farm_plot_prop_cult_lrs ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_single ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_married ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_divorced ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_partners ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_widowed ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_separated ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_wealth_equal ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_wealth_resp ~ xd_baseline$survey_treatment)
t.test(xd_baseline$resp_mar_wealth_spouse ~ xd_baseline$survey_treatment)

t.test(xd_endline$hhmem_num ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_strength ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_farmwork_self ~ xd_endline$survey_treatment)
t.test(xd_endline$farm_plot_prop_cult_lrs ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_single ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_married ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_divorced ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_partners ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_widowed ~ xd_endline$survey_treatment)
t.test(xd_endline$resp_mar_separated ~ xd_endline$survey_treatment)

xd_baseline_desc <- xd_baseline %>%
  group_by(survey_treatment) %>%
  summarize(N = n(),
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T))

xd_endline <- xd %>%
  filter(base_or_end=="endline"& in_both_waves=="panel")

xd_endline_desc <- xd_endline %>%
  group_by(survey_treatment) %>%
  summarize(N = n(),
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T))

xd_descriptives_base <- xd %>%
  filter(in_both_waves=="panel", panel_wave==2014) %>%
  group_by(survey_treatment) %>%
  summarize(N = n(), 
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T),
            mean_hhmem = mean(hhmem_num, na.rm=T),
            sd_hhmem = sd(hhmem_num, na.rm=T),
            mean_single = mean(resp_mar_single, na.rm=T),
            sd_single = sd(resp_mar_single, na.rm=T),
            mean_married = mean(resp_mar_married, na.rm=T),
            sd_married = sd(resp_mar_married, na.rm=T),
            mean_divorced = mean(resp_mar_divorced, na.rm=T),
            sd_divorced = sd(resp_mar_divorced, na.rm=T),
            mean_partner = mean(resp_mar_partners, na.rm=T),
            sd_partner = sd(resp_mar_partners, na.rm=T),
            mean_separated = mean(resp_mar_separated, na.rm=T),
            sd_separated = sd(resp_mar_separated, na.rm=T),
            mean_widowed = mean(resp_mar_widowed, na.rm=T),
            sd_widowed = sd(resp_mar_widowed, na.rm=T),
            mean_eqwealth = mean(resp_mar_wealth_equal, na.rm=T),
            sd_eqwealth = sd(resp_mar_wealth_equal, na.rm=T),
            mean_respmorewealth = mean(resp_mar_wealth_resp, na.rm=T),
            sd_respmorewealth = sd(resp_mar_wealth_resp, na.rm=T),
            mean_resplesswealth = mean(resp_mar_wealth_spouse, na.rm=T),
            sd_resplesswealth = sd(resp_mar_wealth_spouse, na.rm=T),
            mean_str = mean(resp_strength, na.rm=T),
            sd_str = sd(resp_strength, na.rm=T),
            mean_farmwk = mean(resp_farmwork_self, na.rm=T),
            sd_farmwk = sd(resp_farmwork_self, na.rm=T),
            mean_cultprop = mean(farm_plot_prop_cult_lrs, na.rm=T),
            sd_cultprop = sd(farm_plot_prop_cult_lrs, na.rm=T))
write.csv(xd_descriptives_base, "xd_desc_base.csv")

xd_descriptives_end <- xd %>%
  filter(in_both_waves=="panel", panel_wave==2016) %>%
  group_by(survey_treatment) %>%
  summarize(N = n(), 
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T),
            mean_hhmem = mean(hhmem_num, na.rm=T),
            sd_hhmem = sd(hhmem_num, na.rm=T),
            mean_single = mean(resp_mar_single, na.rm=T),
            sd_single = sd(resp_mar_single, na.rm=T),
            mean_married = mean(resp_mar_married, na.rm=T),
            sd_married = sd(resp_mar_married, na.rm=T),
            mean_divorced = mean(resp_mar_divorced, na.rm=T),
            sd_divorced = sd(resp_mar_divorced, na.rm=T),
            mean_partner = mean(resp_mar_partners, na.rm=T),
            sd_partner = sd(resp_mar_partners, na.rm=T),
            mean_separated = mean(resp_mar_separated, na.rm=T),
            sd_separated = sd(resp_mar_separated, na.rm=T),
            mean_widowed = mean(resp_mar_widowed, na.rm=T),
            sd_widowed = sd(resp_mar_widowed, na.rm=T),
            mean_eqwealth = mean(resp_mar_wealth_equal, na.rm=T),
            sd_eqwealth = sd(resp_mar_wealth_equal, na.rm=T),
            mean_respmorewealth = mean(resp_mar_wealth_resp, na.rm=T),
            sd_respmorewealth = sd(resp_mar_wealth_resp, na.rm=T),
            mean_resplesswealth = mean(resp_mar_wealth_spouse, na.rm=T),
            sd_resplesswealth = sd(resp_mar_wealth_spouse, na.rm=T),
            mean_str = mean(resp_strength, na.rm=T),
            sd_str = sd(resp_strength, na.rm=T),
            mean_farmwk = mean(resp_farmwork_self, na.rm=T),
            sd_farmwk = sd(resp_farmwork_self, na.rm=T),
            mean_cultprop = mean(farm_plot_prop_cult_lrs, na.rm=T),
            sd_cultprop = sd(farm_plot_prop_cult_lrs, na.rm=T))
write.csv(xd_descriptives_end, 'xd_desc_end.csv')

##Now only for KbK members versus non-KbK members

xd_descriptives_base_kbk <- xd %>%
  filter(in_both_waves=="panel", panel_wave==2014, survey_treatment==1) %>%
  group_by(in_kbk) %>%
  summarize(N = n(), 
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T),
            mean_hhmem = mean(hhmem_num, na.rm=T),
            sd_hhmem = sd(hhmem_num, na.rm=T),
            mean_single = mean(resp_mar_single, na.rm=T),
            sd_single = sd(resp_mar_single, na.rm=T),
            mean_married = mean(resp_mar_married, na.rm=T),
            sd_married = sd(resp_mar_married, na.rm=T),
            mean_divorced = mean(resp_mar_divorced, na.rm=T),
            sd_divorced = sd(resp_mar_divorced, na.rm=T),
            mean_partner = mean(resp_mar_partners, na.rm=T),
            sd_partner = sd(resp_mar_partners, na.rm=T),
            mean_separated = mean(resp_mar_separated, na.rm=T),
            sd_separated = sd(resp_mar_separated, na.rm=T),
            mean_widowed = mean(resp_mar_widowed, na.rm=T),
            sd_widowed = sd(resp_mar_widowed, na.rm=T),
            mean_eqwealth = mean(resp_mar_wealth_equal, na.rm=T),
            sd_eqwealth = sd(resp_mar_wealth_equal, na.rm=T),
            mean_respmorewealth = mean(resp_mar_wealth_resp, na.rm=T),
            sd_respmorewealth = sd(resp_mar_wealth_resp, na.rm=T),
            mean_resplesswealth = mean(resp_mar_wealth_spouse, na.rm=T),
            sd_resplesswealth = sd(resp_mar_wealth_spouse, na.rm=T),
            mean_str = mean(resp_strength, na.rm=T),
            sd_str = sd(resp_strength, na.rm=T),
            mean_farmwk = mean(resp_farmwork_self, na.rm=T),
            sd_farmwk = sd(resp_farmwork_self, na.rm=T),
            mean_cultprop = mean(farm_plot_prop_cult_lrs, na.rm=T),
            sd_cultprop = sd(farm_plot_prop_cult_lrs, na.rm=T))
write.csv(xd_descriptives_base_kbk, "xd_desc_base_kbk.csv")

xd_descriptives_end_kbk <- xd %>%
  filter(in_both_waves=="panel", panel_wave==2016, survey_treatment==1) %>%
  group_by(in_kbk) %>%
  summarize(N = n(), 
            mean_female = mean(resp_gender_fem, na.rm=T),
            sd_female = sd(resp_gender_fem, na.rm=T),
            mean_age = mean(resp_age, na.rm=T),
            sd_age = sd(resp_age, na.rm=T),
            mean_hhmem = mean(hhmem_num, na.rm=T),
            sd_hhmem = sd(hhmem_num, na.rm=T),
            mean_single = mean(resp_mar_single, na.rm=T),
            sd_single = sd(resp_mar_single, na.rm=T),
            mean_married = mean(resp_mar_married, na.rm=T),
            sd_married = sd(resp_mar_married, na.rm=T),
            mean_divorced = mean(resp_mar_divorced, na.rm=T),
            sd_divorced = sd(resp_mar_divorced, na.rm=T),
            mean_partner = mean(resp_mar_partners, na.rm=T),
            sd_partner = sd(resp_mar_partners, na.rm=T),
            mean_separated = mean(resp_mar_separated, na.rm=T),
            sd_separated = sd(resp_mar_separated, na.rm=T),
            mean_widowed = mean(resp_mar_widowed, na.rm=T),
            sd_widowed = sd(resp_mar_widowed, na.rm=T),
            mean_eqwealth = mean(resp_mar_wealth_equal, na.rm=T),
            sd_eqwealth = sd(resp_mar_wealth_equal, na.rm=T),
            mean_respmorewealth = mean(resp_mar_wealth_resp, na.rm=T),
            sd_respmorewealth = sd(resp_mar_wealth_resp, na.rm=T),
            mean_resplesswealth = mean(resp_mar_wealth_spouse, na.rm=T),
            sd_resplesswealth = sd(resp_mar_wealth_spouse, na.rm=T),
            mean_str = mean(resp_strength, na.rm=T),
            sd_str = sd(resp_strength, na.rm=T),
            mean_farmwk = mean(resp_farmwork_self, na.rm=T),
            sd_farmwk = sd(resp_farmwork_self, na.rm=T),
            mean_cultprop = mean(farm_plot_prop_cult_lrs, na.rm=T),
            sd_cultprop = sd(farm_plot_prop_cult_lrs, na.rm=T))
write.csv(xd_descriptives_end_kbk, 'xd_desc_end_kbk.csv')

#Differences between kbk members and nonmembers within treatment CVs

xd_base_treatment <- xd %>%
  filter(panel_wave==2014 & survey_treatment==1 & in_both_waves=="panel")

t.test(xd_base_treatment$resp_gender_fem ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_age ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$hhmem_num ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_strength ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_farmwork_self ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$farm_plot_prop_cult_lrs ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_single ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_married ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_divorced ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_partners ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_widowed ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_separated ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_wealth_equal ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_wealth_resp ~ xd_base_treatment$in_kbk)
t.test(xd_base_treatment$resp_mar_wealth_spouse ~ xd_base_treatment$in_kbk)

xd_end_treatment <- xd %>%
  filter(panel_wave==2016 & survey_treatment==1 & in_both_waves=="panel")

t.test(xd_end_treatment$hhmem_num ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_strength ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_farmwork_self ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$farm_plot_prop_cult_lrs ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_single ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_married ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_divorced ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_partners ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_widowed ~ xd_end_treatment$in_kbk)
t.test(xd_end_treatment$resp_mar_separated ~ xd_end_treatment$in_kbk)

##KbK activities descriptives and analysis

xd_kbk <- xd %>%
  filter(in_kbk=='KbK' & in_both_waves=='panel' & base_or_end=="endline") %>%
  mutate(kbk_rotorder_dec_alpha = ifelse(kbk_rot_order_dec=="alpha",1,0),
         kbk_rotorder_dec_chair = ifelse(kbk_rot_order_dec=="chair",1,0),
         kbk_rotorder_dec_consensus = ifelse(kbk_rot_order_dec=="consensus",1,0),
         kbk_rotorder_dec_need = ifelse(kbk_rot_order_dec=="need",1,0),
         kbk_rotorder_dec_other = ifelse(kbk_rot_order_dec=="other",1,0),
         kbk_rotorder_dec_random = ifelse(kbk_rot_order_dec=="random",1,0),
         kbk_rotorder_dec_record = ifelse(kbk_rot_order_dec=="record_names",1,0))

#first table gives general characteristics of KbKs including meeting frequencies, rules
#and rotation orders of work flow.
xd_kbk_char <- xd_kbk %>%
  summarize(N = n(),
            mean_meet_freq = mean(kbk_meet_freq, na.rm=T),
            sd_meet_freq = sd(kbk_meet_freq, na.rm=T),
            mean_meet_freq_hrs = mean(kbk_meet_freq_hrs, na.rm=T),
            sd_meet_freq_hrs = sd(kbk_meet_freq_hrs, na.rm=T),
            mean_kbk_rules = mean(kbk_rules, na.rm=T),
            sd_kbk_rules = sd(kbk_rules, na.rm=T),
            mean_kbk_rotorder = mean(kbk_rot_order, na.rm=T),
            sd_kbk_rotorder = sd(kbk_rot_order, na.rm=T))

write.csv(xd_kbk_char, "kbk_meetings.csv")

table(xd_kbk$kbk_rot_order_dec)
kbk_dec <- c(`Consensus`=107,`Record Names` = 86,`Random`=34, `Need` = 28,
             `Alphabetical\norder`=18, `Chair`=15, `Other`=7)
waffle(kbk_dec, rows=10, legend_pos = 'bottom',
       title="How KbK rotation orders were decided")

#Changes in meeting frequency based on seasons/months

table(xd_kbk$kbk_meet_more_mons)
xd_months <- xd_kbk %>%
  summarize(N = n(),
            prop_meetmore_jan = mean(kbk_meet_more_jan, na.rm=T),
            sd_meetmore_jan = sd(kbk_meet_more_jan, na.rm=T),
            prop_meetmore_feb = mean(kbk_meet_more_feb, na.rm=T),
            sd_meetmore_feb = sd(kbk_meet_more_feb, na.rm=T),
            prop_meetmore_mar = mean(kbk_meet_more_mar, na.rm=T),
            sd_meetmore_mar = sd(kbk_meet_more_mar, na.rm=T),
            prop_meetmore_apr = mean(kbk_meet_more_apr, na.rm=T),
            sd_meetmore_apr = sd(kbk_meet_more_apr, na.rm=T),
            prop_meetmore_may = mean(kbk_meet_more_may, na.rm=T),
            sd_meetmore_may = sd(kbk_meet_more_may, na.rm=T),
            prop_meetmore_jun = mean(kbk_meet_more_jun, na.rm=T),
            sd_meetmore_jun = sd(kbk_meet_more_jun, na.rm=T),
            prop_meetmore_jul = mean(kbk_meet_more_jul, na.rm=T),
            sd_meetmore_jul = sd(kbk_meet_more_jul, na.rm=T),
            prop_meetmore_aug = mean(kbk_meet_more_aug, na.rm=T),
            sd_meetmore_aug = sd(kbk_meet_more_aug, na.rm=T),
            prop_meetmore_sep = mean(kbk_meet_more_sep, na.rm=T),
            sd_meetmore_sep = sd(kbk_meet_more_sep, na.rm=T),
            prop_meetmore_oct = mean(kbk_meet_more_oct, na.rm=T),
            sd_meetmore_oct = sd(kbk_meet_more_oct, na.rm=T),
            prop_meetmore_nov = mean(kbk_meet_more_nov, na.rm=T),
            sd_meetmore_nov = sd(kbk_meet_more_nov, na.rm=T),
            prop_meetmore_dec = mean(kbk_meet_more_dec, na.rm=T),
            sd_meetmore_dec = sd(kbk_meet_more_dec, na.rm=T))

xd_kbk_meet_more <- as.data.frame(table(xd_kbk$kbk_meet_more_mons))
write.csv(xd_kbk_meet_more, "KbK Meet More.csv")

#Month <- c('January',"February", "March", "April", "May",
#            "June", "July", "August", "September", "October",
#            "Novemeber", "December")
#Frequency <- c(8,7,26,27,11,9,6,8,7,7,7,7)
#xd_kbk_meet_more_tally <- data.frame(Month, Frequency)

xd_kbk <- xd_kbk %>%
  mutate(mm_jan = ifelse(kbk_meet_more_mons=="January" | 
                           kbk_meet_more_mons=="January December" | 
                           kbk_meet_more_mons=="January February" | 
                           kbk_meet_more_mons=="January February March April May June July",1,0),
         mm_feb = ifelse(kbk_meet_more_mons=="February"| 
                           kbk_meet_more_mons=="February April December"| 
                           kbk_meet_more_mons=="February March April"|
                           kbk_meet_more_mons=="February March April May" | 
                           kbk_meet_more_mons=="January February" | 
                           kbk_meet_more_mons=="January February March April May June July", 1, 0),
         mm_mar = ifelse(kbk_meet_more_mons=="February March April" | 
                           kbk_meet_more_mons=="February March April May" | 
                           kbk_meet_more_mons=="February March April May June July"|
                           kbk_meet_more_mons=="March" | 
                           kbk_meet_more_mons=="March April" | 
                           kbk_meet_more_mons=="March April May" | 
                           kbk_meet_more_mons=="March April May June" |
                           kbk_meet_more_mons=="March April May November December" | 
                           kbk_meet_more_mons=="March April October November December"| 
                           kbk_meet_more_mons=="March November", 1, 0),
         mm_apr = ifelse(kbk_meet_more_mons=="April" | 
                           kbk_meet_more_mons=="April May"| 
                           kbk_meet_more_mons=="February April December"|
                           kbk_meet_more_mons=="February March April"|
                           kbk_meet_more_mons=="February March April May"|
                           kbk_meet_more_mons=="January February March April May June July"|
                           kbk_meet_more_mons=="March April"|
                           kbk_meet_more_mons=="March April May"|
                           kbk_meet_more_mons=="March April May June"|
                           kbk_meet_more_mons=="March April May November December"|
                           kbk_meet_more_mons=="March April October November December",1,0))

xd_kbk <- xd_kbk %>%
  mutate(mm_may = ifelse(kbk_meet_more_mons=="April May"|
                           kbk_meet_more_mons=="February March April May"|
                           kbk_meet_more_mons=="January February March April May June July"|
                           kbk_meet_more_mons=="March April May"|
                           kbk_meet_more_mons=="March April May June"|
                           kbk_meet_more_mons=="March April May November December"|
                           kbk_meet_more_mons=="May"|
                           kbk_meet_more_mons=="May June",1,0),
         mm_jun = ifelse(kbk_meet_more_mons=="January February March April May June July" | 
                           kbk_meet_more_mons=="June"|
                           kbk_meet_more_mons=="June July"|
                           kbk_meet_more_mons=="June November"|
                           kbk_meet_more_mons=="March April May June"|
                           kbk_meet_more_mons=="May June",1,0),
         mm_jul = ifelse(kbk_meet_more_mons=="January February March April May June July" |
                           kbk_meet_more_mons=="July"| 
                           kbk_meet_more_mons=="July August"|
                           kbk_meet_more_mons=="July August September" | 
                           kbk_meet_more_mons=="June July",1,0),
         mm_aug = ifelse(kbk_meet_more_mons=="August"|
                           kbk_meet_more_mons=="August September" | 
                           kbk_meet_more_mons=="August September October" |
                           kbk_meet_more_mons=="July August"|
                           kbk_meet_more_mons=="July August September",1,0),
         mm_sep = ifelse(kbk_meet_more_mons=="August September" | 
                           kbk_meet_more_mons=="August September October"| 
                           kbk_meet_more_mons=="July August September" |
                           kbk_meet_more_mons=="September"|
                           kbk_meet_more_mons=="September October",1,0),
         mm_oct = ifelse(kbk_meet_more_mons=="August September October" | 
                           kbk_meet_more_mons=="March April October November December" |
                           kbk_meet_more_mons=="October" |
                           kbk_meet_more_mons=="October November"|
                           kbk_meet_more_mons=="October",1,0),
         mm_nov = ifelse(kbk_meet_more_mons=="October November" | 
                           kbk_meet_more_mons=="March November" | 
                           kbk_meet_more_mons=="March April October November December" |
                           kbk_meet_more_mons=="March April May November December" | 
                           kbk_meet_more_mons=="June November",1,0),
         mm_dec = ifelse(kbk_meet_more_mons=="December"| 
                           kbk_meet_more_mons=="February April December" | 
                           kbk_meet_more_mons=="January December" | 
                           kbk_meet_more_mons=="March April May November December" | 
                           kbk_meet_more_mons=="March April October November December",1,0))
  
table(xd_kbk$mm_dec)

#Causes for changes in frequency of meeting - more or less

xd_kbk_mm_why <- xd_kbk %>%
  summarize(N = n(),
            mean_need = mean(kbk_freq_more_need, na.rm=T),
            mean_notwait = mean(kbk_freq_more_not_wait, na.rm=T),
            mean_benefits = mean(kbk_freq_more_benefits, na.rm=T),
            mean_timing = mean(kbk_freq_more_timing_farm, na.rm=T),
            mean_other = mean(kbk_freq_more_other, na.rm=T))
write.csv(xd_kbk_mm_why, "kbkmeetmorewhy.csv")

xd_kbk_ml_why <- xd_kbk %>%
  summarize(mean_noben = mean(kbk_freq_less_no_benefits, na.rm=T),
            mean_busy = mean(kbk_freq_less_busy, na.rm=T),
            mean_nowork = mean(kbk_freq_less_no_work, na.rm=T),
            mean_timing = mean(kbk_freq_less_timing_farm, na.rm=T),
            mean_other = mean(kbk_freq_less_other, na.rm=T))
write.csv(xd_kbk_ml_why, "kbkmeetlesswhy.csv")

table(xd_kbk$kbk_freq_more_benefits)
table(xd_kbk$kbk_freq_more_not_wait)
table(xd_kbk$kbk_freq_more_need)
table(xd_kbk$kbk_freq_more_timing_farm)
table(xd_kbk$kbk_freq_more_other)

table(xd_kbk$kbk_freq_less_no_benefits)
table(xd_kbk$kbk_freq_less_busy)
table(xd_kbk$kbk_freq_less_no_work)
table(xd_kbk$kbk_freq_less_timing_farm)
table(xd_kbk$kbk_freq_less_other)

#Examining respondent input into KbK activities by gender

table(xd_kbk$kbk_act_benefit_most)

xd_kbk <- xd_kbk %>%
  mutate(kbk_input_any = ifelse(kbk_act_input_resp =="none of the time", 0, 1),
         kbk_benefit_alltypes = ifelse(kbk_act_benefit_most=="all_types",1,0),
         kbk_benefit_construct = ifelse(kbk_act_benefit_most=="construct",1,0),
         kbk_benefit_housework = ifelse(kbk_act_benefit_most=="housework",1,0),
         kbk_benefit_nonfarm_inc_indiv = ifelse(kbk_act_benefit_most=="nonfarm_inc_indiv",1,0),
         kbk_benefit_nonfarm_inc_shared = ifelse(kbk_act_benefit_most=="nonfarm_inc_shared",1,0),
         kbk_benefit_other = ifelse(kbk_act_benefit_most=="other",1,0),
         kbk_benefit_othersfarm = ifelse(kbk_act_benefit_most=="others_farm",1,0),
         kbk_benefit_sharedfarm = ifelse(kbk_act_benefit_most=="shared_farm",1,0),
         kbk_benefit_ww = ifelse(kbk_act_benefit_most=="water_wood",1,0))
         
xd_kbkact_dec_bygender <- xd_kbk %>%
  group_by(resp_gender_fem) %>%
  summarize(mean_input = mean(kbk_input_any, na.rm=T),
            sd_input = sd(kbk_input_any, na.rm=T),
            mean_hh_input = mean(kbk_act_input_hh,na.rm=T),
            sd_hh_input = sd(kbk_act_input_hh, na.rm=T),
            mean_spouse_input = mean(kbk_act_input_spousepartner,na.rm=T),
            sd_spouse_input = sd(kbk_act_input_spousepartner, na.rm=T),
            mean_ben_all = mean(kbk_benefit_alltypes,na.rm=T),
            sd_ben_all = sd(kbk_benefit_alltypes, na.rm=T),
            mean_bencons = mean(kbk_benefit_construct,na.rm=T),
            sd_bencons = sd(kbk_benefit_construct, na.rm=T),
            mean_benhouse = mean(kbk_benefit_housework,na.rm=T),
            sd_benhouse = sd(kbk_benefit_housework, na.rm=T),
            mean_ben_nonfarmincindiv = mean(kbk_benefit_nonfarm_inc_indiv,na.rm=T),
            sd_ben_nonfarmincindiv = sd(kbk_benefit_nonfarm_inc_indiv, na.rm=T),
            mean_ben_nonfarmincshared = mean(kbk_benefit_nonfarm_inc_shared,na.rm=T),
            sd_ben_nonfarmincshared = sd(kbk_benefit_nonfarm_inc_shared, na.rm=T),
            mean_otherben = mean(kbk_benefit_other,na.rm=T),
            sd_otherben = sd(kbk_benefit_other, na.rm=T),
            mean_ben_otherfarm = mean(kbk_benefit_othersfarm,na.rm=T),
            sd_ben_otherfarm = sd(kbk_benefit_othersfarm, na.rm=T),
            mean_ben_sharedfarm = mean(kbk_benefit_sharedfarm,na.rm=T),
            sd_ben_sharedfarm = sd(kbk_benefit_sharedfarm, na.rm=T),
            mean_ben_ww = mean(kbk_benefit_ww,na.rm=T),
            sd_ben_ww = sd(kbk_benefit_ww, na.rm=T))
write.csv(xd_kbkact_dec_bygender, "Kbkactdec_bygender.csv")

t.test(xd_kbk$kbk_input_any~xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_act_input_hh ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_act_input_spousepartner ~ xd_kbk$resp_gender_fem)

t.test(xd_kbk$kbk_benefit_alltypes ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_construct ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_housework ~ xd_kbk$resp_gender_fem)

t.test(xd_kbk$kbk_benefit_nonfarm_inc_shared ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_other ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_othersfarm ~ xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_sharedfarm ~ xd_kbk$resp_gender_fem)


table(xd_kbk$kbk_benefit_nonfarm_inc_indiv, xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_nonfarm_inc_indiv ~ xd_kbk$resp_gender_fem)
prop.test(x=c(0,4), n=c(91,205))

table(xd_kbk$kbk_benefit_ww, xd_kbk$resp_gender_fem)
t.test(xd_kbk$kbk_benefit_ww ~ xd_kbk$resp_gender_fem)
prop.test(x=c(2,15), n=c(91, 205))


#Respondent time savings on KbKs and/or what they did with that time.

xd_kbk <- xd_kbk %>%
  mutate(kbk_xtra_domestic = ifelse(kbk_xtra_time_food==1 | kbk_xtra_time_care==1 | 
                                      kbk_xtra_time_clean==1 | kbk_xtra_time_fetch==1, 1, 0),
         kbk_xtra_farm = ifelse(kbk_xtra_time_hhfarm==1 | kbk_xtra_time_farmcas==1 | 
                                 kbk_xtra_time_farmnocas==1, 1, 0))

xd_kbk_timeuses <- xd_kbk %>%
  group_by(resp_gender_fem) %>%
  summarize(N = n(),
            mean_savetime = mean(kbk_savetime, na.rm=T),
            sd_savetime = sd(kbk_savetime, na.rm=T),
            mean_domestic = mean(kbk_xtra_domestic, na.rm=T),
            sd_domestic = sd(kbk_xtra_domestic, na.rm=T),
            mean_farm= mean(kbk_xtra_farm, na.rm=T),
            sd_farm = sd(kbk_xtra_farm, na.rm=T),
            mean_incact= mean(kbk_xtra_time_inc_act, na.rm=T),
            sd_incact = sd(kbk_xtra_time_inc_act, na.rm=T),
            mean_leisure= mean(kbk_xtra_time_leisure, na.rm=T),
            sd_leisure = sd(kbk_xtra_time_leisure, na.rm=T),
            mean_smallbus = mean(kbk_smallbus, na.rm=T),
            sd_smallbus = sd(kbk_smallbus, na.rm=T))
write.csv(xd_kbk_timeuses, "kbktimeusechng.csv")

xd_kbk <- xd_kbk %>%
  mutate(gender = ifelse(resp_gender_fem==0, "Male", "Female"))

#Difference of proportions tests for the benefit that individuals derived from KbK participation and 
#what they chose to do with the time that they saved by participating in the KbK

table(xd_kbk$kbk_savetime, xd_kbk$gender)
prop.test(x = c(88,200), n = c(91,205), correct=F)

table(xd_kbk$kbk_xtra_domestic, xd_kbk$gender)
prop.test(x = c(67, 91), n = c(89,200), correct=F)
t.test(xd_kbk$kbk_xtra_domestic~ xd_kbk$gender)

table(xd_kbk$kbk_xtra_farm, xd_kbk$gender)
prop.test(x = c(30,24), n = c(200,88), correct=F)

table(xd_kbk$kbk_xtra_time_inc_act, xd_kbk$gender)
prop.test(x = c(43,30), n = c(200,88), correct=F)
t.test(xd_kbk$kbk_xtra_time_inc_act ~ xd_kbk$gender)

table(xd_kbk$kbk_xtra_time_leisure, xd_kbk$gender)
prop.test(x = c(67,31), n = c(200,88), correct=F)
t.test(xd_kbk$kbk_xtra_time_leisure ~ xd_kbk$gender)

table(xd_kbk$kbk_smallbus, xd_kbk$gender)
prop.test(x = c(15,3), n = c(149,64), correct=F)
t.test(xd_kbk$kbk_xtra_time_leisure ~ xd_kbk$gender)

#Differences in perceptions 

xd_dec_join <- xd_kbk %>%
  group_by(gender) %>%
  summarize(N = n(),
            mean_part_savetime = mean(kbk_part_savetime, na.rm=T),
            sd_part_savetime = sd(kbk_part_savetime, na.rm=T),
            mean_part_taskeasy = mean(kbk_part_taskeasy, na.rm=T),
            sd_part_taskeasy = sd(kbk_part_taskeasy, na.rm=T),
            mean_over_alone = mean(kbk_over_alone, na.rm=T),
            sd_over_alone=sd(kbk_over_alone, na.rm=T))

write.csv(xd_dec_join, "kbkjoindec.csv") 

table(xd_kbk$kbk_part_savetime, xd_kbk$gender)
prop.test(x=c(120,57), n=c(205,91), correct=F)

table(xd_kbk$kbk_part_taskeasy, xd_kbk$gender)
prop.test(x=c(85,34), n=c(205,91), correct=F)

#Checking on numbers from the kbk_meet_more question (explaining why we only have ~74 respondents
#to that question)

xd_kbk <- xd %>%
  filter(in_kbk=="KbK" & panel_wave==2016)

table(xd_kbk$kbk_meet_more)
