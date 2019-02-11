library(readstata13)
library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)

xd <- read.dta13('WAV_HH_PANEL_MOD_8.02.17.dta')

xd1<- xd %>%
  filter(in_both_waves=="panel") %>%
  mutate(treatcondition = ifelse(survey_treatment==0 & in_kbk=="Non-KbK", "Control",
                                 ifelse(survey_treatment==1 & in_kbk=="Non-KbK", "Treat CV, non-KbK", "Treat CV, KbK")))

table(xd1$treatcondition)
  
xd_timeval <- xd1 %>%
  group_by(base_or_end, treatcondition) %>%
  summarize(mean_time_val_house = mean(time_val_all_house, na.rm=T),
            sd_time_val_house = sd(time_val_all_house, na.rm=T),
            mean_time_val_farm = mean(time_val_all_farm, na.rm=T),
            sd_time_val_farm = sd(time_val_all_farm, na.rm=T),
            mean_time_val_biz = mean(time_val2_end_biz, na.rm=T),
            sd_time_val_biz = sd(time_val2_end_biz, na.rm=T))
write.csv(xd_timeval, "timeval_7292018.csv")

#TOT is the group indicator for treatment on the treated
xd1 <- xd1 %>%
  mutate(TOT = ifelse(treatcondition=="Control", 0,
                      ifelse(treatcondition=="Treat CV, KbK", 1, NA)))
table(xd1$treatcondition, xd1$TOT)

#ITT is the group indicator for intent to treat
xd1 <- xd1 %>%
  mutate(ITT = ifelse(treatcondition=="Control", 0, 1))
table(xd1$treatcondition, xd1$ITT)

xd1 <- xd1 %>%
  mutate(endline = as.factor(ifelse(base_or_end=="endline", 0, 1)))

xd_timeval_endline <- xd1 %>%
  filter(base_or_end == "endline")

xd_timeval_baseline <- xd1 %>%
  filter(base_or_end == "baseline")

##Farm plot area descriptives

xd1 <- xd1 %>%
  mutate(totalplotarea = ifelse(base_or_end=="baseline", farm_plot_area_total_2014lrs, farm_plot_area_total_2016lrs))

summary(xd1$totalplotarea)

xd_cultiv_TOT <- xd1 %>%
  group_by(TOT, base_or_end) %>%
  summarize(mean_farmplot = mean(totalplotarea, na.rm=T),
            sd_farmplot = sd(totalplotarea, na.rm=T))

ggplot()

xd_cultiv_ITT <- xd1 %>%
  group_by(ITT, base_or_end) %>%
  summarize(mean_farmplot = mean(totalplotarea, na.rm=T),
            sd_farmplot = sd(totalplotarea, na.rm=T))

write.csv(xd_cultiv_TOT, "xd_cultiv_TOT.csv")
write.csv(xd_cultiv_ITT, "xd_cultiv_ITT.csv")

xd_base <- xd_timeval_baseline
xd_base <- xd_base %>%
  mutate(totalplotarea = ifelse(base_or_end=="baseline", farm_plot_area_total_2014lrs, farm_plot_area_total_2016lrs))

xd_end <- xd_timeval_endline
xd_end <- xd_end %>%
  mutate(totalplotarea = ifelse(base_or_end=="baseline", farm_plot_area_total_2014lrs, farm_plot_area_total_2016lrs))

#looking at resp risk preference

summary(xd1$resp_pref_risk) #answers question about how much they would bet on something

xd_risk <- xd1 %>%
  group_by(treatcondition, panel_wave) %>%
  summarize(N=n(),
            mean_risk = mean(resp_pref_risk, na.rm=T),
            sd_risk = sd(resp_pref_risk, na.rm=T))

write.csv(xd_risk, "riskpreference_812018.csv")

p1 <- ggplot(xd_risk, aes(x=panel_wave, y=mean_risk, color=treatcondition))+
  geom_line()+
  scale_color_discrete()
