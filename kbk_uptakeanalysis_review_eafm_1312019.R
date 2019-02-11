install.packages('plm')
install.packages('multiwayvcov')
library(dplyr)
library(ggplot2)
library(readstata13)
library(plm)
library(lmtest)
library(multiwayvcov)

xd <- read.dta13("WAV_HH_PANEL_MOD_8.02.17.dta")

xd1 <- xd %>%
  filter(in_both_waves=="panel" & dataset_treatment==1 & base_or_end=="baseline") %>%
  mutate(abletodependent_1 = hhmem_ablebody_num / (hhmem_nonable_depends_num + 1),
         kbk = ifelse(in_kbk=="KbK", 1,0))

##Realistically,in the ratio of able-bodied to non-ablebodied HH members, the denominator should
##be penalized by the number of non-ablebodied HH members who are not of age to work on the farm.
##I think this ends up being between ages 8-12.

summary(xd1$abletodependent_1)
summary(xd1$in_kbk)
table(xd1$kbk)

mean_risk <- mean(xd1$resp_pref_risk)
sd_risk <- sd(xd1$resp_pref_risk)

xd1 <- xd1 %>%
  mutate(risk_z = (resp_pref_risk-mean_risk)/sd_risk)
summary(xd1$risk_z)


#Corresponds to model 1 at the bottom of the 1st page spreadsheet, kbk-uptake_3152018.xls
mod1 <- glm(data=xd1, kbk ~ abletodependent_1 + resp_othergroup_active_other_num + 
              risk_z + resp_self_proc_1 + resp_self_proc_2 + resp_age + resp_gender_fem +
              resp_ed_level, family=binomial(link="logit"))
summary(mod1)
exp(mod1$coefficients) #to obtain odds ratios
#estimates of odds ratios are identical to what stata yielded, although slightly (very slightly) different from the 3/15/2018
#file due to differences in how the economic ratios were constructed this time around.  

mod1.vcovCL <-  cluster.vcov(mod1, xd1$survey_village)
# show the clustered standard errors by indicating the correct var-covar matrix
mod1_clu1 <- coeftest(mod1, mod1.vcovCL)
mod1_clu1

##Creating total plot area estimate

summary(xd1$farm_plot_area_total_2014lrs)
summary(xd1$farm_plot_area_total_2013srs)

xd1$abletoarea <- xd1$hhmem_ablebody_num/(xd1$farm_plot_area_total_2014lrs+1)
hist(xd1$farm_plot_area_total_2014lrs, xlim=c(0,50), breaks=100)
hist(xd1$farm_plot_area_total_2013srs, xlim=c(0,50), breaks=100)
summary(xd1$abletoarea)
hist(xd1$abletoarea, breaks=100)
hist(xd1$hhmem_ablebody_num, breaks=25) #most HHs have about 3 able-bodied members


