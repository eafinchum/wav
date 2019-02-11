library(dplyr)
library(ggplot2)
library(ggthemes)
library(pander)
library(readstata13)

xd<-read.dta13('WAV_HH_PANEL_MOD_8.02.17.dta')
xd<-xd %>%
  filter(in_both_waves=="panel")

