library(dplyr)
library(ggplot2)
library(foreign)
#library(readstata13)
library(data.table)
library(tidyverse)
library(readr)
library(DBI)
library(tidyr)
library(rlang)
library('fastLink')
rm(list=ls())

buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")
bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))
rm(buyout, bo_zip_yr)

load(paste0("./output/fullsample_expanded_20002017.RDa"))
geocoded = read.csv("./data/uniqueAddress_20002017_GeoCoded_joinCensusTract2000_ACS2000_NFHLall.csv")

for (y in 12:length(yr)) {
  cohort = Full_sample_expanded[Full_sample_expanded$EVENTYEAR==yr[y],]
  cohort$ZIP = as.numeric(cohort$ZIP)
  
  cohort = left_join(cohort, geocoded, by=c("ADDRESS"="user_address","CITY"="user_city", "STATE"="user_state", "ZIP"= "user_zip"))
  write_dta(cohort, paste0("./output/fullsample_expanded_",yr[y],"_geocoded_ACS.dta"))

}




Full_sample_expanded$s1901_2010_c01_012e = as.numeric(Full_sample_expanded$s1901_2010_c01_012e)
Full_sample_expanded = Full_sample_expanded %>%
  mutate(BUYOUT_HH_Mean_income = ifelse(EVENTYEAR==0,s1901_2010_c01_012e,NA)) %>%
  mutate(N=1)

Full_sample_expanded = Full_sample_expanded %>%
  group_by(PID) %>%
  fill(BUYOUT_HH_Mean_income,  .direction = 'down') %>%
  fill(BUYOUT_HH_Mean_income,  .direction = 'up')

Full_sample_expanded = Full_sample_expanded %>%
  mutate(higherBUYOUT_HH_Mean_income = ifelse(BUYOUT_HH_Mean_income>s1901_2010_c01_012e, 1,0))   

collapse_cohort = Full_sample_expanded %>%
  group_by(EVENTTIME, Treated, EVENTYEAR) %>%
  summarise(mean_move_address = mean(moved_address, na.rm=TRUE),
            mean_move_zip = mean(MOVED_ZIP,  na.rm=TRUE),
            mean_move_state = mean(MOVED_STATE, na.rm=TRUE),
            mean_higher_s1901_2010_c01_012e = mean(higherBUYOUT_HH_Mean_income, na.rm=TRUE),
            mean_s1901_2010_c01_012e= mean(s1901_2010_c01_012e,  na.rm=TRUE),
            sd_move_address = sd(moved_address, na.rm=TRUE), 
            sd_move_zip  = sd(MOVED_ZIP, na.rm=TRUE),
            sd_move_state = sd(MOVED_STATE, na.rm=TRUE),
            sd_higher_s1901_2010_c01_012e = sd(higherBUYOUT_HH_Mean_income, na.rm=TRUE), 
            sd_s1901_2010_c01_012e = sd(s1901_2010_c01_012e, na.rm=TRUE),
            n = n())

collapse = Full_sample_expanded %>%
  group_by(EVENTTIME, Treated) %>%
  summarise(mean_move_address = mean(moved_address, na.rm=TRUE),
            mean_move_zip = mean(MOVED_ZIP,  na.rm=TRUE),
            mean_move_state = mean(MOVED_STATE, na.rm=TRUE),
            mean_SFHA = mean(SFHA, na.rm=TRUE),
            mean_s1901_2010_c01_012e= mean(s1901_2010_c01_012e,  na.rm=TRUE),
            sd_move_address = sd(moved_address, na.rm=TRUE), 
            sd_move_zip  = sd(MOVED_ZIP, na.rm=TRUE),
            sd_move_state = sd(MOVED_STATE, na.rm=TRUE),
            sd_higher_s1901_2010_c01_012e = sd(higherBUYOUT_HH_Mean_income, na.rm=TRUE), 
            sd_s1901_2010_c01_012e = sd(s1901_2010_c01_012e, na.rm=TRUE),
            n = n())

Treated = collapse[collapse$Treated==1 & collapse$EVENTTIME>=-15, ]
Control = collapse[collapse$Treated==0 & collapse$EVENTTIME>=-15, ]

df = as.data.frame(Treated[Treated$EVENTTIME>=-15, 1:3])
df = df[,-2]

df$diff_mean_s1901_2010_c01_012e = Treated$mean_s1901_2010_c01_012e - Control$mean_s1901_2010_c01_012e
df$ci1_mean_s1901_2010_c01_012e = df$diff_mean_s1901_2010_c01_012e - 1.96*(sqrt( ((Treated$sd_s1901_2010_c01_012e^2)/Treated$n) + ((Control$sd_s1901_2010_c01_012e^2)/Control$n))) 
df$ci2_mean_s1901_2010_c01_012e = df$diff_mean_s1901_2010_c01_012e + 1.96*(sqrt( ((Treated$sd_s1901_2010_c01_012e^2)/Treated$n) + ((Control$sd_s1901_2010_c01_012e^2)/Control$n))) 


df$EVENTYEAR = as.factor(df$EVENTYEAR)
ggplot(df) + 
  geom_line(aes(x=EVENTTIME, y=diff_mean_s1901_2010_c01_012e)) +
   geom_ribbon(aes(ymin=ci1_mean_s1901_2010_c01_012e,ymax=ci2_mean_s1901_2010_c01_012e, x=EVENTTIME), 
              alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Mean Census Tract Household Income, 2010", 
       title ="Difference in Census Tract Mean Household Income, After Disaster",
       colour = "Buyout Year") +
  theme_classic() + theme(panel.grid = element_blank()) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  #scale_y_continuous(limits = c(0, .48), 
  #                   breaks = c(0,.1,.2,.3,.4,.5)) +
  annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 12000,
           alpha = .1,fill = "blue") +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)

ggplot(collapse) + 
  geom_line(aes(x=EVENTTIME, y=mean_s1901_2010_c01_012e, group=Treated, color=Treated)) +
 # geom_line(aes(x = EVENTTIME, y=address.ci1, group=Treated, color=Treated), 
  #          width=.1, linetype="dotted") +
  #geom_line( aes(x = EVENTTIME, y=address.ci2, group=Treated, color=Treated), 
  #           width=.1,  linetype="dotted") +
  labs(x = "Event Time (Years)", y = "Mean Census Tract Household Income, 2010", 
       title ="Mean Household Income",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank()) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  #scale_y_continuous(limits = c(0, .48), 
  #                   breaks = c(0,.1,.2,.3,.4,.5)) +
  annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 60000,
           alpha = .1,fill = "blue") +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)

Treated$ci1_s1901_2010_c01_012e = Treated$mean_s1901_2010_c01_012e - 1.96*(Treated$sd_s1901_2010_c01_012e/sqrt(Treated$n))
Treated$ci2_s1901_2010_c01_012e= Treated$mean_s1901_2010_c01_012e + 1.96*(Treated$sd_s1901_2010_c01_012e/sqrt(Treated$n))
Control$ci1_s1901_2010_c01_012e = Control$mean_s1901_2010_c01_012e - 1.96*(Control$sd_s1901_2010_c01_012e/sqrt(Control$n))
Control$ci2_s1901_2010_c01_012e= Control$mean_s1901_2010_c01_012e + 1.96*(Control$sd_s1901_2010_c01_012e/sqrt(Control$n))
ggplot() + 
  geom_line(data=Treated, aes(x=EVENTTIME, y=mean_s1901_2010_c01_012e, color="tomato3")) +
  geom_ribbon(data=Treated, aes(ymin=ci1_s1901_2010_c01_012e, ymax=ci2_s1901_2010_c01_012e, x=EVENTTIME), 
              fill="tomato3", alpha=0.25) +
  geom_ribbon(data=Control, aes(ymin=ci1_s1901_2010_c01_012e, ymax=ci2_s1901_2010_c01_012e, x=EVENTTIME), 
              fill="steelblue3", alpha=0.25) +
  geom_line(data=Control, aes(x=EVENTTIME, y=mean_s1901_2010_c01_012e, color="steelblue3")) +
  labs(x = "Event Time (Years)", y = "Mean Census Tract Household Income, 2010", 
       title ="Mean Household Income",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank()) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  scale_color_manual(name = "Flood Risk Category", values = c("Treated"="tomato3", "Control"="steelblue3")) + 
  annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = 60000,
           alpha = .1,fill = "blue") +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)




#-------Look at N
ggplot(collapse) + 
  geom_line(aes(x=EVENTTIME, y=n, group=Treated, color=Treated)) +
  # geom_line(aes(x = EVENTTIME, y=address.ci1, group=Treated, color=Treated), 
  #          width=.1, linetype="dotted") +
  #geom_line( aes(x = EVENTTIME, y=address.ci2, group=Treated, color=Treated), 
  #           width=.1,  linetype="dotted") +
  labs(x = "Event Time (Years)", y = "Sample Size", 
       title ="Sample Size",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank()) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  #scale_y_continuous(limits = c(0, .48), 
  #                   breaks = c(0,.1,.2,.3,.4,.5)) +
    geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)