# MAIN ANALYSIS
# NOTE: trimmed to packages actually used by this script (dplyr, ggplot2,
# data.table, tidyr, haven, sandwich, lmtest). The original development
# environment also loaded foreign/tidyverse/readr/DBI/rlang/fastLink, but
# none of those are used here, and requiring them just adds heavy/optional
# dependencies (e.g. fastLink needs Java) for replicators.
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(sandwich)
library(lmtest)
library(haven)

rm(list=ls())

#################################################
#### Helper: Clustered SE for event-study models
#################################################
clusterse = function(model) {
  
  # Construct Clustered Standarad errors
  cl_vcov_mat <- vcovCL(m1_move, cluster = ~STATE)
  
  m1coeffs_std <- data.frame(summary(m1_move)$coefficients)
  coi_indices <- row.names(m1coeffs_std)
  coi_indices = which(startsWith(row.names(m1coeffs_std),
                                 'Treated:as.factor(EVENTTIME)'))
  m1coeffs_move = m1coeffs_std[coi_indices,]
  
  EVENTTIME = seq(from = -14, to = 20, by = 1)   # ← your original choice
  
  m1coeffs_move$EVENTTIME = EVENTTIME
  
  m1_move_coeffs_cl <- coeftest(m1_move, vcov = cl_vcov_mat, cluster = ~STATE)
  m1_move_coeffs_cl[coi_indices,]
  
  m1cis <- coefci(m1_move, parm = coi_indices, vcov = vcovCL,
                  cluster = ~STATE)
  m1cis_plot = as.data.frame(m1cis)
  m1cis_plot$EVENTTIME = EVENTTIME
  
  return(list(
    m1cis_plot = m1cis_plot,
    m1_move_coeffs_cl = m1_move_coeffs_cl
  ))
}



#################################################
# Load Data
#################################################

data <- read_dta("./cleandata/TOY_TreatedControlAddresses_forRegression_allyear.dta")

#################################################
# Derived variables
#################################################

data$high_frac_black = ifelse(data$frac_black > 0.5, 1, 0)
data$high_frac_black_75 = ifelse(data$frac_black > 0.2, 1, 0)

data$SFHA = as.factor(data$fld_zone_factor)
data$inSFHA = ifelse(data$SFHA == 3, 1, 0)

# EVENTTIME should already exist, but ensure factor level reference
data <- within(data, EVENTTIME <- as.factor(EVENTTIME))


#################################################
# Construct SFHA Control Subset
#################################################
data_3 = data %>%
  group_by(PID) %>%
  filter(0 %in% treated_sfha_3 | 1 %in% treated_sfha_3)

data_3 <- within(data_3, EVENTTIME <- relevel(as.factor(EVENTTIME), ref="-1"))


#################################################
# Construct other outcomes 
#################################################

data_3$BUYOUT_income_percap = ifelse(data_3$EVENTTIME == 0,
                                     data_3$fam_income_percap, NA)
data_3$BUYOUT_employment = ifelse(data_3$EVENTTIME==0, 
                                  data_3$frac_employed_16plus_2000_TEST, NA)
data_3$BUYOUT_college = ifelse(data_3$EVENTTIME==0, 
                               data_3$frac_college_plus, NA)

data_3 = data_3 %>%
  group_by(PID) %>%
  fill(BUYOUT_income_percap,  .direction = 'down') %>%
  fill(BUYOUT_income_percap,  .direction = 'up') %>%
  fill(BUYOUT_employment,  .direction = 'down') %>%
  fill(BUYOUT_employment,  .direction = 'up') %>%
  fill(BUYOUT_college,  .direction = 'down') %>%
  fill(BUYOUT_college,  .direction = 'up') 

data_3$richer = ifelse(as.numeric(data_3$BUYOUT_income_percap) >
                         as.numeric(data_3$fam_income_percap) &
                         as.numeric(data_3$EVENTTIME) >= 0, 1, 0)
data_3$moreEmployed = ifelse(as.numeric(data_3$BUYOUT_employment) >
                               as.numeric(data_3$frac_employed_16plus_2000_TEST) & 
                               as.numeric(data_3$EVENTTIME)>=0, 1,0)
data_3$moreEducated = ifelse(as.numeric(data_3$BUYOUT_college) > 
                               as.numeric(data_3$frac_college_plus) & 
                               as.numeric(data_3$EVENTTIME)>=0, 1,0)


#################################################
# Collapse datasets and histogram subsets
#################################################
collapse = data %>%
  group_by(EVENTTIME, Treated) %>%
  summarise(
    mean_move_address = mean(moved_address, na.rm=TRUE),
    mean_move_zip = mean(MOVED_ZIP, na.rm=TRUE),
    mean_move_state = mean(MOVED_STATE, na.rm=TRUE),
    mean_SFHA = mean(inSFHA, na.rm=TRUE),
    sd_move_address = sd(moved_address, na.rm=TRUE),
    sd_move_zip = sd(MOVED_ZIP, na.rm=TRUE),
    sd_move_state = sd(MOVED_STATE, na.rm=TRUE),
    sd_SFHA = sd(inSFHA, na.rm=TRUE),
    n = n(),
    .groups = "drop"
  )

collapse <- collapse %>%
  filter(as.numeric(as.character(EVENTTIME)) >= -10) %>%
  # EVENTTIME is a factor at this point; convert to numeric so the
  # collapse_wide x-axis works with scale_x_continuous() in the plots below.
  mutate(EVENTTIME = as.numeric(as.character(EVENTTIME)))

collapse_wide <- dcast(
  as.data.table(collapse),
  EVENTTIME ~ Treated,
  value.var = c(
    "mean_move_address","mean_move_zip","mean_move_state","mean_SFHA",
    "sd_move_address","sd_move_zip","sd_move_state","sd_SFHA","n"
  )
)




collapse_3 = data_3 %>%
  group_by(EVENTTIME, Treated) %>%
  summarise(mean_move_address = mean(moved_address, na.rm=TRUE),
            mean_move_zip = mean(MOVED_ZIP,  na.rm=TRUE),
            mean_move_state = mean(MOVED_STATE, na.rm=TRUE),
            mean_SFHA = mean(as.integer(inSFHA), na.rm=TRUE),
            sd_move_address = sd(moved_address, na.rm=TRUE), 
            sd_move_zip  = sd(MOVED_ZIP, na.rm=TRUE),
            sd_move_state = sd(MOVED_STATE, na.rm=TRUE),
            sd_SFHA = sd(as.integer(inSFHA), na.rm=TRUE), 
            n = n())
collapse_3 = na.omit(collapse_3)
# NOTE: EVENTTIME is a (releveled) factor here, so a direct numeric comparison
# (`EVENTTIME >= -15`) returns NA for every row and silently breaks the dcast
# below (Treated becomes NA, so the pivoted columns end up named "*_NA"
# instead of "*_0"/"*_1"). Use the same as.numeric(as.character(...)) pattern
# already used for `collapse` above. EVENTTIME is already restricted to
# [-15, 20], so this filter keeps all rows -- it only fixes the comparison.
collapse_3 = as.data.table(collapse_3[as.numeric(as.character(collapse_3$EVENTTIME))>=-15,])
# Same factor->numeric fix as above, needed for scale_x_continuous() in the
# flood_3 / move plots below.
collapse_3$EVENTTIME = as.numeric(as.character(collapse_3$EVENTTIME))
collapse_3_wide = dcast(collapse_3,  EVENTTIME ~ Treated,
                        value.var = c("mean_move_address", "mean_move_zip", "mean_move_state",
                                      "mean_SFHA", "sd_move_address", "sd_move_zip", "sd_move_state",
                                      "sd_SFHA", "n"))



highfrac_sub = data %>% filter(high_frac_black == 1 & EVENTTIME == -1)
lowfrac_sub  = data %>% filter(high_frac_black == 0 & EVENTTIME == -1)





#################################################
# Figure 3: Main Regression: Move Address
#################################################

### Moved address 

m1_move <- lm(moved_address ~ as.factor(EVENTYEAR) + as.factor(STATE) +
                as.factor(EVENTYEAR):as.factor(STATE) + Treated +
                as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
              data = data_3)

summary(m1_move)

# Construct Clustered Standarad errors
cl_vcov_mat <- vcovCL(m1_move, cluster = ~STATE)

m1coeffs_std <- data.frame(summary(m1_move)$coefficients)
coi_indices <- row.names(m1coeffs_std)
coi_indices = which(startsWith(row.names(m1coeffs_std), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_move = m1coeffs_std[coi_indices,]
EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_move$EVENTTIME = EVENTTIME

m1_move_coeffs_cl <- coeftest(m1_move, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_move, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot = as.data.frame(m1cis)
m1cis_plot$EVENTTIME = EVENTTIME
m1coeffs_move[nrow(m1coeffs_move) + 1,] = c(0,0,0,0,-1)
m1cis_plot[nrow(m1cis_plot) + 1,] = c(0,0,-1)


move_3_reg = ggplot() + 
  geom_line(data = m1coeffs_move, aes(x=EVENTTIME, y=Estimate), colour="darkgreen") +
  geom_ribbon(data = m1cis_plot, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME),
              fill="darkgreen", alpha=0.25) +
  labs(x="Event Time (Years)", y="Likelihood of moving",
       title="Likelihood of moving to a new address") +
  theme_classic() + theme(panel.grid = element_blank()) +
  scale_x_continuous(limits=c(-10,15)) +
  geom_vline(xintercept=0, col="red") +
  geom_hline(yintercept=0)

pdf("figures/Fig3a__sfha_control_move_address.pdf")
print(move_3_reg)
dev.off()



### Moved zipcode 
m1_zip <- lm(MOVED_ZIP ~ as.factor(EVENTYEAR) + as.factor(STATE) +
               as.factor(EVENTYEAR):as.factor(STATE) + Treated +
               as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
             data = data_3)

# Construct Clustered Standarad errors
cl_vcov_mat <- vcovCL(m1_zip, cluster = ~STATE)

m1coeffs_std <- data.frame(summary(m1_zip)$coefficients)
coi_indices <- row.names(m1coeffs_std)
coi_indices = which(startsWith(row.names(m1coeffs_std), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_move_zip = m1coeffs_std[coi_indices,]
EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_move_zip$EVENTTIME = EVENTTIME

m1_move_coeffs_cl <- coeftest(m1_zip, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_zip, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot_zip = as.data.frame(m1cis)
m1cis_plot_zip$EVENTTIME = EVENTTIME
m1coeffs_move_zip[nrow(m1coeffs_move_zip) + 1,] = c(0,0,0,0,-1)
m1cis_plot_zip[nrow(m1cis_plot_zip) + 1,] = c(0,0,-1)
rm(m1_zip)



### Moved state 
m1_state <- lm(MOVED_STATE ~ as.factor(EVENTYEAR) + as.factor(STATE) +
                 as.factor(EVENTYEAR):as.factor(STATE) + Treated +
                 as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
               data = data_3)

# Construct Clustered Standard errors
cl_vcov_mat <- vcovCL(m1_state, cluster = ~STATE)

m1coeffs_std <- data.frame(summary(m1_state)$coefficients)
coi_indices <- row.names(m1coeffs_std)
coi_indices = which(startsWith(row.names(m1coeffs_std), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_move_state = m1coeffs_std[coi_indices,]
EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_move_state$EVENTTIME = EVENTTIME

m1_move_coeffs_cl <- coeftest(m1_state, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_state, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot_state = as.data.frame(m1cis)
m1cis_plot_state$EVENTTIME = EVENTTIME
m1coeffs_move_state[nrow(m1coeffs_move_state) + 1,] = c(0,0,0,0,-1)
m1cis_plot_state[nrow(m1cis_plot_state) + 1,] = c(0,0,-1)

############ OTHER MOVE PLOT #############
move_zipstate = ggplot() + 
  geom_line(data = m1coeffs_move, aes(x=EVENTTIME, y=Estimate), colour="darkgreen") +
  geom_ribbon(data = m1cis_plot, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="darkgreen", alpha=0.25) +
  geom_line(data = m1coeffs_move_zip, aes(x=EVENTTIME, y=Estimate, colour="brown3")) +
  geom_ribbon(data = m1cis_plot_zip, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="brown3", alpha=0.25) +
  geom_line(data = m1coeffs_move_state, aes(x=EVENTTIME, y=Estimate), colour="deepskyblue4") +
  geom_ribbon(data = m1cis_plot_state, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="deepskyblue4", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of moving", 
       title ="Likelihood of moving (non-buyout high flood risk control)" ) +
  scale_color_manual(name = "Outcome", values = c("Moved Address"="darkgreen","Moved Zip Code"="brown3", "Moved State"="deepskyblue4" )) + 
  theme_classic() + theme(panel.grid = element_blank(), legend.position = c(0.15, 0.9)) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-10, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/Fig3b__sfha_control_moves.pdf")
print(move_zipstate)
dev.off()






### Flood Risk Regression

m1_flood <- lm(inSFHA ~ as.factor(EVENTYEAR) + as.factor(STATE) + Treated +
                 as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
               data = data_3)


####### construct the covariance matrix (SFHA control)
cl_vcov_mat <- vcovCL(m1_flood, cluster = ~STATE)

m1coeffs_std <- data.frame(summary(m1_flood)$coefficients)
coi_indices <- row.names(m1coeffs_std)
coi_indices = which(startsWith(row.names(m1coeffs_std), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_flood = m1coeffs_std[coi_indices,]
EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_flood$EVENTTIME = EVENTTIME

m1_move_coeffs_cl <- coeftest(m1_flood, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_flood, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot_flood = as.data.frame(m1cis)
m1cis_plot_flood$EVENTTIME = EVENTTIME
m1coeffs_flood[nrow(m1coeffs_flood) + 1,] = c(0,0,0,0,-1)
m1cis_plot_flood[nrow(m1cis_plot_flood) + 1,] = c(0,0,-1)
m1coeffs_flood$inverseEstimate = (m1coeffs_flood$Estimate)*-1
m1coeffs_flood$`2.5 %` = (m1cis_plot_flood$`2.5 %`)*-1
m1coeffs_flood$`97.5 %` = (m1cis_plot_flood$`97.5 %`)*-1



floodrisk = ggplot() + 
  geom_line(data = m1coeffs_flood, aes(x=EVENTTIME, y=inverseEstimate), colour="goldenrod1") +
  geom_ribbon(data = m1coeffs_flood, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="goldenrod1", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of not living in SFHA", 
       title ="Likelihood of not living in SFHA (non-buyout high flood risk control)"
  ) +
  theme_classic() + theme(panel.grid = element_blank()) +
  ylim(-0.1,0.3) + 
  scale_x_continuous(name = "Event Time (Years)", limits = c(-10, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  theme(legend.position="none") +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/Fig3c__sfha_control_sfha_reg.pdf")
print(floodrisk)
dev.off()



### Census Tract Outcomes (Income/Education/Employment)

#### More rich
m1_richer <- lm(richer ~ as.factor(EVENTYEAR) + as.factor(STATE) + as.factor(EVENTYEAR):as.factor(STATE)
                + Treated + as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
                data = data_3)
summary(m1_richer)

m1coeffs_std_richer <- data.frame(summary(m1_richer)$coefficients)
coi_indices <- row.names(m1coeffs_std_richer)
coi_indices = which(startsWith(row.names(m1coeffs_std_richer), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_richer = m1coeffs_std_richer[coi_indices,]
EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_richer$EVENTTIME = EVENTTIME
m1coeffs_richer[nrow(m1coeffs_richer) + 1,] = c(0,0,0,0,-1)

cl_vcov_mat <- vcovCL(m1_richer, cluster = ~STATE)

m1_move_coeffs_cl <- coeftest(m1_richer, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_richer, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot = as.data.frame(m1cis)
m1cis_plot$EVENTTIME = EVENTTIME
m1cis_plot[nrow(m1cis_plot) + 1,] = c(0,0,-1)
rm(m1_richer)
gc()


#### More employed
m1_moreEmployed <- lm(moreEmployed ~ as.factor(EVENTYEAR) + as.factor(STATE) 
                      + Treated + as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
                      data = data_3)

m1coeffs_std_moreEmployed  <- data.frame(summary(m1_moreEmployed )$coefficients)
coi_indices <- row.names(m1coeffs_std_moreEmployed )
coi_indices = which(startsWith(row.names(m1coeffs_std_moreEmployed ), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_moreEmployed  = m1coeffs_std_moreEmployed[coi_indices,]
#EVENTTIME = setdiff(-15:20,c(-1))
m1coeffs_moreEmployed $EVENTTIME = EVENTTIME
m1coeffs_moreEmployed[nrow(m1coeffs_moreEmployed ) + 1,] = c(0,0,0,0,-1)

cl_vcov_mat <- vcovCL(m1_moreEmployed, cluster = ~STATE)

m1_Employed_coeffs_cl <- coeftest(m1_moreEmployed, vcov = cl_vcov_mat, cluster = ~STATE)
m1_Employed_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_moreEmployed, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_plot_moreEmployed = as.data.frame(m1cis)
m1cis_plot_moreEmployed$EVENTTIME = EVENTTIME
m1cis_plot_moreEmployed[nrow(m1cis_plot_moreEmployed) + 1,] = c(0,0,-1)
rm(m1_moreEmployed)
gc()


#### More educated
m1_moreEducated <- lm(moreEducated ~ as.factor(EVENTYEAR) + as.factor(STATE) + countyfp
                      + Treated + as.factor(EVENTTIME) + Treated:as.factor(EVENTTIME),
                      data = data_3)

m1coeffs_std_moreEducated  <- data.frame(summary(m1_moreEducated)$coefficients)
coi_indices <- row.names(m1coeffs_std_moreEducated )
coi_indices = which(startsWith(row.names(m1coeffs_std_moreEducated ), 'Treated:as.factor(EVENTTIME)'))
m1coeffs_moreEducated  = m1coeffs_std_moreEducated[coi_indices,]
m1coeffs_moreEducated$EVENTTIME = EVENTTIME
m1coeffs_moreEducated[nrow(m1coeffs_moreEducated ) + 1,] = c(0,0,0,0,-1)

cl_vcov_mat <- vcovCL(m1_moreEducated, cluster = ~STATE)

m1_move_coeffs_cl <- coeftest(m1_moreEducated, vcov = cl_vcov_mat, cluster = ~STATE)
m1_move_coeffs_cl[coi_indices,]
m1cis <- coefci(m1_moreEducated, parm = coi_indices, vcov = vcovCL,
                cluster = ~STATE)
m1cis_moreEducated_plot = as.data.frame(m1cis)
m1cis_moreEducated_plot$EVENTTIME = EVENTTIME
m1cis_moreEducated_plot[nrow(m1cis_moreEducated_plot) + 1,] = c(0,0,-1)

rm(m1_moreEducated)
gc()


move_3_reg = ggplot() + 
  geom_line(data = m1coeffs_richer, aes(x=EVENTTIME, y=Estimate), colour="darkorange3") +
  geom_ribbon(data = m1cis_plot, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="darkorange3", alpha=0.25) +
  geom_line(data = m1coeffs_moreEducated, aes(x=EVENTTIME, y=Estimate), colour="lightsteelblue3") +
  geom_ribbon(data = m1cis_plot_moreEmployed, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="lightsteelblue3", alpha=0.25) + 
  geom_line(data = m1coeffs_moreEmployed, aes(x=EVENTTIME, y=Estimate), colour="yellowgreen") +
  geom_ribbon(data = m1cis_moreEducated_plot, aes(ymin=`2.5 %`, ymax=`97.5 %`, x=EVENTTIME), 
              fill="yellowgreen", alpha=0.25) + 
  theme_classic() + theme(panel.grid = element_blank()) +
  ylim(-.01, .15) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-10, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  theme(legend.position="none") +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/Fi3d__sfha_control_OTHER_clusterSE.pdf")
print(move_3_reg)
dev.off()








#################################################
# Supplemental check mark figures
#################################################


flood_3= ggplot() + 
  geom_line(data = collapse_3_wide,aes(x=EVENTTIME, y=mean_SFHA_1, color="tomato3")) +
  geom_ribbon(data = collapse_3_wide,aes(ymin=(mean_SFHA_1 - 2* (sd_SFHA_1/sqrt(n_1))), ymax=(mean_SFHA_1 + 2*(sd_SFHA_1/sqrt(n_1))), x=EVENTTIME), 
              fill="tomato3", alpha=0.25) +
  geom_ribbon(data = collapse_3_wide,aes(ymin=(mean_SFHA_0 - 2*(sd_SFHA_0/sqrt(n_0))), ymax=(mean_SFHA_0 + 2*(sd_SFHA_0/sqrt(n_0))), x=EVENTTIME), 
              fill="steelblue3", alpha=0.25) +
  geom_line(data = collapse_3_wide,aes(x=EVENTTIME, y=mean_SFHA_0, color="steelblue3")) +
  geom_line(data = collapse_wide,aes(x=EVENTTIME, y=mean_SFHA_0, color="darkorchid1")) +
  geom_ribbon(data = collapse_wide,aes(ymin=(mean_SFHA_0 - 2* (sd_SFHA_0/sqrt(n_0))), ymax=(mean_SFHA_0 + 2*(sd_SFHA_0/sqrt(n_0))), x=EVENTTIME), 
              fill="darkorchid1", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of living in SFHA", 
       title ="Likelihood of living in SFHA",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank(), legend.position = c(0.15, 0.35)) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  scale_color_manual(name = "Flood Risk Category", values = c("Treated"="tomato3", "Control (SFHA)"="steelblue3"
                                                              , "Control (Zip code)"="darkorchid1", "Control (1% & 0.2% \n annual chance)"="cyan2")) + 
  geom_vline(xintercept = 5,  col = "grey", linetype = "dashed") + 
  geom_text(aes(x=5, label="\n5-year phase in", y=0.7), colour="grey", angle=90) +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/SI__sfha_control_floodrisk.pdf")
print(flood_3)
dev.off()



move= ggplot() + 
  geom_line(data = collapse_3_wide,aes(x=EVENTTIME, y=mean_move_address_1, color="tomato3")) +
  geom_ribbon(data = collapse_3_wide,aes(ymin=(mean_move_address_1 - 2* (sd_move_address_1/sqrt(n_1))), ymax=(mean_move_address_1 + 2*(sd_move_address_1/sqrt(n_1))), x=EVENTTIME), 
              fill="tomato3", alpha=0.25) +
  geom_line(data = collapse_3_wide,aes(x=EVENTTIME, y=mean_move_address_0, color="steelblue3")) +
  geom_ribbon(data = collapse_3_wide,aes(ymin=(mean_move_address_0 - 2*(sd_move_address_0/sqrt(n_0))), ymax=(mean_move_address_0 + 2*(sd_move_address_0/sqrt(n_0))), x=EVENTTIME), 
              fill="steelblue3", alpha=0.25) +
  geom_line(data = collapse_wide,aes(x=EVENTTIME, y=mean_move_address_0, color="darkorchid1")) +
  geom_ribbon(data = collapse_wide,aes(ymin=(mean_move_address_0 - 2* (sd_move_address_0/sqrt(n_0))), ymax=(mean_move_address_0 + 2*(sd_move_address_0/sqrt(n_0))), x=EVENTTIME), 
              fill="darkorchid1", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of moving to new address", 
       title ="New Address, SFHA and Zip Code Control Group",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank(), legend.position = c(0.15, 0.85)) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  scale_color_manual(name = "Flood Risk Category", values = c("Treated"="tomato3", "Control (SFHA)"="steelblue3"
                                                              , "Control (Zip code)"="darkorchid1")) + 
  #annotate("rect", xmin = 0, xmax = 5, ymin = 0, ymax = .6,
  #         alpha = .1,fill = "blue") +
  geom_vline(xintercept = 5,  col = "grey", linetype = "dashed") + 
  geom_text(aes(x=5, label="\n5-year phase in", y=0.6), colour="grey", angle=90) +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/SI__sfha_control_moveaddress.pdf")
print(move)
dev.off()


#################################################
# Collapsed datasets by high_frac_black (within Treated / Control)
#################################################
# Feeds the treated_highfracBlack.pdf / treatedControl_highfracBlack.pdf plots
# below: mean/sd/n of moved_address by EVENTTIME x high_frac_black, computed
# separately for the treated (Treated==1) and control (Treated==0) subsets of
# `data`. Mirrors the collapse/collapse_wide pattern above, pivoted on
# high_frac_black instead of Treated.

collapse_treated = data %>%
  filter(Treated == 1) %>%
  group_by(EVENTTIME, high_frac_black) %>%
  summarise(
    mean_move_address = mean(moved_address, na.rm=TRUE),
    sd_move_address = sd(moved_address, na.rm=TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(as.numeric(as.character(EVENTTIME)) >= -10) %>%
  mutate(EVENTTIME = as.numeric(as.character(EVENTTIME)))

collapse_treated_wide <- dcast(
  as.data.table(collapse_treated),
  EVENTTIME ~ high_frac_black,
  value.var = c("mean_move_address", "sd_move_address", "n")
)

collapse_control = data %>%
  filter(Treated == 0) %>%
  group_by(EVENTTIME, high_frac_black) %>%
  summarise(
    mean_move_address = mean(moved_address, na.rm=TRUE),
    sd_move_address = sd(moved_address, na.rm=TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(as.numeric(as.character(EVENTTIME)) >= -10) %>%
  mutate(EVENTTIME = as.numeric(as.character(EVENTTIME)))

collapse_control_wide <- dcast(
  as.data.table(collapse_control),
  EVENTTIME ~ high_frac_black,
  value.var = c("mean_move_address", "sd_move_address", "n")
)


##### PLOTS
move = ggplot() +
  geom_line(data = collapse_treated_wide,aes(x=EVENTTIME, y=mean_move_address_1, color="tomato3")) +
  geom_ribbon(data = collapse_treated_wide,aes(ymin=(mean_move_address_1 - 2* (sd_move_address_1/sqrt(n_1))), ymax=(mean_move_address_1 + 2*(sd_move_address_1/sqrt(n_1))), x=EVENTTIME), 
              fill="tomato3", alpha=0.25) +
  geom_line(data = collapse_treated_wide,aes(x=EVENTTIME, y=mean_move_address_0, color="cyan2")) +
  geom_ribbon(data = collapse_treated_wide,aes(ymin=(mean_move_address_0 - 2* (sd_move_address_0/sqrt(n_0))), ymax=(mean_move_address_0 + 2*(sd_move_address_0/sqrt(n_0))), x=EVENTTIME), 
              fill="cyan2", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of moving", 
       title ="Likelihood of moving to a new address (buyout subset)",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank(), legend.position = c(0.15, 0.9)) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-7, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  scale_color_manual(name = "", values = c("High fraction black"="tomato3", "Low fraction black"="cyan2"
  )) + 
  ylim(0, 0.75) +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/treated_highfracBlack.pdf")
print(move)
dev.off()



move_2 = ggplot() + 
  geom_line(data = collapse_treated_wide,aes(x=EVENTTIME, y=mean_move_address_1, color="tomato3")) +
  geom_ribbon(data = collapse_treated_wide,aes(ymin=(mean_move_address_1 - 2* (sd_move_address_1/sqrt(n_1))), ymax=(mean_move_address_1 + 2*(sd_move_address_1/sqrt(n_1))), x=EVENTTIME), 
              fill="tomato3", alpha=0.25) +
  geom_line(data = collapse_treated_wide,aes(x=EVENTTIME, y=mean_move_address_0, color="cyan2")) +
  geom_ribbon(data = collapse_treated_wide,aes(ymin=(mean_move_address_0 - 2* (sd_move_address_0/sqrt(n_0))), ymax=(mean_move_address_0 + 2*(sd_move_address_0/sqrt(n_0))), x=EVENTTIME), 
              fill="cyan2", alpha=0.25) +
  geom_line(data = collapse_control_wide,aes(x=EVENTTIME, y=mean_move_address_1, color="orangered1")) +
  geom_ribbon(data = collapse_control_wide,aes(ymin=(mean_move_address_1 - 2* (sd_move_address_1/sqrt(n_1))), ymax=(mean_move_address_1 + 2*(sd_move_address_1/sqrt(n_1))), x=EVENTTIME), 
              fill="orangered1", alpha=0.25) +
  geom_line(data = collapse_control_wide,aes(x=EVENTTIME, y=mean_move_address_0, color="royalblue1")) +
  geom_ribbon(data = collapse_control_wide,aes(ymin=(mean_move_address_0 - 2* (sd_move_address_0/sqrt(n_0))), ymax=(mean_move_address_0 + 2*(sd_move_address_0/sqrt(n_0))), x=EVENTTIME), 
              fill="royalblue1", alpha=0.25) +
  labs(x = "Event Time (Years)", y = "Likelihood of moving", 
       title ="Likelihood of moving to a new address (non-buyout, same zip code subset)",
       colour = "Treated") +
  theme_classic() + theme(panel.grid = element_blank(), legend.position = c(0.15, 0.9)) +
  scale_x_continuous(name = "Event Time (Years)", limits = c(-10, 15), 
                     breaks = c(-10, -9, -8, -7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)) + 
  scale_color_manual(name = "", values = c("High fraction black (Treated)"="tomato3", "Low fraction black (Treated)"="cyan2",
                                           "High fraction black (Control)"="orangered1", "Low fraction black(Control)"="royalblue1"
  )) + 
  ylim(0, 0.75) +
  geom_vline(xintercept = 0,  col = "red") + 
  geom_hline(yintercept = 0)
pdf("figures/treatedControl_highfracBlack.pdf")
print(move_2)
dev.off()






###################################
### Supplemental Versions of model
###################################
m3_move <- lm(moved_address ~ as.factor(EVENTYEAR) + as.factor(STATE) + 
                as.factor(EVENTYEAR):as.factor(STATE) +Treated + EVENTTIME + 
                Treated:EVENTTIME,
              data = data_3)
m4_move <- lm(moved_address ~ as.factor(EVENTYEAR) + countyfp + as.factor(STATE) 
              + as.factor(EVENTYEAR):as.factor(STATE) +Treated + EVENTTIME 
              + Treated:EVENTTIME,
              data = data_3)