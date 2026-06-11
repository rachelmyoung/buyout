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
library(lubridate)

rm(list=ls())
setwd("~/project/buyout")
`%!in%` <- Negate(`%in%`)

tdb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/drives/Userhomes/home/rmyoung/crd4_personaddress/reshapeCRD4-db-date.sqlite3") 
tbl_reshape = tbl(tdb, "reshape")


buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")
bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))


mydb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_v2.sqlite3")
tbl_matched = tbl(mydb, "infutorbuyoutperfectaddressmatch")
tbl = tbl_matched %>% 
  select(PID)
buyoutids = as.data.table(tbl)
buyoutids = as.vector(unique(buyoutids$PID))

Control_PIDs = data.frame(PID = 0)
Control_PID_YEAR = data.frame(PID = 0,
                          EVENTYEAR = 0)

for (y in 1:length(yr)) {
  
  yrList = yr[y]
  print(yr[y])
 # yrList = 2010
  longYr = as.character(min(yrList)*10000 + 101) 
  longYr = as.double(as.Date(longYr, tryFormats = "%Y%m%d"))
  
  zipList = unique(na.omit(bo_zip_yr$Zip[bo_zip_yr$Fiscal.Year == yrList]))
  BO_PIDS = buyout$PID
  print(zipList)
  
  # Find people living in the Buyout zip code in the year of the zip code from the reshaped database
  start_time = Sys.time() #start stopwatch for reshaping
  control_all_tbl = tbl_reshape %>% 
    filter(ZIP %in% zipList & FIRSTDATE<=longYr & LASTDATE>=longYr) 
  control_all = as.data.table(control_all_tbl)  
  end_time = Sys.time() #start stopwatch 
  print(end_time - start_time)
  
  control_all %>% 
    group_by(ZIP) %>%
    summarise(n())
  
  control_sub = control_all %>% # keep all that are in the right zip and time but not the perfect matched addresses
    group_by(ZIP) %>%
    slice_sample(prop=0.01) 
  
  control_sub %>% 
    group_by(ZIP) %>%
    summarise(n())
  
  keepid <- control_sub[!duplicated(control_sub[ , c("PID")]), ]  # Delete rows
  keepid = as.data.frame(keepid[,1])
  Control_PIDs = rbind(Control_PIDs , keepid)
  
  
}


Control_PIDs = Control_PIDs[-1,]


control_people_tbl = tbl_reshape %>% filter(PID %in% Control_PIDs)
control_people = as.data.table(control_people_tbl)

control_people = left_join(control_people, Control_PID_YEAR)

save(control_people, file = "./output/control_people_1percent.Rda")


rm(list=ls())

load("./output/control_people.Rda")

control_people = unique(control_people)
control_people$Treated = 0


buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")
bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))

t = unique(buyout[,c('Zip', 'Fiscal.Year')])
t = t[t$Zip!="",]

control_people$EVENTYEAR=""
control_people$Treated=0

for (y in 1:length(yr)) {
  
  yrList = yr[y]
  print(yr[y])
  longYr = as.character(min(yrList)*10000 + 101) 
  longYr = as.double(as.Date(longYr, tryFormats = "%Y%m%d"))
  
  zipList = unique(na.omit(bo_zip_yr$Zip[bo_zip_yr$Fiscal.Year == yrList]))
  print(zipList)
  
  control_people$EVENTYEAR = ifelse(control_people$ZIP %in% zipList & control_people$FIRSTDATE <= longYr & control_people$LASTDATE>=longYr, yrList, control_people$EVENTYEAR )
  
  print(control_people$EVENTYEAR)
  
}


control_people$Fiscal.Year  = control_people$EVENTYEAR
control_people = control_people[order(control_people$PID, control_people$YEAR)] 
control_people = control_people  %>% 
  group_by(PID) %>%
  fill( Fiscal.Year, .direction = "down")  %>% # fills ci down within PID
  fill( Fiscal.Year, .direction = "up") # fills addresses down within PID
print(control_people$Fiscal.Year)


save(control_people, file = "./output/control_people_1percent.Rda")


mydb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_v2.sqlite3")
tbl_matched = tbl(mydb, "infutorbuyoutperfectaddressmatch")
data = as.data.table(tbl_matched)
data$Treated = 1
data$EVENTYEAR = data$Fiscal.Year

data=rbind(setDT(data), setDT(control_people), fill=TRUE)


t = data %>% 
  group_by(EVENTYEAR, Treated) %>%
  summarise(n())

save(data, file = "./output/Treated_Control_people_1percent.Rda")

