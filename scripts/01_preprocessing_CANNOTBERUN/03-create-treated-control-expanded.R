# Just try treated and control move likelihood for 2000 - 2017 treated group 
# .03-create-treated-control-expanded-20002017.R

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
setwd("~/projects/buyout")
source("scripts/FUNCTION_clean_firstLastDate.R")

#narc3 <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/home/rmyoung/hurricane/output/reshapeCRD4-db-date.sqlite3")
#tbl_reshape = tbl(tdb, "reshape")

#tdb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/projects/hurricane/output/reshapeCRD4-db-date.sqlite3")
tdb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "/drives/Userhomes/home/rmyoung/crd4_personaddress/reshapeCRD4-db-date.sqlite3") 
tbl_reshape = tbl(tdb, "reshape")


buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")
bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))

for (y in 12:length(yr)) {
  yrList = yr[y]
  longYr = as.character(min(yrList)*10000 + 101) 
  longYr = as.double(as.Date(longYr, tryFormats = "%Y%m%d"))
  
  print(yrList)
  
  zipList = unique(na.omit(bo_zip_yr$Zip[bo_zip_yr$Fiscal.Year == yrList]))
  #BO_PIDS = buyout$PID
  print(zipList)
  
  #---------------------------------------------------------- CONTROL GROUP 
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
  
  #create new id because there are duplicate people with different PIDs
  control_all=control_all %>%
    group_by(FNAME, LNAME, ADDRESS, ZIP, CRD_DOB) %>%
    mutate(group_id = cur_group_id())
  
  #PIDs for those with duplicate group ids
  dups = control_all$group_id[duplicated(control_all$group_id)]
  dupsDF = control_all[control_all$group_id %in% dups,]
  
  control_all_unique = control_all[unique(control_all$group_id),]
  
  # just keep 1% of the sample
  control_unique_sub = control_all_unique %>% # keep all that are in the right zip and time but not the perfect matched addresses
    group_by(ZIP) %>%
    slice_sample(prop=0.01) 
  
  control_unique_sub %>%  
    group_by(ZIP) %>%
    summarise(n())
  
  constructed_grp_id = control_unique_sub$group_id # select people based on the group ID not the PID because of the duplicate people issue
  
  control_sub_grpid = control_all %>%
    filter(group_id %in% constructed_grp_id)
  
  # remove the full group to save space
  rm(control_all)
  
  keepid <- control_sub_grpid[!duplicated(control_sub_grpid[ , c("PID")]), ]  # Delete rows
  keepid = as.data.frame(keepid[,1])
  id = keepid$PID
  
  
  # Pull in all addresses by the PIDs we found
  start_time = Sys.time() #start stopwatch for reshaping
  control_2010_sub = tbl_reshape %>% 
    filter(PID %in% id) 
  control_all = as.data.table(control_2010_sub)  
  end_time = Sys.time() #start stopwatch 
  print(end_time - start_time)
  
  control_all$Treated = 0
  control_all$EVENTYEAR=yrList
  
  
  control_all = control_all %>%
    mutate(ADDRESS_noapt = gsub("\\ APT.*", "", ADDRESS)) %>%
    mutate(ADDRESS_noapt = gsub("\\ UNIT.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ #.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ STE.*", "", ADDRESS_noapt))
  

  
  paste0("./output/controlgroup_",yrList,".RDa" )
  save(control_all, file=paste0("./output/controlgroup_",yrList,".RDa"))
  

  # need to deal with duplicates
  #create new id because there are duplicate people with different PIDs
  control_all=control_all %>%
    group_by(FNAME, LNAME, CRD_DOB) %>%
    mutate(group_id = cur_group_id())
  
  #PIDs for those with duplicate group ids
  dups = control_all$group_id[duplicated(control_all$group_id)]
  dupsDF = control_all[control_all$group_id %in% dups,]
  
  control_all_dupRemove = control_all %>%
    distinct(FNAME, LNAME, CRD_DOB, EFFDATE, ADDRESS_noapt, ZIP, .keep_all = TRUE)
  
  
  #------------expand
  # control_all = control_all %>% 
  #   mutate(FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01")) %>%
  #   mutate(LASTDATE=as.Date(LASTDATE, origin="1970-01-01")) %>%
  #   mutate(FIRSTYEAR = format(as.Date(FIRSTDATE, origin="1970-01-01"), format = "%Y")) %>%
  #   mutate(LASTYEAR = format(as.Date(LASTDATE, origin="1970-01-01"), format = "%Y"))
  #control_all_dupRemove = clean_firstLastDate(control_all_dupRemove)
  # CLEANING UP THE DATES   
  control_all = control_all  %>%
    mutate(EFFDATE=as.Date(EFFDATE, origin="1970-01-01"),
           FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01"),
           LASTDATE=as.Date(LASTDATE, origin="1970-01-01"),
           ODATE = as.numeric(ODATE)*100 + 01,
           ODATE = as.Date(as.character(ODATE),  tryFormats="%Y%m%d"))
  
  control_all = control_all  %>%
    group_by(PID, EFFDATE) %>%
    mutate(dup_effdate = ifelse(n()>1, 1, 0))  
  
  # Replace duplicate  date with odate   
  control_all$EFFDATE_new =ifelse(control_all$dup_effdate==1 & control_all$time==1 & control_all$ODATE>control_all$EFFDATE, 
                              control_all$ODATE, control_all$EFFDATE)
  
  control_all = control_all %>%
    mutate(EFFDATE_new=as.Date(EFFDATE_new, origin="1970-01-01"))
  
  control_all = control_all %>%
    mutate(FIRSTDATE=EFFDATE_new) 
  
  control_all = control_all %>%
    group_by(PID) %>%
    mutate(LASTDATE_new=lead(EFFDATE_new, n=1)) 
  
  control_all$LASTDATE_new = ifelse(is.na(control_all$LASTDATE_new), as.Date("2021-01-01"), control_all$LASTDATE_new)
  control_all$LASTDATE_new = as.Date(control_all$LASTDATE_new,  origin="1970-01-01")
  control_all$EFFDATE_new = as.Date(control_all$EFFDATE_new,  origin="1970-01-01")

  
  control_all = control_all %>%
    mutate(FIRSTYEAR = format(as.Date(EFFDATE_new, origin="1970-01-01"), format = "%Y")) %>%
    mutate(LASTYEAR = format(as.Date(LASTDATE_new, origin="1970-01-01"), format = "%Y")) 
  
  # want to expand between 1990 - 2020
  seq = seq(1990, 2020, 1)
  
  #control_all_dupRemove$YEAR=as.double(control_all_dupRemove$LASTYEAR) # EXPAND BASED ON LAST DATE
  control_all$YEAR=as.double(control_all$FIRSTYEAR) 
  
  control_all_expand = control_all %>%
    group_by(FNAME, LNAME, CRD_DOB) %>%
    complete(YEAR = seq)
  
  control_all_expand$Treated = 0 # these are all control people so treated==0
  control_all_expand$EVENTYEAR = yr[y] 
  
  control_all_expand = as.data.table(control_all_expand)
  control_all_expand = control_all_expand[order(control_all_expand$FNAME, control_all_expand$LNAME, control_all_expand$CRD_DOB, control_all_expand$YEAR)] 
  
  ### Fill addresses
  control_all_expand = control_all_expand  %>% 
    group_by(FNAME, LNAME, CRD_DOB) %>%
    fill(ADDRESS, ADDRESS_noapt, CITY, STATE, ZIP, IDATE, ODATE, FLAG, HOUSE, 
         PREDIR, STREET, STRTYPE, POSTDIR, GENDER, PRE, MNAME, SUFFIX, DeceasedCD, 
         DOD, EFFDATE, EFFDATE_new, LASTDATE_new, 
         FIRSTDATE, LASTDATE, FIRSTYEAR, LASTYEAR, group_id, 
         EVENTYEAR, Treated, PID, .direction = "down") # fills addresses down within PID
  
  control_all_expand_rmoutofsample =control_all_expand %>%
    filter(!is.na(PID))
  
  rm(control_all_expand)

  control_all_expand_rmoutofsample$BUYOUT_ADDRESS = ifelse(control_all_expand_rmoutofsample$YEAR==yrList, control_all_expand_rmoutofsample$ADDRESS_noapt, NA)
  control_all_expand_rmoutofsample$BUYOUT_STATE = ifelse(control_all_expand_rmoutofsample$YEAR==yrList, control_all_expand_rmoutofsample$STATE, NA)
  control_all_expand_rmoutofsample$BUYOUT_ZIP = ifelse(control_all_expand_rmoutofsample$YEAR==yrList, control_all_expand_rmoutofsample$ZIP, NA)
  
  control_all_expand_rmoutofsample = control_all_expand_rmoutofsample %>%
    group_by(PID) %>%
    fill(BUYOUT_STATE,  .direction = 'down') %>%
    fill(BUYOUT_STATE,  .direction = 'up') %>%
    fill(BUYOUT_ZIP,  .direction = 'down') %>%
    fill(BUYOUT_ZIP,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'down') 
  
  control_all_expand_rmoutofsample = control_all_expand_rmoutofsample %>%
    mutate(MOVED_ZIP = ifelse((BUYOUT_ZIP!=ZIP),1,0)) %>%
    mutate(MOVED_STATE = ifelse((BUYOUT_STATE!=STATE),1,0)) %>%
    #mutate(MOVED_COUNTY = ifelse((BUYOUT_COUNTY!=COUNTY),1,0)) %>%
    mutate(moved_address = ifelse((ADDRESS!=BUYOUT_ADDRESS),1,0))
  
  control_all_expand_rmoutofsample = control_all_expand_rmoutofsample  %>% 
    mutate(EVENTTIME = YEAR - EVENTYEAR)
  
  control_all_expand_rmoutofsample = control_all_expand_rmoutofsample %>%
    filter(EVENTTIME>=-10)
  
  collapse_cohort_control = control_all_expand_rmoutofsample %>%
    group_by(EVENTTIME, Treated, EVENTYEAR) %>%
    summarise(mean_move_address = mean(moved_address, na.rm=TRUE),
              mean_move_zip = mean(MOVED_ZIP,  na.rm=TRUE),
              mean_move_state = mean(MOVED_STATE, na.rm=TRUE))
  
  
  save(control_all_expand_rmoutofsample, file = paste0("./output/2024/controlgroup_",yrList,"_expanded.RDa"))
  
}


#--------------------------------------------TREATED

for (y in 12:length(yr)) {
  yrList = yr[y]
  longYr = as.character(min(yrList)*10000 + 101) 
  longYr = as.double(as.Date(longYr, tryFormats = "%Y%m%d"))
  
  print(yrList)
  
  zipList = unique(na.omit(bo_zip_yr$Zip[bo_zip_yr$Fiscal.Year == yrList]))
  BO_PIDS = buyout$PID
  print(zipList)
  
  buyout_y = buyout[buyout$Fiscal.Year==yrList,]
  buyout_y$Address_clean =  str_replace_all(buyout_y$Address, "[[:punct:]]", " ")
  buyout_y$Address_clean = gsub("DRIVE","DR", buyout_y$Address_clean) 
  buyout_y$Address_clean = gsub("ROAD","RD", buyout_y$Address_clean) 
  buyout_y$Address_clean = gsub("STREET","ST", buyout_y$Address_clean) 
  buyout_y$Address_clean = gsub("AVENUE","AVE", buyout_y$Address_clean) 
  buyout_y$Owner_Name = buyout_y$Owner
  buyout_y = buyout_y %>% separate(col=Owner, into=c("LNAME", "FNAME"), sep=", ")
  
  address = as.vector(buyout_y$Address_clean)
  zip= as.vector(buyout_y$Zip)
  state = as.vector(buyout_y$State)
  
  start_time = Sys.time() #start stopwatch for reshaping
  all_tbl = tbl_reshape %>% 
    filter(ZIP %in% zip & FIRSTDATE<=longYr & LASTDATE>=longYr) 
  treated_all = as.data.table(all_tbl)  
  end_time = Sys.time() #start stopwatch 
  print(end_time - start_time)
  
  treated_all = treated_all[treated_all$ADDRESS!="NONE"]
  
  treated_all = treated_all %>%
    mutate(ADDRESS_noapt = gsub("\\ APT.*", "", ADDRESS)) %>%
    mutate(ADDRESS_noapt = gsub("\\ UNIT.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ #.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ STE.*", "", ADDRESS_noapt))
  
  test = left_join(buyout_y, treated_all, by= c("Address_clean"="ADDRESS_noapt" , "State" = "STATE", "Zip" = "ZIP"))

  test = test %>% 
    mutate(FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01")) %>%
    mutate(LASTDATE=as.Date(LASTDATE, origin="1970-01-01")) 
  
  test=test %>%
    group_by(FNAME.y, LNAME.y, CRD_DOB, Address_clean) %>%
    mutate(group_id = cur_group_id())
  
  dups = test$group_id[duplicated(test$group_id)]
  dupsDF = test[test$group_id %in% dups,]
  
  ids = as.vector(test$PID)
  
  # GRAB ALL THE ADDRESSES FOR THE PIDS FOUND FOR THE TREATED GROUP
  start_time = Sys.time() #start stopwatch for reshaping
  treated = tbl_reshape %>% 
    filter(PID %in% ids) 
  treated_all_address = as.data.table(treated)  
  end_time = Sys.time() #start stopwatch 
  print(end_time - start_time)
  
  #remove duplicate date/address
  treated_all_address = treated_all_address %>%
    distinct(PID, EFFDATE, ADDRESS, CITY, .keep_all = TRUE)
  
  #-----------------expand
  # treated_all_address = treated_all_address %>% 
  #   mutate(FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01")) %>%
  #   mutate(LASTDATE=as.Date(LASTDATE, origin="1970-01-01")) %>%
  #   mutate(FIRSTYEAR = format(as.Date(FIRSTDATE, origin="1970-01-01"), format = "%Y")) %>%
  #   mutate(LASTYEAR = format(as.Date(LASTDATE, origin="1970-01-01"), format = "%Y"))
  
  #treated_all_address = clean_firstLastDate(treated_all_address)
  
  treated_all_address = treated_all_address %>%
    mutate(ADDRESS_noapt = gsub("\\ APT.*", "", ADDRESS)) %>%
    mutate(ADDRESS_noapt = gsub("\\ UNIT.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ #.*", "", ADDRESS_noapt)) %>%
    mutate(ADDRESS_noapt = gsub("\\ STE.*", "", ADDRESS_noapt))
  
  #clean up the addresses (2024)
  treated_all_address = treated_all_address  %>%
    mutate(EFFDATE=as.Date(EFFDATE, origin="1970-01-01"),
           FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01"),
           LASTDATE=as.Date(LASTDATE, origin="1970-01-01"),
           ODATE = as.numeric(ODATE)*100 + 01,
           ODATE = as.Date(as.character(ODATE),  tryFormats="%Y%m%d"))
  
  treated_all_address = treated_all_address  %>%
    group_by(PID, EFFDATE) %>%
    mutate(dup_effdate = ifelse(n()>1, 1, 0))  
  
  # Replace duplicate  date with odate   
  treated_all_address$EFFDATE_new =ifelse(treated_all_address$dup_effdate==1 & treated_all_address$time==1 & treated_all_address$ODATE>treated_all_address$EFFDATE, 
                                  treated_all_address$ODATE, treated_all_address$EFFDATE)
  
  treated_all_address = treated_all_address %>%
    mutate(EFFDATE_new=as.Date(EFFDATE_new, origin="1970-01-01"))
  
  treated_all_address = treated_all_address %>%
    mutate(FIRSTDATE=EFFDATE_new) 
  
  treated_all_address = treated_all_address %>%
    group_by(PID) %>%
    mutate(LASTDATE_new=lead(EFFDATE_new, n=1)) 
  
  treated_all_address$LASTDATE_new = ifelse(is.na(treated_all_address$LASTDATE_new), as.Date("2021-01-01"), treated_all_address$LASTDATE_new)
  treated_all_address$LASTDATE_new = as.Date(treated_all_address$LASTDATE_new,  origin="1970-01-01")
  treated_all_address$EFFDATE_new = as.Date(treated_all_address$EFFDATE_new,  origin="1970-01-01")
  
  
  treated_all_address = treated_all_address %>%
    mutate(FIRSTYEAR = format(as.Date(EFFDATE_new, origin="1970-01-01"), format = "%Y")) %>%
    mutate(LASTYEAR = format(as.Date(LASTDATE_new, origin="1970-01-01"), format = "%Y")) 
  
  
  treated_all_address=treated_all_address %>%
    group_by(FNAME, LNAME, CRD_DOB) %>%
    mutate(group_id = cur_group_id())
  
  dups = treated_all_address$group_id[duplicated(treated_all_address$group_id)]
  dupsDF = treated_all_address[treated_all_address$group_id %in% dups,]
  
  #-------------- EXPAND ALL YEARS
  seq = seq(1985, 2020, 1)
  treated_all_address$YEAR=as.double(treated_all_address$LASTYEAR) 
  
  treated_all_address_expand = treated_all_address %>%
    group_by(group_id) %>%
    complete(YEAR = seq)
  
  
  #-------------- FILL UP THE ADDRESSES
  treated_all_address_expand = as.data.table(treated_all_address_expand)
  treated_all_address_expand = treated_all_address_expand[order(treated_all_address_expand$group_id, treated_all_address_expand$YEAR)] 
  ### Fill addresses
  treated_all_address_expand$Treated = 1
  treated_all_address_expand$EVENTYEAR = yrList
  treated_all_address_expand = treated_all_address_expand  %>% 
    group_by(group_id) %>%
    fill(ADDRESS, ADDRESS_noapt, CITY, STATE, ZIP, GENDER, CRD_DOB, PRE, FNAME, MNAME, LNAME, 
         DeceasedCD, PREDIR, HOUSE, FLAG, ODATE, IDATE, STREET, STRTYPE, PID, ADDRID,
         DOD, EFFDATE, FIRSTDATE, LASTDATE, FIRSTYEAR, LASTYEAR, EVENTYEAR, Treated, .direction = "up") # fills addresses up within PID
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand  %>% 
    filter(FIRSTYEAR<=YEAR)
  

  
  
  #treated_all_address_expand = as.data.table(treated_all_address_expand)
  #treated_all_address_expand = treated_all_address_expand[order(treated_all_address_expand$PID, treated_all_address_expand$YEAR)] 
  ### Fill addresses
  #treated_all_address_expand$Treated = 1
  #treated_all_address_expand$EVENTYEAR = yrList
  #treated_all_address_expand = treated_all_address_expand  %>% 
  #  group_by(PID) %>%
  #  fill(ADDRESS, CITY, STATE, ZIP, HOUSE, PREDIR, STREET, STRTYPE, POSTDIR,
  #       GENDER, CRD_DOB, PRE, FNAME, LNAME, DeceasedCD, DOD, EFFDATE,
  #       FIRSTDATE, LASTDATE, FIRSTYEAR, LASTYEAR, EVENTYEAR, Treated, .direction = "up") # fills addresses down within PID
  
  # treated_all_address_expand_rmoutofsample =  treated_all_address_expand[treated_all_address_expand$YEAR>=treated_all_address_expand$FIRSTYEAR , ]
  #rm(treated_all_address_expand)
  
  #treated_all_address_expand_rmoutofsample$YEAR = treated_all_address_expand_rmoutofsample$EXPAND_YEAR
  treated_all_address_expand_rmoutofsample$BUYOUT_ADDRESS = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$ADDRESS, NA)
  treated_all_address_expand_rmoutofsample$BUYOUT_STATE = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$STATE, NA)
  treated_all_address_expand_rmoutofsample$BUYOUT_ZIP = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$ZIP, NA)
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample %>%
    group_by(PID) %>%
    fill(BUYOUT_STATE,  .direction = 'down') %>%
    fill(BUYOUT_STATE,  .direction = 'up') %>%
    fill(BUYOUT_ZIP,  .direction = 'down') %>%
    fill(BUYOUT_ZIP,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'down') 
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample %>%
    mutate(MOVED_ZIP = ifelse((BUYOUT_ZIP!=ZIP),1,0)) %>%
    mutate(MOVED_STATE = ifelse((BUYOUT_STATE!=STATE),1,0)) %>%
    #mutate(MOVED_COUNTY = ifelse((BUYOUT_COUNTY!=COUNTY),1,0)) %>%
    mutate(moved_address = ifelse((ADDRESS!=BUYOUT_ADDRESS),1,0))
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample  %>% 
    mutate(EVENTTIME = YEAR - EVENTYEAR)
  
  save(treated_all_address_expand_rmoutofsample, file = paste0("./output/2024/treatedgroup_",yrList,"_expanded.RDa"))
  
  collapse_cohort = treated_all_address_expand_rmoutofsample %>%
    group_by(EVENTTIME, Treated, EVENTYEAR) %>%
    summarise(mean_move_address = mean(moved_address, na.rm=TRUE),
              mean_move_zip = mean(MOVED_ZIP,  na.rm=TRUE),
              mean_move_state = mean(MOVED_STATE, na.rm=TRUE))
              
  rm(treated_all_address_expand_rmoutofsample)
  
}

dbDisconnect(tdb)
rm(list=ls())


buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")
bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))

rm(bo_zip_yr, buyout)


# Append treated and control together
for (y in 13:length(yr)) {
  yrList = yr[y]
  load(paste0("./output/2024/treatedgroup_",yrList,"_expanded.RDa"))
  
  treated_all_address_expand_rmoutofsample$YEAR = treated_all_address_expand_rmoutofsample$EXPAND_YEAR
  treated_all_address_expand_rmoutofsample$BUYOUT_ADDRESS = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$ADDRESS, NA)
  treated_all_address_expand_rmoutofsample$BUYOUT_STATE = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$STATE, NA)
  treated_all_address_expand_rmoutofsample$BUYOUT_ZIP = ifelse(treated_all_address_expand_rmoutofsample$YEAR==yrList, treated_all_address_expand_rmoutofsample$ZIP, NA)
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample %>%
    group_by(PID) %>%
    fill(BUYOUT_STATE,  .direction = 'down') %>%
    fill(BUYOUT_STATE,  .direction = 'up') %>%
    fill(BUYOUT_ZIP,  .direction = 'down') %>%
    fill(BUYOUT_ZIP,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'up') %>%
    fill(BUYOUT_ADDRESS,  .direction = 'down') 
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample %>%
    mutate(MOVED_ZIP = ifelse((BUYOUT_ZIP!=ZIP),1,0)) %>%
    mutate(MOVED_STATE = ifelse((BUYOUT_STATE!=STATE),1,0)) %>%
    #mutate(MOVED_COUNTY = ifelse((BUYOUT_COUNTY!=COUNTY),1,0)) %>%
    mutate(moved_address = ifelse((ADDRESS!=BUYOUT_ADDRESS),1,0))
  
  treated_all_address_expand_rmoutofsample = treated_all_address_expand_rmoutofsample  %>% 
    mutate(EVENTTIME = YEAR - EVENTYEAR)
  
  
  Full_sample = rbind(treated_all_address_expand_rmoutofsample, control_all_expand_rmoutofsample)
  save(Full_sample, file = paste0("./output/2024/fullsample_",yrList,"_expanded.RDa"))
  
}

# Append all the years together
load("./output/2024/fullsample_2000_expanded.RDa")

Full_sample_expanded = Full_sample
rm(Full_sample)

for (y in 13:length(yr)) {
  yrList = yr[y]
  file = paste0("./output/2024/fullsample_",yrList,"_expanded.RDa")
  
  load(file)
  
  Full_sample_expanded = rbind(Full_sample_expanded,Full_sample)
  rm(Full_sample)
}


Full_sample_expanded = Full_sample_expanded %>%  filter(EVENTTIME>=-15)
save(Full_sample_expanded, file = paste0("./output/2024/fullsample_expanded_20002017.RDa"))




# Save just the unique addresses for geocoding
rm(Full_sample_expanded)
load("./output/2024/fullsample_2000_expanded.RDa")

unique_address = unique(Full_sample[,15:18])
rm(Full_sample)

for (y in 13:length(yr)) {
  yrList = yr[y]
  file = paste0("./output/2024/fullsample_",yrList,"_expanded.RDa")
  
  load(file)
  unique_address_y = unique(Full_sample[,15:18])
  
  unique_address = rbind(unique_address,unique_address_y)
  rm(unique_address_y)
}


save(unique_address, file = paste0("./output/uniqueAddress_20002017.RDa"))
write_csv(unique_address, file = paste0("./output/uniqueAddress_20002017.csv"))

write_csv(unique_address, file = paste0("/drives/drive1/ForDownload/Young_uniqueAddress_20002017.csv"))


collapse_cohort = treated_all_address_expand_rmoutofsample %>%
  group_by(EVENTTIME, Treated) %>%
  summarise(mean_move_address = mean(moved_address, na.rm=TRUE))