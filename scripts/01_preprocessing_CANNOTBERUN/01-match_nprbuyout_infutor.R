#############################################
# This program selects the fuzzy matched 
# Infutor addresses and IDs from the 
# NPR buyout data and stores them in a new database
#############################################

#sink("~/project/buyout/scripts/sub_buyout_zips_LOG.txt")

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



start_time_total = Sys.time() #start stopwatch for reshaping

setwd("~/project/buyout")
source("./scripts/FUNCTION_clean_firstLastDate.R")
'%!in%' <- function(x,y)!('%in%'(x,y))


#------- Connect to the reshaped Infutor Data ----------#
mydb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./output/reshapeCRD4-db-date.sqlite3")
con_tbl <- tbl(mydb, "reshape")



#---- Read in the buyout addresses -----#
buyout = read.csv("./rawdata/femabuyouts_geocodio_731a9aab2411ecabf5fe354a1e8b56413558ca09.csv")

bo_zip_yr = buyout %>% mutate(n=1) %>% group_by(Fiscal.Year, Zip) %>% summarise(length=sum(n))
bo_zip_yr = bo_zip_yr[bo_zip_yr$Zip!="",]
yr = unique(na.omit(bo_zip_yr$Fiscal.Year))
yearscounter = 6
IDS = c()

#for(z in 1:length(zips)) { # Loop over the zip codes in the NPR buyout data
for(y in 6:8) {  
  print(y)
  
  if (yearscounter==1) {
    yrList = yr[1:5]
  }
  if (yearscounter==2) {
    yrList = yr[6:8]
  }
  if (yearscounter==3) {
    yrList = yr[9:11]
  }
  if (yearscounter==4) {
    yrList = yr[12:15]
  }
  if (yearscounter==5) {
    yrList = yr[16:18]
  }
  if (yearscounter==6) {
    yrList = yr[19:21]
  }
  if (yearscounter==7) {
    yrList = yr[22:24]
  }
  if (yearscounter==8) {
    yrList = yr[25:29]
  }
  print(yrList)
  
  minYr = as.character(min(yrList)*10000 + 101) 
  minYr = as.double(as.Date(minYr, tryFormats = "%Y%m%d"))
  maxYr = as.character(max(yrList)*10000 + 101) 
  maxYr = as.double(as.Date(maxYr, tryFormats = "%Y%m%d"))
  
  zipList = unique(na.omit(bo_zip_yr$Zip[bo_zip_yr$Fiscal.Year %in% yrList]))
  
  print(head(zipList))
  
  buyout.zipsub = buyout %>%
    filter(Zip %in% zipList & Fiscal.Year %in% yrList) 
  buyout.zipsub = buyout.zipsub %>% separate(Owner, c("LNAME", "FNAME"), ",", extra = "drop")
  
  
  #---- Keep rows in the Infutor Data that are the z zipcode ----#
  print("read in data from database")
  start_time = Sys.time() #start stopwatch for reshaping
  
  tbl = con_tbl %>% 
    filter(ZIP %in% zipList & FIRSTDATE<=minYr & LASTDATE>=maxYr) 
    
  
  infutor = as.data.table(tbl)
  
  end_time = Sys.time() #start stopwatch for reshaping
  print(end_time - start_time)
  
  
   infutor = infutor %>% 
     mutate(FIRSTYEAR = format(as.Date(FIRSTDATE, origin="1970-01-01"), format = "%Y")) %>%
     mutate(LASTYEAR = format(as.Date(LASTDATE, origin="1970-01-01"), format = "%Y"))
  
  
  #----------- Filter addresses for people living in ZIP in the right years ------------#
  filter_infutor_id = c()
  for(z in 1:length(zipList)) {
    print(z)
    zip = zipList[z]
    
    infutor.zip = infutor %>% filter(ZIP==zip) # filter the read in infutor data by each zip
    
    boYr = unique(buyout.zipsub$Fiscal.Year[buyout.zipsub$Zip==zip])
    
  if(length(infutor.zip)>1) {
  cat("There are",length(boYr), "years in zip", zip)
  
  for(y in 1:length(boYr)) {
    test = infutor.zip %>% filter((FIRSTYEAR<=boYr[y] & LASTYEAR>=boYr[y])) # filter the filtered zip infutor data for within the relevant time period 
    
    x = test$PID # keep the ids of the people in the right zip in the right time period
    
    filter_infutor_id = append(filter_infutor_id,  x) # add to the list
    #print(filter_infutor_id)
  }
  } 
  else {
    print("No Zips")
    next
  }
  }
  filter_infutor_id = unique(filter_infutor_id)
  length(filter_infutor_id)
  write.table(filter_infutor_id, file = "./output/01-PIDs_rightZip_rightTime.csv", append = TRUE)
  
  
  infutor = infutor %>% filter(PID %in% filter_infutor_id) # filter the people identified as in the right zip in the right time period
  
  outdb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_v2.sqlite3")
  if(yearscounter==1) {
    dbWriteTable(outdb, "rightZiprightTime", infutor, overwrite = TRUE) 
  } else {
    dbWriteTable(outdb, "rightZiprightTime", infutor, append = TRUE) 
  }
  dbDisconnect(outdb)
  
  
  #----------- Grab the exact matches Address and Name ------------#
  
  all_matched = left_join(infutor, buyout.zipsub, by=c("ADDRESS" = "Address", "CITY"="City", "STATE"="State", "ZIP"="Zip"))
  all_matched = all_matched %>% select(-c("Street"))
  
  matched_id = all_matched$PID[!is.na(all_matched$Fiscal.Year)]
  
  matched = all_matched[all_matched$PID %in% matched_id] # This is the panel of addresses for the people that had a perfect match with buyout address

  others = all_matched[all_matched$PID %!in% matched_id] # This is the panel of everyone not perfect matched
  
  ## Check the number of matches against the size of buyout.
  ### Break if less than 80% match
  # if(sum(!is.na(all_matched$Fiscal.Year))/nrow(buyout.zipsub)<.8 ) {
  #   # Eventually put fuzzy match in here
  #   write.table(zip, file="./output/Buyout_Unmatched_ZIP.csv",
  #               append=TRUE,
  #               col.names = FALSE,
  #               sep = ',') 
  # } 

  name_match = left_join(infutor, buyout.zipsub, by=c("ADDRESS" = "Address","LNAME" = "LNAME", "FNAME"="FNAME"))
  name_match = name_match %>% select(-c("Street", "City", "State", "Zip"))
  
  #----------- Save the matches and the people in the zip in the right time period and the combined people in database ------------#
  
  outdb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_v2.sqlite3")
  if(yearscounter==1) {
    dbWriteTable(outdb, "infutorbuyoutperfectaddressmatch", matched, overwrite = TRUE) 
    dbWriteTable(outdb, "infutorotherslivinginzip", others, overwrite = TRUE) 
    dbWriteTable(outdb, "infutorbuyoutperfectnamematch", name_match, overwrite = TRUE) 
  } else {
    dbWriteTable(outdb, "infutorbuyoutperfectaddressmatch", matched, append = TRUE) 
    dbWriteTable(outdb, "infutorotherslivinginzip", others, append = TRUE) 
    dbWriteTable(outdb, "infutorbuyoutperfectnamematch", name_match,append = TRUE)
  }
  dbDisconnect(outdb)
  cat("finished", yearscounter)
  
  rm(matched, others, name_match, infutor.zip, test, infutor)
  
  yearscounter = yearscounter + 1
}

dbDisconnect(mydb)



end_time_total = Sys.time() #start stopwatch for reshaping
print(end_time_total - start_time_total)






  #--- Keep rows that fuzzy match the address ----#
  if(nrow(buyout.zipsub)>1) {
  g1 <- gammaCK2par(infutor$ADDRESS, buyout.zipsub$Address, cut.a = 0.85, method = "lv")

  if(length(g1$matches2)>0) {

    temp <- list()
    for(i in 1:length(g1$matches2)) {
      temp[[i]] <- expand.grid(unlist(g1$matches2[[i]][1]),
                               unlist(g1$matches2[[i]][2]))
    }
    ids <- do.call('rbind', temp)
    matched.1 <- infutor[ids$Var1, ]
    matched.2 = buyout.zipsub[ids$Var2,]

    matched.1$id = 1:nrow(matched.1)
    matched.2$id = 1:nrow(matched.2)

    #--- Keep rows that are in the right time interval ----#
    matched.all = merge(matched.1, matched.2, by = "id")
    keep = matched.all %>%
      filter(FIRSTDATE<=as.Date(ISOdate(Fiscal.Year, 1, 1)) & LASTDATE>= as.Date(ISOdate(Fiscal.Year, 1, 1))) %>%
      mutate(buyout_match_flag = 1) %>%
      rename(ID_bo = ID) %>%
      rename(Address_bo = Address) %>%
      rename(City_bo = City) %>%
      rename(State_bo = State) %>%
      rename(Zip_bo = Zip) %>%
      rename(Street_bo = Street)


    # Find the IDs that lived in the matched address
    id_tbl = table(unique(keep$PID))
    PIDS = as.data.table(id_tbl,col.names=c("id","freq"))
    PIDS = PIDS[,-2]




    ## add IDs and full Infutor address data to new database
    outdb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_2010_v2.sqlite3")
    if(zipcounter==1) {
   # dbWriteTable(outdb, "infutorzip", infutor, overwrite = TRUE)
    dbWriteTable(outdb, "infutormatchedaddresses", keep, overwrite = TRUE)
    dbWriteTable(outdb, "ids", PIDS, overwrite = TRUE)
     } else {
    #dbWriteTable(outdb, "infutorzip", infutor, append = TRUE)
    dbWriteTable(outdb, "infutormatchedaddresses", keep, append = TRUE)
    dbWriteTable(outdb, "ids", PIDS, append = TRUE)
    }
    dbDisconnect(outdb)

  } else {
    cat("No Address Matches for", zip, '\n')
    write.table(buyout.zipsub, file = "./output/buyoutzips_failed.csv", append = TRUE)
  }
  } else {
    cat("Buyout", zip, "has too few addresses", '\n')
    write.table(buyout.zipsub, file = "./output/buyoutaddresses_failed.csv", append = TRUE)
  }

  ### Store the counts in case things fail
  write.table(zipcounter, file = "./output/counter_zip.csv")

  zipcounter = zipcounter + 1




dbDisconnect(mydb)


#---- Grab unique IDs from sub_infutor_buyout -----#

mydb <- dbConnect(RSQLite::SQLite(), "./data/sub_infutor_buyout_2010_v2.sqlite3")
con_tbl <- tbl(mydb, "infutormatchedaddresses")
con_tbl_id <- tbl(mydb, "ids")
id_dt = as.data.table(con_tbl_id)
add_dt = as.data.table(con_tbl)
dbDisconnect(mydb)

#---- All addresses for unique IDs -----#

mydb <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./output/reshapeCRD4-db-date.sqlite3")
con_tbl <- tbl(mydb, "reshape")

id = id_dt$V1

tbl = con_tbl %>%
  filter(PID %in% id)

bo_ppl = as.data.table(tbl)

#----- Join the buyout address flag with the panel of addresses by ID -----#

con_tbl_flag = con_tbl %>%
  select("PID", "buyout_match_flag", "ADDRESS", "CITY", "ZIP", "STATE")

bo_ppl_f = left_join(bo_ppl, add_dt)


#----------- Save RData File ------------#
save(bo_ppl_f, file = "./data/infutor_bo_ppl_2010_v2.RData")

dbDisconnect(mydb)

end_time_total = Sys.time() #start stopwatch for reshaping
print(end_time_total - start_time_total)

sink()


