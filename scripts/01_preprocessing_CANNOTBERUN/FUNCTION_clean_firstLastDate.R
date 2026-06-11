#########################################################
# Description: Function that fixes first and last data in the
#             Infutor data. Important prep for Expanding to 
#             person-year level.
#             This also sets up to expand based on LASTDATE.
#
# Date: 11/11/2021
# Author: Rachel Young
# Name: FUNCTION_clean_firstLastDate.R
#########################################################
library(lubridate)

clean_firstLastDate = function(all_addresses) {

    substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  #--------- Try to fix the last date to catch some of the people that have multiple addresses with same EFFDATE
  all_addresses = all_addresses %>%
    mutate(FIRSTDATE=as.Date(FIRSTDATE, origin="1970-01-01")) %>%
    mutate(LASTDATE=as.Date(LASTDATE, origin="1970-01-01")) %>%
    mutate(ODATE = as.numeric(ODATE)*100 + 01) %>%
    mutate(ODATE = as.Date(as.character(ODATE),  tryFormats="%Y%m%d")) %>%
    mutate(DOD = as.numeric(DOD)*100 + 01) %>%
    mutate(DOD = as.Date(as.character(DOD),  tryFormats="%Y%m%d")) %>%
    mutate(CRD_DOB = ifelse(substrRight(CRD_DOB, 2)=="00", (as.numeric(CRD_DOB) + 01), as.numeric(CRD_DOB))) %>%
    mutate(CRD_DOB = ifelse(as.numeric(CRD_DOB)>190000, (as.numeric(CRD_DOB)*100 + 01), (as.numeric(CRD_DOB)*10000 + 101))) %>%
    mutate(CRD_DOB = as.Date(as.character(CRD_DOB), tryFormats="%Y%m%d"))
 
  # find duplicate EFFDATES
  all_addresses = all_addresses %>%
    group_by(PID, EFFDATE) %>%
    mutate(dup_effdate = ifelse(n()>1, 1, 0))
  
  # Replace duplicate last date with odate if first date and last date are equal  
  all_addresses$LASTDATE =ifelse(all_addresses$dup_effdate==1 & all_addresses$FIRSTDATE== all_addresses$LASTDATE & !is.na(all_addresses$ODATE), 
                                   all_addresses$ODATE, all_addresses$LASTDATE) 
  
  all_addresses = all_addresses %>% mutate(LASTDATE=as.Date(LASTDATE, origin="1970-01-01")) 
  
  # Replace last date if person died
  all_addresses = all_addresses %>% 
    mutate(DOD = ifelse(DeceasedCD=="Y" & is.na(DOD), 
                        as.Date(ODATE,  origin="1970-01-01"), 
                        as.Date(DOD,  origin="1970-01-01")))  # replace missing DOD with ODATE
  
  all_addresses = all_addresses %>% 
    mutate(DOD = ifelse(DeceasedCD=="Y" & is.na(DOD) & is.na(ODATE), 
                        as.Date((CRD_DOB %m+% years(80)),origin="1970-01-01") , 
                        as.Date(DOD,  origin="1970-01-01")))  # replace missing DOD with year of birth + 90 years
  
  all_addresses = all_addresses %>%
    mutate(DOD = as.Date(DOD,  origin="1970-01-01")) 
  
  max = c("2020/08/01")
  all_addresses$LASTDATE =  ifelse(all_addresses$DeceasedCD=="Y" & !is.na(all_addresses$DOD), 
                                   all_addresses$DOD, all_addresses$LASTDATE) 
  
  
  all_addresses$LASTDATE =  ifelse(all_addresses$LASTDATE>as.Date(as.character(max)), 
                                   as.Date(max, tryFormats = "%Y/%m/%d"), all_addresses$LASTDATE)
  
  
  all_addresses = all_addresses %>%
    mutate(LASTDATE = as.Date(LASTDATE,  origin="1970-01-01")) 
  
  # Create first and last year (MiGHT NEED TO ROUND)
  all_addresses = all_addresses %>% 
    mutate(FIRSTYEAR = format(as.Date(FIRSTDATE, origin="1970-01-01"), format = "%Y")) %>%
    mutate(LASTYEAR = format(as.Date(LASTDATE, origin="1970-01-01"), format = "%Y"))
  
  all_addresses = all_addresses %>% 
    mutate(FIRSTMONTH = format(as.Date(FIRSTDATE, origin="1970-01-01"), format = "%m")) %>%
    mutate(LASTMONTH = format(as.Date(LASTDATE, origin="1970-01-01"), format = "%m"))
  
 
  #plots = list(n_plot, address_move_plot, zip_move_plot, state_move_plot, n_disasteryear_plot)
  return(all_addresses)

}
