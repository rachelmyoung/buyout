#NEW7-uniqueaddress-event-year.R
library(data.table)
library(tidyverse)
library(readr)
library(DBI)
library(tidyr)
library(rlang)
library('fastLink')
library(sandwich)
library(lmtest)
library(geosphere)

# Create empty dataframe for PIDs and TREATED Dummy
data <- data.frame(ADDRESS = 0,
                   CITY = 0,                     
                   ZIP = 0,
                   STATE = 0, 
                   EVENTYEAR = 0,
                   xmin = 0,
                   xmax = 0,
                   ymin = 0,
                   ymax = 0)


for (y in 2000:2017) {
  print(y)
  
  #load(paste0("~/project/buyout/output/fullsample_",y,"_expanded.RDa"))
  Full_sample=read_dta(paste0("~/project/buyout/output/TreatedControlAddresses_forRegression_",y,".dta"))
  subset= Full_sample[, (colnames(Full_sample) %in% c('ADDRESS', 'CITY', 
                                  'ZIP', 'STATE', 'EVENTYEAR', 'xmin', 'xmax', 'ymin', 'ymax'))]
  subset = subset %>%
       distinct()
  
  write_csv(subset, file = paste0("output/NEW7-uniqueaddress-event-year",y,".csv"))
  
  data = rbind(data, subset)  
}

data = data[-1,]
save(data, file = "NEW7-uniqueaddress-event-year.RData")