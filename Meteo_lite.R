# Data are pulled from ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/
# The two Waterville codes are:
# 726073 14615 WATERVILLE ROBERT LAFLEUR ARP US   ME KWVL  +44.533 -069.667 +0094.5 20060101 20171225
# 726073 99999 WATERVILLE R LAFLEUR          US   ME KWVL  +44.533 -069.683 +0101.0 19880105 20071231
#
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# Create the list of links
link <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/"
yr <- 1991:2019 
YM <- ifelse(yr > 2005, paste(link,yr,"/","726073-14615-",yr,".gz",sep="" ),
                        paste(link,yr,"/","726073-99999-",yr,".gz",sep="" ))

# Define the column names
col.names <- c("year","month", "day", "hour", "temp", "dp", "press", "Dir", 
                 "speed", "sky", "precip_1hr", "precip_6hr")

dat <- data.frame()  
for(i in YM) {
  df <- read_fwf(i, col_types="iiiinnnnnnnn",
                   fwf_widths(c(4,3,3,3,6,6,6,6,6,6,6,6),
                              col_names = col.names),
                   na = "-9999")

  
  # Add missing combinations of month, day, year
  df1 <- df %>%  complete(year,month,day,hour) 
  
  # Process some of the attributes
  df2 <- df1 %>% 
    mutate(Date = ymd_h(paste(year,month,day,hour,tz = "UTC")),
           Date_EST =  with_tz( Date, "America/New_York"),
           Speed = speed / 10 * 2.23694, # convert m/s to mph after rescaling
           Temp = 1.8 * temp / 10 + 32, # convert C to F after rescaling
           DP   = 1.8 * dp / 10 + 32,
           Press = press / 10, # rescale pressure, note that 1 hectopascal = 1 mb
           Precip_1hr = precip_1hr / 10 * 0.0393701,  # rescale and convert to inches
           Precip_6hr = precip_6hr / 10 * 0.0393701) %>% 
    filter(!is.na(Date)) %>%  # Remove invalid dates created by the "complete()" function
    select(Date, Date_EST, Temp,DP,Speed, Dir, Press, Precip_1hr,Precip_6hr)
  
  dat <- rbind(dat,df2)
  # The FTP site does not like being hit too often over a short period of time
  # from the same IP address, so to prevent being locked out, pause in
  # between each download
  Sys.sleep(4) 
}

saveRDS(dat,"met1991_2019.Rds")

