##
## finding continuous hours with pre-defined max temperature
##

library(dbplyr) # Using development version 1.1.0.9 as of Dec 2017
library(dplyr)
library(lubridate)
library(tidyr)

# Create database connection
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                      host = rstudioapi::askForPassword("hostname"),
                      user = "edauser",
                      dbname = "postgres",
                      password = rstudioapi::askForPassword("Password"))

# Connect to meteorological data table
met <- tbl(con, in_schema("eda", "wat_met2"))

# Set the max temperature to filter b
maxvl <- 5

# Query data and identify temperature values less than 5
met2 <- met %>% select(temp,date) %>% 
  arrange(date) %>% collect () %>% 
  mutate( isless = as.numeric(temp < maxvl)) %>% 
  collect()

# Preliminary grouping of records
df2 <- met2 %>%
  filter(!is.na(temp)) %>% 
  mutate(isless = as.numeric(temp >= maxvl), # No typo; we are interested in less than maxvl
         z = cumsum(isless)) %>%
  group_by(z) %>%
  mutate(grp = row_number() -1) %>%
  arrange(desc(row_number())) %>% 
  ungroup()

# Create unique groups by date id and cumulative hours id
flag = 0
df2$hrs <- NA
df2$grp2 <- NA
for (i in 1:nrow(df2)){
  if( df2$grp[i] > 0 ){
    if(flag == 0) {
      max =  df2$grp[i]
      dyr =  decimal_date(df2$date[i])
      flag = 1
    }
    df2$hrs[i] = max
    df2$grp2[i] = dyr
  }else{
    flag = 0
  }
}

# Summarize by date id and cumulative hours id
df3 <- df2 %>% 
  filter(isless == 0) %>% 
  select(date, temp, grp2, hrs) %>% 
  group_by(grp2,hrs) %>% 
  summarize(mindt = min(date), maxdt = max(date),
            mintp = min(temp), maxtp = max(temp)) 

DBI::dbDisconnect(con)

# y <- met2 %>% filter(is.na(temp)==0) %>% mutate(run = cumsum(isless) * isless) %>% pull(run)


## Test
# df <- data.frame(a = c(10,4,1,-1,2,12,6,6,-10,-10,4,10,1,2,1))
# df3 <- df %>%
#   mutate(y = a >= 5,
#          z = cumsum(y)) %>%
#   group_by(z) %>%
#   mutate(grp = row_number() -1) %>%
#   arrange(desc(row_number()))
# 
# flag = 0
# df3$grp2 <- NA
# for (i in 1:nrow(df3)){
#   if( df3$grp[i] > 0 ){
#     if(flag == 0) {max =  df3$grp[i]; flag = 1}
#     df3$grp2[i] = max
#   }else{
#     flag = 0
#   }
# }
## End of test


