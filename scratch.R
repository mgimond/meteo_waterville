library(dplyr)
library(lubridate)

dat <- readRDS("met1991_2022.Rds")

dat1 <- dat %>%
  filter(year(Date) %in% c(1993, 1994, 1995, 2003:2021),
         month(Date) %in% 1:3) %>% 
  mutate(thaw =                                                                              r44444444
         temp.lag = (lag(Temp, 1) + lag(Temp, 2) +  lag(Temp, 3) + lag(Temp, 4)) / 4,
         temp.lead = (Temp + lead(Temp, 1) +  lead(Temp, 2) + lead(Temp, 3) ) / 4,
         lag = lag(thaw, 1) +lag(thaw, 2) +  lag(thaw, 3) + lag(thaw, 4),
         diff = abs(lag - lead) == 4,
         temp.diff = diff * abs(temp.lead - temp.lag))

dat2 <- dat1 %>% 
  filter(diff == 1) %>% 
  group_by(year = year(Date)) %>% 
  summarise(sum = sum(diff, na.rm = TRUE), mean = mean(temp.diff, na.rm = TRUE)) 

library(ggplot2)
ggplot(dat2, aes(year,sum, col = mean))+ geom_point() + geom_smooth()+
  scale_color_continuous()



  
dat1

         
         