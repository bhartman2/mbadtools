## code to prepare `moody` dataset goes here

# read wage growth data
library(readxl)
wage_growth_data <- read_excel("../inst/wage-growth-data.xlsx", 
                               skip = 1)
# read ICS survey data
library(readxl)
redbk01a <- read_excel("../inst/redbk01a.xls")

# wrangle survey data
umics = redbk01a %>% 
  rename(Month=`Date  of  Survey`, Year=2) %>% na.omit %>% 
  mutate(Mo = match(Month, month.name)) %>%
  mutate(Date=paste0(Year,"-",Mo,"-01"), 
         Datep=as.POSIXct(as.Date(Date))%m+%months(1))

umics1 = umics %>% 
  dplyr::filter(Datep>=waget[1,]$Date) %>% 
  select(Datep, ICS)

# wrangle Wgrowth data
waget = wage_growth_data %>% select(1,2) %>% rename(Date=1, Wgrowth=2) %>%
  mutate(Wgrowth=as.numeric(Wgrowth)) %>% na.omit %>%
  mutate(Month = lubridate::month(Date), Year=lubridate::year(Date))

# reate data frame
moody = data.frame(waget, umics1) %>% select(Date, Wgrowth, ICS)

## create dataset in /data folder
usethis::use_data(moody, overwrite = TRUE)