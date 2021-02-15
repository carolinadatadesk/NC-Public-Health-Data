library(dplyr)
library(gsheet)

CompiledLHDExpenditures <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Zc10pam92Y1218F90eXn9ri7-a4GQI1vhzING33nNSI/edit#gid=0") %>%
  select(1:5)






#####


pagedata <- CompiledLHDExpenditures %>% filter(year >=2010, year<=2018) %>%
  pivot_wider(id_cols = c(lhd_name,county_name, lhd_area_type), names_from = year, values_from = expenditures, names_prefix = "year") %>%
  filter(!is.na(year2010),
         !is.na(year2011),
         !is.na(year2012),
         !is.na(year2013),
         !is.na(year2014),
         !is.na(year2015),
         !is.na(year2016),
         !is.na(year2017),
         !is.na(year2018)) %>%
  arrange(lhd_name)

yearmeans<-pagedata %>% summarise_if(is.numeric, mean)

setwd("./data/html_reports/LHD_reports")
