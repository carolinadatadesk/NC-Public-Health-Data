library(tidyverse)
library(gsheet)
library(here)


CompiledLHDExpenditures <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Zc10pam92Y1218F90eXn9ri7-a4GQI1vhzING33nNSI/edit#gid=0") %>%
  select(1:5)

write.csv(CompiledLHDExpenditures,here("data", "handmade","CompiledLHDExpenditures.csv"),row.names=FALSE)
