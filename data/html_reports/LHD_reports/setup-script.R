library(tidyverse)
library(gsheet)

CompiledLHDExpenditures <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1Zc10pam92Y1218F90eXn9ri7-a4GQI1vhzING33nNSI/edit#gid=0") %>% select(1:5)


# Adding Population Data
county_pop <- read_delim("https://linc.osbm.nc.gov/explore/dataset/census-population-and-housing-linc/download/?format=csv&disjunctive.area_name=true&disjunctive.year=true&disjunctive.variable=true&refine.area_type=County&refine.variable=Population+Estimate+(BEA+per+Capita+Denominator)&timezone=America/New_York&lang=en&use_labels_for_header=true&csv_separator=%3B",
                         ";", escape_double = FALSE, col_types = cols_only(`Area Name` = col_character(),
                                                                           Year = col_double(), Value = col_double()),
                         trim_ws = TRUE) %>%
  rename(county_name = `Area Name`, year = `Year`)


county_pop$county_name <-  str_sub(county_pop$county_name, end = -8)


county_pop<- county_pop %>%
  filter(county_name %in% c("Granville","Vance")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Granville & Vance") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)


county_pop<- county_pop %>%
  filter(county_name %in% c("Avery", "Mitchell", "Yancey")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Avery, Mitchell & Yancey") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)


county_pop<- county_pop %>%
  filter(county_name %in% c("Martin", "Tyrrell", "Washington")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Martin, Tyrrell & Washington") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)

county_pop<- county_pop %>%
  filter(county_name %in% c("Camden", "Chowan", "Currituck", "Bertie", "Gates", "Hertford","Pasquotank", "Perquimans")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Camden, Chowan, Currituck, Bertie, Gates, Hertford,Pasquotank, Perquimans") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)

county_pop<- county_pop %>%
  filter(county_name %in% c("Alleghany", "Ashe", "Watauga")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Alleghany, Ashe, Watauga") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)

county_pop<- county_pop %>%
  filter(county_name %in% c("Rutherford", "Polk", "McDowell")) %>%
  group_by(year) %>%
  summarise(Value=sum(Value)) %>%
  mutate(county_name = "Rutherford, Polk, McDowell") %>%
  select(county_name, year, Value) %>%
  union_all(county_pop)%>%

  rename(population=Value)




CompiledLHDExpenditures<- left_join(CompiledLHDExpenditures, county_pop, c("county_name" = "county_name", "year" = "year"))

### LHD Per Capita Spending
CompiledLHDExpenditures <-
  CompiledLHDExpenditures %>%
  filter(!is.na(population), !is.na(expenditures)) %>%
  mutate(expenditures_per_capita = expenditures / population)

CompiledLHDExpenditures %>%
  select(year, lhd_name, county_name, expenditures_per_capita)



##### Inflation Rates
# originally sourced from https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=96&series=a&first_year=2010&last_year=2019&scale=-99&categories=survey&thetable= but doing the math here to ensure that we can make apples-to-apples comparison with KHD data and story.

infl_rates <-
  read_csv("https://raw.githubusercontent.com/khnews/2020-underfunded-under-threat-data/master/data/01-state-public-health-agencies.csv") %>%
  filter(!is.na(expenditures), !is.na(expenditures_infl),) %>%
  group_by(year) %>%
  summarize(total_raw = max(expenditures, na.rm=TRUE), total_infl = max(expenditures_infl, na.rm=TRUE)) %>%
  mutate(multiplier2019 = total_infl/ total_raw)

#Ugh. Have to hand-code two years
infl_rates <- infl_rates %>%
  add_row(year = 2012, multiplier2019 = 114.969 / 100) %>%
  add_row(year = 2013, multiplier2019 = 114.969 / 103.279)

infl_rates %>%
  select(year, multiplier2019) %>%
  arrange(year)

#### Using the infl_rates data to create a new dataframe of all LHD expenditures, adjusted for inflation.
CompiledLHDExpenditures <-
  left_join(CompiledLHDExpenditures, infl_rates, by=c("year"="year")) %>%
  mutate(expenditures_infl = expenditures * multiplier2019,
         expenditures_infl_per_capita = expenditures_infl / population) %>%
  select(year, lhd_name, lhd_area_type, county_name, population, expenditures, expenditures_infl, expenditures_per_capita, expenditures_infl_per_capita)



CompiledLHDExpenditures <- CompiledLHDExpenditures %>%
  filter(year >=2010, year<=2018)

wide_CompiledLHDExpenditures <- CompiledLHDExpenditures %>%
  filter(year >=2010, year<=2018) %>%
  pivot_wider(id_cols = c(lhd_name,county_name, lhd_area_type), names_from = year, values_from = c(population, expenditures, expenditures_infl, expenditures_per_capita, expenditures_infl_per_capita)) %>%
filter(!is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010),
         !is.na(expenditures_2010)) %>%
  mutate(
    per_cap_rank_2010 = rank(expenditures_infl_per_capita_2010),
    per_cap_rank_2018 = rank(expenditures_infl_per_capita_2018)) %>%
  arrange(lhd_name)

yearmeans<-wide_CompiledLHDExpenditures %>% summarise_if(is.numeric, mean)

graphmeans<- yearmeans %>%
  select(starts_with("expenditures_infl_per_capita_")) %>%
  pivot_longer(cols = starts_with("expenditures_infl_per_capita_"),
               names_to = "year",
               names_prefix = "expenditures_infl_per_capita_",
               values_to = "avg_expenditures_infl_per_capita")

denominator <- nrow(wide_CompiledLHDExpenditures)

#setwd("./data/html_reports/LHD_reports")
