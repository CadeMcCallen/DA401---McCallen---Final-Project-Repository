library(dplyr)
ag <- read.csv("initialData/USDA_AcresPlanted_Harvested_YieldBU.csv")
##ag <- ag %>% select(-Program, -Period, -Week.Ending, -Geo.Level, -State, -State.ANSI, -Zip.Code, -Region, -watershed_code, -Watershed, -Commodity, -Domain, -Domain.Category)

library(tidyr)

ag <- read.csv("initialData/USDA_AcresPlanted_Harvested_YieldBU.csv")
ag_wide <- ag %>%
  select(Year, Ag.District, Ag.District.Code, County, County.ANSI, 
         Data.Item, Value, CV....) %>%
  pivot_wider(
    names_from = Data.Item, #changed
    values_from = c(Value, `CV....`),
    names_sep = "_"
  )

library(dplyr)

#I used chatgpt to help with renaming
ag_clean <- ag_wide %>%
  rename(
    AgDistrict = Ag.District,
    AgDistrictCode = Ag.District.Code,
    CountyANSI = County.ANSI
  ) %>%
    select(-matches("SILAGE")) %>%
    rename_with(~case_when(
    .x == "Value_CORN - ACRES PLANTED" ~ "corn_planted",
    .x == "Value_CORN, GRAIN - ACRES HARVESTED" ~ "corn_harvested",
    .x == "Value_CORN, GRAIN - YIELD, MEASURED IN BU / ACRE" ~ "corn_yield",
    .x == "CV...._CORN - ACRES PLANTED" ~ "cv_corn_planted",
    .x == "CV...._CORN, GRAIN - ACRES HARVESTED" ~ "cv_corn_harvested",
    .x == "CV...._CORN, GRAIN - YIELD, MEASURED IN BU / ACRE" ~ "cv_corn_yield",
    TRUE ~ .x
  ))



#new variable and numeric columns
ag_clean <- ag_clean %>%
  mutate(
    corn_planted = as.numeric(gsub(",", "", corn_planted)),
    corn_harvested = as.numeric(gsub(",", "", corn_harvested)),
    corn_percent_harvest = if_else(
      !is.na(corn_planted) & corn_planted > 0,
      (corn_harvested / corn_planted) * 100,
      NA_real_
    )
  )

ag_clean <- ag_clean %>%
  mutate(
    corn_percent_harvest = (corn_harvested / corn_planted)
  )

#Final column names (chatgpt assist)
ag_clean <- ag_clean %>%
  rename(
    year = Year,
    agDistrict = AgDistrict,
    agDistrictCode = AgDistrictCode,
    county = County,
    countyANSI = CountyANSI,
    cornPlanted = corn_planted,
    cornHarvested = corn_harvested,
    cornYield = corn_yield,
    cvCornPlanted = cv_corn_planted,
    cvCornHarvested = cv_corn_harvested,
    cvCornYield = cv_corn_yield,
    cornPercentHarvest = corn_percent_harvest
  )

ag_clean$county[ag_clean$county == "OTHER (COMBINED) COUNTIES"] <- "OTHER COUNTIES"



#write.csv(ag_clean, "agData1.csv", row.names = FALSE)




