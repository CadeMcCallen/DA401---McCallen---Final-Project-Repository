#Columns: 
#year
#agDistrict
#county
#countyANSI
#cornPlanted
#cornHarvested
#cornYield
#cvCornPlanted
#cvCornHarvested
#cvCornYield
#cornPercentHarvest
#month
#precip
#avgT
#minT
#frozen
#month_num

#Set up for analysis
library(dplyr)
library(glmmTMB)
library(performance)
library(DHARMa)
df <- final %>%
  filter(missingWeather == FALSE) %>%
  select(-missingWeather, -ID, -agDistrictCode)

#For the beta
df <- df %>%
  mutate(
    cornPercentHarvest = if_else(
      cornPercentHarvest == 1, 0.9999,
      if_else(cornPercentHarvest == 0, 0.0001, cornPercentHarvest)
    )
  )





#Analysis -------------------------------------------------------------------------------------------------
model <- glmmTMB(
  cornPercentHarvest ~ precip *factor(month) + avgT*factor(month) + minT + (1 | county) + (1 | year),
  data = df,
  family = beta_family(link = "logit")
)

summary(model)

##precip *factor(month) + avgT + minT + (1 | county) + (1 | year)   -  Gave sig with febuary and higher precip levels
##precip *factor(month) + avgT*factor(month) + minT + (1 | county) + (1 | year) - Gave sig for higher precip in feb and july












