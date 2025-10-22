library(dplyr)
precip <- read.csv("initialData/precip.csv")
avgT <- read.csv("initialData/avgTemp.csv")
minT <- read.csv("initialData/minTemp.csv")

#I used chatGPT to generate this list
counties <- c(
  "Adams","Allen","Ashland","Ashtabula","Athens","Auglaize","Belmont","Brown",
  "Butler","Carroll","Champaign","Clark","Clermont","Clinton","Columbiana",
  "Coshocton","Crawford","Cuyahoga","Darke","Defiance","Delaware","Erie",
  "Fairfield","Fayette","Franklin","Fulton","Gallia","Geauga","Greene",
  "Guernsey","Hamilton","Hancock","Hardin","Harrison","Henry","Highland",
  "Hocking","Holmes","Huron","Jackson","Jefferson","Knox","Lake","Lawrence",
  "Licking","Logan","Lorain","Lucas","Madison","Mahoning","Marion","Medina",
  "Meigs","Mercer","Miami","Monroe","Montgomery","Morgan","Morrow","Muskingum",
  "Noble","Ottawa","Paulding","Perry","Pickaway","Pike","Portage","Preble",
  "Putnam","Richland","Ross","Sandusky","Scioto","Seneca","Shelby","Stark",
  "Summit","Trumbull","Tuscarawas","Union","Van Wert","Vinton","Warren",
  "Washington","Wayne","Williams","Wood","Wyandot"
)

precip <- precip %>%
  # change -99.9 for temps
  mutate(across(everything(), ~na_if(., -9.99))) %>%
  
  # this is getting the county and year from the id
  mutate(
    county_code = as.numeric(substr(ID, 3, 5)),
    year = as.numeric(substr(ID, nchar(ID)-3, nchar(ID))),
    county_index = ((county_code - 1) / 2) + 1,
    county = counties[county_index]
  ) %>%
    filter(year >= 2001 & year <= 2021) %>%
    select(-county_code, -county_index)

avgT <- avgT %>%
  # change -99.9 for temps
  mutate(across(everything(), ~na_if(., -99.9))) %>%
    mutate(
    county_code = as.numeric(substr(ID, 3, 5)),
    year = as.numeric(substr(ID, nchar(ID)-3, nchar(ID))),
    county_index = ((county_code - 1) / 2) + 1,
    county = counties[county_index]
  ) %>%
  filter(year >= 2001 & year <= 2021) %>%
  select(-county_code, -county_index)


minT <- minT %>%
  # change -99.9 for temps
  mutate(across(everything(), ~na_if(., -99.9))) %>%
  mutate(
    county_code = as.numeric(substr(ID, 3, 5)),
    year = as.numeric(substr(ID, nchar(ID)-3, nchar(ID))),
    county_index = ((county_code - 1) / 2) + 1,
    county = counties[county_index]
  ) %>%
  filter(year >= 2001 & year <= 2021) %>%
  select(-county_code, -county_index)





###--------------------Combine -------------------------------
  
library(tidyr)

avgT_long <- avgT %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "avgT"
  )

precip_long <- precip %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "precip"
  )

minT_long <- minT %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "month",
    values_to = "minT"
  )

combined <- precip_long %>%
  left_join(avgT_long %>% select(-ID),
            by = c("county", "year", "month")) %>%
  left_join(minT_long %>% select(-ID),
            by = c("county", "year", "month")) %>%
  rename(ID = ID)  # precipitation ID is kept as ID in case needed

combined <- combined %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(county, year, month)



combined <- combined %>%
  mutate(
    month = factor(month, levels = month.abb),
    frozen = if_else(minT < 32, 1, 0)
  )

#write.csv(combined, "weatherData.csv", row.names = FALSE)

