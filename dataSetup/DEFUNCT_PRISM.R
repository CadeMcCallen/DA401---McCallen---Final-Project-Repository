#Not work:
library(prism)
prism_set_dl_dir("~/PRISM3")
get_prism_monthlys(type = "ppt", years = 2020, mon = 1, keepZip = FALSE)

get_prism_monthlys(type = "ppt", years = years, mon = 1:12, keepZip = FALSE)





#Theory from chat GPT:W
library(prism)
library(sf)
library(exactextractr)
library(dplyr)
library(tigris)
library(raster)

prism_set_dl_dir("C:/Users/mccal/DA/DA401/PRISM3")

years <- 2001:2022

get_prism_monthlys(type = "ppt", years = years, mon = 1:12, keepZip = FALSE, re_run = TRUE)
get_prism_monthlys(type = "tmean", years = years, mon = 1:12, keepZip = FALSE)
get_prism_monthlys(type = "tmin", years = years, mon = 1:12, keepZip = FALSE)

ohio_counties <- counties(state = "OH", cb = TRUE, class = "sf") %>%
  st_transform(crs = 4326) # match PRISM projection (WGS84)

extract_prism <- function(var) {
  message("Processing variable: ", var)
    file_codes <- prism_archive_ls()[grepl(var, prism_archive_ls())]
    df_list <- lapply(file_codes, function(f) {
        raster_path <- prism_stack(f)
    r <- raster::raster(raster_path)
        if (nlayers(r) == 0) {
      warning("Could not load raster for file code: ", f, ". Skipping.")
      return(NULL)
    }
    
    d <- prism_date(f)
    vals <- exactextractr::exact_extract(r, ohio_counties, 'mean')
    
    data.frame(
      county = ohio_counties$NAME,
      date = d,
      value = vals
    )
  })
  
  df <- bind_rows(df_list[!sapply(df_list, is.null)])
  names(df)[3] <- var
  return(df)
}


ppt_df <- extract_prism("ppt")
tmean_df <- extract_prism("tmean")
tmin_df <- extract_prism("tmin")

message("Combining data frames...")
full_df <- ppt_df %>%
  left_join(tmean_df, by = c("county", "date")) %>%
  left_join(tmin_df, by = c("county", "date")) %>%
  mutate(
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  )

message("Saving final dataset...")
write.csv(full_df, "ohio_prism_monthly_2001_2022.csv", row.names = FALSE)

head(full_df)