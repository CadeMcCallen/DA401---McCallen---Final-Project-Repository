#Set up for analysis
library(dplyr)
library(glmmTMB)
library(performance)
library(DHARMa)
library(tidyr)
library(nlme)

df <- final %>%
  filter(missingWeather == FALSE) %>%
  select(-missingWeather, -ID, -agDistrictCode)

### Linear Mixed Effect --- Doesn't work because there isn't variation between crop yield in months I think IGNORE:

df <- df %>%
  mutate(
    cornYield = as.numeric(str_replace_all(cornYield, ",", "")) #ADD TO ORIGINAL DATA FRAME!!!
  )

# df_model <- df %>%
#   mutate(
#     month_num = case_when(
#       month == "Jan" ~ 1,
#       month == "Feb" ~ 2,
#       month == "Mar" ~ 3,
#       month == "Apr" ~ 4,
#       month == "May" ~ 5,
#       month == "Jun" ~ 6,
#       month == "Jul" ~ 7,
#       month == "Aug" ~ 8,
#       month == "Sep" ~ 9,
#       month == "Oct" ~ 10,
#       month == "Nov" ~ 11,
#       month == "Dec" ~ 12,
#       TRUE ~ NA_real_
#     )
#   ) %>%
#   filter(!is.na(cornYield), !is.na(precip), !is.na(avgT), !is.na(minT), !is.na(frozen))


#For final weather csv with above
#write.csv(df_model, "weather_cornyield_final.csv", row.names = FALSE)










df_model <- read.csv("data/weather_cornyield_final.csv")

model_yield <- lme(
  cornYield ~ precip + avgT,            
  random = ~ 1 | county,                           # random intercept for county (idea to nest in year, didn't work when done this way)
  correlation = corAR1(form = ~ month_num | county/year),  # AR(1) within county-year
  data = df_model,
  method = "REML"
)

summary(model_yield)



### Linear Mixed Effect --- Attempt 2  ---- AvgT and Precip
df_wide <- df_model %>%
  select(year, county, cornYield, month, month_num, precip, avgT, minT, frozen) %>%
  pivot_wider(
    id_cols = c(year, county, cornYield), names_from = month_num,               
    values_from = c(precip, avgT, minT, frozen) 
  ) %>%
  distinct()

#This gets the columns in one variable since there are 24 of them in in the model
monthly_vars <- grep("precip_|avgT_", names(df_wide), value = TRUE)

#This just makes it into  formula
fixed_formula <- as.formula(paste("cornYield ~", paste(monthly_vars, collapse = " + ")))
# print(fixed_formula)


df_wide <- df_wide %>%
  mutate(across(starts_with(c("precip_", "avgT_", "minT_")), scale))


model_yield_final <- lme(
  fixed_formula,                
  random = ~ 1 | county,        
  data = df_wide,             
  method = "REML"
)

summary(model_yield_final)


library(broom.mixed)

#I used chat GPT to help with this visualization since I could not figure out the broom.mixed package
fixed_effects_df <- broom.mixed::tidy(model_yield_final) %>%
  filter(effect == "fixed", term != "(Intercept)") %>% 
  mutate(
    # Separate variable type (precip, avgT, etc.) and month number
    variable = gsub("_\\d+", "", term),
    month_num = as.numeric(gsub(".*_", "", term)),
    # Use month abbreviations for a cleaner plot x-axis
    month = month.abb[month_num],
    # Calculate 95% Confidence Interval using the calculated std.error
    CI_lower = estimate - 1.96 * std.error,
    CI_upper = estimate + 1.96 * std.error
  )

library(ggplot2)
#PLOT 1
ggplot(fixed_effects_df, aes(x = month, y = estimate, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = CI_lower, ymax = CI_upper),
                  position = position_dodge(width = 0.5), size = 1.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of Monthly Weather Variables on Annual Corn Yield",
    x = "Month",
    y = "Change in Corn Yield (bu/acre) per unit increase in variable"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

##Attempt to make significance marked (not working)
fixed_effects_df <- fixed_effects_df %>%
  mutate(month = factor(month, levels = month_order)) %>%
  mutate(
    is_significant = case_when(
      (CI_lower > 0 & CI_upper > 0) ~ TRUE,
      (CI_lower < 0 & CI_upper < 0) ~ TRUE,
      TRUE ~ FALSE
    )
  )

ggplot(fixed_effects_df, 
       aes(x = month, y = estimate, color = variable)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(data = fixed_effects_df, size = 4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(
    title = "Monthly Weather Effects on Corn Yield",
    x = "Month",
    y = "Impact on Yield (Unit increase on BU/Acre"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  ) 

monthly_vars_2 <- grep("precip_|frozen_", names(df_wide), value = TRUE)
fixed_formula_2 <- as.formula(paste("cornYield ~", paste(monthly_vars_2, collapse = " + ")))
# print(fixed_formula)


model_yield_final_frozen <- lme(
  fixed_formula_2,                
  random = ~ 1 | county,        # Random intercept for county
  data = df_wide,             
  method = "REML"
)

summary(model_yield_final)


###--------------------------------------------------LMEM with frozen
library(lme4)

#used gemini to type these out
model_final_frozen <- lmer(
  cornYield ~ 
    precip_1 + precip_2 + precip_3 + precip_4 + precip_5 + precip_6 +
    precip_7 + precip_8 + precip_9 + precip_10 + precip_11 + precip_12 +
    frozen_1 + frozen_2 + frozen_3 + frozen_4 + frozen_5 + frozen_6 +
    frozen_7 + frozen_8 + frozen_9 + frozen_10 + frozen_11 + frozen_12 +
    (1 | county),
  data = df_wide
)
summary(model_final_frozen)

fixed_effects_df <- tidy(model_final_frozen, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    variable = ifelse(grepl("precip", term), "Precipitation", "Frost/Freeze"),
    month_num = as.integer(gsub(".*_([0-9]+)$", "\\1", term))
  ) %>%
  filter(!is.na(estimate)) 

month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

fixed_effects_df <- fixed_effects_df %>%
  mutate(month = factor(month_names[month_num], levels = month_names))

ggplot(fixed_effects_df, aes(x = month, y = estimate, color = variable)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.1, linewidth = 1) + 
  geom_point(size = 3) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) + theme_bw() + 
  labs(title = "Monthly Weather Effects on Corn Yield (Fixed Effects)",
    x = "Month",
    y = "Impact on Yield (Estimate on BU/Acre)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )


