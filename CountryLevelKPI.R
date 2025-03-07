# First Version: 27th January by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the data for KPI reporting at country-level
# Central switchboard gives you option of choosing which diseases to run


rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Set Year reference
start_year_min     = 2014                 # This cuts off the first year and removes years were HIVneg with the lag are wrong
start_year_base    = 2021                 # This year is the base year of reporting, in this case 2021
end_year_all       = 2023                 # This is the year of latest partner data
end_year_sdg       = 2030                 # This is the final year of prediction
end_year_gap       = 2028
start_year_hiv     = 2018
start_year_tb      = 2014
start_year_malaria = 2014
end_year_hiv       = 2023
end_year_tb        = 2019
end_year_malaria   = 2019
start_year_gp_hiv  = 2020
start_year_gp_tm   = 2015

# Install packages if neccesary
if(firstrun>0) {
  install.packages("dplyr")
  installed.packages("tidyverse")
}

library(dplyr) # require instead
library(data.table)   # For like function (%like%)
library(tidyr)

# Set computer, wd and load data
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/Data sources/")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/Investment Case 8th/KPIs/RCode"
  
  # Load the pip data 
  df_hiv2 = read.csv("df_partner_hiv_12Dec.csv", stringsAsFactors = FALSE)
  df_malaria2  = read.csv("df_partner_malaria_6Jan.csv", stringsAsFactors = FALSE)
  df_tb2      =  read.csv("df_partner_tb_12Dec.csv", stringsAsFactors = FALSE)
  
  # Order by country alphabetically
  df_hiv2     = df_hiv2[order(df_hiv2$ISO3),]
  df_tb2      = df_tb2[order(df_tb2$ISO3),]
  df_malaria2 = df_malaria2[order(df_malaria2$ISO3),]
  
  # Load GP # TODO: needs to be updated
  df_hiv_gp     = read.csv("hiv_gp_for_kpi.csv", stringsAsFactors = FALSE)
  df_tb_gp      = read.csv("tb_gp_for_kpi_v2.csv", stringsAsFactors = FALSE)
  df_malaria_gp = read.csv("malaria_gp_for_kpi.csv", stringsAsFactors = FALSE)
  
  # Order by country alphabetically
  df_hiv_gp     = df_hiv_gp[order(df_hiv_gp$country),]
  df_tb_gp      = df_tb_gp[order(df_tb_gp$ISO3),]
  df_malaria_gp = df_malaria_gp[order(df_malaria_gp$ISO3),]
  
  # Load list of countries
  df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
  df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
  df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)
}

# List of indicators
list_indc_hiv_pip     = c("ISO3", "Year", "hiv_cases_n_pip", "hiv_deaths_n_pip", "HIVpos_n_pip", "Population_n_pip")
list_indc_tb_pip      = c("ISO3", "Year", "tb_cases_n_pip", "tb_deaths_n_pip", "tb_pop_n_pip")
list_indc_malaria_pip = c("ISO3", "Year", "malaria_cases_n_pip", "malaria_deaths_n_pip", "malaria_par_n_pip")  
list_ind              = c("ISO3", "Year", "incidence", "mortality")

# data that needs to be removed because nto public for hiv - map countries and indicator
countries_to_na_aids_deaths <- c("CAF", "CMR", "GNQ", "KAZ", "MDV", "PRK", "STP", "UKR", "VEN")
countries_to_na_hiv_inc <- c("CAF", "CMR", "GNQ", "MDG", "MDV", "PAK", "PRK", "SOM", "STP", "UKR")


# Start the coding here: 
# Filter out countries we do not need: 
df_hiv2        = filter(df_hiv2, ISO3 %in% df_iso_hiv$ISO3)             # by eligible countries
df_tb2         = filter(df_tb2, ISO3 %in% df_iso_tb$ISO3)               # by eligible countries
df_malaria2    = filter(df_malaria2, ISO3 %in% df_iso_malaria$ISO3)     # by eligible countries

# Filter out indicators we do not need: 
df_hiv     = subset(df_hiv2, select = names(df_hiv2) %in% list_indc_hiv_pip)
df_tb      = subset(df_tb2, select = names(df_tb2) %in% list_indc_tb_pip)
df_malaria = subset(df_malaria2, select = names(df_malaria2) %in% list_indc_malaria_pip)

# Add HIVneg for hiv
df_hiv = df_hiv %>%
  mutate(
    HIVneg = lag(Population_n_pip)-lag(HIVpos_n_pip),
  )

# Compute incidence and mortality
df_hiv = df_hiv %>%
  mutate(
    incidence = hiv_cases_n_pip / HIVneg,
    mortality = hiv_deaths_n_pip / Population_n_pip,
  )

df_tb = df_tb %>%
  mutate(
    incidence = tb_cases_n_pip / tb_pop_n_pip,
    mortality = tb_deaths_n_pip / tb_pop_n_pip,
  )

df_malaria = df_malaria %>%
  mutate(
    incidence = malaria_cases_n_pip / malaria_par_n_pip,
    mortality = malaria_deaths_n_pip / malaria_par_n_pip,
  )

# Filter out years we do not need (which will also remove hivneg that are wrong in first year per country)
df_hiv      = df_hiv %>% filter(Year>=start_year_min)
df_tb       = df_tb %>% filter(Year>=start_year_min)
df_malaria  = df_malaria %>% filter(Year>=start_year_min)

# Rename variable names
df_hiv = rename(df_hiv, cases = hiv_cases_n_pip)
df_hiv = rename(df_hiv, deaths = hiv_deaths_n_pip)
df_tb = rename(df_tb, cases = tb_cases_n_pip)
df_tb = rename(df_tb, deaths = tb_deaths_n_pip)
df_malaria = rename(df_malaria, cases = malaria_cases_n_pip)
df_malaria = rename(df_malaria, deaths = malaria_deaths_n_pip)


# Section 1. Compute continuation of recent trends
# Extract an cases df
df_hiv_inc      = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "cases"))
df_tb_inc       = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "cases"))
df_malaria_inc  = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "cases"))

# Extract a deaths df
df_hiv_mort     = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "deaths"))
df_tb_mort      = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "deaths"))
df_malaria_mort = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "deaths"))

# Get latest value to update intercept later
df_hiv_inc_intercept      = df_hiv_inc %>% filter(Year==end_year_all)
df_tb_inc_intercept       = df_tb_inc %>% filter(Year==end_year_all)
df_malaria_inc_intercept  = df_malaria_inc %>% filter(Year==end_year_all)

df_hiv_mort_intercept     = df_hiv_mort %>% filter(Year==end_year_all)
df_tb_mort_intercept      = df_tb_mort %>% filter(Year==end_year_all)
df_malaria_mort_intercept = df_malaria_mort %>% filter(Year==end_year_all)

# Filter to the necessary years, for each disease
df_hiv_inc_trend     = df_hiv_inc %>% filter(Year>=start_year_hiv & Year<=end_year_hiv)
df_tb_inc_trend      = df_tb_inc %>% filter(Year>=start_year_tb & Year<=end_year_tb)
df_malaria_inc_trend = df_malaria_inc %>% filter(Year>=start_year_malaria & Year<=end_year_malaria)

df_hiv_mort_trend     = df_hiv_mort %>% filter(Year>=start_year_hiv & Year<=end_year_hiv)
df_tb_mort_trend      = df_tb_mort %>% filter(Year>=start_year_tb & Year<=end_year_tb)
df_malaria_mort_trend = df_malaria_mort %>% filter(Year>=start_year_malaria & Year<=end_year_malaria)

# Get average annual change
df_hiv_inc_trend = df_hiv_inc_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = cases-lag(cases),
    AAC = mean(annual_change, na.rm = TRUE),
  )

df_tb_inc_trend = df_tb_inc_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = cases-lag(cases),
    AAC = mean(annual_change, na.rm = TRUE),
  )

df_malaria_inc_trend = df_malaria_inc_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = cases-lag(cases),
    AAC = mean(annual_change, na.rm = TRUE),
  )

df_hiv_mort_trend = df_hiv_mort_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = deaths-lag(deaths),
    AAC = mean(annual_change, na.rm = TRUE),
  )

df_tb_mort_trend = df_tb_mort_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = deaths-lag(deaths),
    AAC = mean(annual_change, na.rm = TRUE),
  )

df_malaria_mort_trend = df_malaria_mort_trend %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = deaths-lag(deaths),
    AAC = mean(annual_change, na.rm = TRUE),
  )

# Filter for last year to keep average annual change
df_hiv_inc_AAC     = df_hiv_inc_trend %>% filter(Year==end_year_hiv)
df_tb_inc_AAC       = df_tb_inc_trend %>% filter(Year==end_year_tb)
df_malaria_inc_AAC  = df_malaria_inc_trend %>% filter(Year==end_year_malaria)

df_hiv_mort_AAC      = df_hiv_mort_trend %>% filter(Year==end_year_hiv)
df_tb_mort_AAC       = df_tb_mort_trend %>% filter(Year==end_year_tb)
df_malaria_mort_AAC  = df_malaria_mort_trend %>% filter(Year==end_year_malaria)

df_hiv_inc_AAC     = subset(df_hiv_inc_AAC, select = c(ISO3, AAC))
df_tb_inc_AAC      = subset(df_tb_inc_AAC, select = c(ISO3, AAC))
df_malaria_inc_AAC = subset(df_malaria_inc_AAC, select = c(ISO3, AAC))

df_hiv_mort_AAC     = subset(df_hiv_mort_AAC, select = c(ISO3, AAC))
df_tb_mort_AAC      = subset(df_tb_mort_AAC, select = c(ISO3, AAC))
df_malaria_mort_AAC = subset(df_malaria_mort_AAC, select = c(ISO3, AAC))


# Section 2. Fit the models
# Fit Linear and exponential models per disease and indicator, per country
# First HIV incidence
model_lm = df_hiv_inc_trend %>% group_by(ISO3) %>% do(model = lm(cases ~ Year, data = .))
coef_lm_hiv_inc <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_hiv_inc_trend %>% group_by(ISO3) %>% do(model = lm(log(cases+0.001) ~ Year, data = .))
coef_exp_hiv_inc <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_hiv_inc = df_hiv_inc_intercept %>% left_join(df_hiv_inc_AAC, by='ISO3') %>% 
  left_join(coef_lm_hiv_inc, by='ISO3') %>% 
  left_join(coef_exp_hiv_inc, by='ISO3')

# Update intercept
coef_hiv_inc = coef_hiv_inc %>%
  mutate(
    ISO3 = ISO3,
    intercept_lm  = cases - (slope_lm*Year),
    intercept_exp = log(cases) - (slope_exp*Year)
  )
coef_hiv_inc = subset(coef_hiv_inc, select = -c(Year))

# Then HIV mortality
model_lm = df_hiv_mort_trend %>% group_by(ISO3) %>% do(model = lm(deaths ~ Year, data = .))
coef_lm_hiv_mort <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_hiv_mort_trend %>% group_by(ISO3) %>% do(model = lm(log(deaths+0.001) ~ Year, data = .))
coef_exp_hiv_mort <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_hiv_mort = df_hiv_mort_intercept %>% left_join(df_hiv_mort_AAC, by='ISO3') %>% 
  left_join(coef_lm_hiv_mort, by='ISO3') %>% 
  left_join(coef_exp_hiv_mort, by='ISO3')

# Update intercept
coef_hiv_mort = coef_hiv_mort %>%
  mutate(
    ISO3 = ISO3,
    intercept_lm  = deaths - (slope_lm*Year),
    intercept_exp = log(deaths) - (slope_exp*Year)
  )
coef_hiv_mort = subset(coef_hiv_mort, select = -c(Year))

# First TB incidence
model_lm = df_tb_inc_trend %>% group_by(ISO3) %>% do(model = lm(cases ~ Year, data = .))
coef_lm_tb_inc <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_tb_inc_trend %>% group_by(ISO3) %>% do(model = lm(log(cases+0.001) ~ Year, data = .))
coef_exp_tb_inc <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_tb_inc = df_tb_inc_intercept %>% left_join(df_tb_inc_AAC, by='ISO3') %>% 
  left_join(coef_lm_tb_inc, by='ISO3') %>% 
  left_join(coef_exp_tb_inc, by='ISO3')

# Update intercept
coef_tb_inc = coef_tb_inc %>%
  mutate(
    ISO3 = ISO3,
    intercept_lm  = cases - (slope_lm*Year),
    intercept_exp = log(cases) - (slope_exp*Year)
  )
coef_tb_inc = subset(coef_tb_inc, select = -c(Year))

# Then TB mortality
df_tb_mort_trend     = df_tb_mort_trend %>% filter(ISO3!='PRK') # HARD coding
model_lm = df_tb_mort_trend %>% group_by(ISO3) %>% do(model = lm(deaths ~ Year, data = .))
coef_lm_tb_mort <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_tb_mort_trend %>% group_by(ISO3) %>% do(model = lm(log(deaths+0.001) ~ Year, data = .))
coef_exp_tb_mort <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_tb_mort = df_tb_mort_intercept %>% left_join(df_tb_mort_AAC, by='ISO3') %>% 
  left_join(coef_lm_tb_mort, by='ISO3') %>% 
  left_join(coef_exp_tb_mort, by='ISO3')

# Update intercept
coef_tb_mort = coef_tb_mort %>%
  mutate(
    ISO3 = ISO3,
    intercept_lm  = deaths - (slope_lm*Year),
    intercept_exp = log(deaths) - (slope_exp*Year)
  )
coef_tb_mort = subset(coef_tb_mort, select = -c(Year))

# First malaria incidence
model_lm = df_malaria_inc_trend %>% group_by(ISO3) %>% do(model = lm(cases ~ Year, data = .))
coef_lm_malaria_inc <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_malaria_inc_trend %>% group_by(ISO3) %>% do(model = lm(log(cases+0.001) ~ Year, data = .))
coef_exp_malaria_inc <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_malaria_inc = df_malaria_inc_intercept %>% left_join(df_malaria_inc_AAC, by='ISO3') %>% 
  left_join(coef_lm_malaria_inc, by='ISO3') %>% 
  left_join(coef_exp_malaria_inc, by='ISO3')

# Update intercept
coef_malaria_inc = coef_malaria_inc %>%
  mutate(
    ISO3 = ISO3,
    intercept_lm  = cases - (slope_lm*Year),
    intercept_exp = log(cases) - (slope_exp*Year)
  )
coef_malaria_inc = subset(coef_malaria_inc, select = -c(Year))

# Then malaria mortality
model_lm = df_malaria_mort_trend %>% group_by(ISO3) %>% do(model = lm(deaths ~ Year, data = .))
coef_lm_malaria_mort <- model_lm %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_lm  = coef(model)[1],
    slope_lm      = coef(model)[2]
  )

model_exp = df_malaria_mort_trend %>% group_by(ISO3) %>% do(model = lm(log(deaths+0.001) ~ Year, data = .))
coef_exp_malaria_mort <- model_exp %>%     # Get the key coefficients
  summarise(
    ISO3 = ISO3,
    intercept_exp = coef(model)[1],
    slope_exp     = coef(model)[2]
  )

# Bind 2023 information with model data
coef_malaria_mort = df_malaria_mort_intercept %>% left_join(df_malaria_mort_AAC, by='ISO3') %>% 
  left_join(coef_lm_malaria_mort, by='ISO3') %>% 
  left_join(coef_exp_malaria_mort, by='ISO3')

# Update intercept
coef_malaria_mort = coef_malaria_mort %>%
  mutate(
    intercept_lm  = deaths - (slope_lm*Year),
    intercept_exp = log(deaths) - (slope_exp*Year)
  )
coef_malaria_mort = subset(coef_malaria_mort, select = -c(Year))



# Section 3. Make the projections
# Make a new empty dataframe for every country and prospective year, hen merge in slope and intercept and compute future incidence based on linear
# First HIV incidence
years = c(end_year_all:end_year_sdg)
hiv_inc_proj = expand.grid(ISO3=df_iso_hiv$ISO3, Year=years)
hiv_inc_proj = hiv_inc_proj[order(hiv_inc_proj$ISO3),]
hiv_inc_proj = merge(hiv_inc_proj, coef_hiv_inc, by = "ISO3", all.y=TRUE)
hiv_inc_proj = hiv_inc_proj %>%
  mutate(
    cases = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
      ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
hiv_inc_proj = subset(hiv_inc_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
hiv_inc_proj = hiv_inc_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_hiv_inc = rbind(df_hiv_inc, hiv_inc_proj)
df_hiv_inc = df_hiv_inc[order(df_hiv_inc$ISO3),]

# Then HIV mortality
years = c(end_year_all:end_year_sdg)
hiv_mort_proj = expand.grid(ISO3=df_iso_hiv$ISO3, Year=years)
hiv_mort_proj = hiv_mort_proj[order(hiv_mort_proj$ISO3),]
hiv_mort_proj = merge(hiv_mort_proj, coef_hiv_mort, by = "ISO3", all.y=TRUE)
hiv_mort_proj = hiv_mort_proj %>%
  mutate(
    deaths = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
             ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
hiv_mort_proj = subset(hiv_mort_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
hiv_mort_proj = hiv_mort_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_hiv_mort = rbind(df_hiv_mort, hiv_mort_proj)
df_hiv_mort = df_hiv_mort[order(df_hiv_mort$ISO3),]

# First TB incidence
years = c(end_year_all:end_year_sdg)
tb_inc_proj = expand.grid(ISO3=df_iso_tb$ISO3, Year=years)
tb_inc_proj = tb_inc_proj[order(tb_inc_proj$ISO3),]
tb_inc_proj = merge(tb_inc_proj, coef_tb_inc, by = "ISO3", all.y=TRUE)
tb_inc_proj = tb_inc_proj %>%
  mutate(
    cases = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
             ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
tb_inc_proj = subset(tb_inc_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
tb_inc_proj = tb_inc_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_tb_inc = rbind(df_tb_inc, tb_inc_proj)
df_tb_inc = df_tb_inc[order(df_tb_inc$ISO3),]

# Then TB mortality
years = c(end_year_all:end_year_sdg)
tb_mort_proj = expand.grid(ISO3=df_iso_tb$ISO3, Year=years)
tb_mort_proj = tb_mort_proj[order(tb_mort_proj$ISO3),]
tb_mort_proj = merge(tb_mort_proj, coef_tb_mort, by = "ISO3", all.y=TRUE)
tb_mort_proj = tb_mort_proj %>%
  mutate(
    deaths = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
             ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
tb_mort_proj = subset(tb_mort_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
tb_mort_proj = tb_mort_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_tb_mort = rbind(df_tb_mort, tb_mort_proj)
df_tb_mort = df_tb_mort[order(df_tb_mort$ISO3),]

# First malaria incidence
years = c(end_year_all:end_year_sdg)
malaria_inc_proj = expand.grid(ISO3=df_iso_malaria$ISO3, Year=years)
malaria_inc_proj = malaria_inc_proj[order(malaria_inc_proj$ISO3),]
malaria_inc_proj = merge(malaria_inc_proj, coef_malaria_inc, by = "ISO3", all.y=TRUE)
malaria_inc_proj = malaria_inc_proj %>%
  mutate(
    cases = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
             ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
malaria_inc_proj = subset(malaria_inc_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
malaria_inc_proj = malaria_inc_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_malaria_inc = rbind(df_malaria_inc, malaria_inc_proj)
df_malaria_inc = df_malaria_inc[order(df_malaria_inc$ISO3),]

# Then malaria mortality
years = c(end_year_all:end_year_sdg)
malaria_mort_proj = expand.grid(ISO3=df_iso_malaria$ISO3, Year=years)
malaria_mort_proj = malaria_mort_proj[order(malaria_mort_proj$ISO3),]
malaria_mort_proj = merge(malaria_mort_proj, coef_malaria_mort, by = "ISO3", all.y=TRUE)
malaria_mort_proj = malaria_mort_proj %>%
  mutate(
    deaths = 
      ifelse(slope_exp<0, exp(slope_exp*Year + intercept_exp),
             ifelse(slope_exp>0, (slope_lm*Year) + intercept_lm, NA)))
malaria_mort_proj = subset(malaria_mort_proj, select = -c(intercept_lm, intercept_exp, slope_lm, slope_exp, AAC))
malaria_mort_proj = malaria_mort_proj %>% filter(Year>end_year_all)

# Merge historic and projections
df_malaria_mort = rbind(df_malaria_mort, malaria_mort_proj)
df_malaria_mort = df_malaria_mort[order(df_malaria_mort$ISO3),]




# Section 4. Clean up the cases and deaths CRT
# Filter for the years to be compared
df_hiv_inc_final   = df_hiv_inc %>% filter(Year==end_year_all | Year==end_year_gap)
df_hiv_mort_final  = df_hiv_mort %>% filter(Year==end_year_all | Year==end_year_gap)

df_tb_inc_final   = df_tb_inc %>% filter(Year==end_year_all | Year==end_year_gap)
df_tb_mort_final  = df_tb_mort %>% filter(Year==end_year_all | Year==end_year_gap)

df_malaria_inc_final   = df_malaria_inc %>% filter(Year==end_year_all | Year==end_year_gap)
df_malaria_mort_final  = df_malaria_mort %>% filter(Year==end_year_all | Year==end_year_gap)

# Pivot to get into format
df_hiv_inc_final = df_hiv_inc_final %>%
  pivot_wider(names_from = Year, values_from = c(cases))
colnames(df_hiv_inc_final) <- c('ISO3','Number_of_cases_new_infections_latest','Number_of_cases_new_infections_gap_year')

df_hiv_mort_final = df_hiv_mort_final %>%
  pivot_wider(names_from = Year, values_from = c(deaths))
colnames(df_hiv_mort_final) <- c('ISO3','Number_of_deaths_latest','Number_of_deaths_gap_year')

df_tb_inc_final = df_tb_inc_final %>%
  pivot_wider(names_from = Year, values_from = c(cases))
colnames(df_tb_inc_final) <- c('ISO3','Number_of_cases_new_infections_latest','Number_of_cases_new_infections_gap_year')

df_tb_mort_final = df_tb_mort_final %>%
  pivot_wider(names_from = Year, values_from = c(deaths))
colnames(df_tb_mort_final) <- c('ISO3','Number_of_deaths_latest','Number_of_deaths_gap_year')

df_malaria_inc_final = df_malaria_inc_final %>%
  pivot_wider(names_from = Year, values_from = c(cases))
colnames(df_malaria_inc_final) <- c('ISO3','Number_of_cases_new_infections_latest','Number_of_cases_new_infections_gap_year')

df_malaria_mort_final = df_malaria_mort_final %>%
  pivot_wider(names_from = Year, values_from = c(deaths))
colnames(df_malaria_mort_final) <- c('ISO3','Number_of_deaths_latest','Number_of_deaths_gap_year')



# Section 5. Now do 2021 to 2023 rate difference
df_hiv_rate_red      = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "incidence", "mortality"))
df_tb_rate_red       = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "incidence", "mortality"))
df_malaria_rate_red  = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "incidence", "mortality"))

# Filter for the years to be compared
df_hiv_rate_red     = df_hiv_rate_red %>% filter(Year==end_year_all | Year==start_year_base)
df_tb_rate_red      = df_tb_rate_red %>% filter(Year==end_year_all | Year==start_year_base)
df_malaria_rate_red = df_malaria_rate_red %>% filter(Year==end_year_all | Year==start_year_base)

# Compute reduction, first pivot
df_hiv_rate_red = df_hiv_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

df_hiv_rate_red = df_hiv_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_2023 - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_2023 - mortality_2021) / mortality_2021,
  )
df_hiv_rate_red = subset(df_hiv_rate_red, select = -c(incidence_2021, mortality_2021))

df_tb_rate_red = df_tb_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

df_tb_rate_red = df_tb_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_2023 - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_2023 - mortality_2021) / mortality_2021,
  )
df_tb_rate_red = subset(df_tb_rate_red, select = -c(incidence_2021, mortality_2021))

df_malaria_rate_red = df_malaria_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

df_malaria_rate_red = df_malaria_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_2023 - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_2023 - mortality_2021) / mortality_2021,
  )
df_malaria_rate_red = subset(df_malaria_rate_red, select = -c(incidence_2021, mortality_2021))

df_malaria_rate_red[df_malaria_rate_red == Inf | df_malaria_rate_red == -Inf] <- 0


# Section 6. Now get GP
# Clean GP df
df_hiv_gp     = subset(df_hiv_gp, select = -c(incidence, mortality))
df_tb_gp      = subset(df_tb_gp, select = c(ISO3, Year, Cases, Deaths))
df_malaria_gp = subset(df_malaria_gp, select = -c(country, incidence, mortality))

df_hiv_gp = rename(df_hiv_gp, ISO3 = country)
df_hiv_gp = rename(df_hiv_gp, Year = year)
df_hiv_gp = rename(df_hiv_gp, cases_sdg = cases)
df_hiv_gp = rename(df_hiv_gp, deaths_sdg = deaths)

df_tb_gp = rename(df_tb_gp, cases_sdg = Cases)
df_tb_gp = rename(df_tb_gp, deaths_sdg = Deaths)

df_malaria_gp = rename(df_malaria_gp, Year = year)
df_malaria_gp = rename(df_malaria_gp, cases_sdg = cases)
df_malaria_gp = rename(df_malaria_gp, deaths_sdg = deaths)

# Make clean subset of pip data
df_hiv_baseline      = df_hiv%>% filter(Year==start_year_gp_hiv)
df_hiv_baseline      = subset(df_hiv_baseline, select = -c(Year, HIVpos_n_pip, Population_n_pip, HIVneg, incidence, mortality))

df_tb_baseline       = df_tb%>% filter(Year==start_year_gp_tm)
df_tb_baseline       = subset(df_tb_baseline, select = -c(Year, tb_pop_n_pip, incidence, mortality))

df_malaria_baseline  = df_malaria%>% filter(Year==start_year_gp_tm)
df_malaria_baseline  = subset(df_malaria_baseline, select = -c(Year, malaria_par_n_pip, incidence, mortality))

# Compute ratio at baseline
df_hiv_gp_baseline = df_hiv_gp %>% filter(Year==start_year_gp_hiv)
df_hiv_gp_baseline = df_hiv_gp_baseline %>% left_join(df_hiv_baseline, by='ISO3')
df_hiv_gp_baseline = df_hiv_gp_baseline %>%
  mutate(
    ratio_cases = (cases/cases_sdg),
    ratio_deaths = (deaths/deaths_sdg),
  )
df_hiv_gp_baseline = subset(df_hiv_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths))

df_tb_gp_baseline = df_tb_gp %>% filter(Year==start_year_gp_tm)
df_tb_gp_baseline = df_tb_gp_baseline %>% left_join(df_tb_baseline, by='ISO3')
df_tb_gp_baseline = df_tb_gp_baseline %>%
  mutate(
    ratio_cases = (cases/cases_sdg),
    ratio_deaths = (deaths/deaths_sdg),
  )
df_tb_gp_baseline = subset(df_tb_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths))

df_malaria_gp_baseline = df_malaria_gp %>% filter(Year==start_year_gp_tm)
df_malaria_gp_baseline = df_malaria_gp_baseline %>% left_join(df_malaria_baseline, by='ISO3')
df_malaria_gp_baseline = df_malaria_gp_baseline %>%
  mutate(
    ratio_cases = (cases/cases_sdg),
    ratio_deaths = (deaths/deaths_sdg),
  )
df_malaria_gp_baseline = subset(df_malaria_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths))

# Merge pip and gp data
df_hiv_gp = df_hiv_gp %>% left_join(df_hiv_gp_baseline, by='ISO3')
df_hiv_gp = df_hiv_gp %>%
  mutate(
    cases_sdg = (cases_sdg *ratio_cases),
    deaths_sdg = (deaths_sdg*ratio_deaths),
  )

df_tb_gp = df_tb_gp %>% left_join(df_tb_gp_baseline, by='ISO3')
df_tb_gp = df_tb_gp %>%
  mutate(
    cases_sdg = (cases_sdg *ratio_cases),
    deaths_sdg = (deaths_sdg*ratio_deaths),
  )

df_malaria_gp = df_malaria_gp %>% left_join(df_malaria_gp_baseline, by='ISO3')
df_malaria_gp = df_malaria_gp %>%
  mutate(
    cases_sdg = (cases_sdg *ratio_cases),
    deaths_sdg = (deaths_sdg*ratio_deaths),
  )

# Filter for the years to be compared
df_hiv_gp     = df_hiv_gp %>% filter(Year==end_year_gap)
df_tb_gp      = df_tb_gp %>% filter(Year==end_year_gap)
df_malaria_gp = df_malaria_gp %>% filter(Year==end_year_gap)

#Keep only data we need
df_hiv_gp     = subset(df_hiv_gp, select = -c(Year, ratio_cases, ratio_deaths))
df_tb_gp      = subset(df_tb_gp, select = -c(Year, ratio_cases, ratio_deaths))
df_malaria_gp = subset(df_malaria_gp, select = -c(Year, ratio_cases, ratio_deaths))




#Section 7. Combine everything by disease
df_hiv_final = df_hiv_inc_final %>% left_join(df_hiv_mort_final, by='ISO3') %>% 
              left_join(df_hiv_gp, by='ISO3') %>% 
                left_join(df_hiv_rate_red, by='ISO3')

df_tb_final = df_tb_inc_final %>% left_join(df_tb_mort_final, by='ISO3') %>% 
  left_join(df_tb_gp, by='ISO3') %>% 
  left_join(df_tb_rate_red, by='ISO3')

df_malaria_final = df_malaria_inc_final %>% left_join(df_malaria_mort_final, by='ISO3') %>% 
  left_join(df_malaria_gp, by='ISO3') %>% 
  left_join(df_malaria_rate_red, by='ISO3')

# Make the gap
df_hiv_final = df_hiv_final %>%
  mutate(
    Gap_in_nr_cases = (Number_of_cases_new_infections_gap_year - cases_sdg),
    Gap_in_nr_deaths = (Number_of_deaths_gap_year - deaths_sdg),
  )

df_tb_final = df_tb_final %>%
  mutate(
    Gap_in_nr_cases = (Number_of_cases_new_infections_gap_year - cases_sdg),
    Gap_in_nr_deaths = (Number_of_deaths_gap_year - deaths_sdg),
  )

df_malaria_final = df_malaria_final %>%
  mutate(
    Gap_in_nr_cases = (Number_of_cases_new_infections_gap_year - cases_sdg),
    Gap_in_nr_deaths = (Number_of_deaths_gap_year - deaths_sdg),
  )

# Compute % share of the gap
df_hiv_final = df_hiv_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

df_tb_final = df_tb_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

df_malaria_final = df_malaria_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

# Make a clean df for each indicator
df_kpi_I2_hiv     = subset(df_hiv_final, select = names(df_hiv_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_2023", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_hiv <- df_kpi_I2_hiv %>%
  mutate(
    'Component'      = "HIV/AIDS",
  )

# remove some data that is not public
df_kpi_I2_hiv$incidence_2023[df_kpi_I2_hiv$ISO3 %in% countries_to_na_hiv_inc] <- NA

df_kpi_I2_tb      = subset(df_tb_final, select = names(df_tb_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_2023", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_tb <- df_kpi_I2_tb %>%
  mutate(
    'Component'      = "TB",
  )

df_kpi_I2_malaria = subset(df_malaria_final, select = names(df_malaria_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_2023", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_malaria <- df_kpi_I2_malaria %>%
  mutate(
    'Component'      = "Malaria",
  )

df_kpi_I2 = rbind(df_kpi_I2_hiv, df_kpi_I2_tb, df_kpi_I2_malaria)


df_kpi_I2 <- df_kpi_I2 %>%
  mutate(
    'KPI code'       = "KPI I2",
    'Reporting year' = 2025,
    "Reporting half" = "H1",
    "Data period"    = 2023,
    ) %>%
  select("KPI code", "Reporting year", "Reporting half", "Data period", 
         "ISO3", "Component", "Number_of_cases_new_infections_latest", "incidence_2023", "cases_share", "Change_in_incidence_rate")

df_kpi_I2 <- df_kpi_I2 %>%
  rename(
    "ISO" = "ISO3",
    "Number of cases/infections, latest" = "Number_of_cases_new_infections_latest",
    "Incidence rate" = 'incidence_2023',
    "share of gap to SDG" = "cases_share",
    "variation incidence rate" = "Change_in_incidence_rate"
  )

df_kpi_I1_hiv     = subset(df_hiv_final, select = names(df_hiv_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_2023", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_hiv <- df_kpi_I1_hiv %>%
  mutate(
    'Component'      = "HIV/AIDS",
  )
# remove some data that is not public
df_kpi_I1_hiv$Number_of_deaths_latest[df_kpi_I1_hiv$ISO3 %in% countries_to_na_aids_deaths] <- NA

df_kpi_I1_tb      = subset(df_tb_final, select = names(df_tb_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_2023", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_tb <- df_kpi_I1_tb %>%
  mutate(
    'Component'      = "TB",
  )

df_kpi_I1_malaria = subset(df_malaria_final, select = names(df_malaria_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_2023", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_malaria <- df_kpi_I1_malaria %>%
  mutate(
    'Component'      = "Malaria",
  )

df_kpi_I1 = rbind(df_kpi_I1_hiv, df_kpi_I1_tb, df_kpi_I1_malaria)

df_kpi_I1 <- df_kpi_I1 %>%
  mutate(
    'KPI code'       = "KPI I1",
    'Reporting year' = 2025,
    "Reporting half" = "H1",
    "Data period"    = 2023,
  ) %>%
  select("KPI code", "Reporting year", "Reporting half", "Data period", 
         "ISO3", "Component", "Number_of_deaths_latest", "mortality_2023", "deaths_share", "Change_in_mortality_rate")

df_kpi_I1 <- df_kpi_I1 %>%
  rename(
    "ISO" = "ISO3",
    "Number of deaths, latest" = "Number_of_deaths_latest",
    "Mortality rate" = 'mortality_2023',
    "share of gap to SDG" = "deaths_share",
    "variation mortality rate" = "Change_in_mortality_rate"
  )



# Save the information relating to continuation of recent trends
write.csv(coef_hiv_inc,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_hiv_inc.csv", row.names=FALSE)
write.csv(coef_hiv_mort,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_hiv_mort.csv", row.names=FALSE)

write.csv(coef_tb_inc,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_tb_inc.csv", row.names=FALSE)
write.csv(coef_tb_mort,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_tb_mort.csv", row.names=FALSE)

write.csv(coef_malaria_inc,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_malaria_inc.csv", row.names=FALSE)
write.csv(coef_malaria_mort,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/coef_malaria_mort.csv", row.names=FALSE)

# Save the final output
write.csv(df_hiv_final,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/HIV_final_data.csv", row.names=FALSE)
write.csv(df_tb_final,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/TB_final_data.csv", row.names=FALSE)
write.csv(df_malaria_final,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/Malaria_final_data.csv", row.names=FALSE)

# Save the final output
write.csv(df_kpi_I1,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/df_kpi_I1.csv", na = "", row.names=FALSE)
write.csv(df_kpi_I2,file="/Users/mc1405/Dropbox/The Global Fund/KPI reporting 2021-2028/ROutput/df_kpi_I2.csv", na = "", row.names=FALSE)


