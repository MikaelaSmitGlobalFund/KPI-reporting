# First Version: 27th January by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the data for KPI reporting at country-level
# Central switchboard gives you option of choosing which diseases to run


rm(list = ls())

#######################
# Central Switchboard #
#######################

# User
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Set Year reference
start_year_min     = 2014                 # This cuts off the first year and removes years were HIVneg with the lag are wrong
end_year_all       = 2023                 # This is the year of latest partner data
end_year_sdg       = 2030                 # This is the final year of prediction
start_year_hiv     = 2018
start_year_tb      = 2014
start_year_malaria = 2014
end_year_hiv       = 2023
end_year_tb        = 2019
end_year_malaria   = 2019

# Install packages if neccesary
if(firstrun>0) {
  install.packages("dplyr")
}

library(dplyr) # require instead
library(data.table)   # For like function (%like%)
library(tidyr)


# Set computer, wd and load data
if (computer ==1){
  setwd("/Users/mc1405/TGF_data/IC8/partner/")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/Investment Case 8th/KPIs/RCode"
  
  # Load the pip data 
  df_hiv2 = read.csv("hiv/2024_12_12/df_partner_hiv_12Dec.csv", stringsAsFactors = FALSE)
  df_malaria2  = read.csv("malaria/2024_12_12/df_partner_malaria_6Jan.csv", stringsAsFactors = FALSE)
  df_tb2      =  read.csv("tb/2024_12_12/df_partner_tb_12Dec.csv", stringsAsFactors = FALSE)
  
  # Load list of countries
  setwd("/Users/mc1405/Dropbox/The Global Fund/Investment Case 8th/KPIs/RCode")
  df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
  df_iso_tb      = read.csv("List of countries/tb_iso.csv", stringsAsFactors = FALSE)
  df_iso_malaria = read.csv("List of countries/malaria_iso.csv", stringsAsFactors = FALSE)
}

# List of indicators
list_indc_hiv_pip     = c("ISO3", "Year", "hiv_cases_n_pip", "hiv_deaths_n_pip", "HIVpos_n_pip", "Population_n_pip")
list_indc_tb_pip      = c("ISO3", "Year", "tb_cases_n_pip", "tb_deathsnohiv_n_pip", "tb_pop_n_pip")
list_indc_malaria_pip = c("ISO3", "Year", "malaria_cases_n_pip", "malaria_deaths_n_pip", "malaria_par_n_pip")  
list_ind              = c("ISO3", "Year", "incidence", "mortality")


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
    mortality = tb_deathsnohiv_n_pip / tb_pop_n_pip,
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

# Extract an incidence df
df_hiv_inc      = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "incidence"))
df_tb_inc       = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "incidence"))
df_malaria_inc  = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "incidence"))

# Extract a mortality df
df_hiv_mort     = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "mortality"))
df_tb_mort      = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "mortality"))
df_malaria_mort = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "mortality"))

# Filter to the necessary years, for each disease
df_hiv_inc     = df_hiv_inc %>% filter(Year>=start_year_hiv & Year<=end_year_hiv)
df_tb_inc      = df_tb_inc %>% filter(Year>=start_year_tb & Year<=end_year_tb)
df_malaria_inc = df_malaria_inc %>% filter(Year>=start_year_malaria & Year<=end_year_malaria)

df_hiv_mort     = df_hiv_mort %>% filter(Year>=start_year_hiv & Year<=end_year_hiv)
df_tb_mort      = df_tb_mort %>% filter(Year>=start_year_tb & Year<=start_year_tb)
df_malaria_mort = df_malaria_mort %>% filter(Year>=start_year_malaria & Year<=start_year_malaria)

# Get average annual change
df_hiv_inc = df_hiv_inc %>%
  group_by(ISO3) %>%
  mutate(
    annual_change = incidence-lag(incidence),
  )

df_hiv_inc2 = df_hiv_inc %>%
  group_by(ISO3) %>%
  mutate(
    AAC = mean(annual_change, na.rm = TRUE),
  )

# Get latest value to update intercept later
df_hiv_inc_intercept      = df_hiv_inc %>% filter(Year==end_year_all)
df_tb_inc_intercept       = df_tb_inc %>% filter(Year==end_year_all)
df_malaria_inc_intercept  = df_malaria_inc %>% filter(Year==end_year_all)

df_hiv_mort_intercept     = df_hiv_mort %>% filter(Year==end_year_all)
df_tb_mort_intercept      = df_tb_mort %>% filter(Year==end_year_all)
df_malaria_mort_intercept = df_malaria_mort %>% filter(Year==end_year_all)





# Fit Linear models per disease and indicator, per country
model_lm = df_hiv_inc %>% group_by(ISO3) %>% do(model = lm(incidence ~ Year, data = .))
coef_lm_hiv_inc <- model_lm %>%     # Get the key coefficients
  summarise(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )

# Bind 2023 information with model data
coef_lm_hiv_inc <- cbind(df_hiv_inc_intercept, coef_lm_hiv_inc)

# Update intercept
coef_lm_hiv_inc = coef_lm_hiv_inc %>%
  mutate(
    intercept = incidence - (slope*Year)
  )
coef_lm_hiv_inc = subset(coef_lm_hiv_inc, select = -c(Year))
  
  

# Fit exponential models per disease and indicator, per country
model_exp = df_hiv_inc %>% group_by(ISO3) %>% do(model = lm(log(incidence) ~ Year, data = .))
coef_exp_hiv_inc <- model_exp %>%     # Get the key coefficients
  summarise(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )

# Bind 2023 information with model data
coef_exp_hiv_inc <- cbind(df_hiv_inc_intercept, coef_exp_hiv_inc)

# Update intercept
coef_exp_hiv_inc = coef_exp_hiv_inc %>%
  mutate(
    intercept = log(incidence) - (slope*Year)
  )
coef_exp_hiv_inc = subset(coef_exp_hiv_inc, select = -c(Year))



# Make a new empty dataframe for every country and prospective year
# Then merge in slope and intercept and compute future incidence based on linear
years = c(end_year_all:end_year_sdg)
hiv_inc_lm = expand.grid(ISO3=df_iso_hiv$ISO3, Year=years)
hiv_inc_lm = hiv_inc_lm[order(hiv_inc_lm$ISO3),]
hiv_inc_lm = merge(coef_lm_hiv_inc, hiv_inc_lm, by = "ISO3", all.y=TRUE)
hiv_inc_lm = hiv_inc_lm %>%
  mutate(
    incidence = (slope*Year) + intercept
  )

# Now exponential projection
years = c(end_year_all:end_year_sdg)
hiv_inc_exp = expand.grid(ISO3=df_iso_hiv$ISO3, Year=years)
hiv_inc_exp <- hiv_inc_exp[order(hiv_inc_exp$ISO3),]
hiv_inc_exp <- merge(coef_exp_hiv_inc, hiv_inc_exp, by = "ISO3", all.y=TRUE)
hiv_inc_exp = hiv_inc_exp %>%
  mutate(
    incidence = exp(slope*Year + intercept)
  )



write.csv(hiv_inc_exp,file="/Users/mc1405/Dropbox/The Global Fund/Investment Case 8th/KPIs/RCode/hiv_exp_test.csv", row.names=FALSE)



