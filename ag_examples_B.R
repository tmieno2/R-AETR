# THIS FILE CONTAINS STEPS TO:
# - prepare dataset  df_pop_grain   
# - conduct a regression analysis with grain sales and population data 

library(tidyverse)
library(modelr)
library(rnassqs)
library(RColorBrewer)
library(stargazer)

# ----------------------------------------------------------------------------------------------
# county level population data: 
# ----------------------------------------------------------------------------------------------
county_pop1 <- read_lines("datasets/raw/us.1969_2017.19ages.adjusted.txt")
county_pop2 <- read_lines("datasets/raw/us.1990_2017.19ages.adjusted.txt")
# https://seer.cancer.gov/popdata/popdic.html

# Race
# 1969+ data:
# 1 = White
# 2 = Black
# 3 = Other
# 1990+ data:
#   1 = White
# 2 = Black
# 3 = American Indian/Alaska Native
# 4 = Asian or Pacific Islander 

# Origin
# only Applicable to 1990+ data:
# 0 = Non-Hispanic
# 1 = Hispanic
# 9 = Not applicable in 1969+ W,B,O files 

# Sex
# 1 = Male
# 2 = Female 

# 19 Age group data:
# 00 = 0 years
# 01 = 1-4 years
# 02 = 5-9 years
# 03 = 10-14 years
# 04 = 15-19 years
# …
# 17 = 80-84 years
# 18 = 85+ years

df_county_pop1 <-
  data.frame(Year = substr(county_pop1, 1,4) %>% as.numeric(),
             St_name = substr(county_pop1, 5,6),
             FIPS = substr(county_pop1, 7, 11),
             registry =  substr(county_pop1, 12, 13),
             race = substr(county_pop1, 14, 14),
             origin = substr(county_pop1, 15, 15),
             sex = substr(county_pop1, 16, 16),
             age_group =  substr(county_pop1, 17, 18),
             population = substr(county_pop1, 19, 27)%>% as.numeric())
save(df_county_pop1, file = "datasets/df_county_pop1.RData")


# ----------------------------------------------------------------------------------------------
# download data and prepare a dataset of county-level grain production, df_pop_grain  
# ----------------------------------------------------------------------------------------------

# for more information about rnassqs, see: 
# https://github.com/ropensci/rnassqs
# https://ropensci.github.io/rnassqs/articles/rnassqs.html

NASSQS_TOKEN <- "C9B668A9-3062-3CE5-96B8-2D6D1AC432BF"
# Set your api key before requesting data
nassqs_auth(key = NASSQS_TOKEN)


# ------- download grain data ------------
# example: Parameters to query on and data call
params <- list(commodity_desc = "CORN", year__GE = 2012, state_alpha = c("VA", "WA"))
d <- nassqs(params)


# example: check queries 
params1 <- list(source_desc = "survey", commodity_desc = "CORN",  reference_period_desc = "year",
                short_desc = c("CORN, GRAIN - PRODUCTION, MEASURED IN BU"),
                agg_level_desc = "county", 
                state_fips_code__NE = 99,
                year = c(1970,1980,1990,2000, 2017, c(2005,2010,2014,2015, 2016)))
d1 <- nassqs(params1)
d1 <- d1 %>% select(sector_desc, short_desc, 
                    state_alpha, state_fips_code, state_ansi, 
                    county_code, county_ansi, county_name,
                    year, commodity_desc, Value, 
                    location_desc, asd_code, asd_desc )

d1$year %>% table

# It turns out there is more data points in 1972 than in 1970. We use 1972. 
# First define a base parameter list to modify for each new query
base_params_grain <- list(
  source_desc = "survey", 
  reference_period_desc = "year",
  agg_level_desc = "county", 
  state_fips_code__NE = 99,
  year = c(1972,1982,1992,2002, 2017)
)

# List of parameters that vary for each query
# production at the county-level 
param_list_grain <- list(
  corn = list(
    commodity_desc = "CORN",
    short_desc = "CORN, GRAIN - PRODUCTION, MEASURED IN BU"),
  soybean = list(
    commodity_desc = "soybeans",
    short_desc = "soybeans - PRODUCTION, MEASURED IN BU"),
  wheat = list(
    commodity_desc = "wheat",
    short_desc = "wheat - PRODUCTION, MEASURED IN BU"),
  sorghum = list(
    commodity_desc = "sorghum",
    short_desc = "sorghum, grain - PRODUCTION, MEASURED IN BU"),
  rice = list(
    commodity_desc = "rice",
    short_desc = "rice - PRODUCTION, MEASURED IN cwt"),
  tobacco = list(
    commodity_desc = "tobacco",
    # short_desc = "tobacco - PRODUCTION, MEASURED IN lb"),
    short_desc = "tobacco, flue-cured (class 1) - PRODUCTION, MEASURED IN lb"),
  cotton_upland = list(
    commodity_desc = "cotton",
    short_desc = "COTTON, UPLAND - PRODUCTION, MEASURED IN 480 LB BALES")
  # note: pima is a minor variety and is ignored here
  # cotton_pima = list( 
  #   commodity_desc = "cotton",
  #   short_desc = "COTTON, PIMA - PRODUCTION, MEASURED IN 480 LB BALES")
)

# Iterate through different variable queries
data_list_grain <- lapply(param_list_grain, function(var_params) {
  # Create the new parameter list and append the query items that vary by query
  params <- base_params_grain
  for(n in names(var_params)) { 
    params[[n]] <- var_params[[n]]
  }
  nassqs(params)
})

data_list_grain$corn %>% with(table(year))
data_list_grain$wheat %>% with(table(year))
data_list_grain$soybean %>% with(table(year))


base_params_price <- base_params_grain
base_params_price$reference_period_desc <- c("marketing year","year")
base_params_price$agg_level_desc <-  c("state", "national")
base_params_price$state_fips_code__NE <- NULL 

param_list_price <- list(
  corn = list(
    commodity_desc = "CORN",
    short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU"),
  soybean = list(
    commodity_desc = "soybeans",
    short_desc = "soybeans - PRICE RECEIVED, MEASURED IN $ / BU"),
  wheat = list(
    commodity_desc = "wheat",
    short_desc = "wheat - PRICE RECEIVED, MEASURED IN $ / BU"),
  sorghum = list(
    commodity_desc = "sorghum",
    short_desc = "sorghum, grain - PRICE RECEIVED, MEASURED IN $ / cwt"), # different unit from production
  rice = list(
    commodity_desc = "rice",
    short_desc = "rice - PRICE RECEIVED, MEASURED IN $ / cwt"),
  tobacco = list(
    commodity_desc = "tobacco",
    short_desc = "tobacco - PRICE RECEIVED, MEASURED IN $ / lb"),
  cotton_upland = list(
    commodity_desc = "cotton",
    short_desc = "COTTON, UPLAND - PRICE RECEIVED, MEASURED IN $ / LB") # different unit from production
  # cotton_pima = list(
  #   commodity_desc = "cotton",
  #   short_desc = "COTTON, PIMA - PRICE RECEIVED, MEASURED IN $ / LB") # different unit from production
)

data_list_price <- lapply(param_list_price, function(var_params) {
  # Create the new parameter list and append the query items that vary by query
  params <- base_params_price
  for(n in names(var_params)) { 
    params[[n]] <- var_params[[n]]
  }
  nassqs(params)
})


# Using dplyr to bind the data list
df_grain <- bind_rows(data_list_grain) %>% 
  select(short_desc, state_alpha, state_fips_code,
         county_code, county_name,
         year, commodity_desc, Value)

df_price <- bind_rows(data_list_price)  %>% 
  select(short_desc, state_alpha, state_fips_code,
         county_code, county_name,
         year, commodity_desc, Value)

save(df_grain, df_price, file = "datasets/NASS_data_grain.RData")



# ---------------- merge datasets ----------------------
# county ag production data 
load(file = "datasets/df_county_pop1.RData")
load(file = "datasets/NASS_data_grain.RData")


# CPI used for adusting for price levels
cpi <- read_csv("datasets/raw/cpi.csv")
base_cpi <- cpi$Annual[cpi$Year==2017]
cpi <- cpi %>% mutate(
  cpi_base_82_84 = Annual,
  cpi_base_2017 = Annual/base_cpi
) %>% select(Year, cpi_base_82_84, cpi_base_2017)


df_price <-  df_price %>% 
  mutate(price = as.numeric(Value),
         price_desc = short_desc) %>%
  # filter(price_desc!= "COTTON, PIMA - PRICE RECEIVED, MEASURED IN $ / LB") %>% # drop PIMA cotton price 
  select(state_fips_code, year, commodity_desc, price, price_desc) %>%
  mutate(
    # sorghum: 56 lb/bu  https://grains.org/markets-tools-data/tools/converting-grain-units/
    price = ifelse(commodity_desc=="SORGHUM", 56/100 * price, price),
    price = ifelse(commodity_desc=="COTTON", 480 * price, price)
  )


df_grain_prod <- inner_join(df_grain, df_price,
                            by=c("state_fips_code", "year", "commodity_desc"))
df_grain_prod <- df_grain_prod %>%
  mutate(
    value = gsub(",","", Value) %>%  as.numeric(),
    sales = value * price # technically speaking its the value of production, not sales
  )


df_grain_prod %>% group_by(year, commodity_desc) %>%
  summarise(
    price = mean(price, na.rm = T),
    value = sum(value, na.rm = T),
    sales = sum(sales, na.rm=T)) %>% print(n=100)

# collpase and add grain sales values across grains 
df_grain_collapse <- df_grain_prod %>% 
  group_by(year, state_alpha, state_fips_code, county_code, county_name) %>%
  summarise(grain_prod = sum(sales, na.rm = T)) %>%
  ungroup() %>%
  mutate( fips = paste0(state_fips_code, county_code, sep ="")) %>%
  left_join(cpi, by=c("year"="Year")) %>%
  mutate(
    grain_prod = grain_prod / cpi_base_2017,
    grain_prod = ifelse(is.na(grain_prod), 0, grain_prod)
  )


# county population data 
df_pop1 <- df_county_pop1 %>% filter(Year %in% c(1972,1982,1992,2002,2017))

df_pop1 <- df_pop1 %>% 
  mutate(age_group_num = as.integer(age_group),
         age_group2 = cut(age_group_num, breaks = c(-Inf, 3, 7, 11, 15, Inf),
                          labels = c("age_0-14", "age_15-29",
                                     "age_30-44","age_45-59",
                                     "age_60-up")))
df_pop1$age_group2 %>% table

df_pop1 <- df_pop1 %>% group_by(Year, St_name, FIPS, age_group2) %>%
  summarise(pop = sum(population, na.rm=T))

df_pop1 <- df_pop1 %>% 
  arrange(FIPS, age_group2, Year) %>%
  group_by(FIPS,  age_group2) %>% 
  mutate( pop.lag = dplyr::lag(pop, 1), 
          pop_ch_pct = (pop - pop.lag)/pop.lag* 100)


# merge datasets 
df_pop1$FIPS %>% unique() %>% length()
df_grain_collapse$fips %>% unique() %>% length() 
df_pop_grain <- full_join(df_pop1, df_grain_collapse, by = c("Year"="year", "FIPS"="fips")) %>%
  filter(!(St_name %in% c("KR", "HI", "AK"))) # exclude Hawaii and Alaska; there's not much data  

save(df_pop_grain, file= "df_pop_grain.RData")



# ----------------------------------------------------------------------------------------------
#  regression analysis 
# ----------------------------------------------------------------------------------------------
source("helpers.R")

load(file= "df_pop_grain.RData")


# create a FIPS indicator for having a non-missing data in 1982  
idx_data_exist_82 <- df_pop_grain %>% filter(Year == 1982) %>%
  select(FIPS, grain_prod)  %>% filter(!is.na(grain_prod)) %>%
  select(FIPS) %>% unique() 


df_pop_grain <- df_pop_grain %>%
  left_join(idx_data_exist_82 %>% mutate(data_exist_82 = 1)) %>%
  group_by(Year, FIPS) %>%
  mutate(
    # inpute na with zero for year>1980 if data existed for the county in 1980 
    grain_prod = ifelse(!is.na(grain_prod), grain_prod, 
                        ifelse(data_exist_82 ==1 & Year > 1982, 0, NA)),   
    pop_tot = sum(pop, na.rm=T),
    grain_prod_person = grain_prod/10^3/pop_tot
  )

df_pop_grain <- df_pop_grain %>%
  arrange(FIPS, age_group2, Year) %>%
  group_by(FIPS, age_group2) %>% 
  mutate(
    pop.lag4 = dplyr::lag(pop, 4), 
    pop_ch_pct4 = ifelse(pop.lag4==0, 0, (pop - pop.lag4)/pop.lag4 * 100),
    pop_tot.lag =  dplyr::lag(pop_tot, 1),
    pop_tot_ch_pct =  ifelse(pop_tot.lag==0, 0, (pop_tot - pop_tot.lag)/pop_tot.lag * 100),
    # note: population total lag is needed only for one age group, set the rest to NA 
    pop_tot.lag4 =   ifelse(age_group2!="age_0-14", NA, 
                            dplyr::lag(pop_tot, 4)), 
    pop_tot_ch_pct4 = ifelse(pop_tot.lag4==0, 0,
                             (pop_tot - pop_tot.lag4)/pop_tot.lag4 * 100),
    grain_prod_person.lag =  dplyr::lag(grain_prod_person, 1), 
    grain_prod.lag = dplyr::lag(grain_prod, 1), 
    grain_ch = grain_prod - grain_prod.lag,
    grain_ch_pct = ifelse(grain_prod.lag ==0, 0, grain_ch/grain_prod.lag * 100),
    grain_prod.lag4 = dplyr::lag(grain_prod, 4), 
    grain_ch4 = grain_prod - grain_prod.lag4,
    grain_ch_pct4 = ifelse(grain_prod.lag4 ==0, 0, grain_ch4/grain_prod.lag4 * 100),
    grain_prod_person.lag4 =  dplyr::lag(grain_prod_person, 4)
  ) %>%
  ungroup()

# duplicate variables with names that are explanatory
df_pop_grain <- df_pop_grain %>%
  mutate(
    pop_tot_ch_pct_72_17 = pop_tot_ch_pct4, 
    ln_grain_prod_person_1972  = log(grain_prod_person.lag4+1),
    ln_grain_prod_1972 = log(grain_prod.lag4/10^6+1),
    grain_ch_pct_72_17 = grain_ch_pct4, 
    ln_grain_prod_person  = log(grain_prod_person.lag+1),
    ln_grain_prod.lag = log(grain_prod.lag/10^6+1),
    grain_ch_pct_neg100 = 1 * grain_ch_pct==-100,
    grain_ch_pct_0 = 1 * grain_ch_pct==0
  )

breaks_1 <- c(-Inf, 0, 2.5,  5, 7.5,  10,  15, 20,  Inf)
df_pop_grain <- df_pop_grain %>% my_cat_var(grain_prod_person, breaks_1)

map_county_cat_var(df_pop_grain  %>% filter(Year == 1972, 
                                            age_group2=="age_0-14",
                                            !is.na(cat_grain_prod_person)),
                   var =  cat_grain_prod_person, lower_48 =T, 
                   legend_varname = "Grain Production, $1,000/person",
                   title = "Commodity Grain Production per County Resident, 1972",
                   caption = "Data Source: NASS. Price level is adjusted for 2017 dollars.")


breaks_2 <- c(-Inf,-75, -50, -25, 0, 50, 100, 200,  Inf)
df_pop_grain <- df_pop_grain %>% my_cat_var(grain_ch_pct4, breaks_2)

map_county_cat_var(df_pop_grain  %>% filter(age_group2=="age_0-14",
                                            !is.na(cat_grain_ch_pct4)),
                   var =  cat_grain_ch_pct4, lower_48 =T, 
                   var_colors = brewer.pal(8, "RdBu"),
                   legend_varname = "Grain Production change, %",
                   title =  "Change in Commodity Grain Production, 1972-2017",
                   caption = "Data Source: NASS. Price level is adjusted for 2017 dollars.")



# check production of zero 
tmp <- left_join(geo_county, df_pop_grain,  by =c("GEOID"="FIPS")) %>% 
  mutate(zero_production = grain_prod == 0)

tmp %>% filter(Year == 1972, age_group2=="age_0-14") %>%
  ggplot() + 
  geom_sf(aes(fill = zero_production)) +
  coord_sf(datum = NA) + theme_minimal() 

tmp %>% filter(Year == 2002, age_group2=="age_0-14") %>%
  ggplot() + 
  geom_sf(aes(fill = zero_production)) +
  coord_sf(datum = NA) + theme_minimal() 

tmp %>% filter(Year == 2017, age_group2=="age_0-14") %>%
  ggplot() + 
  geom_sf(aes(fill = zero_production)) +
  coord_sf(datum = NA) + theme_minimal() 

df_pop_grain <- df_pop_grain %>% my_cat_var(pop_tot_ch_pct_72_17, breaks_2)

map_county_cat_var(df_pop_grain  %>% filter(!is.na(pop_tot_ch_pct_72_17)),
                   var =  cat_pop_tot_ch_pct_72_17, lower_48 =T, 
                   legend_varname = "Population Change, %",
                   var_colors = brewer.pal(8, "RdBu"),
                   title = "Population change, 1972-2017",
                   caption = "Data Source: National Cancer Institute SEER Program")

df_pop_grain  %>% 
  ggplot(aes(x = grain_prod_person.lag4, y = pop_tot_ch_pct_72_17 )) +
  geom_point(alpha=.2) +
  labs(x = "Grain production per person in 1972, $1,000", 
       y = "Population change 1972-2017, % ")

df_pop_grain  %>% filter(pop_tot_ch_pct_72_17 < 500) %>%
  ggplot(aes(x = grain_prod_person.lag4 + 1, y = pop_tot_ch_pct_72_17 )) +
  geom_point(alpha=.2) + geom_smooth() +
  coord_trans(x = "log10") +
  labs(x = "Grain production per person in 1972, $1,000", 
       y = "Population change 1972-2017, % ")

df_pop_grain  %>% filter(pop_tot_ch_pct_72_17 < 500) %>%
  ggplot(aes(x = grain_prod.lag4/10^6 + 1, y = pop_tot_ch_pct_72_17 )) +
  geom_point(alpha=.2) + geom_smooth() +
  coord_trans(x = "log10") +
  labs(x = "Grain production in 1972 in log-scale, $ 1 M", 
       y = "Population change 1972-2017, % ")

df_pop_grain  %>% filter(pop_tot_ch_pct_72_17 < 500, grain_ch_pct4 < 500) %>%
  ggplot(aes(x = grain_ch_pct4, y = pop_tot_ch_pct_72_17 )) +
  geom_point(alpha=.2) + 
  geom_smooth(data = df_pop_grain  %>% filter(pop_tot_ch_pct_72_17 < 500, 
                                              grain_ch_pct4 < 500,
                                              grain_ch_pct4!=-100))   +
  labs(x = "Grain production change in  1972-2017, %", 
       y = "Population change 1972-2017, % ")



# impute data pop_tot_ch_pct_72_17 >= 500 or grain_ch_pct_72_17 >=500 with NA
df_pop_grain <- df_pop_grain  %>%
  mutate(
    pop_tot_ch_pct_72_17 = ifelse(pop_tot_ch_pct_72_17 < 500, pop_tot_ch_pct_72_17, NA),
    grain_ch_pct_72_17 = ifelse(grain_ch_pct_72_17 < 500, grain_ch_pct_72_17, NA),
  )

lm_1 <- lm(pop_tot_ch_pct_72_17 ~ ln_grain_prod_person_1972 + grain_ch_pct_72_17 +
             (grain_ch_pct_72_17==-100)  + St_name, 
           data = df_pop_grain)

lm_2 <- lm(pop_tot_ch_pct_72_17 ~   ln_grain_prod_1972 + grain_ch_pct_72_17 +
             (grain_ch_pct_72_17==-100) + St_name, 
           data = df_pop_grain)

stargazer(lm_1, lm_2, type="text", keep = c("ln_grain_prod_person_1972",
                                            "ln_grain_prod_1972",
                                            "grain_ch_pct_72_17"),
          add.lines = list(c("State fixed effects","Yes","Yes")),
          out="reg_result_1.txt")


# add model predictions, except states that have no grain production 
df_pop_grain_res <- df_pop_grain %>% 
  filter(!(St_name %in% c("CT", "DC", "MA", "ME", "NH", "RI", "VT"))) %>%
  add_residuals(lm_1, var="resid_lm_1") %>%
  add_residuals(lm_2, var="resid_lm_2") 

breaks_3 <- c(-Inf, -75, -50, -25, 0, 25, 50, 75,  Inf)
df_pop_grain_res <- df_pop_grain_res %>% 
  my_cat_var(resid_lm_1, breaks_3) %>%
  my_cat_var(resid_lm_2, breaks_3)

map_county_cat_var(df_pop_grain_res  %>% filter(!is.na(resid_lm_1)),
                   var =  cat_resid_lm_1, lower_48 =T, 
                   legend_varname = "Residual",
                   var_colors = brewer.pal(8, "RdBu"),
                   title = "Model 'lm_1' Residual in Population Change, 1972-2017")

map_county_cat_var(df_pop_grain_res %>% filter(!is.na(resid_lm_2)),
                   var =  cat_resid_lm_2, lower_48 =T, 
                   legend_varname = "Residual",
                   var_colors = brewer.pal(8, "RdBu"),
                   title = "Model 'lm_2' Residual in Population Change, 1972-2017")





df_pop_grain <- df_pop_grain %>% my_cat_var(pop_ch_pct4, breaks_2)

map_county_cat_var(df_pop_grain  %>% filter(!is.na(pop_ch_pct4), age_group2 == "age_15-29"),
                   var =  cat_pop_ch_pct4, lower_48 =T, 
                   legend_varname = "Population Change, %",
                   var_colors = brewer.pal(8, "RdBu"),
                   title = "Population change, age group 15-29, 1972-2017",
                   caption = "Data Source: National Cancer Institute SEER Program")

map_county_cat_var(df_pop_grain  %>% filter(!is.na(pop_ch_pct4), age_group2 == "age_60-up"),
                   var =  cat_pop_ch_pct4, lower_48 =T, 
                   legend_varname = "Population Change, %",
                   var_colors = brewer.pal(8, "RdBu"),
                   title = "Population change, age group 60 or above, 1972-2017",
                   caption = "Data Source: National Cancer Institute SEER Program")


# impute data pop_ch_pct >= 500 or grain_ch_pct >=500 with NA
df_pop_grain <- df_pop_grain  %>%
  mutate(
    pop_ch_pct = ifelse(pop_ch_pct < 500, pop_ch_pct, NA),
    grain_ch_pct = ifelse(grain_ch_pct < 500, grain_ch_pct, NA),
  )


lm_3 <- lm(pop_ch_pct ~ ln_grain_prod.lag + grain_ch_pct +
             grain_ch_pct_0 + grain_ch_pct_neg100 + St_name, 
           data = df_pop_grain  %>% filter(age_group2=="age_15-29",Year == 1982))

lm_4 <- lm(pop_ch_pct ~ ln_grain_prod.lag + grain_ch_pct +
             grain_ch_pct_0 + grain_ch_pct_neg100 + St_name, 
           data = df_pop_grain  %>% filter(age_group2=="age_60-up",Year == 1982))

lm_5 <- lm(pop_ch_pct ~ ln_grain_prod.lag + grain_ch_pct +
             grain_ch_pct_0 + grain_ch_pct_neg100 + St_name, 
           data = df_pop_grain  %>% filter(age_group2=="age_15-29",Year == 2017))

lm_6 <- lm(pop_ch_pct ~ ln_grain_prod.lag + grain_ch_pct +
             grain_ch_pct_0 + grain_ch_pct_neg100  + St_name, 
           data = df_pop_grain  %>% filter(age_group2=="age_60-up",Year == 2017))

stargazer(lm_3, lm_4, lm_5, lm_6, type="text",
          keep = c("ln_grain_prod.lag", "grain_ch_pct"),
          add.lines = list(c("State fixed effects",rep("Yes",4)),
                           c("Sample age group","Age 15-29", "60 and up", "Age 15-29", "60 and up"),
                           c("Sample time period", "1972-82", "1972-82", "2002-2017","2002-2017")),
          omit.stat=c("f", "ser"),
          out="reg_result_2.txt")


# create the age-group and time period combination 
df_pop_grain <- df_pop_grain %>% 
  mutate(age_era = paste0(age_group2, ":", Year, sep=''))

# create a regression function to be applied to a given data.frame
pop_ch_model <- function(df) {
  lm( pop_ch_pct ~ ln_grain_prod.lag + grain_ch_pct + 
        grain_ch_pct_0 + grain_ch_pct_neg100 + St_name, data=df)
}

# function to run a model by group via nest()
run_model_by_group <- function(df, group_var, model_as_function) {
  group_var <- enquo(group_var)
  df2 <- df %>% group_by(!!group_var) %>% nest()
  df2 %>% mutate(
    model = map(data, model_as_function),
    rlt  = map(model, summary) %>% map(coefficients) %>% map(data.frame),
    varname = map(rlt, rownames),
    estimate = map(rlt, ~ .x$Estimate),   
    st_error = map(rlt, ~ .x$Std..Error),
    t_stat = map(rlt, ~ .x$t.value)
  )
}

lm_pop_age_era <-  
  run_model_by_group(df_pop_grain  %>% filter(!is.na(age_group2), Year >= 1980),
                     group_var = age_era, 
                     model_as_function = pop_ch_model)

lm_pop_age_era %>% print(n=5)

rlt_age_era <- lm_pop_age_era %>% select(age_era, varname, estimate, st_error, t_stat) %>%
  unnest(cols = c("varname", "estimate", "st_error", "t_stat")) 
rlt_age_era %>% print(n=5)

rlt_age_era <- rlt_age_era %>% ungroup() %>%
  mutate(Year = str_extract(age_era, "\\d{4}") %>% as.numeric(), 
         age_group = str_extract(age_era, "[:alpha:]+.\\d+-\\d+"),
         age_group = ifelse(!is.na(age_group), age_group, 
                            str_extract(age_era, "[:alpha:]+.\\d+-[:alpha:]+")),
         age_lower = str_extract(age_era, "\\d+") %>% as.numeric(),
         era = recode(Year,'1982'='72-82', '1992'='82-92','2002'='92-02','2017'='02-17'),
         age_era2 = paste0(gsub("_"," ",age_group), ", ",era))


# extract coefficient esimates of interests
rlt_age_era2 <- rlt_age_era %>%
  filter(varname %in% c("ln_grain_prod.lag","grain_ch_pct", 
                        "grain_ch_pct_0TRUE", "grain_ch_pct_neg100TRUE")) %>% 
  mutate(ymin = estimate - 1.96*st_error, 
         ymax = estimate + 1.96*st_error,
         age_era2 = reorder(age_era2, Year + age_lower/100, FUN = mean),
         varname2 = factor(varname, levels=c("ln_grain_prod.lag","grain_ch_pct",
                                             "grain_ch_pct_0TRUE", "grain_ch_pct_neg100TRUE"),
                           labels = c("logged grain production in beginning year", 
                                      "percent change in grain production",
                                      "indicator: grain production was zero",
                                      "indicator: grain production declined to zero"),
                           ordered =T)) 

rlt_age_era2 %>%
  filter(varname %in% c("ln_grain_prod.lag","grain_ch_pct")) %>% 
  ggplot(aes(x = age_era2, y = estimate, color = factor(Year))) + 
  geom_hline(yintercept = 0, colour = "white", size = 1.5) + 
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +  
  facet_wrap(~varname2, scales = "free_x") +
  labs(y = "Coefficient estimates with 95% confidence intervals",
       x = "Age group and time period") +
  theme(legend.position = "none")  + coord_flip()

# rlt_age_era2 %>%
#   filter(varname %in% c("grain_ch_pct_0TRUE", "grain_ch_pct_neg100TRUE")) %>% 
#   ggplot(aes(x = age_era2, y = estimate, color = factor(Year))) + 
#   geom_hline(yintercept = 0, colour = "white", size = 1.5) + 
#   geom_point() +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax)) +  
#   facet_wrap(~varname2, scales = "free_x") +
#   labs(y = "Coefficient estimates with 95% confidence intervals",
#        x = "Age group and time period") +
#   theme(legend.position = "none")  + coord_flip()


# popular press articles on this topic:
# https://www.citylab.com/life/2018/05/mapping-americas-aging-population/561200/

