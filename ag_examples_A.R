# THIS FILE CONTAINS STEPS TO:
# - Prepare Ag Census data 
# - Prepare datasets df_NAICS and df_NAICS_simple and generate plots
# - Prepare data and generate sankey diagrams


library(tidyverse)
library(modelr)
library(broom)
library(knitr)
library(gt)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(viridis)
library(tidycensus)
library(stargazer)

# ----------------------------------------------------------------------------------------------
#  prepare downloaded Ag Census data for analyses 
# ----------------------------------------------------------------------------------------------

# summary table data of the Census of Agriculture 2017 obtained from: 
# https://www.nass.usda.gov/Publications/AgCensus/2017/index.php
df17 <- read_tsv("datasets/raw/2017_cdqt_data.txt", col_names = FALSE)

colnames(df17) <- c("census_chapter","census_table", "X3", "X4", "Sector", "Item", "Commodity", 
                    "geog_level", "St_code", "St_name", "State_name", "Co_code", "Co_name", "Class", "Value")

# X1: Chapter 1 US/State level, 2 State/county level 
# X2: tables 1-77 
# X5: Sector - ANIMALS & PRODUCTS, CROPS, DEMOGRAPHICS, ECONOMICS, ENVIRONMENTAL 
# X8: NATIONAL, STATE, COUNTY
# X9: St_code 99-US
# Class applies when there are data provided by farm size etc. 

df17 %>% head

# separate data into national, state, and county level datasets 
us17 <- df17 %>% filter(census_chapter==1, St_code==99)
state17 <- df17 %>% filter(census_chapter==1, St_code!=99)
county17 <- df17 %>% filter(census_chapter==2, St_code!=99) 
df17 <- NULL # delete the large dataset from the memory 

save(us17, state17, county17, file="datasets/Ag_census_2017.RData")




#  --------------------------- assemble dataset df_NAICS ---------------------------
load(file="datasets/Ag_census_2017.RData")

# create a base dataset "farms_NAICS" 
farms_NAICS <-  
  state17 %>% filter(census_table==75, 
                     Item== "FARM OPERATIONS - NUMBER OF OPERATIONS",
                     X3==1) %>%
  mutate(farms = Value) %>% 
  select(St_code, St_name, State_name, Class, farms)

# create a dataset containing other interests of variables
vars_NAICS <- 
  state17 %>% filter(census_table==75) %>%
  mutate(Item2 = recode(Item, 
                        "FARM OPERATIONS - ACRES OPERATED"="acre_total",
                        "AG LAND, CROPLAND - ACRES"="acre_cropland",
                        "AG LAND, WOODLAND - ACRES"="acre_woodland",
                        "AG LAND, PASTURELAND, (EXCL CROPLAND & WOODLAND) - ACRES"="acre_pastureland",
                        "AG LAND, (EXCL CROPLAND & PASTURELAND & WOODLAND) - ACRES"="acre_other",
                        "COMMODITY TOTALS, INCL GOVT PROGRAMS - RECEIPTS, MEASURED IN $"="revenue_sales_plus_gov",
                        "COMMODITY TOTALS - SALES, MEASURED IN $"="revenue_sales",
                        "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $"="net_income_operation",
                        "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $"="farm_related_income",
                        "INTEREST - EXPENSE, MEASURED IN $"="interest_expense",
                        "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $"="asset_value_land_buildings",
                        "MACHINERY TOTALS - ASSET VALUE, MEASURED IN $"="asset_value_machinery",
                        "PRODUCERS, (ALL) - NUMBER OF PRODUCERS"="producers",
                        "LABOR, HIRED - NUMBER OF WORKERS"="hired_labor_workers",
                        "LABOR, HIRED - EXPENSE, MEASURED IN $" = "hired_labor_expense",
                        "LABOR, UNPAID - NUMBER OF WORKERS"="unpaid_labor_workers"))  %>%
  filter(Item2 %in% c("acre_total", "acre_cropland", "acre_woodland", "acre_pastureland", "acre_other",
                      "revenue_sales_plus_gov", "revenue_sales",
                      "net_income_operation","farm_related_income", "interest_expense",
                      "asset_value_land_buildings","asset_value_machinery",
                      "producers", "hired_labor_workers","hired_labor_expense", "unpaid_labor_workers")) %>%
  select(St_code, Class, Item2, Value) %>%
  pivot_wider(names_from = Item2, values_from = Value)

# merge two datasets 
df_NAICS <- farms_NAICS %>% left_join(vars_NAICS, by = c("St_code", "Class")) 


# create additional variables 
df_NAICS <- df_NAICS %>%
  mutate(
    net_income_total = net_income_operation + farm_related_income, 
    asset_value_total = asset_value_land_buildings + asset_value_machinery,
    USDA_region = ifelse(St_name %in% c("WA","OR","ID","CA","NV","UT","AZ"), "Pacific West", 
                         ifelse(St_name %in% c("MT","ND","SD","WY","NE","CO","KS","NM","TX","OK"), "Plains",
                                ifelse(St_name %in% c("MN","WI","IA","IL","MO","MI","IN","OH","KY"), "Midwest",
                                       ifelse(St_name %in% c("AR","LA","MS","TN","AL","FL","NC", "SC","GA"),
                                              "Southeast", "Northeast")))),
    NAICS = gsub("\\)", "", gsub("NAICS CLASSIFICATION: \\(", "", Class)), 
    NAICS_subset = NAICS %in% c("11191", "11192","11193 & 11194 & 11199"), 
    NAICS_label = recode(NAICS,
                         "1111"="Oilseed and grain farming",
                         "1112"="Vegetable and melon farming",
                         "1113"="Fruit and tree nut farming",
                         "1114"="Greenhouse, nursery, and floriculture production",
                         "1119"="Other crop farming",
                         "11191"="Tobacco farming",
                         "11192"="Cotton farming",
                         "11193 & 11194 & 11199"= "Sugarcane farming, hay farming, and all other crop farming",
                         "112111"="Beef cattle ranching and farming",
                         "112112"="Cattle feedlots",
                         "11212"="Dairy cattle and milk production",
                         "1122"="Hog and pig farming",
                         "1123"="Poultry and egg production",
                         "1124"="Sheep and goat farming",
                         "1125 & 1129"="Animal aquaculture and other animal production"),
    NAICS_cat = ifelse(NAICS_subset, NA, 
                       ifelse(NAICS %in% c("1111", "1112","1113", "1114", "1119"), "Crop", 
                              ifelse(NAICS %in% c("112111","112112","11212","1122",
                                                  "1123","1124","1125 & 1129"), "Livestock", NA)))
  ) %>% filter(!is.na(NAICS))

save(df_NAICS, file="datasets/df_NAICS.RData")


#  --------------------------- assemble dataset df_inventory_NAICS ---------------------------
library(rnassqs)

NASSQS_TOKEN <- "C9B668A9-3062-3CE5-96B8-2D6D1AC432BF"
nassqs_auth(key = NASSQS_TOKEN)

# download livestock inventories 
base_params_inventory  <- list(
  source_desc = "survey", 
  # reference_period_desc = "year",
  agg_level_desc = c("state", "national"),
  year = 2017
)

param_list_inventory <- list(
  hogs = list(
    commodity_desc = "HOGS",
    short_desc = "HOGS - INVENTORY, MEASURED IN $"),
  cattle = list(
    commodity_desc = "CATTLE",
    short_desc = "CATTLE, INCL CALVES - INVENTORY, MEASURED IN $"),
  cattle_milk = list(
    commodity_desc = "CATTLE",
    short_desc = "CATTLE, COWS, MILK - INVENTORY"),
  cattle_beef = list(
    commodity_desc = "CATTLE",
    short_desc = "CATTLE, COWS, BEEF - INVENTORY"),
  # chickens - broiler and turkey inventory values are added below
  chickens = list(
    commodity_desc = "CHICKENS",
    short_desc = "CHICKENS, (EXCL BROILERS) - INVENTORY, MEASURED IN $"),
  broiler_price = list(
    commodity_desc = "chickens",
    short_desc = "CHICKENS, BROILERS - PRICE RECEIVED, MEASURED IN $ / LB"), 
  broiler_production_head = list(
    commodity_desc = "chickens",
    short_desc = "chickens, broilers - PRODUCTION, MEASURED IN head"),
  broiler_production_lb = list(
    commodity_desc = "chickens", 
    short_desc = "CHICKENS, BROILERS - PRODUCTION, MEASURED IN lb"),
  turkey_price = list(
    commodity_desc = "turkeys", 
    short_desc = "turkeys - PRICE RECEIVED, MEASURED IN $ / lb"), 
  turkey_production_head = list(
    commodity_desc = "turkeys", 
    short_desc = "TURKEYS - PRODUCTION, MEASURED IN HEAD"),
  turkey_production_lb = list(
    commodity_desc = "turkeys", 
    short_desc = "TURKEYS - PRODUCTION, MEASURED IN LB")
)

data_list_inventory <- lapply(param_list_inventory , function(var_params) {
  # Create the new parameter list and append the query items that vary
  # by query
  params <- base_params_inventory 
  for(n in names(var_params)) { 
    params[[n]] <- var_params[[n]]
  }
  nassqs(params)
}) %>%  bind_rows() %>% 
  select(sector_desc, short_desc, 
         state_alpha, state_fips_code, state_ansi, 
         # county_code, county_ansi, county_name,
         year, reference_period_desc, commodity_desc, Value
         # location_desc, asd_code, asd_desc 
         ) %>%
  mutate(Value = gsub(",","", Value) %>% as.numeric())


# construct chicken and turkey price per head 
chicken_price <- data_list_inventory %>% 
  filter(short_desc == "CHICKENS, BROILERS - PRICE RECEIVED, MEASURED IN $ / LB",
         reference_period_desc == "YEAR") %>% 
  mutate( price_lb = Value) 

turkey_price <- data_list_inventory %>%
  filter(short_desc == "TURKEYS - PRICE RECEIVED, MEASURED IN $ / LB",
         reference_period_desc == "YEAR") %>% 
  mutate( price_lb = Value) 

chicken_heads <- data_list_inventory %>%
  filter(short_desc=="CHICKENS, BROILERS - PRODUCTION, MEASURED IN HEAD", 
         reference_period_desc == "YEAR") %>% 
  mutate( heads = Value) %>% select(state_alpha, year, heads)

chicken_lbs <- data_list_inventory %>%
  filter(short_desc=="CHICKENS, BROILERS - PRODUCTION, MEASURED IN LB", 
         reference_period_desc == "YEAR", state_alpha=="US")  %>% 
  mutate( lbs = Value) %>% select(state_alpha, year, lbs)

turkey_heads <- data_list_inventory %>%
  filter(short_desc=="TURKEYS - PRODUCTION, MEASURED IN HEAD", 
         reference_period_desc == "YEAR")  %>% 
  mutate( heads = Value) %>% select(state_alpha, year, heads)

turkey_lbs <- data_list_inventory %>%
  filter(short_desc=="TURKEYS - PRODUCTION, MEASURED IN LB", 
         reference_period_desc == "YEAR", state_alpha=="US")  %>% 
  mutate( lbs = Value) %>% select(state_alpha, year, lbs)

chicken_price_head  <- chicken_price %>% 
  left_join(chicken_heads %>% filter(state_alpha=="US")) %>% 
  left_join(chicken_lbs) %>%
  mutate( lbs_head = lbs/heads,
          price_head = price_lb * lbs_head,
          price_desc = "CHICKENS, BROILER - PRICE RECEIVED, MEASURED IN $ / HEAD")

turkey_price_head  <- turkey_price %>% 
  left_join(turkey_heads %>% filter(state_alpha=="US")) %>%
  left_join(turkey_lbs) %>%
    mutate( lbs_head = lbs/heads,
            price_head = price_lb * lbs_head,
            price_desc = "TURKEYS - PRICE RECEIVED, MEASURED IN $ / HEAD")

# download inventory broilers and turkeys from ag census 
inventory_broilers <- nassqs(
  list(
    source_desc = "census", 
    agg_level_desc = "state", 
    commodity_desc = c("CHICKENS","TURKEYS"),
    domaincat_desc= "NOT SPECIFIED",
    short_desc = c("CHICKENS, BROILERS - INVENTORY", 
                   "TURKEYS - INVENTORY"),
    year = 2017)) %>%
  select(sector_desc, short_desc, 
         state_alpha, state_fips_code, state_ansi, 
         year, commodity_desc, Value ) %>% 
  mutate(Value = gsub(",","", Value) %>% as.numeric())

# function to replace NA with 0 
na_to_zero <- function(var) ifelse(is.na(var), 0, var)

# create variables containing values of poultry inventory 
inventory_broilers_wide <- inventory_broilers %>% 
  select(state_alpha, commodity_desc, Value) %>%
  pivot_wider(names_from = commodity_desc, values_from = Value) %>%
  mutate(CHICKENS = na_to_zero(CHICKENS),
         TURKEYS = na_to_zero(TURKEYS))

birds_inventory <- inventory_broilers_wide %>% 
  mutate(chicken_price_head = chicken_price_head$price_head,
         turkey_price_head = turkey_price_head$price_head,
         broiler_inventory = CHICKENS * chicken_price_head,
         turkey_inventory = TURKEYS * turkey_price_head)
  
inventory_poultry <- 
  data_list_inventory %>%
  filter(short_desc=="CHICKENS, (EXCL BROILERS) - INVENTORY, MEASURED IN $", 
         state_alpha!="US", state_alpha!="OT") %>% 
  mutate(Value_nonbroiler_chickens = Value) %>% select(-Value) %>%
  full_join(birds_inventory %>% select(state_alpha, broiler_inventory, turkey_inventory)) %>%
  mutate(
    Value_nonbroiler_chickens = na_to_zero(Value_nonbroiler_chickens),
    broiler_inventory = na_to_zero( broiler_inventory), 
    turkey_inventory = na_to_zero(turkey_inventory), 
    Value = Value_nonbroiler_chickens +  broiler_inventory +  turkey_inventory,
    commodity_desc = "POULTRY") %>%
  select(state_alpha, commodity_desc, Value) 

# add $3.5 / chicken equiv. facility asset for processing, hatchery, and feed mill 
# https://www.acppubs.com/articles/7398-costco-poultry-processing-plant-to-boost-nebraska-economy
asset_plant_chicken <- 3.5
asset_plant_turkey <- 3.5 * (turkey_price_head$lbs_head/chicken_price_head$lbs_head) 

inventory_poultry2 <- inventory_poultry %>% 
  left_join(chicken_heads %>% select(state_alpha, heads)) %>%
  mutate(heads_chicken = heads) %>% select(-heads) %>%
  left_join(turkey_heads %>% select(state_alpha, heads)) %>%
  mutate(
    heads_chicken = na_to_zero(heads_chicken),
    heads = na_to_zero(heads), 
    plant_asset = asset_plant_chicken * heads_chicken + asset_plant_turkey * heads)

# add value of live bird inventory and value of plant assets 
inventory_poultry3 <- inventory_poultry2 %>%
  mutate(Value = Value + plant_asset) %>% 
  select(state_alpha, commodity_desc, Value)
  

# split the inventory value of cattle into milk cows, beef-ranch, and beef-feedlot
inventory_beef_cows <- data_list_inventory %>%
  filter(short_desc == "CATTLE, COWS, BEEF - INVENTORY") %>%
  mutate(beef_cows = Value) %>%
  select(state_alpha,  beef_cows)
  
inventory_milk_cows <- data_list_inventory %>%
  filter(short_desc == "CATTLE, COWS, MILK - INVENTORY") %>%
  mutate(milk_cows = Value) %>%
  select(state_alpha,  milk_cows)

inventory_cow_value <- data_list_inventory %>%
  filter(short_desc ==  "CATTLE, INCL CALVES - INVENTORY, MEASURED IN $") %>%
  mutate(asset_value = Value) %>%
  select(state_alpha,  asset_value)

# split values of CATTLE, INCL CALVES between beef and milk 
inventory_cows <- inventory_milk_cows %>% 
  full_join(inventory_beef_cows, by = "state_alpha") %>%
  full_join(inventory_cow_value, by = "state_alpha") %>%
  mutate(
    beef_milk_cows = beef_cows + milk_cows,
    ratio_milk_cows = milk_cows/beef_milk_cows,
    Value_Beef = (1-ratio_milk_cows) * asset_value,
    Value_Milk = ratio_milk_cows * asset_value
  ) 

# split values of Value_Beed between ranch and feedlot 
load(file="datasets/df_NAICS.RData")
sales_ratio_ranch <- df_NAICS %>% filter(NAICS %in% c("112111", "112112")) %>% select(St_name, NAICS, revenue_sales) %>%
  mutate(NAICS2 = paste0("N_",NAICS)) %>% select(-NAICS) %>%
  pivot_wider(names_from = NAICS2, values_from = revenue_sales) %>%
  mutate(N_112111 = ifelse(is.na(N_112111), 0, N_112111),
         N_112112 = ifelse(is.na(N_112112), 0, N_112112),
         ratio_ranch = N_112111/(N_112111 + N_112112),
         ratio_ranch = ifelse(is.na(ratio_ranch), 0, ratio_ranch))  

inventory_cows2 <- inventory_cows %>%
  left_join(sales_ratio_ranch %>% select(St_name, ratio_ranch), 
            by = c("state_alpha"="St_name")) %>%
  mutate(
    Value_Ranch = ratio_ranch * Value_Beef,
    Value_Feedlot = (1-ratio_ranch) * Value_Beef
  )

inventory_cows_long <- inventory_cows2 %>%
  select(state_alpha, Value_Ranch, Value_Feedlot, Value_Milk) %>%
  pivot_longer(cols = -state_alpha, names_to =  "commodity_desc", values_to = "Value")


# put the poultry and cattle inventory together with hogs
df_inventory_value <- data_list_inventory %>%
  filter(commodity_desc == "HOGS") %>%
  bind_rows(inventory_poultry3) %>%
  bind_rows(inventory_cows_long ) %>%
  mutate(NAICS = recode(commodity_desc,
                        "POULTRY" = "1123",
                        "HOGS" ="1122",
                        "Value_Ranch" = "112111",
                        "Value_Feedlot" = "112112",
                        "Value_Milk" = "11212")) 

df_inventory_value %>% group_by(commodity_desc) %>% summarise(sum(Value, na.rm=T))
df_inventory_value %>% group_by(NAICS) %>% summarise(sum(Value, na.rm=T))

df_inventory_NAICS <- df_inventory_value %>%
  group_by(state_alpha, NAICS) %>%
  summarise(inventory_value = sum(Value, na.rm=TRUE))

df_inventory_NAICS %>% arrange(desc(inventory_value))

df_inventory_NAICS %>% group_by(NAICS) %>% summarise(sum(inventory_value))

save(df_inventory_NAICS, file="datasets/df_inventory_NAICS.RData")


# check asset and profitability of poultry sector in the past
asset_profit_poultry <- nassqs(list(
  source_desc = "census", 
  agg_level_desc = "national", 
  domaincat_desc= "NAICS CLASSIFICATION: (1123)",
  short_desc = c("AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $", 
                 "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $",
                 "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $"),
  year = c(1992,1997,2002, 2007, 2012, 2017))) %>%
  select(sector_desc, short_desc, 
             state_alpha, state_fips_code, state_ansi, 
             year, commodity_desc, Value) %>% 
  mutate(Value = gsub(",","", Value) %>% as.numeric())

asset_profit_poultry



#  --------------------------- assemble dataset df_NAICS_simple ---------------------------
load(file="datasets/df_NAICS.RData")
load(file="datasets/df_inventory_NAICS.RData")

# merge two datasets and create additional variables 
df_NAICS_2 <- df_NAICS %>%
  filter(!is.na(NAICS_subset)) %>%
  left_join(df_inventory_NAICS, by = c("St_name"="state_alpha", "NAICS")) %>%
  mutate(
    NAICS_simple = recode(NAICS, 
                          '1111'='Grain',                 
                          '1112'='Vegetable',
                          '1113'='Fruit and nut',
                          '1114'='Other_crop',
                          '1119'='Other_crop',
                          '11191'=NULL, 
                          '11192'=NULL, 
                          '11193 & 11194 & 11199' =NULL, 
                          '112111'='Cattle ranch',
                          '112112'='Cattle feedlot',
                          '11212'='Dairy',
                          '1122'='Hog',
                          '1123'='Poultry and egg',
                          '1124'='Other_animal',
                          '1125 & 1129'='Other_animal'),
    inventory_value = ifelse(is.na(inventory_value), 0, inventory_value),
    asset_value_total =  asset_value_total + inventory_value
  ) 

df_NAICS_2  %>% arrange(desc(asset_value_total)) %>% select(St_name, NAICS_simple, asset_value_total)
df_NAICS_2  %>% group_by(NAICS) %>% 
  summarise(asset_value_total =sum(asset_value_total, na.rm=T),
            net_income_total = sum(net_income_total, na.rm=T))


# aggregate some species into a state-NAICS level dataset 
df_NAICS_simple <- df_NAICS_2 %>% 
  group_by(St_name, USDA_region, NAICS_cat, NAICS_simple) %>%
  summarise(
    revenue_sales = sum(revenue_sales, na.rm=T)/10^9,
    asset_value_total = sum(asset_value_total, na.rm=T)/10^9,
    net_income_total = sum(net_income_total, na.rm=T)/10^9,
    interest_expense = sum(interest_expense, na.rm=T)/10^9,
    hired_labor_workers = sum(hired_labor_workers, na.rm=T),
    unpaid_labor_workers = sum(unpaid_labor_workers, na.rm=T)
  ) %>%
  mutate(return_on_asset = net_income_total/asset_value_total,
         return_on_asset2 = ifelse(asset_value_total==0, 0,
                                   (net_income_total + interest_expense +
                               - unpaid_labor_workers * (45000 * 0.42 + 20000 * 0.58) /10^9)/asset_value_total),
         hired_to_unpaid = ifelse(unpaid_labor_workers==0, 0, hired_labor_workers/unpaid_labor_workers),
         asset_per_unpaid = ifelse(unpaid_labor_workers==0, 0, asset_value_total * 10^3/unpaid_labor_workers),
         debt_at_5pct = interest_expense/.05,
         debt_to_asset_ratio = ifelse(asset_value_total==0, 0, debt_at_5pct/asset_value_total)) %>%
  filter(!is.na(NAICS_cat), !is.na(NAICS_simple))

df_NAICS_simple %>% with(table(NAICS_simple, NAICS_cat))


save(df_NAICS_simple, file="datasets/df_NAICS_simple.RData")


# plot data: worker vs asset per operator 
df_NAICS_simple %>% filter(NAICS_cat=="Crop", revenue_sales > .01) %>%
  ggplot(aes(x = hired_to_unpaid, y = asset_per_unpaid, 
             color=NAICS_simple, 
             size = revenue_sales)) + geom_point(alpha=.5) +
  geom_label_repel(aes(label = St_name), size=3, show.legend = FALSE,  
                   data = df_NAICS_simple %>%
                     filter(NAICS_cat=="Crop", revenue_sales > .01) %>%
                     filter(hired_to_unpaid > 8 | asset_per_unpaid >5 | revenue_sales >10)) +
  guides(color =guide_legend(title = "Industry Category"),
         size = guide_legend(title = "Revenue, $ billion")) +
  labs(x = "Hired worker per operator, persons", 
       y = "Agricultural asset per operator, $ million",
       caption = paste(
         "Data Source: US Census of Agriculture, 2017."))
ggsave(file = "worker_vs_asset_crop.png")


df_NAICS_simple %>% filter(NAICS_cat=="Livestock", revenue_sales > .01) %>%
  ggplot(aes(x = hired_to_unpaid, y = asset_per_unpaid, 
             color=NAICS_simple, 
             size = revenue_sales)) + geom_point(alpha=.5) +
  geom_label_repel(aes(label = St_name), size=3, show.legend = FALSE,  
                   data = df_NAICS_simple %>%
                     filter(NAICS_cat=="Livestock", revenue_sales > .01) %>%
                     filter(hired_to_unpaid > 10 | asset_per_unpaid > 5 | revenue_sales >10)) +
  guides(color =guide_legend(title = "Industry Category"),
         size = guide_legend(title = "Revenue, $ billion")) +
  labs(x = "Hired worker per operator, persons", 
       y = "Agricultural asset per operator, $ million",
       caption = paste(
         "Data Source: US Census of Agriculture & NASS Survey, 2017."))
ggsave(file = "worker_vs_asset_livestock.png")


# plot data: debt vs return per asset 
df_NAICS_simple %>% filter(NAICS_cat=="Crop", revenue_sales > .01) %>%
  ggplot(aes(x = debt_to_asset_ratio, y = return_on_asset, 
             color=NAICS_simple, 
             size = revenue_sales)) + geom_point(alpha=.5) +
  geom_label_repel(aes(label = St_name), size=3, show.legend = FALSE,  
                   data = df_NAICS_simple %>%
                     filter(NAICS_cat=="Crop", revenue_sales > .01) %>%
                     filter(debt_to_asset_ratio > .15 | return_on_asset > .08 | revenue_sales >10))  +
  guides(color =guide_legend(title = "Industry Category"),
         size = guide_legend(title = "Revenue, $ billion")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Debt to asset ratio", 
       y = "Return on asset",
       caption = paste(
         "Data Source: US Census of Agriculture, 2017."))
ggsave(file = "debt_vs_return_crop.png")



df_NAICS_simple %>% filter(NAICS_cat=="Livestock", revenue_sales > .01, 
                           return_on_asset < .4) %>% # omit outliers over 40% ROA
  ggplot(aes(x = debt_to_asset_ratio, y = return_on_asset, 
             color=NAICS_simple, 
             size = revenue_sales)) + geom_point(alpha=.5) +
  geom_label_repel(aes(label = St_name), size=3, show.legend = FALSE,  
                   data = df_NAICS_simple %>%
                     filter(NAICS_cat=="Livestock", revenue_sales > .01) %>%
                     filter(debt_to_asset_ratio > .25 | return_on_asset > .25 |
                              revenue_sales >5, return_on_asset < .4)) +
  guides(color =guide_legend(title = "Industry Category"),
         size = guide_legend(title = "Revenue, $ billion")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Debt to asset ratio", 
       y = "Return on asset",
       caption = paste(
         "Data Source: US Census of Agriculture & NASS Survey, 2017."))
ggsave(file = "debt_vs_return_livestock.png")



# ----------------------------------------------------------------------------------------------
# farm counts by size and industry sankey diagram 
# ----------------------------------------------------------------------------------------------

us17 %>% filter(census_table==48) %>%
  select(Item) %>% unique() %>% print(n=1000)

us17 %>% filter(census_table==48) %>%
  select(Class) %>% unique() %>% print(n=1000)

us17 %>% filter(census_table==48) %>% with(table(Commodity))

# farms by NAICS and sales class 
us17 %>% filter(census_table==72) %>%
  select(Item, Class) %>% unique() %>% 
  filter(grepl("NAICS", Class)) %>% print(n=1000)


# farms by NAICS and farm area etc.
us17 %>% filter(census_table==75) %>%   
  filter(grepl("FARM OPERATIONS", Item), 
         grepl("NAICS", Class)) %>% 
  select(Item, Class) %>% unique() %>% print(n=200) 

us17 %>% filter(census_table==73) %>%   
  filter(#grepl("FARM OPERATIONS", Item), 
         grepl("NAICS", Class)) %>% 
  select(Item, Class) %>% unique() %>% print(n=200) 



params1 <- list(source_desc = "census", commodity_desc = "FARM OPERATIONS",
                # domain_desc = "NAICS CLASSIFICATION AND ECONOMIC CLASS",
                domain_desc = "ECONOMIC CLASS AND NAICS CLASSIFICATION",
                short_desc = c("FARM OPERATIONS - NUMBER OF OPERATIONS"),
                agg_level_desc = "national", 
                # state_fips_code__NE = 99,
                year = 2017)
d1 <- nassqs(params1) 

farms_industry <- d1 %>% select(domaincat_desc, Value) 

farms_industry <- farms_industry %>%
  mutate(
    Class = str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..\\d+.\\d+.[A-Z]+.\\d+.\\d+..."), 
    Class = ifelse(!is.na(Class), Class,
                   str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..\\d+.\\d+.\\d+.[A-Z]+.[A-Z]+...")
                   ), # for "ECONOMIC CLASS: (1,000,000 OR MORE $)"
    Class = ifelse(!is.na(Class), Class,
                    str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..[A-Z]+.[A-Z]+.\\d+.\\d+...")
    ), # for "ECONOMIC CLASS: (LESS THAN 1,000 $)"
    Class_min = gsub(" OR", "", gsub(" TO","", str_extract(
      gsub(",","", domaincat_desc), "\\d+.[:alpha:]{2}"))) %>% as.numeric(),
    Class_min = ifelse(is.na(Class_min), 0, Class_min),
    NAICS = gsub("\\(","", gsub("\\)","",
                                str_extract(domaincat_desc, "\\([:digit:]{4,}\\)"))),
    NAICS = ifelse(!is.na(NAICS), NAICS, 
                   gsub("\\(","", gsub("\\)","",
                                       str_extract(domaincat_desc, "\\([:digit:]{4,}...[:digit:]{4,}\\)")))),
    NAICS = ifelse(!is.na(NAICS), NAICS, 
                   str_extract(domaincat_desc, "[:digit:]{4,}...[:digit:]{4,}...[:digit:]{4,}")),
    Value = gsub(",","", Value) %>% as.integer()
  ) %>% filter(!is.na(Class), !is.na(NAICS))

farm_economic_class <- farms_industry %>% arrange(Class_min) %>% select(Class,Class_min) %>% unique() 
farm_economic_class

farms_industry$Class %>% table()
farms_industry$NAICS %>% table()


farms_industry_simple <- farms_industry %>%
  mutate(
    NAICS_simple = recode(NAICS, 
                          '1111'='Grain',                 
                          '1112'='Vegetable',
                          '1113'='Fruit and nut',
                          '1114'='Other_crop',
                          '1119'='Other_crop',
                          '11191'=NULL, 
                          '11192'=NULL, 
                          '11193' =NULL, 
                          '11194' =NULL, 
                          '11199' =NULL, 
                          '11193 & 11194 & 11199' =NULL, 
                          '112111'='Beef Cattle',
                          '112112'='Beef Cattle',
                          '11212'='Dairy',
                          '1122'='Hog',
                          '1123'='Poultry and egg',
                          '1124'='Other_animal',
                          '1125 & 1129'='Other_animal',
                          '1125'=NULL,
                          '1129'=NULL
                          )) %>%
  filter(!is.na(NAICS_simple)) %>%
  mutate(class_cat = cut(Class_min + 1, breaks = c(-Inf, 10^4, 10^5, 10^6, Inf),
                         labels = c("Farm Sales < $10K", 
                                    "Farm Sales $10K-$100K",
                                    "Farm Sales $100K-$1M", 
                                    "Farm Sales > $1M"))) %>%
  group_by(NAICS_simple, class_cat) %>%
  summarise(Value = sum(Value, na.rm=T)) %>% 
  group_by(class_cat) %>%
  mutate(Value_total = sum(Value, na.rm=T), 
         percent = round(Value/Value_total, 2)) %>% 
  arrange(class_cat, NAICS_simple)

farms_industry_simple




farms_by_sales <- us17 %>% filter(census_table==72) %>%
  filter(grepl("NAICS", Class)) %>% 
  select(Item, Class, Value)

farms_by_sales <- farms_by_sales %>%
  mutate(
    Class2 = str_extract(Class, "[A-Z]+.[A-Z]+:..\\d+.\\d+.[A-Z]+.\\d+.\\d+..."), 
    Class2 = ifelse(!is.na(Class2), Class2,
                   str_extract(Class, "[A-Z]+.[A-Z]+:..\\d+.\\d+.\\d+.[A-Z]+.[A-Z]+...")
    ), # for "ECONOMIC CLASS: (1,000,000 OR MORE $)"
    Class2 = ifelse(!is.na(Class2), Class2,
                    str_extract(Class, "[A-Z]+.[A-Z]+:..[A-Z]+.[A-Z]+.\\d+.\\d+...")
    ), # for "(LESS THAN 1,000 $)"
    Class_min = gsub(" OR", "", gsub(" TO","", str_extract(
      gsub(",","", Class), "\\d+.[:alpha:]{2}"))) %>% as.numeric(),
    Class_min = ifelse(is.na(Class_min), 0, Class_min),
    NAICS = gsub("\\(","", gsub("\\)","",
                                str_extract(Class, "\\([:digit:]{4,}\\)"))),
    NAICS = ifelse(!is.na(NAICS), NAICS, 
                   gsub("\\(","", gsub("\\)","",
                                       str_extract(Class, "\\([:digit:]{4,}...[:digit:]{4,}\\)")))),
    NAICS = ifelse(!is.na(NAICS), NAICS, 
                   str_extract(Class, "[:digit:]{4,}...[:digit:]{4,}...[:digit:]{4,}")),
    Value = gsub(",","", Value) %>% as.integer()
  ) %>% filter(!is.na(Class2), !is.na(NAICS))

farms_by_sales_class <- farms_by_sales %>% arrange(Class_min) %>% select(Class2, Class_min) %>% unique() 
farms_by_sales_class

farms_by_sales %>% with(table(Class2))
farms_by_sales %>% with(table(NAICS))


farms_by_sales_simple <- farms_by_sales %>%
  mutate(
    NAICS_simple = recode(NAICS, 
                          '1111'='Grain',                 
                          '1112'='Vegetable',
                          '1113'='Fruit and nut',
                          '1114'='Other_crop',
                          '1119'='Other_crop',
                          '11191'=NULL, 
                          '11192'=NULL, 
                          '11193' =NULL, 
                          '11194' =NULL, 
                          '11199' =NULL, 
                          '11193 & 11194 & 11199' =NULL, 
                          '112111'='Beef Cattle',
                          '112112'='Beef Cattle',
                          '11212'='Dairy',
                          '1122'='Hog',
                          '1123'='Poultry and egg',
                          '1124'='Other_animal',
                          '1125 & 1129'='Other_animal',
                          '1125'=NULL,
                          '1129'=NULL
    )) %>%
  filter(!is.na(NAICS_simple)) %>%
  mutate(class_cat = cut(Class_min + 1, breaks = c(-Inf, 10^4, 10^5, 10^6, Inf),
                         labels = c("Farm Sales < $10K", 
                                    "Farm Sales $10K-$100K",
                                    "Farm Sales $100K-$1M", 
                                    "Farm Sales > $1M"))) %>%
  group_by(NAICS_simple, class_cat) %>%
  summarise(Value = sum(Value, na.rm=T)) %>% 
  group_by(class_cat) %>%
  mutate(Value_total = sum(Value, na.rm=T), 
         percent = round(Value/Value_total, 2)) %>% 
  arrange(class_cat, NAICS_simple)


# Economic class and farm sales class are very similar
farms_industry_simple$Value %>% sum()
farms_by_sales_simple$Value %>% sum()
farms_industry_simple
farms_by_sales_simple



# Sankey diagram 
library(flipPlots)

color5_scheme <- c("#480D2D", "#BC1B3A", "#E9A101", "#F8D800", "#88B60B")
color9_farms <- color5_scheme[c(3,3,5,5,3,3,5,3,5)]
color4_size <-  c("#8F706D", "#DA6441", "#F0C53F", "#B5D0DD")

farms_by_sales_sankey <- farms_by_sales_simple %>% ungroup() %>%
  mutate(v1= paste('Farms', sum(Value,na.rm = T) %>% comma())) %>%
  arrange(NAICS_simple)

SankeyDiagram(
  farms_by_sales_sankey %>% select(v1, class_cat, NAICS_simple),
  link.color = "Source", #  "Target", # 
  weights = farms_by_sales_sankey$Value,
  max.categories = 18,
  colors =  c("#8F706D", color4_size, color9_farms), 
  font.size = 18, 
  sinks.right = FALSE,
  label.show.varname = FALSE,
  label.show.percentages=TRUE)  



# ----- sales value sankey diagram -------------
nassqs_param_values("commodity_desc")

tmp <- nassqs_param_values("domain_desc")
tmp[grepl("NAICS",tmp)]

params2 <- list(source_desc = "census", commodity_desc = "TOBACCO", #"GRAIN",
                domain_desc_LIKE = "FARM SALES", 
                # short_desc = c("GRAIN - SALES, MEASURED IN $"),
                agg_level_desc = "national", 
                # state_fips_code__NE = 99,
                year = 2017)
d2 <- nassqs(params2) 

tmp2 <- d2 %>% select(commodity_desc, short_desc, domaincat_desc, Value) 
tmp3 <- tmp2 %>% filter(grepl("FARM SALES:",domaincat_desc),
                !grepl("50,000 OR MORE",domaincat_desc))
tmp3 %>% select(short_desc, domaincat_desc)



base_params_sales  <- list(
  source_desc = "census", 
  # commodity_desc = "GRAIN",
  domain_desc_LIKE = "FARM SALES", 
  # short_desc = c("GRAIN - SALES, MEASURED IN $"),
  agg_level_desc = "national", 
  # state_fips_code__NE = 99,
  year = 2017
  )

param_list_sales <- list(
  grain = list(
    commodity_desc = "GRAIN",
    short_desc = "GRAIN - SALES, MEASURED IN $"),
  vegetable = list(
    commodity_desc = "VEGETABLE TOTALS",
    short_desc = "VEGETABLE TOTALS, INCL SEEDS & TRANSPLANTS, IN THE OPEN - SALES, MEASURED IN $"),
  fruit = list(
    commodity_desc = "FRUIT & TREE NUT TOTALS",
    short_desc = "FRUIT & TREE NUT TOTALS - SALES, MEASURED IN $"),
  # tobacco = list(
  #   commodity_desc = "TOBACCO",
  #   short_desc = "TOBACCO - SALES, MEASURED IN $"),
  cotton = list(
    commodity_desc = "COTTON",
    short_desc = "COTTON, LINT & SEED - SALES, MEASURED IN $"),
  horticulture = list(
    commodity_desc = "HORTICULTURE TOTALS",
    short_desc = "HORTICULTURE TOTALS, (EXCL CUT TREES & VEGETABLE SEEDS & TRANSPLANTS) - SALES, MEASURED IN $"),
  # crop_total = list(
  #   commodity_desc = "CROP TOTALS",
  #   short_desc = "CROP TOTALS - SALES, MEASURED IN $"),
  poultry = list(
    commodity_desc = "POULTRY TOTALS",
    short_desc = "POULTRY TOTALS, INCL EGGS - SALES, MEASURED IN $"),
  hog = list(
    commodity_desc = "HOGS",
    short_desc = "HOGS - SALES, MEASURED IN $"),
  cattle = list(
    commodity_desc = "CATTLE",
    short_desc = "cattle, incl calves - SALES, MEASURED IN $"),
  milk = list(
    commodity_desc = "MILK",
    short_desc = "MILK - SALES, MEASURED IN $"),
  commodity_total = list(
    commodity_desc = "COMMODITY TOTALS",
    short_desc = "COMMODITY TOTALS - SALES, MEASURED IN $")
)


data_list_sales <- lapply(param_list_sales , function(var_params) {
  # Create the new parameter list and append the query items that vary
  # by query
  params <- base_params_sales 
  for(n in names(var_params)) { 
    params[[n]] <- var_params[[n]]
  }
  nassqs(params)
}) %>%  bind_rows() %>% 
  # select( commodity_desc, short_desc, domaincat_desc, Value ) %>%
  mutate(Value = gsub(",","", Value) %>% as.numeric())

df_sales <- data_list_sales %>% filter(grepl("FARM SALES:",domaincat_desc),
                   !grepl("50,000 OR MORE",domaincat_desc))

sales_commodity_total <- df_sales %>% filter(commodity_desc== "COMMODITY TOTALS") %>%
  select( commodity_desc, short_desc, domaincat_desc, Value )

df_sales2 <- df_sales %>% filter(commodity_desc != "COMMODITY TOTALS") %>%
  select( commodity_desc, short_desc, domaincat_desc, Value ) 


df_sales2 <- df_sales2 %>%
  bind_rows(sales_commodity_total %>% filter(domaincat_desc %in% df_sales2$domaincat_desc)) %>%
  mutate(
    Class = str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..\\d+.\\d+.[A-Z]+.\\d+.\\d+..."), 
    Class = ifelse(!is.na(Class), Class,
                   str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..\\d+.\\d+.\\d+.[A-Z]+.[A-Z]+...")
    ), # for "ECONOMIC CLASS: (1,000,000 OR MORE $)"
    Class = ifelse(!is.na(Class), Class,
                   str_extract(domaincat_desc, "[A-Z]+.[A-Z]+:..[A-Z]+.[A-Z]+.\\d+.\\d+...")
    ), # for "ECONOMIC CLASS: (LESS THAN 1,000 $)"
    Class_min = gsub(" OR", "", gsub(" TO","", str_extract(
      gsub(",","", domaincat_desc), "\\d+.[:alpha:]{2}"))) %>% as.numeric(),
    Class_min = ifelse(is.na(Class_min), 0, Class_min)
  ) %>% filter(!is.na(Class))

df_sales_class <- df_sales2 %>% arrange(Class_min) %>% select(Class,Class_min) %>% unique() 
df_sales_class


df_sales2_simple <- df_sales2 %>%
  mutate(
    commodity = recode(commodity_desc, 
                       'GRAIN'='Grain',                 
                       'VEGETABLE TOTALS'='Vegetable',
                       'FRUIT & TREE NUT TOTALS'='Fruit and nut',
                       'HORTICULTURE TOTALS'='Horticulture',
                       'TOBACCO' = 'Tobacco',
                       'COTTON' = 'Cotton',
                       'CATTLE'='Beef Cattle',
                       'MILK'='Dairy',
                       'HOGS'='Hog',
                       'POULTRY TOTALS'='Poultry and egg',
                       'COMMODITY TOTALS'='Commodity total'
    )) %>%
  filter(!is.na(commodity)) %>%
  mutate(class_cat = cut(Class_min + 1, breaks = c(-Inf, 10^4, 10^5, 10^6, Inf),
                         labels = c("Farm Sales < $10K", 
                                    "Farm Sales $10K-$100K",
                                    "Farm Sales $100K-$1M", 
                                    "Farm Sales > $1M"))) %>%
  group_by(commodity, class_cat) %>%
  summarise(Value = sum(Value, na.rm=T)) %>% 
  arrange(class_cat, commodity)

tmp1 <- df_sales2_simple %>% filter(commodity=='Commodity total') 
tmp2 <- df_sales2_simple %>% filter(commodity!='Commodity total') %>% group_by(class_cat) %>% summarise(Sum = sum(Value))

tmp_sales_other <- left_join(tmp1, tmp2, by = "class_cat") %>% mutate(Diff = Value - Sum) %>% ungroup()
  

df_sales2_simple <- df_sales2_simple %>% filter(commodity!='Commodity total') %>%
  bind_rows(tmp_sales_other %>%
              mutate(commodity = "Other", Value = Diff) %>% 
              select(commodity, class_cat, Value))

df_sales2_simple %>% print(n=50)


color5_scheme <- c("#480D2D", "#BC1B3A", "#E9A101", "#F8D800", "#88B60B")
color10_sales <- color5_scheme[c(3,5,3,5,5,3,5,2,3,5)]
color11_sales <- color5_scheme[c(3,5,3,5,5,3,5,2,3,5,5)]
color4_size <-  c("#8F706D", "#DA6441", "#F0C53F", "#B5D0DD")

df_sales_sankey <- df_sales2_simple %>% ungroup() %>%
  mutate(v1= paste('Sales, M$:', (sum(Value, na.rm = T)/10^6) %>% comma()))%>%
  arrange(commodity)

SankeyDiagram(
  df_sales_sankey %>% select(v1, class_cat, commodity),
  link.color = "Source", #  "Target", # 
  weights = df_sales_sankey$Value,
  max.categories = 18,
  colors =  c("#B5D0DD", color4_size, color10_sales), 
  font.size = 18, 
  sinks.right = FALSE,
  label.show.varname = FALSE,
  label.show.percentages=TRUE)  



# ----------------------------------------------------------------------------------------------
# dygraphs example 
# ----------------------------------------------------------------------------------------------


library(dygraphs)
library(rnassqs)
library(lubridate)

milk_price <- nassqs(list(
  source_desc = "survey", 
  agg_level_desc = "state", 
  short_desc = c("milk - price received, MEASURED IN $ / cwt"),
  year = c(1990:2019))) %>%
  select(sector_desc, short_desc, 
         state_alpha, state_fips_code, state_ansi, 
         year, freq_desc, reference_period_desc, group_desc, commodity_desc, Value) %>% 
  mutate(Value = gsub(",","", Value) %>% as.numeric())

milk_price %>% head()

ts_milk_price <- milk_price %>% filter(freq_desc == "MONTHLY") %>%
  mutate(month = recode(reference_period_desc, 
                        'JAN'='01', 'FEB'='02', 'MAR'='03', 'APR'='04', 'MAY'='05', 'JUN'='06',
                        'JUL'='07', 'AUG'='08', 'SEP'='09', 'OCT'='10', 'NOV'='11', 'DEC'='12'),
         yr_mo = paste(year, month, '-01', sep='-') %>% ymd) %>%
  select(state_alpha, year, month, yr_mo, Value) %>% arrange(yr_mo) 


WI <- ts_milk_price %>% filter(state_alpha=="WI") %>% 
  select(Value) %>% 
  ts(start=c(1990, 1), end=c(2019, 08), frequency=12) 

PA <- ts_milk_price %>% filter(state_alpha=="PA") %>% 
  select(Value) %>% 
  ts(start=c(1990, 1), end=c(2019, 08), frequency=12)

CA <- ts_milk_price %>% filter(state_alpha=="CA") %>% 
  select(Value) %>% 
  ts(start=c(1990, 1), end=c(2019, 08), frequency=12) 

TX <- ts_milk_price %>% filter(state_alpha=="TX") %>% 
  select(Value) %>% 
  ts(start=c(1990, 1), end=c(2019, 08), frequency=12) 


cbind(CA, PA) %>% dygraph(main = "Monthly Milk Price, $/cwt") %>% dyRangeSelector()
# %>% dyOptions(stackedGraph = TRUE)

save(ts_milk_price, file="ts_milk_price.RData")

