# THIS FILE CONTAINS FUNCTIONS TO:
# - generate a map with US state or county level data in two methods: 
#   - one nethod using geom_polygon()
#   - another using geom_sf() 


# ------------------------------------------------------
# geographic shape data 

counties <- map_data("county") # using ggplot2
states <- map_data("state") # using ggplot2

# ---- county shape map ---------------------------------
# library(tigris)
# library(stringi)
# shape_counties  <- counties(cb=TRUE, resolution = "20m")
# shape_counties$FIPS <- shape_counties$GEOID
# shape_counties$id   <- rownames(shape_counties@data)
# shape_counties@data$NAME <- stri_encode(shape_counties@data$NAME,"","UTF-8")
#
# save(shape_counties,  file = "datasets/shape_counties.RData")

# load(file = "datasets/shape_counties.RData") # obtained using tigris
# df_shape_counties <- tidy(shape_counties) # convert to data frame
# df_shape_counties <- df_shape_counties %>%
#   inner_join(shape_counties@data, by="id") 
# df_shape_counties$region <- df_shape_counties$FIPS
# 
# df_shape_states <- tidy(shape_states) # convert to data frame
# df_shape_states <- df_shape_states %>%
#   inner_join(shape_states@data, by="id") 
# df_shape_states$region <- df_shape_states$FIPS
# 
# save(df_shape_states, df_shape_counties, file = "datasets/df_shape_counties.RData")
# ------------------------------------------------------

load(file = "datasets/df_shape_counties.RData")

# function to clean up the legend labels for ranges e.g., (100, 200] -> 100 to 200
my_ordered_levels <- function(breaks) {
  breaks <- format(breaks, big.mark=",", scientific=FALSE) 
  rlt <- paste(lag(breaks), "to", breaks)[-1] 
  rlt[1] <- paste("<", breaks[2])
  rlt[length(rlt)] <- paste(">", breaks[(length(breaks)-1)])
  rlt
}

# function to convert a continous variable into a categorical variable by given breaks 
my_cat_var  <- function(df, var, breaks, new_varname=NULL) {
  var <- enquo(var)
  if (is.null(new_varname)) new_varname <- paste0("cat_", ensym(var))
  df %>% mutate(!!new_varname :=  cut(!!var, breaks = breaks, ordered_result =TRUE,
                                      labels = my_ordered_levels(breaks)))
}

# function to map discretized/categorical data on county or state shape  
map_county_data_cat <-  function(df, var = cat_Value, map_id = FIPS, 
                                 var_colors = brewer.pal(8, "YlGnBu"), 
                                 level = "county", state_border_color = "white",
                                 legend_varname = NULL, title = NULL, caption = NULL) {
  # object 'states' must be loaded when defining this function
  # objects 'df_shape_counties' and 'df_shape_states' must be loaded 
  
  var_txt <- ifelse(!is.null(legend_varname), legend_varname, as.character(ensym(var))) 
  var <- enquo(var) # see rlang 
  map_id <- enquo(map_id)
  
  if (level == "county") {
    df_shape <- df_shape_counties
  } else {
    df_shape <- df_shape_states
  }
  
  ggplot() +
    geom_map(data = df,  # layer values 
             aes(map_id = !!map_id, fill = !!var),
             map = df_shape) +
    geom_polygon(data = states,  # layer states 
                 # data =df_shape_states,  ## when including Alaska etc. 
                 mapping = aes(x = long, y = lat, group = group),
                 colour =state_border_color, fill = NA) +
    coord_quickmap() +
    theme_void() +
    labs(fill = var_txt, title =  title, caption = caption) + 
    scale_fill_manual(values = var_colors, drop=FALSE) #+
    # theme(legend.position = "bottom") +
    # guides(fill=guide_legend(ncol=4, byrow=TRUE))
}




# # ----- alternatively, using tidycensus -----
# # create dataset geo_us_data used below 
# library(tidycensus)
# library(tidyverse)
# library(viridis)
# library(stringi)
# 
# census_api_key("34bec756dae5c342ae746638d1d45c6b2229af7f")
# options(tigris_use_cache = TRUE)
# 
# 
# us_county_income0 <- get_acs(geography = "county", variables = "B19013_001")
# 
# us_county_income1 <- get_acs(geography = "county", variables = "B19013_001",
#                              shift_geo = FALSE, geometry = TRUE)
# 
# us_county_income2 <- get_acs(geography = "county", variables = "B19013_001",
#                             shift_geo = TRUE, geometry = TRUE)
# 
# ggplot(us_county_income2) + # <- doesn't work with us_county_income1
#   geom_sf(aes(fill = estimate), color = NA) +
#   coord_sf(datum = NA) +
#   theme_minimal() +
#   scale_fill_viridis_c()
# 
# 
# us_county_income2 %>% 
#   # filter(!str_detect(NAME, "Hawaii|Alaska")) %>% 
#   filter(!grepl("Hawaii",NAME), !grepl("Alaska",NAME)) %>%
#   ggplot() +
#   geom_sf(aes(fill = estimate), color = NA) +
#   coord_sf(datum = NA) +
#   theme_minimal() +
#   scale_fill_viridis_c()
# 
# 
# us_state_income2 <- get_acs(geography = "state", variables = "B19013_001",
#                             shift_geo = TRUE, geometry = TRUE)
# 
# geo_state <- us_state_income2 %>% mutate(variable=NULL, estimate=NULL, moe=NULL)
# geo_county <- us_county_income2 %>% mutate(variable=NULL, estimate=NULL, moe=NULL)
# 
# save(geo_state, geo_county, file = "datasets/geo_us_data.RData")



load(file = "datasets/geo_us_data.RData") # using geo_state and geo_county etc. generated above 
# this function uses geo_county and geo_state geometry and geom_sf(.)
map_county_cat_var <-  function(df, var = cat_Value, map_id = FIPS, 
                                lower_48 = FALSE,  
                                 var_colors = NULL, # e.g., brewer.pal(8, "YlGnBu"), 
                                 legend_varname = NULL, title = NULL, caption = NULL) {
  var <- enquo(var)
  var_txt <- ifelse(!is.null(legend_varname), legend_varname, as.character(ensym(var))) 
  map_id <- ensym(map_id) %>% as.character()
  
  if (lower_48) {
    county <- geo_county %>% filter(!grepl("Hawaii",NAME), !grepl("Alaska",NAME))
    state <- geo_state %>% filter(!grepl("Hawaii",NAME), !grepl("Alaska",NAME))
  } else {
    county <- geo_county
    state <- geo_state
  }
  
  map1 <- 
    left_join(county, df,  by = c("GEOID"= map_id)) %>% 
    ggplot() + 
    geom_sf(data = state, color="grey20", fill = NA) + 
    geom_sf(aes(fill = !!var), alpha=.9, na.rm = F, color=NA) +
    geom_sf(data = state, color = "#f4f6f6", size = .2, fill = NA) + 
    coord_sf(datum = NA) + 
    theme_minimal() + 
    scale_fill_viridis_d(direction = -1, option="D", na.value= "#e7e7e7") +
    labs(fill = var_txt, title =  title, caption = caption) 
  
  if (!is.null(var_colors)) {
    map1 + scale_fill_manual(values = var_colors, drop=FALSE)  
  } else {
    map1
  }
}

## practice 
# census_api_key("34bec756dae5c342ae746638d1d45c6b2229af7f")
# us_county_income0 <- get_acs(geography = "county", variables = "B19013_001")
# 
# us_county_income0 <- us_county_income0 %>%
#   my_cat_var(estimate, 
#              breaks = c(0, seq(25000, 120000, 15000), Inf))
# 
# us_county_income0 %>%
#   map_county_cat_var(var = cat_estimate,  lower_48 = FALSE, map_id=GEOID)
# 
# us_county_income0 %>%
#   map_county_cat_var(var = cat_estimate,  lower_48 = TRUE, map_id=GEOID)




# ------------- misc functions ----------------

# wrapper function gsub with x being the first argument  
gsub1 <- function(x, pattern,replacement,...) gsub(pattern,replacement, x,...)

# function to replace NA with 0 
na_to_zero <- function(var) ifelse(is.na(var), 0, var)

