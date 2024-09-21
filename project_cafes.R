# Data wrangling
library(sf)
library(osmdata)
library(tidyverse)
library(OpenStreetMap)
library(janitor)
library(readxl)
library(stringr)
library(geodata)
library(GEOmap)
library('tidygeocoder')

# Visualization
library(tmap)
library(tmaptools)
library(ggmap)
library(grid)
library(ggplot2)
library(rasterVis)
library(corrplot)
library(forcats)

# Machine learning
library(lgr)
library(mlr3)
library(mlr3spatiotempcv)
library(mlr3learners)
library(mlr3viz)
library(remotes)
# remotes::install_github("mlr-org/mlr3extralearners@*release")
library(mlr3extralearners)
library(sperrorest)
library(mlr3tuning)
library(glmnet)
library(spatstat)
library(gstat)
library(terra)
library(geosphere)
library(knitr)
library(stars)
library(hash)

cwd = 'C:/Users/saura/OneDrive/Desktop/DATA-6500/Project'
options(dplyr.summarise.inform = FALSE)
api_secret <- '<API_KEY>'
register_google(key = api_secret)

# defining bounding box of Toronto
toronto_bbox = st_bbox(c(xmin = -79.6392, 
          xmax = -79.115952,
          ymin = 43.403221,
          ymax = 43.855457),
        crs = st_crs(4326)) %>% st_as_sfc()

# importing cafe dataset
cafes_data = read_csv(paste(cwd,'/cafes_old_data.csv',sep='')) |>
  select(name, longitude, latitude, is_closed)
cafes_data = na.omit(cafes_data)

# querying Toronto border
toronto_border = opq(toronto_bbox) |> 
  add_osm_feature(key = "boundary", value = "administrative") |> 
  add_osm_feature(key = 'admin_level', value = '6') |>
  osmdata_sf() |> 
  (\(x) x$osm_multipolygons)() |> 
  filter(name == "Toronto") |> 
  select()

# converting tabular cafe data into sf DataFrame
cafes_data_sf = cafes_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

# subsetting cafes only within Toronto border and removing duplicate rows
cafes_data_sf = cafes_data_sf[toronto_border, , op = st_within]  
cafes_data_sf = cafes_data_sf[!duplicated(cafes_data_sf), ]

# retrieving basemap of Toronto showing streets
toronto_basemap = read_osm(toronto_border, type='apple-iphoto', mergeTiles=TRUE)

# querying Old Toronto/downtown border
toronto_downtown_border = opq(toronto_bbox) |> 
  add_osm_feature(key = "boundary", value = "administrative") |> 
  add_osm_feature(key = 'admin_level', value = '8') |>
  osmdata_sf() |> 
  (\(x) x$osm_multipolygons)() |> 
  filter(name == "Old Toronto") |> 
  select()

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_border)+
  tm_borders(lwd = 2)+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = 'is_closed', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)+
  tm_shape(toronto_downtown_border)+
  tm_borders(col = 'blue', lwd = 2)

# retrieving basemap of Old Toronto showing streets
downtown_basemap = read_osm(toronto_downtown_border, type='apple-iphoto', mergeTiles=TRUE)
downtown_locations = tm_shape(downtown_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_downtown_border)+
  tm_borders(lwd = 2, col = 'blue')+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = 'is_closed', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)

# bounding box for downtown Toronto
area_of_detail = st_bbox(c(xmin = -79.49282, 
                           xmax = -79.27851,
                           ymin = 43.61038,
                           ymax = 43.73602),
                         crs = st_crs(4326)) %>% st_as_sfc()

toronto_border_map = tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_rgb(alpha = 0.5)+
  tm_shape(area_of_detail)+
  tm_borders(lwd = 2)+
  tm_shape(toronto_border)+
  tm_borders(lwd = 2)

downtown_locations
print(toronto_border_map, vp = viewport(0.7, 0.8, width = 0.2, height = 0.2))

# reading the commercial rent per square feet data and converting it to an sf object
rent_per_sqft = read.csv(paste(cwd, '/rent_per_sqft.csv',sep='')) |>
  select(longitude, latitude, rent)
rent_per_sqft = na.omit(rent_per_sqft)
rent_per_sqft_sf = rent_per_sqft %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)
rent_per_sqft_sf = rent_per_sqft_sf[!duplicated(rent_per_sqft_sf),]

# subsetting downtown and outside regions
downtown_rents_sf = rent_per_sqft_sf[toronto_downtown_border,,op=st_within]
outside_rents_sf = rent_per_sqft_sf[toronto_downtown_border,,op=st_disjoint]

mean_rent_downtown = mean(downtown_rents_sf$rent)
mean_rent_outside = mean(outside_rents_sf$rent)

downtown_rents_sf$in_downtown = TRUE
outside_rents_sf$in_downtown = FALSE

rent_downtown_outside_sf <- rbind(downtown_rents_sf, outside_rents_sf)

ggplot(rent_downtown_outside_sf, aes(x=in_downtown, y=rent, fill=in_downtown)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Rent per square feet",x="IN DOWNTOWN?", y="$/sqft")+
  ylim(0, 100)+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))
MWU_rent = wilcox.test(rent ~ in_downtown, data=rent_downtown_outside_sf)
MWU_rent

# loading the population and block area data
population_data = read_csv(paste(cwd,'/2016_92-151_XBB.csv',sep='')) |>
  select(pop = 'DBpop2016/IDpop2016',
         area = 'DBarea2016/IDsup2016',
         latitude = 'DArplat/Adlat', 
         longitude = 'DArplong/ADlong', 
         name = 'ERname/REnom')

# subsetting Toronto and converting it to an sf collection
population_data <- population_data[str_detect(population_data$name, "Toronto"), ]
population_sf = population_data %>% st_as_sf(coords = c("longitude", "latitude"), crs=4326)
toronto_population_sf = population_sf[toronto_border, , op = st_within]

# calculating population density
toronto_population_sf = toronto_population_sf |>
  mutate(pop_density = pop/area)
toronto_population_sf = na.omit(toronto_population_sf)

# projecting point sf population data into crs 6345 
toronto_population_proj =  toronto_population_sf |>
  select(pop) |>
  st_transform(crs = 6345)

# projecting point sf population density data into crs 6345 
toronto_pop_density_proj =  toronto_population_sf |>
  select(pop_density) |>
  st_transform(crs = 6345)

# rasterizing population and pop density
toronto_border_proj = toronto_border |>
  select() |>
  st_transform(crs = 6345) |>
  rast(resolution = 500)

population_raster = rasterize(toronto_population_proj, toronto_border_proj, 
                               field = 'pop', fun = 'sum', na.rm = T)

pop_density_raster = rasterize(toronto_pop_density_proj, toronto_border_proj, 
                              field = 'pop_density', fun = 'mean', na.rm = T)

gplot(pop_density_raster) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(y = 'latitude', x = 'longitude')+
  coord_equal()

# querying subway, street car and bus station locations in Toronto
toronto_stations = opq(toronto_bbox) |> 
  add_osm_feature(key = "operator", value = "Toronto Transit Commission") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  select(name) |>
  drop_na()

toronto_stations = toronto_stations[toronto_border, , op = st_within]

downtown_stations = toronto_stations[toronto_downtown_border, , op = st_within]
tm_shape(downtown_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(downtown_stations)+
  tm_dots(size = 0.10, col = 'blue')

# querying tourist spots in Toronto
toronto_attractions = opq(toronto_bbox) |> 
  add_osm_feature(key = "tourism", value = c("attraction", 'museum', 'auqarium',
                              'artwork', 'gallery',
                              'theme_park', 'viewpoint', 'zoo', 'yes')) |> 
  osmdata_sf() |> 
  (\(x) x$osm_polygons)() |> 
  select(name)

toronto_attractions = toronto_attractions[toronto_border,,op=st_within]

feature_scores = hash()
# calculating distance between cafes and nearest public transport stop
nearest_stop_func <- function(df_sf) {
  cafes_stop_dist = st_distance(df_sf, toronto_stations)
  cafes_stop_nearest = apply(cafes_stop_dist, 1, FUN = min)
  df_sf$nearest_stop_dist = cafes_stop_nearest
  
  return (df_sf)
}

cafes_data_sf = nearest_stop_func(cafes_data_sf)
MWU_nearest_stop_dist = wilcox.test(nearest_stop_dist ~ is_closed, data=cafes_data_sf)
MWU_nearest_stop_dist
feature_scores[['nearest_stop_dist']] = MWU_nearest_stop_dist$p.value

# creating feature capturing accessibility from public transport stops 
num_stops_nearby_func <- function(df_sf) {
  cafes_stop_dist = st_distance(df_sf, toronto_stations)
  n_rows = dim(df_sf)[1]
  n_stops = dim(toronto_stations)[1]
  n_stops_arr <- array(numeric(), c(n_rows,0))
  for (ind in 1:n_rows) {
    inv_dist_stop_dist = as.numeric(sum(1/cafes_stop_dist[ind,]))
    n_stops_arr = append(n_stops_arr, n_stops/inv_dist_stop_dist)
  }
  df_sf$num_stops_nearby = n_stops_arr
  return (df_sf)
}

cafes_data_sf = num_stops_nearby_func(cafes_data_sf)
MWU_num_stops_nearby = wilcox.test(num_stops_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_stops_nearby
feature_scores[['num_stops_nearby']] = MWU_num_stops_nearby$p.value

# Creating feature capturing number of nearby cafes 
num_cafes_nearby_func <- function(df_sf) {
  cafes_cafes_dist = st_distance(df_sf, df_sf)
  n_rows = dim(df_sf)[1]
  n_cafes_arr <- array(numeric(), c(n_rows,0))
  for (ind in 1:n_rows) {
    inv_dist_cafe_dist = sum(1.0/(as.numeric(cafes_cafes_dist[ind,]) + 1.0))
    n_cafes_arr = append(n_cafes_arr, n_rows/inv_dist_cafe_dist)
  }
  df_sf$num_cafes_nearby = n_cafes_arr
  return (df_sf)
}

cafes_data_sf = num_cafes_nearby_func(cafes_data_sf)
MWU_num_cafes_nearby = wilcox.test(num_cafes_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_cafes_nearby
feature_scores[['num_cafes_nearby']] = MWU_num_cafes_nearby$p.value

# creating feature capturing population and pop density within 2 blocks 
n_customers_func <- function(df_sf) {
  n_rows = dim(df_sf)[1]
  n_customers_arr <- array(numeric(), c(n_rows,0))
  # n_cust_density_arr <- array(numeric(), c(n_rows,0))
  cafes_pop_dist = st_distance(df_sf, toronto_population_sf)
  total_pop = sum(toronto_population_sf$pop)
  
  for (ind in 1:n_rows) {
    inv_dist_pop_dist = sum(toronto_population_sf$pop/(as.numeric(cafes_pop_dist[ind,]) + 1.0))
    # inv_dist_pop_dens_dist = sum(toronto_population_sf$pop_density/(as.numeric(cafes_pop_dist[ind,]) + 1.0))
    n_customers_arr = append(n_customers_arr, total_pop/inv_dist_pop_dist)
    # n_cust_density_arr = append(n_cust_density_arr, inv_dist_pop_dens_dist)
  }
  df_sf$num_customers_nearby = n_customers_arr
  # df_sf$cust_density_nearby = n_cust_density_arr
  
  return (df_sf)
}

cafes_data_sf = n_customers_func(cafes_data_sf)
MWU_num_customers_nearby = wilcox.test(num_customers_nearby ~ is_closed, data=cafes_data_sf)
# MWU_cust_density_nearby = wilcox.test(cust_density_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_customers_nearby
# MWU_cust_density_nearby
feature_scores[['num_customers_nearby']] = MWU_num_customers_nearby$p.value
# feature_scores[['cust_density_nearby']] = MWU_cust_density_nearby$p.value

# calculating rent at the cafe location using inverse distance weighting
toronto_border_transformed = toronto_border |>
  st_transform("EPSG:6345")

grd_toronto = st_bbox(toronto_border_transformed)  |>
  st_as_stars(dx = 300)  |>
  st_crop(toronto_border_transformed)

rent_per_sqft_trans_sf =  rent_per_sqft_sf |>
  st_transform("EPSG:6345")

inv_dist_weight <- idw(rent~1, rent_per_sqft_trans_sf, grd_toronto)
interpolated_rent <- data.frame(grd_toronto, inv_dist_weight) |>
  select(x, y, var1.pred)
interpolated_rent = na.omit(interpolated_rent)

interpolated_rent_trans = interpolated_rent |>
  st_as_sf(coords = c("x", "y"), crs=6345) |>
  st_transform('EPSG:4326')

tm_shape(inv_dist_weight[1,,])+
  tm_raster(title = "Predicted rent", 
            palette = "-Spectral",
            breaks = seq(0, 90, by = 10))

nearest_rent_func <- function(df_sf) {
  cafe_nearest_rent = st_distance(df_sf, interpolated_rent_trans)
  n_rows = dim(df_sf)[1]
  n_rent_idw_arr <- array(numeric(), c(n_rows,0))
  for (ind in 1:n_rows) {
    nearest_point_ind = which.min(cafe_nearest_rent[ind,])
    n_rent_idw_arr = append(n_rent_idw_arr, as.numeric(cafe_nearest_rent[ind, nearest_point_ind]))
  }
  df_sf$rent_idw = n_rent_idw_arr
  return (df_sf)
}

cafes_data_sf = nearest_rent_func(cafes_data_sf)
MWU_rent_idw = wilcox.test(rent_idw ~ is_closed, data=cafes_data_sf)
MWU_rent_idw
feature_scores[['rent_idw']] = MWU_rent_idw$p.value

# creating feature capturing whether cafe is in downtown or not
in_downtown_func <- function(df_sf) {
  n_rows = dim(df_sf)[1]
  in_downtown_arr <- array(numeric(), c(n_rows,0))
  for (ind in 1:n_rows) {
    in_downtown = length(df_sf$geometry[ind][toronto_downtown_border,,op=st_within])
    in_downtown_arr = append(in_downtown_arr, in_downtown)
  }
  df_sf$in_downtown = as.logical(in_downtown_arr)
  
  return (df_sf)
}

cafes_data_sf = in_downtown_func(cafes_data_sf)
# creating feature capturing whether cafe is a chain or not
chain_cafes = c('Aroma espresso bar', 'Aroma Espresso Bar', 'Delimark Cafe', 'Delimark Cafes', 'Starbucks','Tim Horton', 'Tim Horton Donuts', 'Tim Horton’s','Tim Hortons')

chain_or_no_func <- function(df_sf) {
  n_rows = dim(df_sf)[1]
  chain_arr <- array(logical(), c(n_rows,0))
  for (ind in 1:n_rows) {
    name = df_sf$name[ind]
    chain_or_no = name %in% chain_cafes
    chain_arr = append(chain_arr, chain_or_no)
  }
  df_sf$chain_or_no = chain_arr
  return (df_sf)
}

cafes_data_sf = chain_or_no_func(cafes_data_sf)
cafes_open_data_sf = cafes_data_sf[(cafes_data_sf$is_closed == FALSE),]
cafes_closed_data_sf = cafes_data_sf[(cafes_data_sf$is_closed == TRUE),]

cafes_closed_sf_chain = cafes_closed_data_sf[(cafes_closed_data_sf$chain_or_no == TRUE),]
cafes_open_sf_chain = cafes_open_data_sf[(cafes_open_data_sf$chain_or_no == TRUE),]
cafes_closed_sf_other = cafes_closed_data_sf[(cafes_closed_data_sf$chain_or_no == FALSE),]
cafes_open_sf_other = cafes_open_data_sf[(cafes_open_data_sf$chain_or_no == FALSE),]

observed_table <- matrix(c(dim(cafes_closed_sf_chain)[1],
                           dim(cafes_open_sf_chain)[1],
                           dim(cafes_closed_sf_other)[1],
                           dim(cafes_open_sf_other)[1]),
                         nrow = 2, ncol = 2, byrow = T)
rownames(observed_table) <- c('Chain', 'Not chain')
colnames(observed_table) <- c('Closed', 'Open')

expected_distribution <- chisq.test(observed_table)
expected_distribution
feature_scores[['chain_or_no']] = expected_distribution$p.value

# Creating feature capturing distance to the nearest open cafe
nearest_open_cafe_func <- function(df_sf) {
  cafe_cafe_dist = st_distance(df_sf, df_sf)
  closed_cafes = which(df_sf$is_closed == TRUE)
  n_rows = dim(df_sf)[1]
  cafe_nearest <- array(numeric(), c(n_rows,0))
  
  for (ind in 1:n_rows) {
    cafe_open_nearest = as.numeric(cafe_cafe_dist[ind,])
    cafe_open_nearest = min(cafe_open_nearest[-append(closed_cafes, ind)])
    cafe_nearest = append(cafe_nearest, cafe_open_nearest)
  }
  df_sf$nearest_cafe_dist = cafe_nearest
  return (df_sf)
}

cafes_data_sf = nearest_open_cafe_func(cafes_data_sf)
MWU_nearest_cafe_dist = wilcox.test(nearest_cafe_dist ~ is_closed, data=cafes_data_sf)
MWU_nearest_cafe_dist
feature_scores[['nearest_cafe_dist']] = MWU_nearest_cafe_dist$p.value

# creating feature capturing distance to nearest chain cafe
chain_cafes_df = cafes_data_sf[(cafes_data_sf$chain_or_no == TRUE),]
nearest_chain_func <- function(df_sf) {
  cafe_chain_dist = st_distance(df_sf, chain_cafes_df)
  cafe_chain_nearest = apply(cafe_chain_dist, 1, FUN = min)
  df_sf$nearest_chain_dist = cafe_chain_nearest
  
  return (df_sf)
}

cafes_data_sf = nearest_chain_func(cafes_data_sf)
non_chain_cafes_sf = cafes_data_sf[(cafes_data_sf$chain_or_no == FALSE),]
MWU_dist_nearest_chain = wilcox.test(nearest_chain_dist ~ is_closed, data=non_chain_cafes_sf)
MWU_dist_nearest_chain
feature_scores[['nearest_chain_dist']] = MWU_dist_nearest_chain$p.value

# querying parking spots in the city
toronto_parking = opq(toronto_bbox) |> 
  add_osm_feature(key = "amenity", value = "parking") |> 
  # add_osm_feature(key = "fee", value = "no") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  select()

toronto_parking = toronto_parking[toronto_border,,op=st_within]

# creating feature capturing number of free parking spots within 1 km of each cafe
num_parking_func <- function(df_sf) {
  n_rows = dim(df_sf)[1]
  n_parking_arr <- array(numeric(), c(n_rows,0))
  cafes_parking_dist = st_distance(df_sf, toronto_parking)
  n_parking_spots = dim(toronto_parking)[1]
  
  for (ind in 1:n_rows) {
    inv_dist_parking_dist = as.numeric(sum(1.0/cafes_parking_dist[ind,]))
    n_parking_arr = append(n_parking_arr, n_parking_spots/inv_dist_parking_dist)
  }
  df_sf$num_parking_nearby = n_parking_arr
  
  return (df_sf)
}

cafes_data_sf = num_parking_func(cafes_data_sf)
MWU_num_parking_nearby = wilcox.test(num_parking_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_parking_nearby
feature_scores[['num_parking_nearby']] = MWU_num_parking_nearby$p.value

# ggplot(cafes_data_sf, aes(x=is_closed, y=num_parking_nearby, fill=is_closed)) + 
#   geom_boxplot(alpha=0.5)+
#   theme(legend.position="none")+
#   labs(title="Number of free parking spots nearby",x="Is Closed?", y="Number")+
#   theme(plot.title = element_text(size=10), 
#         axis.text.y =element_text(size=10),
#         axis.text.x =element_text(size=10))

# querying tourist spots in Toronto
toronto_attractions = opq(toronto_bbox) |> 
  add_osm_feature(key = "tourism", value = c("attraction", 'museum', 'auqarium',
                              'artwork', 'gallery',
                              'theme_park', 'viewpoint', 'zoo', 'yes')) |> 
  osmdata_sf() |> 
  (\(x) x$osm_polygons)() |> 
  # filter(name == "Toronto") |> 
  select(name)

toronto_attractions = toronto_attractions[toronto_border,,op=st_within]
toronto_attractions = toronto_attractions[!duplicated(toronto_attractions), ]

cafe_attraction_func <- function(df_sf) {
  cafe_attractions_dist = st_distance(df_sf, toronto_attractions)
  cafe_attractions_nearest = apply(cafe_attractions_dist, 1, FUN = min)
  df_sf$nearest_attraction_dist = cafe_attractions_nearest
  
  return (df_sf)
}

cafes_data_sf = cafe_attraction_func(cafes_data_sf)
MWU_dist_nearest_attraction = wilcox.test(nearest_attraction_dist ~ is_closed, data=cafes_data_sf)
MWU_dist_nearest_attraction
feature_scores[['nearest_attraction_dist']] = MWU_dist_nearest_attraction$p.value

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_border)+
  tm_borders(lwd = 2)+
  tm_shape(toronto_attractions)+
  tm_dots(size = 0.20)+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)+
  tm_shape(toronto_downtown_border)+
  tm_borders(col = 'blue', lwd = 2)

# feature capturing proximity to attractions in Toronto
num_attraction_func <- function(df_sf) {
  n_rows = dim(df_sf)[1]
  n_attraction_arr <- array(numeric(), c(n_rows,0))
  cafes_attraction_dist = st_distance(df_sf, toronto_attractions)
  n_attractions = dim(toronto_attractions)[1]
  
  for (ind in 1:n_rows) {
    inv_dist_attraction = as.numeric(sum(1.0/cafes_attraction_dist[ind,]))
    n_attraction_arr = append(n_attraction_arr, n_attractions/inv_dist_attraction)
  }
  df_sf$num_attraction_nearby = n_attraction_arr
  
  return (df_sf)
}

cafes_data_sf = num_attraction_func(cafes_data_sf)
MWU_dist_attraction = wilcox.test(num_attraction_nearby ~ is_closed, data=cafes_data_sf)
MWU_dist_attraction
feature_scores[['num_attraction_dist']] = MWU_dist_attraction$p.value

# querying side streets in city
toronto_small_streets = opq(toronto_bbox) |> 
  add_osm_features(features = list(
   'highway' = 'tertiary',
   'highway' = 'residential'
   )) |> 
  osmdata_sf() |> 
  (\(x) x$osm_lines)() |> 
  select(name)

toronto_small_streets = toronto_small_streets[toronto_border,,op=st_within]

cafe_nearest_small_street_func <- function(df_sf) {
  cafe_small_street_dist = st_distance(df_sf, toronto_small_streets)
  small_street_nearest = apply(cafe_small_street_dist, 1, FUN = min)
  df_sf$nearest_small_street_dist = small_street_nearest
  
  return (df_sf)
}

cafes_data_sf = cafe_nearest_small_street_func(cafes_data_sf)
MWU_dist_nearest_small_street = wilcox.test(nearest_small_street_dist ~ is_closed, data=cafes_data_sf)
MWU_dist_nearest_small_street
feature_scores[['nearest_small_street_dist']] = MWU_dist_nearest_small_street$p.value

# ggplot(cafes_data_sf, aes(x=is_closed, y=nearest_small_street_dist, fill=is_closed)) + 
#   geom_boxplot(alpha=0.5)+
#   theme(legend.position="none")+
#   ylim(0,300)+
#   labs(title="Nearest small street distance",x="Is Closed?", y="Number")+
#   theme(plot.title = element_text(size=10), 
#         axis.text.y =element_text(size=10),
#         axis.text.x =element_text(size=10))

# querying main streets in city
toronto_main_streets = opq(toronto_bbox) |> 
  add_osm_features(features = list(
    'highway' = 'primary',
    'highway'= 'secondary'
  )) |> 
  osmdata_sf() |> 
  (\(x) x$osm_lines)() |> 
  select(name)

toronto_main_streets = toronto_main_streets[toronto_border,,op=st_within]

cafe_nearest_main_street_func <- function(df_sf) {
  cafe_main_street_dist = st_distance(df_sf, toronto_main_streets)
  main_street_nearest = apply(cafe_main_street_dist, 1, FUN = min)
  df_sf$nearest_main_street_dist = main_street_nearest
  
  return (df_sf)
}

cafes_data_sf = cafe_nearest_main_street_func(cafes_data_sf)
MWU_dist_nearest_main_street = wilcox.test(nearest_main_street_dist ~ is_closed, data=cafes_data_sf)
MWU_dist_nearest_main_street
feature_scores[['nearest_main_street_dist']] = MWU_dist_nearest_main_street$p.value

ggplot(cafes_data_sf, aes(x=is_closed, y=nearest_main_street_dist, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  ylim(0,200)+
  labs(title="Nearest main street distance",x="Is Closed?", y="Distance")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

# loading restaurant data for all cuisines
restaurant_data = read_csv(paste(cwd,'/trt_rest.csv',sep='')) |>
  select(Category,
         name = `Restaurant Name`,
         longitude = `Restaurant Longitude`,
         latitude = `Restaurant Latitude`,
         address = `Restaurant Address`)
restaurant_data = na.omit(restaurant_data)

restaurant_data_sf = restaurant_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)
restaurant_data_sf = restaurant_data_sf[toronto_border, , op = st_within]  
restaurant_data_sf = restaurant_data_sf[!duplicated(restaurant_data_sf), ]

# creating feature capturing number of restaurants 
num_rest_nearby_func <- function(df_sf){
  n_rows = dim(df_sf)[1]
  n_rest_arr <- array(numeric(), c(n_rows,0))
  cafes_rest_dist = st_distance(df_sf, restaurant_data_sf)
  num_rests = dim(restaurant_data_sf)[1]
  
  for (ind in 1:n_rows) {
    inv_dist_rest_dist = sum(1.0/(as.numeric(cafes_rest_dist[ind,]) + 1.0))
    n_rest_arr = append(n_rest_arr, num_rests/inv_dist_rest_dist)
  }
  df_sf$num_rest_nearby = n_rest_arr
  
  return (df_sf)
}

cafes_data_sf = num_rest_nearby_func(cafes_data_sf)
MWU_num_rest_nearby = wilcox.test(num_rest_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_rest_nearby
feature_scores[['num_rest_nearby']] = MWU_num_rest_nearby$p.value

ggplot(cafes_data_sf, aes(x=is_closed, y=num_rest_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(x="Is Closed?", y="Distance")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

feature_scores
columns = names(cafes_data_sf)
numerical_cols = columns[columns != 'name']
numerical_cols = numerical_cols[numerical_cols != 'is_closed']
numerical_cols = numerical_cols[numerical_cols != 'geometry']
numerical_cols = numerical_cols[numerical_cols != 'chain_or_no']
numerical_cols = numerical_cols[numerical_cols != 'in_downtown']

corr_mat = cor(st_drop_geometry(cafes_data_sf[numerical_cols]), st_drop_geometry(cafes_data_sf[numerical_cols]))
corrplot(corr_mat)

cor(cafes_data_sf$num_stops_nearby, 
                   cafes_data_sf$num_customers_nearby, method = 'pearson')**2

col_drop = c('name')
cafes_data_red_sf = cafes_data_sf[ , !(names(cafes_data_sf) %in% col_drop)]

cafes = cafes_data_red_sf %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select( 
    c(names(cafes_data_red_sf), 'is_closed', 'lon', 'lat')) |>
  sf::st_drop_geometry(cafes)

cafes$is_closed = as.factor(cafes$is_closed)

task = mlr3spatiotempcv::TaskClassifST$new(
  id = 'store_closure_prediction',
  backend = mlr3::as_data_backend(cafes),
  target = 'is_closed',
  positive = 'TRUE',
  coordinate_names = c('lon', 'lat'),
  coords_as_features = FALSE,
  crs = 'EPSG:4326'
)
elastic_net_learner = mlr3::lrn('classif.glmnet', predict_type = 'prob')
tune_level = mlr3::rsmp('spcv_coords', folds = 5)
elastic_net_search_space = paradox::ps(
  alpha = paradox::p_dbl(lower = 0, upper = 1),
  lambda = paradox::p_dbl(lower = 0, upper = 10)
)
tuner = mlr3tuning::tnr('grid_search', batch_size = 1, resolution = 5)
terminator = trm("none")
tune_elastic_net = mlr3tuning::AutoTuner$new(
  learner = elastic_net_learner,
  resampling = tune_level,
  measure = mlr3::msr('classif.auc'),
  search_space = elastic_net_search_space,
  terminator = terminator,
  tuner = tuner
)
resampling_spcv = mlr3::rsmp('repeated_spcv_coords', folds = 5, repeats = 5)
model_cv = mlr3::resample(
  task = task,
  learner = tune_elastic_net,
  resampling = resampling_spcv,
  store_models = TRUE,
  encapsulate = 'evaluate'
)
score_elastic_net_model = model_cv$score(measure = mlr3::msr("classif.auc"))
mean_roc_auc_elastic_net = round(mean(score_elastic_net_model$classif.auc),2)
mean_roc_auc_elastic_net
std_roc_auc_elastic_net = round(sd(score_elastic_net_model$classif.auc),2)
std_roc_auc_elastic_net

best_model_elastic_net_from_cv = model_cv$learners[[1]]$model$tuning_instance$result
best_alpha = best_model_elastic_net_from_cv$alpha
best_lambda = best_model_elastic_net_from_cv$lambda

random_forest_learner = mlr3::lrn('classif.randomForest', predict_type = 'prob')
rf_search_space = paradox::ps(
  ntree = paradox::p_int(lower = 100, upper = 500),
  nodesize = paradox::p_int(lower = 1, upper = 5)
)

tune_random_forest = mlr3tuning::AutoTuner$new(
  learner = random_forest_learner,
  resampling = tune_level,
  measure = mlr3::msr('classif.auc'),
  search_space = rf_search_space,
  terminator = terminator,
  tuner = tuner
)

model_cv = mlr3::resample(
  task = task,
  learner = tune_random_forest,
  resampling = resampling_spcv,
  store_models = TRUE,
  encapsulate = 'evaluate'
)
score_rf_model = model_cv$score(measure = mlr3::msr("classif.auc"))
mean_roc_auc_rf = round(mean(score_rf_model$classif.auc),2)
mean_roc_auc_rf
std_roc_auc_rf = round(sd(score_rf_model$classif.auc),2)
std_roc_auc_rf

best_model_rf_from_cv = model_cv$learners[[1]]$model$tuning_instance$result
best_ntree = best_model_rf_from_cv$ntree
best_nodesize = best_model_rf_from_cv$nodesize

best_elastic_net_model = mlr3::lrn('classif.glmnet', predict_type = 'prob', 
                       alpha = best_alpha, lambda = best_lambda)

best_rf_model = mlr3::lrn('classif.randomForest', predict_type = 'prob', 
                          ntree = best_ntree, nodesize = best_nodesize, importance = 'gini')

best_elastic_net_model$train(task)

best_rf_model$train(task)
best_rf_model$importance()

feature_names_ordered = names(best_rf_model$importance())
n_rows = length(feature_names_ordered)
feature_imp_array <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows){
  feature_imp_array = append(feature_imp_array, best_rf_model$importance()[[ind]])
}
feat_imp_df = data.frame(name = feature_names_ordered, val = feature_imp_array)

feat_imp_df %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

test_df = rent_per_sqft_sf |>
  mutate(rent_idw = rent)
test_df$chain_or_no = TRUE

test_df = cafe_attraction_func(test_df)
test_df = cafe_nearest_main_street_func(test_df)
test_df = cafe_nearest_small_street_func(test_df)
test_df = in_downtown_func(test_df)
test_df = n_customers_func(test_df)
test_df = nearest_chain_func(test_df)
test_df = nearest_open_cafe_func(test_df)
test_df = nearest_stop_func(test_df)
test_df = num_attraction_func(test_df)
test_df = num_cafes_nearby_func(test_df)
test_df = num_parking_func(test_df)
test_df = num_rest_nearby_func(test_df)
test_df = num_stops_nearby_func(test_df)

names(test_df)
test_df = test_df[ , !(names(test_df) %in% c('rent'))]

test_cafes = test_df %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select(
    c(names(test_df), 'lon', 'lat')) |>
  sf::st_drop_geometry(test_cafes)

prediction_elastic_net = best_elastic_net_model$predict_newdata(test_cafes)
prediction_rf = best_rf_model$predict_newdata(test_cafes)

best_locations_elastic_net_sorted = sort(prediction_elastic_net$prob[,1], index.return=TRUE)
worst_locations_elastic_net_sorted = sort(prediction_elastic_net$prob[,2], index.return=TRUE)

best_locations_rf_sorted = sort(prediction_rf$prob[,1], index.return=TRUE)
worst_locations_rf_sorted = sort(prediction_rf$prob[,2], index.return=TRUE)
mean(sort(prediction_rf$prob[,1])[1:10])
mean(sort(prediction_rf$prob[,2])[1:10])

top_10_best_locations = test_df[c(best_locations_rf_sorted$ix[1:10]),]
top_10_worst_locations = test_df[c(worst_locations_rf_sorted$ix[1:10]),]
top_10_best_locations$label = c('best locations')
top_10_worst_locations$label = c('worst locations')
best_worst_locations_sf = rbind(top_10_best_locations, top_10_worst_locations)

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(best_worst_locations_sf)+
  tm_dots(col = 'label', size = 0.5, title = 'location quality', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)+
  tm_shape(toronto_border)+
  tm_borders()

toronto_wards = opq(toronto_bbox) |> 
  add_osm_feature(key = "boundary", value = "administrative") |> 
  add_osm_feature(key = 'admin_level', value = '9') |>
  osmdata_sf() |> 
  (\(x) x$osm_multipolygons)() |> 
  select(name)

central_downtown = toronto_wards[(toronto_wards$name %in% c('Spadina—Fort York', 'Toronto Centre')),]
central_downtown_basemap = read_osm(central_downtown$geometry, type='apple-iphoto', mergeTiles=TRUE)

tm_shape(central_downtown_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(top_10_best_locations)+
  tm_dots(col = 'label', size = 0.5, title = 'location quality', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)

location_list = list()
n_rows = length(best_worst_locations_sf)

best_worst_locations = best_worst_locations_sf %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                    lat = sf::st_coordinates(.)[,2]) |>
  sf::st_drop_geometry(best_worst_locations)

reverse <- best_worst_locations %>%
  reverse_geocode(lat = lat, long = lon, method = 'osm',
                  address = address_found, full_results = TRUE)

best_worst_addresses = reverse |> select('label', 'address_found')
address_list = list()
for (ind in 1:dim(best_worst_locations)[1]){
  address = best_worst_addresses$address_found[ind]
  address = str_remove(address, 'Golden Horseshoe, ')
  if (reverse$name[ind] == ''){
    address_list = c(address_list, address)
  }
  else{
    address_list = c(address_list, str_remove(address, 
                                paste(reverse$name[ind], ', ', sep = '')))
  }
}
best_worst_addresses$address = address_list
best_worst_addresses = best_worst_addresses[ , !(names(best_worst_addresses) %in% c('address_found'))]
best_worst_addresses$address
