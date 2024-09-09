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

# Visualization
library(tmap)
library(tmaptools)
library(ggmap)
library(grid)
library(ggplot2)
library(rasterVis)

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
cwd = 'C:/Users/saura/OneDrive/Desktop/DATA-6500/Project'
options(dplyr.summarise.inform = FALSE)
# api_secret <- '#### YOUR API KEY'
# register_google(key = api_secret)

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

# querying free parking lots in Toronto.
toronto_parking = opq(toronto_bbox) |> 
  add_osm_feature(key = "amenity", value = "parking") |> 
  add_osm_feature(key = "fee", value = "no") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  select()

toronto_parking = toronto_parking[toronto_border,,op=st_within]

# querying tourist spots in Toronto
toronto_attractions = opq(toronto_bbox) |> 
  add_osm_feature(key = "tourism", value = c("attraction", 'museum', 'auqarium',
                              'artwork', 'gallery',
                              'theme_park', 'viewpoint', 'zoo', 'yes')) |> 
  osmdata_sf() |> 
  (\(x) x$osm_polygons)() |> 
  select(name)

toronto_attractions = toronto_attractions[toronto_border,,op=st_within]

ggplot(rent_downtown_outside_sf, aes(x=in_downtown, y=rent, fill=in_downtown)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Rent per square feet",x="IN DOWNTOWN?", y="$/sqft")+
  ylim(0, 100)+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))
MWU_rent = wilcox.test(rent ~ in_downtown, data=rent_downtown_outside_sf)

# calculating distance between cafes and nearest public transport stop
cafes_stop_dist = st_distance(cafes_data_sf, toronto_stations)
cafes_stop_nearest = apply(cafes_stop_dist, 1, FUN = min)
cafes_data_sf$nearest_stop_dist = cafes_stop_nearest
MWU_nearest_stop_dist = wilcox.test(nearest_stop_dist ~ is_closed, data=cafes_data_sf)

# creating feature capturing number of stops within 2 blocks of each cafe
delta = 0.001 # 100 meters or 2 blocks
n_rows = dim(cafes_data_sf)[1]
n_stops_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_stops = cafes_data_sf[bbox, , op = st_within]
  n_stops_arr = append(n_stops_arr, dim(n_stops)[1])
}
cafes_data_sf$num_stops_nearby = n_stops_arr
MWU_num_stops_nearby = wilcox.test(num_stops_nearby ~ is_closed, data=cafes_data_sf)

# Creating feature capturing number of cafes within bounding box of 2 blocks
delta = 0.001 # 100 meters or 2 blocks
n_rows = dim(cafes_data_sf)[1]
n_cafes_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_cafes = cafes_data_sf[bbox, , op = st_within]
  n_cafes_arr = append(n_cafes_arr, dim(n_cafes)[1]-1)
}
cafes_data_sf$num_cafes_nearby = n_cafes_arr
MWU_num_cafes_nearby = wilcox.test(num_cafes_nearby ~ is_closed, data=cafes_data_sf)

# creating feature capturing population and pop density within 2 blocks 
delta = 0.001
n_customers_arr <- array(numeric(), c(n_rows,0))
n_cust_density_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_customers_bbox = toronto_population_sf[bbox, , op = st_within]
  n_customers = sum(n_customers_bbox$pop)
  n_cust_density = mean(n_customers_bbox$pop_density)
  n_customers_arr = append(n_customers_arr, n_customers)
  n_cust_density_arr = append(n_cust_density_arr, n_cust_density)
}
cafes_data_sf$num_customers_nearby = n_customers_arr
cafes_data_sf$cust_density_nearby = n_cust_density_arr
MWU_num_customers_nearby = wilcox.test(num_customers_nearby ~ is_closed, data=cafes_data_sf)
MWU_cust_density_nearby = wilcox.test(cust_density_nearby ~ is_closed, data=cafes_data_sf)

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

cafe_nearest_rent = st_distance(cafes_data_sf, interpolated_rent_trans)
n_rows = dim(cafes_data_sf)[1]
n_rent_idw_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  nearest_point_ind = which.min(cafe_nearest_rent[ind,])
  n_rent_idw_arr = append(n_rent_idw_arr, as.numeric(cafe_nearest_rent[ind, nearest_point_ind]))
}
cafes_data_sf$rent_idw = n_rent_idw_arr
MWU_rent_idw = wilcox.test(rent_idw ~ is_closed, data=cafes_data_sf)

# creating feature capturing whether cafe is in downtown or not
in_downtown_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  in_downtown = length(cafes_data_sf$geometry[ind][toronto_downtown_border,,op=st_within])
  in_downtown_arr = append(in_downtown_arr, in_downtown)
}
cafes_data_sf$in_downtown = as.logical(in_downtown_arr)

# creating feature capturing whether cafe is a chain or not
chain_cafes = c('Aroma espresso bar', 'Aroma Espresso Bar', 'Delimark Cafe', 'Delimark Cafes', 'Starbucks','Tim Horton', 'Tim Horton Donuts', 'Tim Horton’s','Tim Hortons')

chain_arr <- array(logical(), c(n_rows,0))
for (ind in 1:n_rows) {
  name = cafes_data_sf$name[ind]
  chain_or_no = name %in% chain_cafes
  chain_arr = append(chain_arr, chain_or_no)
}
cafes_data_sf$chain_or_no = chain_arr
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

# Creating feature capturing distance to the nearest open cafe
cafe_cafe_dist = st_distance(cafes_data_sf, cafes_open_data_sf)
n_rows = dim(cafes_data_sf)[1]
cafe_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_cafe_nearest = as.numeric(min(cafe_cafe_dist[ind,]))
  if (cafe_cafe_nearest == 0.0){
    cafe_cafe_nearest = sort(cafe_cafe_dist[ind,])[2]
  }
  cafe_nearest = append(cafe_nearest, as.numeric(cafe_cafe_nearest))
}

cafes_data_sf$nearest_cafe_dist = cafe_nearest
MWU_nearest_cafe_dist = wilcox.test(nearest_cafe_dist ~ is_closed, data=cafes_data_sf)

# creating feature capturing distance to nearest chain cafe
chain_cafes_df = cafes_data_sf[(cafes_data_sf$chain_or_no == TRUE),]
cafe_chain_dist = st_distance(cafes_data_sf, chain_cafes_df)
n_rows = dim(cafes_data_sf)[1]
chain_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_chain_nearest = as.numeric(min(cafe_chain_dist[ind,]))
  chain_nearest = append(chain_nearest, as.numeric(cafe_chain_nearest))
}
cafes_data_sf$nearest_chain_dist = chain_nearest
non_chain_cafes_sf = cafes_data_sf[(cafes_data_sf$chain_or_no == FALSE),]
MWU_dist_nearest_chain = wilcox.test(nearest_chain_dist ~ is_closed, data=non_chain_cafes_sf)

# querying free parking spots in the city
toronto_parking = opq(toronto_bbox) |> 
  add_osm_feature(key = "amenity", value = "parking") |> 
  add_osm_feature(key = "fee", value = "no") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  select()

toronto_parking = toronto_parking[toronto_border,,op=st_within]

# creating feature capturing number of free parking spots within 1 km of each cafe
delta = 0.01 # 1000 meters 
n_rows = dim(cafes_data_sf)[1]
n_parking_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_parking = toronto_parking[bbox, , op = st_within]
  n_parking_arr = append(n_parking_arr, dim(n_parking)[1])
}
cafes_data_sf$num_parking_nearby = n_parking_arr
MWU_num_parking_nearby = wilcox.test(num_parking_nearby ~ is_closed, data=cafes_data_sf)

parking = ggplot(cafes_data_sf, aes(x=is_closed, y=num_parking_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  ylim(0,200)+
  labs(title="Number of free parking spots nearby",x="Is Closed?", y="Number")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

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

cafe_attractions_dist = st_distance(cafes_data_sf, toronto_attractions)
n_rows = dim(cafes_data_sf)[1]
attraction_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_attraction_nearest = as.numeric(min(cafe_attractions_dist[ind,]))
  attraction_nearest = append(attraction_nearest, as.numeric(cafe_attraction_nearest))
}
cafes_data_sf$nearest_attraction_dist = attraction_nearest
MWU_dist_nearest_attraction = wilcox.test(nearest_attraction_dist ~ is_closed, data=cafes_data_sf)

# querying streets in city
toronto_streets = opq(toronto_bbox) |> 
  add_osm_features(features = list(
   'highway' = 'primary',
   'highway'= 'secondary'
   # 'highway' = 'tertiary'
   # 'highway' = 'residential'
   )) |> 
  osmdata_sf() |> 
  (\(x) x$osm_lines)() |> 
  select(name)

toronto_streets = toronto_streets[toronto_border,,op=st_within]

cafe_street_dist = st_distance(cafes_data_sf, toronto_streets)
n_rows = dim(cafes_data_sf)[1]
street_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_street_nearest = as.numeric(min(cafe_street_dist[ind,]))
  street_nearest = append(street_nearest, as.numeric(cafe_street_nearest))
}
cafes_data_sf$nearest_street_dist = street_nearest
MWU_dist_nearest_street = wilcox.test(nearest_street_dist ~ is_closed, data=cafes_data_sf)

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

# creating feature capturing number of restaurants within a block of each cafe
delta = 0.0005 # 100 meters or 2 blocks
n_rows = dim(cafes_data_sf)[1]
n_rest_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_rest = restaurant_data_sf[bbox, , op = st_within]
  n_rest_arr = append(n_rest_arr, dim(n_rest)[1]-1)
}
cafes_data_sf$num_rest_nearby = n_rest_arr
MWU_num_rest_nearby = wilcox.test(num_rest_nearby ~ is_closed, data=cafes_data_sf)

ggplot(cafes_data_sf, aes(x=is_closed, y=num_rest_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(x="Is Closed?", y="Number")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

tm_shape(inv_dist_weight[1,,])+
  tm_raster(title = "Predicted rent", 
            palette = "-Spectral",
            breaks = seq(0, 90, by = 10))

cafes = cafes_data_sf %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select('is_closed', 
         'lon', 
         'lat', 
         'chain_or_no', 
         'num_parking_nearby',
         'nearest_street_dist',
         'num_rest_nearby',
         'rent_idw') |>
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
#random_forest_learner = mlr3::lrn('classif.randomForest', predict_type = 'prob')
tune_level = mlr3::rsmp('spcv_coords', folds = 5)
#random_forest_learner = mlr3::lrn('classif.randomForest', predict_type = 'prob')
search_space = paradox::ps(
  alpha = paradox::p_dbl(lower = 0, upper = 1),
  lambda = paradox::p_dbl(lower = 0, upper = 10)
)
# search_space = paradox::ps(
#   ntree = paradox::p_int(lower = 100, upper = 500),
#   nodesize = paradox::p_int(lower = 1, upper = 5)
# )
tuner = mlr3tuning::tnr('grid_search', batch_size = 1, resolution = 5)
terminator = trm("none")
tune_elastic_net = mlr3tuning::AutoTuner$new(
  learner = elastic_net_learner,
  resampling = tune_level,
  measure = mlr3::msr('classif.auc'),
  search_space = search_space,
  terminator = terminator,
  tuner = tuner
)
resampling_spcv = mlr3::rsmp('repeated_spcv_coords', folds = 5, repeats = 5)
model_cv = mlr3::resample(
  task = task,
  learner = tune_elastic_net,
  # learner = tune_random_forest,
  resampling = resampling_spcv,
  store_models = TRUE,
  encapsulate = 'evaluate'
)
score_model = model_cv$score(measure = mlr3::msr("classif.auc"))
mean_roc_auc = round(mean(score_model$classif.auc),2)
mean_roc_auc
std_roc_auc = round(sd(score_model$classif.auc),2)
std_roc_auc

best_model_from_cv = model_cv$learners[[1]]$model$tuning_instance$result
best_alpha = best_model_from_cv$alpha
best_lambda = best_model_from_cv$lambda

best_model = mlr3::lrn('classif.glmnet', predict_type = 'prob', 
                       alpha = best_alpha, lambda = best_lambda)

best_model$train(task)
test_df = rent_per_sqft_sf |>
  mutate(rent_idw = rent)
test_df$chain_or_no = FALSE

delta = 0.01 # 100 meters or 2 blocks
n_rows = dim(test_df)[1]
n_parking_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = test_df$geometry[[ind]][1]
  lat = test_df$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_parking = toronto_parking[bbox, , op = st_within]
  n_parking_arr = append(n_parking_arr, dim(n_parking)[1])
}
test_df$num_parking_nearby = n_parking_arr

cafe_street_dist = st_distance(test_df, toronto_streets)
n_rows = dim(test_df)[1]
street_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_street_nearest = as.numeric(min(cafe_street_dist[ind,]))
  street_nearest = append(street_nearest, as.numeric(cafe_street_nearest))
}
test_df$nearest_street_dist = street_nearest

delta = 0.0005 # 100 meters or 2 blocks
n_rows = dim(test_df)[1]
n_rest_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  long = test_df$geometry[[ind]][1]
  lat = test_df$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  n_rest = restaurant_data_sf[bbox, , op = st_within]
  n_rest_arr = append(n_rest_arr, dim(n_rest)[1])
}
test_df$num_rest_nearby = n_rest_arr
test_df = test_df[!duplicated(test_df),]

test_cafes = test_df %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select(
         'lon', 
         'lat', 
         'chain_or_no', 
         'num_parking_nearby',
         'nearest_street_dist',
         'num_rest_nearby',
         'rent_idw') |>
  sf::st_drop_geometry(test_cafes)

test_cafes$lon = as.double(test_cafes$lon)
test_cafes$lat = as.double(test_cafes$lat)
test_cafes$chain_or_no = as.logical(test_cafes$chain_or_no)
test_cafes$num_parking_nearby = as.double(test_cafes$num_parking_nearby)
test_cafes$nearest_street_dist = as.double(test_cafes$nearest_street_dist)
test_cafes$num_rest_nearby = as.double(test_cafes$num_rest_nearby)
test_cafes$rent_idw = as.double(test_cafes$rent_idw)

prediction = best_model$predict_newdata(test_cafes)
best_locations_sorted = sort(prediction$prob[,1], index.return=TRUE)
worst_locations_sorted = sort(prediction$prob[,2], index.return=TRUE)

top_10_best_locations = test_df[c(best_locations_sorted$ix[1:10]),]
top_10_worst_locations = test_df[c(worst_locations_sorted$ix[1:10]),]

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(top_10_best_locations)+
  tm_dots(size = 0.5, col = 'green')+
  tm_shape(top_10_worst_locations)+
  tm_dots(size = 0.5, col = 'red')+
  tm_shape(toronto_border)+
  tm_borders()
