library(sf)
library(osmdata)
library(tidyverse)
library(tmap)
library(osmdata)
library(tmaptools)
library(OpenStreetMap)
library(janitor)
library(readxl)
library(ggmap)
library(grid)
library(stringr)
library(ggplot2)
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
library(geodata)
library(spatstat)
library(terra)
library(GEOmap)
library(geosphere)
library(rasterVis)
library(stars)
options(dplyr.summarise.inform = FALSE)

cwd = 'C:/Users/saura/OneDrive/Desktop/DATA-6500/Project'
# api_secret <- '#### YOUR API KEY'
# register_google(key = api_secret)

options(dplyr.summarise.inform = FALSE)

toronto_bbox = st_bbox(c(xmin = -79.6392, 
          xmax = -79.115952,
          ymin = 43.403221,
          ymax = 43.855457),
        crs = st_crs(4326)) %>% st_as_sfc()
cafes_data = read_csv(paste(cwd,'/cafes_old_data.csv',sep='')) |>
  select(name, longitude, latitude, is_closed)
cafes_data = na.omit(cafes_data)

toronto_border = opq(toronto_bbox) |> 
  add_osm_feature(key = "boundary", value = "administrative") |> 
  add_osm_feature(key = 'admin_level', value = '6') |>
  osmdata_sf() |> 
  (\(x) x$osm_multipolygons)() |> 
  filter(name == "Toronto") |> 
  select()

cafes_data_sf = cafes_data %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)
cafes_data_sf = cafes_data_sf[toronto_border, , op = st_within]  
cafes_data_sf = cafes_data_sf[!duplicated(cafes_data_sf), ]

toronto_basemap = read_osm(toronto_border, type='apple-iphoto', mergeTiles=TRUE)
tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = 'is_closed', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 1.0, legend.width = 0.6)+
  tm_shape(toronto_border)+
  tm_borders()

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
  tm_borders()+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = 'is_closed', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)+
  tm_shape(toronto_downtown_border)+
  tm_borders(col = 'blue', lwd = 2)

downtown_basemap = read_osm(toronto_downtown_border, type='apple-iphoto', mergeTiles=TRUE)
downtown_locations = tm_shape(downtown_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_downtown_border)+
  tm_borders(lwd = 2)+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = 'is_closed', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 0.8, legend.width = 0.6)

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
  tm_borders()

downtown_locations
print(toronto_border_map, vp = viewport(0.7, 0.8, width = 0.2, height = 0.2))

cafes_open_data_sf = cafes_data_sf[(cafes_data_sf$is_closed == FALSE),]
cafes_closed_data_sf = cafes_data_sf[(cafes_data_sf$is_closed == TRUE),]

cafes_closed_sf_downtown = cafes_closed_data_sf[toronto_downtown_border,,op=st_within]
cafes_open_sf_downtown = cafes_open_data_sf[toronto_downtown_border,,op=st_within]
cafes_closed_sf_other = cafes_closed_data_sf[toronto_downtown_border,,op=st_disjoint]
cafes_open_sf_other = cafes_open_data_sf[toronto_downtown_border,,op=st_disjoint]

observed_table <- matrix(c(dim(cafes_closed_sf_downtown)[1], 
                           dim(cafes_open_sf_downtown)[1], 
                           dim(cafes_closed_sf_other)[1], 
                           dim(cafes_open_sf_other)[1]), 
                         nrow = 2, ncol = 2, byrow = T)
rownames(observed_table) <- c('Downtown', 'Rest of Toronto')
colnames(observed_table) <- c('Closed', 'Open')
observed_table

expected_distribution <- chisq.test(observed_table)
expected_distribution
round(expected_distribution$expected)

toronto_subway_stations = opq(toronto_bbox) |> 
  # add_osm_feature(key = "station", value = "subway") |> 
  add_osm_feature(key = "operator", value = "Toronto Transit Commission") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  select(name) |>
  drop_na()

toronto_subway_stations = toronto_subway_stations[toronto_border, , op = st_within]
downtown_subway_stations = toronto_subway_stations[toronto_downtown_border, , op = st_within]
tm_shape(downtown_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(downtown_subway_stations)+
  tm_dots(size = 0.20, col = 'blue')

cafes_subway_dist = st_distance(cafes_data_sf, toronto_subway_stations)
cafes_subway_nearest = apply(cafes_subway_dist, 1, FUN = min)
cafes_data_sf$nearest_subway_dist = cafes_subway_nearest
MWU_nearest_stop_dist = wilcox.test(nearest_subway_dist ~ is_closed, data=cafes_data_sf)
MWU_nearest_stop_dist

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
MWU_num_stops_nearby

ggplot(cafes_data_sf, aes(x=is_closed, y=nearest_subway_dist, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Nearest Subway Distance",x="IS CLOSED?", y="Distance in meters")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

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
MWU_nearest_cafe_dist

ggplot(cafes_data_sf, aes(x=is_closed, y=nearest_cafe_dist, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Nearest Cafe Distance",x="IS CLOSED?", y="Distance in meters")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

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
MWU_num_cafes_nearby

ggplot(cafes_data_sf, aes(x=is_closed, y=num_cafes_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Number of cafes",x="IS CLOSED?", y="Number")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

population_data = read_csv(paste(cwd,'/2016_92-151_XBB.csv',sep='')) |>
  select(pop = 'DBpop2016/IDpop2016',
         area = 'DBarea2016/IDsup2016', #`DBarea2016/IDsup2016`
         latitude = 'DArplat/Adlat', 
         longitude = 'DArplong/ADlong', 
         name = 'ERname/REnom')

population_data <- population_data[str_detect(population_data$name, "Toronto"), ]
population_sf = population_data %>% st_as_sf(coords = c("longitude", "latitude"), crs=4326)
toronto_population_sf = population_sf[toronto_border, , op = st_within]
toronto_population_sf = toronto_population_sf |>
  mutate(pop_density = pop/area)
toronto_population_sf = na.omit(toronto_population_sf)
  
toronto_population_proj =  toronto_population_sf |>
  select(pop) |>
  st_transform(crs = 6345)

toronto_pop_density_proj =  toronto_population_sf |>
  select(pop_density) |>
  st_transform(crs = 6345)

toronto_border_proj = toronto_border |>
  select() |>
  st_transform(crs = 6345) |>
  rast(resolution = 500)

population_raster = rasterize(toronto_population_proj, toronto_border_proj, 
                               field = 'pop', fun = 'sum', na.rm = T)

pop_density_raster = rasterize(toronto_pop_density_proj, toronto_border_proj, 
                              field = 'pop_density', fun = 'mean', na.rm = T)

gplot(population_raster) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = c("blue","red")) +
  labs(y = 'Latitude', x = 'Longitude' )+
  coord_equal()

gplot(pop_density_raster) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_color_gradient2(low = "blue", 
                        high = "red", 
                        mid = "white",
                        # Make the gradient2 "switch" at a midpoint of 13
                        midpoint = 100)+
  labs(y = 'Latitude', x = 'Longitude')+
  coord_equal()

tm_shape(pop_density_raster)+
  tm_raster(labels = c("very low", "low","med-low","medium","high"), 
            palette = "-Spectral",
            title = "Intensity")

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
  n_cust_density = sum(n_customers_bbox$pop_density)
  n_customers_arr = append(n_customers_arr, n_customers)
  n_cust_density_arr = append(n_cust_density_arr, n_cust_density)
}
cafes_data_sf$num_customers_nearby = n_customers_arr
cafes_data_sf$cust_density_nearby = n_cust_density_arr
MWU_num_customers_nearby = wilcox.test(num_customers_nearby ~ is_closed, data=cafes_data_sf)
MWU_num_customers_nearby

MWU_cust_density_nearby = wilcox.test(cust_density_nearby ~ is_closed, data=cafes_data_sf)
MWU_cust_density_nearby

rent_per_sqft = read.csv(paste(cwd, '/rent_per_sqft.csv',sep='')) |>
  select(longitude, latitude, rent)
rent_per_sqft = na.omit(rent_per_sqft)
rent_per_sqft_sf = rent_per_sqft %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)
rent_per_sqft_sf = rent_per_sqft_sf[!duplicated(rent_per_sqft_sf),]

downtown_rents_sf = rent_per_sqft_sf[toronto_downtown_border,,op=st_within]
outside_rents_sf = rent_per_sqft_sf[toronto_downtown_border,,op=st_disjoint]

mean_rent_downtown = mean(downtown_rents_sf$rent)
mean_rent_outside = mean(outside_rents_sf$rent)

mean_rent_arr <- array(numeric(), c(n_rows,0))
delta = 0.01
for (ind in 1:n_rows) {
  long = cafes_data_sf$geometry[[ind]][1]
  lat = cafes_data_sf$geometry[[ind]][2]
  bbox = st_bbox(c(xmin = long - delta, 
                   xmax = long + delta,
                   ymin = lat - delta,
                   ymax = lat + delta),
                 crs = st_crs(4326)) %>% st_as_sfc()
  rent_bbox = rent_per_sqft_sf[bbox, , op = st_within]
  mean_rent = mean(rent_bbox$rent)
  if (is.na(mean_rent)){
    in_downtown = length(cafes_data_sf$geometry[ind][toronto_downtown_border,,op=st_within])
    if (in_downtown == 0){
      mean_rent = mean_rent_outside
    }
    else {mean_rent = mean_rent_downtown}
  }
  mean_rent_arr = append(mean_rent_arr, mean_rent)
}

cafes_data_sf$mean_rent_nearby = mean_rent_arr
MWU_mean_rent_nearby = wilcox.test(mean_rent_nearby ~ is_closed, data=cafes_data_sf)
MWU_mean_rent_nearby

# toronto_wards = opq(toronto_bbox) |> 
#   add_osm_feature(key = "boundary", value = "administrative") |> 
#   add_osm_feature(key = 'admin_level', value = '9') |>
#   osmdata_sf() |> 
#   (\(x) x$osm_multipolygons)() |> 
#   select(name) |>
#   st_transform("EPSG:6345")
# 
# # plot(toronto_wards$geometry)
# 
toronto_border_transformed = toronto_border |>
  st_transform("EPSG:6345")

grd_toronto = st_bbox(toronto_border_transformed)  |>
  st_as_stars(dx = 300)  |>
  st_crop(toronto_border_transformed)

plot(grd_toronto)
rent_per_sqft_trans_sf =  rent_per_sqft_sf |>
  st_transform("EPSG:6345")

inv_dist_weight <- idw(rent~1, rent_per_sqft_trans_sf, grd_toronto)
tm_shape(inv_dist_weight[1,,])+
  tm_raster(title = "Predicted rent", 
            palette = "-Spectral",
            breaks = seq(0, 90, by = 10))
result <- data.frame(grd_toronto, inv_dist_weight) |>
  select(x, y, var1.pred)
result = na.omit(result)

result_trans = result |>
  st_as_sf(coords = c("x", "y"), crs=6345) |>
  st_transform('EPSG:4326')

cafe_nearest_rent = st_distance(cafes_data_sf, result_trans)
n_rows = dim(cafes_data_sf)[1]
n_rent_idw_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  nearest_point_ind = which.min(cafe_nearest_rent[ind,])
  n_rent_idw_arr = append(n_rent_idw_arr, as.numeric(cafe_nearest_rent[ind, nearest_point_ind]))
}
cafes_data_sf$rent_idw = n_rent_idw_arr
MWU_rent_idw = wilcox.test(rent_idw ~ is_closed, data=cafes_data_sf)
MWU_rent_idw

ggplot(cafes_data_sf, aes(x=is_closed, y=rent_idw, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Rent IDW",x="Is Closed?", y="$/Sqft")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

# +
#   tm_shape(rent_per_sqft_trans_sf)+
#   tm_dots( size = 0.1)+
#   tm_layout(legend.outside = TRUE,
#             frame = FALSE,
#             main.title = "rent predictions using IDW",
            # )

plot(toronto_border$geometry)
plot(rent_per_sqft_sf[(rent_per_sqft_sf$rent == 900),]$geometry, add = T)

ggplot(cafes_data_sf, aes(x=is_closed, y=mean_rent_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Rent",x="IS CLOSED?", y="$/SQFT")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

in_downtown_arr <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  in_downtown = length(cafes_data_sf$geometry[ind][toronto_downtown_border,,op=st_within])
  in_downtown_arr = append(in_downtown_arr, in_downtown)
}
cafes_data_sf$in_downtown = as.logical(in_downtown_arr)
MWU_mean_rent_downtown_vs_rest = wilcox.test(mean_rent_nearby ~ in_downtown, data=cafes_data_sf)
MWU_mean_rent_downtown_vs_rest

ggplot(cafes_data_sf, aes(x=in_downtown, y=mean_rent_nearby, fill=in_downtown)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Rent per square feet",x="IN DOWNTOWN?", y="$/sqft")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

MWU_num_customers_downtown = wilcox.test(num_customers_nearby ~ in_downtown, data=cafes_data_sf)
MWU_num_customers_downtown

MWU_num_cust_density_downtown = wilcox.test(cust_density_nearby ~ in_downtown, data=cafes_data_sf)
MWU_num_cust_density_downtown

chain_cafes = c('Aroma espresso bar', 'Aroma Espresso Bar', 'Delimark Cafe', 'Delimark Cafes', 'Starbucks',
                'Tim Horton', 'Tim Horton Donuts', 'Tim Horton’s',
                'Tim Hortons')

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
observed_table
expected_distribution <- chisq.test(observed_table)
expected_distribution
round(expected_distribution$expected)

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
MWU_dist_nearest_chain

toronto_parking = opq(toronto_bbox) |> 
  add_osm_feature(key = "amenity", value = "parking") |> 
  add_osm_feature(key = "fee", value = "no") |>
  osmdata_sf() |> 
  (\(x) x$osm_points)() |> 
  # filter(name == "Toronto") |> 
  select()

toronto_parking = toronto_parking[toronto_border,,op=st_within]

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_parking)+
  tm_dots(size = 0.05, col = 'blue')+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = '', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 1.0, legend.width = 0.6)+
  tm_shape(toronto_border)+
  tm_borders()

delta = 0.01 # 100 meters or 2 blocks
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
MWU_num_parking_nearby

cafes_data_sf_downtown = cafes_data_sf[(cafes_data_sf$in_downtown == FALSE),]
ggplot(cafes_data_sf_downtown, aes(x=is_closed, y=num_parking_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Number of free parking spots nearby",x="Is Closed?", y="Number")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))


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

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(toronto_attractions)+
  tm_dots(size = 0.5, col = 'blue')+
  tm_shape(cafes_data_sf)+
  tm_dots(col = 'is_closed', size = 0.20, title = '', palette=c('darkgreen', 'red'))+
  tm_layout(legend.text.size = 1.0, legend.width = 0.6)+
  tm_shape(toronto_border)+
  tm_borders()


cafe_attractions_dist = st_distance(cafes_data_sf, toronto_attractions)
n_rows = dim(cafes_data_sf)[1]
attraction_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_attraction_nearest = as.numeric(min(cafe_attractions_dist[ind,]))
  attraction_nearest = append(attraction_nearest, as.numeric(cafe_attraction_nearest))
}
cafes_data_sf$nearest_attraction_dist = attraction_nearest
MWU_dist_nearest_attraction = wilcox.test(nearest_attraction_dist ~ is_closed, data=cafes_data_sf)
MWU_dist_nearest_attraction

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
plot(toronto_border$geometry)
plot(toronto_streets$geometry, add = T)


cafe_street_dist = st_distance(cafes_data_sf, toronto_streets)
n_rows = dim(cafes_data_sf)[1]
street_nearest <- array(numeric(), c(n_rows,0))
for (ind in 1:n_rows) {
  cafe_street_nearest = as.numeric(min(cafe_street_dist[ind,]))
  street_nearest = append(street_nearest, as.numeric(cafe_street_nearest))
}
cafes_data_sf$nearest_street_dist = street_nearest
MWU_dist_nearest_street = wilcox.test(nearest_street_dist ~ is_closed, data=cafes_data_sf)
MWU_dist_nearest_street

#cafes_data_sf$geometry[[1]][1]

ggplot(cafes_data_sf, aes(x=is_closed, y=nearest_street_dist, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Nearest street distance",x="Is Closed?", y="distance in meters")+
  ylim(0,100)+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

median(cafes_data_sf[(cafes_data_sf$is_closed == TRUE),]$nearest_street_dist)
median(cafes_data_sf[(cafes_data_sf$is_closed == FALSE),]$nearest_street_dist)

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
MWU_num_rest_nearby

# median(cafes_data_sf[(cafes_data_sf$is_closed == TRUE),]$num_rest_nearby)
# median(cafes_data_sf[(cafes_data_sf$is_closed == FALSE),]$num_rest_nearby)

ggplot(cafes_data_sf, aes(x=is_closed, y=num_rest_nearby, fill=is_closed)) + 
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none")+
  labs(title="Number of nearby restaurants",x="Is Closed?", y="Number")+
  theme(plot.title = element_text(size=10), 
        axis.text.y =element_text(size=10),
        axis.text.x =element_text(size=10))

cafes = cafes_data_sf %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select('is_closed', 
         # 'nearest_subway_dist', 
         # 'nearest_cafe_dist', 
         # 'num_cafes_nearby',
         # 'num_customers_nearby', 
         # 'mean_rent_nearby', 
         'lon', 
         'lat', 
         # 'in_downtown',
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
test_df = rent_per_sqft_sf
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
test_df = test_df[!duplicated(test_df), ]

test_cafes = test_df %>% dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                                        lat = sf::st_coordinates(.)[,2]) |>
  select(
         'lon', 
         'lat', 
         'chain_or_no', 
         'num_parking_nearby',
         'nearest_street_dist',
         'num_rest_nearby') |>
  sf::st_drop_geometry(test_cafes)

test_cafes$lon = as.double(test_cafes$lon)
test_cafes$lat = as.double(test_cafes$lat)
test_cafes$chain_or_no = as.logical(test_cafes$chain_or_no)
test_cafes$num_parking_nearby = as.double(test_cafes$num_parking_nearby)
test_cafes$nearest_street_dist = as.double(test_cafes$nearest_street_dist)
test_cafes$num_rest_nearby = as.double(test_cafes$num_rest_nearby)

prediction = best_model$predict_newdata(test_cafes)
best_locations_sorted = sort(prediction$prob[,1], index.return=TRUE)
worst_locations_sorted = sort(prediction$prob[,2], index.return=TRUE)

top_10_best_locations = test_df[c(best_locations_sorted$ix[1:10]),]
top_10_worst_locations = test_df[c(worst_locations_sorted$ix[1:10]),]
best_locations = test_df[(best_locations_sorted$x < 0.2),]
worst_locations = test_df[(worst_locations_sorted$x < 0.4),]

tm_shape(toronto_basemap, simplify = 0.1, unit = 'km')+
  tm_scale_bar(width = 0.12)+
  tm_rgb(alpha = 0.5)+
  tm_shape(best_locations)+
  tm_dots(size = 0.5, col = 'green')+
  tm_shape(worst_locations)+
  tm_dots(size = 0.5, col = 'red')+
  tm_shape(toronto_border)+
  tm_borders()
