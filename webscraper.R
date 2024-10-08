library(rvest)
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
library(webdriver)
library(httr)
library(htm2txt)
cwd = 'C:/Users/saura/OneDrive/Desktop/DATA-6500/Project'
api_secret <- '<API Key>'
register_google(key = api_secret)

rest_locations_old_data = read_csv(paste(cwd,'/trt_rest.csv',sep=''))
rest_locations_old_data = rest_locations_old_data |>
  select(Category,
         name = `Restaurant Name`,
         longitude = `Restaurant Longitude`,
         latitude = `Restaurant Latitude`,
         address = `Restaurant Address`)

cafes_old_data = rest_locations_old_data[(rest_locations_old_data$Category == 'Cafes'),]

n_rows = dim(cafes_old_data)[1]

status_list = list()
for (ind in 1:n_rows){
  name = cafes_old_data$name[ind]
  address = cafes_old_data$address[ind]
  splits = strsplit(name, ' ')
  search_name = paste(unlist(splits), collapse = '+') # returns firstName+lastName
  splits = strsplit(address, '\n')
  street = splits[[1]][1]
  splits = strsplit(street, ' ')
  search_address = paste(unlist(splits), collapse = '+') # returns XXX+name+street
  search_name = paste(search_name,search_address,sep='+') # returns firstName+lastName+XXX+name+street
  search_name = paste(search_name, 'toronto', sep='+')
  link <- paste('https://www.google.com/search?q=',search_name,sep='')
  lines <- readLines(link)

  status = FALSE
  for (line in lines){
    if (str_detect(str_to_lower(line), 'permanently closed')){
      status = TRUE
    }
  }
  status_list <- c(status_list, status)
}
status_list = as.logical(status_list)
cafes_old_data$is_closed = status_list
write.csv(cafes_old_data, paste(cwd, '/cafes_old_data.csv',sep=''))  

desc_list = list()
for (page_no in 1:93){
  link <- paste('https://www.zolo.ca/toronto-real-estate/commercial-for-lease/page-',as.character(page_no),sep ='')
  page <- read_html(link)

  summaries_css <- page %>%
    html_elements(css = ".card-listing")
  desc = html_text(summaries_css)
  desc_list <- c(desc_list, desc)
}

rent_info = do.call(rbind, Map(data.frame,
                              info=desc_list))
write.csv(rent_info, paste(cwd, '/rent_info.csv',sep=''))

rent_info = read.csv(paste(cwd, '/rent_info.csv',sep=''))
rent_info_not_null = rent_info[,2][!str_detect(rent_info[,2], 'X/sqft')]
rent_info_not_null_per_sqft = rent_info_not_null[str_detect(rent_info_not_null, '/sqft')]
rent_per_sqft_list = list()
n_rows = length(rent_info_not_null_per_sqft)
for (ind in 1:n_rows){
  rent = str_extract_all(rent_info_not_null_per_sqft[ind], '\\d+/sqft')[1]
  rent = str_extract_all(rent, '\\d+')
  rent = as.integer(rent)
  rent_per_sqft_list <- c(rent_per_sqft_list, rent)
}

rent_per_sqft = do.call(rbind, Map(data.frame, rent=rent_per_sqft_list))
longitude_list = list()
latitude_list = list()
n_rows = length(rent_info_not_null_per_sqft)
for (ind in 1:n_rows){
  loc = str_extract(rent_info_not_null_per_sqft[ind], '.+ Toronto, ON')
  long_lat = geocode(location = loc)
  longitude_list <- c(longitude_list, long_lat[[1]])
  latitude_list <- c(latitude_list, long_lat[[2]])
}

rent_per_sqft = do.call(rbind, Map(data.frame,
                                       longitude=longitude_list,
                                       latitude=latitude_list,
                                      rent=rent_per_sqft_list))
write.csv(rent_per_sqft, paste(cwd, '/rent_per_sqft.csv',sep=''))
