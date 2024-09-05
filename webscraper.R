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