## mapping test ###############################################################
library(sf)
library(tidyverse)

uc <- st_read("data/01-raw/maps/Local_Administrative_Units_Level_1_January_2018_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom.shp")

uc %>% 
  filter(grepl("^E", lau118cd)) -> eng
plot(st_geometry(eng))

uc %>% 
  filter(grepl("^S", lau118cd)) -> sco
plot(st_geometry(sco))

## extracting tables from pdf

library(tabulizer)
x <- extract_tables(here::here("data/01-raw/nhm-15-16.pdf"), pages = 81, 
                    method = "decide")
x

x <- extract_tables(here::here("data/01-raw/wal-15-16-pcn.pdf"), pages = 102, 
                    method = "stream")
x

## extracting json files
library(jsonlite)

query1 <- "Column_ItemName_ENG"
query1.value <- "Gross%20expenditure"
query2 <- "Row_ItemName_ENG"
query2.value <- "Parking%20of%20vehicles"

query <- paste0("http://open.statswales.gov.wales/en-gb/dataset/lgfs0009?$filter=",
                query1, "%20eq%20%27", query1.value, "%27%20and%20",
                query2, "%20eq%20%27", query2.value, "%27")
 
x <- jsonlite::fromJSON(query)
data <- x[[2]]
unique(data$Year


query1 <- "Year_Code"
query1.value <- "201718"

query.2 <- paste0("http://open.statswales.gov.wales/en-gb/dataset/lgfs0009?$filter=",
                  query1, "%20eq%20%27", query1.value, "%27")

x <- jsonlite::fromJSON(query.2)



