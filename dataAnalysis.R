# R script for analyzing the BBW nest project data

## getting the data from google drive

library(googledrive)
library(readr)
library(googlesheets4)

# need to login once
drive_auth()

#search for the file

data <- read_csv(drive_read_string("BBW nest data cleaned.csv"))

#=====================================================
# getting MODIS landcover data
#====================================================
library(MODISTools) #MCD12Q1
library(tidyverse)

data_clean <- data %>%
  mutate(
    year = as.numeric(substr(eventDate, 7, 10)), # Extracts YYYY from MM/DD/YYYY
    start_date = paste0(year, "-01-01"),
    end_date = paste0(year, "-12-31")
  )

#function to extract the landcover by the year
extract_modis_landcover <- function(decimalLatitude, 
                                    decimalLongitude, 
                                    start_date, 
                                    end_date) {
  tryCatch({ #incase a lan/lon is out of bounds or there is an error
    mt_subset(
      product = "MCD12Q1",
      lat = decimalLatitude,
      lon = decimalLongitude,
      band = "LC_Type1",
      start = start_date,
      end = end_date,
      km_lr = 0,          
      km_ab = 0
    )
  }, error = function(e) {
    return(NULL)
  })
}


all_modis_data <- data_clean %>%
  mutate(
    modis_result = pmap(
      list(
        decimalLatitude  = decimalLatitude, 
        decimalLongitude = decimalLongitude, 
        start_date = start_date, 
        end_date = end_date
      ), 
      extract_modis_landcover
    )
  ) %>%
  unnest(modis_result)

#converting the values to landcover types

landLookup <- tibble (
  value = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
  landcover = c(
    "Evergreen Needleleaf Forest", "Evergreen Broadleaf Forest", 
    "Deciduous Needleleaf Forest", "Deciduous Broadleaf Forest", 
    "Mixed Forests", "Closed Shrublands", "Open Shrublands", 
    "Woody Savannas", "Savannas", "Grasslands", "Permanent Wetlands", 
    "Croplands", "Urban and Built-up", "Cropland/Natural Vegetation Mosaic", 
    "Snow and Ice", "Barren", "Water Bodies"
  )
)

#join it back to the dataset
all_modis_labeled <- all_modis_data %>%
  left_join(landLookup, by = "value")

data_final <- all_modis_labeled
