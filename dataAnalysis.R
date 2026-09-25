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

data_final <- all_modis_labeled %>% 
  # Remove the entire row if "remove" or "private submission" appears anywhere
  filter(!if_any(where(is.character), 
                 ~ str_detect(coalesce(., "NA"),
                              regex("remove|private submission", 
                                    ignore_case = TRUE)))) %>%
  # standardise text capitalisation for position 
  mutate(position = str_to_lower(position))

write.csv("data_final", "BBWdata.csv")

#==========================================================
# creating summary tables
#=========================================================
library(gt)

# Define the function
export_nest_table <- function(data, ..., file_name) {
  
  # Capture how many variables were passed
  vars <- enquos(...)
  
  # Process data: count, pivot wider with a separator, calculate totals
  processed_data <- data %>%
    count(specificEpithet, ...) %>%
    pivot_wider(
      names_from = c(...), 
      values_from = n, 
      values_fill = 0, 
      names_sep = "___"
    ) %>%
    mutate(Total = rowSums(across(where(is.numeric)))) %>%
    bind_rows(
      summarise(., across(where(is.numeric), sum), across(where(is.character), ~ "Total"))
    )
  
  # Build the gt table
  table_obj <- processed_data %>% gt()
  
  # If multiple variables were passed, add hierarchical spanner headers automatically
  if (length(vars) > 1) {
    table_obj <- table_obj %>% tab_spanner_delim(delim = "___")
  }
  
  # Save to Word
  table_obj %>% gtsave(file_name)
  message("Successfully saved: ", file_name)
}

export_nest_table(data_final, landcover, 
                  file_name = "speciesLandcover.docx")
export_nest_table(data_final, position, 
                  file_name = "speciesPosition.docx")
export_nest_table(data_final, nestCategory, 
                  file_name =  "speciesNestCategory.docx")
export_nest_table(data_final, nestCategory, nestDescription,
                  file_name = "speciesCategoryDescription.docx")
export_nest_table(data_final, )
