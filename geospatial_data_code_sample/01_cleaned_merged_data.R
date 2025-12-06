library(tidyverse)
library(lubridate)



      #The use of '#__' indicates code that I was using to
      #continually look at and examine the data, especially
      #after having transforming it. I hash these lines of
      #code so that when the script is executed, numerous 
      #tabs don't simultaneously get opened



#Loading in &------------------------------------------------------------------- 
#storing data-------------------------------------------------------------------

#Loading in raw csv file data and storing data
temp_data <- read_csv('data/country_dailyavg_ERA5_tavg_2015.csv')
geospatial_data <- read_csv('data/country_geospatial.csv')



#Reshaping & converting mXX_dZZ-------------------------------------------------
#strings to date values in temp_data--------------------------------------------

#Looking at data & checking for missing values
#__head(temp_data)
#__View(temp_data)
#__sum(is.na(temp_data))


#Reshaping data to longform to facilitate later analysis & looking at 
#data & checking for missing values
reshaped_temp_data <- temp_data |>
  pivot_longer(
    cols = !(iso3:name),
    names_to = 'date',
    values_to = 'temperature_celsius'
  )

#__head(reshaped_temp_data)
#__View(reshaped_temp_data)
#__sum(is.na(reshaped_temp_data))


#Parsing date column string values & converting to ISO8601 
#format (i.e. YYYY-MM-DD) -- I paste '2015_' to add missing 
#yr values. I then checking for any issues w/ the data
parsed_reshaped_temp_data <- reshaped_temp_data |>
  mutate(
    date = ymd(paste0('2015_', str_remove_all(date, '[md]')))
  )

#__head(parsed_reshaped_temp_data)
#__View(parsed_reshaped_temp_data)
#__sum(is.na(parsed_reshaped_temp_data))



#Add column for region & lowercasing all---------------------------------------- 
#columns to aid in joining two files later--------------------------------------

#Looking at geospatial data & checking for missing values
#__head(geospatial_data)
#__View(geospatial_data)
#__sum(is.na(geospatial_data))


#Locating missing data. 
geospatial_data |> 
  filter(if_any(everything(), is.na))

      #Located 3 missing values for FIPS column.
      #Located coding mistake for ISO2 value for 
      #Namibia ('NA' is the ISO2 value inputted
      #for Namibia). 

      #These missing values & coding errors do 
      #not affect the analysis, as FIPS and ISO2 
      #country codes will not be used in analysis 
      #given that all ISO3 unique country code 
      #values are present. Analyis may proceed 
      #unhindered.


#Adding region column & converting it to  
#categorical variable to facilitate analysis
geospatial_region_data <- geospatial_data |>
  mutate(
    region = case_when(
      LAT > 23 ~ 'north',
      LAT < -23 ~ 'south',
      LAT <= 23 & LAT >= -23  ~ 'tropics'
    ),
    region = factor(region, levels = c(
      'north',
      'tropics',
      'south'
    ))
  )


#Looking at & checking data
#__head(geospatial_region_data)
#__View(geospatial_region_data)
#__sum(is.na(geospatial_region_data))


#Making all column names lowercase (to match lower 
#case columns in other csv file for later merging)
l_case_geospatial_data <- geospatial_region_data |>
  rename_with(str_to_lower)


#Checking transformed data
#__head(l_case_geospatial_data)



#Merging parsed_reshaped_temp_data---------------------------------------------- 
#with l_case_geospatial_data----------------------------------------------------

#Merging parsed_reshaped_temp_data with 
#l_case_geospatial_data. I use a left_join 
#merge because parsed_reshaped_temp_data 
#has more dimensions, & left_join automatically 
#copies row values across duplicate matches 
#for iso3
merged_data <- parsed_reshaped_temp_data |>
  left_join(l_case_geospatial_data, by = "iso3") |>
  select(-name.y) |>
  rename(name = name.x)


#Looking at & checking data for errors
#__head(merged_data)
#__View(merged_data)
#__sum(is.na(merged_data))

      #1460 missing values found


#Locating 1460 missing values
#__View(
#__merged_data |>
#__filter(if_any(everything(), is.na))
#__)

merged_data |>
  select(where(~ any(is.na(.))))

      #Missing row values are confined to the columns fips and 
      #iso2. Result of 4 missing values in l_case_geospatial_csv 
      #being copied across all rows in left_join with 
      #parsed_reshaped_temp_data  (recall the parsed_reshaped_temp_data 
      #has considerably more rows that l_case_geospatial_csv)

      #The issue is innocuous given neither fips nor iso2 forms 
      #the basis for any later analysis


#Reordering columns for readability so as to create 
#final csv file for basis of later analysis
final_merged_data <- merged_data |>
  relocate(
    fips,
    iso2,
    iso3,
    name,
    region,
    lat,
    lon,
    date,
    temperature_celsius,
    area,
    pop2005
  )

#Saving cleaned and merged data
write_csv(final_merged_data, 'data/clean_merged_data.csv')

     #Note I don't drop rows with values missing for 
     #area & pop2005. In the R script 02_task_exhibits.R, 
     #I drop the aforementioned rows. This is because 
     #aspects of clean_merged_data.csv will form the basis 
     #of later analysis, & for some of this analysis I
     #will use the daily temperature for ALL rows
     #(including those without population)

#__View(final_merged_data)

