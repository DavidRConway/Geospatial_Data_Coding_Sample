library(tidyverse)
library(lubridate)
library(stargazer)
library(rnaturalearth)
library(sf)
library(viridis)
library(ggridges)



      #The use of '#__' indicates code that I was using to
      #continually look at and examine the data, especially
      #after having transforming it. I hash these lines of
      #code so that when the script is executed, numerous 
      #tabs don't simultaneously get opened



#Loading and storing------------------------------------------------------------
#merged dataset-----------------------------------------------------------------
merged_data <- read_csv('data/clean_merged_data.csv')



#Computing daily weighted population--------------------------------------------
#& area averages for each region------------------------------------------------

#Computing annual statistics of daily weighted average temperatures for
#for population & area for each region. Ensuring to only relevant missing 
#values by keeping 'na.rm = TRUE' within individual calculations
region_daily_temps <- merged_data |>
  group_by(region, date) |>
  summarize(
    region_pop_daily_weighted_temp  = sum(temperature_celsius * pop2005, na.rm = TRUE) /
      sum(pop2005, na.rm = TRUE),
    region_area_daily_weighted_temp = sum(temperature_celsius * area, na.rm = TRUE) /
      sum(area, na.rm = TRUE),
    .groups = 'drop'
  )


#Looking at data
#__View(region_daily_temps)

#There are 1095 rows. This is because
#there are 3x365observations (i.e. one
#for each region for each day of the year)



#Creating stargazer table to summarize distributions of daily-------------------
#weighted average temperatures for area & population by region-------------------

#Obtaining and coding relevant summary statistics for
#stargazer table. Rounding values for readability
region_tb_stats <- region_daily_temps |>
  group_by(region) |>
  summarize(
    N_pop = sum(!is.na(region_pop_daily_weighted_temp)),   
    pop_weighted_temp = mean(region_pop_daily_weighted_temp, na.rm = TRUE),
    sd_pop = sd(region_pop_daily_weighted_temp, na.rm = TRUE), 
    
    N_area = sum(!is.na(region_area_daily_weighted_temp)),
    area_weighted_temp = mean(region_area_daily_weighted_temp, na.rm = TRUE),
    sd_area = sd(region_area_daily_weighted_temp, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

print(region_tb_stats)

#Creating stargazer table 
#with LaTeX output file
stargazer(as.data.frame(region_tb_stats), 
          type = 'latex', 
          summary = FALSE, 
          rownames = FALSE,
          header = TRUE,
          title = 'Annual Temperature Statistics by Region (C°)',
          label = 'tab:region_stats',
          out = 'data/region_tb_stats.tex')







#PLotting daily weighted averages-----------------------------------------------
#(population & area) through time-----------------------------------------------


#Pivoting data to facilitate presenting
#both weighted averages on same panel &
#then generating time series plots (by
#region) for population and area averages
region_daily_temps |>
  pivot_longer(
    c(region_pop_daily_weighted_temp, region_area_daily_weighted_temp),
    names_to = 'weight_avg_type',
    values_to = 'temperature'
  ) |>
  mutate(
    region = str_to_title(region),
    weight_avg_type = recode(
      weight_avg_type,
      region_pop_daily_weighted_temp = 'Daily Population Weighted Average Temperature by Region (2015)',
      region_area_daily_weighted_temp = 'Daily Area Weighted Average Temperature by Region (2015)'
    )
  ) |>
  ggplot(aes(date, temperature, color = region)) +
  geom_line() +
  scale_x_date(date_breaks = '1 month', date_labels = '%b', expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::breaks_width(5), name = 'Temperature °C') +
  scale_colour_brewer(palette = 'Set2') +
  facet_wrap(~ weight_avg_type, nrow = 1) +
  labs(x = 'Date', color = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = 'bold', size = 11),
    legend.position  = 'bottom',
    text = element_text(family = 'serif')
  )

ggsave('figures/02_daily_time_series_plots.png', width = 9.5, height = 3.8)



#Creating world temperature heatmap for annual average population/area----------
#weighted temperature and using sf and rnaturalearth----------------------------

#Obtaining country-level
#annual weighted temps
country_annual_temps <- merged_data |>
  group_by(iso3) |>
  summarize(
    annual_pop_weighted_temp = sum(temperature_celsius * pop2005, na.rm = TRUE) /
      sum(pop2005, na.rm = TRUE),
    annual_area_weighted_temp = sum(temperature_celsius * area, na.rm = TRUE) /
      sum(area, na.rm = TRUE),
    .groups = 'drop'
  )

#Norway & France have unmatching ISO3 codes due
#to incommensurate data on territories across
#world and country_annual_temps datasets.
#Conducting a forced matching to ensure Norway
#& France appear on world heatmap
world <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sf') |>
  mutate(iso_a3 = case_when(
    admin == 'France' ~ 'FRA',
    admin == 'Norway' ~ 'NOR',
    TRUE ~ iso_a3
  ))

#Joining world (rnaturalearth)
#with country_annual_temps
world_temps_joined <- world |>
  left_join(country_annual_temps, by = c('iso_a3' = 'iso3'))

#Plotting population and area weighted average
#temps as a faceted plot
world_temps_joined |>
  pivot_longer(
    cols = c(annual_pop_weighted_temp, annual_area_weighted_temp),
    names_to = 'temp_type',
    values_to = 'temperature'
  ) |>
  mutate(
    temp_type = recode(
      temp_type,
      annual_pop_weighted_temp = 'Annual Population Weighted Average Temperature by Country (2015)',
      annual_area_weighted_temp = 'Annual Area Weighted Average Temperature by Country (2015)'
    )
  ) |>
  ggplot() +
  geom_sf(aes(fill = temperature), color = NA) +
  scale_fill_viridis(option = 'magma',
                     name = 'Temperature (°C)',
                     na.value = 'gray90'
  ) +
  facet_wrap(~ temp_type, nrow = 2) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 13, face = 'bold'),
    legend.position = 'bottom',
    text = element_text(family = 'serif')
  ) +
  guides(
    fill = guide_colorbar(barwidth = 10, barheight = 0.5)
  )

ggsave('figures/03_world_map_faceted.png', width = 8.5, height = 7)



#Creating ridgline plot---------------------------------------------------------
#using ggridges-----------------------------------------------------------------

# Pivoting data to facilitate ridgeline plot panel 
#& piping to ggplot() to generate plots. Figure is
#faceted by region and weight type
region_daily_temps |>
  pivot_longer(
    cols = c(region_pop_daily_weighted_temp, region_area_daily_weighted_temp),
    names_to = 'type',
    values_to = 'temp'
  ) |>
  mutate(
    month = month(date, label = TRUE, abbr = TRUE),
    region = str_to_title(region),
    type = recode(
      type,
      region_pop_daily_weighted_temp = 'Population Weighted Temperature',
      region_area_daily_weighted_temp = 'Area Weighted Temperature'
    )
  ) |>
  ggplot(aes(temp, month, fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = '(°C)', option = 'C') +
  facet_grid(type ~ region) +
  labs(
    title = 'Distribution of Daily Weighted Temperatures by Region & Month (2015)',
    x = 'Temperature (°C)',
    y = 'Month'
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = '#F5F5F5', color = NA),
    panel.grid.major = element_line(color = 'white', size = 0.5),  
    panel.grid.minor = element_blank(),                            
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
    strip.text = element_text(face = 'bold', size = 10),
    text = element_text(family = 'serif'),
  )

ggsave('figures/04_temp_ridge_plot_faceted.png', width = 6.5, height = 6.5)