#Tasks 1 and 3 require the two csv files to be reshaped, mutated & merged

#I. I reshape temp_data & then parse/convert mXX_dZZ strings to date-month values

#II. I add an additional column to geospatial_data for extra-tropical Northern 
#Hemisphere, extra-tropical Southern Hemisphere & the tropics

#III. I merge the two csv files to create a single dataframe, providing the basis 
#of my analysis/exhibits

#Note I regularly was checking the data for issues using head(), View() etc.
#In order not to reopen all these dataframes when the code is run, I have hashed
#them in where possible



#In this script I create exhibits for Tasks 1-3

#I. For Task 1 I compute the daily weighted temperature averages for population 
#and area for each region

#II. For Task 1, q.1 I create a table using stargazer to summarize the 
#distribution of daily temperature in 2015 for the three regions (depicting both 
#the means and variances)

#III. For Task 1, q.1 I create a plot depicting how the weighted daily 
#temperatures (for both population and area) vary through time for each region

#IV. For Task 2, q. 1 I create a plot using simulated data to elucidate 
#relevant differences betwene using OLS and poisson regression models to fit
#daily ILI-temperature data

#V. For Task 3 q.1 I use sf and rnaturalearth to create world temperature 
#heatmaps for annual weighted temperature averages for population and area

#VI. For TAsk 3 q.2 I use ggridges to create a ridgeline plot for monthly 
#weighted average temperatures for each region
