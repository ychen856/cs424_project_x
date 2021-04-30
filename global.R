library(stringr)
library(countrycode)
library(dplyr)
################### read file ####################
power_plant_df <- read.csv("global_power_plant_database.csv", sep = ",", header = TRUE)
power_plant_df <- power_plant_df[-c(9:24)]

power_plant_df$continent <- countrycode(sourcevar = power_plant_df[, "country_long"],
                            origin = "country.name",
                            destination = "continent")

power_plant_df <- power_plant_df %>% 
  mutate(continent2 = if_else(country_long %in% c("United States of America", "Mexico", "Canada", "Guatemala", "Cuba", "Haiti", "Dominican Republic", "Honduras", "Nicaragua", 
                                                  "El Salvador", "Costa Rica", "Panama", "Jamaica", "Trinidad and Tobago", "Belize", "Bahamas", "Barbados", "Saint Lucia", 
                                                  "Grenada", "Saint Vincent and the Grenadines", "Antigua and Barbuda", "Dominica", "Saint Kitts and Nevis"), "North America", ""
                              ))
power_plant_df <- power_plant_df[order(-power_plant_df$capacity_mw),]

######################## input list ##########################
energySource_dist <- c("Select All", "Hydro", "Gas", "Oil", "Wind", "Nuclear", "Coal", "Solar", "Waste", "Biomass", "Wave and Tidal", "Petcoke", "Geothermal", "Cogeneration", "Storage", "Other")
continent_dist <- c("North America", "Asia", "Europe", "Africa", "Americas", "Oceania")


#######################  location ########################
continent_location_df <- data.frame (continent  = c("North America", "Asia", "Europe", "Africa", "Americas", "Oceania"),
                                     longtitude = c(-96.777522, 85.321201, 80.983931, 19.952600, -80.726134, 142.361791),
                                     latitude = c(43.879495, 29.911902, 55.787703, -0.873944, 22.974072, -27.473599),
                                     zoom = c(3, 3, 3, 3, 2.5, 4)
)
 



