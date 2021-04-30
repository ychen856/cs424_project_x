library(stringr)
library(countrycode)
################### read file ####################
#2018
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

#power_plant_df <- subset(power_plant_df, country_long == "Afghanistan")
################### Data modification ##################
#2018
#gen_2018$PLANT_LAT <- as.double(gen_2018$PLANT_LAT)
#gen_2018$PLANT_LONG <- as.double(gen_2018$PLANT_LONG)


#gen_2018 <- subset(gen_2018, !(is.na(PLANT_LAT)|is.na(PLANT_LONG)))

#gen_2018$COAL_GEN <- sub("^-", "^$", gen_2018$COAL_GEN)
#gen_2018$OIL_GEN <- sub("^-", "^$", gen_2018$OIL_GEN)
#gen_2018$GAS_GEN <- sub("^-", "^$", gen_2018$GAS_GEN)
#gen_2018$NUCLEAR_GEN <- sub("^-", "^$", gen_2018$NUCLEAR_GEN)
#gen_2018$HYDRO_GEN <- sub("^-", "^$", gen_2018$HYDRO_GEN)
#gen_2018$BIOMASS_GEN <- sub("^-", "^$", gen_2018$BIOMASS_GEN)

#gen_2018$COAL_GEN <- ifelse(gen_2018$COAL_GEN == 0, NA, gen_2018$COAL_GEN)
#gen_2018$OIL_GEN <- ifelse(gen_2018$OIL_GEN == 0, NA, gen_2018$OIL_GEN)



######################## input list ##########################
energySource_dist <- c("Select All", "Hydro", "Gas", "Oil", "Wind", "Nuclear", "Coal", "Solar", "Waste", "Biomass", "Wave and Tidal", "Petcoke", "Geothermal", "Cogeneration", "Storage", "Other")
continent_dist <- c("North America", "Asia", "Europe", "Africa", "Americas", "Oceania")

#year_dist <- c("2000", "2010", "2018")
#state_dist <- state.name
#map_dist <- c("Light", "Dark", "Terrain")

#source_idle_new <- c("New Plants", "Idle Plants")

####################### state location ########################
continent_location_df <- data.frame (continent  = c("North America", "Asia", "Europe", "Africa", "Americas", "Oceania"),
                                     longtitude = c(-96.777522, 85.321201, 80.983931, 19.952600, -80.726134, 142.361791),
                                     latitude = c(43.879495, 29.911902, 55.787703, -0.873944, 22.974072, -27.473599),
                                     zoom = c(3, 3, 3, 3, 2.5, 4)
)
 
#state_location <- data.frame (state  = c("US", "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA","MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
#                  longtitude = c(-94.6, -86.902298, -153.369141, -111.093735, -92.199997, -119.417931, -105.358887, -72.699997, -75.5, -81.760254, -83.441162, -155.844437, -114.742043, -89, -86.126976, -93.581543, -98, -85.270020, -92.329102, -68.972168, -76.641273, -71.382439, -84.506836, -94.636230, -90, -92.603760, -109.533691, -100, -117.224121, -71.5, -74.871826, -106.018066, -75, -80.793457, -100.437012, -82.996216, -96.921387, -120.5, -77.194527, -71.5, -81.163727,-100,-86.660156, -100, -111.950684, -72.699997, -78.024902, -120.740135, -80.5, -89, -107.290283),
#                  latitude = c(39, 32.318230, 66.160507, 34.048927, 34.799999, 36.778259, 39.113014, 41.599998, 39, 27.994402, 33.247875, 19.741755, 44.068203, 40, 40.273502, 42.032974, 38.5, 37.839333, 30.391830, 45.367584, 39.045753, 42.407211, 44.182205, 46.392410, 33, 38.573936, 46.965260, 41.5, 39.876019, 44, 39.833851, 34.307144, 43, 35.782169, 47.650589	, 40.367474, 36.084621, 44, 41.203323, 41.700001, 33.836082, 44.500000, 35.860119, 31, 39.419220, 44.000000, 37.926868, 47.751076, 39, 44.5, 43.075970),
#                  zoom = c(5, 7, 5, 6, 7, 6, 7, 9, 8, 6, 7, 7, 6, 6.5, 7, 7, 6.5, 6.5, 6.5, 6.5, 6.5, 7.5, 6, 6, 7, 6.5, 6, 6, 6, 8, 8, 7, 6.5, 6.5, 7, 7, 7, 7, 6.5, 9, 7, 6.5, 6, 6, 7, 8, 7, 6.5, 7, 7, 6)
#)



