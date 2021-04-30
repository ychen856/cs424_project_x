source("global.R")

getMapTable <- function(data, stateInput, energySourceInput) {
  if(stateInput == "US") {
    gen_year_state <- data
  }
  else {
    gen_year_state <- subset(data, STATE == stateInput)
  }
  slice_gen_year_state <- data.frame(matrix(ncol = 6, nrow = 0))
  slice_gen_year_state_col <- c("gppd_idnr", "name", "longitude", "latitude" , "capacity_mw", "primary_fuel")
  colnames(slice_gen_year_state) <- slice_gen_year_state_col
  slice_gen_year_state$longitude <- as.double(slice_gen_year_state$PLANT_LONG)
  slice_gen_year_state$latitude <- as.double(slice_gen_year_state$PLANT_LAT)
  slice_gen_year_state$capacity_mw <- as.numeric(slice_gen_year_state$GEN)
  
  #Hydro
  if("Hydro" %in% energySourceInput) {
    coal_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "COAL_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    coal_plants_table<-coal_plants_table[!(is.na(coal_plants_table$COAL_GEN)),]
    names(coal_plants_table)[names(coal_plants_table) == "COAL_GEN"] <- c("GEN")
    
    if(nrow(coal_plants_table) > 0) {
      coal_plants_table$SOURCE <- "Hydro"
      slice_gen_year_state <- rbind(slice_gen_year_state, coal_plants_table)
    }
  }
  
  #Gas
  if("Gas" %in% energySourceInput) {
    oil_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "OIL_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    oil_plants_table <- oil_plants_table[!(is.na(oil_plants_table$OIL_GEN)),]
    names(oil_plants_table)[names(oil_plants_table) == "OIL_GEN"] <- c("GEN")
    
    if(nrow(oil_plants_table) > 0) {
      oil_plants_table$SOURCE <- "Gas"
      slice_gen_year_state <- rbind(slice_gen_year_state, oil_plants_table)
    }
  }
  
  #Gas
  if("Gas" %in% energySourceInput) {
    gas_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "GAS_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    gas_plants_table <- gas_plants_table[!(is.na(gas_plants_table$GAS_GEN)),]
    names(gas_plants_table)[names(gas_plants_table) == "GAS_GEN"] <- c("GEN")
    
    if(nrow(gas_plants_table) > 0) {
      gas_plants_table$SOURCE <- "Gas"
      slice_gen_year_state <- rbind(slice_gen_year_state, gas_plants_table)
    }
  }
  
  #Nuclear
  if("Nuclear" %in% energySourceInput) {
    nuclear_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "NUCLEAR_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    nuclear_plants_table <- nuclear_plants_table[!(is.na(nuclear_plants_table$NUCLEAR_GEN)),]
    names(nuclear_plants_table)[names(nuclear_plants_table) == "NUCLEAR_GEN"] <- c("GEN")
    
    if(nrow(nuclear_plants_table) > 0) {
      nuclear_plants_table$SOURCE <- "Nuclear"
      slice_gen_year_state <- rbind(slice_gen_year_state, nuclear_plants_table)
    }
  }
  
  #Hydro
  if("Hydro" %in% energySourceInput) {
    hydro_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "HYDRO_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    hydro_plants_table <- hydro_plants_table[!(is.na(hydro_plants_table$HYDRO_GEN)),]
    names(hydro_plants_table)[names(hydro_plants_table) == "HYDRO_GEN"] <- c("GEN")
    
    if(nrow(hydro_plants_table) > 0) {
      hydro_plants_table$SOURCE <- "Hydro"
      slice_gen_year_state <- rbind(slice_gen_year_state, hydro_plants_table)
    }
  }
  
  #Biomass
  if("Biomass" %in% energySourceInput) {
    biomass_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "BIOMASS_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    biomass_plants_table <- biomass_plants_table[!(is.na(biomass_plants_table$BIOMASS_GEN)),]
    names(biomass_plants_table)[names(biomass_plants_table) == "BIOMASS_GEN"] <- c("GEN")
    
    if(nrow(biomass_plants_table) > 0) {
      biomass_plants_table$SOURCE <- "Biomass"
      slice_gen_year_state <- rbind(slice_gen_year_state, biomass_plants_table)
    }
  }
  
  #Wind
  if("Wind" %in% energySourceInput) {
    wind_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "WIND_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    wind_plants_table <- wind_plants_table[!(is.na(wind_plants_table$WIND_GEN)),]
    names(wind_plants_table)[names(wind_plants_table) == "WIND_GEN"] <- c("GEN")
    
    if(nrow(wind_plants_table) > 0) {
      wind_plants_table$SOURCE <- "Wind"
      slice_gen_year_state <- rbind(slice_gen_year_state, wind_plants_table)
    }
  }
  
  #Solar
  if("Solar" %in% energySourceInput) {
    solar_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "SOLAR_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    solar_plants_table <- solar_plants_table[!(is.na(solar_plants_table$SOLAR_GEN)),]
    names(solar_plants_table)[names(solar_plants_table) == "SOLAR_GEN"] <- c("GEN")
    
    if(nrow(solar_plants_table) > 0) {
      solar_plants_table$SOURCE <- "Solar"
      slice_gen_year_state <- rbind(slice_gen_year_state, solar_plants_table)
    }
  }
  
  #Geothermal
  if("Geothermal" %in% energySourceInput) {
    geothermal_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "GEOTHERMAL_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    geothermal_plants_table <- geothermal_plants_table[!(is.na(geothermal_plants_table$GEOTHERMAL_GEN)),]
    names(geothermal_plants_table)[names(geothermal_plants_table) == "GEOTHERMAL_GEN"] <- c("GEN")
    
    if(nrow(geothermal_plants_table) > 0) {
      geothermal_plants_table$SOURCE <- "Geothermal"
      slice_gen_year_state <- rbind(slice_gen_year_state, geothermal_plants_table)
    }
  }
  
  #Other
  if("Other" %in% energySourceInput) {
    other_plants_table <- gen_year_state[c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "OTHER_GEN", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    other_plants_table <- other_plants_table[!(is.na(other_plants_table$OTHER_GEN)),]
    names(other_plants_table)[names(other_plants_table) == "OTHER_GEN"] <- c("GEN")
    
    if(nrow(other_plants_table) > 0) {
      other_plants_table$SOURCE <- "Other"
      slice_gen_year_state <- rbind(slice_gen_year_state, other_plants_table)
    }
  }
  
  if("Select All" %in% energySourceInput) {
    unknown_plants_table <- gen_year_state[is.na(gen_year_state$COAL_GEN) 
                                                & is.na(gen_year_state$OIL_GEN)
                                                & is.na(gen_year_state$GAS_GEN)
                                                & is.na(gen_year_state$NUCLEAR_GEN)
                                                & is.na(gen_year_state$HYDRO_GEN)
                                                & is.na(gen_year_state$BIOMASS_GEN)
                                                & is.na(gen_year_state$WIND_GEN)
                                                & is.na(gen_year_state$SOLAR_GEN)
                                                & is.na(gen_year_state$GEOTHERMAL_GEN)
                                                & is.na(gen_year_state$OTHER_GEN),
                                         ][c("ORIS_CODE", "PLANT_NAME", "PLANT_LONG", "PLANT_LAT", "TOTAL_RENEWABLE_PER", "TOTAL_NONRENEWABLE_PER")]
    if(nrow(unknown_plants_table) > 0) {
      unknown_plants_table$GEN <- 0
      unknown_plants_table$SOURCE <- "Unknown"
      slice_gen_year_state <- rbind(slice_gen_year_state, unknown_plants_table)
    }
  }
  
  
  
  #order generation, let smaller circle at the top layer
  slice_gen_year_state <- slice_gen_year_state[order(-slice_gen_year_state$GEN),]
  
  slice_gen_year_state$U_PLANT_LAT <- jitter(slice_gen_year_state$PLANT_LAT, factor = 0.1)
  slice_gen_year_state$U_PLANT_LONG <- jitter(slice_gen_year_state$PLANT_LONG, factor = 0.1)
  
  return(slice_gen_year_state)
}

getTableByYear <- function(yearInput) {
  if(yearInput == "2000") {
    return (gen_2000)
  }
  else if (yearInput == "2010") {
    return (gen_2010)
  }
  else if (yearInput == "2018") {
    return (gen_2018)
  }
}

getSliceIdleNewTable <- function(yearInput, plantTypeInput, energySourceInput) {
  if(yearInput == "2010") {
    gen_year_exist <- old_2010
    gen_year_idle <- idle_2010
    gen_year_new <- new_2010
    gen_year_all <- gen_2010
  }
  else {
    gen_year_exist <- old_2018
    gen_year_idle <- idle_2018
    gen_year_new <- new_2018
    gen_year_all <- gen_2018
  }
  
  gen_year <- gen_year_all
  slice_gen <- getMapTable(gen_year, "US", energySourceInput)
  
  if("New Plants" %in% plantTypeInput && "Idle Plants" %in% plantTypeInput) {
    slice_gen_new <- getMapTable(gen_year_new, "US", energySourceInput)
    slice_gen <- getMapTable(gen_year_idle, "US", energySourceInput)
    slice_gen <- rbind(slice_gen, slice_gen_new)
  }
  else if("New Plants" %in% plantTypeInput) {
    gen_year <- gen_year_new
    slice_gen <- getMapTable(gen_year, "US", energySourceInput)
  }
  else if("Idle Plants" %in% plantTypeInput) {
    gen_year <- gen_year_idle
    slice_gen <- getMapTable(gen_year, "US", energySourceInput)
  }
  return (slice_gen)
}