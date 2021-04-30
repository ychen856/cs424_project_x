library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(fontawesome)
library(stringr)
library(ggplot2)
library(tidyverse)
library(fiftystater)
library(scales)
library(usmap)
library(DT)
library(shinycssloaders)
source("global.R")
source("utility.R")


# Define server logic required to draw a histogram
function(input, output, session) {

  ################ page 1 ###################
  #Data Filter
  #energy source input check box
  observe({
    mapData <- power_plant_df
    print("fresh")
    if("Select All" %in% input$energySourceInput)  {
      updateCheckboxGroupInput(session,"energySourceInput", selected=c("Select All", energySource_dist))
    }
    
    if(input$continent == "North America") {
      mapData <- subset(mapData, continent2 == "North America")
    }
    else {
      mapData <- subset(mapData, continent == input$continent)
    }
    mapData <- subset(mapData, primary_fuel %in% input$energySourceInput)
    
    print(input$hide)
    
    if(input$hide == TRUE) {
      temp <- subset(mapData, capacity_mw <= input$slider[1])
      mapData <- subset(mapData, capacity_mw >= input$slider[2])
      mapData <- rbind(temp, mapData)
    }
    else {
      mapData <- subset(mapData, capacity_mw >= input$slider[1])
      mapData <- subset(mapData, capacity_mw <= input$slider[2])
    }
    
    m <- getLeafletMap(mapData, input$continent)
    
    output$leaf <- renderLeaflet({
      m
    })
  })

  
  #reset button first
  observeEvent(input$reset, {
    mapData <- power_plant_df
    print("fresh")
    if("Select All" %in% input$energySourceInput)  {
      updateCheckboxGroupInput(session,"energySourceInput", selected=c("Select All", energySource_dist))
    }
    
    
    if(input$continent == "North America") {
      mapData <- subset(mapData, continent2 == "North America")
    }
    else {
      mapData <- subset(mapData, continent == input$continent)
    }
    mapData <- subset(mapData, primary_fuel %in% input$energySourceInput)
    
    if(input$hide == TRUE) {
      temp <- subset(mapData, capacity_mw <= input$slider[1])
      mapData <- subset(mapData, capacity_mw >= input$slider[2])
      mapData <- rbind(temp, mapData)
    }
    else {
      mapData <- subset(mapData, capacity_mw >= input$slider[1])
      mapData <- subset(mapData, capacity_mw <= input$slider[2])
    }
    
    m <- getLeafletMap(mapData, input$continent)
    
    output$leaf <- renderLeaflet({
      m
    })
  })

  #################### generate graph ####################
  getLeafletMap2 <- function(power_plant_df, continentInput) {
    pal <- colorFactor(palette = c("#004949","#009292","#ff6db6", "#490092", "#006ddb", "#b66dff", 
                         "#920000","#924900","#db6d00","#ffff6d", "#0000ff", "#00ff00", 
                         "#00ffff", "#ff0000", "#a9a9a9"),
                       domain = power_plant_df$primary_fuel)
    
    leaflet(power_plant_df) %>%
      addProviderTiles(
        providers$CartoDB.Positron, group = "Light"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter, group = "Dark"
      ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap, group = "Terrain"
      ) %>%
      setView(
        lng = subset(continent_location_df, continent == continentInput)$longtitude, 
        lat = subset(continent_location_df, continent == continentInput)$latitude,
        zoom = subset(continent_location_df, continent == continentInput)$zoom
      ) %>%
      addCircles(
        color = ~pal(primary_fuel),
        stroke = FALSE, 
        fillOpacity = 0.2,
        lng = ~longitude, 
        lat = ~latitude,
        weight = 1,
        radius = ~sqrt(capacity_mw/5)*3000,
        popup = ~paste("country name: ", country_long, "<br>",
                      "plant name: ", name, "<br>",
                      "capacity: ", capacity_mw, "<br>",
                      "primary fuel type: ",primary_fuel)
      )%>%
      addLegend("bottomright", pal = pal, values = ~primary_fuel, title="Energy Source") %>%
      addLayersControl(
        baseGroups = c("Light", "Dark", "Terrain"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  getLeafletMap <- function(power_plant_df, continentInput) {
    leaflet() %>% 
      addProviderTiles(
        providers$CartoDB.Positron, group = "Light"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter, group = "Dark"
      ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap, group = "Terrain"
      ) %>%
      setView(
        lng = subset(continent_location_df, continent == continentInput)$longtitude, 
        lat = subset(continent_location_df, continent == continentInput)$latitude,
        zoom = subset(continent_location_df, continent == continentInput)$zoom
      ) %>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Hydro"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000, 
                 fillOpacity=0.2,
                 color = "#004949",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Gas"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000, 
                 fillOpacity=0.2,
                 color = "#009292",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Oil"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#ff6db6",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Wind"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#490092",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Nuclear"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#006ddb",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Coal"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#b66dff",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Solar"),
                 lat = ~latitude,
                 lng = ~longitude, 
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#920000",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Waste"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#924900",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Biomass"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#db6d00",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Wave and Tidal"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#ffff6d",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Petcoke"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#0000ff",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Geothermal"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#00ff00",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Cogeneration"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#00ffff",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Storage"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#ff0000",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      )%>%
      addCircles(data = subset(power_plant_df, primary_fuel == "Other"),
                 lat = ~latitude,
                 lng = ~longitude,
                 weight = 1,
                 radius = ~sqrt(capacity_mw/5)*3000,
                 fillOpacity=0.2,
                 color = "#a9a9a9",
                 popup = ~paste("country name: ", country_long, "<br>",
                                "plant name: ", name, "<br>",
                                "capacity: ", capacity_mw, "<br>",
                                "primary fuel type: ",primary_fuel)
      ) %>%
      addLegend("bottomright", colors= c("#004949","#009292","#ff6db6", "#490092", "#006ddb", "#b66dff", 
                                          "#920000","#924900","#db6d00","#ffff6d", "#0000ff", "#00ff00", 
                                         "#00ffff", "#ff0000", "#a9a9a9"), 
                labels=c("Hydro", "Gas", "Oil", "Wind", "Nuclear", "Coal", 
                          "Solar", "Waste", "Biomass", "Wave and Tidal", "Petcoke", "Geothermal",
                          "Cogeneration", "Storage", "Other"), title="Energy Source") %>%
      addLayersControl(
        baseGroups = c("Light", "Dark", "Terrain"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
}
 

