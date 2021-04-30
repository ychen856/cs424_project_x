library(shiny)
library(leaflet)
library(shinyWidgets)
source("global.R")



#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(class = "p-0 m-0",
    includeCSS("www/dashboard.css"),
    tags$head(
        tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
    ),
    # Application title
    title = "CS424 Project 2",
        tags$nav(class = "head shadow p-0 m-0 pl-0",
            tags$ul(class = "title p-0 mr-auto mt-0 mb-0",
                tags$li(
                    tags$p("CS424 Project X")
                )
            ),
            tags$ul(class = "name p-0",
                tags$li(class = "text-lg mt-auto mb-auto", "Yi-Chun Chen")
            )
        ),
        tags$div(class = "p-0",
            navbarPage("",
                #energy plants location start
                tabPanel("Global Power Plants", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            column(12, class = "p-0",
                                tags$div(class = "card border-title shadow",
                                    #card Start
                                    tags$div(class = "card-body",
                                        tags$div(
                                            column(7,
                                                tags$div(class = "title",
                                                    tags$span("Power Plants")
                                                )
                                            ),
                                            column(1,
                                                tags$div(class = "filter",
                                                        checkboxInput("hide", label = "Hide selected", value = FALSE)
                                                )
                                            ),
                                            column(4,
                                                tags$div(class = "cust-text",
                                                    sliderInput("slider", "Capacity Range:",
                                                        value = c(0, 22500), min = 0, max = 22500),
                                                )
                                            )
                                        ),
                                        fluidRow(style = "margin: 2px",
                                            column(2, style = "background-color: white",
                                                tags$div(
                                                    tags$div(class = "subtitle",
                                                        tags$i(class = "fas fa-search"),
                                                            "Data Filter:"
                                                    ),
                                                    tags$div(class = "filter",
                                                        selectizeInput("continent", "Continent: ", choices = c(continent_dist), selected = "North America")
                                                    ), #energy source filter end
                                                    #Energy source filter start
                                                    tags$div(class = "filter",
                                                        checkboxGroupInput("energySourceInput", "Energy source: ", choices = c(energySource_dist), selected = "Select All")
                                                    ), #energy source filter end
                                                    actionButton("reset", "Reset view")
                                                )
                                            ),
                                            column(10,
                                                tags$div(class = "row",
                                                    column(12,
                                                        tags$div(class = "subtitle",
                                                            tags$i(class = "fas fa-map-marked-alt"),
                                                                "Map:"
                                                        ),
                                                        tags$div(style = "height: 650px",
                                                            shinycssloaders::withSpinner(
                                                                leafletOutput("leaf", height = 630),
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        ) #End of fluid row
                                    ) #End of card body
                                ) #End of card
                            )
                        )
                    )
                ), #energy plants location end
                #About page start
                tabPanel("About", class = "p-0",
                    mainPanel(class = "panel p-0",
                        fluidRow(
                            #Total Amount of Energy generation start
                                column(12, class = "p-0",
                                    tags$div(class = "card border-title shadow",
                                        #card Start
                                        tags$div(class = "card-body",
                                            tags$div(class = "title",
                                                tags$span("About")
                                        ),
                                        tags$div(class = "p-5",
                                             tags$div(
                                                tags$span(class = "cust-text-md", "Author: "),
                                                tags$span("Yi-Chun Chen")
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Date: "),
                                                tags$span("03.15.2020")
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Data Source: "),
                                                tags$a(href = "https://datasets.wri.org/dataset/globalpowerplantdatabase", "https://datasets.wri.org/dataset/globalpowerplantdatabase"),
                                            ),
                                            tags$div(
                                                tags$span(class = "cust-text-md", "Git Repository: "),
                                                tags$a(href = "https://github.com/ychen856/cs424_project_x.git", "https://github.com/ychen856/cs424_project_x.git")
                                            )
                                        ),
                                    )
                                )
                            )
                        )
                    )
                ) #About page end
            )
        )
    )





