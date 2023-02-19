library(sf)
library(shiny)
library(leaflet)
library(tidyverse)


sf_hb_discharge_mort <- st_read("sf_hb_discharge_mort.shp")

# ESRI driver renames columns when writing file so have to rename them here
sf_hb_discharge_mort <- sf_hb_discharge_mort %>% 
  rename("mortality_easr" = "mrtlty_",
         "num_deaths" = "nmbr_f_dt")





# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
      tabPanel("Health Board",
               fluidRow(br()
               ),
               fluidRow(
                 column(width = 4, 
               
                 leafletOutput("hb_map")
                 )
      )
      ),
      tabPanel("Council Area",
               fluidRow(br()
               ),
               fluidRow(
                 leafletOutput("ca_map")
               )
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$hb_map <- renderLeaflet({
    
   mytext_mort_hb <- paste(
      "Health Board: ", sf_hb_discharge_mort$HBName,"<br/>", 
      "EASR: ", sf_hb_discharge_mort$mortality_easr, "<br/>", 
      "Deaths: ", sf_hb_discharge_mort$num_deaths, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # originally used colorQuantile but colorNumeric creates a continuous color range 
    # for continuous input which is what we have. 
    pal_mort_hb <- colorNumeric(
      palette = "Purples",
      domain = sf_hb_discharge_mort$mortality_easr)
    
    leaflet(sf_hb_discharge_mort) %>% 
      addTiles()  %>% 
      setView( lat=57, lng=-5 , zoom=6) %>%
      addPolygons(stroke = TRUE, 
                  color = "black", 
                  weight = 1,
                  fillOpacity = 0.8, 
                    smoothFactor = 0.5, 
                 fillColor = ~pal_mort_hb(mortality_easr),
                  label = mytext_mort_hb,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto",
                    opacity = 0.75)
)
  }) 
  
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
