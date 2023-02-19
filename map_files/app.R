library(sf)
library(shiny)
library(leaflet)

scot_hb_shapefile <- st_read(here::here("map_files/sf_hb_discharge_mort.shp"))


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Cerebrovascular Disease in Scotland"),
    
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
      "Health Board: ", sf_hb_mort_data$HBName,"<br/>", 
      "EASR: ", sf_hb_mort_data$easr, "<br/>", 
      "Deaths: ", sf_hb_mort_data$number_of_deaths, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # originally used colorQuantile but colorNumeric creates a continuous color range 
    # for continuous input which is what we have. 
    pal_mort_hb <- colorNumeric(
      palette = "Purples",
      domain = sf_hb_mort_data$easr)
    
    leaflet(sf_hb_mort_data) %>% 
      addTiles()  %>% 
      setView( lat=57, lng=-5 , zoom=6) %>%
      addPolygons(stroke = TRUE, 
                  color = "black", 
                  weight = 1,
                  fillOpacity = 0.8, 
                  #          smoothFactor = 0.5, 
                  #          fillColor = ~colorQuantile("Purples", easr)(easr) ,
                  fillColor = ~pal_mort_hb(easr),
                  label = mytext_mort_hb,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto",
                    opacity = 0.75))
  }) 
  
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
