library(shiny)
library(leaflet)
library(sf)

# VA counties - downloaded via the awesome tigris package
shape <- tigris::counties(state = "VA", class = "sf")


# Define UI 
ui <- fluidPage(
    
    # Application title
    titlePanel("The Old Dominion"),
    
    # Top panel with county name
    verticalLayout(
        
        wellPanel(textOutput("cnty")),
    
        sidebarLayout(
            sidebarPanel(
                h3("Factors"),
                h6("Assign a weight value for every factor and press the 'find!' button"),
                sliderInput("SOL_slider", 
                            label = h4("SOL Pass Rate"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("SOLADV_slider", 
                            label = h4("SOL Advanced Pass Rate"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("HSG_slider", 
                            label = h4("High School Gradaution Rates"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("CP_slider", 
                            label = h4("Children in Poverty"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("HF_slider", 
                            label = h4("Limited Access to Healthy Food"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("CM_slider", 
                            label = h4("Child Mortality"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("SHI_slider", 
                            label = h4("Severe Housing Issues"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("WB_slider", 
                            label = h4("White/Black Segregation"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                sliderInput("WNW_slider", 
                            label = h4("White/Non-White Segregation"), 
                            min = 0, 
                            max = 10, 
                            value = 0),
                actionButton("action", label = "Find!")),
                
        # the map itself
        mainPanel(
            leafletOutput("map")
            )
        )
    )
)

# Define server logic       
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Stamen.Toner") %>% 
            addPolygons(data = shape, 
                        fillColor = "aliceblue", 
                        color = "grey",
                        layerId = ~COUNTYNS)
    })
    
    # Clicking the map for name
    observe({ 
        event <- input$map_shape_click
        output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)