library(shiny)
library(leaflet)
library(sf)
library (tmap)
library (tmaptools)
library (dplyr)

# VA counties - downloaded via the awesome tigris package
shape <- tigris::counties(state = "VA", class = "sf")

# Define UI 
ui <- fluidPage(
    
    # Application title
    titlePanel("Virginia School County Ranking"),
    
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
            leafletOutput("my_tmap")
            )
        )
    )
)

# Define server logic       
server <- function(input, output, session) {
    
    output$my_tmap <- renderLeaflet({
        # Data Used
        mydata <- score_data
        mymap <- st_read("tl_2016_51_cousub.shp", stringsAsFactors = FALSE)
        str(mymap)
        # Joining Data 
        map_and_data <- inner_join(mymap, mydata)
        str(map_and_data)
        # Creating tmap 
        tm <- tm_shape(map_and_data)+ tm_polygons("TotalScore", id = "County", pallete = "Greens", zindex = 401)
        tmap_leaflet(tm)
    })
    
    observeEvent(input$action, {
        
      
        score_data$DynamicScore <- ((input$SOL_slider * score_data$SOLPassRate +
                                         input$SOLADV_slider * score_data$SOLAdvPassRate +
                                         input$HSG_slider * score_data$HighGrad +
                                         input$CP_slider * score_data$ChildPoverty +
                                         input$HF_slider * score_data$HealthyFoods +
                                         input$CM_slider * score_data$Child_Mortality +
                                         input$SHI_slider * score_data$Severe_Housing_Issues +
                                         input$WB_slider * score_data$WhiteBlackSeg +
                                         input$WNW_slider * score_data$WhiteNonWhiteSeg)/
                                        ( input$SOLADV_slider+ input$SOLADV_slider+ input$WNW_slider+input$WB_slider+input$SHI_slider+input$CM_slider+input$HF_slider+input$CP_slider+input$HSG_slider))
        
        mydata_2 <- score_data
        mymap_2 <- st_read("tl_2016_51_cousub.shp", stringsAsFactors = FALSE)
        str(mymap_2)
        # Joining Data 
        map_and_data_2 <- inner_join(mymap_2, mydata_2)
        
        # Creating tmap 
        tmapProxy("my_tmap", session, {
        tm_remove_layer(401) +
        tm_shape(map_and_data_2) +
        tm_polygons("DynamicScore", id = "County", pallete = "Greens", zindex = 401)
            })
        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)