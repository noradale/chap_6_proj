#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Virginia County Rankings"),
  
  # Sidebar with a slider input for importance weight 
  sidebarLayout(
    sidebarPanel(
      h2('Factor Weightings'),
      sliderInput("foodaccess",
                  "Limited Access to Healthy Foods:",
                  min = 0,
                  max = 10,
                  value = 7),
      sliderInput("child_poverty",
                  "Children in Poverty:",
                  min = 0,
                  max = 10,
                  value = 9),
      sliderInput("structure_Age",
                  "Age of Structure:",
                  min = 0,
                  max = 10,
                  value = 7),
      sliderInput("grad_rate",
                  "High School Graduation Rate:",
                  min = 0,
                  max = 10,
                  value = 5),
      sliderInput("seg_nonwhite",
                  "Residential Segregation White/NonWhite",
                  min = 0,
                  max = 10,
                  value = 9),
      sliderInput("seg_black",
                  "Residential Segregation White/Black:",
                  min = 1,
                  max = 10,
                  value = 8),
      sliderInput("child_mortality",
                  "Child Mortality",
                  min = 1,
                  max = 10,
                  value = 9),
      sliderInput("housing_issues",
                  "Severe Housing Issues",
                  min = 1,
                  max = 10,
                  value = 9),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("mymap"),
      dataTableOutput("data")
      
    )
  )
)



# Run the application 
shinyApp(ui = ui, server = server)
