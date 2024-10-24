


library(shiny)
library(bs4Dash)
library(leaflet)
library(mapdeck)
library(highcharter)
library(rsconnect)
library(leaflet.extras2)
library(DT)
library(shinyWidgets)


ui <- bs4DashPage(
  
  # ------------------------------- Header
  header = bs4DashNavbar(
    title = "StatFlow Dashboard",
    compact = T,
    fixed = T
  ),
  # ------------------------------- End Header
  
  # ------------------------------- Sidebar
  sidebar = bs4DashSidebar(
    
  ),
  # ------------------------------- End Sidebar
  
  # ------------------------------- Body
  body = bs4DashBody(
    
  ),
  # ------------------------------- End Body
  
  # ------------------------------- Control bar
  controlbar = bs4DashControlbar(
    
  ),
  # ------------------------------- End Control bar
  
  
  # ------------------------------- Footer
  footer = bs4DashFooter(
    
  )
  # ------------------------------- End Footer
 
)


server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)



  