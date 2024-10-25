


library(shiny)
library(bs4Dash)
library(leaflet)
library(mapdeck)
library(highcharter)
library(rsconnect)
library(leaflet.extras2)
library(DT)
library(shinyWidgets)
library(waiter)

# This is meant to run Hackathon_code_ideas.RMD first and then run this file 

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
    id = "sidebar",
    minified = F,
    width = 350,
    bs4SidebarMenu( ### Sidebar Menu
      bs4SidebarMenuItem("Data Exploration", tabName="tabExploration"),
      bs4SidebarMenuItem("Prediction", tabName="tabPrediction")
    )
  ),
  # ------------------------------- End Sidebar
  
  # ------------------------------- Body
  body = bs4DashBody(
    
    useWaiter(),
    autoWaiter(html = spin_loader()),
    bs4TabItems(
      #-------------------- Data Exploration Tab
      bs4TabItem(
        tabName= "tabExploration",
        fluidRow(
          bs4Dash::column(
            width = 5,
            bs4Card(
              title = "Some Chart",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              background = "navy",
              id = "somechart"
            )
          ),
          bs4Dash::column(
            width = 7,
            bs4Card(
              title = "Some other Chart",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              background = "navy",
              id = "someotherchart"
            )
          )
        )
      ), 
      #-------------------- End Data Exploration Tab
      
      
      #-------------------- Data Prediction Tab
      bs4TabItem(
        tabName= "tabPrediction",
        fluidRow(
          bs4Dash::column(
            width = 12,
            bs4Card(
              title = "Some Inputs",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "550px",
              maximizable = T,
              background = "navy",
              id = "somechart",
              radioGroupButtons(
                inputId = "inputResponse",
                label = "Choose Response",
                choices = c("Irrigation", 
                            "Moisture"),
                justified = TRUE,
                selected = "Irrigation"
              ),
              sliderInput(
                inputId = "inputIrrorMoist",
                label = "Select Irrigation",
                min = 0,
                max = 5,
                value = 0
              ),
              sliderInput(
                inputId = "inputInitialMoist",
                label = "Initial Moisture",
                min = 0,
                max = 1,
                value = 0
              ),
              sliderInput(
                inputId = "inputDepth",
                label = "Depth",
                min = 0,
                max = 15,
                value = 0
              ),
              radioGroupButtons(
                inputId = "Id073",
                label = "Select Sensor",
                choices = c("AquaSpy", "CropX", "Sentek"),
                individual = TRUE,
                checkIcon = list(
                  yes = tags$i(class = "fa fa-circle", 
                               style = "color: steelblue"),
                  no = tags$i(class = "fa fa-circle-o", 
                              style = "color: steelblue"))
              )
              
              
            )
            
          )
        )
      )
      
      #-------------------- End Data Prediction Tab
    )
    
    
    tags$style(HTML(".content-wrapper {  background-image: url('Folder2.png'); background-size: contain; background-repeat: no-repeat;  background-position: center;width: 100vw;
    height: 100vh; }"))
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

  observeEvent(input$inputResponse,
               {
                 
                 if(input$inputResponse == "Moisture") {
                   updateSliderInput(
                     inputId = "inputIrrorMoist", 
                     label = "Select Irrigation",
                     min = 0,
                     max = 5,
                     step = 0.01
                   )
                 } else if (input$inputResponse == "Irrigation") {
                   updateSliderInput(
                     inputId = "inputIrrorMoist",
                     label = "Desired Moisture",
                     min = 0,
                     max = 1,
                     step = 0.01,
                     value = 0
                   )
                 }
                 
                 
               })


   
}

# Run the application 
shinyApp(ui = ui, server = server)




  
