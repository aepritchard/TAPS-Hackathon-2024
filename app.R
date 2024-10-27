


library(renv)
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
library(highcharter)
library(dplyr)
library(plyr)
library(fresh)

# Create Theme
theme <- fresh::create_theme(
  bs4dash_status(
    primary = "#512888"
  )
)
# This is meant to run Hackathon_code_ideas.RMD first and then run this file 
# To get the app ui only need to run ui server and shinyapp


all_data_sensors_moisture <- read.csv("https://www.dropbox.com/scl/fi/1gpmfwnwj6cesqjozvj1i/all_data_sensors_moisture.csv?rlkey=0qqmtdodrtbojgdiqzigho9ud&st=zxqdrjqe&dl=1")
merged_data <- read.csv("https://www.dropbox.com/scl/fi/1gpmfwnwj6cesqjozvj1i/all_data_sensors_moisture.csv?rlkey=0qqmtdodrtbojgdiqzigho9ud&st=wbqmjoi6&dl=1")

merged_data$Date = as.Date(merged_data$Timestamp)
all_data_sensors_moisture$Date = as.Date(all_data_sensors_moisture$Timestamp)

ui <- bs4DashPage(
  
  freshTheme = theme,
  
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
            width = 6,
            bs4Card(
              sidebar = bs4CardSidebar(
                id = "scatterplotsidebar",
                startOpen = T,
                width = 50,
                virtualSelectInput(
                  inputId = "hcspTeam",
                  label = "Select Team",
                  choices = unique(all_data_sensors_moisture$Team),
                  multiple = T,
                  selected = 3,
                  width = "200px"
                ),
                radioGroupButtons(
                  inputId = "hcspSensor",
                  label = "Select Sensor",
                  choices = c("Sentek", "AquaSpy", "CropX"),
                  selected = "CropX"
                ),
                radioGroupButtons(
                  inputId = "hcspSummaryLevel",
                  label = "Select Frequency",
                  choices = c("Continuous", "Daily_Average", "Weekly_Average"),
                  selected = "Weekly_Average"
                ),
                radioGroupButtons(
                  inputId = "hcspResponse",
                  label = "Select Y-Axis",
                  choices = c("Moisture", "Depth"),
                  selected = "Moisture"
                ),
                actionBttn(
                  inputId = "hcspReload",
                  label = "Reload",
                  style = "jelly", 
                  color = "danger"
                )
                
              ),
              title = "Scatterplot",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              status = "primary",
              id = "scatterplot",
              highchartOutput(outputId = "hcspPlot")
            )
          ),
          bs4Dash::column(
            width = 6,
            bs4Card(
              title = "Barchart",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              status = "primary",
              id = "barchart",
              highchartOutput(outputId = "hcbarchart")
            )
          ),
          bs4Dash::column(
            width = 6,
            bs4Card(
              title = "Piechart",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              status = "primary",
              id = "piechart",
              highchartOutput(outputId = "hcpiechart")
            )
          ),
          bs4Dash::column(
            width = 6,
            bs4Card(
              title = "Treemap",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "440px",
              maximizable = T,
              status = "primary",
              id = "treemap",
              highchartOutput(outputId = "hctreemap")
            )
          ),
          
        )
      ), 
      #-------------------- End Data Exploration Tab
      
      
      #-------------------- Data Prediction Tab
      bs4TabItem(
        tabName= "tabPrediction",
        fluidRow(
          bs4Dash::column(
            width = 5,
            bs4Card(
              title = "Some Inputs",
              width = NULL,
              collapsible = T,
              headerBorder = T,
              height = "550px",
              maximizable = T,
              status = "primary",
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
    ),
    
    tags$style(HTML(".content-wrapper {  background-image: url('Folder2.png'); background-size: fit-content; background-repeat: no-repeat;  background-position: center; }"))
    
    
  ),
  # ------------------------------- End Body
  
  # ------------------------------- Control bar
  controlbar = bs4DashControlbar(
    id = "controlbar",
    width = 1200,
    DTOutput("table")
  ),
  # ------------------------------- End Control bar
  
  
  # ------------------------------- Footer
  footer = bs4DashFooter(
    
  )
  # ------------------------------- End Footer
  
)


server <- function(input, output, session) {
  
  
  # ----------------------------- Tab Exploration (TE)
  
  # ----------------------- TE: Scatterplot 
  # Reactive Data Scatterplot 
  data.react.hscpscatterplot <- reactive({
    all_data_sensors_moisture %>% filter(
      Team %in% input$hcspTeam, Sensor == input$hcspSensor,
      Summarization_level == input$hcspSummaryLevel
    )
  })
  
  
  
  observeEvent(
    eventExpr = {
      input$hcspSummaryLevel
      input$hcspResponse
    },
    handlerExpr = {
      if(input$hcspResponse == "Moisture") {
        if(input$hcspSummaryLevel == "Continuous") {
          output$hcspPlot <- renderHighchart(
            expr = {
              data.react.hscpscatterplot() %>% 
                hchart("column", hcaes(x = "Timestamp", y = "Moisture", group = "Team")) %>%
                hc_navigator(enabled = T) %>%
                hc_add_theme(hc_theme_economist()) %>%
                hc_xAxis(title = list(text = "Date")) %>%
                hc_yAxis(title = list(text = "Total Moisture"))
            }
          )
        } else if(input$hcspSummaryLevel %in% c("Daily_Average", "Weekly_Average")) {
          output$hcspPlot <- renderHighchart(
            expr = {
              data.react.hscpscatterplot() %>% 
                hchart("column", hcaes(x = "Date", y = "Moisture", group = "Team")) %>%
                hc_navigator(enabled = T) %>%
                hc_add_theme(hc_theme_economist()) %>%
                hc_xAxis(title = list(text = "Date")) %>%
                hc_yAxis(title = list(text = "Total Moisture"))
            }
          )
        }  
      } else if (input$hcspResponse == "Depth") {
        if(input$hcspSummaryLevel == "Continuous") {
          output$hcspPlot <- renderHighchart(
            expr = {
              data.react.hscpscatterplot() %>% 
                hchart("column", hcaes(x = "Timestamp", y = "Depth", group = "Team")) %>%
                hc_navigator(enabled = T) %>%
                hc_add_theme(hc_theme_economist()) %>%
                hc_xAxis(title = list(text = "Date")) %>%
                hc_yAxis(title = list(text = "Depth"))
            }
          )
        } else if(input$hcspSummaryLevel %in% c("Daily_Average", "Weekly_Average")) {
          output$hcspPlot <- renderHighchart(
            expr = {
              data.react.hscpscatterplot() %>% 
                hchart("column", hcaes(x = "Date", y = "Depth", group = "Team")) %>%
                hc_navigator(enabled = T) %>%
                hc_add_theme(hc_theme_economist()) %>%
                hc_xAxis(title = list(text = "Date")) %>%
                hc_yAxis(title = list(text = "Depth"))
            }
          )
        } 
      }
      
    }
  )
  
  
  
  
  
  # ----------------------- End TE: Scatterplot
  
  # ----------------------------- End Tab Exploration (TE)
  
  # ----------------------------- Tab Prediction Inputs 
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
  # ----------------------------- End Tab Prediction Inputs 
  
  
  
  
  # ---------------- Data Table in Control bar
  output$table <- renderDT(
    data.react.hscpscatterplot()
  )
  # ---------------- End Data Table in Control bar
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)



temp <- all_data_sensors_moisture %>% 
  filter(Team %in% c(3, 4, 16), Sensor == "CropX", Summarization_level == "Weekly_Average") 




temp %>% hchart("column", hcaes(x = "Date", y = "Moisture", group = Team)) %>%
  hc_navigator(enabled = T) %>%
  hc_add_theme(hc_theme_economist()) %>%
  hc_xAxis(title = list(text = "Date")) %>%
  hc_yAxis(title = list(text = "Total Moisture"))




class(all_data_sensors_moisture)



temp <- all_data_sensors_moisture %>% filter(
  Team %in% c(9, 10, 16),
  Sensor %in% "AquaSpy",
  Summarization_level == "Weekly_Average")

temp

temp %>% hchart("column", hcaes(x = "Date", y = "Moisture", group = "Team"))


merged_data 

# Area Chart
# Scatterplot 
# get rid of continuous 




