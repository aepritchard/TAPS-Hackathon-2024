




library(mgcv)
library(shiny)
library(bs4Dash)
library(leaflet)
library(mapdeck)
library(highcharter)
library(rsconnect)
library(leaflet.extras2)
library(DT)
library(tidyverse)
library(shinyWidgets)
library(fresh)  
library(waiter)
library(viridisLite)
library(shinycssloaders)
library(magrittr)
library(glue)
library(dygraphs)
library(sf)
library(leaflet)
library(terra)
library(ggthemes)
library(gt)
library(gtsummary)
library(shinyjs)
library(sodium)

# Create Theme
theme <- fresh::create_theme(
  bs4dash_status(
    primary = "#512888",
    secondary = "#FFFFFF"
  )
)




# ------------------------ Data Cleaning/Prep

# Prediction Data cleaning
url = "https://www.dropbox.com/scl/fi/4od5qok99fwqnrbywnmn9/all_sensor_weather_irrigation_data.csv?rlkey=io4vnd1lv46wzmpmwi932x0ng&st=g3vfgnbf&dl=1"
all_data = read.csv(url, header=T, sep=",")
all_data$Total_water = all_data$Irrigation_Amount + all_data$PRECIP # Total water = Irrigation + Precipitation
all_data$Moisture = all_data$Moisture/100 # Convert from percentage to proportion
model_data = all_data[all_data$Summarization_level == "Weekly_Average", ]
model_data = model_data[!is.na(model_data$Moisture),]
model_data = model_data[!is.na(model_data$Initial_Moisture),]
model_data$Total_water_ln = log(model_data$Total_water+1)
# End Prediction Data cleaning

# Prediction Prep
moisture_predictor = gam(Moisture ~ poly(Depth,4) + Sensor + poly(PRECIP,4) + poly(Irrigation_Amount,4) + poly(TEMP2MAVG,4) + Initial_Moisture, data = na.omit(model_data), family=betar())
temp_predictor = gam(Soil_Temp ~ poly(Depth,4) + Sensor + poly(PRECIP,4) + poly(Irrigation_Amount,4) + poly(TEMP2MAVG,4) + Initial_Soil_Temp, data=na.omit(model_data))



# End Prediction Prep

# Data Exploration Data Cleaning
all_data_sensors_moisture <- read.csv("https://www.dropbox.com/scl/fo/p344ba5vo02mmzb9bz0fc/AD3EVjUOJNQYysahsle6DnE/Data/all_data_sensors_moisture.csv?rlkey=dyaro79gn4bon9lqsfhmrowg3&dl=1")
merged_data <- read.csv("https://www.dropbox.com/scl/fo/p344ba5vo02mmzb9bz0fc/ANdTYq5hOU_ot0OlsYI6R60/Data/all_sensor_weather_irrigation_data.csv?rlkey=dyaro79gn4bon9lqsfhmrowg3&dl=1")
merged_data$Date = as.Date(merged_data$Timestamp)
all_data_sensors_moisture$Date = as.Date(all_data_sensors_moisture$Timestamp)
merged_data$Total_water = merged_data$Irrigation_Amount + merged_data$PRECIP # Total water = Irrigation + Precipitation
all_data_sensors_moisture$Moisture <- round(all_data_sensors_moisture$Moisture, 2)
all_data_sensors_moisture$Depth <- round(all_data_sensors_moisture$Depth, 2)
all_data_sensors_moisture$Soil_Temp <- round(all_data_sensors_moisture$Soil_Temp, 2)
merged_data$Moisture <- round(merged_data$Moisture, 2)
merged_data$Depth <- round(merged_data$Depth, 2)
merged_data$TEMP2MAVG <- round(merged_data$TEMP2MAVG, 2)
merged_data$Soil_Temp <- round(merged_data$Soil_Temp, 2)
merged_data <- merged_data %>% select(-X, -Timestamp)
all_data_sensors_moisture <- all_data_sensors_moisture %>% select(-X, -Timestamp)
merged_data$Initial_Moisture <- round(merged_data$Initial_Moisture, 2)
merged_data$Initial_Soil_Temp <- round(merged_data$Initial_Soil_Temp, 2)


# Normalize aquaspy data
s_min = min(all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="Sentek",]$Moisture, na.rm=T)
s_max = max(all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="Sentek",]$Moisture, na.rm=T)
s_range = s_max - s_min

a_min = min(all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="AquaSpy",]$Moisture, na.rm=T)
a_max = max(all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="AquaSpy",]$Moisture, na.rm=T)
a_range = a_max - a_min

model_data[model_data$Sensor=="AquaSpy",]$Moisture = (model_data[model_data$Sensor=="AquaSpy",]$Moisture-s_min)*s_max/a_range
model_data[model_data$Sensor=="AquaSpy",]$Initial_Moisture = (model_data[model_data$Sensor=="AquaSpy",]$Initial_Moisture-s_min)*s_max/a_range
all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="AquaSpy",]$Moisture = (all_data_sensors_moisture[all_data_sensors_moisture$Sensor=="AquaSpy",]$Moisture-s_min)*s_max/a_range
merged_data[merged_data$Sensor=="AquaSpy",]$Moisture = (merged_data[merged_data$Sensor=="AquaSpy",]$Moisture-s_min)*s_max/a_range
merged_data[merged_data$Sensor=="AquaSpy",]$Initial_Moisture = (merged_data[merged_data$Sensor=="AquaSpy",]$Initial_Moisture-s_min)*s_max/a_range
all_data[all_data$Sensor=="AquaSpy",]$Moisture = (all_data[all_data$Sensor=="AquaSpy",]$Moisture-s_min)*s_max/a_range

# Soil Prep
soil_texture = read.csv("https://www.dropbox.com/scl/fi/tnjzy0tfbn4051up8a1nw/24-KSU-TAPS-Soil-texture.csv?rlkey=sc10qo91wx8l712xyyusg5a0w&st=378m67i1&dl=1")
shallow_soil = read.csv("https://www.dropbox.com/scl/fi/1nd7iuvpqlh8zojkcu83s/24-KSU-TAPS-Shallow-soil-sampling.csv?rlkey=o5f6n8coqx9wtv6y4irebnyxv&st=wdsht9jt&dl=1")

soil_texture_cols_desired = soil_texture[,c("Plot.ID" , "Sample.Depth..in.", "OMC....", "Soil.Textural.Class", "Sand....", "Silt....", "Clay....")]
shallow_soil_cols_desired = shallow_soil[,c("Plot.ID", "Soil.pH", "Excess.Lime","Organic.matter....","Nitrate.Nitrogen..ppm.",   
                                            "Nitrate.Nitrogen..lbs.of.N.acre.","Phosphorus..ppm." , "Potassium..ppm." ,                
                                            "Sulfur..ppm.", "Sulfur..lbs.of.S.acre.","Calicium..ppm." , 
                                            "Magnesium..ppm.", "Sodium..ppm.","Zinc..ppm."         ,             
                                            "Iron..ppm.","Manganese..ppm.","Copper..ppm."   ,                 
                                            "Boron..ppm.","CEC..meq.per.100g.", "CEC.K....","CEC.Ca....","CEC.Mg....", "CEC.Na...." )]

colnames(soil_texture_cols_desired) = c("Plot_ID" , "Sample Depth", "OMC", "Soil Textural Class", "Sand", "Silt", "Clay")
colnames(shallow_soil_cols_desired) = c("Plot_ID", "Soil pH", "Excess Lime","Organic matter","Nitrate Nitrogen ppm",   
                                        "Nitrate Nitrogen lbs of N acre ","Phosphorus ppm" , "Potassium ppm" ,                
                                        "Sulfur ppm", "Sulfur lbs of S acre","Calicium ppm" , 
                                        "Magnesium ppm", "Sodium ppm","Zinc ppm"         ,             
                                        "Iron ppm","Manganese ppm","Copper ppm"   ,                 
                                        "Boron ppm","CEC meq per 100g", "CEC K","CEC Ca","CEC Mg", "CEC Na" )
# End Soil Data

# Imaging Data
url = "https://www.dropbox.com/scl/fi/3k6bsosyqq3jobhhr8zd7/all_imaging_combined.csv?rlkey=tvf89h02rx28930cuuqqtyznh&st=stlvwy0r&dl=1"
all_imaging_data = read.csv(url)
all_imaging_data$Image_Source = sapply(all_imaging_data$image_type, function(x) unlist(strsplit(x,"_"))[1])
all_imaging_data$Image_Type = sapply(all_imaging_data$image_type, function(x) unlist(strsplit(x,"_"))[2])
all_imaging_data[all_imaging_data$Image_Type=="Orthomosaic",]$Image_Type = "Orthomosiac"


# Map Prep
plot_shapes = st_read("F:/Data/Hackathon2024/Datasets/Plot boundaries/Map with all plots/2024_Colby_TAPS_Harvest_Area.shp")
plot_shapes = plot_shapes[,c("Block_ID", "TRT_ID",   "Plot_ID",  "geometry")]
colnames(plot_shapes)[2] = "Team"
plot_shapes <- st_transform(plot_shapes, crs = '+proj=longlat +datum=WGS84')
plot_shapes_shallow_soil = merge(plot_shapes, shallow_soil_cols_desired, by="Plot_ID", all=T)
plot_shapes_imaging = merge(plot_shapes, all_imaging_data, by="Plot_ID", all=T)


# ------------------------ End Data Cleaning

# Information Files
path_explorationInformation <- "F:/Data/Hackathon2024/explorationInformation.html"
path_mappingInformation <- "F:/Data/Hackathon2024/mappingInformation.html"
path_predictionInformation <- "F:/Data/Hackathon2024/predictionInformation.html"
path_dataInformation <- "F:/Data/Hackathon2024/dataInformation.html"
path_summaryStatistics <- "F:/DataHackathon2024/dataInformation.html"



# Login Page
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin:0 auto; padding: 0;height: 85vh; display: flex; justify-content: center; align-items: center;", class = "wrapper",
                 wellPanel(
                   tags$h2("Welcome to your TAPS dashboard!", class = "blink-purple", style = "padding-top: 0;color:#333; font-weight:900;text-align:center;border-radius: 5px;"),
                   tags$h4("Presented by Statflow", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   br(),
                   hr(style = "height:2px;border:none;color:LightGray;background-color:LightGray;"),
                   br(),
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   tags$script('
                                $(document).keyup(function(event) {
                                    if (event.key == "Enter") {
                                        $("#login").click();
                                    }
                                });
                                '),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#512888;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: TAPS  Password: password")
                   ))
)

credentials = data.frame(
  username_id = c("TAPS","a"),
  passod   = sapply(c("password","a"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

# End Login Page



ui <- bs4DashPage(
  # Set theme
  freshTheme = theme,
  help = NULL,
  
  # Header
  header = bs4DashNavbar(
    title = "StatFlow Dashboard",
    compact = T,
    fixed = T
  ),
  # End Header
  
  # Sidebar
  sidebar = bs4DashSidebar(
    id = "sidebar",
    minified = F,
    width = 350,
    uiOutput("sidebarpanel"),uiOutput("logoutbtn")
  ),
  # End Sidebar
  
  # Body
  body = bs4DashBody(
    tags$head(
      tags$style(
        "@keyframes blink {
          50% {
            background-color: #512888;
            border-radius: 5px;
            box-shadow: 0 0 10px 2px #512888;
            color: white;
          }
        }
        .blink-purple {
          animation: blink 1s 0.5s 2 linear;
        }
      "
      )
    ),
    
    shinyjs::useShinyjs(), uiOutput("body"),img(src='TAPS.png',style= 'position:absolute; right:10px;bottom:10px;' , width = 200),
    
    tags$style(HTML(".content-wrapper {  background-image: url('Folder1.png'); background-size: auto 75%; background-repeat: no-repeat;  background-position: center; }
                        .content-wrapper::before {content: '';position: absolute;top: 0px;right: 0px; bottom: 0px;left: 0px;background-color: rgba(255,255,255,0.95);}
                        "))
  ), 
  # End Body

  
  # Footer
  footer = bs4DashFooter(
    
  ),
  # End Footer
  
  
)



server <- function(input, output, session) {
  
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #FFFFFF !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px; position: absolute; right: 16px; bottom: 16px; list-style-type: none;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      bs4SidebarMenu( ### Sidebar Menu
        bs4SidebarMenuItem("Data Exploration", tabName="tabExploration"),
        bs4SidebarMenuItem("Mapping", tabName="tabMapping"),
        bs4SidebarMenuItem("Soil Temperature and Moisture Prediction", tabName="tabPrediction"),
        bs4SidebarMenuItem("Summary Tables", tabName="tabSummaryTables"),
        bs4SidebarMenuItem("Data", tabName="tabDataTables")
      )
    }
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      # Tabs (defining tabs from Sidebar Menu)
      bs4TabItems(
        # Data Exploration Tab
        bs4TabItem(
          tabName = "tabExploration", class = "active",
          fluidRow(
            
            # Explanation 
            bs4Dash::column(
              width = 12,
              bs4Card(
                title = "Information",
                width = NULL,
                height = "220px",
                collapsible = T,
                headerBorder = T,
                status = "primary",
                id = "cardexplorationInformation",
                div(style = 'height: 100%', htmlOutput("explorationInformation") %>% withSpinner(color = "#512888", proxy.height = "220px"))
              )
            ),
            # End Explanation 
            
            
            
            # Time Plot 
            bs4Dash::column(
              width = 6,
              bs4Card(
                title = "Average Soil Temperature/Moisture by Sensor and Team Over Time",
                width = NULL,
                height = "440px",
                collapsible = F,
                headerBorder = T,
                maximizable = F,
                status = "primary",
                id = "cardTimePlot",
                # Time Plot Sidebar
                sidebar = bs4CardSidebar(
                  id = "cardTimePlotSidebar",
                  easyClose = F,
                  startOpen = T,
                  width = 50,
                  background = "#512888",
                  radioGroupButtons(
                    inputId = "tpYAxis",
                    label = "Select Soil Temperature/Total Moisture",
                    status = "primary",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white")),
                    choiceNames = c("Soil Temperature", "Soil Moisture"),
                    choiceValues = c("Soil_Temp", "Moisture"),
                    selected = "Moisture"
                  ),
                  radioGroupButtons(
                    inputId = "tpSensor",
                    status = "primary",
                    label = "Select Sensor",
                    choices = c("AquaSpy", "CropX", "Sentek"),
                    individual = TRUE,
                    selected = "Sentek",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  radioGroupButtons(
                    inputId = "tpFrequency",
                    label = "Select Frequency",
                    status = "primary",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white")),
                    choiceNames = c("Daily Average", "Weekly Average"),
                    choiceValues = c("Daily_Average", "Weekly_Average"),
                    selected = "Weekly_Average"
                  ),
                  virtualSelectInput(
                    inputId = "tpTeam",
                    label = "Select Teams",
                    choices = sort(unique(all_data_sensors_moisture$Team)),
                    multiple = T,
                    selected = 16,
                    width = "200px"
                  )
                  
                ),
                # End Time Plot Sidebar
                highchartOutput("timePlot") %>% withSpinner(color = "#512888")
              )
            ),
            # End Time Plot 
            
            # 3D Area Plot
            bs4Dash::column(
              width = 6,
              bs4Card(
                title = "Average Soil Temperature/Moisture by Team and Depth over Time",
                width = NULL, 
                maximizable = F,
                height = "440px",
                collapsible = F,
                headerBorder = T,
                status = "primary",
                id = "card3DAreaPlot",
                # 3D Area Plot Sidebar
                sidebar = bs4CardSidebar(
                  id = "card3DAreaPlotSidebar",
                  width = 50,
                  easyClose = F,
                  startOpen = T,
                  background = "#512888",
                  radioGroupButtons(
                    inputId = "apYAxis",
                    label = "Select Soil Temperature/Moisture",
                    status = "primary",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white")),
                    choiceNames = c("Soil Temperature", "Soil Moisture"),
                    choiceValues = c("Soil_Temp", "Moisture"),
                    selected = "Moisture"
                  ),
                  radioGroupButtons(
                    inputId = "apSensor",
                    status = "primary",
                    label = "Select Sensor",
                    choices = c("AquaSpy", "CropX", "Sentek"),
                    individual = TRUE,
                    selected = "Sentek",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  virtualSelectInput(
                    inputId = "apDepth",
                    label = "Select Depth (in)",
                    choices = unique(merged_data$Depth),
                    multiple = T,
                    selected = c(1.97, 13.78),
                    width = "200px"
                  ),
                  virtualSelectInput(
                    inputId = "apTeam",
                    label = "Select Team",
                    choices = sort(unique(merged_data$Team)),
                    multiple = F,
                    selected = 3,
                    width = "200px"
                  )
                ),
                # End 3D Area Plot Sidebar
                highchartOutput("areaPlot") %>% withSpinner(color = "#512888")
              )
            ),
            # End 3D Area Plot
            
            # Box Plot
            bs4Dash::column(
              width = 6,
              bs4Card(
                title = "Soil Temperature/Moisture by Sensor",
                width = NULL,
                maximizable = F,
                height = "440px",
                collapsible = T,
                headerBorder = T,
                status = "primary",
                id = "cardBoxPlot",
                # Box Plot Sidebar
                sidebar = bs4CardSidebar(
                  id = "cardBoxPlotSidebar",
                  startOpen = T,
                  width = 50,
                  background = "#512888",
                  easyClose = F,
                  airDatepickerInput(
                    inputId = "bpDate",
                    label = "Date Range",
                    multiple = 2,
                    clearButton = T,
                    value = c(min(all_data_sensors_moisture$Date),max(all_data_sensors_moisture$Date)),
                    width = "200px"
                  ),
                  radioGroupButtons(
                    inputId = "bpYAxis",
                    label = "Select Soil Temperature/Total Moisture",
                    status = "primary",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white")),
                    choiceNames = c("Soil Temperature", "Moisture"),
                    choiceValues = c("Soil_Temp", "Moisture"),
                    selected = "Moisture"
                  )
                ),
                # End Box Plot Sidebar
                highchartOutput("boxPlot") %>% withSpinner(color = "#512888")
              )
            ),
            # End Box Plot
            
            # Scatter Plot
            
            
            bs4Dash::column(
              width = 6,
              bs4Card(
                title = "Weekly Average Soil Temperature and Moisture by Sensor/Depth",
                width = NULL,
                maximizable = F,
                height = "440px",
                collapsible = T,
                headerBorder = T,
                status = "primary", 
                id = "cardScatterPlot",
                # Scatter Plot Sidebar
                sidebar = bs4CardSidebar(
                  id = "cardBoxPlotSidebar",
                  startOpen = T,
                  width = 50,
                  background = "#512888",
                  easyClose = F,
                  airDatepickerInput(
                    inputId = "spDate",
                    label = "Date Range",
                    multiple = 2,
                    clearButton = T,
                    value = c(min(all_data_sensors_moisture$Date),max(all_data_sensors_moisture$Date)),
                    width = "200px"
                  ),
                  radioGroupButtons(
                    inputId = "spGroup",
                    label = "Select Grouping Variable",
                    status = "primary",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white")),
                    choiceNames = c("Sensor", "Depth"),
                    choiceValues = c("Sensor", "Depth"),
                    selected = "Sensor"
                  )
                ),
                # End Scatter Plot Sidebar
                highchartOutput("scatterPlot") %>% withSpinner(color = "#512888")
              )
            )
            
            # End Scatter Plot
          )
          
        ),
        # End Data Exploration Tab
        
        # Mapping Tab
        bs4TabItem(
          tabName = "tabMapping",
          fluidRow(
            # Explanation 
            bs4Dash::column(
              width = 12,
              bs4Card(
                title = "Information",
                width = NULL,
                height = "440px",
                collapsible = T,
                headerBorder = T,
                status = "primary",
                id = "cardmappingInformation",
                div(style = 'height: 100%', htmlOutput("mappingInformation") %>% withSpinner(color = "#512888", proxy.height = "220px"))
              )
            ),
            # End Explanation 
            
            bs4Dash::column(
              width = 6,
              bs4Card(
                leafletOutput("mapShallow"),
                title = "Shallow Soil Mapping",
                width=NULL,
                height="500px",
                collapsible = T,
                headerBorder = T,
                maximizable = T,
                status = "primary",
                id = "mapShallow",
                sidebar = bs4CardSidebar(
                  id = "mapSoilSidebar",
                  startOpen = T,
                  width = 50,
                  background = "#512888",
                  radioGroupButtons(
                    inputId = "tpShallowBase",
                    status = "primary",
                    label = "Select Base Map",
                    choices = c("Default", "Satellite"),
                    individual = TRUE,
                    selected = "Default",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  pickerInput(
                    inputId = "tpShallowVar",
                    label = "Select Variable",
                    choices = colnames(plot_shapes_shallow_soil)[4:25],
                    width = "200px",
                  )
                )
              ) ),
            bs4Dash::column(
              width = 6,
              bs4Card(
                leafletOutput("mapImaging"),
                title = "Ceres and Drone Mapping",
                width=NULL,
                height="500px",
                collapsible = T,
                headerBorder = T,
                maximizable = T,
                status = "primary",
                id = "mapImaging",
                sidebar = bs4CardSidebar(
                  id = "mapImageSidebar",
                  startOpen = T,
                  width = 50,
                  background = "#512888",
                  radioGroupButtons(
                    inputId = "tpImageBase",
                    status = "primary",
                    label = "Select Base Map",
                    choices = c("Default", "Satellite"),
                    individual = TRUE,
                    selected = "Default",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  radioGroupButtons(
                    inputId = "tpImageSource",
                    status = "primary",
                    label = "Select Imaging Source",
                    choiceNames = c("Ceres", "Drone Flights"),
                    choiceValues = c("CERES", "Drone"),
                    individual = TRUE,
                    selected = "CERES",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  radioGroupButtons(
                    inputId = "tpImageType",
                    status = "primary",
                    label = "Select Imaging Source",
                    choices = c("NDVI", "MCARI2", "Orthomosiac"),
                    individual = TRUE,
                    selected = "NDVI",
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  ),
                  pickerInput(
                    inputId = "tpImagingDates",
                    label = "Select Date",
                    choices = unique(plot_shapes_imaging$date),
                    width = "200px",
                  )
                )
              ) ),
          )),
        # End Mapping Tab
        
        
        # Data Prediction Tab
        bs4TabItem(
          tabName= "tabPrediction",
          fluidRow(
            
            # Explanation 
            bs4Dash::column(
              width = 12,
              bs4Card(
                title = "Information",
                width = NULL,
                height = "220px",
                collapsible = T,
                headerBorder = T,
                status = "primary",
                id = "cardmappingInformation",
                div(style = 'height: 100%', htmlOutput("predictionInformation") %>% withSpinner(color = "#512888", proxy.height = "220px"))
              )
            ),
            # End Explanation 
            bs4Dash::column(
              width = 5,
              status = "primary",
              bs4Card(
                title = "Model Specifications",
                width = NULL,
                collapsible = T,
                headerBorder = T,
                height = "900px",
                maximizable = T,
                status = "primary",
                id = "somechart",
                radioGroupButtons(
                  inputId = "inputResponse",
                  status = "primary",
                  label = "Choose Response",
                  choiceNames = c("Moisture", "Soil Temperature"),
                  choiceValues = c("Moisture", 
                                   "Soil_Temp"),
                  justified = TRUE,
                  selected = "Moisture",
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle", 
                                 style = "color: white"),
                    no = tags$i(class = "fa fa-circle-o", 
                                style = "color: white"))
                ),
                radioGroupButtons(
                  inputId = "inputPredictor",
                  status = "primary",
                  label = "Choose Predictor",
                  choiceNames = c("Temperature", "Irrigation Amount", "Precipitation", "Depth"),
                  choiceValues = c("TEMP2MAVG", "Irrigation_Amount", "PRECIP", "Depth"),
                  justified = TRUE,
                  selected = "TEMP2MAVG",
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle", 
                                 style = "color: white"),
                    no = tags$i(class = "fa fa-circle-o", 
                                style = "color: white"))
                ),
                sliderInput(
                  inputId = "inputInitialMoistureOrSoilTemp",
                  label = "Select Initial Moisture",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = .01
                ),
                sliderInput(
                  inputId = "inputTemp",
                  label = "Select Temperature",
                  min = 65,
                  max = 80,
                  value = 65,
                  step = .1
                ),
                sliderInput(
                  inputId = "inputPrecip",
                  label = "Select Precipitation",
                  min = 0,
                  max = 15,
                  value = 0,
                  step = 1
                ),
                sliderInput(
                  inputId = "inputIrrigation",
                  label = "Select Irrigation",
                  min = 0,
                  max = 1,
                  value = 0,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "inputDepth",
                  label = "Select Depth",
                  min = 0,
                  max = 48,
                  value = 0,
                  step = 1
                ),
                radioGroupButtons(
                  inputId = "inputSensor",
                  status = "primary",
                  label = "Select Sensor",
                  choices = c("AquaSpy", "CropX", "Sentek"),
                  individual = TRUE,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle", 
                                 style = "color: white"),
                    no = tags$i(class = "fa fa-circle-o", 
                                style = "color: white"))
                )
              )
            ),
            bs4Dash::column(
              width = 7,
              bs4Card(
                title = "Model Prediction with 95% Prediction Interval",
                width = NULL,
                collapsible = T,
                headerBorder = T,
                height = "900px",
                maximizable = T,
                status = "primary",
                id = "somechart",
                highchartOutput("modelChart") %>% withSpinner(color = "#512888")
              )
            )
          ),
          
        ),
        # End Prediction Tab
        
        # Summary Tables Tab
        bs4TabItem(
          tabName = "tabSummaryTables",
          fluidRow(
            bs4Dash::column(
              width = 3,
              bs4Card(
                width = NULL,
                status = "primary",
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                title = "Average Soil Temperature/Moisture by Sensor & Team Over Time",
                gt_output("summarytable1")
              )
            ),
            bs4Dash::column(
              width = 3,
              bs4Card(
                width = NULL,
                collapsible = T,
                collapsed = F,
                status = "primary",
                headerBorder = T,
                title = "Average Soil Temperature/Moisture by Team & Depth over Time",
                gt_output("summarytable2")
              )
            ),
            bs4Dash::column(
              width = 3,
              bs4Card(
                width = NULL,
                collapsible = T,
                collapsed = F,
                status = "primary",
                headerBorder = T,
                title = "Soil Temperature/Moisture by Sensor",
                gt_output("summarytable3")
              )
            ),
            bs4Dash::column(
              width = 3,
              bs4Card(
                width = NULL,
                collapsible = T,
                collapsed = F,
                status = "primary",
                headerBorder = T,
                title = "Weekly Average Soil Temperature & Moisture by Sensor/Depth",
                gt_output("summarytable4")
              )
            ),
          )
        ),
        # End Summary Tables Tab
        
        # Data Tables Tab
        bs4TabItem(
          tabName = "tabDataTables",
          fluidRow(
            
            bs4Dash::column(
              width = 12,
              bs4Card(
                title = "Data Tables",
                sidebar = bs4CardSidebar(
                  id = "dataTableSidebar",
                  background = "#512888",
                  startOpen = T,
                  radioGroupButtons(
                    inputId = "tableChoice",
                    status = "primary",
                    label = "Choose Table",
                    choices = c("Weekly/Daily Averages for Moisture", "3D Area Plot", "Box Plot of Moistures by Sensor", "Scatterplot"),
                    selected = "Weekly/Daily Averages for Moisture",
                    individual = TRUE,
                    checkIcon = list(
                      yes = tags$i(class = "fa fa-circle", 
                                   style = "color: white"),
                      no = tags$i(class = "fa fa-circle-o", 
                                  style = "color: white"))
                  )
                ),
                width = NULL,
                height = "1000px",
                headerBorder = T,
                status = "primary",
                id = "dataTable",
                DTOutput("dataTable") 
              )
            )
          )
        )
        # End Data Tables Tab
      )
      
    } else {
      loginpage
    }
  })
  
  
  
  # ------------------------------------------- Tab Mapping
  # Ceres map
  observeEvent(input$tpImageSource,
               {
                 if(input$tpImageSource == "CERES") {
                   updateRadioGroupButtons(
                     inputId = "tpImageType",
                     status = "primary",
                     label = "Select Imaging Source",
                     choices = c("NDVI", "MCARI2"),
                     selected = "NDVI",
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-circle", 
                                    style = "color: white"),
                       no = tags$i(class = "fa fa-circle-o", 
                                   style = "color: white"))
                   )
                 } else {
                   updateRadioGroupButtons(
                     inputId = "tpImageType",
                     status = "primary",
                     label = "Select Imaging Source",
                     choices = c("NDVI", "Orthomosiac"),
                     selected = "NDVI",
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-circle", 
                                    style = "color: white"),
                       no = tags$i(class = "fa fa-circle-o", 
                                   style = "color: white"))
                   )
                 }
               })
  
  observeEvent(input$tpImageType,
               observeEvent(input$tpImageSource, {
                 data_dates = plot_shapes_imaging %>% filter(Image_Source == input$tpImageSource,
                                                             Image_Type==input$tpImageType)
                 updatePickerInput(
                   inputId = "tpImagingDates",
                   label = "Select Date",
                   choices = unique(data_dates$date) 
                 ) })
  )
  imageDataReact = reactive({
    plot_shapes_imaging %>% filter(Image_Source == input$tpImageSource, Image_Type == input$tpImageType, date == input$tpImagingDates)
  })
  
  
  output$mapImaging = renderLeaflet({
    colorPal = colorNumeric(palette=tableau_gradient_pal(palette = "Red-Green-Gold Diverging", type = "ordered-diverging"), domain=imageDataReact()$raster_value)
    if(input$tpImageBase=="Satellite"){
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = -101.0654552, lat = 39.38612737, zoom = 17) %>%
        addPolygons(data=imageDataReact(),
                    color=colorPal(imageDataReact()$raster_value),
                    fillColor= colorPal(imageDataReact()$raster_value),
                    stroke = 1, opacity = 1, fillOpacity=0.8, 
                    popup=paste0("Plot: ", imageDataReact()$Plot_ID, "<br>", 
                                 "Block: ", imageDataReact()$Block_ID, "<br>",
                                 "Team: ", imageDataReact()$Team, "<hr>",
                                 "Image Date: ", input$tpImagingDates, "<br>",
                                 "Image Source: ", input$tpImageSource, "<br>",
                                 "Image Type: ", input$tpImageType, "<br>",
                                 "Value: ", imageDataReact()$raster_value)) %>%
        addLegend(data=imageDataReact(), 
                  values = imageDataReact()$raster_value,
                  pal=colorPal, title="Legend", opacity=1)
    } else {
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        setView(lng = -101.0654552, lat = 39.38612737, zoom = 17) %>%
        addPolygons(data=imageDataReact(),
                    color=colorPal(imageDataReact()$raster_value),
                    fillColor= colorPal(imageDataReact()$raster_value),
                    stroke = 1, opacity = 1, fillOpacity=0.8, 
                    popup=paste0("Plot: ", imageDataReact()$Plot_ID, "<br>", 
                                 "Block: ", imageDataReact()$Block_ID, "<br>",
                                 "Team: ", imageDataReact()$Team, "<hr>",
                                 "Image Date: ", input$tpImagingDates, "<br>",
                                 "Image Source: ", input$tpImageSource, "<br>",
                                 "Image Type: ", input$tpImageType, "<br>",
                                 "Value: ", imageDataReact()$raster_value)) %>%
        addLegend(data=imageDataReact(), 
                  values = imageDataReact()$raster_value,
                  pal=colorPal, title="Legend", opacity=1)
    }
  })
  
  # ------------- Shallow map
  
  data_plot = reactive({
    data_plot = plot_shapes_shallow_soil
    data_plot$Response = unlist(st_drop_geometry(plot_shapes_shallow_soil[,input$tpShallowVar]))
    data_plot
  })
  
  output$mapShallow = renderLeaflet({
    
    if(input$tpShallowBase=="Satellite"){
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = -101.0654552, lat = 39.38612737, zoom = 17) %>%
        addPolygons(data = data_plot(), 
                    color =  colorNumeric("plasma", data_plot()$Response)(data_plot()$Response),
                    fillColor= colorNumeric("plasma", data_plot()$Response)(data_plot()$Response),
                    stroke = 1, opacity = 1, fillOpacity=0.8, 
                    popup=paste0("Plot: ", data_plot()$Plot_ID, "<br>", 
                                 "Block: ", data_plot()$Block_ID, "<br>",
                                 "Team: ", data_plot()$Team, "<hr>",
                                 input$tpShallowVar, ": ", data_plot()$Response)) %>%
        addLegend(data=data_plot(), 
                  values = data_plot()$Response,
                  pal=colorNumeric("plasma", data_plot()$Response), title="Legend", opacity=1)
    } else {
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        setView(lng = -101.0654552, lat = 39.38612737, zoom = 17) %>%
        addPolygons(data = data_plot(), 
                    color =  colorNumeric("plasma", data_plot()$Response)(data_plot()$Response),
                    fillColor= colorNumeric("plasma", data_plot()$Response)(data_plot()$Response),
                    stroke = 1, opacity = 1, fillOpacity=0.8, 
                    popup=paste0("Plot: ", data_plot()$Plot_ID, "<br>", 
                                 "Block: ", data_plot()$Block_ID, "<br>",
                                 "Team: ", data_plot()$Team, "<hr>",
                                 input$tpShallowVar, ": ", data_plot()$Response)) %>%
        addLegend(data=data_plot(), 
                  values = data_plot()$Response,
                  pal=colorNumeric("plasma", data_plot()$Response), title="Legend", opacity=1)
    }
    
    
  })
  
  # ------------------------------------------- End Tab Mapping
  
  
  
  
  # ------------------------------------------- Tab Exploration (TE) 
  
  # ------------------------- Time Plot
  
  data_for_sensor = reactive({
    all_data_sensors_moisture %>% filter(
      Sensor == input$tpSensor,
      Summarization_level == input$tpFrequency
    ) %>% drop_na(Moisture)
  })
  
  observeEvent(input$tpSensor, {
    updateVirtualSelect(
      inputId = "tpTeam",
      label = "Select Teams",
      choices = sort(unique(data_for_sensor()$Team)),
      selected = 16,
    )
  })
  
  
  # tpTeam, tpSensor, tpFrequency
  data.react.timeplot <- reactive({
    if (input$tpYAxis == "Moisture") {
      all_data_sensors_moisture %>% filter(
        Team %in% input$tpTeam,
        Sensor == input$tpSensor,
        Summarization_level == input$tpFrequency
      ) %>% drop_na(Moisture)      
    } else if (input$tpYAxis == "Soil_Temp") {
      all_data_sensors_moisture %>% filter(
        Team %in% input$tpTeam,
        Sensor == input$tpSensor,
        Summarization_level == input$tpFrequency
      ) %>% drop_na(Soil_Temp)
    }
    
  })
  
  output$timePlot <- renderHighchart({
    if (input$tpYAxis == "Moisture") {
      data.react.timeplot() %>% 
        hchart("column", hcaes(x = "Date", y = "Moisture", group = "Team")) %>%
        hc_navigator(enabled = T) %>%
        hc_add_theme(hc_theme_gridlight()) %>%
        hc_xAxis(title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Soil Moisture"))        
    } else if (input$tpYAxis == "Soil_Temp") {
      data.react.timeplot() %>% 
        hchart("column", hcaes(x = "Date", y = "Soil_Temp", group = "Team")) %>%
        hc_navigator(enabled = T) %>%
        hc_add_theme(hc_theme_gridlight()) %>%
        hc_xAxis(title = list(text = "Date")) %>%
        hc_yAxis(title = list(text = "Soil Temperature"))   
    }
    
  }) 
  
  
  # ------------------------- End Time Plot 
  
  
  # ------------------------- 3D Area Plot 
  
  data_for_sensor_area = reactive({
    merged_data %>% filter(
      Sensor == input$apSensor,
    ) %>% drop_na(Moisture)
  })
  
  observeEvent(input$apSensor, {
    updateVirtualSelect(
      inputId = "apTeam",
      label = "Select Teams",
      choices = sort(unique(data_for_sensor_area()$Team)),
      selected = 16,
    )
    updateVirtualSelect(
      inputId = "apDepth",
      label = "Select Depth (in)",
      choices = sort(unique(data_for_sensor_area()$Depth)),
      selected = sort(unique(data_for_sensor_area()$Depth))[1:2],
    )
  })
  
  
  data.react.3DAreaPlot <- reactive({
    if (input$apYAxis == "Moisture") {
      merged_data %>% filter(
        Team %in% input$apTeam,
        Depth %in% input$apDepth,
        Sensor == input$apSensor,
        Summarization_level == "Weekly_Average"
      ) %>% drop_na(Moisture)  
    } else if (input$apYAxis == "Soil_Temp") {
      merged_data %>% filter(
        Team %in% input$apTeam,
        Depth %in% input$apDepth,
        Sensor == input$apSensor,
        Summarization_level == "Weekly_Average"
      ) %>% drop_na(Soil_Temp) 
    }
    
  })
  
  output$areaPlot <- renderHighchart({
    if (input$apYAxis == "Moisture") {
      data.react.3DAreaPlot() %>% hchart("area", hcaes(x = "Date", y = "Moisture", group = "Depth")) %>%
        hc_yAxis(
          floor = round(min(data.react.3DAreaPlot()$Moisture)),
          title = list(text = "Soil Moisture")
        ) %>%
        hc_navigator(
          enabled = T
        ) %>%
        hc_add_theme(hc_theme_gridlight())   
    } else if (input$apYAxis == "Soil_Temp") {
      data.react.3DAreaPlot() %>% hchart("area", hcaes(x = "Date", y = "Soil_Temp", group = "Depth")) %>%
        hc_yAxis(
          floor = round(min(data.react.3DAreaPlot()$Soil_Temp)),
          title = list(text = "Soil Temperature")
        ) %>%
        hc_navigator(
          enabled = T
        ) %>%
        hc_add_theme(hc_theme_gridlight()) 
    }
    
  })
  
  
  
  # ------------------------- End 3D Area Plot 
  
  # ------------------------- Box Plot
  data.react.boxplot <- reactive({
    if (input$bpYAxis == "Moisture") {
      all_data_sensors_moisture %>% filter(
        Date >= input$bpDate[1] & Date <= input$bpDate[2], 
        Summarization_level == "Continuous"
      ) %>% drop_na(Moisture)  
    } else if (input$bpYAxis == "Soil_Temp") {
      all_data_sensors_moisture %>% filter(
        Date >= input$bpDate[1] & Date <= input$bpDate[2], 
        Summarization_level == "Continuous"
      ) %>% drop_na(Soil_Temp)
    }
  })
  
  
  output$boxPlot <- renderHighchart({
    if (input$bpYAxis == "Moisture") {
      hcboxplot(x = data.react.boxplot()$Moisture, var = data.react.boxplot()$Sensor) %>%
        hc_plotOptions(
          boxplot = list(
            fillColor = '#B290DF',
            lineColor = 'black',
            boxDashColor = '#000000',
            boxDashStyle = 'Solid', 
            lineWidth = 2.2,
            medianColor = '#000000',
            medianDashStyle = 'solid',
            medianWidth = 3,
            stemColor = '#000000',
            stemDashStyle = 'solid',
            stemWidth = 1,
            whiskerColor = '#000000',
            whiskerLength = '20%',
            whiskerWidth = 3
          )
        ) %>%
        hc_yAxis(
          min = 0,
          max = 80,
          title = list(text = "Soil Moisture")
        ) %>% 
        hc_xAxis(
          title = list(text = "Sensor")
        ) %>%
        hc_chart(
          type = "column"
        ) %>%
        hc_add_theme(hc_theme_gridlight()) 
    } else if (input$bpYAxis == "Soil_Temp") {
      hcboxplot(x = data.react.boxplot()$Soil_Temp, var = data.react.boxplot()$Sensor) %>%
        hc_plotOptions(
          boxplot = list(
            fillColor = '#B290DF',
            lineColor = 'black',
            boxDashColor = '#000000',
            boxDashStyle = 'Solid', 
            lineWidth = 2.2,
            medianColor = '#000000',
            medianDashStyle = 'solid',
            medianWidth = 3,
            stemColor = '#000000',
            stemDashStyle = 'solid',
            stemWidth = 1,
            whiskerColor = '#000000',
            whiskerLength = '20%',
            whiskerWidth = 3
          )
        ) %>%
        hc_yAxis(
          min = 60,
          max = 85,
          title = list(text = "Soil Temperature")
        ) %>% 
        hc_xAxis(
          title = list(text = "Sensor")
        ) %>%
        hc_chart(
          type = "column"
        ) %>%
        hc_add_theme(hc_theme_gridlight()) 
    }
    
  })
  
  
  # ------------------------- End Box Plot
  
  
  # ------------------------- Scatter Plot
  data.react.scatterplot <- reactive({
    all_data_sensors_moisture %>% filter(
      Date >= input$spDate[1] & Date <= input$spDate[2], 
      Summarization_level == "Weekly_Average"
    ) %>% drop_na(Moisture, Soil_Temp)  
  })
  
  output$scatterPlot <- renderHighchart({
    if (input$spGroup == "Sensor") {
      data.react.scatterplot() %>% hchart('scatter', hcaes(x = Soil_Temp, y = Moisture, group = Sensor)) %>% 
        hc_xAxis(title = list(text = "Weekly Average Soil Temperature")) %>%
        hc_yAxis(title = list(text = "Weekly Average Soil Moisture")) %>% 
        hc_plotOptions(series = list(sensor = list(inactive = list(opacity = 0.5)))) %>%
        hc_add_theme(hc_theme_gridlight())
    } else if (input$spGroup == "Depth") {
      data.react.scatterplot() %>% hchart('scatter', hcaes(x = Soil_Temp, y = Moisture, color = Depth)) %>% 
        hc_xAxis(title = list(text = "Weekly Average Soil Temperature")) %>%
        hc_yAxis(title = list(text = "Weekly Average Soil Moisture")) %>% 
        hc_plotOptions(series = list(sensor = list(inactive = list(opacity = 0.5)))) %>%
        hc_colorAxis(stops = color_stops(10, viridis(10))) %>%
        hc_legend(layout = "horizontal", align = "center", verticalAlign = "bottom", title = list(text = "Depth (in.)")) %>%
        hc_add_theme(hc_theme_gridlight())
    }
    
  }) 
  
  
  # ------------------------- End Scatter Plot
  
  output$dataTable <- renderDT(
    data.react.timeplot()
  )
  
  # Data Tables
  observeEvent(
    eventExpr = {
      input$tableChoice
    },
    handlerExpr = {
      
      if(input$tableChoice == "Weekly/Daily Averages for Moisture") {
        output$dataTable <- renderDT(
          data.react.timeplot() %>% select(-Summarization_level)
        )
      } else if (input$tableChoice == "3D Area Plot") {
        output$dataTable <- renderDT(
          data.react.3DAreaPlot()
        )
      } else if (input$tableChoice == "Box Plot of Moistures by Sensor") {
        output$dataTable <- renderDT(
          data.react.boxplot()
        )
      } else if (input$tableChoice == "Scatterplot") {
        output$dataTable <- renderDT(
          data.react.scatterplot()
        )
      }
      
    }
  )
  # End Data Tables
  
  # Summary Data for the summary statistics and stuff
  output$summarytable1 <-
    render_gt(
      data.react.timeplot() %>%
        tbl_summary(
          missing = "ifany"
        ) %>%
        add_stat_label() %>%
        add_n() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Summary Statistics Table**"))
    )
  
  output$summarytable2 <-
    render_gt(
      data.react.3DAreaPlot() %>%
        tbl_summary(
          missing = "ifany"
        ) %>%
        add_stat_label() %>%
        add_n() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Summary Statistics Table**"))
    )
  
  output$summarytable3 <-
    render_gt(
      data.react.boxplot() %>%
        tbl_summary(
          missing = "ifany"
        ) %>%
        add_stat_label() %>%
        add_n() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Summary Statistics Table**"))
    )
  
  output$summarytable4 <-
    render_gt(
      data.react.scatterplot() %>%
        tbl_summary(
          missing = "ifany"
        ) %>%
        add_stat_label() %>%
        add_n() %>%
        # CONVERT TO A {gt} TABLE! VERY IMPORTANT STEP!
        as_gt() %>%
        tab_header(md("**Summary Statistics Table**"))
    )
  
  
  
  
  
  # End Summary Data for the summary statistics and stuff
  
  
  
  # ------------------------------------------- End Tab Exploration (TE)
  
  
  
  # ------------------------------------------- Tab Prediction (TP) 
  
  # Change Input based on Response 
  observeEvent(input$inputResponse,
               {
                 if(input$inputResponse == "Moisture") {
                   updateSliderInput(
                     inputId = "inputInitialMoistureOrSoilTemp", 
                     label = "Select Initial Moisture",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.01
                   )
                 } else if (input$inputResponse == "Soil_Temp") {
                   updateSliderInput(
                     inputId = "inputInitialMoistureOrSoilTemp",
                     label = "Select Initial Soil Temperature",
                     min = 60,
                     max = 90,
                     step = 0.01,
                     value = 60
                   )
                 }
               })
  
  # Calculate Model Predictions
  
  # Model Input
  # Depth, Sensor, TEMP2MAVG, Initial_Moisture, Irrigation_Amount, PRECIP
  
  # testText <- reactive({
  #   
  #   response <- input$inputResponse
  #   predictor <- input$inputPredictor
  #   depth <- input$inputDepth
  #   sensor <- input$inputSensor
  #   temp <- input$inputTemp
  #   initMoist <- input$inputInitialMoistureOrSoilTemp
  #   initSoilTemp <- input$inputInitialMoistureOrSoilTemp
  #   initIrrigation <- input$inputIrrigation
  #   initPrecip <- input$inputPrecip
  #   
  #   testText <- message(
  #     glue(
  #       "input$inputResponse = {response},
  #        input$inputDepth = {depth},
  #        input$inputSensor = {sensor},
  #        input$inputTemp = {temp},
  #        input$inputInitialMoisture = {initMoist},
  #        input$inputInitialSoilTemp = {initSoilTemp},
  #        input$inputIrrigation = {initIrrigation},
  #        input$inputPrecip = {initPrecip}
  #       "
  #     )
  #   )
  #   
  #   print(paste("Response:",response))
  #   print(paste("Predictor:", predictor))
  #   print(paste("Depth:",depth))
  #   print(paste("Sensor:",sensor))
  #   print(paste("Temp:",temp))
  #   print(paste("InitMoist:",initMoist))
  #   print(paste("InitSoilTemp:",initSoilTemp))
  #   print(paste("InitIrrigation:",initIrrigation))
  #   print(paste("InitPrecip:",initPrecip))
  #   
  #   testText
  # })
  # 
  # output$testText <- renderText(
  #   testText()
  # )
  
  
  
  sequences <- reactive({
    if(input$inputResponse == "Moisture") {
      sequences <- list(
        depth_range = seq(0,48, length.out = 150),
        precip_range = seq(0,15, length.out = 150),
        irrigationamount_range = seq(0, 1, length.out = 150),
        temp_range = seq(66,80, length.out = 150)
      )
    } else if (input$inputResponse == "Soil_Temp") {
      sequences <- list(
        depth_range = seq(0,48, length.out = 150),
        precip_range = seq(0,15, length.out = 150),
        irrigationamount_range = seq(0, 1, length.out = 150),
        temp_range = seq(66,80, length.out = 150)
      )
    }
    
    return(sequences)
  })
  

  
  
  
  inputList <- reactive({
    validate(
      need(sequences(), "For stuff")
    )
    if (input$inputResponse == "Moisture") {
      if (input$inputPredictor == "TEMP2MAVG") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = sequences()[["temp_range"]],
          Initial_Moisture = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = rep(input$inputPrecip, 1)
        )
      } else if (input$inputPredictor == "Irrigation_Amount") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Moisture = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = sequences()[["irrigationamount_range"]],
          PRECIP = rep(input$inputPrecip, 1)
        )
      } else if (input$inputPredictor == "PRECIP") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Moisture = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = sequences()[["precip_range"]]
        )
      } else if (input$inputPredictor == "Depth") {
        inputList <- list(
          Depth = sequences()[["depth_range"]],
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Moisture = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = rep(input$inputPrecip, 1)
        )
      }
    } else if (input$inputResponse == "Soil_Temp") {
      if (input$inputPredictor == "TEMP2MAVG") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = sequences()[["temp_range"]],
          Initial_Soil_Temp = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = rep(input$inputPrecip, 1)
        )
      } else if (input$inputPredictor == "Irrigation_Amount") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Soil_Temp = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = sequences()[["irrigationamount_range"]],
          PRECIP = rep(input$inputPrecip, 1)
        )
      } else if (input$inputPredictor == "PRECIP") {
        inputList <- list(
          Depth = rep(input$inputDepth, 1),
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Soil_Temp = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = sequences()[["precip_range"]]
        )
      } else if (input$inputPredictor == "Depth") {
        inputList <- list(
          Depth = sequences()[["depth_range"]],
          Sensor = rep(input$inputSensor, 1),
          TEMP2MAVG = rep(input$inputTemp, 1),
          Initial_Soil_Temp = rep(input$inputInitialMoistureOrSoilTemp, 1),
          Irrigation_Amount = rep(input$inputIrrigation, 1),
          PRECIP = rep(input$inputPrecip, 1)
        )
      }
    }
    return(inputList)
  })
  
  

  
  
  predictors.react <- reactive({
    validate(
      need(inputList(), "Need inputList")
    )
    if (input$inputResponse == "Moisture") {
      data.frame(
        Depth = inputList()[["Depth"]],
        Sensor = inputList()[["Sensor"]],
        TEMP2MAVG = inputList()[["TEMP2MAVG"]],
        Initial_Moisture = inputList()[["Initial_Moisture"]],
        Irrigation_Amount = inputList()[["Irrigation_Amount"]],
        PRECIP = inputList()[["PRECIP"]]
      )
    } else if (input$inputResponse == "Soil_Temp") {
      data.frame(
        Depth = inputList()[["Depth"]],
        Sensor = inputList()[["Sensor"]],
        TEMP2MAVG = inputList()[["TEMP2MAVG"]],
        Initial_Soil_Temp = inputList()[["Initial_Soil_Temp"]],
        Irrigation_Amount = inputList()[["Irrigation_Amount"]],
        PRECIP = inputList()[["PRECIP"]]
      )
    }
  })
  
  
  

  
  predictions.react <- reactive({
    validate(
      need(predictors.react(), "Need predictions.react")
    )
    if (input$inputResponse == "Moisture") {
      data.frame(predict(moisture_predictor, newdata = predictors.react(), type="response", se.fit=TRUE))
    } else if (input$inputResponse == "Soil_Temp") {
      data.frame(predict(temp_predictor, newdata = predictors.react(), type="response", se.fit=TRUE))
    }
  })
  

  
  x_axis_label.react <- reactive({
    if (input$inputPredictor == "TEMP2MAVG") {
      "Temperature"
    } else if (input$inputPredictor == "Irrigation_Amount") {
      "Irrigation Amount"
    } else if (input$inputPredictor == "PRECIP") {
      "Precipitation Amount"
    } else if (input$inputPredictor == "Depth") {
      "Depth"
    }
  })
  y_axis_label.react <- reactive({
    if (input$inputResponse == "Moisture") {
      "Expected Soil Moisture"
    } else if (input$inputResponse == "Soil_Temp") {
      "Expected Soil Temperature"
    }
  })
  
  
  combinedData.react <- reactive({
    
    validate(
      need(predictors.react(), "Need predictors.react"),
      need(predictions.react(), "Need predictions.react")
    )
    
    # Merge with data
    combinedData = cbind(predictors.react(), predictions.react())
    
    # Find upper and lower confidence limits
    combinedData$Lower = combinedData$fit - 1.96*combinedData$se.fit
    combinedData$Upper = combinedData$fit + 1.96*combinedData$se.fit
    
    combinedData$fit = round(combinedData$fit, 4)
    combinedData$Lower = round(combinedData$Lower, 4)
    combinedData$Upper = round(combinedData$Upper, 4)
    # Define predictor variable
    combinedData$Predictor = round(combinedData[,input$inputPredictor], 4)
    
    return(combinedData)
  })
  
  output$modelChart <- renderHighchart({
    hchart(combinedData.react(), type="line", name = "Prediction", hcaes(x=Predictor, y=fit), color="black") %>%
      hc_add_series(combinedData.react(), type="arearange", name="95% Prediction Interval",
                    hcaes(x=Predictor, low=Lower, high=Upper), color="#D3C0ED", opacity=0.65) %>%
      hc_xAxis(title = list(text = x_axis_label.react())) %>%
      hc_yAxis(title = list(text = y_axis_label.react()))
  })
  
  
  
  
  # ------------------------------------------- End Tab Prediction (TP)
  
  
  
  
  
  # ------------------------------------------- Information Stuff
  
  
  output$explorationInformation <- renderUI(
    includeHTML(path_explorationInformation)
  )
  
  output$mappingInformation <- renderUI(
    includeHTML(path_mappingInformation)
  )
  
  output$predictionInformation <- renderUI(
    includeHTML(path_predictionInformation)
  )
  
  output$dataInformation <- renderUI(
    includeHTML(path_dataInformation)
  )
  
  # -------------------------------------------- End Information Stuff
  
  
  
}


shinyApp(ui, server)




