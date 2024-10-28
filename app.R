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
library(fresh)
library(shinyjs)
library(sodium)

# Create Theme
theme <- fresh::create_theme(
    bs4dash_status(
        primary = "#512888",
        secondary = "#FFFFFF"
    )
)

# # Load in datasets
# all_data_sensors_moisture <- read.csv("https://www.dropbox.com/scl/fi/1gpmfwnwj6cesqjozvj1i/all_data_sensors_moisture.csv?rlkey=0qqmtdodrtbojgdiqzigho9ud&st=zxqdrjqe&dl=1")
# merged_data <- read.csv("https://www.dropbox.com/scl/fi/1gpmfwnwj6cesqjozvj1i/all_data_sensors_moisture.csv?rlkey=0qqmtdodrtbojgdiqzigho9ud&st=wbqmjoi6&dl=1")
# dim(all_data_sensors_moisture)
# 
# # Fix dates
# merged_data$Date = as.Date(merged_data$Timestamp)
# all_data_sensors_moisture$Date = as.Date(all_data_sensors_moisture$Timestamp)

loginpage <- div(id = "loginpage", style = "width: 700px; max-width: 100%; margin: 0 auto; padding: 20px;", class = "wrapper",
                 wellPanel(
                     tags$h2("Welcome to your TAPS dashboard!", class = "text-center", style = "padding-top: 0;color:#333; font-weight:800;"),
                     tags$h4("Presented by Statflow", class = "text-center", style = "padding-top: 0;color:#333; font-weight:500;"),
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
        
        shinyjs::useShinyjs(), uiOutput("body"),img(src='TAPS.png',style= 'position:absolute; right:10px;bottom:10px;' , width = 200),
        
        tags$style(HTML(".content-wrapper {  background-image: url('Folder1.png'); background-size: auto 75%; background-repeat: no-repeat;  background-position: center; }
                        .content-wrapper::before {content: '';position: absolute;top: 0px;right: 0px; bottom: 0px;left: 0px;background-color: rgba(255,255,255,0.90);}"))
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
                bs4SidebarMenuItem("Prediction", tabName="tabPrediction")
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            
            # Tabs (defining tabs from Sidebar Menu)
            bs4TabItems(
                # Data Exploration Tab
                bs4TabItem(
                    tabName = "tabExploration", class = 'active',
                    fluidRow(
                        # Time Plot 
                        bs4Dash::column(
                            width = 6,
                            bs4Card(
                                title = "Weekly/Daily Averages for Moisture",
                                width = NULL,
                                height = "440px",
                                collapsible = T,
                                headerBorder = T,
                                status = "primary",
                                id = "cardTimePlot",
                                # Time Plot Sidebar
                                sidebar = bs4CardSidebar(
                                    id = "cardTimePlotSidebar",
                                    startOpen = T,
                                    width = 50,
                                    background = "#512888",
                                    # virtualSelectInput(
                                    #     inputId = "tpTeam",
                                    #     label = "Select Team",
                                    #     choices = unique(all_data_sensors_moisture$Team),
                                    #     multiple = T,
                                    #     selected = 3,
                                    #     width = "200px"
                                    # ),
                                    radioGroupButtons(
                                        inputId = "tpSensor",
                                        status = "primary",
                                        label = "Select Sensor",
                                        choices = c("AquaSpy", "CropX", "Sentek"),
                                        individual = TRUE,
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
                                    actionButton(
                                        inputId = "tpReload",
                                        label = "Reload",
                                        status = "secondary",
                                        outline = T,
                                        gradient = F
                                    )
                                )
                                # End Time Plot Sidebar
                            )
                        ),
                        # End Time Plot 
                        # 3D Area Plot
                        bs4Dash::column(
                            width = 6,
                            bs4Card(
                                title = "3D Area Plot",
                                width = NULL, 
                                height = "440px",
                                collapsible = T,
                                headerBorder = T,
                                status = "primary",
                                id = "card3DAreaPlot"
                                # 3D Area Plot Sidebar
                                # End 3D Area Plot Sidebar
                            )
                        )
                        # End 3D Area Plot
                    ),
                    fluidRow(
                        bs4Dash::column(
                            width = 6,
                            bs4Card(
                                title = "Data Tables 1",
                                width = NULL,
                                height = "1000px",
                                collapsible = T,
                                collapsed = T,
                                headerBorder = T,
                                status = "primary",
                                id = "dataTable1"
                            )
                        ),
                        bs4Dash::column(
                            width = 6,
                            bs4Card(
                                title = "Data Tables 2",
                                width = NULL,
                                height = "1000px",
                                collapsible = T,
                                collapsed = T,
                                headerBorder = T,
                                status = "primary",
                                id = "dataTable2"
                                
                            )
                        )
                    )
                ),
                # End Data Exploration Tab
                # Data Prediction Tab
                bs4TabItem(
                    tabName= "tabPrediction", 
                    fluidRow(
                        bs4Dash::column(
                            width = 5,
                            status = "primary",
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
                                    status = "primary",
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
                                    inputId = "modelSensor",
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
                        )
                    )
                )
                # End Prediction Tab
            )
            # End Tabs (from Sidebar Menu)
        }
        else {
            loginpage
        }
    })
    # So the structure of this... is 
    # Tab Exploration --- Stuff in Tab Exploration
    
    # Time Plot --- Stuff for Timeplot
    # End Time Plot --- End Stuff for Timeplot
    
    # 3D Area Plot --- Stuff for 3D Area Plot
    # 3D Area Plot --- End Stuff for 3D Area Plot
    # End Tab Exploration --- End Stuff in Tab Exploration
    # Tab Prediction --- Stuff in Tab Prediction
    # Change Input based on Response 
    # End Tab Prediction --- End Stuff in Tab Prediction
    # ------------------------------------------- Tab Exploration (TE) 
    
    # ------------------------- Time Plot 
    
    # Data Table for Time Plot
    output$tableTimePlot <- renderDT(
        data.react.timeplot()
    )
    # End Data Table for Time Plot
    
    # ------------------------- End Time Plot 
    
    
    # ------------------------- 3D Area Plot 
    # Data Table for 3D Area Plot
    output$table3DAreaPlot <- renderDT(
        data.react.3DAreaPlot()
    )
    # End Data Table for 3D Area Plot
    # ------------------------- End 3D Area Plot 
    # ------------------------------------------- End Tab Exploration (TE)
    
    # ------------------------------------------- Tab Prediction (TP) 
    # Change Input based on Response 
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
    # ------------------------------------------- End Tab Prediction (TP) 
    
    
}


shinyApp(ui, server)
