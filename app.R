library(shiny)

ui <- navbarPage(
    "App Title",
    tabPanel(
        "Plot",
        tags$style(
        "body {  background-image: url('Folder2.png'); background-size: contain; background-repeat: no-repeat;  background-position: center;width: 100vw;
    height: 100vh;} 
        .navbar-nav li a {
        font-size: 20px;
        font-weight: bold;
      }
    "
        ),
        tabsetPanel(
            tabPanel("A"),
            tabPanel("B"),
            tabPanel("C")
        )
    ),
    tabPanel("Summary"),
    tabPanel("Table")
)

server <- function(input, output) {}

shinyApp(ui, server)