library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      "Options",
      selectInput("OS_selection", "Operating System", choices=c("Android", "iOS"), multiple=TRUE),
      selectInput("model_selection", "Device Model", choices=c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "	iPhone 12", "Samsung Galaxy S21"), multiple=TRUE),
      checkboxGroupInput("num_vars", "Numeric Variables",
                         choiceValues=c("App Usage Time (min/day)", "Screen On Time (hours/day)", "Battery Drain (mAh/day)", "Number of Apps Installed", "Data Usage (MB/day)", "Age"), 
                         choiceNames=c("App Usage Time", "Screen On Time", "Battery Drain", "Number of Apps Installed", "Data Usage", "Age")),
      sliderInput("var1_range", "Range for first variable", min=0, max=1000, value=c(0, 1000)),
      sliderInput("var2_range", "Range for second variable", min=0, max=1000, value=c(0, 1000)),
      actionButton("go", "Filter Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About", "tab1 content"
        ),
        tabPanel(
          "Data Download", "tab2 content"
        ),
        tabPanel(
          "Data Exploration", "tab3 content"
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
