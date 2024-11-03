library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      "sidebar text"
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "tab1", "tab1 content"
        ),
        tabPanel(
          "tab2", "tab2 content"
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
