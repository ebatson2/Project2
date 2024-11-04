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
          "Data Exploration", "tab3 content"
        ),
        
        tabPanel(
          "Data Download", "tab2 content"
        ),
        
        tabPanel(
          "About",
          h2("Purpose"),
          p("This app allows users to explore the mobile phone data from kaggle. 
            Users can choose which numeric variables to include, as well as choose which levels of each categorical variable to include in plot generation. 
            The tabs can then be used to explore the output given by subsetting."),
          h2("Background"),
          HTML("
            <p>The dataset used in this app is synthetic and generated based on simulated user behavior. 
            It does not represent actual user data.
            There are 700 samples of user data, and the original dataset includes a column containing the classification of the level of usage, 
            where 1 is a \"light usage\" user and 5 is an \"extreme usage\" user.
              More information about this dataset, as well as a link to download the original dataset, can be found at 
              <a href='https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset'>this kaggle link</a>.
            </p>"),
          h2("Tour of the App"),
          HTML("
            <p>The 
              <b>sidebar</b>
              (found on the far left or top of the app) can be used to subset the data in ways of interest to the user. 
            Then the 
              <b>Data Exploration</b> tab above can be used to view tabular and graphical summaries of the dataset as a whole, or the subset chosen in the sidebar. 
            The 
              <b>Data Download</b> 
            tab can be used to download the dataset, subsetted or whole. 
            Lastly, the 
              <b>About</b>
            tab, which you are currently reading, contains information about the app and background on the dataset.
               </p>"),
          HTML('<center>
                  <img src="dataset-cover.jpeg", align="center", width="600px"/>
               </center>')
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

# Run the application 
shinyApp(ui = ui, server = server)
