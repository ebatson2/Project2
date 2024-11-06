library(shiny)
library(bslib)
library(readr)
library(tidyverse)
library(ggstatsplot)

df <- read_csv("user_behavior_dataset.csv")

all_num_vars <- c("App Usage Time (min/day)", "Screen On Time (hours/day)", "Battery Drain (mAh/day)", "Number of Apps Installed", "Data Usage (MB/day)", "Age")

col_extrema <- function(df) {
  return(df|>
           summarize(across(where(is.numeric), 
                            list("max" = max, "min" = min), 
                            .names = "{.fn}_{.col}")))
}

extreme_vals = unlist(col_extrema(df))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Options"),
      
      h3("Filter Data Based on Categorical Varaibles"),
      selectInput("OS_selection", "Operating System", choices=c("Android", "iOS"), multiple=TRUE),
      selectInput("model_selection", "Device Model", choices=c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "	iPhone 12", "Samsung Galaxy S21"), multiple=TRUE),
      
      h3("Filter Data Based on Numerical Variables"),
      
      selectInput("num_var1", "Numeric Variable 1", choices=c("", all_num_vars)),
      conditionalPanel(
        condition = "input.num_var1.length !== 0" ,
        sliderInput("var1_range", "Choose your range of values:", min=0, max=1000, value=c(0, 1000)),
      ),
      
      selectInput("num_var2", "Numeric Variable 2", choices=c("", all_num_vars)),
      conditionalPanel(
        condition = "input.num_var2.length !== 0" ,
        sliderInput("var2_range", "Choose your range of values:", min=0, max=1000, value=c(0, 1000)),
      ),
      
      actionButton("go_subset", "Filter Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data Exploration",
          navset_card_underline(
            
            nav_panel("Plots", "content1"),
            
            nav_panel("Numerical Summary", "content2"),
            
            nav_panel("Data", "content3"),
            
          )
        ),
        
        tabPanel(
          "Data Download", 
          DT::DTOutput('tbl'),
          downloadButton("go_download", "Download Data")
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
  cur_data <- df
  
  # update based on numeric variable 1 being selected
  observeEvent(input$num_var1, {
    
    min <- extreme_vals[paste0("min_", input$num_var1)][[1]]
    max <- extreme_vals[paste0("max_", input$num_var1)][[1]]
    
    updateSliderInput(session, "var1_range", max=max, min=min, value=c(min, max))
  })
  
  # update based on numeric variable 2 being selected
  observeEvent(input$num_var2, {
    
    min <- extreme_vals[paste0("min_", input$num_var2)][[1]]
    max <- extreme_vals[paste0("max_", input$num_var2)][[1]]
    
    updateSliderInput(session, "var2_range", max=max, min=min, value=c(min, max))
  })
  
  # subset data on download page when user clicks "filter data" action button
  observeEvent(input$go_subset, {
    print("cur_data1")
    print(cur_data)
    
    cur_data <- cur_data |>
      filter(`Operating System` %in% input$OS_selection) |>
      filter(`Device Model` %in% input$model_selection) |>
      filter(!!sym(input$num_var1) >= input$var1_range[1]) |>
      filter(!!sym(input$num_var1) <= input$var1_range[2]) |>
      filter(!!sym(input$num_var2) >= input$var2_range[1]) |>
      filter(!!sym(input$num_var2) <= input$var2_range[2])
    
    print("cur_data2")
    print(cur_data)
    
    proxy <- DT::dataTableProxy('tbl')
    DT::replaceData(proxy, cur_data)
  })
  
  # table to display in Data Download tab
  output$tbl = DT::renderDT(cur_data)
  
  # handler for download button
  output$go_download <- downloadHandler(
    filename = function() {
      paste0('mobile_phone_data_', Sys.Date(), '.csv')
    },
    content = function(file) {
      write.csv(cur_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
