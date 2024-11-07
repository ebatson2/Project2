library(shiny)
library(bslib)
library(readr)
library(tidyverse)
library(ggstatsplot)
library(plotly)

df <- read_csv("user_behavior_dataset.csv")

# update column types so IDs and behavior class are not numeric and get rid of spaces
df <- df |>
  mutate(User_ID=as.character(`User ID`)) |>
  mutate(User_Behavior_Class=as.factor(`User Behavior Class`)) |>
  rename(Operating_System=`Operating System`) |>
  rename(Device_Model=`Device Model`) |>
  rename(App_Usage_Time_min_per_day=`App Usage Time (min/day)`) |>
  rename(Screen_On_Time_hr_per_day=`Screen On Time (hours/day)`) |>
  rename(Battery_Drain_mAh_per_day=`Battery Drain (mAh/day)`) |>
  rename(Data_Usage_MB_per_day=`Data Usage (MB/day)`) |>
  rename(Number_of_Apps_Installed=`Number of Apps Installed`)

all_num_vars <- c("App_Usage_Time_min_per_day", "Screen_On_Time_hr_per_day", "Battery_Drain_mAh_per_day", "Number_of_Apps_Installed", "Data_Usage_MB_per_day", "Age")
all_cat_vars <- c("Device_Model", "Operating_System", "Gender", "User_Behavior_Class")

col_extrema <- function(df) {
  return(df|>
           summarize(across(where(is.numeric), 
                            list("max" = max, "min" = min), 
                            .names = "{.fn}_{.col}")))
}

extreme_vals = unlist(col_extrema(df))

# function for finding measures of center and spread
find_center_and_spread <- function(df, group="None", num_vars) {
  if(group %in% all_cat_vars){
    return(df|>
       group_by_({{group}}) |>
       summarize(across(all_of(num_vars), 
          list("mean" = mean, "median" = median, "sd"=sd, "IQR"=IQR), 
          .names = "{.fn}_{.col}")))
  } else {
    return(df|>
      summarize(across(all_of(num_vars), 
        list("mean" = mean, "median" = median, "sd"=sd, "IQR"=IQR), 
        .names = "{.fn}_{.col}")))
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Options"),
      
      h3("Filter Data Based on Categorical Varaibles"),
      selectInput("OS_selection", "Operating System", choices=c("Android", "iOS"), multiple=TRUE),
      selectInput("model_selection", "Device Model", choices=c("Google Pixel 5", "OnePlus 9", "Xiaomi Mi 11", "iPhone 12", "Samsung Galaxy S21"), multiple=TRUE),
      
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
        
        # Data Exploration tab
        tabPanel(
          "Data Exploration",
          navset_card_underline(
            
            # Numerical Summaries
            nav_panel("Numerical Summaries", 
                      checkboxInput("show_num_summary", "Get summary of numerical variables"),
                      conditionalPanel(
                        condition="input.show_num_summary",
                        selectInput("summary_num_vars", "Numerical Variables", choices=c(all_num_vars), multiple=TRUE),
                        
                        checkboxInput("across_cat", "Get summary across a categorical variable"),
                        conditionalPanel(
                          condition="input.across_cat",
                          # selectInput("across_cat_var", "Categorical Variable", choices=c(all_cat_vars)),
                          selectizeInput("across_cat_var", "Categorical Variable", choices=c("", all_cat_vars)),
                        ),
                          
                        actionButton("go_num_summary", "Get Summary"),
                        
                        uiOutput("num_summary_text"),
                        DT::DTOutput('num_summary')
                      ),

                      checkboxInput("show_cat_summary", "Get summary of categorical variables"),
                      conditionalPanel(
                        condition="input.show_cat_summary",
                        selectInput("summary_cat_var1", "Categorical Variables", choices=c("", all_cat_vars)),
                        selectInput("summary_cat_var2", "Categorical Variables", choices=c("", all_cat_vars)),
                        actionButton("go_cat_summary", "Get Summary"),
                        uiOutput("cat_summary_one_way_text"),
                        DT::DTOutput('cat_summary_one_way'),
                        uiOutput("cat_summary_two_way_text"),
                        DT::DTOutput('cat_summary_two_way')
                      )
            ),
            
            # Graphical Summaries
            nav_panel("Plots",
                      h2("Density Plots"),
                      selectInput("plot1_x_var", "Select Variable for x-axis", choices=c("App_Usage_Time_min_per_day", "Age")),
                      selectInput("plot1_color_var", "Select Variable for Color", choices=c("Gender", "Operating_System")),
                      shinycssloaders::withSpinner(plotlyOutput("plot1")),
                      
                      h2("Scatter Plots"),
                      selectInput("plot2_x_var", "Select Variable for x-axis", choices=c("Operating_System", "Age")),
                      selectInput("plot2_y_var", "Select Variable for y-axis", choices=c("App_Usage_Time_min_per_day", "Screen_On_Time_hr_per_day", "Number_of_Apps_Installed")),
                      selectInput("plot2_color_var", "Select Variable for Color", choices=c("Gender", "Operating_System")),
                      shinycssloaders::withSpinner(plotlyOutput("plot2")),
                      
                      h2("Scatter Plots with Faceting"),
                      selectInput("plot3_x_var", "Select Variable for x-axis", choices=c("Screen_On_Time_hr_per_day", "App_Usage_Time_min_per_day")),
                      selectInput("plot3_y_var", "Select Variable for y-axis", choices=c("App_Usage_Time_min_per_day", "Battery_Drain_mAh_per_day")),
                      selectInput("plot3_color_var", "Select Variable for Color", choices=c("Gender", "Operating_System")),
                      selectInput("plot3_facet_var", "Select Variable for faceting", choices=c("Operating_System", "Device_Model")),
                      shinycssloaders::withSpinner(plotlyOutput("plot3")),
                      
                      h2("Boxplots"),
                      selectInput("plot4_x_var", "Select Variable for x-axis", choices=c("Operating_System", "Device_Model")),
                      selectInput("plot4_y_var", "Select Variable for y-axis", choices=c("Battery_Drain_mAh_per_day", "Age")),
                      shinycssloaders::withSpinner(plotlyOutput("plot4")),
                      
                      h2("Heat Maps"),
                      selectInput("plot5_x_var", "Select Variable for x-axis", choices=c("Screen_On_Time_hr_per_day", "Age")),
                      selectInput("plot5_y_var", "Select Variable for y-axis", choices=c("Battery_Drain_mAh_per_day", "Screen_On_Time_hr_per_day")),
                      shinycssloaders::withSpinner(plotlyOutput("plot5"))
            )
          )
        ),
        
        # Data Download tab
        tabPanel(
          "Data Download", 
          DT::DTOutput('data_table'),
          downloadButton("go_download", "Download Data")
        ),
        
        # About tab
        tabPanel(
          "About",
          h2("Purpose"),
          p("This app allows users to explore the mobile phone dataset from kaggle. 
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
  
  get_filtered_data <- reactive({
    filtered_data <- df
    
    # check which filter values exist and use them to filter the data
    if(length(input$OS_selection)>0)    filtered_data <- filter(filtered_data, `Operating_System` %in% input$OS_selection)
    if(length(input$model_selection)>0) filtered_data <- filter(filtered_data, `Device_Model` %in% input$model_selection)
    
    # if(length(input$var1_range[1])>0) {
    if(length(input$var1_range[1])>0 && (input$num_var1 %in% all_num_vars)) {
        filtered_data <- filtered_data |>
        filter(!!sym(input$num_var1) >= input$var1_range[1]) |>
        filter(!!sym(input$num_var1) <= input$var1_range[2])}
    
    if(length(input$var2_range[1])>0 && (input$num_var2 %in% all_num_vars)) {
      filtered_data <- filtered_data |>
        filter(!!sym(input$num_var2) >= input$var2_range[1]) |>
        filter(!!sym(input$num_var2) <= input$var2_range[2])}
    
    return (filtered_data)
  })
  
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
    cur_data <- df
    
    # check which filter values exist and use them to filter the data
    if(length(input$OS_selection)>0)    cur_data <- filter(cur_data, `Operating_System` %in% input$OS_selection)
    if(length(input$model_selection)>0) cur_data <- filter(cur_data, `Device_Model` %in% input$model_selection)

    if(length(input$var1_range[1])>0) {
      cur_data <- cur_data |>
                  filter(!!sym(input$num_var1) >= input$var1_range[1]) |>
                  filter(!!sym(input$num_var1) <= input$var1_range[2])}

    if(length(input$var2_range[1])>0) {
      cur_data <- cur_data |>
                  filter(!!sym(input$num_var2) >= input$var2_range[1]) |>
                  filter(!!sym(input$num_var2) <= input$var2_range[2])}
    
    # update download data
    proxy <- DT::dataTableProxy('data_table')
    DT::replaceData(proxy, cur_data)
  })
  
  # table to display in Data Download tab
  output$data_table = DT::renderDT(cur_data)
  
  cat_listen <- reactive({list(input$go_cat_summary, input$go_subset)})
  
  # Data Exploration tab: respond to request for categorical variable summary
  observeEvent(cat_listen(), {
    
    if(input$summary_cat_var1 %in% all_cat_vars){
      
      data <- get_filtered_data()
  
      # generate contingency tables
      one_way <- as.data.frame(table(data[,c(input$summary_cat_var1)]))
      two_way <- as.data.frame(table(as.vector(unlist(data[,c(input$summary_cat_var1)])), as.vector(unlist(data[,c(input$summary_cat_var2)]))))
      colnames(two_way) <- c(input$summary_cat_var1, input$summary_cat_var2, "Freq")
  
      # output one one-way table and one two-way table
      output$cat_summary_one_way = DT::renderDT(one_way)
      output$cat_summary_two_way = DT::renderDT(two_way)
      
      # render table title for one-way contingency table
      output$cat_summary_one_way_text <- renderUI({
        text <- isolate(paste0("One-way contingency table for ", input$summary_cat_var1))
        h2(text)
      })
      
      # render table title for two-way contingency table
      output$cat_summary_two_way_text <- renderUI({
        text <- isolate(paste0("Two-way contingency table for ", input$summary_cat_var1, " and ", input$summary_cat_var2))
        h2(text)
      })
    }
  })

  num_listen <- reactive({list(input$go_num_summary, input$go_subset)})
  
  # Data Exploration tab: respond to request for numerical variable summary
  observeEvent(num_listen(), {
    data <- get_filtered_data()
    
    # generate and output table
    summary_table <- find_center_and_spread(data, num_vars=input$summary_num_vars, group=input$across_cat_var)
    output$num_summary = DT::renderDT(summary_table)

    # render table title
    output$num_summary_text <- renderUI({
      h2("Measures of centers and spread")
    })
  })  

  # Data Exploration tab: render plots
  # Density Plots
  output$plot1 <- renderPlotly({
    data <- get_filtered_data()
    plot <- ggplot(data, aes_string(x = input$plot1_x_var)) + 
      geom_density(alpha = 0.5, aes_string(fill = input$plot1_color_var)) + 
      ggtitle(paste0(input$plot1_x_var, " By ", input$plot1_color_var))
    
    ggplotly(plot)
  })
  
  # Scatter Plots
  output$plot2 <- renderPlotly({
    data <- get_filtered_data()
    plot <- ggplot(data, aes_string(x = input$plot2_x_var, y = input$plot2_y_var, color=input$plot2_color_var)) + 
      geom_point(position = "jitter") + 
      ggtitle(paste0(input$plot2_y_var, " vs. ", input$plot2_x_var, " (Color=", input$plot2_color_var, ")"))
    
    ggplotly(plot)
  })
  
  # Scatter Plots with faceting
  output$plot3 <- renderPlotly({
    data <- get_filtered_data()
    plot <- ggplot(data, aes_string(x = input$plot3_x_var, y = input$plot3_y_var, color=input$plot3_color_var)) + 
      geom_point(position = "jitter") + 
      ggtitle(paste0(input$plot3_y_var, " vs. ", input$plot3_x_var, " By ", input$plot3_facet_var, " (Color=", input$plot3_color_var, ")")) +
      facet_wrap(~ get(input$plot3_facet_var))
    
    ggplotly(plot)
  })
  
  # Boxplots
  output$plot4 <- renderPlotly({
    data <- get_filtered_data()
    plot <- ggplot(data) +
      geom_boxplot(aes_string(x = input$plot4_x_var, y = input$plot4_y_var, fill=input$plot4_x_var)) + 
      ggtitle(paste0(input$plot4_y_var, " vs. ", input$plot4_x_var))
    
    ggplotly(plot)
  })
  
  # Heat Maps
  output$plot5 <- renderPlotly({
    data <- get_filtered_data()
    plot <- ggplot(data, aes_string(x = input$plot5_x_var, y = input$plot5_y_var)) + 
      geom_bin_2d() +
      ggtitle(paste0(input$plot5_y_var, " vs. ", input$plot5_x_var))
    
    ggplotly(plot)
  })
  
  # handler for download button
  output$go_download <- downloadHandler(
    filename = function() {
      paste0('mobile_phone_data_', Sys.Date(), '.csv')
    },
    content = function(file) {
      
      cur_data <- df
      
      # check which filter values exist and use them to filter the data
      if(length(input$OS_selection)>0)    cur_data <- filter(cur_data, `Operating_System` %in% input$OS_selection)
      if(length(input$model_selection)>0) cur_data <- filter(cur_data, `Device_Model` %in% input$model_selection)
      
      if(length(input$var1_range[1])>0) {
        cur_data <- cur_data |>
          filter(!!sym(input$num_var1) >= input$var1_range[1]) |>
          filter(!!sym(input$num_var1) <= input$var1_range[2])}
      
      if(length(input$var2_range[1])>0) {
        cur_data <- cur_data |>
          filter(!!sym(input$num_var2) >= input$var2_range[1]) |>
          filter(!!sym(input$num_var2) <= input$var2_range[2])}
      
      write.csv(cur_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
