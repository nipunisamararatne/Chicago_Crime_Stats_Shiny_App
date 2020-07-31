if(!require('pacman'))install.packages('pacman')
pacman::p_load(shiny, htmltools,leaflet,tidyverse, plyr, 
               viridis, stringr,lubridate, ggplot2, shinydashboard)

# Load data
chicago18_crimes <- read_csv("Crimes_2018.csv")



#Overall Pre-processing:
#check for missing data
sum(is.na.data.frame(chicago18_crimes))

#remove missing data
chicago18_crimes <- na.omit(chicago18_crimes)



#Tab 1 Pre-Processing:
#create month column
chicago18_crimes$Month <- as.integer(substr(chicago18_crimes$Date, start = 1, stop = 2))

#frequency table of primary type & month
tab1_data <- as.data.frame(table(chicago18_crimes$`Primary Type`, chicago18_crimes$Month))

#rename columns of frequency table
names(tab1_data) <- c("Crime_Type", "Month", "Frequency")

#rename month names
tab1_data$Month <- month.name[tab1_data$Month]



#Tab 2 Pre-Processing
#create new tab2 column for use in tab 2
chicago18_crimes <- chicago18_crimes %>%
  mutate(tab2 = paste('<b> Crime Type:',chicago18_crimes$`Primary Type`,'</b>')) %>%
  mutate(tab2 = paste(sep = '<br/> Date & Time: ', tab2,chicago18_crimes$Date)) %>%
  mutate(tab2 = paste(sep = '<br/> Location: ', tab2,chicago18_crimes$`Location Description`))



#Tab 3 Pre-processing
#turn date to date format so that hr will be in 24hr clock standard
chicago18_crimes$Date <- mdy_hms(chicago18_crimes$Date)

#create hour column
chicago18_crimes$Hour <- as.integer(substr(chicago18_crimes$Date, start = 12, stop = 13))

#frequency table of primary type vs hour
tab3_data <-  as.data.frame(table(chicago18_crimes$`Primary Type`, chicago18_crimes$Hour))

#rename column names
names(tab3_data) <- c("Crime Type", "Hour", "Frequency")



#Shiny App

sidebar <- dashboardSidebar(width = 100,
                            sidebarMenu(
                              menuItem("Barchart",tabName = "barchart"),
                              menuItem("Map", tabName = "map"),
                              menuItem("Heatmap", tabName = "heatmap")
                            )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "barchart",
            h2("Frequency of Crime by Type of Crime & Month"),
            selectInput(inputId = "month",
                        label = ("Select month:"),
                        choices = tab1_data$Month, 
                        selected = 1),
            selectInput(inputId = "primarytype", 
                        label = "Select crime type(s):",
                        choices = tab1_data$Crime_Type, 
                        multiple = TRUE),
            actionButton(inputId = "runChart",
                         label = "Generate Chart"),
            plotOutput(outputId = "tab1output", width = 800, height = 800)
    ),
    
    tabItem(tabName = "map",
            h2("Location of Crimes by Date"),
            dateRangeInput(
              inputId = "dates",
              label = "Select dates:",
              start = "2018-01-01",
              end = "2018-12-31",
              format = "yyyy/mm/dd",
              separator = "to"
            ),leafletOutput(outputId = "tab2output", width = 1200, height = 1200)
    ),
    
    tabItem(tabName = "heatmap",
            h2("Hour of Day vs. Type of Crime"),
            plotOutput(outputId = "tab3output", width = 800, height = 800)
    )
  )
)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Shiny Assignment 1"),
    sidebar,
    body
  ),
  
  server = function(input, output) { 
    
    #filter tab1data based on input about month & primarytype
    tab1Input <- reactive({
      tab1_data %>%
        filter(Crime_Type %in% input$primarytype & Month == input$month)
    })
    
    #create chart based on input & action button
    chartGeneration <- eventReactive(input$runChart,{
      #barchart code
      x <- ggplot(data=tab1Input(), aes(x = reorder(Crime_Type, Frequency), y = Frequency))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE)) + theme_classic() +
        coord_flip() + xlab("Type of Crime") + ylab("Frequency") + 
        labs(title = paste("Frequency of Crime Type(s) in ", input$month, sep=""))
      x + geom_text(aes(label = Frequency), hjust = 0)
    })
    
    #display created chart
    output$"tab1output" <- renderPlot({
      chartGeneration()
    })
    
    output$"tab2output" <- renderLeaflet({
      
      #to stop computer from glitching only used 300 obs
      chicago18_crimes <- sample_n(chicago18_crimes, 300)
      
      #filter dates based on input
      tab2Input <- reactive({
        chicago18_crimes %>%
          filter(Date >= input$dates[1] & Date <= input$dates[2])
      })
      
      #map code
      leaflet(tab2Input()) %>%
        addTiles(group = 'OSM') %>% 
        addMarkers(~Longitude, 
                   ~Latitude, popup = ~tab2)%>% 
        setView(lng = -87.6, lat = 42, zoom = 11)  
    }) 
    
    #heatmap code
    output$"tab3output" <- renderPlot({
      ggplot(tab3_data, aes(x = tab3_data$Hour, y = tab3_data$`Crime Type`, 
                            fill= tab3_data$Frequency)) + scale_fill_viridis(discrete=FALSE,
                                                                             name = "Frequency") + xlab("Hour") + ylab("Type of Crime")+ geom_tile()
    }) 
  }
)
