    library(shiny)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(jsonlite)
    library(httr)
    library(leaflet)
    library(ggmap)
    library(maps)
    library(maptools)
    library(RgoogleMaps)
    library(sp)
    
    # Get google api key and register
    apikey ='AIzaSyBXyHYUVZMJopu3T8Ao2RRdNEyKLl5Arvo'
    register_google(key = apikey)
    
    # Load data.
    data <- read.csv('rawData.csv')
    data1 <- read.csv('data.csv')
    censusTracts <- readLines('kcCensusTracts.geojson', warn = FALSE) %>%
        paste(collapse = '\n') %>%
        fromJSON(simplifyVector = FALSE)
    neighborhoods <- readLines('kcNeighborhoods.geojson', warn = FALSE) %>%
        paste(collapse = '\n') %>%
        fromJSON(simplifyVector = FALSE)
    
    # Set geoJSON style.
    censusTracts$style = list(
        weight = 1,
        color = '#000000',
        opacity = 1,
        fillOpacity = 0.4
    )
    
    neighborhoods$style = list(
        weight = 1,
        color = '#000000',
        opacity = 1,
        fillOpacity = 0.4
    )
    
    # Format variables.
    data1$Stat_Date <- as.character(data1$Stat_Date)
    data1$Stat_Date <- as_date(data1$Stat_Date)
    
    # Merge Select Columns from both datasets.
    merged <- merge(x = data, y = data1[, c('Report_No', 'Stat_Date')], by = 'Report_No')
    
    merged$Stat_Date <- as.character(merged$Stat_Date)
    merged$Stat_Date <- as_date(merged$Stat_Date)
    
    # Functions
    
    
    # Define UI for application that draws a histogram
    ui <- fluidPage(
        
        navbarPage(
            
            title = 'KCPD Crime Analysis Project',
            
            theme = 'bootstrap5.css',
            
            tabPanel('Project Overview',
                     tags$section(
                         id = 'greetings',
                         tags$div(
                             class = 'container',
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12 text-center',
                                     img(src = 'kcpd_patch.png'),
                                     tags$h2(
                                         class = 'section-heading text-uppercase',
                                         "Crime in Kansas City, MO"
                                     ),
                                     tags$h3(
                                         class = 'section-subheading text-muted',
                                         "A predictive analysis of KCPD crime data from 2016 thru 2019."
                                     )
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     tags$p(
                                         "In 1974, in an experiment conducted by", tags$a(href = 'https://www.policefoundation.org/projects/the-kansas-city-preventive-patrol-experiment/', target = '_blank', 'Kelling et al'), " in support of The Police Foundation initiatives, it was concluded that random police patrols had little to no effect on occurrence of crime or the public’s feeling of safety. "
                                     ),
                                     tags$p(
                                         "The study suggested that more emphasis should be placed on targeted crime prevention instead of random patrols. A", tags$a(href = 'https://onlinelibrary.wiley.com/doi/full/10.1002/CL2.159', target = '_blank', 'review of the effects of police patrols'), " was then published in 2016 showing that hot-spot patrolling did result in a significant reduction of crime in those areas. " 
                                     ),
                                     tags$p(
                                         "In light of this result, we would like to help the Kansas City Police Department in determining where these hot spots are around the city. To do this we have prepared this application to aid in that endeavor. First we will dive into some basic crime statistics around the city. Then, using a model built from 2016 to 2018 crime data, we predict hot-spots for 2019 crime in Kansas City.  The results are compared to the actual 2019 crime hot-spots.  This data was obtained from", tags$a(href = 'https://data.kcmo.org', target = '_blank', "The City of Kansas City's Open Data page"), "."
                                     ),
                                     tags$p(
                                         "To use this application click on the different tabs to advance through the story and see our interactive visualizations. Our hope is that the KCPD can use this to gain some insight into crime hotspots around the city and get context to specific areas' density of crime."
                                    ),
                                    tags$p(
                                        "On the next page we will show some interactive crime statistics."
                                    )
                                ),
                                tags$div(
                                    class = 'col-lg-2'
                                )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-8'
                                 ),
                                 tags$div(
                                     class = 'col-lg-2'
                                 )
                             )
                         )
                     )
                ),
            
            tabPanel('Basic Crime Statistics',
                     tags$div(
                         class = 'container',
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12 text-center',
                                 tags$h2(
                                     "An Overview of Crime in Kansas City"
                                 )
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12',
                                 tags$p('On this page we take a look at some of the basic crime statistics from the KCPD crime Data from 2016 - 2018. With this tool you can choose an offense category from the drop down menu on the left. You can also filter by which year you would like to see. It currently defaults to all 3 years. By switching offense categories you can see the interactive heatmap and daily crime counts dynamically changes. In the Kansas City area we have identified the top 5 most occurring crimes across 2016-2018. Those crimes are Simple Assault, Aggravated Assault, Destruction/Damage/Vandalism of Property, All Other Offenses, and Burglary/Breaking & Entering. Knowing the most frequent crimes allows you to have the necessary resources on hand to respond to any of these situations.')
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12',
                                 tags$p('Here is a bargraph with the top five offenses per year. Notice how the top offence for all three years was Simple Assault.'),
                                 plotOutput('topFive'),
                                 tags$br()
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12',
                                 tags$hr()
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-4',
                                 checkboxGroupInput('years',
                                                    'Choose the years you want.',
                                                    choices = unique(merged$Year),
                                                    selected = c(2016, 2017, 2018)),
                                 selectInput('offenses',
                                             'Choose an Offense',
                                             choices = sort(unique(merged$Offense.Description), decreasing = FALSE))
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 tags$p('And here is a heatmap of Kansas City.'),
                                 plotOutput('heatmap2'),
                                 tags$br(),
                                 tags$p('And here we have an plot of daily crime counts.'),
                                 plotOutput('CrimePerDay')
                                 
                             )
                         )
                     )
                ),
            
            tabPanel('Hot Spot Modeling',
                         tags$div(
                             class = 'container',
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12 text-center',
                                     tags$h2(
                                         "Hot Spot Modeling"
                                     )
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12',
                                     tags$p('This tab shows the results of our predictive model.  The model was built on the 2016-2018 crime data and predicts the top 10 crime hot spots for 2019. Below we see two heatmaps. On the left are the predicted hotspots plotted on a map of Kansas City. To the right are the actual hotspots found in the 2019 data. Earlier we referenced a study that showed random patrolling was not effective. We have predicted the top ten census tracts with the highest total counts of crime in order to promote the use of hot-spot patrolling in place of random patrolling. As you can see our predictions were not 100% accurate when compared to the actual 2019 data, but clusters are found in similar areas.While the census tracts identified by our model differ from the actual crime hot-spots, they should be considered for patrols as well.  This is because they are predicted based on census fields most highly correlated with total crime. These census fields are: 1) “Income Population 15 years and over 10000 to 14999”, 2) “Population for whom poverty status is determined”, 3) “Poverty Below 100 percent of the poverty level”, 4) “Income Population 15 years and over 65000 to 74999”, 5) “Age 45 to 54 years” and 6)”Total Female”. With the identification of crime dense census tracts we now have a targeted approach to police dispatch and patrolling. Unless census data in the area changes drastically, these hot-spots should remain in similar locations for 2020.')
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12'
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-5',
                                     img(src = 'predcrimeplot.png', width = 600, height = 800)
                                 ),
                                 tags$div(
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-5',
                                     img(src = 'actualcrimeplot.png', width = 600, height = 800)
                                 )
                             )
                        )
                     ),
            tabPanel('Summary Data',
                     tags$div(
                         class = 'container',
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12 text-center',
                                 tags$h2(
                                     "Summary Data"
                                 )
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-2'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 p('On this tab we show the summary of total counts of crime per offense description for each reported year. This also gives context to the types of crimes that seem to be most prevalent in the KC area. 
')
                             ),
                             tags$div(
                                 class = 'col-lg-2'
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-12',
                                 dataTableOutput('data')
                             )
                         )
                     )
            )
            
        )
        
    )
    
    # Define server logic required to draw a histogram
    server <- function(input, output, session) {
        
        output$CrimePerDay <- renderPlot({
            
            merged %>%
                subset(Offense.Description == input$offenses &
                           Year == input$years) %>%
                ggplot(aes(x = Stat_Date)) +
                geom_point(stat = 'count', color = 'darkorchid4') +
                labs(title = "Daily Offense Count",
                     y = "Offenses Per Day")
            
        })
        
        output$heatmap <- renderLeaflet({
            
            map <- leaflet(width = 600, height = 800) %>%
                addTiles() %>% 
                setView(lng = mean(data1$Lon), lat = mean(data1$Lat), zoom = 9)  %>%
                addGeoJSON(neighborhoods, color = 'red')
            
            
            
            print(map)
            
        })
        
        output$heatmap2 <- renderPlot({
            
            kcdf <- get_map("kansas city, missouri", zoom =  11)
            kcMap1 <- ggmap(kcdf, extent = "device") +
                stat_density2d(aes_string(x = 'Lon', y = 'Lat'),
                               data = subset(merged, Year == input$years &
                                                     Offense.Description == input$offenses), 
                               color = 'red',
                               alpha = 0.5)
                
            
            print(kcMap1)
            
        })
        
        output$topFive <- renderPlot({
            
                merged %>%
                    subset(Year == input$years) %>%
                    group_by(Offense.Description) %>%
                    count() %>%
                    ungroup() %>%
                    top_n(n = 5) %>%
                    ggplot(aes(reorder(Offense.Description, n), n)) +
                    geom_bar(stat = 'identity', color = 'black', fill = '#E66100') +
                    coord_flip() +
                    geom_text(aes(label = n)) +
                        coord_flip() +
                    labs(title = 'Top Five Crimes in Kansas City',
                        x = '',
                        y = 'Number of Crimes')
            
        })
        
        output$data <- renderDataTable({
            
                merged %>%
                group_by(Offense.Description) %>%
                summarize(`2016` = sum(Year == 2016),
                          `2017` = sum(Year == 2017),
                          `2018` = sum(Year == 2018))
            
            })
        
    }
    
    # Run the application       
    shinyApp(ui = ui, server = server)