    library(shiny)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(jsonlite)
    library(httr)
    library(leaflet)
    
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
                                     p(
                                         "In 1974, in an experiment conducted by", tags$a(href = 'https://www.policefoundation.org/projects/the-kansas-city-preventive-patrol-experiment/', 'Kelling et al'), " in support of The Police Foundation initiatives, it was concluded that random police patrols had little to no effect on occurrence of crime or the public’s feeling of safety. "
                                     ),
                                     p(
                                         "The study suggested that more emphasis should be placed on targeted crime prevention instead of random patrols. A", tags$a(href = 'https://onlinelibrary.wiley.com/doi/full/10.1002/CL2.159', 'review of the effects of police patrols'), " was then published in 2016 showing that hot-spot patrolling did result in a significant reduction of crime in those areas. " 
                                     ),
                                     p(
                                         "In light of this result, we would like to help the Kansas City Police Department in determining where these hot spots are around the city. To do this we have prepared this application to aid in that endeavor. First we will dive into some basic crime statistics around the city. Then we will present a model using data from 2016 through 2018 on crime in Kansas City. This data was obtained from", tags$a(href = 'https://data.kcmo.org', "The City of Kansas City's Open Data page"), "."
                                     ),
                                     p(
                                         "To use this  application click on the different tabs to advance through the story and see our interactive visualizations. Our hope is that the KCPD can use this to gain some insight crime hotspots around the city and can find some benefit."
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
                                 class = 'col-lg-2'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 p('On this page we Take a look at some of the basic crime stats from the KCPD crime Data from 2016 - 2019. To manipulate the visualisations use the controls on the left.')
                             ),
                             tags$div(
                                 class = 'col-lg-2'
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
                                 selectInput('offenses',
                                             'Choose an Offense',
                                             choices = sort(unique(merged$Offense.Description), decreasing = FALSE)),
                                 checkboxGroupInput('years',
                                                    'Choose the years you want.',
                                                    choices = unique(merged$Year),
                                                    selected = c(2016, 2017, 2018))
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 tags$p('The final product will include an interactive heatmap that is filterable by Offense and Year. There will also be a section for mouse hover telling what neighborhood you are in.'),
                                 leafletOutput('heatmap')
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-4'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 tags$br()
                             )
                         ),
                         tags$div(
                             class = 'row',
                             tags$div(
                                 class = 'col-lg-4'
                             ),
                             tags$div(
                                 class = 'col-lg-8',
                                 tags$p('And here we have an interactive plot of daily crime counts.'),
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
                                     class = 'col-lg-2'
                                 ),
                                 tags$div(
                                     class = 'col-lg-8',
                                     p('On this tab we model the 2019 crime data. Below we see two heatmaps. On the left is the predicted hotspots plotted on a map of Kansas City. To the right are the actual hotspots found in the 2019 data.')
                                 ),
                                 tags$div(
                                     class = 'col-lg-2'
                                 )
                             ),
                             tags$div(
                                 class = 'row',
                                 tags$div(
                                     class = 'col-lg-12',
                                     selectInput('offenses2',
                                                 'Choose an Offense',
                                                 choices = sort(unique(merged$Offense.Description), decreasing = FALSE))
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
        
    }
    
    # Run the application       
    shinyApp(ui = ui, server = server)