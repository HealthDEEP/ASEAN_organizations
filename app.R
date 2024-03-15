library(shiny)
library(leaflet)
library(leafpop)
library(DT)

# reads in data 
organisation <-read.csv("./data/aseandata.csv")

# ui object
ui <- fluidPage(
  titlePanel(
    h1("Organizations - ASEAN", style = "color:#3474A7"),
    h3("International and regional organizations - ASEAN", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="Role", 
                  label ="Sector", 
                  choices=unique(organisation$Role)
      )
    ),
    mainPanel(
      leafletOutput(outputId = "map"),
      DTOutput(outputId = "table",
               width = "100%", height = "auto")
                    )
    )
  )


server = function(input, output) {
  organisation2<- dplyr::select(organisation,Role,id_name,URL)
  output$table <- renderDT(
    # CHANGE by input$Role
    datafiltered <- (outputId =
                       organisation2[which(organisation2$Role == input$Role), ]),
    extensions = "Buttons",
    options = list(paging = TRUE,
                   scrollX=TRUE,
                   searching = TRUE,
                   ordering = TRUE,
                   dom = 'l<"sep">Bfrtip',
                   buttons = c('csv', 'excel'),
                   pageLength=10,
                   lengthMenu=c(10,20,50,100)
                     
    ))
  output$map <- renderLeaflet({
    # CHANGE by input$Role
    datafiltered <- organisation[which(organisation$Role == input$Role), ]
    
    # reactive expression code required here to connect with ui selection?
    datafiltered  %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(~lon,~lat,fillColor = "lightblue",
                       label = ~as.character(id_name),
                       popup=popupTable(datafiltered, zcol = 3:5),
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F, 
                                                             maxClusterRadius = 100))
    
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
