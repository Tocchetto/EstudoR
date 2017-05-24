library(shiny)
library(leaflet)
library(shinyBS)
library(shinyjs)
library(RColorBrewer)
library(DT)
library(rgdal)
library(raster)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), #tags style para criar estilo css 
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom=10, left=10,
                checkboxInput("show_communities", "Cidades", TRUE), #Quando clicar aqui mostrar os marcadores em cima das cidades
                checkboxInput("show_extent", "Crop/mask", FALSE), #Abrir a opção para upload do shape
                checkboxInput("show_colpal", "Color options", FALSE)
                #checkboxInput("legend", "Legend", TRUE) #?
  ),
  absolutePanel(top = 10, right = 10, #Cria um painel cujo conteúdo está posicionado top 10 e right 10.
    sliderInput("range", "Década", min(quakes$mag), max(quakes$mag), #Constrói um widget de controle deslizante
                value = range(quakes$mag), step = 0.1
                ),
    selectInput("colors", "Color Scheme", #Cria uma lista de seleção
                rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
    checkboxInput("legend", "Show legend", TRUE), #Cria uma caixa de seleção
  conditionalPanel("input.show_extent == true", #Cria um painel visível 
                  fluidRow( #cria linhas
                     column(6,
                            actionButton("btn_modal_shp", "Upload shapefile", class="btn-block"), #Cria um botão de ação ou link 
                            uiOutput("Shp_On") #bloco
                     ),
                     column(3, h4("Mask:")),
                     column(3, uiOutput("Mask_in_use"))
                   )
  ))
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],] #filtra de acordo com a escala de magnitude do conjunto de dados
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag) #cria uma paleta de cores de acordo com os terremotos
  })
  
  output$map <- renderLeaflet({ #retorna o mapa que é um objeto
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>% #adiciona camadas ao mapa
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) #limites do mapa
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal() #o objeto "pal" recebe a paleta de cores
    
    leafletProxy("map", data = filteredData()) %>% #leaflet Proxy modifica um mapa que já está sendo executado na página
      clearShapes() %>% #limpa o que tinha anteriormente 
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777", #adiciona novas cores aos circulos
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag) #weight é a borda e fillcolor é a cor de preenchimento
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)
