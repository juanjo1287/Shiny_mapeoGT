#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


#################################
#########################################################
###############################################################################################
###################### Generar dashboard HTML interactivo #####################################
# --- Dashboard Shiny interactivo para visualización y extracción de valores raster ---

library(shiny)
library(leaflet)
library(terra)
library(DT)
library(plotly)
library(sf)

# Cargar raster stack recortado
raster_stack <- rast("F:/PDSER2025_Planificacion/Modelos_selec_var_suelos/Raster_limte_var_suelo/raster_stack_recortado.tif")
layer_names <- c("AI", "Ca", "Fe", "K", "Mg", "MO%", "P", "pH")
names(raster_stack) <- layer_names

# cargar shape de áreas con bajo numero o nulo numero de muestras
shape <- terra::vect("F:/PDSER2025_Planificacion/Vectores Mapeo suelo/Municipios_NA.shp")
shape_sf <- sf::st_as_sf(shape) # Convertir a sf para leaflet

# Definir unidades para cada variable
layer_units <- c(
  "AI" = "Cmol (+)/kg",
  "Ca" = "Cmol (+)/kg",
  "Fe" = "ppm",
  "K" = "Cmol (+)/kg",
  "Mg" = "Cmol (+)/kg",
  "MO%" = "%",
  "P" = "ppm",
  "pH" = ""
)



# UI
ui <- fluidPage(
  titlePanel("Dashboard Interactivo de Mapas Raster de Suelos"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("layers", "Seleccionar capas a visualizar/comparar:", 
                         choices = setNames(layer_names, 
                                            paste0(layer_names, " (", layer_units[layer_names], ")")),
                         selected = layer_names[1]),
      helpText("Haga clic en el mapa para extraer valores de los raster seleccionados."),
      actionButton("clear_points", "Limpiar puntos seleccionados")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa",
                 leafletOutput("map", height = 1000),
                 DTOutput("pixel_values")
        ),
        tabPanel("Gráficas",
                 uiOutput("stats_selected_points")
        ),
        tabPanel("Comparar Mapas",
                 uiOutput("compare_maps_ui"),
                 uiOutput("compare_maps")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive para almacenar puntos seleccionados
  points <- reactiveVal(data.frame(lon = numeric(), lat = numeric()))
  
  # Paletas de colores
  palettes <- list(
    "AI" = colorNumeric("Blues", NULL),
    "Ca" = colorNumeric("Oranges", NULL),
    "Fe" = colorNumeric("Reds", NULL),
    "K" = colorNumeric("Purples", NULL),
    "Mg" = colorNumeric("Greens", NULL),
    "MO%" = colorNumeric("YlOrBr", NULL),
    "P" = colorNumeric("YlGn", NULL),
    "pH" = colorNumeric("RdYlBu", NULL)
  )
  
  # Renderizar mapa principal
  output$map <- renderLeaflet({
    req(input$layers)
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      setView(lng = mean(ext(raster_stack)[1:2]), lat = mean(ext(raster_stack)[3:4]), zoom = 8)
    for (nm in input$layers) {
      m <- addRasterImage(
        m, raster_stack[[nm]], colors = palettes[[nm]], opacity = 0.7, group = nm, project = FALSE
      ) %>%
        addLegend(
          pal = palettes[[nm]], values = values(raster_stack[[nm]]), 
          title = paste0(nm, ifelse(layer_units[nm] != "", paste0(" (", layer_units[nm], ")"), "")), 
          group = nm, position = "bottomright"
        )
    }
    # Agregar shape con ashurado negro
    m <- m %>%
      addPolygons(
        data = shape_sf,
        fill = FALSE,
        color = "black",
        weight = 2,
        opacity = 1,
        dashArray = "5,5",
        group = "Áreas sin muestra",
        label = "Áreas sin muestra"
      )
    m %>% addLayersControl(
      baseGroups = c("OpenStreetMap", "Satélite"),
      overlayGroups = c(input$layers, "Áreas sin muestra"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })
  
  # Manejar clics en el mapa
  observeEvent(input$map_click, {
    click <- input$map_click
    df <- points()
    points(rbind(df, data.frame(lon = click$lng, lat = click$lat)))
  })
  
  # Limpiar puntos
  observeEvent(input$clear_points, {
    points(data.frame(lon = numeric(), lat = numeric()))
  })
  
  # Mostrar valores de pixeles seleccionados
  output$pixel_values <- renderDT({
    req(nrow(points()) > 0)
    coords <- points()
    vals <- extract(raster_stack[[input$layers]], coords[, c("lon", "lat")])
    df <- cbind(Punto = seq_len(nrow(coords)), coords, vals)
    # Cambiar nombres de columnas para incluir unidades
    colnames(df)[-(1:3)] <- paste0(input$layers, " (", layer_units[input$layers], ")")
    datatable(df, options = list(pageLength = 5))
  })
  
  # Mostrar puntos en el mapa con números
  observe({
    df <- points()
    leafletProxy("map") %>%
      clearMarkers()
    if (nrow(df) > 0) {
      for (i in seq_len(nrow(df))) {
        leafletProxy("map") %>%
          addLabelOnlyMarkers(
            lng = df$lon[i], lat = df$lat[i],
            label = as.character(i),
            labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, style = list(
              "color" = "black", "font-weight" = "bold", "font-size" = "16px", "background" = "white"
            ))
          )
      }
    }
  })
  
  # Estadísticas y gráficas de los puntos seleccionados
  output$stats_selected_points <- renderUI({
    req(nrow(points()) > 0)
    coords <- points()
    vals <- extract(raster_stack, coords[, c("lon", "lat")])
    df <- cbind(coords, vals)
    stats <- NULL
    if (nrow(df) > 0) {
      # Calcular estadísticas para cada variable
      stats <- sapply(df[ , -(1:2), drop = FALSE], function(x) {
        c(Media = mean(x, na.rm = TRUE),
          Mínimo = min(x, na.rm = TRUE),
          Máximo = max(x, na.rm = TRUE),
          SD = sd(x, na.rm = TRUE))
      })
      stats <- as.data.frame(t(stats))
      stats$Unidad <- layer_units[rownames(stats)]
      stats <- stats[, c("Unidad", "Media", "Mínimo", "Máximo", "SD")]
      stats[] <- lapply(stats, function(x) if(is.numeric(x)) round(x, 3) else x)
    }
    tagList(
      h4("Estadísticas de los puntos seleccionados"),
      if (!is.null(stats)) DT::datatable(stats, options = list(dom = 't', ordering = FALSE)),
      h4("Histogramas de los puntos seleccionados"),
      plotlyOutput("hist_selected_points")
    )
  })
  
  output$hist_selected_points <- renderPlotly({
    req(nrow(points()) > 0)
    coords <- points()
    vals <- extract(raster_stack, coords[, c("lon", "lat")])
    df <- as.data.frame(vals)
    # Un solo histograma por variable, todos los puntos seleccionados
    plot_list <- lapply(names(df), function(nm) {
      plot_ly(x = df[[nm]], type = "histogram", name = paste0(nm, " (", layer_units[nm], ")"))
    })
    subplot(plot_list, nrows = length(plot_list), shareX = FALSE, titleY = TRUE)
  })
  
  # Comparar dos mapas lado a lado
  output$compare_maps_ui <- renderUI({
    selectInput("compare_layers", "Seleccionar dos capas para comparar:", 
                choices = setNames(layer_names, 
                                   paste0(layer_names, " (", layer_units[layer_names], ")")),
                selected = layer_names[1:2], multiple = TRUE)
  })
  
  output$compare_maps <- renderUI({
    req(input$compare_layers)
    req(length(input$compare_layers) == 2)
    fluidRow(
      column(6, leafletOutput("map_left", height = 400)),
      column(6, leafletOutput("map_right", height = 400))
    )
  })
  
  output$map_left <- renderLeaflet({
    req(input$compare_layers)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addRasterImage(raster_stack[[input$compare_layers[1]]], 
                     colors = palettes[[input$compare_layers[1]]], opacity = 0.7, project = FALSE) %>%
      addLegend(pal = palettes[[input$compare_layers[1]]], 
                values = values(raster_stack[[input$compare_layers[1]]]), 
                title = paste0(input$compare_layers[1], 
                               ifelse(layer_units[input$compare_layers[1]] != "", 
                                      paste0(" (", layer_units[input$compare_layers[1]], ")"), "")), 
                position = "bottomright") %>%
      addPolygons(
        data = shape_sf,
        fill = FALSE,
        color = "black",
        weight = 2,
        opacity = 1,
        dashArray = "5,5",
        group = "Áreas sin muestra",
        label = "Áreas sin muestra"
      ) %>%
      setView(lng = mean(ext(raster_stack)[1:2]), lat = mean(ext(raster_stack)[3:4]), zoom = 8)
  })
  
  output$map_right <- renderLeaflet({
    req(input$compare_layers)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addRasterImage(raster_stack[[input$compare_layers[2]]], 
                     colors = palettes[[input$compare_layers[2]]], opacity = 0.7, project = FALSE) %>%
      addLegend(pal = palettes[[input$compare_layers[2]]], 
                values = values(raster_stack[[input$compare_layers[2]]]), 
                title = paste0(input$compare_layers[2], 
                               ifelse(layer_units[input$compare_layers[2]] != "", 
                                      paste0(" (", layer_units[input$compare_layers[2]], ")"), "")), 
                position = "bottomright") %>%
      addPolygons(
        data = shape_sf,
        fill = FALSE,
        color = "black",
        weight = 2,
        opacity = 1,
        dashArray = "5,5",
        group = "Áreas sin muestra",
        label = "Áreas sin muestra"
      ) %>%
      setView(lng = mean(ext(raster_stack)[1:2]), lat = mean(ext(raster_stack)[3:4]), zoom = 8)
  })
}

# Lanzar app
shinyApp(ui, server)


