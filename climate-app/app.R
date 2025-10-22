library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(geoR)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(shinycssloaders)

if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  remotes::install_github("ropensci/rnaturalearthhires")
}

library(rnaturalearthhires)

# -----------------------------
# Load data
# -----------------------------
rainfall_data <- read.csv(here::here("data/dataset_2025_aus.csv"))
rainfall_data$Monthly_Rainfall_mm <- rainfall_data$Monthly_Rainfall_mm + 0.1

# NSW polygon
states_sf <- ne_states(country = "Australia", returnclass = "sf")
nsw_sf <- states_sf %>% filter(name == "New South Wales")
nsw_sp <- as_Spatial(nsw_sf)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("NSW Rainfall Explorer (2025)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:", choices = month.name),
      radioButtons("mode", "Mode:", choices = c("Rain Stations", "Universal Kriging")),
      actionButton("run", "Update Map"),
      tags$hr(),
      helpText("Hover over stations to see rainfall. Click 'Update Map' after changing month or mode.")
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        leafletOutput("map", height = "700px"),
        type = 6, color = "#00aaff"
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  map_data <- eventReactive(input$run, {
    month_i <- match(input$month, month.name)
    df <- rainfall_data %>%
      filter(Month == month_i) %>%
      filter(!is.na(Monthly_Rainfall_mm))
    req(nrow(df) > 0)
    
    if (input$mode == "Rain Stations") {
      return(list(mode = "stations", df = df))
    } else {
      # Try loading precomputed raster first
      raster_file <- paste0("data/raster_nsw_", month_i, ".rds")
      if (file.exists(raster_file)) {
        r <- readRDS(raster_file)
        return(list(mode = "kriging", df = df, raster = r))
      }
      
      # If raster not found, compute on-the-fly (fallback for local use)
      withProgress(message = "Computing Kriging...", value = 0, {
        incProgress(0.2, detail = "Preparing data")
        
        geodata <- as.geodata(cbind(df$Longitude, df$Latitude, log10(df$Monthly_Rainfall_mm)))
        geodata <- jitterDupCoords(geodata, max = 1e-5)
        
        v <- variog(geodata, option = "bin")
        fitted_model <- variofit(
          v,
          cov.model = "lin",
          ini.cov.pars = c(0.1, 0.1),
          nugget = 0.01,
          weights = "equal"
        )
        
        incProgress(0.3, detail = "Fitting variogram model")
        
        # Prediction grid over NSW
        grid_by <- if (nrow(df) > 100) 0.2 else 0.1
        bbox <- st_bbox(nsw_sf)
        grid <- expand.grid(
          Longitude = seq(bbox["xmin"], bbox["xmax"], by = grid_by),
          Latitude  = seq(bbox["ymin"], bbox["ymax"], by = grid_by)
        )
        
        incProgress(0.3, detail = "Performing Kriging")
        
        uk_model <- krige.conv(
          geodata = geodata,
          locations = as.matrix(grid),
          krige = krige.control(
            trend.d = "2nd",
            trend.l = "2nd",
            obj.model = fitted_model
          )
        )
        
        # Save predictions
        grid$rain_pred <- 10^(uk_model$predict)
        grid_sf <- st_as_sf(grid, coords = c("Longitude", "Latitude"), crs = 4326)
        
        r <- rasterFromXYZ(cbind(st_coordinates(grid_sf), grid_sf$rain_pred))
        crs(r) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
        r <- raster::crop(r, nsw_sp)
        r <- raster::mask(r, nsw_sp)
        r[is.na(r[])] <- NA
        
        incProgress(0.2, detail = "Done")
        return(list(mode = "kriging", df = df, raster = r))
      })
    }
  })
  
  # -----------------------------
  # Render Leaflet map
  # -----------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addPolygons(
        data = nsw_sf, fill = FALSE, color = "black", weight = 1
      )
  })
  
  observe({
    res <- map_data()
    req(res)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearImages() %>%
      clearControls()
    
    # Unified color palette
    all_values <- res$df$Monthly_Rainfall_mm
    if (res$mode == "kriging" && !is.null(res$raster)) {
      all_values <- c(all_values, values(res$raster))
    }
    pal <- colorNumeric("YlGnBu", domain = all_values, na.color = "transparent")
    
    # Rain stations
    leafletProxy("map") %>%
      addCircleMarkers(
        data = res$df,
        lng = ~Longitude, lat = ~Latitude,
        radius = 5,
        color = ~pal(Monthly_Rainfall_mm),
        fillColor = ~pal(Monthly_Rainfall_mm),
        fillOpacity = 0.9,
        label = ~paste0(
          "<b>Station ID:</b> ", Station_ID,
          "<br><b>Rainfall:</b> ", round(Monthly_Rainfall_mm, 1), " mm"
        ),
        labelOptions = labelOptions(direction = "auto", textsize = "12px")
      )
    
    # Kriging raster
    if (res$mode == "kriging" && !is.null(res$raster)) {
      leafletProxy("map") %>%
        addRasterImage(
          res$raster,
          colors = pal,
          opacity = 0.85,
          project = TRUE
        )
    }
    
    # Legend
    leafletProxy("map") %>%
      addLegend(
        pal = pal,
        values = all_values,
        title = "Rainfall (mm)"
      )
  })
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui, server)

