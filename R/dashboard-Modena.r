library(shiny)
library(sf)
library(mapview)
library(dplyr)
library(leaflet)

options(shiny.port = 8888)
mapviewOptions(default = TRUE)

streamfile <- paste( getwd(), "/../Data/stream.csv", sep = "", collapse = NULL)

map_image_web <- paste( getwd(), "/../Images/map.html", sep = "", collapse = NULL)
map_image_png <- paste( getwd(), "/../Images/map.png", sep = "", collapse = NULL)

road_attiraglio <- st_multilinestring(list(rbind(c(10.934070, 44.654792),c(10.934499, 44.655202))))
road_attiraglio2 <- st_multilinestring(list(rbind(c(10.933446, 44.654193),c(10.934368, 44.655104),c(10.934525, 44.655202),c(10.934583, 44.655172), c(10.934733,44.655175), c(10.934822,44.655238),c(10.934843,44.655266),c(10.936375,44.655274),c(10.937449,44.655166))))
road_canaletto <- st_multilinestring(list(rbind(c(10.933487, 44.656217),c(10.934195,44.655732),c(10.934603, 44.655309))))
road_canaletto2 <- st_multilinestring(list(rbind(c( 10.934512, 44.655365),c(10.934222, 44.655703),c(10.933978, 44.655915),c(10.930990, 44.657819),c(10.930739, 44.658001))))
road_montalcini <- st_multilinestring(list(rbind(c(10.931728,44.655004),c(10.934223,44.653813))))
road_fanti <- st_multilinestring(list(rbind(c(10.929026,44.656234),c(10.929310,44.656222),c(10.931728,44.655004))))
road_mazzoni <- st_multilinestring(list(rbind(c(10.932857,44.652442),c(10.934105, 44.653557),c(10.934417,44.653969),c(10.934607,44.654279),c(10.934642,44.654855),c(10.934647,44.655161))))
road_montessori <- st_multilinestring(list(rbind(c(10.928966,44.656453),c(10.929030, 44.656658),c(10.929299,44.657032),c(10.929966,44.657778),c(10.930107,44.657914),c(10.930279,44.658043))))

modena_roads <- st_sfc(road_attiraglio2,road_canaletto2,road_montalcini,road_fanti,road_mazzoni,road_montessori, crs="EPSG:4326")
menu_selector <- c("LinkID","Speed_av","NOx","HC","CO","PM","PN" ,"NO")
menu_title <- c("Roads","Speed (km/h)","NOx (kg/h)","HC (kg/h)", "CO (kg/h)","PM (kg/h)","PN (kg/h)","NO (kg/h)")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Modena Pollution Map"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(position = "right",
    sidebarPanel(
      selectInput("gasType", "Gas type",
                  choices = menu_selector[1:8],
                  selected = menu_selector[2]
      ),
      width = 2

    ),
    # Show a plot of the generated distribution
    mainPanel(
      mapviewOutput("mapPlot"),
      width = 10)
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  reader <- reactiveFileReader(intervalMillis = 200, NULL, filePath =
                                 streamfile, readFunc = read.csv)

  dataframe <- reactive({
    vehicle_data <- reader()
    clean_data <- vehicle_data %>% select("LinkID","link_speed_av","NOx","HC","CO","PM","PN","NO")
    mean_data <- aggregate( clean_data[, 2:8], list(clean_data$LinkID), mean)
    colnames(mean_data) <- c("LinkID", "Speed_av","NOx", "HC","CO", "PM" ,"PN" ,"NO")
    final_data <- st_sf(mean_data[1],mean_data[2],mean_data[3],mean_data[4],mean_data[5],mean_data[6],mean_data[7],mean_data[8], geometry = modena_roads)
    f <-final_data
    f
  })


  zcolumn <- reactive({
    value <- NULL
    if(input$gasType != "All")  value <-which(menu_selector== input$gasType)
    value
  })


  output$mapPlot <- renderLeaflet({
    input$data
    #get the data
    f <- dataframe()
    zcolValue <- zcolumn()
    #generate a map
    mapviewOptions(basemaps = c("OpenStreetMap"), vector.palette =  colorRampPalette(c("dark blue","blue","cyan","springgreen","olivedrab1","orange","orangered","red")))
    mapview(f, zcol = menu_selector[zcolValue], layer.name = menu_title[zcolValue], at = c(-Inf, 0.2, 0.5, 1.0, 5.0, 10.0, 20.0, 50.0, Inf), zoom = 14, legend = TRUE, lwd = 5)@map
    # map_result <- mapview(f, zcol = zcolValue, at = seq(0.0, 25.0, 2.0), zoom = 16, legend = TRUE, lwd = 6)
    # mapshot(map_result,url = map_image_web, file = map_image_png)
  })
}

# Run the application
app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 8888, launch.browser = FALSE)


