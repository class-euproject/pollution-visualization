library(shiny)
library(sf)
library(mapview)
library(dplyr)
library(leaflet)

options(shiny.port = 8888)
mapviewOptions(default = TRUE)

streamfile <- paste( getwd(), "/../Data/stream.csv", sep = "", collapse = NULL)

map_image_web <- paste( getwd(), "/Images/map.html", sep = "", collapse = NULL)
map_image_png <- paste( getwd(), "/Images/map.png", sep = "", collapse = NULL)

city_roads <-st_read("../Data/modena_roads.csv")
city_roads$WKT <- NULL

menu_selector <- c("LinkID","Speed_av","NOx","HC","CO","PM","PN" ,"NO")
menu_title <- c("Roads","Speed (km/h)","NOx (kg/h)","HC (kg/h)", "CO (kg/h)","PM (kg/h)","PN (kg/h)","NO (kg/h)")

legendLabel =  c("< 0.2", "0,2 - 0.5","0.5 - 1.0","1.0 - 5.0","5.0 - 10.0", "10.0 - 20.0","20.0 - 50.0","> 50")
legendAt = c(-Inf, 0.2, 0.5, 1.0, 5.0, 10.0, 20.0, 50.0, Inf)
colourPalete = c("#00008b", "blue", "cyan", "springgreen", "#b0ec41", "orange", "orangered", "red")

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
    dataframe <- merge(mean_data, city_roads, by="LinkID")
    geodataframe <-st_sf(dataframe, crs="EPSG:4326")
    f <-geodataframe
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
    mapviewOptions(basemaps = c("OpenStreetMap"), vector.palette =  colorRampPalette(colourPalete))
    mapview(f, zcol = menu_selector[zcolValue], layer.name = menu_title[zcolValue], at = legendAt, zoom = 15, legend = FALSE, lwd = 5)@map %>% addLegend("topleft", colors = colourPalete, labels = legendLabel, title = menu_title[zcolValue], opacity = 1)
  })
}

# Run the application
app <- shinyApp(ui = ui, server = server)
runApp(app, host ="0.0.0.0", port = 8888, launch.browser = FALSE)

