library(shinythemes)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(geojsonio)
library(leaflet)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(dplyr)

# Load data ---------------------------------------------------------------
countries = read.csv("map_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("map_data/110m.geojson", what = "sp")
dat <- read.csv("map_data/data.csv", header = T, sep = ",")

# Pre-process data --------------------------------------------------------
data <- merge(x = dat, y = countries, by.x = "iso3c", by.y = "alpha3") %>%
  filter(!is.na(iso3c))
data$quantity_norm <- (data$quantity-min(data$quantity))/(max(data$quantity)-min(data$quantity))

# set mapping colour for each outbreak
col = "#cc4c02"


# Generate interactive maps -----------------------------------------------
# creat sars interactive map (needs to include polygons and circles as slider input not recognised upon initial loading)

#Only paint countries present in both datasets
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% data$iso3c, ]

# Create a color gradient
bins = as.vector(quantile(data$quantity_norm, probs = seq(0,1, length.out=9)))
pal <- colorBin("Oranges", domain = data$quantity_norm, bins = bins)

# Filter data based on the raster, and very important to remove duplicated countries
data_polygon <- data[data$iso3c %in% plot_map$ADM0_A3, ]
data_polygon <- data_polygon[!duplicated(data_polygon$iso3c),]
data_polygon <- data_polygon[order(match(data_polygon$iso3c, plot_map$ADM0_A3)),]

# Map with quantity
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  # addLayersControl(
  #   position = "bottomright",
  #   overlayGroups = c("2003-SARS (cumulative)", "2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"),
  #   options = layersControlOptions(collapsed = FALSE)) %>% 
  # hideGroup(c("2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~pal(data_polygon$quantity_norm), #group = "2003-SARS (cumulative)",
                label = sprintf("<strong>%s</strong><br/>Quantity: %g", data_polygon$country.name.en, data_polygon$quantity) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = col),
                textsize = "15px", direction = "auto")) %>%
  addCircleMarkers(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(quantity_norm)*5, 
                   fillOpacity = 0.2, color = col, #group = "2003-SARS (cumulative)",
                   label = sprintf("<strong>%s</strong><br/>Quantity: %g", data$country, data$quantity) %>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px", "color" = col),
                     textsize = "15px", direction = "auto")) 

# Map with overall
factpal <- colorFactor(brewer.pal(n = length(unique(data_polygon$overall)), name = "Dark2"), data_polygon$overall)
factpal_corrected <- function(x, value = "No data", color="white"){
  y <- factpal(x)
  y[x==value] <- color
  y
}

overallmap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~factpal_corrected(data_polygon$overall), #group = "2003-SARS (cumulative)",
              label = sprintf("<strong>%s</strong><br/>%s", data_polygon$country.name.en, data_polygon$overall) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = "gray40"),
                textsize = "15px", direction = "auto"))

overallmap_pie = leaflet(plot_map) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = col, #group = "2003-SARS (cumulative)",
              label = sprintf("<strong>%s</strong><br/>%s", data_polygon$country.name.en, data_polygon$overall) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = "gray40"),
                textsize = "15px", direction = "auto"))

# Other useful functions --------------------------------------------------
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)


# plot functions ----------------------------------------------------------
density_plot <- function(data){
  ggplot(data, aes(x = quantity)) + geom_density(fill=col, color=col, alpha=0.3) +
    ylab("density") +  xlab("quantity") + theme_bw() + 
    scale_colour_manual(values=c(col)) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
}

piechart_plot <- function(data, x = "basic.systems", title=NULL, turn_gray = NULL, colfun = NULL){

  if(nrow(data)==0){
    dat <- data.frame(x= "No data", proportions = 100, ypos = 50)
    colnames(dat)[1] <- x
    col_names <- c("No data")
    cols_pie <- c("white")
  }else{
    dat <- data %>% group_by(!!sym(x)) %>% summarize(value=n()) %>%
      arrange(desc(!!sym(x))) %>%
      mutate(proportions = value / sum(value) *100) %>%
      mutate(ypos = cumsum(proportions)- 0.5*proportions )
    col_names <- dat %>% pull(!!sym(x))
    if(is.null(colfun)){
      cols_pie <- brewer.pal(n = length(col_names), name = "Dark2")
    }else{
      cols_pie <- colfun(col_names)
    }
  }

  cols_text <- rep("white", length(col_names))
  names(cols_pie) <- col_names
  names(cols_text) <- col_names
  if(!is.null(turn_gray) & (turn_gray %in% names(cols_pie))){
    cols_pie[turn_gray] <- "white"
    cols_text[turn_gray] <- "gray30"    
  }
  
  p <- ggplot(dat, aes(x="", y=proportions, fill=!!sym(x))) +
    geom_bar(stat="identity", width=1, color="gray90", alpha = 0.5) +
    coord_polar("y", start=0) +
    theme_void() + 
    theme(legend.title = element_blank()) +
    geom_text(aes(y = ypos, label = paste0(round(proportions,1), "%")), color = cols_text, size=2) +
    scale_fill_manual(values = cols_pie)
  
  if(!is.null(title)){p <- p + ggtitle(title)}
  return(p)
}


# UI side of SHINY --------------------------------------------------------
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">WMO exercise</a>'), id="nav",
             windowTitle = "WMO exercise",
             tabPanel("country map",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h4(textOutput("overall_counts"), align = "right"),
                                        plotOutput("density_plot", height="130px", width="100%"),
                                        sliderInput(
                                          inputId = "plotQuantity",
                                          label = h5("Select mapping quantity"),
                                          min = floor_dec(min(data$quantity), 2),
                                          max = ceiling_dec(max(data$quantity), 2),
                                          value = c(floor_dec(min(data$quantity), 2),ceiling_dec(max(data$quantity), 2)),
                                          step=0.01)
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 100, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("user", label = "", icon = icon("user"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://bernatbramon.com/"))),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("github", label = "", icon = icon("github"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://github.com/bernibra/shiny-template"))),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("wmo", label = "", icon = icon("globe"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://public.wmo.int/en")))
                          
                          
                      )
             ),
             tabPanel("pie map",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap_services", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        plotOutput("pie_overall", height="130px", width="100%")
                          ),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 100, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("user", label = "", icon = icon("user"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://bernatbramon.com/"))),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("github", label = "", icon = icon("github"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://github.com/bernibra"))),
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("wmo", label = "", icon = icon("globe"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://public.wmo.int/en")))
                          
                          
                      )
             ),
             tabPanel("Pie charts",
                      div(
                        tags$head(includeCSS("styles.css")),
                      fluidRow(
                        column(5,
                               div(id = "button-div", 
                                   checkboxGroupButtons(
                                        inputId = "region",
                                        label = "Region",
                                        choices = unique(data$continent),
                                        justified=T,
                                        size = "sm",
                                        status = "custom-class",
                                        individual = T,
                                        direction = "vertical"
                                      )
                                   )
                        ),
                        column(5, offset = 2,
                               div(id = "button-div", 
                                   checkboxGroupButtons(
                                     inputId = "income",
                                     label = "Income",
                                     choices = unique(data$income),
                                     justified=T,
                                     size = "sm",
                                     status = "custom-class",
                                     individual = T,
                                     direction = "vertical"
                                   )
                               )
                              )      
                      ),
                      fluidRow(br()),
                      fluidRow(
                        column(4,
                               # h4("Basic Systems", align = "left"),
                               plotOutput("pie_basic")
                        ),
                        column(4,
                               # h4("Provision & Application", align = "left"),
                               plotOutput("pie_provision")
                        ),
                        column(4,
                               # h4("Governance", align = "left"),
                               plotOutput("pie_governance")
                        )  
                      ),
                      fluidRow(
                        div(
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap_services_selected")
                        )
                      )
                    )
             )
  )
)


# SHINY SERVER ------------------------------------------------------------

server = function(input, output, session) {
  
  # FIRST TAB: country map
  
  reactive_db = reactive({
    if(diff(input$plotQuantity)>0.2){
      data %>% filter(quantity>=input$plotQuantity[1] & quantity<=input$plotQuantity[2])      
    }else{
      data %>% filter(quantity>=mean(input$plotQuantity)-0.1 & quantity<=mean(input$plotQuantity)+0.1)            
    }
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db()$iso3c, ]
  })
  
  output$overall_counts <- renderText({
    paste0(prettyNum(mean(reactive_db()$quantity), big.mark=","), " value")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  output$density_plot <- renderPlot({
    density_plot(data)
  })

  observeEvent(input$plotQuantity, {
    output$density_plot <- renderPlot({density_plot(reactive_db())})
  })
  
  observeEvent(input$plotQuantity, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      # clearShapes() %>%
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(quantity_norm)*5, 
                       fillOpacity = 0.1, color = col, #group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s</strong><br/>Quantity: %g", reactive_db()$country, reactive_db()$quantity) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = col),
                         textsize = "15px", direction = "auto"))# %>%
      # addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~pal(reactive_db()$quantity_norm))
  })

  #### SECOND TAB: overall pie with map
  
  # First pie
  output$pie_overall <- renderPlot({
    piechart_plot(data, x = "overall", turn_gray = "No data", colfun=factpal)
  })
  
  # Map
  output$mymap_services <- renderLeaflet({ 
    overallmap
  })
  
  #### THIRD TAB: pie charts

  reactive_pies = reactive({
    if(is.null(input$region)){
      if(is.null(input$income)){
        data_ <- data
      }else{
        data_ <- data %>% filter(income %in% input$income)
      }
    }else{
      if(is.null(input$income)){
        data_ <- data %>% filter(continent %in% input$region)
      }else{
        data_ <- data %>% filter(income %in% input$income) %>% filter(continent %in% input$region)
      }
    }
    print(data_)
    return(data_)
  })
  
  # First pie
  output$pie_basic <- renderPlot({
    piechart_plot(data, x = "basic.systems", title="Basic Systems", turn_gray = "No data", colfun=factpal)
  })

  observeEvent(c(input$region , input$income), {
    output$pie_basic <- renderPlot({piechart_plot(reactive_pies(), x = "basic.systems", title="Basic Systems", turn_gray = "No data", colfun=factpal)})
  })

  # Second pie    
  output$pie_provision <- renderPlot({
    piechart_plot(data, x = "provision", title="Provision & Application", turn_gray = "No data", colfun=factpal)
  })
  
  observeEvent(c(input$region , input$income), {
    output$pie_provision <- renderPlot({piechart_plot(reactive_pies(), x = "provision", title="Provision & Application", turn_gray = "No data", colfun=factpal)})
  })
  
  # Third pie
  output$pie_governance <- renderPlot({
    piechart_plot(data, x = "governance", title="Governance", turn_gray = "No data")
  })
  
  observeEvent(c(input$region , input$income), {
    output$pie_governance <- renderPlot({piechart_plot(reactive_pies(), x = "governance", title="Governance", turn_gray = "No data")})
  })
  
  # Map under pies
  output$mymap_services_selected <- renderLeaflet({
    overallmap_pie %>%  fitBounds(~-40,-30,~60,70)
  })

  reactive_polygons_pie = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_pies()$iso3c, ]
  })

  observeEvent(c(input$region , input$income), {
    leafletProxy("mymap_services_selected") %>%
      clearShapes() %>%
        addPolygons(data = reactive_polygons_pie(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = col, #group = "2003-SARS (cumulative)",
        )
  })
}


# runApp(shinyApp(ui, server), launch.browser = TRUE)
# rsconnect::deployApp(appDir = ".", appName = "wmo-exercise", appFiles = c( "app.R", "styles.css", paste0("./map_data/", list.files("./map_data/"))))

shinyApp(ui, server)


