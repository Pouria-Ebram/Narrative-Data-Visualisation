# Import required libraries
# load required packages

require(dplyr)
require(maps)
require(ggplot2)
require(reshape2)
require(RColorBrewer)
require(leaflet)
require(geojsonio)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)


# set a color for hover over
covid_col = "#641E16"

# import data
country_data <- read.csv('param.csv')
life_data <- read.csv('life.csv')
health_data <- read.csv('health.csv')
age_data <- read.csv('age.csv')
smoke_data <- read.csv('smoke.csv')
density_data <- read.csv('density.csv')
worldcountry = geojson_read("50m.geojson", what = "sp")


# change the order of the categories in country data for plotting later
country_data$age_cat <- ordered(country_data$age_cat, levels=c("Old (over 37)", "Mid (27-37)", "Yng (15-27)"))
country_data$life_cat <- ordered(country_data$life_cat, levels=c("High (over 77)","Mid (70-77)","Low (50-70)"))
country_data$health_cat <- ordered(country_data$health_cat, levels=c("High (over 8%)","Mid (5%-8%)","Low (1%-5%)"))
country_data$smoke_cat <- ordered(country_data$smoke_cat, levels=c("High (over 24%)","Mid (18%-24%)","Low (2%-18%)"))
country_data$dens_cat <- ordered(country_data$dens_cat, levels=c("ED (over 220) ppskm","HD (96-220) ppskm","MD (42-96) ppskm","LD (2-42) ppskm"))

# filter out outlier countries for density
out_country <- subset(country_data,pop_dens_ppsqkm < 1000)
# filter outliers for health expenditure
health_country <- subset(country_data,health_expend_gdp < 13)

# fix the date
age_data$date = as.Date(age_data$date, format='%d/%m/%y')
life_data$date = as.Date(life_data$date, format='%d/%m/%y')
health_data$date = as.Date(health_data$date, format='%d/%m/%y')
smoke_data$date = as.Date(smoke_data$date, format='%d/%m/%y')
density_data$date = as.Date(density_data$date, format='%d/%m/%y')

# functions for age line charts
trend_plot = function(param) {
  if (param=="Median Age"){
    plot_data <- age_data
    plot_data$age <- ordered(plot_data$age, levels=c("Old", "Mid", "Yng"))
    my_values <- c("Old"="#E4321D", "Mid"="#FFC300", "Yng"="#20BD1D")
    
  }
  else if (param=="Life Expectancy") {
    plot_data <- life_data
    plot_data$life <- ordered(plot_data$life, levels=c("H", "M", "L"))
    my_values <-c("H"="#010C41", "M"="#6E80DE", "L"="#87888A")
    
  }
  else if (param=="Health Expenditure") {
    plot_data <- health_data
    plot_data$health <- ordered(plot_data$health, levels=c("H", "M", "L"))
    my_values <-c("H"="#F10723", "M"="#E5B4BA", "L"="#6C6162")
    
  }
  else if (param=="Smoking Adults") {
    plot_data <- smoke_data
    plot_data$smoke <- ordered(plot_data$smoke, levels=c("H", "M", "L"))
    my_values <-c("H"="#2E1301", "M"="#C26B31", "L"="#817F7E")
  }
  else if (param=="Population Density") {
    plot_data <- density_data
    plot_data$density <- ordered(plot_data$density, levels=c("ED", "HD", "MD", "LD"))
    my_values <-c("ED"="#0D284B", "HD"="#3C669E", "MD"="#89A9D5", "LD"="#D4E1F3")
  }
  g1 = ggplot(plot_data, aes(x = date, y = avg, color = plot_data[,2])) + geom_line() + geom_point(size = 2, alpha = 0.8) +
    labs(x = "Date", y = "avg cases per mill", color=param) +
    scale_color_manual(values = my_values) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
          plot.title = element_text(size = 10, face = "bold", color = "red"),
          plot.margin = margin(5, 12, 5, 5))
    
  g1
}


#trend_plot("Life Expectancy")

# function for rate - age scatter plot
scatter_plot = function(param) {
  if (param=="Median Age"){
    plot_data <- country_data
    my_x <- "median_age"
    my_cat <- "age_cat"
    my_y <- "slope"
    x_lab <- "Median Age"
    y_lab <- "COVID19 Rate"
    my_labels <- c("Old", "Mid", "Yng")
    my_values <- c("#E4321D", "#FFC300", "#20BD1D")
  }
  else if (param=="Life Expectancy") {
    plot_data <- country_data
    my_x <- "life_exp"
    my_y <- "median_age"
    my_cat <- "life_cat"
    x_lab <- "Life Expectancy"
    y_lab <- "Median Age"
    my_labels=c("H", "M", "L")
    my_values <- c("#010C41", "#6E80DE", "#87888A")
  }
  else if (param=="Health Expenditure") {
    plot_data <- health_country
    my_x <- "health_expend_gdp"
    my_y <- "median_age"
    my_cat <- "health_cat"
    x_lab <- "Health expenditure as % of GDP"
    y_lab <- "Median Age"
    my_labels=c("H", "M", "L")
    my_values <- c("#F10723", "#E5B4BA", "#6C6162")
  }
  else if (param=="Smoking Adults") {
    plot_data <- country_data
    my_x <- "smoking_adults"
    my_y <- "median_age"
    my_cat <- "smoke_cat"
    x_lab <- "Smoking % of Adults"
    y_lab <- "Median Age"
    my_labels=c("H", "M", "L")
    my_values <- c("#2E1301", "#C26B31", "#817F7E")
  }
  else if (param=="Population Density") {
    plot_data <- out_country
    my_x <- "pop_dens_ppsqkm"
    my_y <- "median_age"
    my_cat <- "dens_cat"
    x_lab <- "Population density - ppsqkm"
    y_lab <- "Median Age"
    my_labels=c("ED", "HD", "MD", "LD")
    my_values <- c("#0D284B", "#3C669E", "#89A9D5", "#D4E1F3")
  }
  g1 = ggplot(plot_data, aes_string(x = my_x, y = my_y, color = my_cat)) + 
    geom_point(size = 2) +
    geom_smooth(method=lm, size = 1, colour = 'black', se=FALSE) + 
    labs(x = x_lab, y = y_lab, color=param) +
    scale_color_manual(labels = my_labels, values = my_values) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 10),
          plot.title = element_text(size = 10, face = "bold", color = "red"),
          plot.margin = margin(5, 12, 5, 5))
    
  g1
}

#scatter_plot("Health Expenditure")

# select large countries for mapping polygons
cv_large_countries = country_data %>% filter(country %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$country %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$country),]

# create plotting parameters for map
bins = c(0,1,5,10,50,100,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$country, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80)


# create the client side
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19 Rate Analysis", id="nav",
             
             tabPanel("COVID-19",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 650, fixed=TRUE,
                                        draggable = TRUE, height = 850,
                                        
                                        span(tags$i(h3("Spread of COVID-19 and the undeniable influence of age")), style="color:#641E16"),
                                        
                                        span(tags$i(h5("Demonstrating the relationship between the rate of increase of COVID-19 in countries and various factors may show how some factors had a stronger relationship than
                                                       others, However looking more carefully at these factors we can discover an underlying role that age plays in differentiating the strength of these relationships. 
                                                       To view the relationship of each factor with the Rate of the spread of disease and its relationship with age select the attribute from the dropdown, 
                                                       to view individual countries information hover over the map")), style="color:#0B0B0C"),
                                        
                                        pickerInput("att_select", "Attribute Selector:",   
                                                    choices = c("Median Age", "Life Expectancy", "Health Expenditure", "Smoking Adults", "Population Density"), 
                                                    selected = c("Median Age"),
                                                    multiple = FALSE),
                                        
                                        span(h5(textOutput("chart_text")), style="color:#0B0B0C"),
                                        
                                        plotOutput("trend_plot", height="200px", width="80%"),
                                        
                                        plotOutput("rate_age_plot", height="200px", width="80%"),
                          
                                        span(tags$i(h6("Rate in this study has been calculated as the slope of the best fit line to the cumulative number of cases 
                                                       per million population by Date in each country. COVID Data is taken from 9/1/20 to 12/4/20. 
                                                       For each attribute some countries may have been removed as outliers. It is important to note that the aim of this study
                                                       is to demonstrate the existance or non-existance of a relationship rather than testing hypothesis.")), style="color:#045a8d"),
                          ),
                          
                          
                      )
             )
             
             
  )
)
                      
                
# create the server side
server <- function(input, output) {
  
  # create a reactive element based on input selection
  selection <- reactive({input$att_select})
  
 
  # text based on input selection
  output$chart_text <- renderText({
    if (selection() == "Median Age"){
      paste("Looking at the graphs below there is a clear positive relationship between the median age of a country and the rate of the spread of the disease, 
            the first graph showing the average cumulative cases by date for countries grouped by their age category, which shows a clear distinction between the average number of cases
            in each age group as the older age groups have higher nu,ber of cases and. The second plot showing a scatter plot of the median age and the Rate of spread of disease with the trend line capturing the positive relationship, indicating 
            he higher the age category the higher the rate of the spread of the disease")
    }
    else if (selection() == "Life Expectancy"){
      paste("Looking at the first graph We observe an interesting trend that countries with higher life expectancy are showing a higher overall figure for average number of cases and there is a clear 
              seperation of each category. If we think that life expectancy has very similar trend to that of age with the spread of COVID-19, this is not far from the truth an older population indicates higher life expectancy and this 
              is captured with the scatter plot, showing the strong relationship between median age and life expectancy of countries.")
    }
    else if (selection() == "Health Expenditure"){
      paste("Categorising countries by their health expenditure seems to show a rather unexpected result at first. We observe from the first graphs that countries which have spent more of their share of GDP on health in recent years, 
            seem to show a higher number of cases as well as a higher slope of the trend line. This is also visible from the large red circles on the map. Again an older population may be perceived as the underlying reason for having a higher health expenditure.
            looking at the second graph demonstrates this relationship to some extent with a positive trend between median age and health expenditure, indicating age could be influencing this relationship as well")
    }
    else if (selection() == "Smoking Adults"){
      paste("Visualising the relationship the avergae number of cases grouped by countries smoking category shows some relationship, the slope of the line graphs differs slightly for high 
            and medium smoking countries compared to low smoking countries, the relationship of smoking with median age is not as clear as the ones for health expenditure and
            life expectancy and the second graph somewhat confirms this. Suggesting that age does not play as important role here as with the other factors and also COVID-19 spread relationship is not as
            clear as previous factors")
    }
    else if (selection() == "Population Density"){
      paste("Looking at the first graphs below we can't observe any clear separation between the average number of cases for categories of countries by population density 
            and also the rate of the growing number of cases doesn't differ which indicates the least significant relationship in the study. interesting enough the scatter plot also shows almost no relationship between density 
            and median age which further highlights the influence of age in this study and the factors relationships with the spread of COVID-19. indicating age could have very well played
            an underlying role in the relationship of these factors and the Rate of the disease")
    }
  }) 
  
  # plot the graphs in the panel
  output$trend_plot <- renderPlot({
    trend_plot(input$att_select)
  })
  
  output$rate_age_plot <- renderPlot({
    scatter_plot(input$att_select)
  })
  
  
  output$mymap <- renderLeaflet({ 
    if (selection() == "Median Age"){
      pal <- colorFactor(
            palette = c("#E4321D", "#FFC300", "#20BD1D"),
           domain = country_data$age_cat)
        
      leafletProxy("mymap", data = country_data) %>% 
        clearMarkers() %>%
        clearShapes() %>%
        addCircleMarkers(lat = ~ cen_lat, lng = ~ cen_long, weight = 1, radius = ~(slope)*(1/3), 
                         fillOpacity = 0.5, color = ~pal(age_cat),
                         label = sprintf("<strong>%s </strong><br/>Population: %d<br/>Rate of Spread: %.4g<br/>median age: %.3g", country_data$country, country_data$latest_total_pop, country_data$slope, country_data$median_age) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                           textsize = "15px", direction = "auto"
                         )) %>%
        addLegend(position = "bottomright", pal=pal, values = ~age_cat, title = "Median Age", opacity = 0.7, labFormat = labelFormat())
        
       }
       else if (selection() == "Life Expectancy"){
         pal <- colorFactor(
                palette = c("#010C41", "#6E80DE", "#87888A"),
                domain = country_data$life_cat)
         
         leafletProxy("mymap", data = country_data) %>% 
           clearMarkers() %>%
           clearShapes() %>%
           addCircleMarkers(lat = ~ cen_lat, lng = ~ cen_long, weight = 1, radius = ~(slope)*(1/3), 
                            fillOpacity = 0.5, color = ~pal(life_cat),
                            label = sprintf("<strong>%s </strong><br/>Population: %d<br/>Rate of Spread: %.4g<br/>Life Expectancy: %.3g", country_data$country, country_data$latest_total_pop, country_data$slope, country_data$life_exp) %>% lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                              textsize = "15px", direction = "auto"
                            )) %>%
           addLegend(position = "bottomright", pal=pal, values = ~life_cat, title = "Life Expectancy", opacity = 0.7)
         
       }
       else if (selection() == "Health Expenditure"){
         pal <- colorFactor(
           palette = c("#F10723", "#E5B4BA", "#6C6162"),
           domain = health_country$health_cat)
         
         leafletProxy("mymap", data = health_country) %>% 
           clearMarkers() %>%
           clearShapes() %>%
           addCircleMarkers(lat = ~ cen_lat, lng = ~ cen_long, weight = 1, radius = ~(slope)*(1/3), 
                            fillOpacity = 0.5, color = ~pal(health_cat),
                            label = sprintf("<strong>%s </strong><br/>Population: %d<br/>Rate of Spread: %.4g<br/>GDP percentage on health Expenditure: %.3g", health_country$country, health_country$latest_total_pop, health_country$slope, health_country$health_expend_gdp) %>% lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                              textsize = "15px", direction = "auto"
                            )) %>%
           addLegend(position = "bottomright", pal=pal, values = ~health_cat, title = "Health Expenditure", opacity = 0.7)
         
       }
      else if (selection() == "Smoking Adults"){
        pal <- colorFactor(
          palette = c("#2E1301", "#C26B31", "#817F7E"),
          domain = country_data$smoke_cat)
      
        leafletProxy("mymap", data = country_data) %>% 
          clearMarkers() %>%
          clearShapes() %>%
          addCircleMarkers(lat = ~ cen_lat, lng = ~ cen_long, weight = 1, radius = ~(slope)*(1/3), 
                         fillOpacity = 0.5, color = ~pal(smoke_cat),
                         label = sprintf("<strong>%s </strong><br/>Population: %d<br/>Rate of Spread: %.4g<br/>Smoking Percentage of Adults: %.3g", country_data$country, country_data$latest_total_pop, country_data$slope, country_data$smoking_adults) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                           textsize = "15px", direction = "auto"
                         )) %>%
          addLegend(position = "bottomright", pal=pal, values = ~smoke_cat, title = "Smoking Adults", opacity = 0.7)
      
      }
      else if (selection() == "Population Density"){
        pal <- colorFactor(
          palette = c("#0D284B", "#3C669E", "#89A9D5", "#D4E1F3"),
          domain = out_country$dens_cat)
      
        leafletProxy("mymap", data = out_country) %>% 
          clearMarkers() %>%
          clearShapes() %>%
          addCircleMarkers(lat = ~ cen_lat, lng = ~ cen_long, weight = 1, radius = ~(slope)*(1/3), 
                         fillOpacity = 0.5, color = ~pal(dens_cat),
                         label = sprintf("<strong>%s </strong><br/>Population: %d<br/>Rate of Spread: %.4g<br/>Population Density (ppsqkm): %.3g", out_country$country, out_country$latest_total_pop, out_country$slope, out_country$pop_dens_ppsqkm) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                           textsize = "15px", direction = "auto"
                         )) %>%
          addLegend(position = "bottomright", pal=pal, values = ~dens_cat, title = "Population Density", opacity = 0.7)
      }
      basemap
  })


  
}

shinyApp(ui = ui, server = server)     



