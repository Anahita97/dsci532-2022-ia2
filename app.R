
library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(dashHtmlComponents)
library(dplyr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      htmlLabel(" "),
      dccGraph(id = "plot_area"),
      dccSlider(
        id = "year_slider",
        min = 1942,
        max = 2021,
        marks = list(
          "1950" = "1950",
          "1970" = "1970",
          "1990" = "1990",
          "2000" = "2000",
          "2005" = "2005",
          "2010" = "2010",
          "2015" = "2015",
          "2020" = "2020"
        ),
        value = 2015
      )
    )
  )
)


app$callback(
  output("plot_area", "figure"),
  list(input("year_slider", "value")),
  function(year) {
    country_codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
    world_map <- read.csv("world_map_data.csv")
    
    summation <- world_map %>%
      filter(release_year <= {{year}}) %>%
      group_by(country) %>%
      summarize(total_count = sum(count))
    
    merged <- merge(summation, country_codes, by.x = "country", by.y = "COUNTRY")
    
    # map
    l <- list(color = toRGB("black"), width = 0.5)
    g <- list(
      scope = 'world',
      visible = F,
      showcountries = T,
      countrycolor = toRGB("lightgrey"),
      resolution = 50,
      showland = TRUE,
      landcolor = toRGB("lightgrey")
    )
    
    p <- plot_ly(merged,
                 type = "choropleth",
                 locations = ~CODE,
                 z = ~total_count,
                 colors = c("#ff8080", 
                            "#ff2e2e", 
                            "#ff0e0e", 
                            "#dc0000",
                            "#c30000",
                            "#ab0000", 
                            "#8a0000"),
                 marker = list(line = l)) %>%
      colorbar(title = "Total Count") %>%
      layout(clickmode = "event+select",
             title = "Number of Movies and TV shows Worldwide on Netflix",
             geo = g)
    p
  }
)

app$run_server(host = '0.0.0.0')