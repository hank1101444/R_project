source("global.R")

shinyServer(function(input, output) {
  # 地圖繪製邏輯
  # Return the requested dataset
  datasetInput <- reactive({
    selected_type = input$thief_type
    selected_year = input$thief_year
    x=NULL
    if (selected_type == "All") {
      x = filter(thief, thief$year == input$thief_year)
    } else {
      x = filter(thief, thief$type == input$thief_type & thief$year == input$thief_year)
    }
    x
  })
  
  output$mapPlot <- renderLeaflet({
    theft_icon <- icons(
      iconUrl = "images.png",
      iconWidth = 30, iconHeight = 30
    )
    police_icon <- icons(
      iconUrl = "police.png",
      iconWidth = 30, iconHeight = 30
    )
    data = datasetInput()
    #print(nrow(data))
    m = leaflet()
    m = addTiles(m) 
    # thief
    m = addMarkers(m, lng = data$lon, lat = data$lat,
                   popup = as.character(data$lat), icon=theft_icon)
    # police station
    m = addMarkers(m, lng = stations$lon, lat = stations$lat,
                   popup = as.character(stations$lat), icon=police_icon)
    m
    })
  
  output$distancePlot <- renderPlotly({
    data = datasetInput()
    #print(nrow(data))
    
    p = plot_ly(data, x = ~distance_to_nearest_station, type = 'histogram')
    p
  })
  output$circle_dist <- renderPlotly({
    data = datasetInput()
    #print(nrow(data))
    
    fig = plot_ly(data = data, x = ~lon, y = ~lat, text = ~paste("距離: ", distance_to_nearest_station, "公尺"),
                   marker = list(size = ~distance_to_nearest_station/100, color = ~distance_to_nearest_station, colorscale = 'Blues', showscale = TRUE),
                   type = 'scatter', mode = 'markers')
    
    fig = fig %>% layout(
          xaxis = list(title = '經度'),
          yaxis = list(title = '緯度'))
    fig
  })
  
  output$station_compare <- renderPlotly({
    data = datasetInput()
    crime_counts = data %>%
      group_by(station) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    #print(crime_counts)
    # use crime_counts levels
    crime_counts$station = factor(crime_counts$station, levels = crime_counts$station)
    # bar
    fig = plot_ly(crime_counts, x = ~station, y = ~count, type = 'bar',
                   marker = list(color = ~count, colorscale = 'Rainbow'),
                   text = ~paste("犯罪數量: ", count),
                   hoverinfo = 'text+x')
    
    current_type = NULL
    if(input$thief_type == "All"){
      current_type = "所有類型"
    }else{
      current_type = input$thief_type
    }
    
    title_text = sprintf("派出所犯罪事件數量: %s (%s 年)", current_type, input$thief_year)
    fig = fig %>% layout(title = title_text,
                          margin = list(t = 70),
                          xaxis = list(title = '派出所'),
                          yaxis = list(title = '犯罪數量'))
    fig
  })
  output$theftTrends <- renderPlotly({
    data <- thief %>%
      filter(type %in% unique(thief$type)) %>%
      group_by(type, year) %>%
      summarise(count = n())  
    
    p <- plot_ly(data, x = ~year, y = ~count, type = 'scatter', mode = 'lines+markers', color = ~type,
                 hoverinfo = 'text', text = ~paste(type, "年份:", year, "<br>案件數:", count))
    
    p <- p %>% layout(title = "各類型竊盜案件按年份的趨勢",
                      margin = list(t = 70),
                      xaxis = list(title = "年份"),
                      yaxis = list(title = "案件數量"),
                      hovermode = "closest")
    
    p
  })
})
