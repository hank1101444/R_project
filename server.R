source("global.R")

shinyServer(function(input, output, session) { # add session
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
      iconUrl = "thief.png",
      iconWidth = 45, iconHeight = 45
    )
    police_icon <- icons(
      iconUrl = "police.png",
      iconWidth = 45, iconHeight = 45
    )
    data = datasetInput()
    #print(nrow(data))
    m = leaflet()
    m = addTiles(m) 
    # thief
    m = addMarkers(m, 
                    lng = data$lon, 
                    lat = data$lat,
                    popup = paste("犯案時間：", data$year, "-", data$month, "-", data$date, "<br>",
                                  "類別：", data$type),
                    icon = theft_icon)
    
    # police station
    m = addMarkers(m, 
                    lng = stations$lon, 
                    lat = stations$lat,
                    popup = paste("名稱：", stations$name, "<br>",
                                  "地址：", stations$address, "<br>",
                                  "電話：", stations$telephone),
                    icon = police_icon,
                   layerId = stations$name # help to find station
                   )
    m
    })
  
  # looking for button 
  observeEvent(input$go, {
    req(input$stationName)
    #print(input$stationName) #debug
    #print(stations$name[1]) 
    station = stations[stations$name == input$stationName, ]
    if (nrow(station) > 0) {
      leafletProxy("mapPlot", session) %>% # return the proxy
        setView(lng = station$lon, lat = station$lat, zoom = 15)
        #openPopup(layerId = station$name)
    } else {
      showNotification("未找到該派出所", type = "error")
    }
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
    data <- datasetInput()
    
    
    crime_counts <- data %>%
      group_by(station) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    #print(crime_counts)
    # na_rows <- thief[rowSums(is.na(thief)) > 0, ]
    # print(na_rows)
    crime_counts = na.omit(crime_counts)
    if(nrow(crime_counts) == 0) {
      text_to_display <- "This year all stations counts are NA"
    } else {
      text_to_display <- ""
    }
    
    crime_counts$station <- factor(crime_counts$station, levels = crime_counts$station)
    
    fig <- plot_ly(crime_counts, x = ~station, y = ~count, type = 'bar',
                   marker = list(color = ~count, colorscale = 'Rainbow'),
                   text = ~paste("犯罪數量: ", count),
                   hoverinfo = 'text+x')
    
    current_type <- if(input$thief_type == "All") "所有類型" else input$thief_type
    title_text <- sprintf("派出所犯罪事件數量: %s (%s 年)", current_type, input$thief_year)
    
    fig <- fig %>% layout(
      title = title_text,
      annotations = list(
        x = 0.5,
        y = 0.5,
        xref = 'paper',
        yref = 'paper',
        text = text_to_display,
        showarrow = FALSE,
        font = list(
          family = 'Courier New, monospace',
          size = 16,
          color = '#ff0000'
        ),
        align = 'center'
      ),
      margin = list(t = 70),
      xaxis = list(title = '派出所'),
      yaxis = list(title = '犯罪数量')
    )
    
    return(fig)
  })
  
  output$na_stations <- renderDataTable({
    na_data = filter(thief, type == "住宅竊盜" & is.na(station))[,1:7]
    # na_rows <- thief[rowSums(is.na(thief)) > 0, ]
    # print(na_rows)
    na_data
  })
  
  output$theftTrends <- renderPlotly({
    data <- thief %>%
      filter(type %in% unique(thief$type)) %>%
      group_by(type, year) %>%
      summarise(count = n())  
    
    p <- plot_ly(data, x = ~year, y = ~count, type = 'scatter', mode = 'lines+markers', color = ~type,
                 hoverinfo = 'text', text = ~paste(type, "年份:", year, "<br>案件數:", count))
    
    p <- p %>% layout(#title = "各類型竊盜案件按年份的趨勢",
                      #margin = list(t = 70),
                      xaxis = list(title = "年份"),
                      yaxis = list(title = "案件數量"),
                      hovermode = "closest")
    
    p
  })
  
  remove_outliers <- function(x){
    qnt = quantile(x, probs=c(.25, .75))
    H= 1.5 * IQR(x)
    y=x
    y[x < (qnt[1] - H)] = NA
    y[x > (qnt[2] + H)] = NA
    y
  }
  
  output$boxPlot <- renderPlotly({
    data = filter(thief, thief$year == input$thief_year)
    filtered_data = data[data$year == input$thief_year, ]
    x = filtered_data$distance_to_nearest_station
    y = remove_outliers(x)
    filtered_data$distance_to_nearest_station = y
    
    p = plot_ly()
    crime_types = unique(filtered_data$type)
    for (crime_type in crime_types) {
      p = add_trace(p,
                     data = filtered_data[filtered_data$type == crime_type, ],
                     y = ~distance_to_nearest_station,
                     name = crime_type,
                     type = 'box')
    }
    p = layout(p,
               margin = list(t = 70),
                yaxis = list(title = '距離（m）'),
                xaxis = list(title = '犯罪類型'),
                title = paste('年份:', input$thief_year, '的犯罪類型距離最近警局的距離分佈'))
    p
  })
  
  
  output$d3 <- renderPlotly({
    data = datasetInput()
    x = data$distance_to_nearest_station
    y = remove_outliers(x)
    data$distance_to_nearest_station = y
    plot_ly(data, x = ~lon, y = ~lat, z = ~distance_to_nearest_station, type = 'scatter3d', mode = 'markers',
            color = ~type, 
            marker = list(size = 5, color = ~type, colorscale = 'Viridis', showscale = TRUE), 
            hoverinfo = 'text', 
            text = ~paste("類型：", type, 
                          "<br>經度：", lon, 
                          "<br>緯度：", lat, 
                          "<br>至最近警局距離：", distance_to_nearest_station, "米")) %>%
      layout(
             title = sprintf("%s (%s 年)", input$thief_type, input$thief_year),
             titlefont = list(size = 20),
             margin = list(t = 70),
             scene = list(xaxis = list(title = '經度'),
                          yaxis = list(title = '緯度'),
                          zaxis = list(title = '至最近警局距離（米）')))
    
  })
  
  output$timeSeriesPlot <- renderPlotly({
    data <- thief
    data$date <- as.Date(with(data, paste(year, month, date, sep = "-")), "%Y-%m-%d")
    data$year_month <- with(data, paste(year, sprintf("%02d", month), sep = "-"))  
    
    # groupby type & year & month
    data_grouped <- data %>%
      group_by(type, year_month) %>%
      summarise(count = n(), .groups = 'drop')

    plot_ly(data_grouped, x = ~year_month, y = ~count, type = 'scatter', mode = 'lines+markers',
            color = ~type, colors = RColorBrewer::brewer.pal(8, "Dark2"),
            hoverinfo = 'text', text = ~paste("Type:", type, "<br>Count:", count, "<br>Month:", year_month)) %>%
      layout(#title = "犯罪事件隨時間的變化",
             xaxis = list(title = "年-月"),
             yaxis = list(title = "案件數量"),
             hovermode = "closest")
  })
  
  output$pieChart <- renderPlotly({
      data = datasetInput()
      summary_data <- data %>%
        group_by(breau) %>%
        summarise(count = n(), .groups = 'drop') %>%
        arrange(desc(count))
      
      fig = plot_ly(summary_data, labels = ~breau, values = ~count, type = 'pie',
                     textinfo = 'label+percent', insidetextorientation = 'radial') %>%
        layout(margin = list(t = 80), title = paste("", input$thief_type, input$thief_year))
      fig
    })
})
