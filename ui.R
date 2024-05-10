source("global.R")
controlPanel <- function() {
  thief_types = unique(thief$type)  
  thief_years = thief$year
  inputList = list(
    selectInput("thief_type", "選擇竊盜類型", 
                choices = c("All" = "All", thief_types),
                selected = "機車竊盜"),
    sliderInput("thief_year", "選擇年份",
                min = min(thief_years), 
                max = max(thief_years),
                value = max(thief_years),
                step = 1)
  )
  return(inputList)
}

ui <- dashboardPage(
  dashboardHeader(title = "桃園各式竊盜案件與派出所分析", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("地圖與數據分析", tabName = "main"),
      menuItem("比較派出所", tabName = "station_compare_tab"),
      controlPanel()
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                column(width = 12,
                       box(
                         title = "地圖顯示", 
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         leafletOutput("mapPlot", height = "300px")
                       )
                ),
                column(width = 12, 
                       box(
                         title = "距離分析(combine thief.csv & thief.csv)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("circle_dist", height = "275px")
                       )
                )
              )
      ),
      tabItem(tabName = "station_compare_tab",
              fluidRow(
                column(width = 12,
                       box(
                         title = "各派出所犯罪事件數量比較",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("station_compare", height = "450px")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "各類型竊盜案件按年份的趨勢",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("theftTrends", height = "450px")
                       )
                )
              )
              
      )
    )
  )
)
