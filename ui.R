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
  skin = "blue",
  dashboardHeader(title = "桃園各式竊盜案件與派出所分析", titleWidth = 320),
  dashboardSidebar(
    width = 320,
    tags$head(
      tags$style(HTML("
        /* Fix the sidebar to the left */
        .main-sidebar {
          position: fixed;   /* Fixed Sidebar (stay in place on scroll) */
          height: 100%;      /* Full height */
          overflow-y: auto;  /* Disable horizontal scroll */
          width: 320px;      /* Sidebar width */
        }
        /* Adjust body padding when sidebar is fixed */
        .content-wrapper {
          margin-left: 320px; /* Same as sidebar width */
        }
        /* 修改侧边栏中所有文本的字体大小 */
        .sidebar .sidebar-menu {
          font-size: 16px;  
        }
        /* 特别针对菜单项的字体大小 */
        .sidebar-menu li a {
          font-size: 20px;  
        }
      "))
    ),
    sidebarMenu(
      controlPanel(),
      textInput("stationName", "輸入派出所名稱", placeholder = "ex:中壢派出所"),
      actionButton("go", "定位派出所"),
      menuItem("地圖與距離分析", tabName = "main"),
      menuItem("各派出所犯罪事件數量比較", tabName = "station_compare_tab"),
      menuItem("分析", tabName = "analyze")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                column(width = 12,
                       box(
                         title = "竊盜案vs.警察局地圖 (使用左邊控制面板操作)", 
                         status = "info",
                         solidHeader = TRUE,
                         width = 12,
                         leafletOutput("mapPlot", height = "400px")
                       )
                ),
                column(width = 12, 
                       box(
                         title = "案發位置與最近警局散點圖 (Combine thief.csv & police.csv)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("circle_dist", height = "300px")
                       )
                )
              )
      ),
      tabItem(tabName = "station_compare_tab",
              fluidRow(
                column(width = 12,
                       box(
                         title = "各派出所年度及類型竊盜案數量 (使用左邊控制面板操作)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("station_compare", height = "450px")
                       )
                ),
                column(width = 7,
                       box(
                         title = "各區所收到的案件數量",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("pieChart", height = "500px")
                       )
                ),
                column(width = 5,
                       box(
                         title = "111及112住宅竊盜缺少派出所信息 (若選擇此條件將顯示警告)",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         DTOutput("na_stations")
                       )
                )
              )
      ),
      tabItem(tabName = "analyze",
              fluidRow(
                column(width = 6,
                       box(
                         title = "犯案地點與警局之距離分佈 (需調整年份, remove outliers)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("boxPlot", height = "450px")
                       )
                ),
                column(width = 6,
                       box(
                         title = "犯罪事件的3D地理分布 (remove outliers)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("d3", height = "450px")
                       )
                )
              ),
              fluidRow(
                column(width = 5,
                       box(
                         title = "各類型竊盜案件按年份的趨勢",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("theftTrends", height = "450px")
                       )
                ),
                column(width = 7,
                       box(
                         title = "犯罪事件隨時間的變化",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         plotlyOutput("timeSeriesPlot", height = "450px")
                       )
                )
              )
        )
      )
  )
)