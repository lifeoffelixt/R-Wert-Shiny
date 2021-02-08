library(shiny)
library(shinydashboard)
require(plotly)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Verschiedene Berechnung des R-Werts in der COVID-19 Pandemie", titleWidth = 600),
    
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Deutschland", tabName = "germany", icon=icon("head-side-mask")),
        menuItem(text = "Bayern", tabName = "bavaria", icon=icon("head-side-mask")),
        menuItem(text = "München", tabName = "muc", icon=icon("head-side-mask"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem("germany",
                plotly::plotlyOutput("ger_plot"),
                selectInput("method_ger",
                            "Choose Method:",
                            selected = "RKI R 4 Tage",
                            multiple = T,
                            choices = list("RKI R 4 Tage",
                                           "RKI R 7 Tage",
                                           "FZ Jülich",
                                           "Wallinga Teunis",
                                           "Wallinga Lipsitch"))
                
        ),
        tabItem("bavaria",
                plotly::plotlyOutput("bay_plot"),
                selectInput("method_bay",
                            "Choose Method:",
                            selected = "RKI R 4 Tage",
                            multiple = T,
                            choices = list("RKI R 4 Tage",
                                           "RKI R 7 Tage",
                                           "FZ Jülich",
                                           "Wallinga Teunis",
                                           "Wallinga Lipsitch",
                                           "StabLab"))
        ),
        tabItem("muc",
                plotly::plotlyOutput("muc_plot"),
                selectInput("method_muc",
                            "Choose Method:",
                            selected = "RKI R 4 Tage",
                            multiple = T,
                            choices = list("RKI R 4 Tage",
                                           "RKI R 7 Tage",
                                           "Wallinga Teunis",
                                           "Wallinga Lipsitch",
                                           "StabLab"))
        )
      )
    )
  )
)
