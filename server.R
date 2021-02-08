require(shiny)
require(ggplot2)
require(plotly)
require(data.table)
server <- function(input, output, session) {
  
  deutschland_select <- function(df, listv){
    df2 <- df %>%
      filter(variable %in% listv)
    
    months <- c("15. April 2020", "15. Mai 2020", "15. Juni 2020", "15. Juli 2020", "15. August 2020",
                "15. September 2020", "15. Oktober 2020", "15. November 2020", "15. Dezember 2020", "15. Januar 2021")
    
    colors <- c("RKI R 4 Tage" = "#56B4E9",
                "RKI R 7 Tage" = "#0072B2",
                "StabLab" = "#F0E442",
                "Wallinga Lipsitch" = "#CC79A7",
                "Wallinga Teunis" = "#009E73")
    
    deutschland_nowcast <- readRDS("data/deutschland_nowcast.rds")
    
    Neuerkrankungen <- ggplot(data = deutschland_nowcast, aes(x = Datum)) + 
      geom_line(aes(y = NeuErkr), stat = "identity", size = 1) +
      scale_y_continuous(expand = c(0,0), limits  = c(0, 30000)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      labs(colour = "") +
      xlab("") +
      ylab("Neuerkrankungen") +
      theme_bw(base_size = 12) 
    
    R <- ggplot(data = df2, aes(x = Datum, y=value )) +
      geom_line(aes(color = variable), stat = "identity", size = 1) + # RKI 4 Tage
      geom_hline(yintercept=1, linetype="dashed", color = "black") +
      coord_cartesian(ylim = c(0, 2.5)) +
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(0,2.5, 0.25)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      scale_colour_manual(values = colors) +
      labs(colour = "") +
      xlab("") +
      ylab("R") +
      theme_bw(base_size = 12)
    
    
    subplot(Neuerkrankungen, R,
            nrows = 2,
            shareX = TRUE)
  }
  
  bay_select <- function(df, listv){
    df2 <- df %>%
      filter(variable %in% listv)
    
    months <- c("15. April 2020", "15. Mai 2020", "15. Juni 2020", "15. Juli 2020", "15. August 2020",
                "15. September 2020", "15. Oktober 2020", "15. November 2020", "15. Dezember 2020", "15. Januar 2021")
    
    colors <- c("RKI R 4 Tage" = "#56B4E9",
                "RKI R 7 Tage" = "#0072B2",
                "FZ Jülich" = "#D55E00",
                "Wallinga Lipsitch" = "#CC79A7",
                "Wallinga Teunis" = "#009E73",
                "StabLab" = "#F0E442")

    bayern <- readRDS("data/bayern_nowcast.rds")
    
    Neuerkrankungen <- ggplot(data = bayern, aes(x = Datum)) + 
      geom_line(aes(y = nowcast), stat = "identity", size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      labs(colour = "") +
      xlab("") +
      ylab("Neuerkrankungen") +
      theme_bw(base_size = 12) 
    
    R <- ggplot(data = df2, aes(x = Datum, y=value )) +
      geom_line(aes(color = variable), stat = "identity", size = 1) +
      geom_hline(yintercept=1, linetype="dashed", color = "black") +
      coord_cartesian(ylim = c(0, 2.5)) +
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(0,2.5, 0.25)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      scale_colour_manual(values = colors) +
      labs(colour = "") +
      xlab("") +
      ylab("R") +
      theme_bw(base_size = 12)
    
    
    subplot(Neuerkrankungen, R,
            nrows = 2,
            shareX = TRUE)
  }
  
  muc_select <- function(df, listv){
    df2 <- df %>%
      filter(variable %in% listv)
    
    months <- c("15. April 2020", "15. Mai 2020", "15. Juni 2020", "15. Juli 2020", "15. August 2020",
                "15. September 2020", "15. Oktober 2020", "15. November 2020", "15. Dezember 2020", "15. Januar 2021")
    
    colors <- c("RKI R 4 Tage" = "#56B4E9",
                "RKI R 7 Tage" = "#0072B2",
                "FZ Jülich" = "#D55E00",
                "Wallinga Lipsitch" = "#CC79A7",
                "Wallinga Teunis" = "#009E73",
                "StabLab" = "#F0E442")
    
    muc <- readRDS("data/muc_nowcast.rds")
    
    Neuerkrankungen <- ggplot(data = muc, aes(x = Datum)) + 
      geom_line(aes(y = nowcast), stat = "identity", size = 1) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      labs(colour = "") +
      xlab("") +
      ylab("Neuerkrankungen") +
      theme_bw(base_size = 12) 
    
    R <- ggplot(data = df2, aes(x = Datum, y=value )) +
      geom_line(aes(color = variable), stat = "identity", size = 1) +
      geom_hline(yintercept=1, linetype="dashed", color = "black") +
      coord_cartesian(ylim = c(0, 2.5)) +
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(0,2.5, 0.25)) +
      scale_x_date(expand = c(0,0),
                   breaks = seq(as.Date("2020-04-15"),by = "month", length.out = 10),
                   labels = months) +
      scale_colour_manual(values = colors) +
      labs(colour = "") +
      xlab("") +
      ylab("R") +
      theme_bw(base_size = 12)
    
    
    subplot(Neuerkrankungen, R,
            nrows = 2,
            shareX = TRUE)
  }
  
  output$ger_plot <- renderPlotly({
    deutschland_data <- readRDS("data/deutschland_data.rds")
    req(between(length(input$method_ger), 1, 5))
    Vergleichsplot <- deutschland_select(deutschland_data, input$method_ger)
    print(ggplotly(Vergleichsplot))
  })
  
  output$bay_plot <- renderPlotly({
    bayern_data <- readRDS("data/bayern_data.rds")
    req(between(length(input$method_bay), 1, 6))
    Vergleichsplot <- deutschland_select(bayern_data, input$method_bay)
    print(ggplotly(Vergleichsplot))
  })
  
  output$muc_plot <- renderPlotly({
    muc_data <- readRDS("data/muc_data.rds")
    req(between(length(input$method_muc), 1, 5))
    Vergleichsplot <- deutschland_select(muc_data, input$method_muc)
    print(ggplotly(Vergleichsplot))
  })
}