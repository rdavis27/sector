library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Sector Analysis"),

    tabsetPanel(
        type = "tabs",
        tabPanel(
            "Sectors",
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    textInput("symbols", "Symbols", value = "SPY"),
                    selectInput("sector", "Sector",
                                c("Use above symbols","Indices","Equity ETFs",
                                  "Sectors1","Sectors2","Tech1 (FANG)","Tech2"),
                                selected = "Indices"),
                    selectInput("graph", "Graph",
                                c("adjusted price","cumulative % change","percent change","% change vs Symbol"),
                                selected = "cumulative % change"),
                    #checkboxInput("ylogscale", "Y Log Scale", TRUE),
                    #splitLayout(
                        selectInput("yscale", "Y Scale",
                                    c("linear","log2","log2 from","log10","log10 from"),
                                    selected = "linear"),
                        numericInput("ymin", "Y Min", 0),
                    #),
                    dateRangeInput('dateRange2',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = '2010-01-01', end = Sys.Date()),
                    radioButtons("span2", "Timespan:", inline = TRUE,
                                 c("Use above dates","1M","3M","6M","YTD","1Y","2Y","5Y","10Y","MAX"),
                                 selected = "3M"),
                    radioButtons("freq2", "Frequency:", inline = TRUE,
                                 c("daily","weekly","monthly","yearly"), selected = "daily"),
                    numericInput("sma", "SMA", 1),
                    textInput("datefmt", "Date Format", value = "#%y-%m-%d"),
                    checkboxInput("cache", "Cache Data", TRUE),
                    checkboxInput("linetype", "Linetype", FALSE)
                ),
                mainPanel(
                    width = 10,
                    plotOutput("distPlot2"),
                    verbatimTextOutput("myText2")
                )
            )
        ),
        tabPanel(
            "Chart",
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    textInput("symbol", "Symbol", value = "SPY"),
                    selectInput("type", "Type", c("auto","candlesticks","matchsticks","bars","line"), selected = "auto"),
                    selectInput("theme", "Theme", c("white","black"), selected = "white"),
                    fluidRow(
                        column(4, style='padding:1px', selectInput("ilab1", "Overlay 1", c("none","EMA","SMA"), selected = "none", width = 80)),
                        column(4, style='padding:1px', numericInput("ival1", "value", 200, width = 80)),
                        column(4, style='padding:1px', selectInput("icol1", "color", c("blue","red","green","purple","orange"), selected = "blue", width = 80))
                    ),
                    fluidRow(
                        column(4, style='padding:1px', selectInput("ilab2", "Overlay 2", c("none","EMA","SMA"), selected = "none", width = 80)),
                        column(4, style='padding:1px', numericInput("ival2", "value", 50, width = 80)),
                        column(4, style='padding:1px', selectInput("icol2", "color", c("blue","red","green","purple","orange"), selected = "red", width = 80))
                    ),
                    checkboxInput("volume", "Volume", TRUE),
                    checkboxInput("bollinger", "Bollinger Bands", FALSE),
                    checkboxInput("multicol", "4-colored Candles", FALSE),
                    dateRangeInput('dateRange',
                                   label = 'Date range input: yyyy-mm-dd',
                                   start = '2010-01-01', end = Sys.Date()),
                    radioButtons("span", "Timespan:", inline = TRUE,
                                 c("Use above dates","1M","3M","6M","YTD","1Y","2Y","5Y","10Y","MAX"), selected = "3M"),
                    numericInput("lag", "Lag", 1)
                ),
                mainPanel(
                    width = 10,
                    plotOutput("distPlot"),
                    verbatimTextOutput("myText")
                )
            )
        )
    )
))
