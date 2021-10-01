library(shiny)
library(tidyverse)
library(lubridate)
#library(quantmod)
#library(PerformanceAnalytics)
library(ggplot2)
#library(reshape2)
library(BatchGetSymbols)
library(quantmod)

#library(iClick)
#library(plyr)
#library(scales)

shinyServer(function(input, output) {
    getData <- reactive({
        if (input$span == "1M"){
            strDate = paste0(Sys.Date() %m-% months(1),"::")
        }
        else if (input$span == "3M"){
            strDate = paste0(Sys.Date() %m-% months(3),"::")
        }
        else if (input$span == "6M"){
            strDate = paste0(Sys.Date() %m-% months(6),"::")
        }
        else if (input$span == "YTD"){
            startyr = today()
            month(startyr) = 1
            day(startyr) = 1
            strDate = paste0(as.character(startyr),"::")
        }
        else if (input$span == "1Y"){
            strDate = paste0(Sys.Date() - years(1),"::")
        }
        else if (input$span == "2Y"){
            strDate = paste0(Sys.Date() - years(2),"::")
        }
        else if (input$span == "5Y"){
            strDate = paste0(Sys.Date() - years(5),"::")
        }
        else if (input$span == "10Y"){
            strDate = paste0(Sys.Date() - years(10),"::")
        }
        else if (input$span == "MAX"){
            strDate = "::"
        }
        else{
            strDate = paste0(as.character(input$dateRange[1]),"::",as.character(input$dateRange[2]))
        }
        strDate <<- strDate
        
        gdata <<- getSymbols(input$symbol, src = 'yahoo', from = '1900-01-01', auto.assign = FALSE)
        #gdata <<- get(input$symbol) # required if auto.assign = TRUE
    })
    output$distPlot <- renderPlot({
        getData()
        chartSeries(gdata, name = input$symbol, type = input$type, subset = strDate, theme = input$theme, multi.col = input$multicol)
        tas = NULL
        if (input$volume == TRUE){
            tas = c(tas, addVo())
        }
        if (input$bollinger == TRUE){
            tas = c(tas, addBBands())
        }
        if (input$ilab1 == "EMA"){
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addEMA(n = num1, col = col1))
            }
        }
        else if (input$ilab1 == "SMA"){
            if (input$ival1 > 0){
                num1 <<- input$ival1
                col1 <<- input$icol1
                tas = c(tas, addSMA(n = num1, col = col1))
            }
        }
        if (input$ilab2 == "EMA"){
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addEMA(n = num2, col = col2))
            }
        }
        else if (input$ilab2 == "SMA"){
            if (input$ival2 > 0){
                num2 <<- input$ival2
                col2 <<- input$icol2
                tas = c(tas, addSMA(n = num2, col = col2))
            }
        }
        #print(paste0("chartSeries(", input$symbol, ", ", strDate, ")"))
        cat(file = stderr(), paste0("chartSeries(", input$symbol, ", ", strDate, ")\n"))
        if (is.null(tas)){
            chartSeries(gdata, name = input$symbol, type = input$type, TA = NULL, subset = strDate, theme = input$theme, multi.col = input$multicol)
        }
        else{
            chartSeries(gdata, name = input$symbol, type = input$type, TA = tas, subset = strDate, theme = input$theme, multi.col = input$multicol)
        }
        #chartSeries(get(input$symbol), name = input$symbol, subset = '2010-01-01::2016-12-02', theme = "white")
    })
    output$myText <- renderPrint({
        cat(paste0(input$symbol,"\n"))
        #print(get("SPY"))
        sdata <- gdata[strDate] 
        print(head(sdata))
        print(tail(sdata))
    })
    # Chart2 functions ########################################################
    getData2 <- reactive({
        last.date <- Sys.Date()
        if (input$span2 == "1M"){
            strDate1 <- Sys.Date() %m-% months(1)
        }
        else if (input$span2 == "3M"){
            strDate1 <- Sys.Date() %m-% months(3)
        }
        else if (input$span2 == "6M"){
            strDate1 <- Sys.Date() %m-% months(6)
        }
        else if (input$span2 == "YTD"){
            startyr <- today()
            month(startyr) <- 1
            day(startyr) <- 1
            strDate1 <- as.character(startyr)
        }
        else if (input$span2 == "1Y"){
            strDate1 <- Sys.Date() - years(1)
        }
        else if (input$span2 == "2Y"){
            strDate1 <- Sys.Date() - years(2)
        }
        else if (input$span2 == "5Y"){
            strDate1 <- Sys.Date() - years(5)
        }
        else if (input$span2 == "10Y"){
            strDate1 <- Sys.Date() - years(10)
        }
        else if (input$span2 == "MAX"){
            strDate1 <- as.Date("1900-01-01")
        }
        else{
            strDate1  <- as.Date(input$dateRange2[1])
            last.date <- as.Date(input$dateRange2[2])
        }
        strDate1 <<- strDate1
        
        ##gdata <<- getSymbols(input$symbol2, src = 'yahoo', from = '1900-01-01', auto.assign = FALSE)
        #gdata <<- get(input$symbol2) # required if auto.assign = TRUE
        # set dates
        first.date <- strDate1 # was Sys.Date() - 60
        last.date <- Sys.Date()
        freq.data <- input$freq2
        gdate1 <<- first.date
        gdate2 <<- last.date
        # set tickers
        if (input$sector == "Indices"){
            tickers <- c('^DJI','EEM','EFA','^IXIC','^GSPC')
            tnames <- c('Dow','Emerging','Foreign','Nasdaq','S&P 500')
        } else if (input$sector == "Equity ETFs"){
            tickers <- c('VWO','VEA','VSS','VTI','VO','VB')
            tnames <- c('Emerg','Int_All','Int_Sm','US_All','US_Mid','US_Sm')
        } else if (input$sector == "Sectors1"){
            tickers <- c('XLC','XLF','XLV','XLRE','XLK','XLU')
            tnames <-  c('Communication','Financials','Health_Care','Real_Estate','Technology','Utilities')
        } else if (input$sector == "Sectors2"){
            tickers <- c('XLY','XLE','XLI','XLB','XLP')
            tnames <-  c('Discretionary','Energy','Industrials','Materials','Staples')
        } else if (input$sector == "Tech1 (FANG)"){
            tickers <- c('AAPL','AMZN','FB','GOOG','NFLX')
            tnames  <- c('Apple','Amazon','Facebook','Google','Netflix')
        } else if (input$sector == "Tech2"){
            tickers <- c('AAPL','AMD','NVDA','MSFT','QCOM','TSM')
            tnames  <- c('AAPL','AMD','NVDA','MSFT','QCOM','TSM')
        } else {
            tickers <- unlist(strsplit(input$symbols,","))
            tnames  <- tickers
        }
        gtickers <<- tickers
        l.out <- BatchGetSymbols(tickers = tickers, 
                                 first.date = first.date,
                                 last.date = last.date,
                                 thresh.bad.data = 0.0,
                                 freq.data = freq.data,
                                 do.complete.data = TRUE,
                                 do.fill.missing.prices = FALSE,
                                 do.cache = input$cache,
                                 cache.folder = file.path(tempdir(), 
                                 'BGS_Cache') ) # cache in tempdir()
        #print(l.out) #DEBUG
        #lll <<- l.out #DEBUG
        gdata <- as.data.frame(l.out$df.tickers)
        gdata$tname <- gdata$ticker
        for (i in 1:length(tickers)){
            gdata$tname[gdata$ticker == tickers[i]] <- tnames[i]
        }
        gdata$ticker <- gdata$tname
        #colnames(gdata) <- c("price.open","price.high","price.low","price.close",
        #                     "volume","price.adjusted","ref.date","ticker",
        #                     "ret.adjusted.prices","ret.closing.prices")
        gdata <<- gdata
        dd <- gdata[,c('ref.date','ticker','price.adjusted')]
        ee <- spread(dd,ticker,price.adjusted)
        ff <- ee
        if (input$graph == "adjusted price"){
            ff[,i] <- ee[,i]
        }
        else if (input$graph == "percent change"){
            for (i in 2:NCOL(ee)){
                ff[,i] <- 100 * ee[,i] / lag(ee[,i]) - 100
            }
        }
        else if (input$graph == "% change vs Symbol"){
            tickers0 <- unlist(strsplit(input$symbols,","))
            base <- BatchGetSymbols(tickers = tickers0[1],
                                    first.date = first.date,
                                    last.date = last.date,
                                    thresh.bad.data = 0.0,
                                    freq.data = freq.data,
                                    do.complete.data = TRUE,
                                    do.fill.missing.prices = FALSE,
                                    do.cache = FALSE,
                                    cache.folder = file.path(tempdir(),'BGS_Cache'))
            bdata <- as.data.frame(base$df.tickers)
            dd2 <- bdata[,c('ref.date','ticker','price.adjusted')]
            ee2 <- spread(dd2,ticker,price.adjusted)
            for (i in 2:NCOL(ee)){
                ff[,i] <- 100 * ee[,i] / lag(ee[,i]) - 100
            }
            bb <- 100 * ee2[,2] / lag(ee2[,2]) - 100
            bbbb <<- bb #DEBUG
            for (i in 2:NCOL(ee)){
                ff[,i] <- ff[,i] - bb
            }
        }
        else{
            for (i in 2:NCOL(ee)){
                ff[,i] <- 100 * ee[,i] / ee[1,i] - 100
            }
        }
        if (input$sma > 1){
            fltr <- rep(1/input$sma, input$sma)
            for (i in 2:NCOL(ff)){
                ff[,i] <- stats::filter(ff[,i], fltr, sides=1)
            }
        }
        ff
    })
    getLabels <- reactive({
        sector <- input$sector
        if (sector == "Use above symbols"){
            sector <- input$symbols
        }
        else if (sector != "Tech2"){
            sector <- paste0(sector," (",paste(gtickers,collapse=", "),")")
        }
        xlabel <- "Date"
        if (input$graph == "adjusted price"){
            ylabel <- "Adjusted Prices"
            title  <- paste0(sector,": ",ylabel)
        }
        else if (input$graph == "percent change"){
            ylabel <- "Percent Change"
            title  <- paste0(sector,": ",ylabel)
        }
        else if (input$graph == "% change vs Symbol"){
            tickers0 <- unlist(strsplit(input$symbols,","))
            ylabel <- "Percent Change"
            title  <- paste0(sector,": Percent Change versus ",tickers0[1])
        }
        else{
            ylabel <- "Cumulative Percent Change"
            title  <- paste0(sector,": ",ylabel)
        }
        labels <- c(title,xlabel,ylabel)
    })
    output$distPlot2 <- renderPlot({
        ff <- getData2()
        labels <- getLabels()
        title  <- labels[1]
        xlabel <- labels[2]
        ylabel <- labels[3]
        #mm <- gather(ff,ticker,value,VB,VEA,VO,VSS,VTI,VWO)
        mm <- gather(ff,ticker,value,colnames(ff)[-1])
        # if (input$yscale != "linear"){
        #     mm$value <- mm$value + 100
        # }
        cat(file = stderr(), paste0("chartSeries(", input$symbol2, ", ", strDate1, ")\n"))
        #gg <- ggplot(data=gdata, aes_string(x="ref.date",y="price.adjusted",group="ticker"))
        gg <- ggplot(data=mm, aes_string(x="ref.date",y="value",group="ticker"))
        #gg <- gg + geom_point(data=mm,aes_string(color="ticker",shape="ticker"), size=3, alpha=0.7)
        gg <- gg + geom_point(aes(color=ticker,shape=ticker), size=3, alpha=0.7)
        if (input$yscale == "log2"){
            require(scales)
            gg <- gg + scale_y_continuous(trans = log2_trans(),
                        breaks = trans_breaks("log2", function(x) 2^x),
                        labels = trans_format("log2", math_format(2^.x)))
        }
        else if (input$yscale == "log10"){
            gg <- gg + scale_y_log10()
        }
        else if (input$yscale == "log10 from"){
            gg <- gg + scale_y_log10(limits = c(input$ymin, NA))
            #gg <- gg + ylim(input$ymin, NA)
        }
        if (input$linetype){
            gg <- gg + geom_line(aes(color=ticker,linetype=ticker), size=1, alpha=0.7)
        }else{
            gg <- gg + geom_line(aes(color=ticker), size=1, alpha=0.7)
        }
        if (substring(input$datefmt,1,1) != "#"){
            gg <- gg + scale_x_date(date_labels = input$datefmt)
        }
        gg <- gg + ggtitle(title) + xlab(xlabel) + ylab(ylabel)
        gg
    })
    output$myText2 <- renderPrint({
        labels <- getLabels()
        cat(paste0(labels[1],"\n\n"))
        ff <- getData2()
        colnames(ff)[1] <- "Date"
        print(ff)
        parmlist <- URLencode(paste0(
            "?symbols=",input$symbols,"&sector=",input$sector,"&graph=",input$graph,
            "&dateRange2[1]=",input$dateRange2[1],"&dateRange2[2]=",input$dateRange2[2],
            "&span2=",input$span2,"&freq2=",input$freq2,"&sma=",input$sma,
            "&datefmt=",input$datefmt,"&cache=",input$cache,"&linetype=",input$linetype))
        cat(file = stderr(), paste0(parmlist,"\n"))
    })
})