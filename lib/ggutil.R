# ggplot candlestick charts

# example, add moving average to this one:
# sma50 <- data.frame(SMA=SMA(Cl(SPY),n=50),Date=index(SPY))
# g + geom_line(aes(x=Date,y=SMA),data=sma50)

ggCandles <- function(symbol.xts, title_param = NA, alpha_param = 1){
  df <- as.data.frame(symbol.xts)
  df$Date <- index(symbol.xts)
  df$change <- ifelse(df$Close > df$Open, "up", ifelse(df$Close < df$Open, "down", "flat"))
  
  # originally the width of the bars was calculated by FXQuantTrader with use of 'periodicity()', which 
  # seems to work ok only with: ‘minute’,‘hourly’, ‘daily’,‘weekly’, ‘monthly’,
  # ‘quarterly’, and ‘yearly’, but can not do 1 sec bars while we want arbitrary bar size support!-)
  # df$width <- as.numeric(periodicity(df)[1])
  # So let us instead find delta (seconds) between 1st and 2nd row and just 
  # use it for all other rows. We check 1st 3 rows to avoid larger "weekend gaps"
  width_candidates <- c(as.numeric(difftime(df$Date[2], df$Date[1]), units = "secs"), 
                        as.numeric(difftime(df$Date[3], df$Date[2]), units = "secs"), 
                        as.numeric(difftime(df$Date[4], df$Date[3]), units = "secs"))
  
  df$width_s = min(width_candidates)  # one (same) candle width (in seconds) for all the bars
  
  # define the vector of candle colours either by name or by rgb()
  #candle_colors = c("down" = "red", "up" = "green", "flat" = "blue")
  candle_colors = c("down" = rgb(192,0,0,alpha=255,maxColorValue=255), "up" = rgb(0,192,0,alpha=255,maxColorValue=255), "flat" = rgb(0,0,192,alpha=255,maxColorValue=255))
  
  g <- ggplot(df, aes(x=Date)) +
    geom_linerange(aes(ymin=Low, 
                       ymax=High, 
                       colour = change), 
                   alpha = alpha_param) +
    theme_bw() +
    ggtitle(title_param) +
    ylab("Price") +
    xlab(NULL) +
    geom_rect(aes(xmin = Date - width_s/2 * 0.9, 
                  xmax = Date + width_s/2 * 0.9, 
                  ymin = pmin(Open, Close), 
                  ymax = pmax(Open, Close), 
                  fill = change), 
              alpha = alpha_param) +
    guides(fill = FALSE, colour = FALSE) +
    scale_color_manual(values = candle_colors) +  # color for line
    scale_fill_manual(values = candle_colors)     # color for candle fill  
  
  # Handle special cases: flat bar and Open == close:
  if (any(df$change == "flat")) 
    g <- g + geom_segment(data = df[df$change == "flat",], 
                          aes(x = Date - width_s / 2 * 0.9, 
                              y = Close, 
                              yend = Close, 
                              xend = Date + width_s / 2 * 0.9, 
                              colour = change), 
                          alpha = alpha_param)
  g
}

#Pass an OHLC object into this function
#also pass two dates formatted as.Date()
ggChartSeries <- function(x, 
                          start=as.Date(index(xts::first(x))), 
                          end=as.Date(index(xts::last(x))))
{
  require(ggplot2)
  
  # the below is done redundantly for ease of maintenance later on
  #First, strip OHLC data (need to vectorize)
  date <- as.Date(time(x))
  open <- as.vector(Op(x))
  high <- as.vector(Hi(x))
  low <- as.vector(Lo(x))
  close <- as.vector(Cl(x))
  
  #Then build the data frame
  xSubset <-data.frame('date'=date,'open'=open,'high'= high,'low'=low,'close'=close)
  
  #We want to construct our candlesticks  
  xSubset$candleLower <- pmin(xSubset$open, xSubset$close)
  xSubset$candleMiddle <- NA
  xSubset$candleUpper <- pmax(xSubset$open, xSubset$close)
  xSubset$fill <- ''
  xSubset$fill[xSubset$open < xSubset$close] = 'white'
  xSubset$fill[xSubset$fill ==''] = 'red'
  
  #Add Moving Averages
  if (nrow(xSubset) > 200)
    xSubset$ma200 <- SMA(xSubset$close, 200)
  #else
  #  xSubset$ma200 <- NULL
  if (nrow(xSubset) > 50)
    xSubset$ma50 <- SMA(xSubset$close, 50)
  #else
  #  xSubset$ma50 <- NULL
  
  #Trim Data
  xSubset <-subset(xSubset, xSubset$date > start & xSubset$date < end)
  
  #Graphing Step
  g <- ggplot(xSubset, aes(x=date, 
                           lower=candleLower, 
                           #middle=candleMiddle, 
                           middle=NA,
                           upper=candleUpper, 
                           ymin=low, 
                           ymax=high)) 
  g <- g + geom_boxplot(stat='identity', aes(group=date, fill=fill))
  if ( "ma50" %in% names(xSubset))
    g <- g + geom_line(aes(x=date, y=ma50))
  if ( "ma200" %in% names(xSubset))
    g <- g + geom_line(aes(x=date, y=ma200))
  g 
}



#' A ggplot version of PerformanceAnalytics performance summary charts
#' @example Returns the ggplot object for further manipulation. 
#' - to position legend use
#' p <- p + theme(legend.position="bottom")
#' 
#' - to eliminate series guide use
#' p <- p + guides(color=FALSE,fill=FALSE)
#' 
#' - to use direct labels to identify series on cumulative return plot without guides
#' require(directlabels)
#' direct.label(p+guides(color=FALSE,fill=FALSE))
#' 
#' @seealso PerformanceAnalytics::charts.PerformanceSummary, 
#' directlabels, dplyr, tidyr, magrittr, ggplot2
#' @param r.xts an XTS object with period returns for each column; 
#' function will compute cumulative return and drawdowns for each item
#' @param ptitle a plot title string, default empty string
#' @param geometric whether cumulative returns should use geometric method
#' @return ggplot object
#' @author mrb
#' 
ggChartsPerformanceSummary3 <- function(r.xts,ptitle="",geometric=TRUE) {
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(tidyr))
  suppressPackageStartupMessages(require(magrittr))
  suppressPackageStartupMessages(require(ggplot2))

  # cumulative return  
  c.xts <- if ( geometric ) {
    cumprod(1+r.xts)
  } else {
      1 + cumsum(r.xts)
  }
  
  # drawdowns
  d.xts <- do.call(cbind,lapply(1:ncol(c.xts),function(j){
    cx <- cummax(c.xts[,j])
    dd <- c.xts[,j] / cx
  }))

  # tagged dataframes to facilitate facet grid subsetting
  pc <- data.frame(Date=index(c.xts),Plot="Cumulative",c.xts,stringsAsFactors = FALSE)
  pr <- data.frame(Date=index(r.xts),Plot="Returns",r.xts,stringsAsFactors = FALSE)
  pd <- data.frame(Date=index(d.xts),Plot="Drawdowns",d.xts,stringsAsFactors = FALSE)
  pf <- bind_rows(pc,pr,pd) %>% gather(Series,Value,3:ncol(pr)) %>% na.omit
  
  # facet plot
  p <- ggplot(pf,aes(x=Date,y=Value,color=Series,fill=Series)) +
    geom_line(data=subset(pf,Plot=="Cumulative")) +
    geom_bar(data=subset(pf,Plot=="Returns"),stat="identity",position="dodge") +
    geom_smooth(data=subset(pf,Plot=="Returns"),method="lm") +
    geom_line(data=subset(pf,Plot=="Drawdowns")) +
    facet_grid(Plot~.,scales="free_y",space="free_y") +
    ggtitle(ptitle) + xlab(NULL) + ylab(NULL) +
    theme(strip.text.y = element_text(size=8))
  return(p)
}


#' A ggplot version of PerformanceAnalytics performance summary charts, cumulative and drawdown only.
#' @example Returns the ggplot object for further manipulation. 
#' - to position legend use
#' p <- p + theme(legend.position="bottom")
#' 
#' - to eliminate series guide use
#' p <- p + guides(color=FALSE,fill=FALSE)
#' 
#' - to use direct labels to identify series on cumulative return plot without guides
#' require(directlabels)
#' direct.label(p+guides(color=FALSE,fill=FALSE))
#' 
#' @seealso PerformanceAnalytics::charts.PerformanceSummary, 
#' directlabels, dplyr, tidyr, magrittr, ggplot2
#' @param r.xts an XTS object with period returns for each column; 
#' function will compute cumulative return and drawdowns for each item
#' @param ptitle a plot title string, default empty string
#' @param geometric whether cumulative returns should use geometric method
#' @return ggplot object
#' @author mrb
#'
ggChartsPerformanceSummary2 <- function(r.xts,ptitle="",geometric=TRUE,drawdown.minima=NA) {
  suppressPackageStartupMessages(require(dplyr))
  suppressPackageStartupMessages(require(tidyr))
  suppressPackageStartupMessages(require(magrittr))
  suppressPackageStartupMessages(require(ggplot2))

  # clean  
  r.xts <- na.omit(na.locf(r.xts))
  
  # cumulative return  
  c.xts <- if ( geometric ) {
    cumprod(1+r.xts)
  } else {
    1 + cumsum(r.xts)
  }
  
  # drawdowns
  d.xts <- do.call(cbind,lapply(1:ncol(c.xts),function(j){
    cx <- cummax(c.xts[,j])
    dd <- c.xts[,j] / cx
  }))
  
  # tagged dataframes to facilitate facet grid subsetting
  pc <- data.frame(Date=index(c.xts),Plot="Cumulative",c.xts,stringsAsFactors = FALSE)
  pd <- data.frame(Date=index(d.xts),Plot="Drawdowns",d.xts,stringsAsFactors = FALSE)
  pf <- bind_rows(pc,pd) %>% gather(Series,Value,3:ncol(pc)) %>% na.omit
  
  mindd <- min(d.xts,na.rm=TRUE)
  
  # facet plot
  p <- ggplot(pf,aes(x=Date,y=Value,color=Series,fill=Series)) +
    geom_line(data=subset(pf,Plot=="Cumulative")) +
    geom_line(data=subset(pf,Plot=="Drawdowns")) +
    facet_grid(Plot~.,scales="free_y",space="free_y") +
    ggtitle(ptitle) + xlab(NULL) + ylab(NULL) +
    theme(strip.text.y = element_text(size=10))
  if ( ! is.na(drawdown.minima) ) {
    p <- p + geom_hline(aes(yintercept=mindd),
                        data=subset(pf,Plot=="Drawdowns"),
                        color=drawdown.minima,
                        linetype="dashed")
  }
  return(p)
}

#' Reset quantstrat environments .strategy, .blotter, and .audit.
#' Creates environments if they do not exist
#' Removes all elements from the environment. 
resetQuantstrat <- function() {
  if (!exists(".strategy")) .strategy <<- new.env(parent=.GlobalEnv)
  if (!exists(".blotter")) .blotter <<- new.env(parent=.GlobalEnv)
  if (!exists(".audit")) .audit <<- new.env(parent=.GlobalEnv)
  suppressWarnings(rm(list=ls(.strategy), pos=.strategy))
  suppressWarnings(rm(list=ls(.blotter), pos=.blotter))
  suppressWarnings(rm(list=ls(.audit), pos=.audit))
  # suppressWarnings(rm(list=ls()))
  FinancialInstrument::currency("USD")
}

plotModelStat <- function(ms, title, y.label = "Value ($)", line.color = "blue") {
  df <- data.frame(coredata(ms), index(ms))
  colnames(df) <- c("Value", "Date")
  g <- ggplot(df, aes(x = Date, y = Value)) +
    geom_line(color = line.color) +
    xlab(NULL) +
    scale_y_continuous(name = y.label, labels = dollar) +
    ggtitle(title)
}

plotPosition <- function (Portfolio, Symbol,Dates = NULL,  ..., TA = NULL) {
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol))
    Symbol <- ls(Portfolio$symbols)[[1]]
  else
    Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer))
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else
      prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(
    freq$scale,
    seconds = {
      mult = 1
    },
    minute = {
      mult = 60
    },
    hourly = {
      mult = 3600
    },
    daily = {
      mult = 86400
    },
    {
      mult = 86400
    }
  )
  if (!isTRUE(freq$frequency * mult == round(freq$frequency, 0) * mult)) {
    n = round((freq$frequency / mult), 0) * mult
  }
  else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1,]))
  if (is.null(Dates))
    Dates <- paste(first(index(Prices)), last(index(Prices)), sep = "::")
  Portfolio$symbols[[Symbol]]$txn <-
    Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <-
    Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1)
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position))))
    Position <- rbind(xts(0, order.by = first(index(Prices) - 1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1)
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else
    CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <-
      rbind(xts(-max(CumPL), order.by = first(index(Drawdown) -
                                                1)), Drawdown)
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates))
    Prices = Prices[Dates]
  chart_Series(Prices, name = Symbol, TA = TA, ...)
  if (!is.null(nrow(Buys)) && nrow(Buys) >= 1)
    (add_TA(
      Buys,
      pch = 2,
      type = "p",
      col = "green",
      on = 1
    ))
  if (!is.null(nrow(Sells)) && nrow(Sells) >= 1)
    (add_TA(
      Sells,
      pch = 6,
      type = "p",
      col = "red",
      on = 1
    ))
  if (nrow(Position) >= 1) {
    (add_TA(
      Positionfill,
      type = "s",
      col = "blue",
      lwd = 2
    )) # was type h
    (add_TA(
      Position,
      type = "p",
      col = "orange",
      lwd = 2,
      on = 2
    ))
  }
  if (!is.null(CumPL))
    (add_TA(CumPL, col = "darkgreen", lwd = 2))
  if (!is.null(Drawdown))
    (add_TA(
      Drawdown,
      col = "darkred",
      lwd = 2,
      yaxis = c(0,-max(CumMax))
    ))
  # plot(current.chob())
  return(current.chob())
}
