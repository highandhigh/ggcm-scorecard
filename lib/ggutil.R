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
