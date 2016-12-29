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

# advanced charts.PerforanceSummary based on ggplot
ggChartsPerformanceSummarySO <- function(rtn.obj,
                                       geometric = TRUE,
                                       main = "",
                                       plot = FALSE)
{
  # load libraries
  suppressPackageStartupMessages(require(ggplot2))
  suppressPackageStartupMessages(require(scales))
  suppressPackageStartupMessages(require(reshape))
  suppressPackageStartupMessages(require(PerformanceAnalytics))
  
  # create function to clean returns if having NAs in data
  clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace = 0) {
    univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)] <- na.replace
    univ.rtn.xts.obj
  }
  
  # Create cumulative return function
  cum.rtn <- function(clean.xts.obj, g = TRUE) {
    x <- clean.xts.obj
    if (g == TRUE) {
      y <- cumprod(x + 1) - 1
    } else {
      y <- cumsum(x)
    }
    y
  }
  
  # Create function to calculate drawdowns
  dd.xts <- function(clean.xts.obj, g = TRUE) {
    x <- clean.xts.obj
    if (g == TRUE) {
      y <- PerformanceAnalytics:::Drawdowns(x)
    } else {
      y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)
    }
    y
  }
  
  # create a function to create a dataframe
  cps.df <- function(xts.obj,geometric)
  {
    x <- clean.rtn.xts(xts.obj)
    series.name <- colnames(xts.obj)[1]
    tmp <- cum.rtn(x,geometric)
    tmp$rtn <- x
    tmp$dd <- dd.xts(x,geometric)
    colnames(tmp) <- c("Index","Return","Drawdown") # names with space
    tmp.df <- as.data.frame(coredata(tmp))
    tmp.df$Date <- as.POSIXct(index(tmp))
    tmp.df.long <- melt(tmp.df,id.var = "Date")
    tmp.df.long$asset <- rep(series.name,nrow(tmp.df.long))
    tmp.df.long
  }
  
  # A conditional statement altering the plot according to the number of assets
  if (ncol(rtn.obj) == 1)
  {
    # using the cps.df function
    df <- cps.df(rtn.obj,geometric)
    # adding in a title string if need be
    if (main == "") {
      title.string <- paste("Asset Performance")
    } else {
      title.string <- main
    }
    
    gg.xts <-
      ggplot(df, aes_string(x = "Date", y = "value", group = "variable")) +
      facet_grid(variable ~ ., scales = "free_y", space = "fixed") +
      geom_line(data = subset(df, variable == "Index")) +
      geom_bar(data = subset(df, variable == "Return"), stat = "identity") +
      geom_line(data = subset(df, variable == "Drawdown")) +
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      ggtitle(title.string) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      ylab("") +
      xlab("")
  }
  else
  {
    # a few extra bits to deal with the added rtn columns
    no.of.assets <- ncol(rtn.obj)
    asset.names <- colnames(rtn.obj)
    df <-
      do.call(rbind,lapply(1:no.of.assets, function(x) {
        cps.df(rtn.obj[,x],geometric)
      }))
    df$asset <- ordered(df$asset, levels = asset.names)
    if (main == "") {
      title.string <-
        paste("Asset",asset.names[1],asset.names[2],asset.names[3],"Performance")
    } else {
      title.string <- main
    }
    
    if (no.of.assets > 5)
    {
      legend.rows <- 5
    } else {
      legend.rows <- no.of.assets
    }
    
    gg.xts <- ggplot(df, aes_string(x = "Date", y = "value")) +
      
      # panel layout
      facet_grid(
        variable ~ ., scales = "free_y", space = "fixed", shrink = TRUE, drop = TRUE, margin =
          , labeller = label_value
      ) + # label_value is default
      
      # display points for Index and Drawdown, but not for Return
      geom_point(
        data = subset(df, variable == c("Index","Drawdown"))
        , aes(colour = factor(asset), shape = factor(asset)), size = 1.2, show_guide = TRUE
      ) +
      
      # manually select shape of geom_point
      scale_shape_manual(values = c(1,2,3)) +
      
      # line colours for the Index
      geom_line(
        data = subset(df, variable == "Index"), aes(colour = factor(asset)), show_guide = FALSE
      ) +
      
      # bar colours for the Return
      geom_bar(
        data = subset(df,variable == "Return"), stat = "identity"
        , aes(fill = factor(asset), colour = factor(asset)), position = "dodge", show_guide = FALSE
      ) +
      
      # line colours for the Drawdown
      geom_line(
        data = subset(df, variable == "Drawdown"), aes(colour = factor(asset)), show_guide = FALSE
      ) +
      
      # horizontal line to indicate zero values
      geom_hline(yintercept = 0, size = 0.5, colour = "black") +
      
      # horizontal ticks
      scale_x_datetime(breaks = date_breaks("6 months"), labels = date_format("%m/%Y")) +
      
      # main y-axis title
      ylab("") +
      
      # main x-axis title
      xlab("") +
      
      # main chart title
      ggtitle(title.string)
    
    # legend
    
    gglegend <- guide_legend(override.aes = list(size = 3))
    
    gg.xts <- gg.xts + guides(colour = gglegend, size = "none") +
      
      # gglegend <- guide_legend(override.aes = list(size = 3), direction = "horizontal") # direction overwritten by legend.box?
      # gg.xts <- gg.xts + guides(colour = gglegend, size = "none", shape = gglegend) + # Warning: "Duplicated override.aes is ignored"
      
      theme(
        legend.title = element_blank()
        , legend.position = c(0,1)
        , legend.justification = c(0,1)
        , legend.background = element_rect()
        , legend.box = "horizontal" # not working?
        , axis.text.x = element_text(angle = 0, hjust = 1)
      )
    
  }
  
  #assign("gg.xts", gg.xts,envir = .GlobalEnv)
  #if (plot == TRUE) {
  #  plot(gg.xts)
  #} else {
  #}
  return(gg.xts)
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
ggChartsPerformanceSummary <- function(r.xts,ptitle="",geometric=TRUE) {
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
  pc <- data.frame(Date=index(c.xts),Plot="Cumulative Returns",c.xts)
  pr <- data.frame(Date=index(r.xts),Plot="Period Returns",r.xts)
  pd <- data.frame(Date=index(d.xts),Plot="Drawdowns",d.xts)
  pf <- bind_rows(pc,pr,pd) %>% gather(Series,Value,3:ncol(pr)) %>% na.omit
  
  # facet plot
  p <- ggplot(pf,aes(x=Date,y=Value,color=Series,fill=Series)) +
    geom_line(data=subset(pf,Plot=="Cumulative Returns")) +
    geom_bar(data=subset(pf,Plot=="Period Returns"),stat="identity",position="dodge") +
    geom_smooth(data=subset(pf,Plot=="Period Returns"),method="lm") +
    geom_line(data=subset(pf,Plot=="Drawdowns")) +
    facet_grid(Plot~.,scales="free_y",space="free_y") +
    ggtitle(ptitle) + xlab(NULL) + ylab(NULL)
  return(p)
}