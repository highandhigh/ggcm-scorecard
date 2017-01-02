#########################################################################
# Copyright (C) 2017 GGCM LLP                                           #
#########################################################################

#' @title RM13 3ROC actuals
#' @author mrb
#' @description Run and report update for fixed model configuration


invisible(suppressPackageStartupMessages(lapply(c("quantmod",
                                                  "PerformanceAnalytics",
                                                  "ggplot2",
                                                  "dplyr",
                                                  "tidyr",
                                                  "scales",
                                                  "directlabels"
                                                  ),
                                                library,
                                                warn.conflicts=FALSE,
                                                character.only=TRUE,
                                                verbose=FALSE)))
invisible(suppressMessages(lapply(c("lib/emit.R",
                                    "lib/rank.R",
                                    "lib/ggutil.R",
                                    "lib/monthly.R"),
                                  source,
                                  verbose=FALSE)))


# general options
options("getSymbols.warning4.0"=FALSE,
        digits=4,
        width=100,
        scipen=100)

study.title <- "Rank Model 13p: ROC3E"
start.date <- "2004-12-01"
enact.date <- "2016-08-01"
stop.date <- Sys.Date()
top.N <- 3
max.levels <- 1
trail.stop.percent <- 0.00
transaction.fee <- -4.95 # per ETF order each way
periods <- c(1,2,3)
weights <- c(0.4,0.3,0.3)
symbols <- c("XLY", # 1998-12-22
             "XLP", # 1998-12-22
             "XLE", # 1998-12-22
             "XLK", # 1998-12-22
             "XLU", # 1998-12-22
             "XLF", # 1998-12-22
             "AGG", # 2003-09-26
             "TLT", # 2002-07-30
             "GLD" # 2004-11-18 limiter
)

getSymbols(symbols,from=start.date, to=stop.date, index.class="POSIXct")
for(symbol in symbols) {
  x<-get(symbol)
  x<-adjustOHLC(x,symbol.name=symbol)
  indexFormat(x)<-'%Y-%m-%d'
  colnames(x)<-gsub("x",symbol,colnames(x))
  assign(symbol,x)
}

# create an xts object of daily adjusted close prices
symbols.close.monthly <- monthlyPrices(symbols)

# create an xts object of the symbol ranks
sym.rank <- applyRank(x=symbols.close.monthly, 
                      rankFun=weightAve3ROC,
                      n=periods, 
                      weights=weights)
sym.rank <- na.fill(sym.rank,fill=ncol(sym.rank)) # ensure trading rules exit
colnames(sym.rank) <- gsub(".Adjusted", ".Rank", colnames(sym.rank))
stopifnot(all.equal(gsub(".Adjusted", "", colnames(symbols.close.monthly)), symbols))

# bind the rank column to the appropriate symbol market data
for(i in 1:length(symbols)) {
  x <- get(symbols[i])
  y <- na.locf(cbind(x,sym.rank[,i]))
  y <- y[,ncol(y)]
  x <- cbind(x,y,join="left")
  assign(symbols[i],x)
}
  

# last six months ranking by component
df <- tail(sym.rank, n=6)
colnames(df) <- gsub(".Rank","",colnames(sym.rank))
textplot( df )
title("Basket Component Ranking")

# last 24 months ranking plot  
df <- as.data.frame(sym.rank)
colnames(df) <- gsub(".Rank","",colnames(sym.rank))
df$Date <- as.Date(rownames(df))
df <- tail(df,n=24) # last 24 months
dfg <- gather(df,Symbol,Value,1:(ncol(df)-1))
p <- ggplot(dfg,aes(x=Date,y=Value)) +
  facet_grid(Symbol~.) +
  geom_step(color="blue") +
  scale_y_reverse(breaks=c(1,3,5,7,9),labels=c("1","3","5","7","9")) +
  ggtitle("3ROC Ranking by Fund (Last 24 Months)") +
  ylab("3ROC Value (1 is Highest)") +
  xlab(NULL) +
  geom_hline(yintercept=top.N,linetype="dashed",color="darkgreen")
p

# returns and performance starting enact date
prices <- NULL
for (symbol in symbols)
  prices <- cbind(prices,Cl(get(symbol)))
colnames(prices) <- symbols
returns <- diff(log(prices))[-1, ]
components <- returns[paste0(enact.date,"::"),]
p <- ggChartsPerformanceSummary(components,
                                ptitle="RM13 3ROC Basket Component Performance")
p


