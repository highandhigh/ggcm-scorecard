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
                                                  "directlabels",
                                                  "quantstrat"
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
enact.date <- "2016-07-31"
stop.date <- Sys.Date()
initial.eq <- 250000
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
sym.rank.df <- as.data.frame(sym.rank)
colnames(sym.rank.df) <- gsub(".Rank","",colnames(sym.rank))
sym.rank.df$Date <- as.Date(rownames(sym.rank.df))
sym.rank.df <- tail(sym.rank.df,n=24) # last 24 months
dfg <- gather(sym.rank.df,Symbol,Value,1:(ncol(df)-1))
sym.rank.p <- ggplot(dfg,aes(x=Date,y=Value)) +
  facet_grid(Symbol~.) +
  geom_step(color="blue") +
  scale_y_reverse(breaks=c(1,3,5,7,9),labels=c("1","3","5","7","9")) +
  ggtitle("3ROC Ranking by Fund (Last 24 Months)") +
  ylab("3ROC Value (1 is Highest)") +
  xlab(NULL) +
  geom_hline(yintercept=top.N,linetype="dashed",color="darkgreen")
sym.rank.p

# returns and performance starting enact date
prices <- NULL
for (symbol in symbols)
  prices <- cbind(prices,Cl(get(symbol)))
colnames(prices) <- symbols
returns <- diff(log(prices))[-1, ]
components <- returns[paste0(enact.date,"::"),]
basket.comp.p <- ggChartsPerformanceSummary(components,
                                ptitle="RM13 3ROC Basket Component Performance")
basket.comp.p

# recreate transactions
# clear the blotter account and portfolios
if (!exists(".blotter"))
  .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)

# setup blotter account and portfolio
acct.name <- "3roc.acct"
port.name <- "3roc.port"
acct.date <- as.Date(enact.date)-1
initPortf(name=port.name, symbols, initDate=acct.date, currency="USD")
initAcct(name=acct.name, portfolios=c(port.name), initDate=acct.date, initEq=initial.eq)

# setup blotter instruments
currency("USD")
for ( mt in symbols) {
  stock(mt,currency="USD",multiplier=1)
}

action.df <- sym.rank[paste0(enact.date,'::'),]
colnames(action.df) <- gsub(".Rank","",colnames(action.df))
previous.symbols <- c()

for ( i in 1:nrow(action.df)) {
  updatePortf(port.name)
  updateAcct(acct.name)
  updateEndEq(acct.name)
  
  ranks.df <- action.df[i]
  top.df <- ranks.df[,which(ranks.df <= top.N)]
  rank.date <- as.Date(index(ranks.df))
  top.symbols <- colnames(top.df)
  port.eq <- initial.eq
  
  if ( i > 1 ) {
    port.eq <- getEndEq(acct.name,rank.date)
  }
  # sell old positions
  for ( ps in previous.symbols[which(previous.symbols %nin% top.symbols )]) {
    psp <- suppressWarnings(to.monthly(get(ps),indexAt="lastof"))
    psp <- dollar(as.numeric(Cl(psp[rank.date,])))
    message(paste(rank.date,"sell position",ps,"at",psp))
  }
  for ( ts in top.symbols ) {
    if ( ts %in% previous.symbols ) {
      message(paste(rank.date,"already hold",ts))
    } else {
      tsp <- suppressWarnings(to.monthly(get(ts),indexAt="lastof"))
      tsp <- dollar(as.numeric(Cl(tsp[rank.date,])))
      message(paste(rank.date,"buy position",ts,"at",tsp))
    }
  }
  previous.symbols <- top.symbols
}


for ( i in sym.rank.df)
"INSTRUMENT_BUY"=function(ti) {
  # count is positive, amount is positive
  count <- as.numeric(ti$ie.count)
  # recorded.price <- ti$ie.amount / count # transaction price not adjusted
  ticker.xts <- get(ti$ticker_symbol)
  adjusted.price <- Cl(ticker.xts[ti$date,])
  addTxn(Portfolio=port.name, 
         Symbol=ti$ticker_symbol, 
         TxnDate=ti$date,
         TxnPrice=adjusted.price,
         TxnQty=count, 
         TxnFees=0,
         verbose=verbose)
}

updatePortf(port.name)
updateAcct(acct.name)
updateEndEq(acct.name)

