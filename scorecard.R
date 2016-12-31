#' @title scorecard
#' @description Scorecard from bivio transactions, models, and blotter integration.
#' @author mrb, \email{mrb@greatgray.org}

# TODO run scorecard models with investment; simulated trades
# TODO format scorecard
# TODO buy-hold equal weight basket 
# TODO ggplot position chart, like chart.Posn
# TODO use formattable for this data frame

invisible(suppressPackageStartupMessages(lapply(c("XML",
                                                  "yaml",
                                                  "timeSeries",
                                                  "quantmod",
                                                  "tidyr",
                                                  "dplyr",
                                                  "magrittr",
                                                  "blotter",
                                                  "lubridate",
                                                  "formattable",
                                                  "stringr",
                                                  "PerformanceAnalytics",
                                                  "ggplot2",
                                                  "directlabels",
                                                  "scales"
),
library,
warn.conflicts=FALSE, 
character.only=TRUE,
verbose=FALSE)))
invisible(suppressMessages(lapply(c("lib/checkBlotter.R",
                                    "lib/emit.R",
                                    "lib/prettyStats.R",
                                    "lib/ggutil.R"
),
source,verbose=FALSE)))

options("stringsAsFactors"=FALSE)
options("getSymbols.auto.assign"=FALSE)
options("getSymbols.warning4.0"=FALSE)

# initialize blotter parameters
init.date <- "2016-08-01"
switch.date <- "2016-08-05"
init.eq <- 250000 # per model
acct.name <- "ggcm"
port.name <- "model"
Sys.setenv(TZ="GMT")
benchmark.symbol <- "SPY"
verbose <- TRUE

# initialize scoredard framework
scorecard <- yaml.load_file("scorecard.yaml")
scorecard.vintage <- scorecard$vintage
scorecard.version <- scorecard$version
scorecard.table <- scorecard$table
scorecard.tickers <- c()

# initialize transaction history
bivio <- "/Users/mrb/Desktop/clubexp.xml"
if ( ! file.exists(bivio) ) 
  stop(paste("Cannot find Bivio export file",bivio))

doc <- xmlParse(bivio)
xmltop <- xmlRoot(doc)

asdf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(as.id=as.Date(xmlValue(yc$import_date)),
             as.key=xmlValue(yc$sync_key)) # not valuable
}

medf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(me.amount=as.numeric(xmlValue(yc$amount)),
             me.type=xmlValue(yc$entry_type),
             me.tax_basis=xmlValue(yc$tax_basis),
             me.tax_category=xmlValue(yc$tax_category),
             me.units=xmlValue(yc$units),
             me.user_id=xmlValue(yc$user_id),
             me.valuation_date=as.Date(xmlValue(yc$valuation_date))
  )
}

aedf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(ae.amount=as.numeric(xmlValue(yc$amount)),
             ae.type=xmlValue(yc$entry_type),
             ae.tax_basis=xmlValue(yc$tax_basis),
             ae.tax_category=xmlValue(yc$tax_category),
             ae.allocate=xmlValue(yc$allocate_equally),
             ae.expense_id=xmlValue(yc$expense_id),
             ae.realm_id=xmlValue(yc$realm_account_id)
  )
}

iedf <- function(y) {
  yc <- xmlChildren(y)
  data.frame(ie.amount=as.numeric(xmlValue(yc$amount)),
             ie.entry=xmlValue(yc$entry_id),
             ie.type=xmlValue(yc$entry_type),
             ie.tax_basis=xmlValue(yc$tax_basis),
             ie.tax_category=xmlValue(yc$tax_category),
             ie.acquisition_date=as.Date(xmlValue(yc$allocate_equally)),
             ie.count=xmlValue(yc$count),
             ie.external_id=xmlValue(yc$external_identifier),
             ie.realm_id=xmlValue(yc$realm_instrument_id)
  )
}

# get symbol data, adjust close, trim history, save to global environment
getAndAdjust <- function(tickers,init_date,switch_date) {
  for ( ticker in tickers) {
    if ( ticker %nin% scorecard.tickers) {
      message(paste("Fetching",ticker))
      dx <- getSymbols(ticker,
                       from=init.date,
                       index.class=c("POSIXt","POSIXct"),
                       warnings=FALSE,
                       verbose=FALSE)
      if ( has.Ad(dx) ) {
        dx <- adjustOHLC(dx,use.Adjusted=TRUE) # adjust dividends, spinoffs, etc.
      }
      dx <- dx[paste(switch.date,"::",sep=''),] # trim prehistorical data
      colnames(dx) <- gsub(paste(ticker,'.',sep=''),"",colnames(dx))
      assign(ticker,dx,envir=.GlobalEnv) # put back into global environment
      scorecard.tickers <<- c(scorecard.tickers,ticker)
    }
  }
}

plotModelStat <- function(ms,title,y.label="Value ($)",line.color="blue") {
  df <- data.frame(coredata(ms),index(ms))
  colnames(df) <- c("Value","Date")
  g <- ggplot(df,aes(x=Date,y=Value)) +
    geom_line(color=line.color) +
    xlab(NULL) +
    scale_y_continuous(name=y.label,labels=dollar) +
    ggtitle(title)
}

plotPosition <- function (Portfolio, Symbol, Dates = NULL, ..., TA = NULL) 
{
  pname <- Portfolio
  Portfolio <- getPortfolio(pname)
  if (missing(Symbol)) 
    Symbol <- ls(Portfolio$symbols)[[1]]
  else Symbol <- Symbol[1]
  Prices = get(Symbol)
  if (!is.OHLC(Prices)) {
    if (hasArg(prefer)) 
      prefer = eval(match.call(expand.dots = TRUE)$prefer)
    else prefer = NULL
    Prices = getPrice(Prices, prefer = prefer)
  }
  freq = periodicity(Prices)
  switch(freq$scale, seconds = {
    mult = 1
  }, minute = {
    mult = 60
  }, hourly = {
    mult = 3600
  }, daily = {
    mult = 86400
  }, {
    mult = 86400
  })
  if (!isTRUE(freq$frequency * mult == round(freq$frequency, 
                                             0) * mult)) {
    n = round((freq$frequency/mult), 0) * mult
  }
  else {
    n = mult
  }
  tzero = xts(0, order.by = index(Prices[1, ]))
  if (is.null(Dates)) 
    Dates <- paste(first(index(Prices)), last(index(Prices)), 
                   sep = "::")
  Portfolio$symbols[[Symbol]]$txn <- Portfolio$symbols[[Symbol]]$txn[Dates]
  Portfolio$symbols[[Symbol]]$posPL <- Portfolio$symbols[[Symbol]]$posPL[Dates]
  Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty
  Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades > 
                                                           0)]
  Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades < 
                                                            0)]
  Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty
  if (nrow(Position) < 1) 
    stop("no transactions/positions to chart")
  if (as.POSIXct(first(index(Prices))) < as.POSIXct(first(index(Position)))) 
    Position <- rbind(xts(0, order.by = first(index(Prices) - 
                                                1)), Position)
  Positionfill = na.locf(merge(Position, index(Prices)))
  CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
  if (length(CumPL) > 1) 
    CumPL = na.omit(na.locf(merge(CumPL, index(Prices))))
  else CumPL = NULL
  if (!is.null(CumPL)) {
    CumMax <- cummax(CumPL)
    Drawdown <- -(CumMax - CumPL)
    Drawdown <- rbind(xts(-max(CumPL), order.by = first(index(Drawdown) - 
                                                          1)), Drawdown)
  }
  else {
    Drawdown <- NULL
  }
  if (!is.null(Dates)) 
    Prices = Prices[Dates]
  chart_Series(Prices, name = Symbol, TA = TA, ...)
  if (!is.null(nrow(Buys)) && nrow(Buys) >= 1) 
    (add_TA(Buys, pch = 2, type = "p", col = "green", on = 1))
  if (!is.null(nrow(Sells)) && nrow(Sells) >= 1) 
    (add_TA(Sells, pch = 6, type = "p", col = "red", on = 1))
  if (nrow(Position) >= 1) {
    (add_TA(Positionfill, type = "s", col = "blue", lwd = 2)) # was type h
    (add_TA(Position, type = "p", col = "orange", lwd = 2, 
            on = 2))
  }
  if (!is.null(CumPL)) 
    (add_TA(CumPL, col = "darkgreen", lwd = 2))
  if (!is.null(Drawdown)) 
    (add_TA(Drawdown, col = "darkred", lwd = 2, yaxis = c(0, 
                                                          -max(CumMax))))
  plot(current.chob())
}

# instrument list and tickers
# trim deceased ticker dates from the ticker names; may need them later
ins <- getNodeSet(doc,"//instrument_list/*[not (instrument_valuation)]")
idf <- xmlToDataFrame(ins)
bivio_tickers <- unique((idf %>% filter(instrument_type=="STOCK"))$ticker_symbol)
bivio_tickers <- sapply(strsplit(bivio_tickers,'-',fixed=TRUE),first)



# transactions
tns <- getNodeSet(doc,"//transaction")
tdf <- bind_rows(lapply(tns,function(x){
  ch <- xmlChildren(x)
  sp <- which(names(ch)=="user_id") + 1
  cv <- bind_rows(lapply(sp:length(names(ch)), function(y) {
    df <- data.frame(
      date=as.Date(xmlValue(ch[["date_time"]])),
      modified=as.Date(xmlValue(ch[["modified_date_time"]])),
      remark=xmlValue(ch[["remark"]]),
      source_class=xmlValue(ch[["source_class"]]),
      user_id=xmlValue((ch[["user_id"]]))
    )
    sv <- switch(names(ch)[y],
                 account_entry=aedf(ch[[y]]),
                 instrument_entry=iedf(ch[[y]]),
                 member_entry=medf(ch[[y]]),
                 account_sync=asdf(ch[[y]])
    )
    df <- bind_cols(df,sv)
    return(df)
  }))
  
  return(cv)
}))


# transactions, instruments, non-NA amounts, with tickers and names attached
ti.df <- left_join(tdf %>% 
                     filter(source_class=="INSTRUMENT") %>% 
                     filter(is.na(ie.amount)==FALSE),
                   idf[,c("instrument_type","name","realm_instrument_id","ticker_symbol")],
                   by=c("ie.realm_id" = "realm_instrument_id")) %>%
  select(-me.amount,-me.type,-me.tax_basis,-me.tax_category,-me.units,-me.user_id) %>%
  select(-me.valuation_date,-as.id,-as.key,-ae.realm_id,-ie.acquisition_date) %>% 
  select(-ae.amount,-ae.type,-ae.tax_basis,-ae.tax_category,-ae.allocate,-ae.expense_id)

# trim to start August 3, 2016, regime transition action date; first buy 2016-08-05
tday <- first(which(ti.df$date==switch.date))
ti.df <- ti.df[tday:nrow(ti.df),]

# trim the capital gains records, bookkeeping not related to blotter
`%nin%` <- Negate(`%in%`) 
ti.df <- ti.df %>% 
  filter(ie.tax_category %nin% c("SHORT_TERM_CAPITAL_GAIN","LONG_TERM_CAPITAL_GAIN"))

# purge some regime overlap tickers; these had transactions after model regime start
# 2016/08 minutes: tickers AFL, ESV, NE, QCOM, WFC and XLP to be kept, all else sell
# Here we ignore the kept tickers anyway so we don't accumulate them for the scorecard.
# 2016/08 minutes: new positions starting model regime: XLP, XLU, TLT (RSO model)
ignore.tickers <- c('AFL','BWXT','COH','EMC','EMN','ESV','FL',
                    'FOSL','GILD','GM','GS','HYLD','MRK','NE',
                    'NOV','NSC','PGNPQ','QCOM','RYU','SLB','T','TROW',
                    'TRV','WFC','XLRE')
ti.df <- ti.df %>% filter(ticker_symbol %nin% ignore.tickers)
transaction.tickers <- unique(ti.df$ticker_symbol)

# benchmark returns applicable to every model
ignore <- getAndAdjust(benchmark.symbol,init.date,switch.date)
benchmark.returns <- diff(Cl(log(get(benchmark.symbol))))[-1,]
colnames(benchmark.returns) <- "Benchmark"
benchmark.cumulatives <- cumprod(1+benchmark.returns)


# identify model configuration files from scorecard
model.files <- unlist(sapply(scorecard.table,function(section) {
  unlist(sapply(section$models,function(m){
    return(m$config)
  }))
}))

# find model status
find.status <- function(name) {
  rv <- "NF"
  for ( si in scorecard.table ) {
    status <- si$status
    for ( mi in si$models ) {
      if ( grepl(name,mi$config) ) {
        rv <- status
      }
    }
  }
  return(rv)
}

# find model live date
find.live <- function(name) {
  rv <- ""
  for ( si in scorecard.table ) {
    if ( grepl(si$status,'activated') ) {
      for ( mi in si$models ) {
        if ( grepl(name,mi$config) ) {
          rv <- mi$live
        }
      }
    }
  }
  return(rv)
}


### initialize scorecard
scorecard.out <- data.frame()
refresh <- today()
for ( mf in model.files ) {
  location <- paste("models",mf,sep='/')
  mc <- yaml.load_file(location)
  df <- data.frame(Status=find.status(mf),
                   ModelID=mc$model,
                   Exp.OOS=mc$backtest$stop,
                   Exp.CAGR=mc$backtest$cagr,
                   Exp.MDD=mc$backtest$mdd,
                   Exp.Sortino=mc$backtest$sortino,
                   Exp.Calmar=mc$backtest$calmar,
                   Data.Live=find.live(mf),
                   Data.Refresh=refresh,
                   Actual.CAGR=NA,
                   Actual.MDD=NA,
                   Actual.Sortino=NA,
                   Actual.Calmar=NA,
                   Owner=mc$partner
  )
  scorecard.out <- bind_rows(scorecard.out,df)
}
rownames(scorecard.out) <- scorecard.out$ModelID

### collect reesults for each model configuration
for ( mf in model.files ) {
  location <- paste("models",mf,sep='/')
  model.configuration <- yaml.load_file(location)
  model.basket <- model.configuration$config$basket
  model.name <- model.configuration$model
  ignore <- getAndAdjust(model.basket,init.date,switch.date)
  
  # final purge of transactions, eliminate non-basket transactions
  transactions.df <- ti.df %>% filter(ticker_symbol %in% model.basket )
  
  # clear the blotter account and portfolios
  if (!exists(".blotter"))
    .blotter <- new.env()
  rm(list=ls(envir=.blotter),envir=.blotter)
  FinancialInstrument::currency("USD")
  
  # setup blotter account and portfolio
  initPortf(name=port.name, model.basket, initDate=init.date, currency="USD")
  initAcct(name=acct.name, portfolios=c(port.name), initDate=init.date, initEq=init.eq)
  
  # buy-hold account
  # initialize quantstrat objects
  initPortf("buyhold.port", symbols=model.basket, initDate=init.date)
  initAcct("buyhold.acct", portfolios="buyhold.port", initDate=init.date, initEq=init.eq)
  
  # setup blotter instruments
  for ( mt in model.basket) {
    stock(mt,currency="USD")
  }
  
  # add transactions to blotter
  for ( i in 1:nrow(transactions.df) ) {
    ti <- transactions.df[i,]
    # switch returns a function having ti parameter
    rv <- switch(ti$ie.type,
                 "INSTRUMENT_COVER_SHORT_SALE"=function(ti) {
                   message(paste("Ignoring option transaction",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_SHORT_SALE"=function(ti) {
                   warning(paste("Ignoring short sale",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_EXERCISE_BUY_OPTION"=function(ti) {
                   message(paste("Ignoring option buy exercise",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_EXERCISE_SELL_OPTION"=function(ti) {
                   message(paste("Ignoring option sell exercise"),ti$date,ti$ticker_symbol)
                 },
                 "INSTRUMENT_SPLIT"=function(ti) {
                   warning(paste("Ignoring instrument split",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_MERGER"=function(ti) {
                   warning(paste("Ignoring merger",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_SPINOFF"=function(ti) {
                   warning(paste("Ignoring spinoff",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_WASH_SALE"=function(ti) {
                   warning(paste("Ignoring wash sale",ti$date,ti$ticker_symbol))
                 },
                 "INSTRUMENT_BUY_COMMISSION"=function(ti) {
                   # amount is positive
                   addTxn(Portfolio=port.name, 
                          Symbol=ti$ticker_symbol, 
                          TxnDate=ti$date,
                          TxnPrice=0, 
                          TxnQty=0, 
                          TxnFees= -ti$ie.amount, 
                          verbose=verbose)
                 },
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
                 },
                 "INSTRUMENT_SELL"=function(ti) {
                   # count is negative, amount is negative
                   count <- as.numeric(ti$ie.count)
                   # recorded.price <- ti$ie.amount / count
                   ticker.xts <- get(ti$ticker_symbol)
                   adjusted.price <- Cl(ticker.xts[ti$date,])
                   addTxn(Portfolio=port.name, 
                          Symbol=ti$ticker_symbol, 
                          TxnDate=ti$date,
                          TxnPrice=adjusted.price,
                          TxnQty=count, 
                          TxnFees=0,
                          verbose=verbose)
                 },
                 "INSTRUMENT_SELL_COMMISSION_AND_FEE"=function(ti) {
                   # amount is negative
                   addTxn(Portfolio=port.name, 
                          Symbol=ti$ticker_symbol, 
                          TxnDate=ti$date,
                          TxnPrice=0, 
                          TxnQty=0, 
                          TxnFees= ti$ie.amount, 
                          verbose=verbose)
                 },
                 "INSTRUMENT_DISTRIBUTION_CASH"=function(ti) {
                   # amount is positive
                   qty <- getPosQty(port.name,ti$ticker_symbol,ti$date)
                   if ( qty > 0 ) {
                     #dps <- ti$ie.amount / qty
                     #addDiv(Portfolio=port.name, 
                     #        Symbol=ti$ticker_symbol, 
                     #        TxnDate=ti$date,
                     #        DivPerShare=dps,
                     #        TxnFees=0, 
                     #        verbose=verbose)
                     message(paste("Ignoring cash distribution",
                                   ti$date,
                                   ti$ticker_symbol,
                                   dollar(ti$ie.amount),
                                   qty))
                   } else {
                     # dividend received after having sold position
                     #addAcctTxn(acct.name,
                     #            ti$date,
                     #            TxnType = "Additions",                                                  
                     #                       ti$ie.amount,
                     #            verbose=verbose)
                     message(paste("Ignoring cash distribution",
                                   ti$date,
                                   ti$ticker_symbol,
                                   dollar(ti$ie.amount),
                                   qty))
                   }
                 },
                 "INSTRUMENT_DISTRIBUTION_RETURN_OF_CAPITAL"=function(ti) {
                   # amount is negative
                   # adjusted close price already considers distributions
                   qty <- getPosQty(port.name,ti$ticker_symbol,ti$date)
                   amount <- abs(ti$ie.amount)
                   if ( qty > 0 ) {
                     # dps <- amount / qty
                     # addDiv(Portfolio=port.name, 
                     #        Symbol=ti$ticker_symbol, 
                     #        TxnDate=ti$date,
                     #        DivPerShare=dps,
                     #        TxnFees=0, 
                     #        verbose=verbose)
                     message(paste("Ignoring capital distribution",
                                   ti$date,
                                   ti$ticker_symbol,
                                   dollar(amount),
                                   qty))
                   } else {
                     # distribution received after having sold position
                     #addAcctTxn(acct.name,
                     #            ti$date,
                     #            TxnType = "Additions",                                               
                     #                       amount,
                     #            verbose=verbose)
                     message(paste("Ignoring capital distribution",
                                   ti$date,
                                   ti$ticker_symbol,
                                   dollar(amount)))
                   }
                 },
                 function(ti) { 
                   warning(paste("TI switch did not match",ti$ie.type)) 
                 }
    ) # switch
    rv(ti)
  }
  
  lastDate <- xts::last(transactions.df)$date
  updatePortf(port.name)
  updateAcct(acct.name)
  updateEndEq(acct.name)
  
  # portfolio plot sanity check
  model.stats <- getAccount(acct.name)$portfolios$model
  ms1 <- plotModelStat(model.stats$Gross.Value,paste(model.name,"Model Stats: Gross Value"))
  ms3 <- plotModelStat(model.stats$Net.Trading.PL,paste(model.name,"Model Stats: Net Trading P&L"))
  print(ms1)
  print(ms3)
  
  # account plot sanity check
  account.summary <- getAccount(acct.name)$summary
  as1 <- plotModelStat(account.summary$End.Eq,paste(model.name,"Account Stats: Ending Equity"))
  print(as1)
  
  
  # component and account returns
  pr <- PortfReturns(acct.name,Portfolios=port.name,period="daily") # all portfolios
  colnames(pr) <- gsub(".DailyEndEq","",colnames(pr))
  ar <- AcctReturns(acct.name)
  cr <- cumprod(1+pr)
  
  # portfolio return status
  pm <- Return.portfolio(pr,wealth.index=FALSE,geometric=FALSE)
  colnames(pm) <- c("Model")
  pm$Total <- rowSums(pm, na.rm=TRUE)
  pm$Cumulative <- cumprod(1+pm$Total)
  pm.epl <- dailyEqPL(port.name)
  
  # individual component prices, drawdown, P&L
  for ( mt in model.basket ) {
    pf <- getPortfolio(port.name)
    position = pf$symbols[[mt]]$txn$Pos.Qty
    if (nrow(position) > 1) 
      # chart.Posn(port.name,mt)
      plotPosition(port.name,mt)
  }
  
  # component return plots
  pr.df <- data.frame(pr) %>% mutate(Date=index(pr))
  gf <- pr.df %>% gather(Symbol,Return,-Date)
  ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
    geom_line() +
    facet_wrap(~Symbol,nrow=3,scales="fixed") +
    xlab(NULL) +
    guides(color=FALSE) +
    ggtitle(paste(model.name,"Basket Element Returns"))
  ggplot(gf,aes(x=Return,fill=Symbol,color=Symbol)) +
    geom_histogram(binwidth=0.01) +
    geom_density() +
    guides(fill=FALSE,color=FALSE) +
    facet_wrap(~Symbol,nrow=3,scales="fixed") +
    ylab("Frequency") +
    xlab(paste("Daily Returns",min(gf$Date),"to",max(gf$Date),sep=' ')) +
    ggtitle(paste(model.name,"Basket Element Return Distributions"))
  
  # component cumulative returns
  cr.df <- data.frame(cr) %>% mutate(Date=index(cr))
  gf <- cr.df %>% gather(Symbol,Return,-Date)
  p <- ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
    geom_line() +
    xlab(NULL) +
    ylab("Component Return") +
    guides(color=FALSE) +
    ggtitle(paste(model.name,"Model Component Cumulative Return"))
  direct.label(p)
  
  # trade stats
  stats <- tradeStats(port.name)
  #formatted.stats <- formatTradeStats(stats)
  #textplot(t(formatted.stats))
  
  stats <- stats %>% select(-Symbol,-Portfolio,
                            -Med.Trade.PL,-Std.Dev.Trade.PL,
                            -Percent.Positive,-Percent.Negative,
                            -Avg.Win.Trade,-Avg.Losing.Trade,
                            -Med.Win.Trade,-Med.Losing.Trade,-Avg.Trade.PL,
                            -Avg.Daily.PL,-Med.Daily.PL,-Std.Dev.Daily.PL,
                            -Med.WinLoss.Ratio,
                            -End.Equity)
  stats[is.na(stats)] <- 0
  formatted.stats <- formattable(stats,list(
    area(col=c(Max.Equity)) ~ normalize_bar("lightgreen",min=0,max=1),
    area(col=c(Min.Equity)) ~ normalize_bar("pink",max=0,min=-1),
    # Largest.Winner = color_tile("white","green"),
    # Largest.Loser = color_tile("red","white"),
    area(col=c(Largest.Winner)) ~ normalize_bar("lightgreen",min=0,max=1),
    area(col=c(Largest.Loser)) ~ normalize_bar("pink",max=0,min=-1),
    # Largest.Winner = accounting(stats$Largest.Winner),
    Avg.WinLoss.Ratio = color_tile("white","green"),
    Profit.To.Max.Draw = color_tile("white","green")
  ))
  for ( fsn in names(formatted.stats) )
    formatted.stats[,fsn] = accounting(stats[,fsn],digits=0)
  print(formatted.stats)
  
  # model performance ratios
  annual.percent <- as.numeric(Return.annualized(pm$Model)) * 100 # percent
  calmar.ratio <- as.numeric(CalmarRatio(pm$Model)) # ratio
  sortino.ratio <- as.numeric(SortinoRatio(pm$Model,MAR=0)) # ratio
  max.drawdown.percent <- maxDrawdown(pm$Model) * 100 # percent
  
  # buy-hold basket comparison
  message(paste("Working",model.name,"buy-hold basket comparison"))
  buyhold.equal.equity <- init.eq / length(model.basket)
  buyhold.equal.weights <- rep(1.0 / length(model.basket),length(model.basket))
  
  for ( ticker in model.basket ) {
    history <- get(ticker)
    price <- as.numeric(Cl(history[switch.date,]))
    quantity <- buyhold.equal.equity / price
    addTxn(Portfolio="buyhold.port", 
           Symbol=ticker, 
           TxnDate=switch.date,
           TxnPrice=price,
           TxnQty=quantity, 
           TxnFees=-4.95,
           verbose=verbose)
  }
  updatePortf("buyhold.port")
  updateAcct("buyhold.acct")
  updateEndEq("buyhold.acct")
  
  # buy-hold account plot 
  buyhold.summary <- getAccount("buyhold.acct")$summary
  bh1 <- plotModelStat(buyhold.summary$End.Eq,
                       paste(model.name,"Buy-Hold Equal-Weight Basket Stats: Ending Equity"))
  print(bh1)
  
  bhr <- PortfReturns("buyhold.acct",Portfolios="buyhold.port",period="daily")
  colnames(bhr) <- gsub(".DailyEndEq","",colnames(bhr))
  bhc <- cumprod(1+bhr)
  
  bhp <- Return.portfolio(bhr,wealth.index=FALSE,geometric=FALSE)
  colnames(bhp) <- c("Buy-Hold")
  bhp$Total <- rowSums(bhp, na.rm=TRUE)
  bhp$Cumulative <- cumprod(1+bhp$Total)
  bhp.epl <- dailyEqPL("buyhold.port")
  
  # basket component cumulative returns  
  bhc.df <- data.frame(bhc) %>% mutate(Date=index(bhc))
  gf <- bhc.df %>% gather(Symbol,Return,-Date)
  p <- ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
    geom_line() +
    xlab(NULL) +
    ylab("Component Return") +
    guides(color=FALSE) +
    ggtitle(paste(model.name,"Buy-Hold Basket Component Cumulative Return"))
  direct.label(p)
  
  # basket aggregate cumulative return
  bhc.df <- data.frame(bhp$Cumulative) %>% mutate(Date=index(bhp$Cumulative))
  p <- ggplot(bhc.df,aes(x=Date,y=Cumulative)) +
    geom_line(color="blue") +
    xlab(NULL) +
    ylab("Cumulative Return") +
    ggtitle(paste(model.name,"Buy-Hold Equal-Weight Basket Cumulative Return"))
  print(p)
  
  bh.annual.percent <- as.numeric(Return.annualized(pm$Model)) * 100 # percent
  bh.calmar.ratio <- as.numeric(CalmarRatio(pm$Model)) # ratio
  bh.sortino.ratio <- as.numeric(SortinoRatio(pm$Model,MAR=0)) # ratio
  bh.max.drawdown.percent <- maxDrawdown(pm$Model) * 100 # percent
  
  # combined active model and buy-hold cumulative return
  xc <- merge(benchmark.cumulatives,pm$Cumulative,bhp$Cumulative)
  colnames(xc) <- c("Benchmark",model.name,"Buy-Hold")
  xc.df <- data.frame(xc,Date=index(xc))
  gf <- xc.df %>% gather(Portfolio,Return,-Date)
  p <- ggplot(gf,aes(x=Date,y=Return,color=Portfolio)) +
    geom_line() +
    xlab(NULL) +
    ylab("Portfolio Return") +
    guides(color=FALSE) +
    ggtitle(paste(model.name,"vs. Buy-Hold Basket Cumulative Return"))
  direct.label(p)
  
  # combined active model and buy-hold drawdowns
  xd <- na.omit(na.locf(drawdowns(timeSeries(merge(benchmark.returns,pm$Total,bhp$Total)))))
  colnames(xd) <- c("Benchmark",model.name,"Buy-Hold")
  xd.df <- data.frame(xd,Date=index(xd))
  gf <- xd.df %>% gather(Portfolio,Drawdown,-Date)
  p <- ggplot(gf,aes(x=Date,y=Drawdown,color=Portfolio)) +
    geom_line() +
    xlab(NULL) +
    ylab("Drawdown") +
    guides(color=FALSE) +
    ggtitle(paste(model.name,"vs. Buy-Hold Basket Drawdowns"))
  direct.label(p, visualcenter)
  
  # save model performance results to scorecard output
  scorecard.out[model.name,'Actual.CAGR'] <- annual.percent
  scorecard.out[model.name,'Actual.MDD'] <- max.drawdown.percent
  scorecard.out[model.name,'Actual.Sortino'] <- sortino.ratio
  scorecard.out[model.name,'Actual.Calmar'] <- calmar.ratio
}

# scorecard rankings for activated and candidate
actual.rank <- scorecard.out %>% 
  filter(Status %in% c('activated','candidate')) %>% 
  mutate(Actual.CAGR.R=dense_rank(desc(Actual.CAGR))) %>%
  mutate(Actual.MDD.R=dense_rank(Actual.MDD)) %>%
  mutate(Actual.Calmar.R=dense_rank(desc(Actual.Calmar))) %>%
  mutate(Actual.Sortino.R=dense_rank(desc(Actual.Sortino))) %>%
  select(ModelID,Actual.CAGR.R,Actual.MDD.R,Actual.Calmar.R,Actual.Sortino.R)
rownames(actual.rank) <- actual.rank$ModelID
scorecard.ranked <- left_join(scorecard.out,actual.rank,by='ModelID')
scorecard.ranked[is.na(scorecard.ranked)] <- ''

# benchmark candlestick chart
# ggCandles(get(benchmark.symbol),title_param="Benchmark")

# performance summary tryptich
# ggChartsPerformanceSummary(bhr,"Buy-Hold Components")

