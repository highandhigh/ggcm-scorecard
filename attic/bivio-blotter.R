#' @title ggcm-models
#' @description Bivio transactions, models, and blotter integration.
#' @author mrb, \email{mrb@greatgray.org}

# TODO run scorecard models with investment; simulated trades
# TODO format scorecard
# TODO buy-hold equal weight basket 
# TODO ggplot position chart, like chart.Posn
# TODO use formattable for this data frame

invisible(suppressPackageStartupMessages(lapply(c("XML",
                                                  "timeSeries",
                                                  "quantmod",
                                                  "tidyr",
                                                  "dplyr",
                                                  "blotter",
                                                  "lubridate",
                                                  "stringr",
                                                  "PerformanceAnalytics",
                                                  "ggplot2",
                                                  "directlabels",
                                                  "scales",
                                                  "formattable"
),
library,
warn.conflicts=FALSE, 
character.only=TRUE,
verbose=FALSE)))
invisible(suppressMessages(lapply(c("lib/checkBlotter.R",
                                    "lib/emit.R",
                                    "lib/prettyStats.R",
                                    "lib/ggutil.R",
                                    "lib/position.R",
                                    "model-update-rm13-roc.R"
),
source,verbose=FALSE)))

options(stringsAsFactors=FALSE)
options(getSymbols.auto.assign=FALSE)
options(getSymbols.warning4.0=FALSE)

# initialize blotter parameters
init.date <- "2016-08-01"
switch.date <- "2016-08-05"
init.eq <- 250000 # per model
acct.name <- "ggcm"
port.name <- "model"
Sys.setenv(TZ="GMT")
benchmark.symbol <- "SPY"

# t <- xmlTreeParse("clubexp.xml")
# topXml <- xmlRoot(t)
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
getAndAdjust <- function(ticker,init_date,switch_date) {
  message(paste("Fetching",ticker))
  dx <- getSymbols(ticker,
                   from=init_date,
                   index.class=c("POSIXt","POSIXct"),
                   warnings=FALSE,
                   verbose=FALSE)
  if ( has.Ad(dx) ) {
    dx <- adjustOHLC(dx,use.Adjusted=TRUE) # adjust dividends, spinoffs, etc.
  }
  #dx <- dx[paste(switch_date,"::",sep=''),] # trim prehistorical data
  colnames(dx) <- gsub(paste(ticker,'.',sep=''),"",colnames(dx))
  assign(ticker,dx,envir=.GlobalEnv) # put back into global environment
}

# export info
ens <- getNodeSet(doc,"//club_export_info")
edf <- xmlToDataFrame(ens) 

# user info
uns <- getNodeSet(doc,"//user")
udf <- xmlToDataFrame(uns)

# tax info 1065
t1ns <- getNodeSet(doc,"//tax_1065")
t1df <- xmlToDataFrame(t1ns)

# tax info k1
tkns <- getNodeSet(doc,"//tax_k1")
tkdf <- xmlToDataFrame(tkns)

# member allocation
mns <- getNodeSet(doc,"//member_allocation")
mdf <- xmlToDataFrame(mns)

# instrument list and tickers
# trim deceased ticker dates from the ticker names; may need them later
ins <- getNodeSet(doc,"//instrument_list/*[not (instrument_valuation)]")
idf <- xmlToDataFrame(ins)
bivio_tickers <- unique((idf %>% dplyr::filter(instrument_type=="STOCK"))$ticker_symbol)
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
                     dplyr::filter(source_class=="INSTRUMENT") %>% 
                     dplyr::filter(is.na(ie.amount)==FALSE),
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
  dplyr::filter(ie.tax_category %nin% c("SHORT_TERM_CAPITAL_GAIN","LONG_TERM_CAPITAL_GAIN"))

# purge some regime overlap tickers; these had transactions after model regime start
# 2016/08 minutes: tickers AFL, ESV, NE, QCOM, WFC and XLP to be kept, all else sell
# Here we ignore the kept tickers anyway so we don't accumulate them for the scorecard.
# 2016/08 minutes: new positions starting model regime: XLP, XLU, TLT (RSO model)
ignore_tickers <- c('AFL','BWXT','COH','EMC','EMN','ESV','FL',
                    'FOSL','GILD','GM','GS','HYLD','MRK','NE',
                    'NOV','NSC','PGNPQ','QCOM','RYU','SLB','T','TROW',
                    'TRV','WFC','XLRE')
ti.df <- ti.df %>% dplyr::filter(ticker_symbol %nin% ignore_tickers)

# fetch then adjust ticker historical data
# remaining symbols are likely model basket elements
transaction_tickers <- unique(ti.df$ticker_symbol)
model_tickers <- c() # empty to start, then fill with non-option tickers
for ( mt in transaction_tickers ) {
  if ( str_detect(mt,"[0-9]") == FALSE ) {
    ignore <- getAndAdjust(mt,init.date,switch.date)
    model_tickers <- c(model_tickers,mt) # add ticker to the model tickers list
  }
}  

# final purge of transactions, eliminate options transactions (for now)
ti.df <- ti.df %>% dplyr::filter(ticker_symbol %in% model_tickers )

# clear the blotter account and portfolios
if (!exists(".blotter"))
  .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)

# setup blotter account and portfolio
initPortf(name=port.name, model_tickers, initDate=init.date, currency="USD")
initAcct(name=acct.name, portfolios=c(port.name), initDate=init.date, initEq=init.eq)

# setup blotter instruments
FinancialInstrument::currency("USD")
for ( mt in model_tickers) {
  stock(mt,currency="USD",multiplier=1)
}

# add transactions to blotter
verbose=TRUE
for ( i in 1:nrow(ti.df) ) {
  ti <- ti.df[i,]
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
                   #            TxnType = "Additions",                                               #                       ti$ie.amount,
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
                   #            TxnType = "Additions",                                               #                       amount,
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

lastDate <- xts::last(ti.df)$date
updatePortf(port.name)
updateAcct(acct.name)
updateEndEq(acct.name)

plotModelStat <- function(ms,title,y.label="Value ($)",line.color="blue") {
  df <- data.frame(coredata(ms),index(ms))
  colnames(df) <- c("Value","Date")
  g <- ggplot(df,aes(x=Date,y=Value)) +
    geom_line(color=line.color) +
    xlab(NULL) +
    scale_y_continuous(name=y.label,labels=dollar) +
    ggtitle(title)
}

# portfolio plot sanity check
model.stats <- getAccount(acct.name)$portfolios$model
ms1 <- plotModelStat(model.stats$Gross.Value,"Model Stats: Gross Value")
ms3 <- plotModelStat(model.stats$Net.Trading.PL,"Model Stats: Net Trading P&L")
ms1
ms3

# account plot sanity check
account.summary <- getAccount(acct.name)$summary
as1 <- plotModelStat(account.summary$End.Eq,"Account Stats: Ending Equity")
as1


# component and account returns
pr <- PortfReturns(acct.name,Portfolios=port.name,period="daily") # all portfolios
colnames(pr) <- gsub(".DailyEndEq","",colnames(pr))
ar <- AcctReturns(acct.name)
colnames(ar) <- "Account"
cr <- cumprod(1+pr)

# benchmark returns
ignore <- getAndAdjust(benchmark.symbol,init.date,switch.date)
# assign(get(benchmark.symbol),benchmark.symbol) # already trimmed
benchmark.returns <- diff(Cl(log(get(benchmark.symbol))))[-1,]
colnames(benchmark.returns) <- "Benchmark"
benchmark.cumulatives <- cumprod(1+benchmark.returns)

# portfolio return status
#pm <- Return.portfolio(pr,wealth.index=FALSE,geometric=FALSE)
#colnames(pm) <- c("Model")
pm <- pr
pm$Actual <- rowSums(pm, na.rm=TRUE)
pm$Cumulative <- cumprod(1+pm$Actual)
pm.epl <- dailyEqPL(port.name)

# individual component prices, drawdown, P&L
for ( mt in model_tickers) {
  # print(chart.Posn(port.name,mt))
  print(chart.Position(port.name,mt))
}

# benchmark candlestick chart
#ggCandles(get(benchmark.symbol),title_param="Benchmark")

# component return plots
pr.df <- data.frame(pr) %>% mutate(Date=index(pr))
gf <- pr.df %>% gather(Symbol,Return,-Date)
ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
  geom_line() +
  facet_wrap(~Symbol,nrow=3,scales="fixed") +
  xlab(NULL) +
  ylab("Realized Return") +
  guides(color=FALSE)
ggplot(gf,aes(x=Return,fill=Symbol,color=Symbol)) +
  geom_histogram(binwidth=0.01) +
  geom_density() +
  guides(fill=FALSE,color=FALSE) +
  facet_wrap(~Symbol,nrow=3,scales="fixed") +
  ylab("Realized Return Frequency") +
  xlab(paste("Daily Returns",min(gf$Date),"to",max(gf$Date),sep=' '))

# component cumulative returns
cr.df <- data.frame(cr) %>% mutate(Date=index(cr))
gf <- cr.df %>% gather(Symbol,Return,-Date)
p <- ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
  geom_line() +
  xlab(NULL) +
  ylab("Realized Return") +
  guides(color=FALSE) +
  ggtitle("Model Component Cumulative Return")
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
formatted.stats


# actual performance ratios
annual.percent <- as.numeric(Return.annualized(pm$Actual)) * 100 # percent
calmar.ratio <- as.numeric(CalmarRatio(pm$Actual)) # ratio
sortino.ratio <- as.numeric(SortinoRatio(pm$Actual,MAR=0.10/12)) # ratio
max.drawdown.percent <- maxDrawdown(pm$Actual) * 100 # percent

# theoretical performance out of sample
mrv <- model_update_rm13_roc()
at.returns <- merge(pm$Actual,mrv$theoretical.returns,benchmark.returns)
# colnames(at.returns) <- c("Actual","Theoretical","Benchmark")
ggChartsPerformanceSummary2(at.returns,ptitle="Theoretical vs. Actual")
