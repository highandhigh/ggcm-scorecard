library(XML)
library(timeSeries)
library(quantmod)
library(tidyr)
library(dplyr)
library(blotter)
library(lubridate)
library(stringr)
library(PerformanceAnalytics)
library(ggplot2)
library(directlabels)
options(stringsAsFactors=FALSE)
options(getSymbols.auto.assign=FALSE)
options(getSymbols.warning4.0=FALSE)
source("prettyStats.R")

# initialize blotter
if (!exists(".blotter"))
  .blotter <- new.env()
init.date <- "2016-09-01"
init.eq <- 2e5
port.name <- "model"
acct.name <- "ggcm"
Sys.setenv(TZ="GMT")
rm(list=ls(envir=.blotter),envir=.blotter)


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

# trim to start September 16, 2016, fund transition buy date
tday <- first(which(ti.df$date=="2016-09-16"))
ti.df <- ti.df[tday:nrow(ti.df),]

# trim the capital gains records, bookkeeping not related to blotter
`%nin%` <- Negate(`%in%`) 
ti.df <- ti.df %>% 
  filter(ie.tax_category %nin% c("SHORT_TERM_CAPITAL_GAIN","LONG_TERM_CAPITAL_GAIN"))

# remaining symbols are likely model basket elements
transaction_tickers <- unique(ti.df$ticker_symbol)

# fetch then adjust ticker historical data
model_tickers <- c()
for ( mt in transaction_tickers ) {
  if ( ! str_detect(mt,"[0-9]") ) {
    message(paste("Fetching",mt))
    dx <- getSymbols(mt,
                     from=init.date,
                     index.class=c("POSIXt","POSIXct"),
                     warnings=FALSE,
                     verbose=FALSE)
    dx <- adjustOHLC(dx,use.Adjusted=TRUE)
    dx <- dx["2016-08::",]
    colnames(dx) <- gsub(paste(mt,'.',sep=''),"",colnames(dx))
    assign(mt,dx,envir=.GlobalEnv)
    model_tickers <- c(model_tickers,mt)
  }
}  

# setup blotter account and portfolio
initPortf(name=port.name, model_tickers, initDate=init.date, currency="USD")
initAcct(name=acct.name, portfolios=c(port.name), initDate=init.date, initEq=init.eq)

# setup blotter instruments
currency("USD")
for ( mt in model_tickers) {
  stock(mt,currency="USD",multiplier=1)
}

# add transactions to blotter
verbose=TRUE
for ( i in 1:nrow(ti.df) ) {
  ti <- ti.df[i,]
  rv <- switch(ti$ie.type,
               "INSTRUMENT_BUY_COMMISSION"=function(ti) {
                 # amount is positive
                 # paste(ti$date,"BUY COMMISSION",ti$ticker_symbol,ti$ie.amount)
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
                 # paste(ti$date,"BUY",ti$ie.count,ti$ticker_symbol,ti$ie.amount)
                 count <- as.numeric(ti$ie.count)
                 price <- ti$ie.amount / count
                 addTxn(Portfolio=port.name, 
                        Symbol=ti$ticker_symbol, 
                        TxnDate=ti$date,
                        TxnPrice=price,
                        TxnQty=count, 
                        TxnFees=0,
                        verbose=verbose)
               },
               "INSTRUMENT_SELL"=function(ti) {
                 # count is negative, amount is negative
                 # paste(ti$date,"SELL",ti$ie.count,ti$ticker_symbol,ti$ie.amount)
                 count <- as.numeric(ti$ie.count)
                 price <- ti$ie.amount / count
                 addTxn(Portfolio=port.name, 
                        Symbol=ti$ticker_symbol, 
                        TxnDate=ti$date,
                        TxnPrice=price,
                        TxnQty=count, 
                        TxnFees=0,
                        verbose=verbose)
               },
               "INSTRUMENT_SELL_COMMISSION_AND_FEE"=function(ti) {
                 # amount is negative
                 # paste(ti$date,"SELL COMMISSION",ti$ticker_symbol,ti$ie.amount)
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
                 # paste(ti$date,"CASH DISTRIBUTION",ti$ticker_symbol,ti$ie.amount)
                 qty <- getPosQty(port.name,ti$ticker_symbol,ti$date)
                 if ( qty > 0 ) {
                 dps <- ti$ie.amount / qty
                 addDiv(Portfolio=port.name, 
                        Symbol=ti$ticker_symbol, 
                        TxnDate=ti$date,
                        DivPerShare=dps,
                        TxnFees=0, 
                        verbose=verbose)
                 } else {
                   # dividend received after having sold position
                   addAcctTxn(acct.name,
                              ti$date,
                              TxnType = "Additions",                                                                      ti$ie.amount,
                              verbose=verbose)
                 }
               },
               "INSTRUMENT_DISTRIBUTION_RETURN_OF_CAPITAL"=function(ti) {
                 # amount is negative
                 # paste(ti$date,"CAPITAL DISTRIBITION",ti$ticker_symbol,ti$ie.amount)
                 qty <- getPosQty(port.name,ti$ticker_symbol,ti$date)
                 amount <- abs(ti$ie.amount)
                 if ( qty > 0 ) {
                   dps <- amount / qty
                   addDiv(Portfolio=port.name, 
                          Symbol=ti$ticker_symbol, 
                          TxnDate=ti$date,
                          DivPerShare=dps,
                          TxnFees=0, 
                          verbose=verbose)
                 } else {
                   # distribution received after having sold position
                   addAcctTxn(acct.name,
                              ti$date,
                              TxnType = "Additions",                                                                      amount,
                              verbose=verbose)
                 }
               },
               function(ti) { message(paste("TI switch did not match",ti$ie.type)) }
               )
  rv(ti)
}

lastDate <- xts::last(ti.df)$date
updatePortf(port.name)
updateAcct(acct.name)
updateEndEq(acct.name)

# portfolio plot sanity check
pm <- getAccount(acct.name)$portfolios$model
plot(pm$Gross.Value)
plot(pm$Realized.PL)
plot(pm$Net.Trading.PL)

# account plot sanity check
am <- getAccount(acct.name)$summary
plot(am$Net.Performance)
plot(am$End.Eq)

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

# component return plots

pr.df <- data.frame(pr) %>% mutate(Date=index(pr))
gf <- pr.df %>% gather(Symbol,Return,-Date)
ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
  geom_line() +
  facet_wrap(~Symbol,nrow=3,scales="fixed") +
  xlab(NULL) +
  guides(color=FALSE)
ggplot(gf,aes(x=Return,fill=Symbol)) +
  geom_histogram(binwidth=0.01) +
  geom_density() +
  guides(fill=FALSE) +
  facet_wrap(~Symbol,nrow=3,scales="fixed") +
  ylab("Frequency") +
  xlab(paste("Daily Returns",min(gf$Date),"to",max(gf$Date),sep=' '))

# component cumulative returns
cr.df <- data.frame(cr) %>% mutate(Date=index(cr))
gf <- cr.df %>% gather(Symbol,Return,-Date)
p <- ggplot(gf,aes(x=Date,y=Return,color=Symbol)) +
  geom_line() +
  xlab(NULL) +
  guides(color=FALSE) +
  ggtitle("Model Component Cumulative Return")
direct.label(p)

# trade stats
stats <- tradeStats(port.name)
formatted.stats <- formatTradeStats(stats)
textplot(t(formatted.stats))


#> names(getAccount(acct.name)$summary)
#[1] "Additions" "Withdrawals" "Realized.PL" "Unrealized.PL" "Interest"        
#[6] "Gross.Trading.PL" "Txn.Fees" "Net.Trading.PL" "Advisory.Fees" "Net.Performance" 
#[11] "End.Eq"   

#> names(getAccount(acct.name)$portfolios$model)
#[1] "Long.Value" "Short.Value" "Net.Value" "Gross.Value" "Realized.PL"    
#[6] "Unrealized.PL" "Gross.Trading.PL" "Txn.Fees" "Net.Trading.PL"  

# > unique(ti.df$ie.tax_category)
# [1] "NOT_TAXABLE"             "QUALIFIED_DIVIDEND"      "LONG_TERM_CAPITAL_GAIN" 
# [4] "SHORT_TERM_CAPITAL_GAIN" "DIVIDEND"  

# > unique(ti.df$ie.type)
# [1] "INSTRUMENT_EXERCISE_BUY_OPTION"    "INSTRUMENT_BUY_COMMISSION"             
# [3] "INSTRUMENT_BUY"                    "INSTRUMENT_DISTRIBUTION_CASH"            
# [5] "INSTRUMENT_SELL"                   "INSTRUMENT_SELL_COMMISSION_AND_FEE"      
# [7] "INSTRUMENT_COVER_SHORT_SALE"       "INSTRUMENT_DISTRIBUTION_RETURN_OF_CAPITAL"
