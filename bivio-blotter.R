library(XML)
library(timeSeries)
library(quantmod)
library(tidyr)
library(dplyr)
library(blotter)
library(lubridate)
library(stringr)
library(PerformanceAnalytics)
options(stringsAsFactors=FALSE)
options(getSymbols.auto.assign=FALSE)

# t <- xmlTreeParse("clubexp.xml")
# topXml <- xmlRoot(t)
bivio <- "clubexp.xml"
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

# trim to start August 2016
ti.df <- ti.df %>% 
  filter(year(date)>2015) %>% 
  filter(month(date)>7)
model_tickers <- unique(ti.df$ticker_symbol)

# fetch yahoo data, adjust it
# return is a list of lists
init.date <- "2016-01-01"
init.eq <- 1e6
port.name <- "model"
acct.name <- "ggcm"
Sys.setenv(TZ="GMT")
# xtsenv <- new.env()
for ( mt in model_tickers ) {
  # env assignment is a list by ticker
#  ev <- list(dx=NA)
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
#    ev <- list(dx=dx)
  }
  # assign(mt,ev,envir=xtsenv)
}  

# setup blotter
symbols <- model_tickers
if (!exists(".blotter"))
  .blotter <- new.env()
rm(list=ls(envir=.blotter),envir=.blotter)
initPortf(name=port.name, symbols, initDate=init.date, currency="USD")
initAcct(name=acct.name, portfolios=c(port.name), initDate=init.date,initEq=init.eq)

# for blotter use
# ee <- getEndEquity(acct.name, currentDate)
# position <- getPosQty(port.name, Symbol=symbols[1], Date=currentDate)
# addTxn(port.name, Symbol=symbols[1],  TxnDate=currentDate, TxnPrice=closePrice, TxnQty = -unitSize , TxnFees=5, verbose=T)
# addTxns()
# addDiv()
# updatePortf(ltportfolio, Dates = currentDate)
# updateAcct(ltaccount, Dates = currentDate)
# updateEndEq(ltaccount, Dates = currentDate)
# plot(getAccount(ltaccount)[["TOTAL"]]$End.Eq)

equity = getEndEq(ltaccount, currentDate)

# transactions by ticker 
# TODO assumes fund XTS has dates for transactions in join
# TODO gather tickers into models, tally by model
for (mt in model_tickers)  {
  
  #ev <- get(mt,envir=.GlobalEnv)
  transactions.df <- ti.df %>% filter(ticker_symbol == mt)
  #ev$tx <- transactions.df
  #ev$df <- NA
  
  #if ( ! is.na(ev$dx[[1]]) ) {
  #  fund.df <- data.frame(coredata(ev$dx),date=index(ev$dx))
  #  fund.df <- left_join(fund.df,transactions.df,by="date")
    
    # keep active transactions
    fund.df <- fund.df %>% 
      filter(!is.na(ie.type))
    
    # compute share position, based on ie.count field
    fund.df$ie.count[is.na(fund.df$ie.count)] <- 0
    fund.df <- fund.df %>% 
      mutate(Position=runSum(fund.df$ie.count,n=1,cumulative=TRUE)) %>%
      mutate(MarketValue=Position*Close)
    fund.df$Position[1] <- 0
    fund.df$MarketValue[1] <- 0
    
    # cash value
    # shares * close + gain
    
    # compute price returns
    # requires timeSeries
#     pr <- returns0(fund.df$Close,
#                   method="discrete",
#                   trim=FALSE,
#                   na.rm=FALSE)
#     fund.df <- bind_cols(fund.df,data.frame(PriceReturn=pr))
    
    # cumulative returns
#    fund.df$CumulativeReturn <- na.fill(cumprod(1+fund.df$CashReturn),0)

    # store merged df in environment list    
    ev$df <- fund.df
  }
  assign(mt,ev,envir = xtsenv)  
}

# > unique(ti.df$ie.tax_category)
# [1] "NOT_TAXABLE"             "QUALIFIED_DIVIDEND"      "LONG_TERM_CAPITAL_GAIN" 
# [4] "SHORT_TERM_CAPITAL_GAIN" "DIVIDEND"  

# > unique(ti.df$ie.type)
# [1] "INSTRUMENT_EXERCISE_BUY_OPTION"    "INSTRUMENT_BUY_COMMISSION"             
# [3] "INSTRUMENT_BUY"                    "INSTRUMENT_DISTRIBUTION_CASH"            
# [5] "INSTRUMENT_SELL"                   "INSTRUMENT_SELL_COMMISSION_AND_FEE"      
# [7] "INSTRUMENT_COVER_SHORT_SALE"       "INSTRUMENT_DISTRIBUTION_RETURN_OF_CAPITAL"
