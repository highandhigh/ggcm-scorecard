#' GGCM scorecard update.
#' @description Scorecard from Bivio transactions, models, and blotter trade integration.
#' @author mrb, \email{mrb@greatgray.org}


# TODO pass model config to model updater functions
# TODO do an account with model portfoliow consolidated, active models

# initialize blotter parameters
init.date <- "2016-08-01"
switch.date <- "2016-08-05"
init.eq <- 250000 # per model
benchmark.symbol <- "SPY"

###### production algorithm
Sys.setenv(TZ = "UTC")
acct.name <- "ggcm"
port.name <- "model"

###### options for many functions
options("stringsAsFactors" = FALSE)
options("getSymbols.auto.assign" = FALSE)
options("getSymbols.warning4.0" = FALSE)
options("verbose"=FALSE)

invisible(suppressPackageStartupMessages(
  lapply(
    c(
      "XML",
      "yaml",
      "zoo",
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
    warn.conflicts = FALSE,
    character.only = TRUE,
    verbose = getOption("verbose")
  )
))



# load everything in lib directory
# should include the model out-of-sample update functions
for ( nm in list.files("lib",pattern="[.][Rr]$")) {
  source(file.path("lib",nm))
}

# 1. initialize scorecard framework
scorecard <- yaml.load_file("scorecard.yaml")
#scorecard.vintage <- scorecard$vintage
#scorecard.version <- scorecard$version
#scorecard.activated <- scorecard$table$activated
#scorecard.candidate <- scorecard$table$candidate
#scorecard.deactivated <- scorecard$table$deactivated
#scorecard.retired <- scorecard$table$retired
scorecard$table$activated <- lapply(scorecard$table$activated,function(i){
  i$status <- 'activated'
  return(i)
})
scorecard$table$candidate <- lapply(scorecard$table$candidate,function(i){
  i$status <- 'candidate'
  return(i)
})
scorecard$table$deactivated <- lapply(scorecard$table$deactivated,function(i){
  i$status <- 'deactivated'
  return(i)
})
scorecard$table$retired <- lapply(scorecard$table$retired,function(i){
  i$status <- 'retired'
  return(i)
})



scorecard.tickers <- c() # filled later

# 2. initialize transaction history
bivio <- Sys.getenv("BIVIO_FILE") 
if (!file.exists(bivio))
  stop(paste("Cannot find Bivio export file", bivio))

doc <- xmlParse(bivio)
xmltop <- xmlRoot(doc)


# get symbol data, adjust close, trim history, save to global environment
getAndAdjust <- function(tickers, init_date, switch_date) {
  for (ticker in tickers) {
    if (ticker %nin% scorecard.tickers) {
      if ( getOption("verbose") ) 
        message(paste("Fetching", ticker))
      dx <- getSymbols(
        ticker,
        from = init.date,
        index.class = c("POSIXt", "POSIXct"),
        warnings = FALSE,
        verbose = getOption("verbose")
      )
      if (has.Ad(dx)) {
        dx <- adjustOHLC(dx, use.Adjusted = TRUE) # adjust dividends, spinoffs, etc.
      }
      dx <- dx[paste(switch.date, "::", sep = ''), ] # trim prehistorical data
      colnames(dx) <- gsub(paste(ticker, '.', sep = ''), "", colnames(dx))
      assign(ticker, dx, envir = .GlobalEnv) # put back into global environment
      scorecard.tickers <<- c(scorecard.tickers, ticker)
    }
  }
}


# instrument list and tickers
# trim deceased ticker dates from the ticker names; may need them later
ins <- getNodeSet(doc, "//instrument_list/*[not (instrument_valuation)]")
idf <- xmlToDataFrame(ins)
bivio_tickers <- unique((idf %>% dplyr::filter(instrument_type == "STOCK"))$ticker_symbol)
bivio_tickers <- sapply(strsplit(bivio_tickers, '-', fixed = TRUE), first)


# transactions
tns <- getNodeSet(doc, "//transaction")
tdf <- bind_rows(lapply(tns, function(x) {
  ch <- xmlChildren(x)
  sp <- which(names(ch) == "user_id") + 1
  cv <- bind_rows(lapply(sp:length(names(ch)), function(y) {
    df <- data.frame(
      date = as.Date(xmlValue(ch[["date_time"]])),
      modified = as.Date(xmlValue(ch[["modified_date_time"]])),
      remark = xmlValue(ch[["remark"]]),
      source_class = xmlValue(ch[["source_class"]]),
      user_id = xmlValue((ch[["user_id"]]))
    )
    sv <- switch(
      names(ch)[y],
      account_entry = aedf(ch[[y]]),
      instrument_entry = iedf(ch[[y]]),
      member_entry = medf(ch[[y]]),
      account_sync = asdf(ch[[y]])
    )
    df <- bind_cols(df, sv)
    return(df)
  }))
  
  return(cv)
}))


# transactions, instruments, non-NA amounts, with tickers and names attached
ti.df <- left_join(
  tdf %>%
    dplyr::filter(source_class == "INSTRUMENT") %>%
    dplyr::filter(is.na(ie.amount) == FALSE),
  idf[, c("instrument_type",
          "name",
          "realm_instrument_id",
          "ticker_symbol")],
  by = c("ie.realm_id" = "realm_instrument_id")
) %>%
  select(-me.amount,
         -me.type,
         -me.tax_basis,
         -me.tax_category,
         -me.units,
         -me.user_id) %>%
  select(-me.valuation_date,
         -as.id,
         -as.key,
         -ae.realm_id,
         -ie.acquisition_date) %>%
  select(-ae.amount,
         -ae.type,
         -ae.tax_basis,
         -ae.tax_category,
         -ae.allocate,
         -ae.expense_id)

# trim to start August 3, 2016, regime transition action date; first buy 2016-08-05
tday <- first(which(ti.df$date == switch.date))
ti.df <- ti.df[tday:nrow(ti.df), ]

# trim the capital gains records, bookkeeping not related to blotter
`%nin%` <- Negate(`%in%`)
ti.df %<>% dplyr::filter(ie.tax_category %nin%
                   c("SHORT_TERM_CAPITAL_GAIN", "LONG_TERM_CAPITAL_GAIN"))

# purge some regime overlap tickers; these had transactions after model regime start
# 2016/08 minutes: tickers AFL, ESV, NE, QCOM, WFC and XLP to be kept, all else sell
# Here we ignore the kept tickers anyway so we don't accumulate them for the scorecard.
# 2016/08 minutes: new positions starting model regime: XLP, XLU, TLT (RSO model)
ignore.tickers <- c(
  'AFL',
  'BWXT',
  'COH',
  'EMC',
  'EMN',
  'ESV',
  'FL',
  'FOSL',
  'GILD',
  'GM',
  'GS',
  'HYLD',
  'MRK',
  'NE',
  'NOV',
  'NSC',
  'PGNPQ',
  'QCOM',
  'RYU',
  'SLB',
  'T',
  'TROW',
  'TRV',
  'WFC',
  'XLRE'
)

ti.df %<>% dplyr::filter(ticker_symbol %nin% ignore.tickers)
transaction.tickers <- unique(ti.df$ticker_symbol)

# identify model configuration files from scorecard
scorecard.table <- c(
  scorecard$table$activated,
  scorecard$table$candidate,
  scorecard$table$deactivated,
  scorecard$table$retired
)
model.files <-
  unlist(lapply(scorecard.table, function(x)
    return (x$config)))

# compute benchmark returns applicable to every model
ignore <- getAndAdjust(benchmark.symbol, init.date, switch.date)
benchmark.returns <- diff(Cl(log(get(benchmark.symbol))))[-1, ]
colnames(benchmark.returns) <- "Benchmark"
benchmark.cumulatives <- cumprod(1 + benchmark.returns)
benchmark.annual.percent <- as.numeric(Return.annualized(benchmark.returns)) * 100
benchmark.calmar.ratio <- as.numeric(CalmarRatio(benchmark.returns))
benchmark.sortino.ratio <- as.numeric(SortinoRatio(benchmark.returns,MAR=0.1/12))
benchmark.max.drawdown.percent <- maxDrawdown(benchmark.returns) * 100

# compute buy-hold returns for each model
scorecard.table <- lapply(scorecard.table,function(scorecard.row) {
  scorecard.row$model <- NA
  model.name <- scorecard.row$id
  
  if ( !is.null(scorecard.row$config) ) {
    # read the model file
    if ( getOption("verbose") )
      message(paste("Working", model.name, "buy-hold basket comparison"))
    scorecard.row$model <- yaml.load_file(file.path("models",scorecard.row$config))
    basket <- scorecard.row$model$config$basket
    
    # save benchmark performance, currently same for each model
    scorecard.row$benchmark.cagr = benchmark.annual.percent
    scorecard.row$benchmark.calmar = benchmark.calmar.ratio
    scorecard.row$benchmark.sortino = benchmark.sortino.ratio
    scorecard.row$benchmark.mdd = benchmark.max.drawdown.percent
    
    # initialize quantstrat objects and blotter
    resetQuantstrat()
    initPortf("buyhold.port", symbols = basket, initDate = init.date)
    initAcct("buyhold.acct", portfolios = "buyhold.port", initDate = init.date, initEq = init.eq)
    ignore <- sapply(basket, function(m) {
        stock(m, currency = "USD")
      })
    
    # ensure basket element tickers have been fetched
    ignore <- getAndAdjust(basket, init.date, switch.date)

    # add a buy transaction for each ticker at switch date, equal weight
    buyhold.equal.equity <- init.eq / length(basket)
    buyhold.equal.weights <- rep(1.0 / length(basket), length(basket))
    for (ticker in basket) {
      history <- get(ticker)
      price <- as.numeric(Cl(history[switch.date, ]))
      quantity <- buyhold.equal.equity / price
      addTxn(
        Portfolio = "buyhold.port",
        Symbol = ticker,
        TxnDate = switch.date,
        TxnPrice = price,
        TxnQty = quantity,
        TxnFees = -4.95,
        verbose = getOption("verbose")
      )
    }
    updatePortf("buyhold.port")
    updateAcct("buyhold.acct")
    updateEndEq("buyhold.acct")
    
    # create buy-hold account plot, save in row
    buyhold.summary <- getAccount("buyhold.acct")$summary
    bhp1 <- plotModelStat(
      buyhold.summary$End.Eq,
      paste(model.name, "Buy-Hold Equal-Weight Basket Stats: Ending Equity"
      )
    )
    
    bhr <- PortfReturns("buyhold.acct", Portfolios = "buyhold.port", period = "daily")
    colnames(bhr) <- gsub(".DailyEndEq", "", colnames(bhr))
    bhc <- cumprod(1 + bhr)
    
    # append columns for portfolio
    bhp <- bhr
    bhp$BuyHold <- rowSums(bhp, na.rm = TRUE)
    bhp$Cumulative <- cumprod(1 + bhp$BuyHold)
    
    # basket component cumulative returns
    bhc.df <- data.frame(bhc) %>% mutate(Date = index(bhc))
    gf <- bhc.df %>% gather(Symbol, Return, -Date)
    bhp2 <- ggplot(gf, aes(x = Date, y = Return, color = Symbol)) +
      geom_line() +
      xlab(NULL) +
      ylab("Component Return") +
      guides(color = FALSE) +
      ggtitle(paste(model.name, "Buy-Hold Basket Component Cumulative Return"))
    bhp2 <- direct.label(bhp2)
    
    # basket aggregate cumulative return
    bhc.df <- data.frame(bhp$Cumulative) %>% mutate(Date = index(bhp$Cumulative))
    bhp3 <- ggplot(bhc.df, aes(x = Date, y = Cumulative)) +
      geom_line(color = "blue") +
      xlab(NULL) +
      ylab("Cumulative Return") +
      ggtitle(paste(model.name, "Buy-Hold Equal-Weight Basket Cumulative Return"))
    
    bh.annual.percent <- as.numeric(Return.annualized(bhp$BuyHold)) * 100 # percent
    bh.calmar.ratio <- as.numeric(CalmarRatio(bhp$BuyHold)) # ratio
    bh.sortino.ratio <- as.numeric(SortinoRatio(bhp$BuyHold, MAR = 0.1 / 12)) # ratio
    bh.max.drawdown.percent <- maxDrawdown(bhp$BuyHold) * 100 # percent
    
    # store in scorecard
    scorecard.row$buyhold <- list(
      r = bhr, 
      c = bhc, 
      portfolio = bhp,
      cagr = bh.annual.percent,
      mdd = bh.max.drawdown.percent,
      sortino = bh.sortino.ratio,
      calmar = bh.calmar.ratio,
      p1 = bhp1, p2 = bhp2, p3 = bhp3
    )
    
  }
  return(scorecard.row)
})

##########
# compute actual performance by transaction for traded models
# compute buy-hold returns for each model
# assumes model loaded into row during prior iteration
# assumes backet element history loaded during prior iteration
scorecard.table <- lapply(scorecard.table,function(scorecard.row) {
  model.name <- scorecard.row$id
  rv <- scorecard.row
  
  # skip rows without model defined
  # skip rows not activated
  if ( length(scorecard.row$model) > 0) {
    if ( scorecard.row$status == 'activated') {
      
      if ( getOption("verbose"))
        message(paste("Computing actuals for",model.name))
      model.basket <- scorecard.row$model$config$basket
      
      # clear the blotter account and portfolios for this model
      resetQuantstrat()
      initPortf(name = port.name, model.basket, initDate = init.date, currency = "USD")
      initAcct(name = acct.name,portfolios = c(port.name),initDate = init.date, initEq = init.eq)
      ignore <- sapply(model.basket, function(m) {
        stock(m, currency = "USD")
      })
      
      # final purge of transactions: eliminate non-basket transactions
      # add transactions to blotter
      transactions.df <- ti.df %>% dplyr::filter(ticker_symbol %in% model.basket)
      by(transactions.df, seq_len(nrow(transactions.df)), function(ti) {
        # switch returns a function having ti parameter
        rv <- switch(ti$ie.type,
                     "INSTRUMENT_COVER_SHORT_SALE" = function(ti) {
                       if ( getOption("verbose"))
                        message(paste("Ignoring option transaction", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_SHORT_SALE" = function(ti) {
                       warning(paste("Ignoring short sale", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_EXERCISE_BUY_OPTION" = function(ti) {
                       if ( getOption("verbose"))
                       message(paste("Ignoring option buy exercise", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_EXERCISE_SELL_OPTION" = function(ti) {
                       if ( getOption("verbose") )
                       message(paste("Ignoring option sell exercise"),
                               ti$date,
                               ti$ticker_symbol)
                     },
                     "INSTRUMENT_SPLIT" = function(ti) {
                       warning(paste("Ignoring instrument split", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_MERGER" = function(ti) {
                       warning(paste("Ignoring merger", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_SPINOFF" = function(ti) {
                       warning(paste("Ignoring spinoff", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_WASH_SALE" = function(ti) {
                       warning(paste("Ignoring wash sale", ti$date, ti$ticker_symbol))
                     },
                     "INSTRUMENT_BUY_COMMISSION" = function(ti) {
                       # amount is positive
                       addTxn(
                         Portfolio = port.name,
                         Symbol = ti$ticker_symbol,
                         TxnDate = ti$date,
                         TxnPrice = 0,
                         TxnQty = 0,
                         TxnFees = -ti$ie.amount,
                         verbose = getOption("verbose")
                       )
                     },
                     "INSTRUMENT_BUY" = function(ti) {
                       # count is positive, amount is positive
                       count <- as.numeric(ti$ie.count)
                       # recorded.price <- ti$ie.amount / count # transaction price not adjusted
                       ticker.xts <- get(ti$ticker_symbol)
                       adjusted.price <- Cl(ticker.xts[ti$date, ])
                       addTxn(
                         Portfolio = port.name,
                         Symbol = ti$ticker_symbol,
                         TxnDate = ti$date,
                         TxnPrice = adjusted.price,
                         TxnQty = count,
                         TxnFees = 0,
                         verbose = getOption("verbose")
                       )
                     },
                     "INSTRUMENT_SELL" = function(ti) {
                       # count is negative, amount is negative
                       count <- as.numeric(ti$ie.count)
                       # recorded.price <- ti$ie.amount / count
                       ticker.xts <- get(ti$ticker_symbol)
                       adjusted.price <- Cl(ticker.xts[ti$date, ])
                       addTxn(
                         Portfolio = port.name,
                         Symbol = ti$ticker_symbol,
                         TxnDate = ti$date,
                         TxnPrice = adjusted.price,
                         TxnQty = count,
                         TxnFees = 0,
                         verbose = getOption("verbose")
                       )
                     },
                     "INSTRUMENT_SELL_COMMISSION_AND_FEE" = function(ti) {
                       # amount is negative
                       addTxn(
                         Portfolio = port.name,
                         Symbol = ti$ticker_symbol,
                         TxnDate = ti$date,
                         TxnPrice = 0,
                         TxnQty = 0,
                         TxnFees = ti$ie.amount,
                         verbose = getOption("verbose")
                       )
                     },
                     "INSTRUMENT_DISTRIBUTION_CASH" = function(ti) {
                       # amount is positive; see ideas if reinstated
                       qty <- getPosQty(port.name, ti$ticker_symbol, ti$date)
                       if (qty > 0) {
                         if ( getOption("verbose") )
                          message(paste(
                             "Ignoring cash distribution",
                            ti$date,
                            ti$ticker_symbol,
                            dollar(ti$ie.amount),
                            qty
                         ))
                       } else {
                         # dividend received after having sold position
                         if ( getOption("verbose"))
                          message(paste(
                             "Ignoring cash distribution",
                            ti$date,
                            ti$ticker_symbol,
                            dollar(ti$ie.amount),
                            qty
                         ))
                       }
                     },
                     "INSTRUMENT_DISTRIBUTION_RETURN_OF_CAPITAL" = function(ti) {
                       # amount is negative; see ideas if reinstated
                       # adjusted close price already considers distributions
                       qty <- getPosQty(port.name, ti$ticker_symbol, ti$date)
                       amount <- abs(ti$ie.amount)
                       if (qty > 0) {
                         if ( getOption("verbose"))
                         message(
                           paste(
                             "Ignoring capital distribution",
                             ti$date,
                             ti$ticker_symbol,
                             dollar(amount),
                             qty
                           )
                         )
                       } else {
                         if ( getOption("verbose"))
                         message(paste(
                           "Ignoring capital distribution",
                           ti$date,
                           ti$ticker_symbol,
                           dollar(amount)
                         ))
                       }
                     },
                     function(ti) {
                       warning(paste("TI switch did not match", ti$ie.type))
                     }) # switch
        rv(ti)
      }) # by transaction
      
      lastDate <- xts::last(transactions.df)$date
      updatePortf(port.name)
      updateAcct(acct.name)
      updateEndEq(acct.name)
      
      # portfolio plot sanity check
      model.stats <- getAccount(acct.name)$portfolios$model
      
      # account plot sanity check
      account.summary <- getAccount(acct.name)$summary
      p1 <- plotModelStat(account.summary$End.Eq,paste(model.name, "Account Stats: Ending Equity"))
      
      # component and account returns
      pr <- PortfReturns(acct.name, Portfolios = port.name, period = "daily")
      colnames(pr) <- gsub(".DailyEndEq", "", colnames(pr))
      ar <- AcctReturns(acct.name)
      cr <- cumprod(1 + pr)
      
      # portfolio return status
      pm <- pr
      pm$Actual <- rowSums(pm, na.rm = TRUE)
      pm$Cumulative <- cumprod(1 + pm$Actual)
      
      # individual component position plots, saved as list, NA if no position
      # use plot(cplots[[1]]) to recover graphic
      cplots <- lapply(model.basket,function(mt){
        pf <- getPortfolio(port.name)
        position = pf$symbols[[mt]]$txn$Pos.Qty
        rv <- NA
        if (nrow(position) > 1)
          rv <- plotPosition(port.name, mt)
        return(rv)
      })
      
      # component return plots
      pr.df <- data.frame(pr) %>% mutate(Date = index(pr))
      gf <- pr.df %>% gather(Symbol, Return, -Date)
      p2 <- ggplot(gf, aes(x = Date, y = Return, color = Symbol)) +
        geom_line() +
        facet_wrap( ~ Symbol, nrow = 3, scales = "fixed") +
        xlab(NULL) +
        guides(color = FALSE) +
        ggtitle(paste(model.name, "Basket Element Returns"))
      p3 <- ggplot(gf, aes(x = Return, fill = Symbol, color = Symbol)) +
        geom_histogram(binwidth = 0.01) +
        geom_density() +
        guides(fill = FALSE, color = FALSE) +
        facet_wrap( ~ Symbol, nrow = 3, scales = "fixed") +
        ylab("Frequency") +
        xlab(paste("Daily Returns", min(gf$Date), "to", max(gf$Date), sep = ' ')) +
        ggtitle(paste(model.name, "Basket Element Return Distributions"))
      
      # component cumulative returns
      cr.df <- data.frame(cr) %>% mutate(Date = index(cr))
      gf <- cr.df %>% gather(Symbol, Return, -Date)
      p4 <- ggplot(gf, aes(x = Date, y = Return, color = Symbol)) +
        geom_line() +
        xlab(NULL) +
        ylab("Component Return") +
        guides(color = FALSE) +
        ggtitle(paste(model.name, "Model Component Cumulative Return"))
      p4 <- direct.label(p4)
      
      # trade stats
      stats <- tradeStats(port.name)
      stats <- formatTradeStats(stats)
      
      # model performance ratios
      annual.percent <- as.numeric(Return.annualized(pm$Actual)) * 100 # percent
      calmar.ratio <- as.numeric(CalmarRatio(pm$Actual)) # ratio
      sortino.ratio <- as.numeric(SortinoRatio(pm$Actual, MAR = 0)) # ratio
      max.drawdown.percent <- maxDrawdown(pm$Actual) * 100 # percent
      
      # store in scorecard row for later presentation
      scorecard.row$actual <- list(
        r = pr, 
        c = cr, 
        portfolio = pm,
        cagr = annual.percent,
        mdd = max.drawdown.percent,
        sortino = sortino.ratio,
        calmar = calmar.ratio,
        p1 = p1, p2 = p2, p3 = p3, p4 = p4,
        cplots = cplots, 
        stats = stats
      )

      rv <- scorecard.row
    }}
  
  return(rv)
}) # lapply

##########
# process out-of-sample results for every model
# re-fetch tickers for all models, ensuring sufficient history
# fetch start date should be oldest backtest stop date
backtestStops <- sapply(scorecard.table,function(sr){
  rv <- NA
  if ( length(sr$model) > 1 ) {
    rv <- sr$model$backtest$stop
  }
  return(rv)
})

oosStartDate <- min(backtestStops,na.rm=TRUE)

# re-query the tickers including benchmark
for ( ticker in c(scorecard.tickers,benchmark.symbol) ) {
  if ( getOption("verbose") ) 
    message(paste("Fetching", ticker))
  dx <- getSymbols(
    ticker,
    from = oosStartDate,
    index.class = c("POSIXt", "POSIXct"),
    warnings = FALSE,
    verbose = getOption("verbose")
  )
  if (has.Ad(dx)) {
    dx <- adjustOHLC(dx, use.Adjusted = TRUE) # adjust dividends, spinoffs, etc.
  }
  colnames(dx) <- gsub(paste(ticker, '.', sep = ''), "", colnames(dx))
  assign(ticker, dx, envir = .GlobalEnv) # put back into global environment
}

# save a copy of the scorecard basket for reset before each model run
scorecard.history <- new.env()
for ( ticker in scorecard.tickers) {
  assign(ticker,get(ticker),envir=scorecard.history)
}

# now run the models with data already acquired
# these should trim their use of the time series to actual OOS start date
scorecard.table <- lapply(scorecard.table,function(scorecard.row) {
  model.name <- scorecard.row$id
  rv <- scorecard.row
  if ( length(scorecard.row$model) > 1 ) {
    f <- scorecard.row$model$config[['function']]
    if ( ! is.null(f) ) {
      if ( str_length(f) > 0 ) {
        if ( getOption("verbose"))
          message(paste("Processing out-of-sample results for",model.name))
        updateModel <- match.fun(f) # may throw an error, untrapped here
        for ( ticker in scorecard.tickers ) {
          assign(ticker,get(ticker,envir=scorecard.history),envir=.GlobalEnv)
        }
        rv <- updateModel(scorecard.row)
      }
    }
  }
  
  return(rv)
})


##########
# results done, build the scorecard
# TODO extract listed data

# creates a portion of the scorecard from the definition and model files
insert.scorecard <- function(scg, group_name = "NA") {
  refresh <- today()
  scorecard.rv <- data.frame()
  for (id in scg) {
    if (id$id != "NA") {
      mc <- yaml.load_file(file.path("models",id$config))
      df <- data.frame(
        Status = group_name,
        ModelID = id$id,
        Owner = id$partner,
        Exp.OOS = mc$backtest$stop,
        Exp.CAGR = mc$backtest$cagr,
        Exp.MDD = mc$backtest$mdd,
        Exp.Sortino = mc$backtest$sortino,
        Exp.Calmar = mc$backtest$calmar,
        Data.Live = ifelse(is.null(id$live), mc$backtest$stop, id$live),
        Data.Refresh = refresh,
        Actual.CAGR = NA,
        Actual.MDD = NA,
        Actual.Sortino = NA,
        Actual.Calmar = NA,
        BuyHold.CAGR = NA,
        BuyHold.MDD = NA,
        BuyHold.Sortino = NA,
        BuyHold.Calmar = NA,
        Benchmark.CAGR = NA,
        Benchmark.MDD = NA,
        Benchmark.Sortino = NA,
        Benchmark.Calmar = NA,
        Location = file.path("models",id$config)
      )
      scorecard.rv <- bind_rows(scorecard.rv, df)
    }
  }
  return(scorecard.rv)
}


scorecard.df <- data.frame()
#scorecard.out <- bind_rows(scorecard.out,insert.scorecard(scorecard.activated, "Activated"))
#scorecard.out <- bind_rows(scorecard.out,insert.scorecard(scorecard.candidate, "Candidate"))
#scorecard.out <- bind_rows(scorecard.out,insert.scorecard(scorecard.deactivated, "Deactivated"))
#scorecard.out <- bind_rows(scorecard.out,insert.scorecard(scorecard.retired, "Retired"))
rownames(scorecard.df) <- scorecard.df$ModelID


# scorecard rankings for activated and candidate
actual.rank <- scorecard.df %>%
  dplyr::filter(Status %in% c('Activated', 'Candidate')) %>%
  mutate(CAGR.R = dense_rank(desc(Actual.CAGR))) %>%
  mutate(MDD.R = dense_rank(Actual.MDD)) %>%
  mutate(Calmar.R = dense_rank(desc(Actual.Calmar))) %>%
  mutate(Sortino.R = dense_rank(desc(Actual.Sortino))) %>%
  select(ModelID, CAGR.R, MDD.R, Calmar.R, Sortino.R)
rownames(actual.rank) <- actual.rank$ModelID
scorecard.ranked <-
  left_join(scorecard.df, actual.rank, by = 'ModelID')
scorecard.ranked[is.na(scorecard.ranked)] <- ''

formatted.scorecard <- formattable(scorecard.ranked,
                                   list(
                                     ModelID = formatter("span", 
                                                         style = x ~ style(background = "lightgray",
                                                                           color = "black")),
                                     Status = formatter("span", style = x ~ ifelse(
                                       x == "Activated",
                                       style(
                                         background = "green",
                                         color = "white",
                                         font.weight = "bold"
                                       ),
                                       ifelse(
                                         x == "Candidate",
                                         style(
                                           background = "yellow",
                                           color = "black",
                                           font.weight = "bold"
                                         ),
                                         ifelse(
                                           x == "Deactivated",
                                           style(
                                             background = "red",
                                             color = "white",
                                             font.weight = "bold"
                                           ),
                                           style(
                                             background = "blue",
                                             color = "white",
                                             font.weight = "bold"
                                           )
                                         )
                                       )
                                     ))
                                   ))
formatted.scorecard


# combined active model and buy-hold cumulative return
#xr <- merge(benchmark.returns, bhp$BuyHold, pm$Actual)
#p <- ggChartsPerformanceSummary2(xr,ptitle = paste(model.name, "vs.", "Buy-Hold Basket"))

