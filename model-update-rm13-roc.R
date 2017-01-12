#########################################################################
# Copyright (C) 2017 GGCM LLP                                           #
#########################################################################

#' @title RM13 3ROC actuals
#' @author mrb
#' @description Run and report update for fixed model configuration

model_update_rm13_roc <- function() {
  invisible(suppressPackageStartupMessages(lapply(c("quantmod",
                                                    "PerformanceAnalytics",
                                                    "ggplot2",
                                                    "dplyr",
                                                    "tidyr",
                                                    "scales",
                                                    "directlabels",
                                                    "quantstrat"
  ),
  require,
  warn.conflicts=FALSE,
  character.only=TRUE
  )))
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
  
  `%nin%` <- Negate(`%in%`) 
  
  # special environment for theoretical prices
  # model.env <- new.env() # problems with quanstrat doing this
  model.env <- .GlobalEnv
  
  # get symbol data, adjust close, trim history, save to global environment
  getAndAdjust <- function(ticker,init_date) {
    message(paste("Fetching",ticker))
    dx <- getSymbols(ticker,
                     from=init_date,
                     index.class=c("POSIXt","POSIXct"),
                     warnings=FALSE,
                     verbose=FALSE,
                     auto.assign = FALSE,
                     env=model.env)
    if ( has.Ad(dx) ) {
      dx <- adjustOHLC(dx,use.Adjusted=TRUE) # adjust dividends, spinoffs, etc.
    }
    # dx <- dx[paste(switch_date,"::",sep=''),] # trim prehistorical data
    colnames(dx) <- gsub(paste(ticker,'.',sep=''),"",colnames(dx))
    assign(ticker,dx,envir=model.env) # put back into global environment
  }
  
  study.title <- "Rank Model 13p: ROC3E"
  start.date <- "2004-12-01"
  enact.date <- "2016-07-31"
  stop.date <- Sys.Date()
  initial.eq <- 250000
  top.N <- 3
  #max.levels <- 1
  #trail.stop.percent <- 0.00
  transaction.fee <- -4.95 # per ETF order each way
  periods <- c(1,2,3)
  weights <- c(0.4,0.3,0.3)
  basket <- c("XLY", # 1998-12-22
               "XLP", # 1998-12-22
               "XLE", # 1998-12-22
               "XLK", # 1998-12-22
               "XLU", # 1998-12-22
               "XLF", # 1998-12-22
               "AGG", # 2003-09-26
               "TLT", # 2002-07-30
               "GLD" # 2004-11-18 limiter
  )
  
  for ( symbol in basket ) {
    ignore <- getAndAdjust(symbol,start.date)
  }
  
  # create an xts object of daily adjusted close prices
  basket.close.monthly <- monthlyPrices(basket,env=model.env)
  colnames(basket.close.monthly) <- basket
  
  # create an xts object of the symbol ranks
  sym.rank <- applyRank(x=basket.close.monthly, 
                        rankFun=weightAve3ROC,
                        n=periods, 
                        weights=weights)
  sym.rank <- na.fill(sym.rank,fill=ncol(sym.rank)) # ensure trading rules exit
  colnames(sym.rank) <- gsub(".Adjusted", ".Rank", colnames(sym.rank))
  stopifnot(all.equal(gsub(".Adjusted", "", colnames(basket.close.monthly)), basket))
  
  # bind the rank column to the appropriate symbol market data
  for(i in 1:length(basket)) {
    x <- get(basket[i],envir=model.env)
    y <- na.locf(cbind(x,sym.rank[,i]))
    y <- y[,ncol(y)]
    x <- cbind(x,y,join="left")
    assign(basket[i],x,envir=model.env)
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
  
  # returns and performance starting enact date
  prices <- NULL
  for (symbol in basket)
    prices <- cbind(prices,Cl(get(symbol,envir=model.env)))
  colnames(prices) <- basket
  returns <- diff(log(prices))[-1, ]
  components <- returns[paste0(enact.date,"::"),]
  #basket.comp.p <- ggChartsPerformanceSummary2(components,
  #                                ptitle="RM13 3ROC Basket Component Performance")
  #basket.comp.p
  
  # recreate transactions
  # clear the blotter account and portfolios
  resetQuantstrat()
  
  # setup blotter account and portfolio
  verbose <- TRUE
  acct.name <- "3roc.acct"
  port.name <- "3roc.port"
  acct.date <- as.Date(enact.date)-1
  initPortf(name=port.name, basket, initDate=acct.date, currency="USD")
  initAcct(name=acct.name, portfolios=c(port.name), initDate=acct.date, initEq=initial.eq)
  
  # setup blotter instruments
  FinancialInstrument::currency("USD")
  for ( mt in basket) {
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
    
    # sell old positions
    for ( ps in previous.symbols[which(previous.symbols %nin% top.symbols )]) {
      psp <- suppressWarnings(to.monthly(get(ps,envir=model.env),indexAt="lastof"))
      psp <- as.numeric(Cl(psp[rank.date,]))
      pos <- as.numeric(getPos(Portfolio=port.name,Symbol=ps,Date=rank.date,Columns="Pos.Qty",n=1))
      message(paste(rank.date,"sell position",ps,pos,"shares","at",dollar(psp)))
      addTxn(Portfolio=port.name,
             Symbol=ps,
             TxnDate=rank.date,
             TxnPrice=psp,
             TxnQty=(-pos),
             TxnFees=transaction.fee,
             verbose=verbose)
      updatePortf(port.name)
      updateAcct(acct.name)
      updateEndEq(acct.name)
    }
    
    # portfolio equity to date
    port.eq <- getEndEq(acct.name,as.character(rank.date)) + initial.eq
    
    for ( ts in top.symbols ) {
      if ( ts %in% previous.symbols ) {
        message(paste(rank.date,"already hold",ts))
      } else {
        tsp <- suppressWarnings(to.monthly(get(ts,envir=model.env),indexAt="lastof"))
        tsp <- as.numeric(Cl(tsp[rank.date,])) # price
        tsi <- port.eq / top.N # investment
        tss <- round(tsi / tsp,0) # shares
        message(paste(rank.date,"buy position",ts,tss,"shares","at",dollar(tsp)))
        addTxn(Portfolio=port.name,
               Symbol=ts,
               TxnDate=rank.date,
               TxnPrice=tsp,
               TxnQty=tss,
               TxnFees=transaction.fee,
               verbose=verbose)
        updatePortf(port.name)
        updateAcct(acct.name)
        updateEndEq(acct.name)
      }
    }
    previous.symbols <- top.symbols
  }
  
  # mark the book and get final equity
  updatePortf(port.name)
  updateAcct(acct.name)
  updateEndEq(acct.name)
  final.eq <- getEndEq(acct.name,stop.date) + initial.eq
  
  pr <- PortfReturns(acct.name)
  colnames(pr) <- gsub(".DailyEndEq","",colnames(pr))
  pr <- pr[paste0(enact.date,'::'),]
  pr$Theoretical <- rowSums(pr)
  #ggChartsPerformanceSummary2(pr$Theoretical,
  #                            ptitle=paste(study.title,"(Theoretical)"),
  #                            drawdown.minima = "gray")
  
  annual.percent <- as.numeric(Return.annualized(pr$Theoretical)) * 100 # percent
  calmar.ratio <- as.numeric(CalmarRatio(pr$Theoretical)) # ratio
  sortino.ratio <- as.numeric(SortinoRatio(pr$Theoretical,MAR=0)) # ratio
  max.drawdown.percent <- maxDrawdown(pr$Theoretical) * 100 # percent
  
  return(list(theoretical.returns=pr$Theoretical,
              component.returns=components,
              ar=annual.percent,
              cr=calmar.ratio,
              sr=sortino.ratio,
              mdp=max.drawdown.percent,
              final.eq=final.eq,
              rank.p=sym.rank.p,
              title=study.title
  ))
}

