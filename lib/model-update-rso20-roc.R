#########################################################################
# Copyright (C) 2017 GGCM LLP                                           #
#########################################################################

#' RSO switching model out-of-sample results
#' @author mrb
#' @description Run and report update for given model configuration
#' @return scorecard row with out-of-sample list element attached
#' 
#' @note Does not yet do rebalancing
#' @note Does not yet do trailing stops

model_update_rso20_roc <- function(scorecard.row) {

  model <- scorecard.row$model
  study.title <- model$model
  # start.date <- model$backtest$start
  enact.date <- model$backtest$stop # end of in-sample is start of out-of-sample
  stop.date <- Sys.Date() # out-of-sample stop is today
  initial.eq <- model$backtest$initeq
  top.N <- model$config$topn
  trail.stop.percent <- model$config$trailstop
  transaction.fee <- model$backtest$transaction
  basket <- model$config$basket
  alpha.fast <- model$config$smoothing[1]
  alpha.slow <- model$config$smoothing[2]
  rebalance.freq <- model$config$rebalance
  benchmarks <- c("SPY")
  
  # trim basket to enact date
  for ( ticker in c(basket,benchmarks) ) {
    dx <- get(ticker,envir=.GlobalEnv)
    dx <- dx[paste0(enact.date,'::',stop.date),]
    assign(ticker,dx,envir=.GlobalEnv)
  }

  # create an xts object of daily adjusted close prices
  basket.close.monthly <- monthlyPrices(basket)
  benchmarks.close.monthly <- monthlyPrices(benchmarks)
  colnames(basket.close.monthly) <- basket
  colnames(benchmarks.close.monthly) <- benchmarks
  
  basket.ratio <- do.call(merge,lapply(basket,function(s) basket.close.monthly[,s] / benchmarks.close.monthly[,1]))
  
  basket.relative.strength <- do.call(merge,lapply(basket,function(s) basket.ratio[,s] / as.numeric(basket.ratio[1,s])))
  basket.slow <- do.call(merge,lapply(basket,function(s) EMA(basket.relative.strength[,s],n=2,ratio=alpha.slow)))
  basket.fast <- do.call(merge,lapply(basket,function(s) EMA(basket.relative.strength[,s],n=2,ratio=alpha.fast)))
  basket.slow[1,] <- 1
  basket.fast[1,] <- 1
  colnames(basket.slow) <- paste(basket,"Slow",sep='.')
  colnames(basket.fast) <- paste(basket,"Fast",sep='.')
  basket.oscillator <- 100 * (basket.fast / basket.slow - 1)
  colnames(basket.oscillator) <- paste(basket,"RSO",sep='.')
  
  basket.rank <- ifelse(basket.oscillator > 0, basket.oscillator, NA)
  basket.rank <- na.fill(rowRank(basket.rank),length(basket))
  colnames(basket.rank) <- gsub(".RSO","",colnames(basket.rank))
  
  # last six months ranking by component
  df <- tail(basket.rank, n=6)
  #textplot( df )
  #title("Basket Component Ranking")
  
  # last 24 months ranking plot  
  basket.rank.df <- as.data.frame(basket.rank)
  basket.rank.df$Date <- as.Date(rownames(basket.rank.df))
  basket.rank.df <- tail(basket.rank.df,n=24) # last 24 months
  dfg <- gather(basket.rank.df,Symbol,Value,1:(ncol(df)-1))
  basket.rank.p <- ggplot(dfg,aes(x=Date,y=Value)) +
    facet_grid(Symbol~.) +
    geom_step(color="blue") +
    scale_y_reverse(breaks=c(1,3,5,7,9),labels=c("1","3","5","7","9")) +
    ggtitle("RSO Ranking by Fund") +
    ylab("RSO Value (1 is Highest)") +
    xlab(NULL) +
    geom_hline(yintercept=top.N,linetype="dashed",color="darkgreen")

  
  # returns and performance starting enact date
  prices <- NULL
  for (symbol in basket)
    prices <- cbind(prices,Cl(get(symbol)))
  colnames(prices) <- basket
  returns <- diff(log(prices))[-1, ]
  components <- returns[paste0(enact.date,"::"),]
  #basket.comp.p <- ggChartsPerformanceSummary2(components,
  #                ptitle="RM20 RSO Basket Component Performance")
  #basket.comp.p

  # bind the columns to the appropriate symbol market data
#   for(i in 1:length(basket)) {
#     # build the combined monthly indicators 
#     z <- basket.ratio[,i]
#     colnames(z) <- "Ratio"
#     z$RS <- coredata(basket.relative.strength[,i])
#     z$Slow <- coredata(basket.slow[,i])
#     z$Fast <- coredata(basket.fast[,i])
#     z$RSO <- coredata(basket.oscillator[,i])
#     z$Rank <- coredata(basket.rank[,i])
#     
#     # insert the monthly indicators into daily OHLC data
#     x <- get(basket[i])
#     x <- na.locf(cbind(x,z))
#     assign(basket[i],x)
#   }

  yr <- ceiling(max(abs(basket.relative.strength)))
  df <- data.frame(coredata(basket.relative.strength),
                   Date=index(basket.relative.strength))
  dfg <- gather(df,Symbol,Value,1:(ncol(df)-1))
  
  # RS in separate panels
  p1 <- ggplot(dfg,aes(x=Date,y=Value)) +
    facet_grid(Symbol~.,scales="free_y") +
    geom_line() +
    ggtitle(paste("Relative Strength",
                  "Market", benchmarks[1], sep=' - ')) +
    xlab(NULL) + ylab("Relative Strength") +
    geom_hline(yintercept=1,linetype="dashed",color="darkgreen")
  #plot(p1)
  
  # RS overlaid
  p2 <- ggplot(dfg,aes(x=Date,y=Value,color=Symbol)) +
    geom_line() +
    ggtitle(paste("Relative Strength",
                  "Market", benchmarks[1], sep=' - ')) +
    xlab(NULL) + ylab("Relative Strength") +
    geom_hline(yintercept=1,linetype="dashed",color="darkgreen")
  p2 <- direct.label(p2)
  
  # RSO
  colnames(basket.oscillator) <- gsub(".RSO","",colnames(basket.oscillator))
  
  yr <- ceiling(max(abs(basket.oscillator)))
  df <- data.frame(coredata(basket.oscillator),
                   Date=index(basket.oscillator))
  dfg <- gather(df,Symbol,Value,1:(ncol(df)-1))

  # RSO in separate panels
  p3 <- ggplot(dfg,aes(x=Date,y=Value)) +
    facet_grid(Symbol~.,scales="free_y") +
    geom_line() +
    ggtitle(paste("Relative Strength Oscillator",
                  "Market", benchmarks[1], sep=' - ')) +
    xlab(NULL) + ylab("Relative Strength Oscillator") +
    geom_hline(yintercept=0,linetype="dashed",color="darkgreen")
  #plot(p)
  
  # RSO overlaid
  p4 <- ggplot(dfg,aes(x=Date,y=Value,color=Symbol)) +
    geom_line() +
    ggtitle(paste("Relative Strength Oscillator",
                  "Market", benchmarks[1], sep=' - ')) +
    xlab(NULL) + ylab("Relative Strength Oscillator") +
    geom_hline(yintercept=0,linetype="dashed",color="darkgreen")
  # plot(p)
  p4 <- direct.label(p4)
  
  
  # recreate transactions
  # clear the blotter account and portfolios
  resetQuantstrat()
  
  # setup blotter account and portfolio
  acct.name <- "rso.acct"
  port.name <- "rso.port"
  acct.date <- as.Date(enact.date)-1
  initPortf(name=port.name, basket, initDate=acct.date, currency="USD")
  initAcct(name=acct.name, portfolios=c(port.name), initDate=acct.date, initEq=initial.eq)
  
  # setup blotter instruments
  for ( mt in basket) {
    stock(mt,currency="USD",multiplier=1)
  }
  
  action.df <- basket.rank[paste0(enact.date,'::'),]
  colnames(action.df) <- gsub(".RSO","",colnames(action.df))
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
      psp <- suppressWarnings(to.monthly(get(ps),indexAt="lastof"))
      psp <- as.numeric(Cl(psp[rank.date,]))
      pos <- as.numeric(getPos(Portfolio=port.name,Symbol=ps,Date=rank.date,Columns="Pos.Qty",n=1))
      message(paste(rank.date,"sell position",ps,pos,"shares","at",dollar(psp)))
      addTxn(Portfolio=port.name,
             Symbol=ps,
             TxnDate=rank.date,
             TxnPrice=psp,
             TxnQty=(-pos),
             TxnFees=transaction.fee,
             verbose=getOption("verbose"))
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
        tsp <- suppressWarnings(to.monthly(get(ts),indexAt="lastof"))
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
               verbose=getOption("verbose"))
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
  #final.eq <- getEndEq(acct.name,stop.date) + initial.eq
  
  pr <- PortfReturns(acct.name)
  colnames(pr) <- gsub(".DailyEndEq","",colnames(pr))
  pr <- pr[paste0(enact.date,'::'),]
  pr$Theoretical <- rowSums(pr)
  pc <- cumprod(1+pr)
  
  p5 <- ggChartsPerformanceSummary2(pr$Theoretical,
                              ptitle=paste(study.title,"(Theoretical)"),
                              drawdown.minima = "gray")
  
  annual.percent <- as.numeric(Return.annualized(pr$Theoretical)) * 100 # percent
  calmar.ratio <- as.numeric(CalmarRatio(pr$Theoretical)) # ratio
  sortino.ratio <- as.numeric(SortinoRatio(pr$Theoretical,MAR=.1/12)) # ratio
  max.drawdown.percent <- maxDrawdown(pr$Theoretical) * 100 # percent
  
  
  scorecard.row$oos <- list(
    r=pr$Theoretical,
    c=pc,
    component.returns=components,
    cagr=annual.percent,
    calmar=calmar.ratio,
    sortino=sortino.ratio,
    mdd=max.drawdown.percent,
    rank=basket.rank,
    rank.p=basket.rank.p,
    plots=c(p1,p2,p3,p4,p5)
  ) 
  
  return(scorecard.row)
}

