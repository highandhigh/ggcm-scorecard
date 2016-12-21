#' Pretty-print trade stats, all rows, four columns
#' @param name order book name for trade stats
prettyStats <- function(name) {
  require(blotter)
  x <- tradeStats(name)
  x <- cbind(x[,1:4],format(x[,5:30],big.mark=',',digits=2))
}

# faber.stats<-tradeStats('faber')[,c('Net.Trading.PL','Max.Drawdown','Num.Trades','Profit.Factor','Std.Dev.Trade.PL','Largest.Winner','Largest.Loser','Max.Equity','Min.Equity')]

formatPerTradeStats <- function(pts) {
  require(scales,quietly=TRUE)
  pts$Init.Pos <- comma(pts$Init.Pos)
  pts$Max.Pos <- comma(pts$Max.Pos)
  pts$Num.Txns <- comma(pts$Num.Txns)
  pts$Max.Notional.Cost <- dollar(pts$Max.Notional.Cost)
  pts$Net.Trading.PL <- dollar(pts$Net.Trading.PL)
  pts$MAE <- dollar(pts$MAE)
  pts$MFE <- dollar(pts$MFE)
  pts$Pct.Net.Trading.PL <- percent(pts$Pct.Net.Trading.PL)
  pts$Pct.MAE <- percent(pts$Pct.MAE)
  pts$Pct.MFE <- percent(pts$Pct.MFE)
  pts$tick.Net.Trading.PL <- prettyNum(pts$tick.Net.Trading.PL,digits=2)
  pts$tick.MAE <- formatC(pts$tick.MAE,digits=2,format='f')
  pts$tick.MFE <- formatC(pts$tick.MFE,digits=2,format='f')
  return(pts)
}


#' Formats the dollars and commas for blotter trade statistics.
#' @param trade statistics object to be modified
#' @return trade statistics object
formatTradeStats <- function(ts) {
  require(scales,quietly=TRUE)
  whole_dollar <- dollar_format(largest_with_cents = 1, negative_parens = TRUE)
  ts$Avg.Daily.PL <- whole_dollar(ts$Avg.Daily.PL)
  ts$Avg.Losing.Trade <- whole_dollar(ts$Avg.Losing.Trade)
  ts$Avg.Trade.PL <- whole_dollar(ts$Avg.Trade.PL)
  ts$Avg.Win.Trade <- whole_dollar(ts$Avg.Win.Trade)
  ts$End.Equity <- whole_dollar(ts$End.Equity)
  ts$Gross.Profits <- whole_dollar(ts$Gross.Profits)
  ts$Gross.Losses <- whole_dollar(ts$Gross.Losses)
  ts$Largest.Winner <- whole_dollar(ts$Largest.Winner)
  ts$Largest.Loser <- whole_dollar(ts$Largest.Loser)
  ts$Max.Drawdown <- whole_dollar(ts$Max.Drawdown)
  ts$Max.Equity <- whole_dollar(ts$Max.Equity)
  ts$Med.Daily.PL <- whole_dollar(ts$Med.Daily.PL)
  ts$Med.Losing.Trade <- whole_dollar(ts$Med.Losing.Trade)
  ts$Med.Trade.PL <- whole_dollar(ts$Med.Trade.PL)
  ts$Med.Win.Trade <- whole_dollar(ts$Med.Win.Trade)
  ts$Min.Equity <- whole_dollar(ts$Min.Equity)
  ts$Net.Trading.PL <- whole_dollar(ts$Net.Trading.PL)
  ts$Std.Dev.Trade.PL <- whole_dollar(ts$Std.Dev.Trade.PL)
  ts$Std.Dev.Daily.PL <- whole_dollar(ts$Std.Dev.Daily.PL)
  ts$Percent.Positive <- percent(ts$Percent.Positive/100)
  ts$Percent.Negative <- percent(ts$Percent.Negative/100)
  ts$Profit.Factor <- prettyNum(ts$Profit.Factor,digits=2)
  ts$Ann.Sharpe <- prettyNum(ts$Ann.Sharpe,digits=2)
  ts$Profit.To.Max.Draw <- prettyNum(ts$Profit.To.Max.Draw,digits=2)
  ts$Avg.WinLoss.Ratio <- prettyNum(ts$Avg.WinLoss.Ratio,digits=2)
  ts$Med.WinLoss.Ratio <- prettyNum(ts$Med.WinLoss.Ratio,digits=2)
  return(ts)
}

formatAccountSummary <- function(acct) {
  require(scales,quietly=TRUE)
  asdf <- as.data.frame(acct$summary)
  for ( i in 1:ncol(asdf)) {
    asdf[,i] <- dollar(asdf[,i])
  }
  return(asdf)
}