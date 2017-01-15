
##########
# results done, build the scorecard
# TODO extract listed data


# creates a portion of the scorecard from the definition and model files
insert.scorecard <- function(scg, group_name = "NA") {
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

