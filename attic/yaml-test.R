library(yaml)
library(dplyr)
library(magrittr)

sc <- yaml.load_file("scorecard.yaml")
scc <- sc$table

for ( i in scc  ) {
  iss <- i$status
  ism <- bind_rows(i$models)
  
  print(iss)
  
  if ( nrow(ism) > 0 )  {
    for ( r in seq_len(nrow(ism)) ) {
      dfr <- ism[r,]
      model <- yaml.load_file(dfr$config)
      history <- bind_rows(model$history)
      config <- model$config
      backtest <- model$backtest
      print(paste("OS",backtest$stop))
      print(history)
      print("ranked sortino")
      sr.df <- history %>% 
        mutate(SR=rank(sortino)) %>% 
        arrange(desc(sortino))
      print(sr.df)
    }
  }
}

