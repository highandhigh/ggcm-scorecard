#####################################################################
#' Read a scorecard update workspace and produce a scorecard artifact
#' @author mrb
#' Copyright (c) 2017 Great Gray Capital Management LLP
#####################################################################

source("scorecard_setup.R")


# find the workspace to use
# the workspace should have been written by scorecard_update
scorecard.workspace <- Sys.getenv(scorecard.workspace.ev)
if ( str_length(scorecard.workspace) > 0) {
  message("Using environment-specified workspace")
} else {
  message("Using latest scorecard workspace")
  # workspace not overridden in environment, so find in directory
  # find the scorecard workspace directory and load files
  scorecard.dir <- Sys.getenv(scorecard.dir.ev)
  if (str_length(scorecard.dir) < 1) {
    stop(paste("Scorecard output directory missing: set",scorecard.dir.ev))
  }
  
  # identify all workspaces in the directory, choose latest one
  scorecard.workspaces <- dir(scorecard.dir,pattern="scorecard_workspace",full.names=TRUE)
  scorecard.workspaces <- base::sort(scorecard.workspaces,decreasing=TRUE)
  scorecard.workspace <- scorecard.workspaces[1]
}

workspace <- new.env()
load(scorecard.workspace,envir=workspace)
scorecard.table <- get("scorecard.table",envir=workspace)

#[1] "%nin%"                          "acct.name"                      "aedf"                          
#[4] "AGG"                            "applyRank"                      "asdf"                          
#[7] "ave3ROC"                        "backtestStops"                  "benchmark.annual.percent"      
#[10] "benchmark.calmar.ratio"         "benchmark.cumulatives"          "benchmark.max.drawdown.percent"
#[13] "benchmark.returns"              "benchmark.sortino.ratio"        "benchmark.symbol"              
#[16] "bivio_tickers"                  "bivio.ev"                       "bivio.file"                    
#[19] "chart.Position"                 "checkBlotterUpdate"             "data.start"                    
#[22] "doc"                            "dx"                             "emit"                          
#[25] "etfReplayRank"                  "formatAccountSummary"           "formatPerTradeStats"           
#[28] "formatTradeStats"               "getAndAdjust"                   "ggCandles"                     
#[31] "ggChartSeries"                  "ggChartsPerformanceSummary2"    "ggChartsPerformanceSummary3"   
#[34] "GLD"                            "idf"                            "iedf"                          
#[37] "ignore"                         "ignore.tickers"                 "image.date"                    
#[40] "image.file"                     "init.date"                      "init.eq"                       
#[43] "ins"                            "medf"                           "model_update_rm_3roc"          
#[46] "model_update_rso20_roc"         "model.files"                    "monthlyAd"                     
#[49] "monthlyPrices"                  "monthlyReturns"                 "nm"                            
#[52] "oosStartDate"                   "plotModelStat"                  "plotPosition"                  
#[55] "port.name"                      "prettyStats"                    "refresh"                       
#[58] "resetQuantstrat"                "rowRank"                        "scorecard"                     
#[61] "scorecard.dir"                  "scorecard.dir.ev"               "scorecard.history"             
#[64] "scorecard.table"                "scorecard.tickers"              "SHY"                           
#[67] "SPY"                            "store.date"                     "strengthROC"                   
#[70] "strengthSMA"                    "switch.date"                    "symbolRank"                    
#[73] "tday"                           "tdf"                            "ti.df"                         
#[76] "ticker"                         "TLT"                            "tns"                           
#[79] "transaction.tickers"            "weightAve3ROC"                  "weightAve3ROCfilter"           
#[82] "weightAve3ROCstrict"            "XLE"                            "XLF"                           
#[85] "XLK"                            "XLP"                            "XLU"                           
#[88] "XLY"                            "xmltop"                    

message(paste("Using workspace",scorecard.workspace))


# load basic information into initial structure
# these fields are found in all table rows
scorecard.df <- bind_rows(
  sapply(seq_along(scorecard.table),function(i){
    r <- scorecard.table[[i]]
    mdf <- data.frame(
      Index=i,
      Status=r$status,
      ModelID=r$id,
      Partner=ifelse(is.null(r$partner),"",r$partner),
      stringsAsFactors = FALSE
    )
    rownames(mdf) <- r$id
    return(mdf)
  },simplify = FALSE)
)

# load expected performance from backtests
scorecard.df <- left_join(scorecard.df,
                          bind_rows(sapply(seq_along(scorecard.table),function(i){
                            r <- scorecard.table[[i]]
                            model <- r$model
                            edf <- data.frame(
                              Index=i
                            )
                            if ( length(model)>1 ) {
                              b<-model$backtest
                              edf <- edf %>%
                                mutate(Exp.CAGR=formatC(b$cagr,format='f',digits=1)) %>%
                                mutate(Exp.MDD=formatC(b$mdd,format='f',digits=1)) %>%
                                mutate(Exp.Sortino=formatC(b$sortino,format='f',digits=1)) %>%
                                mutate(Exp.Calmar=formatC(b$calmar,format='f',digits=1)) %>%
                                mutate(OOS.Date=b[['stop']])
                            }
                          },simplify=FALSE)
                          ),by='Index')

# load out-of-sample performance
scorecard.df <- left_join(scorecard.df,
                          bind_rows(sapply(seq_along(scorecard.table),function(i){
                            r <- scorecard.table[[i]]
                            oos <- r$oos
                            odf <- data.frame(
                              Index=i
                            )
                            if ( length(oos)>1 ) {
                              odf <- odf %>%
                                mutate(OOS.CAGR=formatC(oos$cagr,format='f',digits=1)) %>%
                                mutate(OOS.MDD=formatC(oos$mdd,format='f',digits=1)) %>%
                                mutate(OOS.Sortino=formatC(oos$sortino,format='f',digits=1)) %>%
                                mutate(OOS.Calmar=formatC(oos$calmar,format='f',digits=1))
                            }
                          },simplify=FALSE)
                          ),by='Index')

# load buy-hold performance
scorecard.df <- left_join(scorecard.df,
                          bind_rows(sapply(seq_along(scorecard.table),function(i){
                            r <- scorecard.table[[i]]
                            buyhold <- r$buyhold
                            bhdf <- data.frame(
                              Index=i
                            )
                            if ( length(buyhold)>1 ) {
                              bhdf <- bhdf %>%
                                mutate(BuyHold.CAGR=formatC(buyhold$cagr,format='f',digits=1)) %>%
                                mutate(BuyHold.MDD=formatC(buyhold$mdd,format='f',digits=1)) %>%
                                mutate(BuyHold.Sortino=formatC(buyhold$sortino,format='f',digits=1)) %>%
                                mutate(BuyHold.Calmar=formatC(buyhold$calmar,format='f',digits=1))
                            }
                          },simplify=FALSE)
                          ),by='Index')

# load benchmark performance
scorecard.df <- left_join(scorecard.df,
                          bind_rows(sapply(seq_along(scorecard.table),function(i){
                            r <- scorecard.table[[i]]
                            bhdf <- data.frame(
                              Index=i,
                              Bench.CAGR="",
                              Bench.MDD="",
                              Bench.Sortino="",
                              Bench.Calmar=""
                            )
                            if ( r$status != 'retired' ) {
                              bhdf <- bhdf %>%
                                mutate(Bench.CAGR=formatC(r$benchmark.cagr,format='f',digits=1)) %>%
                                mutate(Bench.MDD=formatC(r$benchmark.mdd,format='f',digits=1)) %>%
                                mutate(Bench.Sortino=formatC(r$benchmark.sortino,format='f',digits=1)) %>%
                                mutate(Bench.Calmar=formatC(r$benchmark.calmar,format='f',digits=1))
                            }
                            return(bhdf)
                          },simplify=FALSE)
                          ),by='Index')

# load actual performance
scorecard.df <- left_join(scorecard.df,
                          bind_rows(sapply(seq_along(scorecard.table),function(i){
                            r <- scorecard.table[[i]]
                            adf <- data.frame(
                              Index=i,
                              Actual.Live="",
                              Actual.CAGR="",
                              Actual.MDD="",
                              Actual.Sortino="",
                              Actual.Calmar=""
                            )
                            if ( r$status == 'activated' ) {
                              a<-r$actual
                              adf <- adf %>%
                                mutate(Actual.Live=r$live) %>%
                                mutate(Actual.CAGR=formatC(a$cagr,format='f',digits=1)) %>%
                                mutate(Actual.MDD=formatC(a$mdd,format='f',digits=1)) %>%
                                mutate(Actual.Sortino=formatC(a$sortino,format='f',digits=1)) %>%
                                mutate(Actual.Calmar=formatC(a$calmar,format='f',digits=1))
                            }
                            return(adf)
                          },simplify=FALSE)
                          ),by='Index')

# scorecard rankings for activated and candidate models
actual.rank <- scorecard.df %>%
  dplyr::filter(Status %in% c('activated')) %>%
  mutate(Actual.CAGR.R = dense_rank(desc(as.numeric(Actual.CAGR)))) %>%
  mutate(Actual.MDD.R = dense_rank(as.numeric(Actual.MDD))) %>%
  mutate(Actual.Calmar.R = dense_rank(desc(as.numeric(Actual.Calmar)))) %>%
  mutate(Actual.Sortino.R = dense_rank(desc(as.numeric(Actual.Sortino)))) %>%
  select(Index,Actual.CAGR.R,Actual.MDD.R,Actual.Calmar.R,Actual.Sortino.R)

oos.rank <- scorecard.df %>%
  dplyr::filter(Status %in% c('activated','candidate')) %>%
  mutate(OOS.CAGR.R = dense_rank(desc(as.numeric(OOS.CAGR)))) %>%
  mutate(OOS.MDD.R = dense_rank(as.numeric(OOS.MDD))) %>%
  mutate(OOS.Calmar.R = dense_rank(desc(as.numeric(OOS.Calmar)))) %>%
  mutate(OOS.Sortino.R = dense_rank(desc(as.numeric(OOS.Sortino)))) %>%
  select(Index,OOS.CAGR.R,OOS.MDD.R,OOS.Calmar.R,OOS.Sortino.R)

buyhold.rank <- scorecard.df %>%
  dplyr::filter(Status %in% c('activated','candidate','deactivated')) %>%
  mutate(BuyHold.Date = workspace[['init.date']]) %>%
  mutate(BuyHold.CAGR.R = dense_rank(desc(as.numeric(BuyHold.CAGR)))) %>%
  mutate(BuyHold.MDD.R = dense_rank(as.numeric(BuyHold.MDD))) %>%
  mutate(BuyHold.Calmar.R = dense_rank(desc(as.numeric(BuyHold.Calmar)))) %>%
  mutate(BuyHold.Sortino.R = dense_rank(desc(as.numeric(BuyHold.Sortino)))) %>%
  select(Index,BuyHold.Date,BuyHold.CAGR.R,BuyHold.MDD.R,BuyHold.Calmar.R,BuyHold.Sortino.R)

benchmark.rank <- scorecard.df %>%
  dplyr::filter(Status %in% c('activated','candidate','deactivated')) %>%
  mutate(Bench.Date = workspace[['init.date']]) %>%
  mutate(Bench.CAGR.R = dense_rank(desc(as.numeric(Bench.CAGR)))) %>%
  mutate(Bench.MDD.R = dense_rank(as.numeric(Bench.MDD))) %>%
  mutate(Bench.Calmar.R = dense_rank(desc(as.numeric(Bench.Calmar)))) %>%
  mutate(Bench.Sortino.R = dense_rank(desc(as.numeric(Bench.Sortino)))) %>%
  select(Index,Bench.Date,Bench.CAGR.R,Bench.MDD.R,Bench.Calmar.R,Bench.Sortino.R)

scorecard.df <- left_join(scorecard.df, actual.rank, by = 'Index')
scorecard.df <- left_join(scorecard.df, oos.rank, by = 'Index')
scorecard.df <- left_join(scorecard.df, buyhold.rank, by = 'Index')
scorecard.df <- left_join(scorecard.df, benchmark.rank, by = 'Index')
scorecard.df[is.na(scorecard.df)] <- ''

#################
# render graphics
#################

#' Creates a PNG file in the SCORECARD_DIR
create_png <- function(df,name="",title,file_date=workspace$image.date,fill_rank=TRUE) {
  file_name = file.path(scorecard.dir,paste0(file_date,'_',name,'.png'))
  png(file_name,width=650,height=25*nrow(df)+30,units="px")
  tg <- textGrob(title,gp=gpar(fontsize=24))
  pg <- tableGrob(df,rows=NULL)
  padding <- unit(5,"mm")
  
  # utility for finding cells
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }
  
  # table body
  table <- gtable_add_rows(pg,heights=grobHeight(tg) + padding,pos=0) 
  # title
  table <- gtable_add_grob(table,tg,1,1,1,ncol(table))
  # border around entire table
  table <- gtable_add_grob(table,
                           grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                           t = 2, b = nrow(table), l = 1, r = ncol(table))

  # dividers for status
  xdf <- df[,1:2]
  adf <- xdf %>% filter(Status=='activated')
  cdf <- xdf %>% filter(Status=='candidate')
  ddf <- xdf %>% filter(Status=='deactivated')
  rdf <- xdf %>% filter(Status=='retired')
  
  recolor_rank <- function(table,sdf,cell_fill,cell_alpha=1) {
    if ( nrow(sdf) > 0 )  {
      indices <- which(sdf==1,arr.ind = TRUE)
      for ( i in 2:nrow(indices) ) { # skips 1,1, which is Index column
        row <- as.numeric(indices[i,1]) + 2
        col <- as.numeric(indices[i,2]) 
        fc <- find_cell(table,row,col,"core-bg")
        table$grobs[fc][[1]][["gp"]] <- gpar(fill=cell_fill,
                                             alpha=cell_alpha)
      }
    }
    return(table)
  }
  
  recolor_status <- function(table,sdf,cell_fill,cell_fg="black",cell_border="darkgray",cell_alpha=1) {
    if ( nrow(sdf) > 0 )  {
      min_sdf <- min(sdf$Index,na.rm=TRUE) + 2
      max_sdf <- max(sdf$Index,na.rm=TRUE) + 2
      table <- gtable_add_grob(table,
                               grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                               t = min_sdf, b = max_sdf, l = 1, r = ncol(table))
      for ( i in min_sdf:max_sdf ) {
        fc <- find_cell(table,i,2,"core-bg")
        table$grobs[fc][[1]][["gp"]] <- gpar(fill=cell_fill, 
                                             col = cell_border, 
                                             alpha = cell_alpha,
                                             lwd=1)
        fc <- find_cell(table,i,2,"core-fg")
        table$grobs[fc][[1]]$gp$col <- cell_fg
      }
    }
    return(table)
  }

  # cell fills and section borders  
  table <- recolor_status(table,adf,"#1ec322",cell_alpha=1.0)
  table <- recolor_status(table,cdf,"#fedd32",cell_alpha=1.0)
  table <- recolor_status(table,ddf,"#fd696a",cell_fg="black",cell_alpha=1.0)
  table <- recolor_status(table,rdf,"#6496fc",cell_fg="white",cell_alpha=1.0)

  # rank one fills
  if ( fill_rank )
    table <- recolor_rank(table,df,"#1ec322",cell_alpha=0.8)
  
  # write it
  grid.newpage()
  grid.draw(table)
  dev.off()
}

# actual performance
gdf <- scorecard.df %>% select(
  Index,Status,ModelID,Partner,starts_with('Actual')
)
colnames(gdf) <- gsub('Actual.','',colnames(gdf))
gdf <- gdf %>% select(
  Index,Status,ModelID,Partner,Live,CAGR,CAGR.R,MDD,MDD.R,Calmar,Calmar.R,Sortino,Sortino.R
)
colnames(gdf) <- c("Index","Status","ID","Partner","Live","CAGR","R","MDD","R","Calmar","R","Sortino","R")
create_png(gdf,"sc_actual","Actual Performance")

# out of sample performance
gdf <- scorecard.df %>% select(
  Index,Status,ModelID,Partner,starts_with('OOS')
)
colnames(gdf) <- gsub('OOS.','',colnames(gdf))
gdf <- gdf %>% select(
  Index,Status,ModelID,Partner,Date,CAGR,CAGR.R,MDD,MDD.R,Calmar,Calmar.R,Sortino,Sortino.R
)
colnames(gdf) <- c("Index","Status","ID","Partner","Date","CAGR","R","MDD","R","Calmar","R","Sortino","R")
create_png(gdf,"sc_oos","Out-of-Sample Performance")

# buy-hold performance
# buy-hold starts on OOS date
gdf <- scorecard.df %>% select(
  Index,Status,ModelID,Partner,starts_with('BuyHold')
)
colnames(gdf) <- gsub('BuyHold.','',colnames(gdf))
gdf <- gdf %>% select(
  Index,Status,ModelID,Partner,Date,CAGR,CAGR.R,MDD,MDD.R,Calmar,Calmar.R,Sortino,Sortino.R
)
colnames(gdf) <- c("Index","Status","ID","Partner","Date","CAGR","R","MDD","R","Calmar","R","Sortino","R")
create_png(gdf,"sc_buyhold","Buy-Hold Basket Performance")

# benchmark performance
gdf <- scorecard.df %>% select(
  Index,Status,ModelID,Partner,starts_with('Bench')
)
colnames(gdf) <- gsub('Bench.','',colnames(gdf))
gdf <- gdf %>% select(
  Index,Status,ModelID,Partner,Date,CAGR,CAGR.R,MDD,MDD.R,Calmar,Calmar.R,Sortino,Sortino.R
)
colnames(gdf) <- c("Index","Status","ID","Partner","Date","CAGR","R","MDD","R","Calmar","R","Sortino","R")
create_png(gdf,"sc_benchmark","Benchmark Performance")

# eof



