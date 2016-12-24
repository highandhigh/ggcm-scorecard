# GGCM Scorecard

These products provide partnership portfolio scorecard and model tracking automation.  The main program performs the following steps:

1. opens and reads a previously-downloaded copy of the partnership data from Bivio
1. filters the Bivio data to recover only the transaction history
1. filters the transaction history to start model data collection in August 2016
1. collects relevant tickers and downloads ticker price history from Yahoo!
1. recreates a price-informed price and trade history for returns analysis
1. performs returns analysis by account and by model
1. produces reports
1. produces scorecard

## Scorecard Format

The scorecard target format is specified in `scorecard.yaml`.  

## Model Format

The model definitions are specified in `models/*.yaml' files. 




