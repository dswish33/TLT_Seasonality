install.packages("tidyverse")
install.packages("quantmod")
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(quantmod)

#function to retrieve price data
get_ohlc <- function(symbol, start_date, end_date) {
  
  daf <- getSymbols(symbol, src = 'yahoo', auto.assign = FALSE, from = start_date, to = end_date)

  colnames(daf) <- c("Open", "High", "Low", "Close", "Volume", "Adj. Close")
  
  ohlc_df <- as.data.frame(daf)
  
  return(ohlc_df)
}

tlt <- get_ohlc("TLT", "2007-06-01", "2023-06-01")
 
#plot adjusted close price data
ggplot(tlt, aes(x = index(tlt), y = tlt$`Adj. Close`)) + geom_line() + ggtitle("TLT Adjusted Close") +
  labs(x = "Date", y = "Adj. Close Price")

#get log returns
tlt <- tlt %>%
  mutate(log_returns = log(Close/lag(Close)))

#get day of month for each row
tlt <- tlt %>%
  mutate(dom = lubridate::day(rownames(tlt))) %>%
  na.omit()

#calculate mean/median returns by day of month
dom_returns <- tlt %>%
  group_by(dom) %>%
  summarize(
    mean_log_returns = mean(log_returns),
    median_log_returns = median(log_returns)
  )

#plot mean/median returns by day of month
dom_returns %>%
  ggplot(aes(x=dom, y = mean_log_returns)) +
    geom_bar(stat = "identity") +
      ggtitle("Mean TLT Returns by Day of Month")

dom_returns %>%
  ggplot(aes(x=dom, y = median_log_returns)) +
    geom_bar(stat = "identity") +
      ggtitle("Median TLT Returns by Day of Month")

#grouping equally in 5 'quantiles'
tlt_years <- tlt %>%
  ungroup() %>%
  mutate(year = lubridate::year(rownames(tlt)),
         date_group = ntile(rownames(tlt), 5)
  )

tlt_years %>%
  group_by(date_group) 

dom_grouped <- tlt_years %>%
  na.omit() %>%
  group_by(date_group, dom) %>%
  summarize(
    mean_log_returns = mean(log_returns),
    median_log_returns = median(log_returns)
  )

#plot mean log returns by day of month for each of the 5 time periods
dom_grouped %>%
  ggplot(aes(x=dom, y = mean_log_returns)) +
    geom_bar(stat = "identity") +
    facet_wrap(~date_group) +
    ggtitle("Mean Log Returns by Day of Month for Five Equal Periods of Time")

#visualizing a long/short strategy over time
#short first 5 days and long last 5 days of each month, do nothing in between
switch <- 5
tlt$date <- as.Date(rownames(tlt))

tlt %>%
  mutate(start_of_month = lubridate::floor_date(date, "month")) %>%
  mutate(strat = case_when(dom <= switch ~ -1, dom >= 31-switch ~ 1, TRUE ~ 0) * log_returns) %>%
  group_by(start_of_month) %>%
  summarize(strat = sum(strat)) %>%
  ggplot(aes(x=start_of_month, y = strat)) +
    geom_bar(stat = "identity") +
    ggtitle("Monthly TLT Returns BOM vs. EOM")

#variables to store log returns for first 5 days and last 5 days of each month in dataset   
bom_returns <- tlt[tlt$dom >= 1 & tlt$dom <= 5, "log_returns" ]
eom_returns <- tlt[tlt$dom >= 26 & tlt$dom <= 31, "log_returns"]

#difference in means t-test
t.test(bom_returns, eom_returns)

#Conclusion:
#Data is historical and thus not indicative of current market regimes.
#However, with such a low p-value, we reject the null hypothesis and accept the alternative
#that there is a significant difference in the means of returns for TLT at the beginning 
#of each month and at the end of each month. The strong statistical findings are backed by 
#further data analysis of separate time period groupings which show that the phenomena 
#existed during different regimes over time. The explanation for such a phenomena is not 
#known and this analysis was done only to practice in-depth data analysis, R code, and
#explore potentially profitable trading strategies.

