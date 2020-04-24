## DCF

## https://www.r-bloggers.com/discounted-expectations/

## https://www.r-bloggers.com/tidy-discounted-cash-flow-analysis-in-r-for-company-valuation/

## https://osm.netlify.app/post/mean-expectations/

## https://www.r-bloggers.com/the-performance-of-small-value-stocks-in-bear-markets/


## https://www.r-bloggers.com/tidy-discounted-cash-flow-analysis-in-r-for-company-valuation/
## detailed DCF analysis.

## https://www.r-bloggers.com/tidy-discounted-cash-flow-analysis-in-r-for-company-valuation/


library(xts)
library(quantmod)
library(tidyquant)
library(tidyverse)
library(ggplot2)
library(TTR)


sp <- getSymbols("^GSPC", src = "yahoo", from = "1950-01-01", to = "2020-01-01",
                 auto.assign = FALSE) %>%
  Ad() %>%
  `colnames<-`("sp")

sp

sp_qtr <- to.quarterly(sp, indexAt = "lastof", OHLC = FALSE)

class(sp_qtr)

timetk::tk_tbl(sp_qtr,rename_index = "Date")

gdp <- getSymbols("GDP", src = "FRED", from = "1950-01-01", to = "2020-01-01",
                  auto.assign = FALSE) %>%
  `colnames<-`("gdp")

plot(gdp)
plot(log(gdp))


summary(sp)

to.quarterly(s)
qtr = index(to.quarterly(sp["1950/2019"], indexAt = 'lastof', OHLC = FALSE))

gdp_eop <- xts(coredata(gdp["1950/2019"]), order.by = qtr) # change to end-of-period

merged <- merge(sp_qtr, gdp_eop)
plot(merged)
plot(log(merged))

head(merged)

qtr_returns <- TTR::ROC(merged, n=1, type = 'discrete')[-1]
(1+colMeans(qtr_returns))^4-1

f <- function(x) round(exp(mean(x, na.rm = TRUE)*4)-1, 3)*100

apply(merged, 2, f())

apply(qtr_returns, 2, function(x) round(exp(mean(x, na.rm = TRUE)*4)-1, 3)*100)

qtr_returns
chart.CumReturns(qtr_returns)
