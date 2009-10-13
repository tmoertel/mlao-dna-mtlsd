## Inflation adjustment relative to the CPI-U.
##
## Tom Moertel <tom@mlao.org>
## 2009-10-11


## First I load CPI data from the BLS.

cpi <- read.csv("http://spreadsheets.google.com/pub?key=tRdVRikb23tPzs1u-STyV4w&single=true&gid=0&output=csv", skip=3)


## Next, because I want to use the CPI to adjust our data for
## inflation, I will extrapolate the index out to the year 2014, which
## is as far at the MTLSD's forecasts go.

cpi.model <- lm(CPI ~ Year, data = cpi)
cpi.predictions <- local({
  cpi.predictions <- data.frame(Year=2009:2014)
  within(cpi.predictions, { CPI <- predict(cpi.model, cpi.predictions) })
})

cpi.extended <- rbind(cpi, cpi.predictions)

mk.inflation.adjuster <- function(base.year) {
  offset <- 1 - min(cpi.extended$Year)
  cpi <- cpi.extended$CPI
  base.index <- cpi[base.year + offset]
  function(year) base.index/cpi[year + offset]
}

## Example use:
##
## adjustment.2008 <- mk.inflation.adjuster(2008)
##
##
## all.data <- transform(all.data,
##                       Adjusted.Valuation.In.2008.Dollars =
##                           Assessed.Valuation * adjustment.2008(Year))
