#!/usr/bin/Rscript

##=============================================================================
##=============================================================================
## Analysis of historical and forecasted real-estate taxes collected by
## the Mt. Lebanon, Pennsylvania, School District.
##
## Analyis by Tom Moertel <tom@mlao.org>.
## 2009-10-11
##
## This analysis is an R program:  http://www.r-project.org/
##=============================================================================
##=============================================================================


require("ggplot2")


##=============================================================================
## Historical property-tax data -- I entered this data by hand from a
## scanned document (source given below) and uploaded it as a Google
## Spreadsheet (URL given below). --Tom
##=============================================================================

## Source:  Mt. Lebanon 2007 Manager's Recommended Budget, "Millage History"
## Source:  http://spreadsheets.google.com/pub?key=t390h-L8SL9ucn3UKJ1-IUQ&single=true&gid=0&output=csv

google_ss_url <- function(key) {
  paste("http://spreadsheets.google.com/pub?key=",
        key, "&single=true&gid=0&output=csv", sep="")
}

as.numeric.with.commas <- function(x) as.numeric(gsub(",", "", x))

historicals <- read.csv(google_ss_url("t390h-L8SL9ucn3UKJ1-IUQ"), skip=3)
historicals[,2] <- sapply(historicals[,2], as.numeric.with.commas, simplify=T)
historicals <- transform(historicals,
                         Assess = ifelse(Year < 2001, 'Pct.025', 'Pct.100'),
                         o.Year = Year - 2001,  # years since 2001 reassesment
                         Trend = "Actual")


##=============================================================================
## More-recent and forecasted MTLSD taxes -- Again, entered by hand from
## source documents, cited below.
##=============================================================================

## The following data are more-recent actual data drawn from the
## MTLSD's 2009-10 budget.
## Source: http://www.mtlsd.org/district/budget/200910budget.asp

recent.data <- data.frame(
    Year = 2008:2009,
    o.Year = 2008:2009 - 2000,
    Assessed.Valuation = c(2148911690, 2167740391),
    School.Mills = c(23.81, 24.11),
    Assess = "Pct.100",
    Trend = "Actual"
)

## The following data represent the MTLSD's forecasted property tax rates
## under the assumption that the district will not make any large changes
## such as renovating the high school or increasing its pension-fund
## contribution rate.
## Source: http://www.mtlsd.org/district/budget/stuff/budget%20forecastsnochangeapril162009.pdf

forecast.data.as.is <- data.frame(
    Year = 2010:2014,
    o.Year = 2010:2014 - 2000,
    Assessed.Valuation = rep(2167740391, 5),
    School.Mills = c(24.72, 25.69, 26.56, 27.69, 28.61),
    Assess = "Pct.100",
    Trend = "Forecast: as is"
)

## The following data represent the MTLSD's forecasted property tax rates
## under the assumption that the district will renovate the high school.
## Source: http://www.mtlsd.org/district/budget/stuff/budgetforecastshsdebtapril162009.pdf

forecast.data.hs <- data.frame(
    Year = 2010:2014,
    o.Year = 2010:2014 - 2000,
    Assessed.Valuation = rep(2167740391, 5),
    School.Mills = c(26.21, 27.25, 28.97, 30.49, 31.40),
    Assess = "Pct.100",
    Trend = "Forecast: +HS"
)

## The following data represent the MTLSD's forecasted property tax rates
## under the assumptions that the district will renovate the high school
## and that the district will be required to increase pension funding.
## Source: http://www.mtlsd.org/district/budget/stuff/budgetforecastspsersandhsdebtapril16%202009.pdf
## Background: http://lebosbupdates.blogspot.com/2008/12/psers-contribution-rates-set-to.html

forecast.data.hs.psers <- data.frame(
    Year = 2010:2014,
    o.Year = 2010:2014 - 2000,
    Assessed.Valuation = rep(2167740391, 5),
    School.Mills = c(26.58, 27.92, 31.77, 33.76, 34.98),
    Assess = "Pct.100",
    Trend = "Forecast: +HS +PSERS"
)

## Now I combine the historical, present, and forecasted data into
## an all-encompassing data set.

all.data <- rbind(historicals[,c("Year", "o.Year", "Assessed.Valuation",
                                 "School.Mills", "Assess", "Trend")],
                  recent.data,
                  forecast.data.as.is,
                  forecast.data.hs,
                  forecast.data.hs.psers)

## Using the combined data set, I plot the district's property-tax
## trends -- historical, current, and forecasted, with and without
## the high-school renovation and with and without PSERS increases.
## The resulting chart provides a visual summary of the district's
## spending (and taxing) trends.

p.hist.and.forecasted.taxes <-
qplot(Year, School.Mills * Assessed.Valuation / 1000,
      data = all.data,
      shape = Assess, colour = Trend,
      main = paste(
        "Historical and Forecasted Property Taxes",
        "Collected For Mt. Lebanon School District",
        sep="\n"),
      xlab = "School Year",
      ylab = expression(textstyle(
          "Total Property Taxes (= Total Assessed Valuation") %*%
          textstyle("Millage Rate)"))
      ) +
  stat_smooth(method="lm", se=F) +   # add trend lines
  scale_colour_hue(breaks=c(
                     "Forecast: +HS +PSERS",
                     "Forecast: +HS",
                     "Forecast: as is",
                     "Actual"),
                   labels=c(
                     "Forecast: +HS +PSERS",
                     "Forecast: +HS",
                     "Forecast: no changes",
                     "Historical")) +
  scale_shape(name="Taxation Method",
                 breaks=c("Pct.100", "Pct.025"),
                 labels=c(
                   "Taxed on 100% assessment",
                   "Taxed on 25% assessment")) +
  scale_y_continuous(formatter="dollar") +
  scale_x_continuous(breaks=seq(1990, 2010, 5),
                     labels=c("1990/1", "1995/6",
                              "2000/1", "2005/6",
                              "2010/1"))
print(p.hist.and.forecasted.taxes)

## I save the chart in PDF and PNG formats.

ggsave("mtlsd-hist-and-forecasted-re-taxes.pdf",
       p.hist.and.forecasted.taxes, width = 10, height = 7.5)

ggsave("mtlsd-hist-and-forecasted-re-taxes.png",
       p.hist.and.forecasted.taxes, width = 8, height = 6, dpi = 100)



##=============================================================================
## Trend models -- Now I want to quantify the district's spending
## growth trends.  I use linear models of the following form:
##
##   spending = y2001_baseline_spending + (growth_rate + trend_factor) * year
##
## where trend_factor is a discrete variable that identifies a
## specific trend such as pre- vs. post-2001 reassessment or
## no-changes vs. high-school renovation.
##=============================================================================


## In the first model, I compare the school district's rate of
## spending growth before and after the county-wide 2001 property-tax
## reassesment: before, spending increased by $1.2 million per year;
## after, by $1.6 million per year.
##
##
## Trend                        Rate by which spending grows
##
## Before reassessment          $1.2 million / year
## After reassessment           $1.6 million / year  (+35% wrt before)

model.1 <- lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year * Assess,
              data = all.data, subset = Year <= 2009)
summary(model.1)

## In the second model, I compare the school district's historical
## post-reassesment spending growth with its forecasted spending
## growth under different scenarios.  Under the district's
## "no-big-changes" scenario, the rate of spending growth increases to
## $2.1 million per year; with the high school renovation, the growth
## rate increases to $3.0 million per year; with the renovation and
## PSERS increases, the rate jumps to $4.9 million per year:
##
##
## Trend                        Rate by which spending grows
##
## Historical baseline          $1.6 million / year
## Forecast: no big changes     $2.1 million / year  (+ 37% wrt baseline)
## Forecast: high school renov. $3.0 million / year  (+ 90% wrt baseline)
## Forecast: h.s.r. and PSERS   $4.9 million / year  (+215% wrt baseline)

model.1.forecasted <-
  lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year * Trend,
     data = all.data, subset = Year >= 2001)
summary(model.1.forecasted)
