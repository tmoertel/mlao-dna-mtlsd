## Analysis of historical and forecasted property taxes collected by
## the Mt. Lebanon, Pennsylvania, School District.
##
## Analyis by Tom Moertel <tom@mlao.org>.
## 2009-10-11
##
## This analysis is an R program:  http://www.r-project.org/

require("ggplot2")


## Historical Tax Data
##
## Source:  Mt. Lebanon 2007 Manager's Recommended Budget, "Millage History"
## http://spreadsheets.google.com/pub?key=t390h-L8SL9ucn3UKJ1-IUQ&single=true&gid=0&output=csv

google_ss_url <- function(key) {
  paste("http://spreadsheets.google.com/pub?key=",
        key, "&single=true&gid=0&output=csv", sep="")
}

as.numeric.with.commas <- function(x) as.numeric(gsub(",", "", x))

historicals <- read.csv(google_ss_url("t390h-L8SL9ucn3UKJ1-IUQ"), skip=3)
historicals[,2] <- sapply(historicals[,2], as.numeric.with.commas, simplify=T)
historicals <- transform(historicals,
                         Assess = ifelse(Year < 2001, 'Pct.025', 'Pct.100'),
                         o.Year = Year - 2001,
                         Trend = "Actual")

## Add more-recent data from MTLSD final and forecasted budgets
## http://www.mtlsd.org/district/budget/stuff/budget%20forecastsnochangeapril162009.pdf
## http://www.mtlsd.org/district/budget/200910budget.asp


recent.data <- data.frame(
    Year = 2008:2014,
    o.Year = 2008:2014 - 2000,
    Assessed.Valuation = c(2148911690, rep(2167740391, 6)),
    School.Mills = c(23.81, 24.11, 24.72, 25.69, 26.56, 27.69, 28.61),
    Assess = rep('Pct.100', 7),
    Trend = c(rep('Actual', 2), rep('Forecast: as is', 5))
)

forecast.data.hs <- data.frame(
    Year = 2010:2014,
    o.Year = 2010:2014 - 2000,
    Assessed.Valuation = rep(2167740391, 5),
    School.Mills = c(26.21, 27.25, 28.97, 30.49, 31.40),
    Assess = 'Pct.100',
    Trend = 'Forecast: +HS'
)

forecast.data.hs.psers <- data.frame(
    Year = 2010:2014,
    o.Year = 2010:2014 - 2000,
    Assessed.Valuation = rep(2167740391, 5),
    School.Mills = c(26.58, 27.92, 31.77, 33.76, 34.98),
    Assess = 'Pct.100',
    Trend = 'Forecast: +HS +PSERS'
)

all.data <- rbind(historicals[,c("Year", "o.Year", "Assessed.Valuation",
                                 "School.Mills", "Assess", "Trend")],
                  recent.data,
                  forecast.data.hs,
                  forecast.data.hs.psers)

all.data <- transform(all.data,
                      Assess = reorder(Assess, -order(Assess)),
                      Trend = reorder(Trend, -order(Trend))
                      )

p.hist.and.forecasted.taxes <-
qplot(Year, School.Mills * Assessed.Valuation / 1000,
      data = all.data,
      linetype = Assess, colour = Trend,
      main = paste(
        "Historical and Forecasted Property Taxes",
        "Collected For Mt. Lebanon School District",
        sep="\n"),
      xlab = "School Year",
      ylab = expression(textstyle(
          "Total Property Taxes (= Total Assessed Valuation") %*%
          textstyle("Millage Rate)"))
      ) + stat_smooth(method="lm", se=F) +
  scale_linetype(name="Taxation Method",
                 labels=c(
                   "Taxed on 100% assessment",
                   "Taxed on 25% assessment")) +
  scale_y_continuous(formatter="dollar") +
  scale_x_continuous(breaks=seq(1990, 2010, 5),
                     labels=c("1990/1", "1995/6",
                              "2000/1", "2005/6",
                              "2010/1"))
print(p.hist.and.forecasted.taxes)

ggsave("mtl-hist-and-forecasted-prop-taxes.pdf",
       p.hist.and.forecasted.taxes, width = 10, height = 7.5)

ggsave("mtl-hist-and-forecasted-prop-taxes.png",
       p.hist.and.forecasted.taxes, width = 8, height = 6, dpi = 100)




## Linear models

model.1 <- lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year * Assess,
              data = all.data, Year <= 2009)
summary(model.1)

model.1.forecasts <-
  lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year * Trend,
     data = all.data, subset = Year >= 2001)
summary(model.1.forecasts)
