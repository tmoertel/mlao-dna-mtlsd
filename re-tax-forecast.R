require("ggplot2")

millages <- read.csv("mtl-usc-forecasted-millage.csv")

melted.millages <- melt(millages, "year", variable_name = "district")


qplot(year, value, data = melted.millages,
      geom = "line",
      group = district,
      colour = district)


qplot(year, mtl.millage / usc.millage, data = millages, geom = "line")

p <-
qplot(year, 200 * (mtl.millage - usc.millage), data = millages, geom = "line",
      main = paste(
        "How much more a $200K home will be taxed",
        "in Mt. Lebanon than in Upper St. Clair",
        sep = "\n"),
      xlab = "School Year (Start)",
      ylab = paste(
        "Forecasted annual tax premium",
        "for living in Mt. Lebanon",
        sep = "\n")) +
  scale_y_continuous(formatter = "dollar")

ggsave("mtl-tax-penalty-vs-usc.pdf", p, width = 5, height = 5, dpi = 72)
ggsave("mtl-tax-penalty-vs-usc.png", p, width = 5, height = 5, dpi = 100)


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
                         o.Year = Year - 2001)

## Add more-recent data from MTLSD final and forecasted budgets
## http://www.mtlsd.org/district/budget/stuff/budget%20forecastsnochangeapril162009.pdf
## http://www.mtlsd.org/district/budget/200910budget.asp
historicals[c(23:29),]$Year <- 2008:2014
historicals[c(23:29),]$o.Year <- 2008:2014 - 2000
historicals[c(23:29),]$Assessed.Valuation <- c(2148911690, rep(2167740391, 6))
historicals[c(23:29),]$School.Mills <- c(23.81, 24.11, 24.72, 25.69, 26.56, 27.69, 28.61)
historicals[c(23:29),]$Assess <- rep('Pct.100', 7)
historicals$Kind <- c(rep('Actual', 25), rep('Forecast', 4))
rownames(historicals) <- 1:29

qplot(Year, School.Mills * Assessed.Valuation / 1000,
      data = subset(historicals),
      colour = Assess, linetype = Kind,
      ) + stat_smooth(method="lm", se=F)


model.1 <- lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year * Assess,
              data = historicals)
summary(model.1)

model.1.actuals <-
  lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year,
     data = historicals, subset = Kind == "Actual" & Year >= 2001)
summary(model.1.actuals)

model.1.forecasts <-
  lm(School.Mills * Assessed.Valuation / 1000 ~ o.Year,
     data = historicals, subset = Kind == "Forecast")
summary(model.1.forecasts)
