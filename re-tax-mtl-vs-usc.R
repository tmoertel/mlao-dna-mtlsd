## Analysis of historical and forecasted property taxes collected by
## the Mt. Lebanon, Pennsylvania, School District as compared to
## Upper St. Clair.
##
## Analyis by Tom Moertel <tom@mlao.org> based on original analysis
## by James Fraasch.
##
## 2009-10-11
##
## This analysis is an R program:  http://www.r-project.org/


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
