getwd()
setwd('~/All about R')

df <- read.delim('gapminderDataFiveYear.tsv')

subset(df, continent == 'Africa')

qplot(x = gdpPercap, y = lifeExp,
      data = df, 
      binwidth =0.01, 
      geom = 'freqpoly')


)) +
  geom_point() + scale_x_log10()


## https://daattali.gitbooks.io/stat545-ubc-github-io/content/block020_multiple-plots-on-a-page.html



getwd()
setwd('~/All_about_R')

getwd()

dataf <- read.delim('gapminderDataFiveYear.tsv')

library(ggplot2)
names(dataf)
qplot(x = gdpPercap, data = dataf)
ggsave('gdpPercapBoxplot.png')

library(ggplot2)
names(dataf)
qplot(x = gdpPercap, data = dataf, binwidth = 500) +
  scale_x_continuous(limits = c(0, 50000), breaks = seq(0, 10000, 1000))

table(dataf$country)
by(dataf$gdpPercap, dataf$country, summary)

qplot(x = lifeExp, data = dataf, binwidth = 1,
      color = I('black'), fill = I('#099009'))
ggsave('lifeExp_color_Boxplot.png')

library(ggplot2)
names(dataf)
qplot(x = lifeExp, data = dataf,
      color = I('black'), fill = I('#099009')) +
      facet_wrap(~continent, ncol = 5)  
ggsave('lifeExp_color_BoxplotperContinent.png')

getwd()
setwd('~/All_about_R')
df <- read.delim('gapminderDataFiveYear.tsv')
 names(df)

 qplot(x = continent, data = df,
       xlab = 'Continents',
       ylab = 'Number of Countries in Sample',
       color = I('black'), fill = I('#F79420'))
 scale_x_log10()
 ggsave('ContinentsBoxplot.png')
 
 df <- read.delim('gapminderDataFiveYear.tsv')
 #changes
 
 
 

 
 
 
 
 
