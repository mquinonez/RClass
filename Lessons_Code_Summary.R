install.packages('gridExtra')
library(gridExtra)

p1 <- qplot(x = friend_count, data = pseudo_facebook)
p2 <- qplot(x = log10(friend_count + 1), data = pseudo_facebook)
p3 <- qplot(x = sqrt(friend_count), data = pseudo_facebook)

grid.arrange(p1, p2, p3,ncol=1)

?gridExtra()

install.packages('gridExtra')
library(gridExtra)

p1 <- qplot(x = friend_count, data = pseudo_facebook)
p2 <- qplot(x = log10(friend_count + 1), data = pseudo_facebook)
p3 <- qplot(x = sqrt(friend_count), data = pseudo_facebook)

grid.arrange(p1, p2, p3,ncol = 1)

#Transforming Data Alternate Solution
## Use scales!
p1 <- ggplot(aes(x = friend_count), data = pseudo_facebook) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3,ncol = 1)

logscale <- qplot(x = log10(friend_count), data = pseudo_facebook)

countScale <- ggplot(aes(x = friend_count), data = pseudo_facebook) +
  geom_histogram() +
  scale_x_log10()

grid.arrange(logscale, countScale, ncol = 2)

qplot(x = friend_count, data = pseudo_facebook) +
  scale_x_log10()

#Frequency Polygons (before we had histograms)
qplot(x = friend_count, data = subset(pseudo_facebook, !is.na(gender)),
      binwidth = 10) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0, 1000, 50)) +
  facet_wrap(~gender)

qplot(x = friend_count, y = data = subset(pseudo_facebook, !is.na(gender)),
      binwidth = 10, geom = 'freqpoly', color = gender) +
  scale_x_continuous(lim = c(0, 1000), breaks = seq(0,1000,50))

#Equivalent ggplot syntax:
ggplot(aes(x = friend_count, y = ..count../sum(..count..)),
       data = subset(pseudo_facebook, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) +
  xlab('Friend Count') +
  ylab('Proportion of users with that friend count')

#Equivalent ggplot syntax for solution video:
  ggplot(aes(x = www_likes), data = subset(pseudo_facebook, !is.na(gender))) +
  geom_freqpoly(aes(color = gender)) +
    scale_x_continuous(lim = c(0, 1000), breaks = seq(0,1000,50))
  
  #Frequency Polygons Solution
qplot(x = www_likes, data = subset(pseudo_facebook, !is.na(gender)),
        geom = 'freqpoly', color = gender) +
    scale_x_continuous() +
    scale_x_log10()
  

#How many likes - using by function 
 by(pseudo_facebook$www_likes, pseudo_facebook$gender, sum)
 
 ## Box Plots
 qplot(x = gender, y = friend_count,
       data = subset(pseudo_facebook, !is.na(gender)),
       geom = 'boxplot', ylim = c(0, 1000))
 
 qplot(x = gender, y = friend_count,
       data = subset(pseudo_facebook, !is.na(gender)),
       geom = 'boxplot') +
   scale_y_continuous(limits = c(0, 1000))
 
 qplot(x = gender, y = friend_count,
       data = subset(pseudo_facebook, !is.na(gender)),
       geom = 'boxplot') +
   coord_cartesian(ylim = c(0, 250))
 
 by(pseudo_facebook$friend_count, pseudo_facebook$gender, summary)
 
 
 names(pseudo_facebook)
 
 by(pseudo_facebook$friendships_initiated, pseudo_facebook$gender, summary)
 
 qplot(x = gender, y = friendships_initiated,
       data = subset(pseudo_facebook, !is.na(gender)),
       geom = 'boxplot') +
   coord_cartesian(ylim = c(0, 150))
 
 ## Getting Logical
 summary(pseudo_facebook$mobile_likes)
 
 summary(pseudo_facebook$mobile_likes > 0)
 
 mobile_check_in <- NA
 pseudo_facebook$mobile_check_in <- ifelse(pseudo_facebook$mobile_likes > 0, 1, 0)
 pseudo_facebook$mobile_check_in <- factor(pseudo_facebook$mobile_check_in)
 summary(pseudo_facebook$mobile_check_in)
 
summary(pseudo_facebook$mobile_check_in)
sum(pseudo_facebook$mobile_check_in == 1)/length(pseudo_facebook$mobile_check_in)

data("diamonds")

summary(diamonds)  

?diamonds

diamonds$color

qplot(x = price, data = diamonds)

ggplot(aes(x = price), data = subset(diamonds)

subset(diamonds, price < 250)

subset(diamonds, price > 15000 | price == 15000)

table(diamonds$price > = 15000)



qplot(x = price, data = diamonds) + binwidth = 10) +
scale_x_continuous(lim = c(0, 500), breaks = seq(0, 1000, 50))

qplot(x = price, data = diamonds, binwidth = 5000)
ggsave('priceHistogram.png')

rlang::last_error()

grid.arrange(p1, p2, p3,ncol = 1)

p1 <- ggplot(aes(x = cut), data = diamonds) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3,ncol = 1)

p1 <- ggplot(aes(x = price), data = diamonds) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3,ncol = 1)

p1 <- ggplot(aes(x = price), data = diamonds) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3,ncol = 1)

p1 <- ggplot(aes(x = cut, y = price, data = subset(diamonds) + geom_histogram()
                 p2 <- p1 + scale_x_log10()
                 p3 <- p1 + scale_x_sqrt()
                 
grid.arrange(p1, p2, p3,ncol = 1)


qplot(x = cut, data = diamonds, binwidth = 30,
      xlab = 'Diamond Cut',
      ylab = 'Diamond Price',
      fill = I('#5760ab')) +
  scale_x_discrete(breaks = seq(0, 113,5))

p1 <- ggplot(aes(x = cut, y = price, data = subset(diamonds) + geom_histogram()
                 p2 <- p1 + scale_x_log10()
                 p3 <- p1 + scale_x_sqrt()
                 p4 <- p1 + scale_x_log10()
                 p5 <- p1 + scale_x_sqrt()
                 
grid.arrange(p1, p2, p3,ncol = 1)
                 
                 
library(gridExtra)

p1 <- qplot(x = cut, data = diamonds)
p2 <- qplot(x = as.numeric (cut), data = diamonds)
p3 <- qplot(x = as.numeric (cut), data = diamonds)
p4 <- qplot(x = as.numeric (cut), data = diamonds)
p5 <- qplot(x = as.numeric (cut), data = diamonds)

p1 <- ggplot(aes(x = cut, y = price, data = subset(diamonds) + geom_histogram()
                 p2 <- p1 + as.numeric()
                 p3 <- p1 + as.numeric()
                 p4 <- p1 + as.numeric()
                 p5 <- p1 + as.numeric()

grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

summary(as.numeric(diamonds$cut))

library(gridExtra)

p1 <- qplot(x = cut, data = diamonds)
p2 <- qplot(x = as.factor (c(100, 10), data = diamonds)
p3 <- qplot(x = as.factor (c(100, 10), data = diamonds)
p4 <- qplot(x = as.factor (c(100, 10), data = diamonds)
p5 <- qplot(x = as.factor (c(100, 10), data = diamonds)

p1 <- ggplot(aes(x = cut, y = price, data = subset(diamonds) + geom_histogram()
                 p2 <- p1 + as.factor()
                 p3 <- p1 + as.factor()
                 p4 <- p1 + as.factor()
                 p5 <- p1 + as.factor()
                 
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)

summary(diamonds$price by cut)

str(diamonds)
diamonds$cut <-factor(diamonds$cut, labels = c("Fair", "Good", "Very Good", "Premium", "Ideal"))

tapply(diamonds$cut, diamonds$price, summary)

qplot(x = price, data = diamonds) + facet_wrap(~cut)


table (diamonds$cut: "Fair")

by(diamonds$price, diamonds$cut, summary)

qplot(x = price/carat, data = diamonds) + facet_wrap(~cut)+ 
  scale_x_log10()

by(diamonds$cut, diamonds$carat, summary)

ggplot(aes(x = carat), data = subset(diamonds)) +
  geom_histogram(aes(facet_wrap(~cut) +
  scale_x_log10()
  
qplot(x = price, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 50) + 
    scale_x_continuous(limits= c(0,2400), breaks=seq(0,2400,200))


qplot(x =price, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 600) +
  facet_wrap(~cut, scales="free_y")

qplot(x =price/carat, data = diamonds, color = I('black'), fill = I('#099DD9'), binwidth = 600) +
  facet_wrap(~cut, scales="free_y") 

by(diamonds$price,diamonds$clarity,summary, digits = max(getOption('digits')))


qplot(x = clarity, y = price,
      data = subset(diamonds),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 8000))

qplot(x = clarity, y = price, data = diamonds, geom = "boxplot") + 
  coord_cartesian(ylim = c(0,7000))

qplot(x = color, y = price,
      data = subset(diamonds),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(700, 7500))


qplot(x = cut, y = price,
      data = subset(diamonds),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 9000))

by(diamonds$price,diamonds$color,summary)

IQR(subset(diamonds, price <7695)$price)

qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot")
ggsave('Price_CaratBoxplot.png'
       
qplot(x = color, y = price/carat, data = diamonds, geom = "freqpoly")

qplot(x = carat, 
      data = diamonds, 
      binwidth =0.01, 
      geom = 'freqpoly') + 
  scale_x_continuous(lim = c(0,5), breaks = seq(0,5,0.5))

by(diamonds$price, diamonds$carat,summary)

head(subset(diamonds, select = 'carat'))

library(plyr)
count(diamonds, 'carat', diamonds$price > 2000)

by(diamonds$price,diamonds$carat, summary, digits = max(getOption('digits')))

table(cut(diamonds[,1], pretty(diamonds[,1]))

summary(foreign_direct_investment_net_inflows_percent_of_gdp)            
      
??foreign_direct_investment_net_inflows_percent_of_gdp

qplot(x = country, y = 2017, data = foreign_direct_investment_net_inflows_percent_of_gdp, geom = "boxplot") +
  coord_cartesian(ylim = TRUE)

t(foreign_direct_investment_net_inflows_percent_of_gdp)

qplot(x = aruba, y = 1969, data = foreign_direct_investment_net_inflows_percent_of_gdp)

ggplot(aes(x = "Yemen", y = "1970"), data = foreign_direct_investment_net_inflows_percent_of_gdp))

t(foreign_direct_investment_net_inflows_percent_of_gdp)
summary(log10(foreign_direct_investment_net_inflows_percent_of_gdp))
summary(foreign_direct_investment_net_inflows_percent_of_gdp)

qplot(x = country, 
      data = foreign_direct_investment_net_inflows_percent_of_gdp, 
      binwidth =0.01, 
      geom = 'freqpoly')

ggplot(aes(x = country), data = subset(foreign_direct_investment_net_inflows_percent_of_gdp))


ggplot(aes(x = 1975), data = subset(foreign_direct_investment_net_inflows_percent_of_gdp)) +
      geom_histogram(aes(facet_wrap(~country)
                     
library(ggplot2)
gDat <- read.delim("gapminderDataFiveYear.tsv")
str(gDat)                     
 
getwd()                    
summary(gapminderDataFiveYear.tsv)

??gapminderDataFiveYear.tsv

gapminderDataFiveYear.tsv[ROWS, COLUMNS]
