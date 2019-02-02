getwd()
#setwd("C:/Users/Abraham/Documents/BED-2056-Introduction-to-Data-Scenice")
rm(list=ls()) # removes all objects from the environment
suppressPackageStartupMessages(require(pacman))
p_load(mosaic,highcharter,quantmod,moments,PerformanceAnalytics,
       ggthemes,xts,zoo,ggthemes,tidyverse,lubridate,tsibble,ggplot2)


date.format <- (format ="%Y%m%d") #setting a date format

USD_NOK<- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=USDNOK.FXSX&csv_format=csv",
                   col_types = cols(quote_date = col_date(date.format))) %>% 
                select(quote_date, close)
names(USD_NOK) <-c("date", "USD_NOK")
#loading EQNR DATA,selecting close prices
EQNR <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=EQNR.OSE&csv_format=csv",
                 col_types = cols(quote_date = col_date(date.format))) %>% 
                  select(quote_date, close)

names(EQNR) <- c("date", "Equinor") #renaming our columns
glimpse(EQNR)
EQNR <- xts(EQNR[,-1], order.by = EQNR$date)
EQNR <-EQNR["2014-01-30/"] #Extracting last five years data

#repeating the process with market data
OSEBX<-read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=OSEBX.OSE&csv_format=csv",
                col_types = cols(quote_date = col_date(date.format))) %>%
                select(quote_date, close)
names(OSEBX) <- c("date", "OSEBX") # naming 

OSEBX <- xts(OSEBX[,-1], order.by = OSEBX$date) # converting into an xts file
OSEBX <- OSEBX["2014-01-30/"]
# lastly our 3 year T-bond
rf <- read_csv("https://www.netfonds.no/quotes/paperhistory.php?paper=STATSOBL-3Y-EFF.NB&csv_format=csv",
               col_types = cols(quote_date = col_date(date.format))) %>%
                select(quote_date, close)
names(rf) <- c("date","rf") #naming
rf<-xts(rf[,-1], order.by = rf$date) # converting to xts
rf <- rf["2014-01-30/"] #extracting dates


# simple visualization of Equinor  
chartSeries(EQNR, name = "EQNR 2014-2019")
barChart(EQNR, subset = "last 6 month", name = "Prices of the past 6 month")

# calculating returns and removing NAs

rEQNR<-diff(log(EQNR$Equinor)) %>% na.omit() 
#lowest/ highest return values and their dates
rEQNR[which.min(rEQNR)]
rEQNR[which.max(rEQNR)]
sd(rEQNR)*sqrt((252)) #sigma.annulized

rOSEBX <- diff(log(OSEBX$OSEBX)) %>% na.omit() # index returns,& then deleting NA 

#calculating the risk free rate
rf <- rf$rf/252
rf <- rf/100
head(rf,3)
#mergging data
rMerged <- merge.xts(rEQNR,rOSEBX)
head(rMerged)
# several descriptiv statistics calculating codes
summary(rMerged)
table.Distributions(rMerged)
table.Stats(rMerged)
            #sigma.annualized
sd(rMerged$Equinor)*sqrt(252)

## Risk adjusted return (return per unit volatility)
SharpeRatio(rMerged$Equinor, Rf = rf, p = 0.95, FUN = "StdDev") 
            
# Rolling correlation
chart.RollingCorrelation(rMerged$Equinor,rMerged$OSEBX,
          colorset=rich12equal, legend.loc="bottomleft", width = 50,
          main = "Rolling correlation, windwo=50 days")

date<-c(2014,2015,2016,2017)
#Rolling standard deviation - annulized volatility
chart.RollingPerformance(rMerged,
                         FUN = "StdDev.annualized", width = 50, colorset = rich12equal,
                         lwd = 2, legend.loc = "topleft",
                         main = "Annualized volatility, 50 days rolling window")

#rolling  regression, 50 days window
charts.RollingRegression(rMerged$Equinor,rMerged$OSEBX, width=50, 
                         Rf=rf, attributes="Beta")


# 63 days (quarterly) rolling preformance
charts.RollingPerformance(rMerged$Equinor, width=63, Rf=rf)

# measuring downside risk through probabilistic estimates --> value at risk
VaR(rMerged$Equinor,method = "historical", p=0.99) #largest potential loss 99%
  
options(scipen=999) # disables scientific notation

#regression analasys
fit<-lm(rEQNR~rOSEBX) %>% print

result<-summary(fit)   %>% print              

#beta: measure of the volatility, or systematic risk, of Equinor
result$coefficients[2,1] %>% print 

# Security summary of Equinor- CAPAM:- market Beta
stargazer(fit, type="text", report="vcst*p", title="Equinor")

# calculating debt equity ratio
USD_NOK<- tail(USD_NOK[1,2])
USD_NOK
date<-c("2014-01-01","2015-01-01","2016-01-01","2017-01-01")
date<-ymd(date)
class(date)
total_equity<-c(50880*USD_NOK$USD_NOK,40128*USD_NOK$USD_NOK,35099*USD_NOK$USD_NOK,39885*USD_NOK$USD_NOK)
total_equity
total_liability <-c(80779*USD_NOK$USD_NOK,69115*USD_NOK$USD_NOK,69431*USD_NOK$USD_NOK,71216*USD_NOK$USD_NOK)

data_2 <- tibble(date=date,Total_Equity=total_equity,Total_liability=total_liability)
data_2 <- data_2 %>%mutate(Total_Assets=c(total_liability+total_equity))
data_2<-data_2 %>% mutate(Debt_ratio=c(total_liability/Total_Assets)*100)
data_2<-data_2 %>% mutate(Equity_ratio=c(total_equity/Total_Assets)*100)


data_2 %>%ggvis(~date,~Debt_ratio, stroke= "Debt ratio") %>% 
  layer_paths() %>% 
  layer_paths(~date,~Equity_ratio, stroke = "Equity ratio") %>%
  add_axis("y",title="Ratio")


?ggvis
