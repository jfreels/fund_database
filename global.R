### Created by Justin Freels
### email: jfreels@gmail.com
### twitter: https://twitter.com/jfreels4
### github: https://github.com/jfreels

# Load libraries
libs<-c("lubridate","plyr","reshape2","xts","PerformanceAnalytics")
lapply(libs,require,character.only=TRUE)

# Load dataset
# set working director
setwd("~/ShinyApps/fund_database")
# read in datafile
z<-read.csv("dataset.csv")
# fix dates
z$date<-ymd(z$date)

vami<-function (ror) { cumprod(na.omit(ror) + 1) }
aror<-function (ror) { (1 + cror(ror))^(12/length(ror)) - 1 }
cror<-function (ror) { tail(vami(ror), 1) - 1 }
asd<-function (ror) { sd(ror) * sqrt(12) }
sharpe<-function (ror) { aror(ror)/asd(ror) }
maxdd<-function (ror) { min(dd(ror)) }
dd<-function (ror) { -(1 - vami(ror)/cummax(c(1, cummax(vami(ror))))[-1]) }
omega<-function (ror) { sum(ror[ror>0])/sum(abs(ror[ror<0])) }