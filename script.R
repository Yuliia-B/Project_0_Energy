library(imputeTS)
library(readxl)
Input_Data <- read_excel("C:/Users/Юля/Desktop/Input Data.xlsx")
tsInput_Data4 <- ts( Input_Data [,4], start=2010, frequency = 12 )
plot(tsInput_Data4)
tsInput_Data4mid <- na_mean(tsInput_Data4, option = "mean", maxgap = Inf)
plot(tsInput_Data4mid)
tsInput_Data4season <- na_seasplit( tsInput_Data4, algorithm = "interpolation", find_frequency = FALSE, maxgap = Inf )
plot(tsInput_Data4season)
stl(tsInput_Data4season [,1], t.window=13, s.window="periodic", robust=TRUE)
perem <- stl(tsInput_Data4season [,1], t.window=13, s.window="periodic", robust=TRUE) 
plot (perem)
stl(tsInput_Data4season [,1], t.window=13, s.window=12, robust=TRUE)
perem <- stl(tsInput_Data4season [,1], t.window=13, s.window=12, robust=TRUE)
plot (perem)
ETS <- forecast(ets(tsInput_Data4season), h=12)
ETS <- forecast(ets(tsInput_Data4season), h=12)
STL <-stlf(tsInput_Data4season, lambda=0, h=12, biasadj=TRUE)
ARIMA <- forecast(auto.arima(tsInput_Data4season, lambda=0, biasadj=TRUE),h=12)
NNAR <- forecast(nnetar(tsInput_Data4season), h=12)
TBATS <- forecast(tbats(tsInput_Data4season, biasadj=TRUE), h=12)
Combination <-(ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] +
                   +                    NNAR[["mean"]] + TBATS[["mean"]])/5
theme_set(theme_light(base_size = 16))
autoplot(tsInput_Data4season, linetype = "dashed") +
autolayer(ETS, series="ETS", PI=FALSE) +
autolayer(ARIMA, series="ARIMA", PI=FALSE) +
autolayer(STL, series="STL", PI=FALSE) +
autolayer(NNAR, series="NNAR", PI=FALSE) +
autolayer(TBATS, series="TBATS", PI=FALSE) +
autolayer(Combination, series="Combination") +
xlab("Рік") +
ylab("Імпорт електроенергії Україною")
guides(colour=guide_legend(title=""))
scale_x_continuous(expand = c(0, 0))
scale_y_continuous(expand = c(0, 0))
scale_x_continuous(breaks= seq(2008, 2021, 2))