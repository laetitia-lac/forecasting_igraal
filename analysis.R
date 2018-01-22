require(fma) 

#part I : visualisation tools
igraal = read.csv("igraal_data.csv")
tsdisplay(igraal) #first look

#first transformation
igraal_for_analysis = igraal[38:169,]#34-> no null, 169->delete january 2018, 38-> beginning in 2007. 
tsdisplay(igraal_for_analysis)

igraal_for_analysis = as.matrix(igraal_for_analysis)
igraal_for_analysis = igraal_for_analysis[24:132,]
igraal_for_analysis = as.numeric(igraal_for_analysis)
tsdisplay(igraal_for_analysis, lag.max = 30)
seasonplot(ts(igraal_for_analysis, freq=12))

plot(decompose(ts(igraal_for_analysis, freq=12), type=c("additive")))
plot(decompose(ts(igraal_for_analysis, freq=12), type=c("multiplicative")))

#part II : Holt Winters Algorithms
HoltWinters(x=ts(igraal_for_analysis, freq=12),seasonal=c("additive"))$SSE
HoltWinters(x=ts(igraal_for_analysis, freq=12),seasonal=c("multiplicative"))$SSE

HoltWinters(x=ts(igraal_for_analysis, freq=12),seasonal=c("additive"))

plot(forecast(HoltWinters(x=ts(igraal_for_analysis, freq=12),seasonal=c("additive"))))

#part III : ARIMA models
#tests
auto.arima(ts(igraal_for_analysis, freq=12), trace=TRUE)

tsdisplay(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,0), seasonal=c(0,0,0))$residuals)#ARIMA(0,1,0)(0,0,0)12
tsdisplay(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,0), seasonal=c(0,1,0))$residuals)#ARIMA(0,1,0)(0,1,0)12
tsdisplay(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,0), seasonal=c(0,1,1))$residuals)#ARIMA(0,1,0)(0,1,1)12
tsdisplay(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,1), seasonal=c(0,1,1))$residuals)#ARIMA(0,1,1)(0,1,1)12

AIC(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,1), seasonal=c(0,1,1)))

plot(forecast(arima(ts(igraal_for_analysis, freq=12), order=c(0,1,1), seasonal=c(0,1,1))))
