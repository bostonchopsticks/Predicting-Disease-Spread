# Set Up R
#  folders structure
projects <- "/Users/namsan/Desktop/Spring\ 2018/PredictiveClass/projects"

# Shortcuts to folders of interest
CleanData <- paste0(projects,"/CleanData")
Dictionaries <- paste0(projects,"/Dictionaries")
RawData <- paste0(projects,"/RawData")
RCode <- paste0(projects,"/RCode")
RData <- paste0(projects,"/RData")
Output <- paste0(projects,"/Output")

tmp.library.list <- c("haven", "zoo", "fUnitRoots", "tseries", "urca", "lmtest", "forecast", "data.table", "readxl","reshape", "quantmod", "ggplot2", "reshape2", "plyr","scales", "hts", "fpp2", "lubridate","stargazer","GGally")
for (i in 1:length(tmp.library.list)) {
  if (!tmp.library.list[i] %in% rownames(installed.packages())) {
    install.packages(tmp.library.list[i])
  }
  library(tmp.library.list[i], character.only = TRUE)
}
rm(tmp.library.list)

# =============================================================================
# Import data
# =============================================================================
trainraw <- data.table(read.csv(paste0(RawData,"/dengue_features_train.csv"), sep=',', stringsAsFactors = F))
trainlraw <- data.table(read.csv(paste0(RawData,"/dengue_labels_train.csv"), sep=',', stringsAsFactors = F))
testraw <- data.table(read.csv(paste0(RawData,"/dengue_features_test.csv"), sep=',', stringsAsFactors = F))
submission <- data.table(read.csv(paste0(RawData,"/submission_format.csv"), sep=',', stringsAsFactors = F))

head(trainraw)
head(trainlraw)

# how much data do we have?
dim(trainraw)

# first let's merge the features data with the labels data
trainprep <- merge(trainraw, trainlraw, by=c("city", "year", "weekofyear"), all.x = T)
nrow(trainprep) == nrow(trainraw) # if true, we did not accidentally merge many-to-one 

trainprep[, reanalysis_sat_precip_amt_mm := NULL]

# so first sort the dataset so it's in a clear order
setkeyv(trainprep, c("city", "year", "weekofyear"))

str(trainprep)

# separate 2 city
sjdata <- trainprep[city=="sj"]

iqdata <- trainprep[city=="iq"]

# =============================================================================
# Split Train and Test 
# =============================================================================

# work on SJ city 
 str(sjdata)

 sjdata[,week_start_date := as.Date(week_start_date,format = "%Y-%m-%d")]

 # preserve 5 years of data for model validation
 sjdata.train <- sjdata[week_start_date < "2003-01-01"]
 sjdata.test <- sjdata[week_start_date >= "2003-01-01"]

# work on IQ city 
 str(iqdata)
 iqdata[,week_start_date := as.Date(week_start_date,format = "%Y-%m-%d")]

 # preserve 2 years of data for model validation
 iqdata.train <- iqdata[week_start_date < "2008-01-01"]
 iqdata.test <- iqdata[week_start_date >= "2008-01-01"]

# =============================================================================
# Descriptive Analysis & Data Visualization
# =============================================================================

# work on SJ city 
 str(sjdata.train)
 #summary
 summary(sjdata.train)
 #stargazer(sjdata.train, type = "text", title="Descriptive statistics - SJ", digits=1, out="SJsummary.txt")

 # correlation plots
 #tiff('Correlation Plot (San Juan).tiff',units="in", width=6, height=4, res=300)
 ggcorr(sjdata.train[, 5:24], label = TRUE, hjust = 0.85, size = 2,color = "grey50",
        label_size = 2) +
   ggplot2::labs(title = "Correlation Plot (San Juan)")
 #dev.off()

 plot(total_cases ~ station_max_temp_c, data=sjdata.train
      ,main = "Scatter Plot of Total Cases and Maximum Temperature in San Juan")
 plot(total_cases ~ reanalysis_specific_humidity_g_per_kg, data=sjdata.train,
      main = "Scatter Plot of Total Cases and Humidity in San Juan")

 ggplot(sjdata.train, aes(week_start_date)) +
  geom_line(aes(y = station_max_temp_c, colour = "station_max_temp_c")) +
  geom_line(aes(y = (y = log(total_cases+1)), 
                colour = "log total_cases")) +
   labs(title = "Time Series Plot of Log Total Cases and Maximum Temperature (San Juan)") +
   theme_bw()
 
 ggplot(sjdata.train, aes(week_start_date)) +
   geom_line(aes(y = log(total_cases+1), 
                 colour = "log total_cases")) +
   geom_line(aes(y = reanalysis_specific_humidity_g_per_kg, 
                 colour = "reanalysis_specific _humidity_g_per_kg")) +
   labs(title = "Time Series Plot of Log Total Cases and Humidity (San Juan)") +
   theme_bw()
  
  # plot total cases with timeline -> seasonal effect
  ggplot(sjdata.train , aes(week_start_date, total_cases)) + 
    geom_line() + 
    scale_y_continuous() +
    scale_x_date(breaks = date_breaks("year"), labels = date_format("%m-%Y")) +
    ylab("Total_cases in San Juan") +
    xlab("")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001)) +
    labs(title = "Time Series Plot of Total Cases (San Juan)") +
    theme_bw()
    
  
  # Omit ndvi_ variables because of the large amount of missing data and 0 correllation
  # sjdata.train[,grep("^ndvi", colnames(sjdata.train)) := NULL]
  
  # build a function to impute data with the most recent that is non missing
  na.locf.data.frame <- 
    function(object, ...) replace(object, TRUE, lapply(object, na.locf, ...))
  
  #fill in NAs
  sjdata.train.imputed <- na.locf.data.frame(sjdata.train)
  summary(sjdata.train.imputed)
  
# Work on IQ city
  str(iqdata.train)
  #summary
  summary(iqdata.train)
  #library(stargazer)
  #stargazer(iqdata.train, type = "text", title="Descriptive statistics - IQ", digits=1, out="IQsummary.txt")
  
  # correlation plots
  #library(GGally)
  ggcorr(iqdata.train[, 5:24], label = TRUE, hjust = 0.85, size = 2,color = "grey50") +
    ggplot2::labs(title = "Correlation Plot (Iquitos)")
  
  plot(total_cases ~ station_max_temp_c, data=iqdata.train)
  plot(total_cases ~ reanalysis_specific_humidity_g_per_kg, data=iqdata.train)
  
  ggplot(iqdata.train, aes(week_start_date)) +
    geom_line(aes(y = station_max_temp_c, colour = "station_max_temp_c")) +
    geom_line(aes(y = (y = log(total_cases+1)), 
                  colour = "log total_cases")) +
    labs(title = "Time Series Plot of Log Total Cases and Maximum Temperature (Iquitos)") +
    theme_bw()
  
  
  ggplot(iqdata.train, aes(week_start_date)) +
    geom_line(aes(y = log(total_cases+1), 
                  colour = "log total_cases")) +
    geom_line(aes(y = reanalysis_specific_humidity_g_per_kg, 
                  colour = "reanalysis_specific _humidity_g_per_kg")) +
    labs(title = "Time Series Plot of Log Total Cases and Humidity (Iquitos)") +
    theme_bw()
  
  # plot total cases with timeline
  ggplot(iqdata.train , aes(week_start_date, total_cases)) + 
    geom_line() + 
    scale_y_continuous() +
    scale_x_date(breaks = date_breaks("year"), labels = date_format("%m-%Y")) +
    ylab("Total_cases in Iquitos") +
    xlab("")+
    theme(axis.text.x=element_text(angle=-70, hjust=0.001)) +
    labs(title = "Time Series Plot of Total Cases (Iquitos)") +
    theme_bw()
  
  #fill in NAs
  iqdata.train.imputed <- na.locf.data.frame(iqdata.train)
  summary(iqdata.train.imputed)
  

# =============================================================================
# Diagnostics 
# =============================================================================

# San Juan
  sjtrain.ts <- ts(sjdata.train$total_cases, frequency = 52, start = c(1990,18))
  sjtrain.ts <- ts(sjdata.train$total_cases, 
     freq=365.25/7, 
     start=decimal_date(ymd("1990-04-30")))
  
  
  # stationary test 
  adf.test(sjdata.train[, total_cases]
           , alternative = "stationary")
  
  # First look at ACF/PACF -> there's seasonality
  ggAcf(sjtrain.ts) +
    labs(title = "Autocorrelation Function Plot (San Juan)") +
    theme_bw()
  ggPacf(sjtrain.ts)  +
    labs(title = "Partial Autocorrelation Function Plot (San Juan)") +
    theme_bw()
  
# Iquito
  # prepare ts
  iqtrain.ts <- ts(iqdata.train.imputed, 
                    freq=365.25/7, 
                    start=decimal_date(ymd("2000-07-01")))
  # stationary test 
  adf.test(iqtrain.ts[, "total_cases"]
           , alternative = "stationary") 
  
  # First look at ACF/PACF -> there's seasonality
  ggAcf(iqtrain.ts[, "total_cases"]) +
    labs(title = "Autocorrelation Function Plot (Iquitos)") +
    theme_bw()
  ggPacf(iqtrain.ts[, "total_cases"]) +
    labs(title = "Partial Autocorrelation Function Plot (Iquitos)") +
    theme_bw()

    
# =============================================================================
# Model 1 - SARIMA 
# =============================================================================
# San Juan
  # fit model
  sj.fit1 <- auto.arima(sjtrain.ts, seasonal = F)
  sj.fit1
  
  checkresiduals(sj.fit1)
  
  #
  fit <- list(aicc=Inf)
  for(i in 1:25)
  {
    fit1 <- auto.arima(sjtrain.ts, xreg=fourier(sjtrain.ts, K=i), seasonal=T)
    if(fit$aicc < fit1$aicc)
      fit1 <- fit
    else break;
  }
  fit1
  i
  checkresiduals(fit1)
  
# Iquito
  # fit model
  iq.fit1 <- auto.arima(iqtrain.ts[, "total_cases"], seasonal =  T)
  iq.fit1
  
  checkresiduals(sj.fit1)
  
  #
  iq.fit <- list(aicc=Inf)
  for(i in 1:25)
  {
    iq.fit1 <- auto.arima(iqtrain.ts[, "total_cases"]
                       , xreg=fourier(iqtrain.ts[, "total_cases"], K=i), seasonal=T)
    if(iq.fit$aicc < fit1$aicc)
      iq.fit1 <- iq.fit
    else break;
  }
  iq.fit1
  i
  checkresiduals(iq.fit1)
  
# =============================================================================
# Model 2 SARIMA-X
# =============================================================================
  # San Juan 
  
  # prepare xreg
  sjtrain.ts1 <- ts(sjdata.train.imputed, 
                    freq=365.25/7, 
                    start=decimal_date(ymd("1990-04-30")))
  
  varlist <- c("precipitation_amt_mm"
             ,"reanalysis_dew_point_temp_k"
             ,"reanalysis_relative_humidity_percent"
             ,"station_avg_temp_c"
             ,"station_diur_temp_rng_c"
             ,"station_max_temp_c"
             ,"station_min_temp_c")
             
  create.tslag <- function(x, dataset) {
    cbind(
      Lag0 = dataset[,x],
      Lag1 = stats::lag(dataset[,x],-1),
      Lag2 = stats::lag(dataset[,x],-2),
      Lag3 = stats::lag(dataset[,x],-3),
      Lag4 = stats::lag(dataset[,x],-4)) %>%
      head(NROW(dataset)) 
  }
  
  precipitation <- create.tslag(x = "precipitation_amt_mm"
                                ,sjtrain.ts1)
  
  dew.temp <- create.tslag(x = "reanalysis_dew_point_temp_k"
                           ,sjtrain.ts1)
  
  relative.humidity <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                    ,sjtrain.ts1)
  
  avg.temp <- create.tslag(x = "station_avg_temp_c"
                                 ,sjtrain.ts1)
  
  diur.temp <- create.tslag(x = "station_diur_temp_rng_c"
                                 ,sjtrain.ts1)
  
  max.temp <- create.tslag(x = "station_max_temp_c"
                                 ,sjtrain.ts1)
  
  min.temp <- create.tslag(x = "station_min_temp_c"
                                 ,sjtrain.ts1)

  
  
  # CCF plot -> week 3,4
  #differencing 
  sjtrain.ts.diff1 = diff(sjtrain.ts,1)
  dew.temp.diff1 = diff(dew.temp[,1],1)
  ccf(dew.temp.diff1,sjtrain.ts.diff1, type = c("correlation","covariance")) 
  
  # final model
  fw <- fourier(sjtrain.ts, K=2)
  
  fit2 <- auto.arima(sjtrain.ts, xreg=cbind(precipitation[,4], relative.humidity[,4]
                                            ,dew.temp[,4], avg.temp[,4]
                                            ,diur.temp[,4],max.temp[,4]
                                            ,min.temp[,4],fw))
                     
  fit2
  
  checkresiduals(fit2)
  
  fit3 <- auto.arima(sjtrain.ts, xreg=cbind(precipitation[,1],relative.humidity[,1]
                                            ,dew.temp[,1],avg.temp[,1]
                                            ,diur.temp[,1],max.temp[,1]
                                            ,min.temp[,1],fw))
  fit3
  checkresiduals(fit3)
  
# IQUITO
  iq.precipitation <- create.tslag(x = "precipitation_amt_mm"
                                ,iqtrain.ts)
  
  iq.dew.temp <- create.tslag(x = "reanalysis_dew_point_temp_k"
                           ,iqtrain.ts)
  
  iq.relative.humidity <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                    ,iqtrain.ts)
  
  iq.avg.temp <- create.tslag(x = "station_avg_temp_c"
                           ,iqtrain.ts)
  
  iq.diur.temp <- create.tslag(x = "station_diur_temp_rng_c"
                            ,iqtrain.ts)
  
  iq.max.temp <- create.tslag(x = "station_max_temp_c"
                           ,iqtrain.ts)
  
  iq.min.temp <- create.tslag(x = "station_min_temp_c"
                           ,iqtrain.ts)
  
  # xreg final model
  iq.fw <- fourier(iqtrain.ts[,"total_cases"], K=1)
  iq.fit2 <- auto.arima(iqtrain.ts[,"total_cases"], xreg=cbind(iq.precipitation[,1]
                                            ,iq.relative.humidity[,1]
                                            ,iq.dew.temp[,1],iq.avg.temp[,1]
                                            ,iq.diur.temp[,1],iq.max.temp[,1]
                                            ,iq.min.temp[,1],iq.fw), seasonal = T)
  iq.fit2
  checkresiduals(iq.fit2)
  
# =============================================================================
# Model 3
# =============================================================================
# San Juan
  fit4 <- nnetar(sjtrain.ts,xreg=sjtrain.ts1[,5:23])
  fit4
  
  #fit4 <- nnetar(sjtrain.ts,xreg=cbind(precipitation[,1],relative.humidity[,1]
                                       #,dew.temp[,1],avg.temp[,1]
                                       #,diur.temp[,1],max.temp[,1]
                                       #,min.temp[,1]))
  #fit4
 
# Iquito
  iq.fit4 <- nnetar(iqtrain.ts[,"total_cases"],xreg=iqtrain.ts[,5:23])
  iq.fit4
  
# =============================================================================
# Model Selection 
# =============================================================================
# San Juan
  #fill in NAs
  sjdata.test.imputed <- na.locf.data.frame(sjdata.test)
  summary(sjdata.test.imputed)
  
  sjtest.ts1 <- ts(sjdata.test.imputed, 
                                freq=365.25/7, 
                                start=decimal_date(ymd("2003-01-01")))
  
  precipitation1 <- create.tslag(x = "precipitation_amt_mm"
                                ,sjtest.ts1)
  
  dew.temp1 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                           ,sjtest.ts1)
  
  relative.humidity1 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                    ,sjtest.ts1)
  
  avg.temp1 <- create.tslag(x = "station_avg_temp_c"
                           ,sjtest.ts1)
  
  diur.temp1 <- create.tslag(x = "station_diur_temp_rng_c"
                            ,sjtest.ts1)
  
  max.temp1 <- create.tslag(x = "station_max_temp_c"
                           ,sjtest.ts1)
  
  min.temp1 <- create.tslag(x = "station_min_temp_c"
                           ,sjtest.ts1)
  # Try ARIMA
  fc0 <- forecast(sj.fit1,h=277)
  plot(fc0)
  accuracy(fc0,sjtest.ts1[,"total_cases"])

  
  # Try SARIMA
  fc1 <- forecast(fit1, xreg=fourier(sjtrain.ts, K=1, h=277))
  plot(fc1)
  accuracy(fc1,sjtest.ts1[,"total_cases"])
  
  # Try SARIMA X
  fwf <- fourier(sjtrain.ts, K=2, h=277)
  fc2 <- forecast(fit2, xreg=cbind(precipitation1[,4],relative.humidity1[,4]
                                   ,dew.temp1[,4],avg.temp1[,4]
                                   ,diur.temp1[,4],max.temp1[,4]
                                   ,min.temp1[,4],fwf), h =277)
  plot(fc2)
  accuracy(fc2,sjtest.ts1[,"total_cases"])
  
  fc3 <- forecast(fit3, xreg=cbind(precipitation1[,1],relative.humidity1[,1]
                                   ,dew.temp1[,1],avg.temp1[,1]
                                   ,diur.temp1[,1],max.temp1[,1]
                                   ,min.temp1[,1],fwf), h =277)
  plot(fc3)
  accuracy(fc3,sjtest.ts1[,"total_cases"])
  
  
  # Try NN
  fc4 <- forecast(fit4, xreg=sjtest.ts1[,5:23], h=277)
  autoplot(fc4)
  accuracy(fc4,sjtest.ts1[,"total_cases"])
  
                  
# IQUITO
  #fill in NAs
  iqdata.test.imputed <- na.locf.data.frame(iqdata.test,na.rm=FALSE)
  iqdata.test.imputed[,station_precip_mm := na.locf(station_precip_mm, fromLast = TRUE)]
  summary(iqdata.test.imputed)
  
  iqtest.ts1 <- ts(iqdata.test.imputed, 
                   freq=365.25/7, 
                   start=decimal_date(ymd("2008-01-01")))
  
  # Try SARIMA
  iq.fc1 <- forecast(iq.fit1, xreg=fourier(iqtest.ts1[,"total_cases"], K=1, h=130))
  plot(iq.fc1)
  accuracy(iq.fc1,iqtest.ts1[,"total_cases"])
  
  iq.precipitation1 <- create.tslag(x = "precipitation_amt_mm"
                                 ,iqtest.ts1)
  
  iq.dew.temp1 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                            ,iqtest.ts1)
  
  iq.relative.humidity1 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                     ,iqtest.ts1)
  
  iq.avg.temp1 <- create.tslag(x = "station_avg_temp_c"
                            ,iqtest.ts1)
  
  iq.diur.temp1 <- create.tslag(x = "station_diur_temp_rng_c"
                             ,iqtest.ts1)
  
  iq.max.temp1 <- create.tslag(x = "station_max_temp_c"
                            ,iqtest.ts1)
  
  iq.min.temp1 <- create.tslag(x = "station_min_temp_c"
                            ,iqtest.ts1)
  
  # Try SARIMA X
  iq.fwf <- fourier(iqtrain.ts[,"total_cases"], K=1, h=130)
  iq.fc2 <- forecast(iq.fit2, xreg=cbind(iq.precipitation1[,1],iq.relative.humidity1[,1]
                                   ,iq.dew.temp1[,1],iq.avg.temp1[,1]
                                   ,iq.diur.temp1[,1],iq.max.temp1[,1]
                                   ,iq.min.temp1[,1],iq.fwf), h =130)
  plot(iq.fc2)
  accuracy(iq.fc2,iqtest.ts1[,"total_cases"])
  
  # Try NN
  iq.fc4 <- forecast(iq.fit4, xreg=iqtest.ts1[,5:23], h=130)
  autoplot(iq.fc4)
  accuracy(iq.fc4,iqtest.ts1[,"total_cases"])
  
  
  

# =============================================================================
# Retrain the best model on the whole train set 
# =============================================================================  
# SAN JUAN
  sjtrain.imputed <- na.locf.data.frame(sjdata)
  summary(sjtrain.imputed)
  sjtrain.full <- ts(sjtrain.imputed, 
                    freq=365.25/7, 
                    start=decimal_date(ymd("1990-04-30")))
  
  precipitation2 <- create.tslag(x = "precipitation_amt_mm"
                                ,sjtrain.full)
  
  dew.temp2 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                           ,sjtrain.full)
  
  relative.humidity2 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                    ,sjtrain.full)
  
  avg.temp2 <- create.tslag(x = "station_avg_temp_c"
                           ,sjtrain.full)
  
  diur.temp2 <- create.tslag(x = "station_diur_temp_rng_c"
                            ,sjtrain.full)
  
  max.temp2 <- create.tslag(x = "station_max_temp_c"
                           ,sjtrain.full)
  
  min.temp2 <- create.tslag(x = "station_min_temp_c"
                           ,sjtrain.full)
  
  # final model
  fw2 <- fourier(sjtrain.full[,"total_cases"], K=2)
  
  fit.final <- auto.arima(sjtrain.full[,"total_cases"]
                     , xreg=cbind(precipitation2[,1], relative.humidity2[,1]
                                            ,dew.temp2[,1], avg.temp2[,1]
                                            ,diur.temp2[,1],max.temp2[,1]
                                            ,min.temp2[,1],fw2))
  
 checkresiduals(fit.final)
 
 
 
# IQUITO
 iqtrain.imputed <- na.locf.data.frame(iqdata)
 summary(iqtrain.imputed)
 iqtrain.full <- ts(iqtrain.imputed, 
                    freq=365.25/7, 
                    start=decimal_date(ymd("2000-07-01")))
 
 iq.precipitation2 <- create.tslag(x = "precipitation_amt_mm"
                                ,iqtrain.full)
 
 iq.dew.temp2 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                           ,iqtrain.full)
 
 iq.relative.humidity2 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                    ,iqtrain.full)
 
 iq.avg.temp2 <- create.tslag(x = "station_avg_temp_c"
                           ,iqtrain.full)
 
 iq.diur.temp2 <- create.tslag(x = "station_diur_temp_rng_c"
                            ,iqtrain.full)
 
 iq.max.temp2 <- create.tslag(x = "station_max_temp_c"
                           ,iqtrain.full)
 
 iq.min.temp2 <- create.tslag(x = "station_min_temp_c"
                           ,iqtrain.full)
 
 # final model
 iq.fw2 <- fourier(iqtrain.full[,"total_cases"], K=1)
 
 iq.fit.final <- auto.arima(iqtrain.full[,"total_cases"]
                         , xreg=cbind(iq.precipitation2[,1], iq.relative.humidity2[,1]
                                      ,iq.dew.temp2[,1], iq.avg.temp2[,1]
                                      ,iq.diur.temp2[,1],iq.max.temp2[,1]
                                      ,iq.min.temp2[,1],iq.fw2))
 # SARIMA
 
 iq.fit0 <- list(aicc=Inf)
 for(i in 1:25)
 {
   iq.fit.final1 <- auto.arima(iqtrain.full[,"total_cases"]
                         , xreg=fourier(iqtrain.full[,"total_cases"], K=i)
                         , seasonal=T)
   if(iq.fit0$aicc <  iq.fit.final1$aicc)
     iq.fit.final1 <- iq.fit0
   else break;
 }
 iq.fit.final1
 i
 
 checkresiduals(iq.fit.final)
 
 iq.fit <- list(aicc=Inf)
 for(i in 1:25)
 {
   iq.fit1 <- auto.arima(iqtrain.ts[, "total_cases"]
                         , xreg=fourier(iqtrain.ts[, "total_cases"], K=i), seasonal=T)
   if(fit$aicc < fit1$aicc)
     iq.fit1 <- iq.fit
   else break;
 }
 iq.fit1
 i
 checkresiduals(iq.fit1)
  
# =============================================================================
# Forecasting
# =============================================================================
# San Juan
  # Process the testraw data
  testraw[, reanalysis_sat_precip_amt_mm := NULL]
  
  # so first sort the dataset so it's in a clear order
  setkeyv(testraw, c("city", "year", "weekofyear"))
  
  summary(testraw)
  
  # separate 2 city
  sjtest <- testraw[city=="sj"]
  
  iqtest <- testraw[city=="iq"]
  
  #fill in NAs
  sjtest.imputed <- na.locf.data.frame(sjtest)
  summary(sjtest.imputed)
  
  # forecast for SJ 
  sjtest.final <- ts(sjtest.imputed, 
                     freq=365.25/7, 
                     start=decimal_date(ymd("1990-04-30")))
  
  precipitation3 <- create.tslag(x = "precipitation_amt_mm"
                                 ,sjtest.final)
  
  dew.temp3 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                            ,sjtest.final)
  
  relative.humidity3 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                     ,sjtest.final)
  
  avg.temp3 <- create.tslag(x = "station_avg_temp_c"
                            ,sjtest.final)
  
  diur.temp3 <- create.tslag(x = "station_diur_temp_rng_c"
                             ,sjtest.final)
  
  max.temp3 <- create.tslag(x = "station_max_temp_c"
                            ,sjtest.final)
  
  min.temp3 <- create.tslag(x = "station_min_temp_c"
                            ,sjtest.final)
  
  sj.fw <- fourier(sjtrain.full[,"total_cases"], K=2, h=260)
  sj.forecast <- forecast(fit.final, xreg=cbind(precipitation3[,1]
                                                ,relative.humidity3[,1]
                                                ,dew.temp3[,1],avg.temp3[,1]
                                                ,diur.temp3[,1],max.temp3[,1]
                                                ,min.temp3[,1],sj.fw), h =260)
  plot(sj.forecast)
  sj.forecast
  

# IQUITO
  #fill in NAs
  iqtest.imputed <- na.locf.data.frame(iqtest)
  summary(iqtest.imputed)
  
  # forecast for IQ
  iqtest.final <- ts(iqtest.imputed, 
                     freq=365.25/7, 
                     start=decimal_date(ymd("2010-07-02")))
  
  iq.precipitation3 <- create.tslag(x = "precipitation_amt_mm"
                                 ,iqtest.final)
  
  iq.dew.temp3 <- create.tslag(x = "reanalysis_dew_point_temp_k"
                            ,iqtest.final)
  
  iq.relative.humidity3 <- create.tslag(x = "reanalysis_relative_humidity_percent"
                                     ,iqtest.final)
  
  iq.avg.temp3 <- create.tslag(x = "station_avg_temp_c"
                            ,iqtest.final)
  
  iq.diur.temp3 <- create.tslag(x = "station_diur_temp_rng_c"
                             ,iqtest.final)
  
  iq.max.temp3 <- create.tslag(x = "station_max_temp_c"
                            ,iqtest.final)
  
  iq.min.temp3 <- create.tslag(x = "station_min_temp_c"
                            ,iqtest.final)
  
  iq.fw.final <- fourier(iqtrain.full[,"total_cases"], K=1, h=156)
  iq.forecast <- forecast(iq.fit.final, xreg=cbind(iq.precipitation3[,1]
                                                ,iq.relative.humidity3[,1]
                                                ,iq.dew.temp3[,1],iq.avg.temp3[,1]
                                                ,iq.diur.temp3[,1],iq.max.temp3[,1]
                                                ,iq.min.temp3[,1],iq.fw.final), h =156)
  
  
  
  plot(iq.forecast)
  iq.forecast
  
# =============================================================================
# Submission
# =============================================================================
  
  sj.mean <- as.numeric(sj.forecast$mean)
  iq.mean <- as.numeric(iq.forecast$mean)
  
  forecast.cases <- c(sj.mean,iq.mean)
  submission[,total_cases := forecast.cases]
  submission$total_cases <- round(submission$total_cases, digits = 0)
  

  #write.csv(submission, file = "DengAI_submission1.csv",row.names=FALSE) 
  
