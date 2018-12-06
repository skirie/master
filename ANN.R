#### ------------------------------------------- ####
#### Modellierung von Treibhausgasen mittels Neuralen Netzwerken ####
#### ------------------------------------------- ####
  
#### -------------------------- ####
  ## Used Packages ####
#### -------------------------- ####
  
  ## Package Function ####
  check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  ## Packages ####
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  check.packages(packages) 
  use_condaenv("r-tensorflow")
  

  #### ####
#### ------------------------------------- ####
  ## load data ####
#### ------------------------------------- ####
  
  ## load data ####
  ## path 
  #setwd("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Daten_und_Auswertung/Auswertung")
  mypath <- getwd()
  
  ## Flux Data ####
  df_raw <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver3.csv"))
  
  ## Weather data from Comox airport ####
  df_comox <- read.csv(paste0(mypath, "/Daten/weatherstats_comox_hourly.csv"))
  
  ## Sun rise / set ####
  df_hel <- read.csv(paste0(mypath, "/Daten/sunsetrise.csv"),
                     header=T, sep=";")
  
  head(df_raw)
  summary(df_raw)
  
  ####  ####
  
#### ------------------------------------- ####
  ## add time ####
#### ------------------------------------- #### 
  
  ## Time format ####
  df_comox$dt <- as.POSIXct(substr(as.character(df_comox$date_time_local), 1, 16), format = "%Y-%m-%e %H:%M", tz = "America/Vancouver")
  
  df_comox <- df_comox[which(df_comox$dt > df_comox$dt[which(df_comox$dt == "2002-1-1 00:00:00")]),]             
  df_comox <- df_comox[which(df_comox$dt < df_comox$dt[which(df_comox$dt == "2017-1-1 02:00:00")]),]
  
  df_raw$dt <- paste0(df_raw$year, "-", df_raw$month, "-", 
                      df_raw$day, " ", df_raw$hour, ":", 
                      df_raw$min)
  df_raw$dt <- as.POSIXct(df_raw$dt, format = "%Y-%m-%d %H:%M", "GMT")
  attr(df_raw$dt, "tzone") <- "America/Vancouver"

  ####  ####
  
#### ------------------------------------- ####
  ## First check of raw data ####
#### ------------------------------------- ####  
  
  ## NaN to NA ####
  df_raw[df_raw == "NaN"] <- NA

  ## expand df_comox to halfhour intervall ####
  dt <- paste0(substr(as.character(df_comox$date_time_local), 1, 14), "30:00")
  dt <- as.POSIXct(dt, format="%Y-%m-%e %H:%M", "America/Vancouver")
  df_ <- df_comox
  df_[,] <- NA
  df_$dt <- dt
  
  df_comox_2 <- rbind(df_comox, df_)
  df_comox_2 <- df_comox_2[order(df_comox_2$dt),]
  
  rm(dt, df_)
  ## Check for Colinearity ####
  summary(df_raw)
  summary(df_comox_2)
  #M <- cor(df_raw_2[,7:30], use = "complete.obs")
  #corrplot.mixed(M)
  #df_raw <- df_raw[,-c(7:11,14:17)]
  df_raw_2 <- df_raw
  summary(df_raw_2)

  ####  ####
  
#### ------------------------------------- ####
  ## Creating additional predictors ####
#### ------------------------------------- ####  
  ## Hours since last precipitation event ####
  summary(df_raw_2$Precip)
  
  df_raw_2$h_last_precip <- NA
  df_raw_2$h_last_precip[which(df_raw_2$Precip >= 0.1)] <- 0
  
  w_1 <- w_2 <- which(df_raw_2$Precip > 0)
  w_2[2:length(w_2)] <- w_2[1:(length(w_2)-1)]
  
  h_lastprec <- w_1 - w_2 - 1
  h_lastprec <- h_lastprec[-1]
  
  h_lastprec_2 <- h_lastprec[which(h_lastprec > 0)]
  w_1.1 <- w_1[which(h_lastprec > 0)]
  
  for (i in 1:length(h_lastprec_2)){
    df_raw_2$h_last_precip[(w_1.1[i]+1):(w_1.1[i]+h_lastprec_2[i])] <- seq(0.5, h_lastprec_2[i]/2, 0.5)
  }
  
  rm(w_1, w_2, h_lastprec, h_lastprec_2, w_1.1)
  summary(df_raw_2$h_last_precip)
  
  ## Precipitation of last 30 days ####
  df_raw_2$precip_30d <- NA
  
  for (i in 30:nrow(df_raw_2)){
    df_raw_2$precip_30d[i] <- sum(df_raw_2$Precip[(i-29):i], na.rm = T)  
  }
  
  which(df_raw_2$Precip[1:30] > 0)
  df_raw_2$precip_30d[1:10] <- 0
  df_raw_2$precip_30d[11:12] <- df_raw_2$precip_30d[10]
  df_raw_2$precip_30d[13:29] <- sum(df_raw_2$precip_30d[1:12], na.rum = T)
  
  summary(df_raw_2$precip_30d)
  
  ## Sinus curves year and day ####
  ## year with peak in winter / summer and spring / autum
  years_ <- unique(df_raw_2$year)
  df_raw_2$year_ws_sin <- NA
  df_raw_2$year_sa_sin <- NA
  
  for (i in 1:length(years_)){
    which_ <- which(df_raw_2$year == years_[i])
    
    df_raw_2$year_ws_sin[which_] <- sin(seq((1.5+(10.5/365*2))*pi, (3.5+(10.5/365*2))*pi,  length.out = length(which_)))
    df_raw_2$year_sa_sin[which_] <- sin(seq((10.5/365*2)*pi, (2+(10.5/365*2))*pi,  length.out = length(which_)))
  }
  
  summary(df_raw_2$year_ws_sin)
  summary(df_raw_2$year_sa_sin)

  #plot(df_raw_2$year_sa_sin)
  #plot(df_raw_2$year_ws_sin)
  
  ## day 
  df_raw_2$day_sin <- NA
  df_raw_2$hours_mins_ <- as.numeric(paste0(df_raw_2$hour, ".", df_raw_2$min))
  hours_mins_ <- unique(df_raw_2$hours_mins_)
  sin_seq <- sin(seq(1.5*pi, 3.5*pi,  length.out = length(hours_mins_)))
  
  for (i in 1:length(hours_mins_)){
    which_ <- which(df_raw_2$hours_mins_ == hours_mins_[i])
    df_raw_2$day_sin[which_] <- sin_seq[i]
  }
  
  summary(df_raw_2$day_sin)
  df_raw_2 <- df_raw_2[,-which(names(df_raw_2) == "hours_mins_")]
  rm(hours_mins_, i, sin_seq, which_, years_, which_2001, which_2002)

  ## Fractional year ####
  ## Short wave in ####
  #Q* <- (SWin - SWout) + (LWin - LWout)
  #df_raw_2$SWin <- df_raw_2$Rnet - (df_raw_2$LWin - df_raw_2$LWout) + df_raw_2$SWout
  #summary(df_raw_2$SWin)
  #hist(df_raw_2$SWin)
  
  ####  ####
  
#### ------------------------------------- ####
  ## Plausibilitytests ####
#### ------------------------------------- #### 
  
  df_raw_3 <- df_raw_2
  ## Tair -20 - 40 ####
  df_raw_3$airT[which(df_raw_3$airT > 40 | df_raw_3$airT < -20)] <- NA
  summary(df_raw_3$airT)
  
  ## Relative Humidity 0 - 100 (105) ####
  summary(df_raw_3$RH)
  df_raw_3$RH[which(df_raw_3$RH > 105 | df_raw_3$RH <= 0)] <- NA
  df_raw_3$RH[which(df_raw_3$RH > 100 & df_raw_3$RH <= 105)] <- 100
  summary(df_raw_3$RH)
  
  ## PPFDin 0 - 2730 ####
  df_raw_3$PPFDin[which(df_raw_3$PPFDin < 0)] <- 0
  df_raw_3$PPFDin[which(df_raw_3$PPFDin > 2730)] <- NA
  summary(df_raw_3$PPFDin)
  
  ## SWin  -5 - 1300 ####
  summary(df_raw_3$Rnet)
  #hist(df_raw_3$Rnet)
  #df_raw_3$Rnet[which(df_raw_3$Rnet < 0 & df_raw_3$Rnet >= -5 )] <- 0
  #df_raw_3$Rnet[which(df_raw_3$Rnet < -5)] <- NA
  #summary(df_raw_3$PPFDin)
  
  ## SWout ####
  df_raw_3$SWout[which(df_raw_3$SWout <= 0)] <- NA
  summary(df_raw_3$SWout)
  
  ## Windspeed ####
  df_raw_3$WindSpeed[which(df_raw_3$WindSpeed < 0)] <- NA
  ####  ####
  
#### ------------------------------------- ####
  ## Fill gaps of predictors ####
#### ------------------------------------- ####   
  
  ## function for detecting location and size of gaps ####
  fun_detect_gaps <- function(df, maxsize = 12){
    rl <- rle(is.na(df))

    # position
    # which gaps are < 12
    w_gap <- which((rl$lengths < maxsize) == T & rl$values == T)
    position_ <- NULL
    
    if (length(w_gap) < 2){
      print("No small gaps")
    } else {
      # start position and gap size
      for (i in 1:length(w_gap)){
        position_[i] <- sum(rl$lengths[1:(w_gap[i]-1)])+1
      }
      size_ <- rl$lengths[w_gap]
      df_retrun <- data.frame("position" = position_, "size" = size_)
    }
    
  }
  
  ## function for interpolating
  fun_interpol <- function(df, df_gaps){
    for (i in 1:nrow(df_gaps)){
      #print(i)
      val_x <- c(df_gaps[i,1] - 1, df_gaps[i,1] + df_gaps[i,2])
      val_y <- c(df[(df_gaps[i,1] - 1)], df[(df_gaps[i,1] + df_gaps[i,2])])
      
      linearMod <- lm(val_y ~ val_x)
      pred_x <- c(df_gaps[i,1]:(df_gaps[i,1] + df_gaps[i,2] - 1))
      
      df[df_gaps[i,1]:(df_gaps[i,1] + df_gaps[i,2] - 1)] <- predict(linearMod, newdata = data.frame(val_x = pred_x))
    }
    return(df)
  }
  
  ## fill gaps of comox dataset -> half hourly data ####
  df_temp_gaps <- fun_detect_gaps(df = df_comox_2$temperature, 12)
  df_comox_2$temperature <- fun_interpol(df = df_comox_2$temperature, df_gaps = df_temp_gaps)
  
  df_rh_gaps <- fun_detect_gaps(df = df_comox_2$relative_humidity, 12)
  df_comox_2$relative_humidity <- fun_interpol(df = df_comox_2$relative_humidity, df_gaps = df_rh_gaps)
  
  df_ws_gaps <- fun_detect_gaps(df = df_comox_2$wind_speed, 12)
  df_comox_2$wind_speed <- fun_interpol(df = df_comox_2$wind_speed, df_gaps = df_ws_gaps)
  
  ## create dataset with all data ####
  df_merged <- merge(df_raw_3, df_comox_2[,c("dt", "temperature", "relative_humidity", "wind_speed")], by = "dt")
  
## Tair ####
  ## gaps smaller 6 hours -> interpolating
  summary(df_merged$airT)
  df_airt_gaps <- fun_detect_gaps(df = df_merged$airT, 12)
  df_merged$airT <- fun_interpol(df = df_merged$airT, df_gaps = df_airt_gaps)
  
  ## GLM
  # hist(df_merged$airT)
  # plot(df_merged$airT ~ df_merged$temperature)
  glm_air <- glm(airT ~ TS_main + PPFDin + Rnet + temperature, data = df_merged)
  summary(glm_air)
  
  ## pseudo r2
  #1 - glm_air$deviance / glm_air$null.deviance # 0.95
  preds <- predict(glm_air, newdata = df_merged[which(is.na(df_merged$airT)),])
  summary(preds)
  df_merged[which(is.na(df_merged$airT)), "airT"] <- unname(preds)
  
  rm(preds, glm_air, df_airt_gaps, df_temp_gaps, df_rh_gaps, df_ws_gaps)
  
## SWout ####
  summary(df_merged$SWout)
  ## smal gaps < 6h interpolating
  df_swout_gaps <- fun_detect_gaps(df = df_merged$SWout, 12)
  df_merged$SWout <- fun_interpol(df = df_merged$SWout, df_gaps = df_swout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$LWout)) == T)) # common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$RH)) == T)) # common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$CO2)) == T)) # common gaps
  
  ## Histogram
  #hist(df_merged$SWout[-which(is.na(df_merged$SWout))])
  
  ## Distribution
  # fit_norm <- fitdistr(df_merged$SWout[-which(is.na(df_merged$SWout))], "normal")
  # fit_lnorm <- fitdistr(df_merged$SWout[-which(is.na(df_merged$SWout))], "lognormal")
  # fit_chi <- fitdistr(df_merged$SWout[-which(is.na(df_merged$SWout))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df_merged$SWout[-which(is.na(df_merged$SWout))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm) # best distribution
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  ## GLM
  glm_swout <- glm(log(SWout) ~ PPFDin + airT + day_sin + Rnet, data = df_merged[-which(is.na(df_merged$SWout)),], family=gaussian(link="identity"))
  summary(glm_swout)

  ## pseudo r2
  # 1 - glm_swout$deviance / glm_swout$null.deviance # 0.26
  preds <- exp(predict(glm_swout, newdata = df_merged[which(is.na(df_merged$SWout)),]))
  summary(preds)
  df_merged[which(is.na(df_merged$SWout)), "SWout"] <- unname(preds)
  
  df_merged$SWout[which(df_merged$SWout > 800)] <- NA
  df_swout_gaps <- fun_detect_gaps(df = df_merged$SWout, 12)
  df_merged$SWout <- fun_interpol(df = df_merged$SWout, df_gaps = df_swout_gaps)
  summary(df_merged$SWout)
  
  rm(preds, glm_swout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_swout_gaps)

## LWout ####
  summary(df_merged$LWout)
  ## smal gaps < 6h interpolating
  df_lwout_gaps <- fun_detect_gaps(df = df_merged$LWout, 12)
  df_merged$LWout <- fun_interpol(df = df_merged$LWout, df_gaps = df_lwout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$SWout)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$RH)) == T)) # common gaps
  
  ## Histogram
  #hist(df_merged$LWout[-which(is.na(df_merged$LWout))])
  
  ## Distribution
  # fit_norm <- fitdistr(df_merged$LWout[-which(is.na(df_merged$LWout))], "normal")
  # fit_lnorm <- fitdistr(df_merged$LWout[-which(is.na(df_merged$LWout))], "lognormal")
  # fit_chi <- fitdistr(df_merged$LWout[-which(is.na(df_merged$LWout))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df_merged$LWout[-which(is.na(df_merged$LWout))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm) # best distribution
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  glm_lwout <- glm(log(LWout) ~ PPFDin + airT + SWout + year_ws_sin + year_sa_sin + day_sin + Rnet, data = df_merged[-which(is.na(df_merged$LWout)),], 
                   family=gaussian(link="identity"))
  summary(glm_lwout)
  
  # pseudo r2
  #1 - glm_lwout$deviance / glm_lwout$null.deviance # 0.98
  preds <- exp(predict(glm_lwout, newdata = df_merged[which(is.na(df_merged$LWout)),]))
  summary(preds)
  df_merged[which(is.na(df_merged$LWout)), "LWout"] <- unname(preds)
  
  rm(preds, glm_lwout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_lwout_gaps)
  
## Soil moisture Main ####
  summary(df_merged$Soil.moisture_main)
  ## smal gaps < 6h interpolating
  df_swmain_gaps <- fun_detect_gaps(df = df_merged$Soil.moisture_main, 12)
  df_merged$Soil.moisture_main <- fun_interpol(df = df_merged$Soil.moisture_main, df_gaps = df_swmain_gaps)
  
  ## Histogram
  #hist(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))])
  
  ## Distribution
  # fit_norm <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "normal")
  # fit_lnorm <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "lognormal")
  # fit_beta <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "beta", 
  #                      start = list(shape1 = 1, shape2 = 1), lower=c(0.1))
  # fit_weibull <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "weibull", 
  #                         start = list(scale = 1, shape = 0.1), lower=c(0.1))
  # fit_chi <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "chi-squared", 
  #                     start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))], "gamma", 
  #                     start = list(shape = 1, rate = 0.1), lower=c(0,0))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm)
  # AIC(fit_beta)
  # AIC(fit_weibull) # Best Distribution, but normal distribution is used due to better visual fit
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  ## Histogram with distributions
  # hist(df_merged$Soil.moisture_main, freq=F, las=1, xlim = c(0,0.6))
  # curve(dweibull(x, fit_weibull$estimate[1], fit_weibull$estimate[2]),
  #       add=T, lwd=2)
  # curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,
  #       lwd=2, col="grey")
  # curve(dnorm(x, fit_norm$estimate[1], fit_norm$estimate[2]), add=T,
  #       lwd=2, col="red")
  # curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,
  #       lwd=2, col="blue")
  # curve(dgamma(x, fit_gam$estimate[1], fit_gam$estimate[2]), add=T,
  #       lwd=2, col="green")
  
  ## Gaussianregrssion fitting and predicting
  glm_smm <- glm(Soil.moisture_main ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d, 
                 data = df_merged[-which(is.na(df_merged$Soil.moisture_main)),], 
                 family = gaussian)
  summary(glm_smm)
  #1 - glm_smm$deviance / glm_smm$null.deviance # 0.77
  
  preds <- predict(glm_smm, newdata = df_merged[which(is.na(df_merged$Soil.moisture_main)),])
  summary(preds)
  preds[which(preds < 0)] <- min(df_merged$Soil.moisture_main, na.rm = T)
  df_merged[which(is.na(df_merged$Soil.moisture_main)),"Soil.moisture_main"] <- unname(preds)
  
  summary(df_merged$Soil.moisture_main)
  
  rm(preds, glm_smm, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam)
  
## LWin ####
  summary(df_merged$LWin)
  ## smal gaps < 6h interpolating
  df_lwin_gaps <- fun_detect_gaps(df = df_merged$LWin, 12)
  df_merged$LWin <- fun_interpol(df = df_merged$LWin, df_gaps = df_lwin_gaps)
  
  ## Histogram
  #hist(df_merged$LWin[-which(is.na(df_merged$LWin))])
  
  ## Distribution
  # fit_norm <- fitdistr(df_merged$LWin[-which(is.na(df_merged$LWin))], "normal")
  # fit_lnorm <- fitdistr(df_merged$LWin[-which(is.na(df_merged$LWin))], "lognormal")
  # fit_chi <- fitdistr(df_merged$LWin[-which(is.na(df_merged$LWin))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df_merged$LWin[-which(is.na(df_merged$LWin))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm) # best distribution
  # AIC(fit_lnorm) 
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  glm_lwin <- glm(LWin ~ PPFDin + airT + SWout + Soil.moisture_main + LWout + Rnet, data = df_merged[-which(is.na(df_merged$LWin)),], 
                   family=gaussian(link="identity"))
  summary(glm_lwin)
  
  ## pseudo r2
  #1 - glm_lwin$deviance / glm_lwin$null.deviance # 0.95
  preds <- predict(glm_lwin, newdata = df_merged[which(is.na(df_merged$LWin)),])
  summary(preds)
  preds[which(preds < 0)] <- NA
  df_merged[which(is.na(df_merged$LWin)), "LWin"] <- unname(preds)
  
  # small gaps fill again 
  df_lwin_gaps <- fun_detect_gaps(df = df_merged$LWin, 12)
  df_merged$LWin <- fun_interpol(df = df_merged$LWin, df_gaps = df_lwin_gaps)
  
  summary(df_merged$LWin)
  
  rm(preds, glm_lwin, fit_norm, fit_lnorm, fit_chi, fit_gam)
  
## Windspeed ####
  summary(df_merged$WindSpeed)
  ## smal gaps < 6h interpolating
  df_wind_gaps <- fun_detect_gaps(df = df_merged$WindSpeed, 12)
  df_merged$WindSpeed <- fun_interpol(df = df_merged$WindSpeed, df_gaps = df_wind_gaps)
  
  ## Histogram
  #hist(df_merged$WindSpeed[-which(is.na(df_merged$WindSpeed))])
  
  ## GLM
  glm_ws <- glm(WindSpeed ~ airT + PPFDin + wind_speed, data = df_merged[-which(is.na(df_merged$WindSpeed)),])
  summary(glm_ws)
  
  ## pseudo r2
  #1 - glm_ws$deviance / glm_ws$null.deviance # 0.11
  preds <- predict(glm_ws, newdata = df_merged[which(is.na(df_merged$WindSpeed)),])
  summary(preds)
  df_merged[which(is.na(df_merged$WindSpeed)), "WindSpeed"] <- unname(preds)
  
  rm(preds, glm_ws)
  
## RH ####
  ## check if significant correlated predictors have common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$LWout)) == T)) # 3% common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$Soil.moisture_main)) == T)) # common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$CO2)) == T)) # common gaps
  
  ## Histogram 
  #hist(df_merged$RH) # beta distribution
  #plot(RH ~ relative_humidity, df_merged)
  summary(df_merged$RH)
  
  ## Values need to be in range (0,1) for beta regression
  df_merged$RH <- df_merged$RH/100
  df_merged$RH[which(df_merged$RH == 1)] <- 1-1e-10
  
  ## Check distributions
  # fit_norm <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "normal")
  # fit_lnorm <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "lognormal")
  # fit_beta <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "beta", start = list(shape1 = 1, shape2 = 1), lower=c(0,0))
  # fit_weibull <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "weibull", start = list(scale = 1, shape = 1), lower=c(0,0))
  # fit_chi <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df_merged$RH[-which(is.na(df_merged$RH))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0,0))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm)
  # AIC(fit_beta) ## Best distribution
  # AIC(fit_weibull)
  # AIC(fit_chi)
  # AIC(fit_gam)
  # 
  # hist(df_merged$RH[-which(is.na(df_merged$RH))], freq=F, las=1, xlim = c(0,2))
  # curve(dweibull(x, fit_weibull$estimate[1], fit_weibull$estimate[2]),
  #       add=T, lwd=2)
  # curve(dlnorm(x, fit_lnorm$estimate[1], fit_lnorm$estimate[2]), add=T,
  #       lwd=2, col="grey")
  # curve(dgamma(x, fit_gam$estimate[1], fit_gam$estimate[2]), add=T,
  #       lwd=2, col="red")
  # curve(dbeta(x, fit_beta$estimate[1], fit_beta$estimate[2]), add=T,
  #       lwd=2, col="blue")
  
  ## Betaregrssion fitting and predicting
  betareg_rh <- betareg(RH ~ airT + PPFDin + relative_humidity + h_last_precip, data = df_merged[-which(is.na(df_merged$RH)),])
  summary(betareg_rh, type = "pearson")
  ## pseudo r2 0.16
  
  preds <- predict(betareg_rh, newdata = df_merged[which(is.na(df_merged$RH)),])
  summary(preds)
  df_merged[which(is.na(df_merged$RH)),"RH"] <- unname(preds)
  
  rm(preds, betareg_rh, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam)
  
## Precipitation ####
  # summary(df_merged$Precip)
  # # smal gaps < 6h interpolating
  # df_precip_gaps <- fun_detect_gaps(df = df_merged$Precip, 12)
  # df_merged$Precip <- fun_interpol(df = df_merged$Precip, df_gaps = df_precip_gaps)
  # 
  # # Histogram
  # hist(df_merged$Precip[-which(is.na(df_merged$Precip))])
  # summary(df_merged$Precip[-which(is.na(df_merged$Precip))])
  
  summary(df_merged)
  #### ####
  
#### ------------------------------------- ####
  ## Sun rise / set ####
#### ------------------------------------- ####    
  
  ## Sun rise / set ####
  df_hel$Datum <- as.character(df_hel$Datum)
  df_hel$Sunrise <- as.character(df_hel$Sunrise)
  df_hel$Sunset <- as.character(df_hel$Sunset)
  
  df_hel_2 <- data.frame(df_hel, as.numeric(do.call(rbind, strsplit(df_hel$Datum,' '))[,1]))
  names(df_hel_2)[5] <- "day"
  
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunset,':'))[,1]))
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunset,':'))[,2]))
  names(df_hel_2)[6:7] <- paste("set", c("h","m"), sep = "_")
  
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunrise,':'))[,1]))
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunrise,':'))[,2]))
  names(df_hel_2)[8:9] <- paste("rise", c("h","m"), sep = "_")
  
  df_hel_2 <- df_hel_2[,-c(2:4)]
  
  ## flag night time data ####
  fun_flag_night <- function(df_flux, df_sun){
    hm_flux <- df_flux$hour * 60 + df_flux$min
    hm_sun_r <- df_sun$rise_h * 60 + df_sun$rise_m
    hm_sun_s <- df_sun$set_h * 60 + df_sun$set_m
    
    flag_ <- rep(0, nrow(df_flux))
    
    for (i in 1:12){
      w_m_raw <- which(df_flux$month == i) # which month
      w_m_sun <- which(df_sun$month == i) # which month
      
      for (j in 1:length(w_m_sun)){
        w_d_raw <- which(df_flux$day == j) # which day
        w_raw <- w_m_raw[which(w_m_raw %in% w_d_raw)] # match of month and day
        
        flag_[w_raw][hm_flux[w_raw] < hm_sun_r[w_m_sun][j] | hm_flux[w_raw] > hm_sun_s[w_m_sun][j]] <- 1
      }
    }
    return(flag_)
  }
  
  df_raw_3$flag_night <- fun_flag_night(df_flux = df_raw, df_sun = df_hel_2)
  
  rm(df_hel, fun_flag_night)
  
  ## First Plots - overview ####
  #df_raw_3$dt <- paste0(df_raw_3$year,"-", df_raw_3$month, "-", df_raw_3$day, " ", df_raw_3$hour, ":", df_raw_3$min)
  #df_raw_3$dt <- as.POSIXct(df_raw_3$dt, format="%Y-%m-%e %H:%M")
  
  #plot(df_raw$NEE_measure[1:17000] ~ df_raw$dt[1:17000], type = "l")
  
  #### ####

#### ------------------------------------- ####
  ## Prepare Data for Respiration Model ####
#### ------------------------------------- ####  
  
  ## Extract Night Data ####
  df_night <- df_raw_3[df_raw_3$flag_night == 1,]
  df_night <- df_night[-which(df_night$PPFDin > 5),]
  
  ## u* correction ####
  # Jassal et al. 2009: 0.19 | Krishnan et al. 2009: 0.16 | Jassal et al. 2010: 0.19 
  df_night$NEE_cor <- df_night$NEE_measure
  df_night$NEE_cor[df_night$ustar < 0.19] <- NA
  summary(df_night$NEE_measure)
  summary(df_night$NEE_cor)
  #percent_gaps <- sum(is.na(df_night$NEE_cor)) / nrow(df_night) ## 78.53 %
  
  ## data frame with NNE != NA ####
  # and without precip, pressure, lw, sw, co2
  df_night_model <-  df_night[!is.na(df_night$NEE_cor),c(7:10, 12:15, 23)]
  df_night_pred <-  df_night[is.na(df_night$NEE_cor),c(7:10, 12:15, 23)]
  
  summary(df_night_model)
  summary(df_night_pred)
  
  M <- cor(df_night_model, use = "complete.obs")
  corrplot.mixed(M)
  
  rm(M, df_hel_2)

