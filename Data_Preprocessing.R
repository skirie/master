#### ------------------------------------------- ####
#### Modellierung von Treibhausgasen mittels Neuralen Netzwerken ####
#### ------------------------------------------- ####
  
#### ------------------------------------- ####
  ## 0 - load data ####
#### ------------------------------------- ####
  
  ## load data ##
  ## path ####
  #setwd("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Daten_und_Auswertung/Auswertung")
  mypath <- getwd()
  
  ## Flux Data ####
  df_raw_o <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver2.csv"))
  df_raw <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver3.csv"))
  
  df_raw_2 <- cbind(df_raw, "ustar" = df_raw_o$ustar, "NEE_measure" = df_raw_o$NEE_measure)
  
  ## Weather data from Comox airport ####
  df_comox <- read.csv(paste0(mypath, "/Daten/weatherstats_comox_hourly.csv"))
  
  ####  ####
  
#### ------------------------------------- ####
  ## 1 - add time ####
#### ------------------------------------- ####
  
  ## First change "NaN" to NA
  ## NaN to NA ####
  df_raw_2[df_raw_2 == "NaN"] <- NA
  
  ## Time format ####
  df_comox$dt <- as.POSIXct(substr(as.character(df_comox$date_time_local), 1, 16), format = "%Y-%m-%e %H:%M", tz = "America/Vancouver")
  
  df_comox <- df_comox[which(df_comox$dt > df_comox$dt[which(df_comox$dt == "2002-1-1 00:00:00")]),]             
  df_comox <- df_comox[which(df_comox$dt < df_comox$dt[which(df_comox$dt == "2017-1-1 02:00:00")]),]
  
  df_raw_2$dt <- paste0(df_raw_2$year, "-", df_raw_2$month, "-", 
                        df_raw_2$day, " ", df_raw_2$hour, ":", 
                        df_raw_2$min)
  df_raw_2$dt <- as.POSIXct(df_raw_2$dt, format = "%Y-%m-%d %H:%M", "GMT")
  attr(df_raw_2$dt, "tzone") <- "America/Vancouver"

  ####  ####
  
#### ------------------------------------- ####
  ## 2 - First check of raw data ####
#### ------------------------------------- ####  
  
  ## expand df_comox to halfhour intervall ####
  dt <- paste0(substr(as.character(df_comox$date_time_local), 1, 14), "30:00")
  dt <- as.POSIXct(dt, format="%Y-%m-%e %H:%M", "America/Vancouver")
  df_ <- df_comox
  df_[, ] <- NA
  df_$dt <- dt
  
  df_comox_2 <- rbind(df_comox, df_)
  df_comox_2 <- df_comox_2[order(df_comox_2$dt), ]
  
  rm(dt, df_, df_raw_o, df_raw, df_comox)
  
  ## Check for Colinearity ####
  # summary(df_raw_2)
  # summary(df_comox_2)
  # M <- cor(df_raw_2[,7:30], use = "complete.obs")
  # corrplot.mixed(M)
  # df_raw <- df_raw[,-c(7:11,14:17)]

  ####  ####
  
#### ------------------------------------- ####
  ## 3 - Creating additional predictors ####
#### ------------------------------------- ####  
  
  ## Hours since last precipitation event ####
  # summary(df_raw_2$Precip)
  
  df_raw_2$h_last_precip <- NA
  df_raw_2$h_last_precip[which(df_raw_2$Precip >= 0.1)] <- 0
  
  w_1 <- w_2 <- which(df_raw_2$Precip > 0)
  w_2[2:length(w_2)] <- w_2[1:(length(w_2) - 1)]
  
  h_lastprec <- w_1 - w_2 - 1
  h_lastprec <- h_lastprec[-1]
  
  h_lastprec_2 <- h_lastprec[which(h_lastprec > 0)]
  w_1.1 <- w_1[which(h_lastprec > 0)]
  
  for (i in 1:length(h_lastprec_2)){
    df_raw_2$h_last_precip[(w_1.1[i] + 1):(w_1.1[i] + h_lastprec_2[i])] <- seq(0.5, h_lastprec_2[i] /2 , 0.5)
  }
  
  rm(w_1, w_2, h_lastprec, h_lastprec_2, w_1.1)
  # summary(df_raw_2$h_last_precip)
  
  ## Precipitation of last 30 days ####
  df_raw_2$precip_30d <- NA
  
  for (i in 30:nrow(df_raw_2)){
    df_raw_2$precip_30d[i] <- sum(df_raw_2$Precip[(i - 29):i], na.rm = T)  
  }
  
  which(df_raw_2$Precip[1:30] > 0)
  df_raw_2$precip_30d[1:10] <- 0
  df_raw_2$precip_30d[11:12] <- df_raw_2$precip_30d[10]
  df_raw_2$precip_30d[13:29] <- sum(df_raw_2$precip_30d[1:12], na.rum = T)
  
  # summary(df_raw_2$precip_30d)
  
  ## Sinus curves year and day ####
  ## year with peak in winter / summer and spring / autum
  years_ <- unique(df_raw_2$year)
  df_raw_2$year_ws_sin <- NA
  df_raw_2$year_sa_sin <- NA
  
  for (i in 1:length(years_)){
    which_ <- which(df_raw_2$year == years_[i])
    
    df_raw_2$year_ws_sin[which_] <- sin(seq((1.5 + (10.5 / 365 * 2)) * pi, (3.5 + (10.5 / 365 * 2)) * pi, 
                                            length.out = length(which_)))
    df_raw_2$year_sa_sin[which_] <- sin(seq((10.5 / 365 * 2) * pi, (2 + (10.5 / 365 * 2)) * pi,
                                            length.out = length(which_)))
  }
  
  # summary(df_raw_2$year_ws_sin)
  # summary(df_raw_2$year_sa_sin)

  # plot(df_raw_2$year_sa_sin)
  # plot(df_raw_2$year_ws_sin)
  
  ## day 
  df_raw_2$day_sin <- NA
  df_raw_2$hours_mins_ <- as.numeric(paste0(df_raw_2$hour, ".", df_raw_2$min))
  hours_mins_ <- unique(df_raw_2$hours_mins_)
  sin_seq <- sin(seq(1.5 * pi, 3.5 * pi,  length.out = length(hours_mins_)))
  
  for (i in 1:length(hours_mins_)){
    which_ <- which(df_raw_2$hours_mins_ == hours_mins_[i])
    df_raw_2$day_sin[which_] <- sin_seq[i]
  }
  
  # summary(df_raw_2$day_sin)
  df_raw_2 <- df_raw_2[, -which(names(df_raw_2) == "hours_mins_")]
  
  rm(hours_mins_, i, sin_seq, which_, years_)

  ## Fractional year ####
  ####  ####
  
#### ------------------------------------- ####
  ## 4 - Plausibilitytests ####
#### ------------------------------------- #### 

  df_raw_3 <- df_raw_2
  ## Tair -20 - 40 ####
  ## occurance of values outside thresholds 
  # length(df_raw_3$airT[which(df_raw_3$airT > 40 | df_raw_3$airT < -20)]) / nrow(df_raw_3)
  
  ## flag
  df_raw_3$airT[which(df_raw_3$airT > 40 | df_raw_3$airT < -20)] <- NA
  # summary(df_raw_3$airT)
  
  ## Relative Humidity 0 - 100 (105) ####
  # summary(df_raw_3$RH)
  
  ## occurance of values outside thresholds 
  # length(df_raw_3$RH[which(df_raw_3$RH > 105 | df_raw_3$RH <= 0)]) / nrow(df_raw_3)
  
  ## flag
  df_raw_3$RH[which(df_raw_3$RH > 105 | df_raw_3$RH <= 0)] <- NA
  df_raw_3$RH[which(df_raw_3$RH > 100 & df_raw_3$RH <= 105)] <- 100
  # summary(df_raw_3$RH)
  
  ## PPFDin 0 - 2730 ####
  ## occurance of values outside thresholds 
  # length(df_raw_3$PPFDin[which(df_raw_3$PPFDin > 2730)]) / nrow(df_raw_3)
  
  ## flag
  df_raw_3$PPFDin[which(df_raw_3$PPFDin < 0)] <- 0
  df_raw_3$PPFDin[which(df_raw_3$PPFDin > 2730)] <- NA
  # summary(df_raw_3$PPFDin)
  
  ## SWout ####
  ## occurance of values outside thresholds 
  # length(df_raw_3$SWout[which(df_raw_3$SWout <= 0)]) / nrow(df_raw_3)
  
  ## flag
  df_raw_3$SWout[which(df_raw_3$SWout <= 0)] <- NA
  # summary(df_raw_3$SWout)
  
  ## Windspeed ####
  ## occurance of values outside thresholds 
  # length(df_raw_3$WindSpeed[which(df_raw_3$WindSpeed < 0)]) / nrow(df_raw_3)
  
  df_raw_3$WindSpeed[which(df_raw_3$WindSpeed < 0)] <- NA
  ####  ####

#### ------------------------------------- ####
  ## 5 - Fill gaps of predictors ####
#### ------------------------------------- ####   
  
  ## function for detecting location and size of gaps ####
  DetectGaps <- function(df, maxsize = 12){
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
        position_[i] <- sum(rl$lengths[1:(w_gap[i] - 1)]) + 1
      }
      size_ <- rl$lengths[w_gap]
      df_retrun <- data.frame("position" = position_, "size" = size_)
    }
    
  }
  
  ## function for interpolating
  InterpolFun <- function(df, df_gaps){
    for (i in 1:nrow(df_gaps)){
      #print(i)
      val_x <- c(df_gaps[i, 1] - 1, df_gaps[i, 1] + df_gaps[i, 2])
      val_y <- c(df[(df_gaps[i, 1] - 1)], df[(df_gaps[i, 1] + df_gaps[i, 2])])
      
      linearMod <- lm(val_y ~ val_x)
      pred_x <- c(df_gaps[i, 1]:(df_gaps[i, 1] + df_gaps[i, 2] - 1))
      
      df[df_gaps[i, 1]:(df_gaps[i, 1] + df_gaps[i, 2] - 1)] <- predict(linearMod, newdata = data.frame(val_x = pred_x))
    }
    return(df)
  }
  
  ## fill gaps of comox dataset -> half hourly data ####
  df_temp_gaps <- DetectGaps(df = df_comox_2$temperature, 12)
  df_comox_2$temperature <- InterpolFun(df = df_comox_2$temperature, df_gaps = df_temp_gaps)
  
  df_rh_gaps <- DetectGaps(df = df_comox_2$relative_humidity, 12)
  df_comox_2$relative_humidity <- InterpolFun(df = df_comox_2$relative_humidity, df_gaps = df_rh_gaps)
  
  df_ws_gaps <- DetectGaps(df = df_comox_2$wind_speed, 12)
  df_comox_2$wind_speed <- InterpolFun(df = df_comox_2$wind_speed, df_gaps = df_ws_gaps)
  
  rm(df_temp_gaps, df_rh_gaps, df_ws_gaps)
  
  ## create dataset with all data ####
  df_merged <- merge(df_raw_3, df_comox_2[, c("dt", "temperature", "relative_humidity", "wind_speed")], by = "dt")
  
## Tair ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$airT))) / nrow(df_merged)
  ## gaps smaller 6 hours -> interpolating
  # summary(df_merged$airT)
  df_airt_gaps <- DetectGaps(df = df_merged$airT, 12)
  df_merged$airT <- InterpolFun(df = df_merged$airT, df_gaps = df_airt_gaps)
  
  ## GLM
  # hist(df_merged$airT)
  # plot(df_merged$airT ~ df_merged$temperature)
  glm_air <- glm(airT ~ TS_main + PPFDin + Rnet + temperature, data = df_merged)
  # summary(glm_air)
  
  ## pseudo r2
  # 1 - glm_air$deviance / glm_air$null.deviance # 0.95
  preds <- predict(glm_air, newdata = df_merged[which(is.na(df_merged$airT)), ])
  # summary(preds)
  df_merged[which(is.na(df_merged$airT)), "airT"] <- unname(preds)
  
  rm(preds, glm_air, df_airt_gaps)
  
## SWout ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$SWout))) / nrow(df_merged)
  
  # summary(df_merged$SWout)
  ## smal gaps < 6h interpolating
  df_swout_gaps <- DetectGaps(df = df_merged$SWout, 12)
  df_merged$SWout <- InterpolFun(df = df_merged$SWout, df_gaps = df_swout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$LWout)) == T)) # common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$RH)) == T)) # common gaps
  # length(which(which(is.na(df_merged$SWout)) %in% which(is.na(df_merged$CO2)) == T)) # common gaps
  
  ## Histogram
  # hist(df_merged$SWout[-which(is.na(df_merged$SWout))])
  
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
  glm_swout <- glm(log(SWout) ~ PPFDin + airT + day_sin + Rnet, data = df_merged[-which(is.na(df_merged$SWout)), ], 
                   family = gaussian(link = "identity"))
  # summary(glm_swout)

  ## pseudo r2
  # 1 - glm_swout$deviance / glm_swout$null.deviance # 0.26
  preds <- exp(predict(glm_swout, newdata = df_merged[which(is.na(df_merged$SWout)), ]))
  # summary(preds)
  df_merged[which(is.na(df_merged$SWout)), "SWout"] <- unname(preds)
  
  df_merged$SWout[which(df_merged$SWout > 800)] <- NA
  df_swout_gaps <- DetectGaps(df = df_merged$SWout, 12)
  df_merged$SWout <- InterpolFun(df = df_merged$SWout, df_gaps = df_swout_gaps)
  # summary(df_merged$SWout)
  
  rm(preds, glm_swout, df_swout_gaps)
  # rm(preds, glm_swout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_swout_gaps)
  
## LWout ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$LWout))) / nrow(df_merged)
  
  # summary(df_merged$LWout)
  ## smal gaps < 6h interpolating
  df_lwout_gaps <- DetectGaps(df = df_merged$LWout, 12)
  df_merged$LWout <- InterpolFun(df = df_merged$LWout, df_gaps = df_lwout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$SWout)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$LWout)) %in% which(is.na(df_merged$RH)) == T)) # common gaps
  
  ## Histogram
  # hist(df_merged$LWout[-which(is.na(df_merged$LWout))])
  
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
  
  glm_lwout <- glm(log(LWout) ~ PPFDin + airT + SWout + year_ws_sin + year_sa_sin + day_sin + Rnet, 
                   data = df_merged[-which(is.na(df_merged$LWout)), ], 
                   family = gaussian(link = "identity"))
  # summary(glm_lwout)
  
  ## pseudo r2
  # 1 - glm_lwout$deviance / glm_lwout$null.deviance # 0.98
  preds <- exp(predict(glm_lwout, newdata = df_merged[which(is.na(df_merged$LWout)), ]))
  # summary(preds)
  df_merged[which(is.na(df_merged$LWout)), "LWout"] <- unname(preds)
  
  rm(preds, glm_lwout, df_lwout_gaps)
  # rm(preds, glm_lwout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_lwout_gaps)
  
## Soil moisture Main ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Soil.moisture_main))) / nrow(df_merged)
  
  summary(df_merged$Soil.moisture_main)
  ## smal gaps < 6h interpolating
  df_swmain_gaps <- DetectGaps(df = df_merged$Soil.moisture_main, 12)
  df_merged$Soil.moisture_main <- InterpolFun(df = df_merged$Soil.moisture_main, df_gaps = df_swmain_gaps)
  
  ## Histogram
  # hist(df_merged$Soil.moisture_main[-which(is.na(df_merged$Soil.moisture_main))])
  
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
                 data = df_merged[-which(is.na(df_merged$Soil.moisture_main)), ], 
                 family = gaussian)
  # summary(glm_smm)
  # 1 - glm_smm$deviance / glm_smm$null.deviance # 0.77
  
  preds <- predict(glm_smm, newdata = df_merged[which(is.na(df_merged$Soil.moisture_main)), ])
  summary(preds)
  preds[which(preds < 0)] <- min(df_merged$Soil.moisture_main, na.rm = T)
  df_merged[which(is.na(df_merged$Soil.moisture_main)), "Soil.moisture_main"] <- unname(preds)
  
  # summary(df_merged$Soil.moisture_main)
  
  rm(preds, glm_smm, df_swmain_gaps)
  # rm(preds, glm_smm, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam, df_swmain_gaps)

## Soil moisture 1 ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Soil.moisture1))) / nrow(df_merged)
  
  # summary(df_merged$Soil.moisture1)
  
  ## smal gaps < 6h interpolating
  df_sm1_gaps <- DetectGaps(df = df_merged$Soil.moisture1, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm1 <- glm(Soil.moisture1 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df_merged[-which(is.na(df_merged$Soil.moisture1)), ], 
                 family = gaussian)
  # summary(glm_sm1)
  ## pseudo r2
  # 1 - glm_sm1$deviance / glm_sm1$null.deviance # 0.91
  
  preds <- predict(glm_sm1, newdata = df_merged[which(is.na(df_merged$Soil.moisture1)), ])
  # summary(preds)
  df_merged$Soil.moisture1[which(is.na(df_merged$Soil.moisture1))] <- unname(preds)
  
  # summary(df_merged$Soil.moisture1)
  
  rm(preds, glm_sm1, df_sm1_gaps)
  
## Soil moisture 2 ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Soil.moisture2))) / nrow(df_merged)
  
  # summary(df_merged$Soil.moisture2)
  
  ## smal gaps < 6h interpolating
  df_sm2_gaps <- DetectGaps(df = df_merged$Soil.moisture2, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm2 <- glm(Soil.moisture2 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df_merged[-which(is.na(df_merged$Soil.moisture2)), ], 
                 family = gaussian)
  # summary(glm_sm2)
  
  ## Pseudo r2
  # 1 - glm_sm2$deviance / glm_sm2$null.deviance # 0.97
  
  preds <- predict(glm_sm2, newdata = df_merged[which(is.na(df_merged$Soil.moisture2)), ])
  # summary(preds)
  df_merged$Soil.moisture2[which(is.na(df_merged$Soil.moisture2))] <- unname(preds)
  
  # summary(df_merged$Soil.moisture2)
  
  rm(preds, glm_sm2, df_sm2_gaps)

## Soil moisture 3 ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Soil.moisture3))) / nrow(df_merged)
  
  # summary(df_merged$Soil.moisture3)
  
  ## smal gaps < 6h interpolating
  df_sm3_gaps <- DetectGaps(df = df_merged$Soil.moisture3, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm3 <- glm(Soil.moisture3 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df_merged[-which(is.na(df_merged$Soil.moisture3)), ], 
                 family = gaussian)
  # summary(glm_sm3)
  ## Pseudo r2
  # 1 - glm_sm3$deviance / glm_sm3$null.deviance # 0.99
  
  preds <- predict(glm_sm3, newdata = df_merged[which(is.na(df_merged$Soil.moisture3)), ])
  # summary(preds)
  df_merged$Soil.moisture3[which(is.na(df_merged$Soil.moisture3))] <- unname(preds)
  
  # summary(df_merged$Soil.moisture3)
  
  rm(preds, glm_sm3, df_sm3_gaps)
  
## Soil moisture 4 ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Soil.moisture4))) / nrow(df_merged)
  
  # summary(df_merged$Soil.moisture4)
  
  ## smal gaps < 6h interpolating
  df_sm4_gaps <- DetectGaps(df = df_merged$Soil.moisture4, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm4 <- glm(Soil.moisture4 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df_merged[-which(is.na(df_merged$Soil.moisture4)), ], 
                 family = gaussian)
  # summary(glm_sm4)
  ## Pseudo r2
  # 1 - glm_sm4$deviance / glm_sm4$null.deviance # 0.92
  
  preds <- predict(glm_sm4, newdata = df_merged[which(is.na(df_merged$Soil.moisture4)), ])
  # summary(preds)
  df_merged$Soil.moisture4[which(is.na(df_merged$Soil.moisture4))] <- unname(preds)
  
  # summary(df_merged$Soil.moisture4)
  
  rm(preds, glm_sm4, df_sm4_gaps)
  
## LWin ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$LWin))) / nrow(df_merged)
  
  # summary(df_merged$LWin)
  ## smal gaps < 6h interpolating
  df_lwin_gaps <- DetectGaps(df = df_merged$LWin, 12)
  df_merged$LWin <- InterpolFun(df = df_merged$LWin, df_gaps = df_lwin_gaps)
  
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
  
  glm_lwin <- glm(LWin ~ PPFDin + airT + SWout + Soil.moisture_main + LWout + Rnet, 
                  data = df_merged[-which(is.na(df_merged$LWin)), ], 
                  family = gaussian(link = "identity"))
  # summary(glm_lwin)
  
  ## pseudo r2
  # 1 - glm_lwin$deviance / glm_lwin$null.deviance # 0.95
  preds <- predict(glm_lwin, newdata = df_merged[which(is.na(df_merged$LWin)), ])
  # summary(preds)
  preds[which(preds < 0)] <- NA
  df_merged[which(is.na(df_merged$LWin)), "LWin"] <- unname(preds)
  
  # small gaps fill again 
  df_lwin_gaps <- DetectGaps(df = df_merged$LWin, 12)
  df_merged$LWin <- InterpolFun(df = df_merged$LWin, df_gaps = df_lwin_gaps)
  
  # summary(df_merged$LWin)
  
  rm(preds, glm_lwin, fit_norm, fit_lnorm, fit_chi, fit_gam, df_lwin_gaps)
  # rm(preds, glm_lwin, df_lwin_gaps)
  
## Windspeed ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$WindSpeed))) / nrow(df_merged)
  
  # summary(df_merged$WindSpeed)
  ## smal gaps < 6h interpolating
  df_wind_gaps <- DetectGaps(df = df_merged$WindSpeed, 12)
  df_merged$WindSpeed <- InterpolFun(df = df_merged$WindSpeed, df_gaps = df_wind_gaps)
  
  ## Histogram
  # hist(df_merged$WindSpeed[-which(is.na(df_merged$WindSpeed))])
  
  ## GLM
  glm_ws <- glm(WindSpeed ~ airT + PPFDin + wind_speed, data = df_merged[-which(is.na(df_merged$WindSpeed)), ])
  # summary(glm_ws)
  
  ## pseudo r2
  # 1 - glm_ws$deviance / glm_ws$null.deviance # 0.11
  preds <- predict(glm_ws, newdata = df_merged[which(is.na(df_merged$WindSpeed)), ])
  # summary(preds)
  df_merged[which(is.na(df_merged$WindSpeed)), "WindSpeed"] <- unname(preds)
  
  rm(preds, glm_ws, df_wind_gaps)
  
## RH ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$RH))) / nrow(df_merged)
  
  ## check if significant correlated predictors have common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$LWout)) == T)) # 3% common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$Soil.moisture_main)) == T)) # common gaps
  # length(which(which(is.na(df_merged$RH)) %in% which(is.na(df_merged$CO2)) == T)) # common gaps
  
  ## Histogram 
  # hist(df_merged$RH) # beta distribution
  # plot(RH ~ relative_humidity, df_merged)
  # summary(df_merged$RH)
  
  ## Values need to be in range (0,1) for beta regression
  df_merged$RH <- df_merged$RH / 100
  df_merged$RH[which(df_merged$RH == 1)] <- 1 - 1e-10
  
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
  betareg_rh <- betareg(RH ~ airT + PPFDin + relative_humidity + h_last_precip, 
                        data = df_merged[-which(is.na(df_merged$RH)), ])
  # summary(betareg_rh, type = "pearson")
  ## pseudo r2 0.16
  
  preds <- predict(betareg_rh, newdata = df_merged[which(is.na(df_merged$RH)), ])
  # summary(preds)
  df_merged[which(is.na(df_merged$RH)),"RH"] <- unname(preds)
  
  rm(preds, betareg_rh)
  # rm(preds, betareg_rh, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam)
  
## Precipitation ####
  ## Missing data - NA 
  # length(which(is.na(df_merged$Precip))) / nrow(df_merged)
  
  # summary(df_merged$Precip)
  # # smal gaps < 6h interpolating
  # df_precip_gaps <- DetectGaps(df = df_merged$Precip, 12)
  # df_merged$Precip <- InterpolFun(df = df_merged$Precip, df_gaps = df_precip_gaps)
  # 
  # # Histogram
  # hist(df_merged$Precip[-which(is.na(df_merged$Precip))])
  # summary(df_merged$Precip[-which(is.na(df_merged$Precip))])
  
  # summary(df_merged)
  #### ####

#### ------------------------------------- ####
  ## 6 - Calculate SWin, mean TS, mean MS and fill gaps ####
#### ------------------------------------- ####   
  
  ## Short wave in ####
  df_merged$SWin <- df_merged$Rnet - (df_merged$LWin - df_merged$LWout) + df_merged$SWout
  
  ## occurance
  # (length(df_merged$SWin[which(df_merged$SWin < -5 )]) + length(df_merged$SWin[which(df_merged$SWin > 1300 )])) / nrow(df_merged)
  df_merged$SWin[which(df_merged$SWin < -5 )] <- NA
  df_merged$SWin[which(df_merged$SWin > 1300 )] <- NA
  df_merged$SWin[which(df_merged$SWin > -5 &  df_merged$SWin < 0 )] <- 0
  # summary(df_merged$SWin)
  # length(which(is.na(df_merged$SWin))) / nrow(df_merged) ## total ammount of gaps
  
  df_swin_gaps <- DetectGaps(df = df_merged$SWin, 12)
  df_merged$SWin <- InterpolFun(df = df_merged$SWin, df_gaps = df_swin_gaps)
  
  # hist(df_merged$SWin)  
  rm(df_swin_gaps)
  ## mean TS ####
  df_merged$TS_mean <- apply(df_merged[, 8:13], 1, mean, na.rm = T)
  # summary(df_merged$TS_mean)
  
  ## mean MS ####
  df_merged$MS_mean <- apply(df_merged[, 15:18], 1, mean, na.rm = T)
  #s ummary(df_merged$MS_mean)
  
  #### ####
  
#### ------------------------------------- ####
  ## 7 - Prepare Data for Respiration Model ####
#### ------------------------------------- ####  
  
  
  df_merged$NEE_cor <- df_merged$NEE_measure
  df_merged$NEE_cor[which(df_merged$ustar < 0.19)] <- NA 

  ## Extract Night and Day Data  / night data with PPFDin < 5####
  df_night <- df_merged[which(df_merged$PPFDin < 5), ]
  df_day <- df_merged[which(df_merged$PPFDin >= 5), ]
  
  # u* correction 
  # Jassal et al. 2009: 0.19 | Krishnan et al. 2009: 0.16 | Jassal et al. 2010: 0.19 
  # df_night$NEE_cor[df_night$ustar < 0.19] <- NA
  
  # data frame for model
  df_night_model <-  df_night[!is.na(df_night$NEE_cor), ]
  
  # data frame for NEE perdiction
  # df_night_pred <-  df_night[is.na(df_night$NEE_cor), ]
  
  df_pred <- rbind(df_day, df_night_pred_PPFD, df_night_pred)
  df_pred_complete <- df_pred[is.na(df_pred$NEE_cor), ]
  
  # summary(df_pred)
  
  df_night_model <- df_night_model[, -c(2:7, 31, 38:40, 44)]
  df_pred_complete <- df_pred_complete[, -c(2:7, 31, 38:40, 44)]
  
  # summary(df_night_model$NEE_cor)
  # sum(is.na(df_night$NEE_cor)) / nrow(df_night) ## 78 % gaps
  
  ## data frame with NNE != NA ####
  ## and without precip, pressure, lw, sw, co2
  
  # summary(df_night_model)
  # summary(df_pred_complete)
  
  # sort by date
  df_merged <- df_merged[order(df_merged$dt),]
  df_night_model <- df_night_model[order(df_night_model$dt), ]
  df_pred_complete <- df_pred_complete[order(df_pred_complete$dt), ]
  
  rm(df_night, df_day, df_night_pred, df_night_pred_PPFD, df_pred, df_comox_2, df_raw_2, df_raw_3)
  #### ####
  
#### ------------------------------------- ####
  ## 8 - Save data ####
#### ------------------------------------- ####  
  
  ## save ####
  save(df_night_model, df_pred_complete, df_merged, file = c(paste0(mypath, "/RData/df_model.RData")))
  