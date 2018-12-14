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
  df.raw.o <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver2.csv"))
  df.raw <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver3.csv"))
  
  df.raw.2 <- cbind(df.raw, "ustar" = df.raw.o$ustar, "NEE_measure" = df.raw.o$NEE_measure)
  
  ## Weather data from Comox airport ####
  df.comox <- read.csv(paste0(mypath, "/Daten/weatherstats_comox_hourly.csv"))
  
  ## Sun rise / set ####
  df.hel <- read.csv(paste0(mypath, "/Daten/sunsetrise.csv"),
                     header=T, sep=";")
  ####  ####
  
#### ------------------------------------- ####
  ## 1 - add time ####
#### ------------------------------------- ####
  
  ## Time format ####
  df.comox$dt <- as.POSIXct(substr(as.character(df.comox$date_time_local), 1, 16), format = "%Y-%m-%e %H:%M", tz = "America/Vancouver")
  
  df.comox <- df.comox[which(df.comox$dt > df.comox$dt[which(df.comox$dt == "2002-1-1 00:00:00")]),]             
  df.comox <- df.comox[which(df.comox$dt < df.comox$dt[which(df.comox$dt == "2017-1-1 02:00:00")]),]
  
  df.raw.2$dt <- paste0(df.raw.2$year, "-", df.raw.2$month, "-", 
                        df.raw.2$day, " ", df.raw.2$hour, ":", 
                        df.raw.2$min)
  df.raw.2$dt <- as.POSIXct(df.raw.2$dt, format = "%Y-%m-%d %H:%M", "GMT")
  attr(df.raw.2$dt, "tzone") <- "America/Vancouver"

  ####  ####
  
#### ------------------------------------- ####
  ## 2 - First check of raw data ####
#### ------------------------------------- ####  
  
  ## NaN to NA ####
  df.raw.2[df.raw.2 == "NaN"] <- NA

  ## expand df.comox to halfhour intervall ####
  dt <- paste0(substr(as.character(df.comox$date_time_local), 1, 14), "30:00")
  dt <- as.POSIXct(dt, format="%Y-%m-%e %H:%M", "America/Vancouver")
  df. <- df.comox
  df.[, ] <- NA
  df.$dt <- dt
  
  df.comox.2 <- rbind(df.comox, df.)
  df.comox.2 <- df.comox.2[order(df.comox.2$dt), ]
  
  rm(dt, df., df.raw.o, df.raw, df.comox.2)
  
  ## Check for Colinearity ####
  # summary(df.raw.2)
  # summary(df.comox.2)
  # M <- cor(df.raw.2[,7:30], use = "complete.obs")
  # corrplot.mixed(M)
  # df.raw <- df.raw[,-c(7:11,14:17)]

  ####  ####
  
#### ------------------------------------- ####
  ## 3 - Creating additional predictors ####
#### ------------------------------------- ####  
  
  ## Hours since last precipitation event ####
  # summary(df.raw.2$Precip)
  
  df.raw.2$h_last_precip <- NA
  df.raw.2$h_last_precip[which(df.raw.2$Precip >= 0.1)] <- 0
  
  w.1 <- w.2 <- which(df.raw.2$Precip > 0)
  w.2[2:length(w.2)] <- w.2[1:(length(w.2) - 1)]
  
  h.lastprec <- w.1 - w.2 - 1
  h.lastprec <- h.lastprec[-1]
  
  h.lastprec.2 <- h.lastprec[which(h.lastprec > 0)]
  w.1.1 <- w.1[which(h.lastprec > 0)]
  
  for (i in 1:length(h.lastprec.2)){
    df.raw.2$h_last_precip[(w.1.1[i] + 1):(w.1.1[i] + h.lastprec.2[i])] <- seq(0.5, h.lastprec.2[i] /2 , 0.5)
  }
  
  rm(w.1, w.2, h.lastprec, h.lastprec.2, w.1.1)
  # summary(df.raw.2$h_last_precip)
  
  ## Precipitation of last 30 days ####
  df.raw.2$precip_30d <- NA
  
  for (i in 30:nrow(df.raw.2)){
    df.raw.2$precip_30d[i] <- sum(df.raw.2$Precip[(i - 29):i], na.rm = T)  
  }
  
  which(df.raw.2$Precip[1:30] > 0)
  df.raw.2$precip_30d[1:10] <- 0
  df.raw.2$precip_30d[11:12] <- df.raw.2$precip_30d[10]
  df.raw.2$precip_30d[13:29] <- sum(df.raw.2$precip_30d[1:12], na.rum = T)
  
  # summary(df.raw.2$precip_30d)
  
  ## Sinus curves year and day ####
  ## year with peak in winter / summer and spring / autum
  years. <- unique(df.raw.2$year)
  df.raw.2$year_ws_sin <- NA
  df.raw.2$year_sa_sin <- NA
  
  for (i in 1:length(years_)){
    which. <- which(df.raw.2$year == years.[i])
    
    df.raw.2$year_ws_sin[which.] <- sin(seq((1.5 + (10.5 / 365 * 2)) * pi, (3.5 + (10.5 / 365 * 2)) * pi, 
                                            length.out = length(which.)))
    df.raw.2$year_sa_sin[which.] <- sin(seq((10.5 / 365 * 2) * pi, (2 + (10.5 / 365 * 2)) * pi,
                                            length.out = length(which.)))
  }
  
  # summary(df.raw.2$year_ws_sin)
  # summary(df.raw.2$year_sa_sin)

  # plot(df.raw.2$year_sa_sin)
  # plot(df.raw.2$year_ws_sin)
  
  ## day 
  df.raw.2$day_sin <- NA
  df.raw.2$hours_mins_ <- as.numeric(paste0(df.raw.2$hour, ".", df.raw.2$min))
  hours.mins <- unique(df.raw.2$hours_mins)
  sin.seq <- sin(seq(1.5 * pi, 3.5 * pi,  length.out = length(hours.mins)))
  
  for (i in 1:length(hours.mins)){
    which. <- which(df.raw.2$hours_mins_ == hours.mins[i])
    df.raw.2$day_sin[which_] <- sin.seq[i]
  }
  
  #summary(df.raw.2$day_sin)
  df.raw.2 <- df.raw.2[, -which(names(df.raw.2) == "hours_mins_")]
  
  rm(hours.mins, i, sin.seq, which., years.)

  ## Fractional year ####
  ####  ####
  
#### ------------------------------------- ####
  ## 4 - Plausibilitytests ####
#### ------------------------------------- #### 
  
  df.raw.3 <- df.raw.2
  ## Tair -20 - 40 ####
  df.raw.3$airT[which(df.raw.3$airT > 40 | df.raw.3$airT < -20)] <- NA
  # summary(df.raw.3$airT)
  
  ## Relative Humidity 0 - 100 (105) ####
  # summary(df.raw.3$RH)
  df.raw.3$RH[which(df.raw.3$RH > 105 | df.raw.3$RH <= 0)] <- NA
  df.raw.3$RH[which(df.raw.3$RH > 100 & df.raw.3$RH <= 105)] <- 100
  # summary(df.raw.3$RH)
  
  ## PPFDin 0 - 2730 ####
  df.raw.3$PPFDin[which(df.raw.3$PPFDin < 0)] <- 0
  df.raw.3$PPFDin[which(df.raw.3$PPFDin > 2730)] <- NA
  # summary(df.raw.3$PPFDin)
  
  ## SWout ####
  df.raw.3$SWout[which(df.raw.3$SWout <= 0)] <- NA
  # summary(df.raw.3$SWout)
  
  ## Windspeed ####
  df.raw.3$WindSpeed[which(df.raw.3$WindSpeed < 0)] <- NA
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
  df_temp_gaps <- DetectGaps(df = df.comox.2$temperature, 12)
  df.comox.2$temperature <- InterpolFun(df = df.comox.2$temperature, df_gaps = df_temp_gaps)
  
  df_rh_gaps <- DetectGaps(df = df.comox.2$relative_humidity, 12)
  df.comox.2$relative_humidity <- InterpolFun(df = df.comox.2$relative_humidity, df_gaps = df_rh_gaps)
  
  df_ws_gaps <- DetectGaps(df = df.comox.2$wind_speed, 12)
  df.comox.2$wind_speed <- InterpolFun(df = df.comox.2$wind_speed, df_gaps = df_ws_gaps)
  
  rm(df_temp_gaps, df_rh_gaps, df_ws_gaps)
  
  ## create dataset with all data ####
  df.merged <- merge(df.raw.3, df.comox.2[, c("dt", "temperature", "relative_humidity", "wind_speed")], by = "dt")
  
## Tair ####
  ## gaps smaller 6 hours -> interpolating
  # summary(df.merged$airT)
  df_airt_gaps <- DetectGaps(df = df.merged$airT, 12)
  df.merged$airT <- InterpolFun(df = df.merged$airT, df_gaps = df_airt_gaps)
  
  ## GLM
  # hist(df.merged$airT)
  # plot(df.merged$airT ~ df.merged$temperature)
  glm_air <- glm(airT ~ TS_main + PPFDin + Rnet + temperature, data = df.merged)
  # summary(glm_air)
  
  ## pseudo r2
  # 1 - glm_air$deviance / glm_air$null.deviance # 0.95
  preds <- predict(glm_air, newdata = df.merged[which(is.na(df.merged$airT)), ])
  # summary(preds)
  df.merged[which(is.na(df.merged$airT)), "airT"] <- unname(preds)
  
  rm(preds, glm_air, df_airt_gaps)
  
## SWout ####
  # summary(df.merged$SWout)
  ## smal gaps < 6h interpolating
  df_swout_gaps <- DetectGaps(df = df.merged$SWout, 12)
  df.merged$SWout <- InterpolFun(df = df.merged$SWout, df_gaps = df_swout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df.merged$SWout)) %in% which(is.na(df.merged$LWout)) == T)) # common gaps
  # length(which(which(is.na(df.merged$SWout)) %in% which(is.na(df.merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$SWout)) %in% which(is.na(df.merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$SWout)) %in% which(is.na(df.merged$RH)) == T)) # common gaps
  # length(which(which(is.na(df.merged$SWout)) %in% which(is.na(df.merged$CO2)) == T)) # common gaps
  
  ## Histogram
  # hist(df.merged$SWout[-which(is.na(df.merged$SWout))])
  
  ## Distribution
  # fit_norm <- fitdistr(df.merged$SWout[-which(is.na(df.merged$SWout))], "normal")
  # fit_lnorm <- fitdistr(df.merged$SWout[-which(is.na(df.merged$SWout))], "lognormal")
  # fit_chi <- fitdistr(df.merged$SWout[-which(is.na(df.merged$SWout))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df.merged$SWout[-which(is.na(df.merged$SWout))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm) # best distribution
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  ## GLM
  glm_swout <- glm(log(SWout) ~ PPFDin + airT + day_sin + Rnet, data = df.merged[-which(is.na(df.merged$SWout)), ], 
                   family = gaussian(link = "identity"))
  # summary(glm_swout)

  ## pseudo r2
  # 1 - glm_swout$deviance / glm_swout$null.deviance # 0.26
  preds <- exp(predict(glm_swout, newdata = df.merged[which(is.na(df.merged$SWout)), ]))
  # summary(preds)
  df.merged[which(is.na(df.merged$SWout)), "SWout"] <- unname(preds)
  
  df.merged$SWout[which(df.merged$SWout > 800)] <- NA
  df_swout_gaps <- DetectGaps(df = df.merged$SWout, 12)
  df.merged$SWout <- InterpolFun(df = df.merged$SWout, df_gaps = df_swout_gaps)
  # summary(df.merged$SWout)
  
  rm(preds, glm_swout, df_swout_gaps)
  # rm(preds, glm_swout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_swout_gaps)
  
## LWout ####
  # summary(df.merged$LWout)
  ## smal gaps < 6h interpolating
  df_lwout_gaps <- DetectGaps(df = df.merged$LWout, 12)
  df.merged$LWout <- InterpolFun(df = df.merged$LWout, df_gaps = df_lwout_gaps)
  
  ## check if highly correlated predictors have common gaps
  # length(which(which(is.na(df.merged$LWout)) %in% which(is.na(df.merged$SWout)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$LWout)) %in% which(is.na(df.merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$LWout)) %in% which(is.na(df.merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$LWout)) %in% which(is.na(df.merged$RH)) == T)) # common gaps
  
  ## Histogram
  # hist(df.merged$LWout[-which(is.na(df.merged$LWout))])
  
  ## Distribution
  # fit_norm <- fitdistr(df.merged$LWout[-which(is.na(df.merged$LWout))], "normal")
  # fit_lnorm <- fitdistr(df.merged$LWout[-which(is.na(df.merged$LWout))], "lognormal")
  # fit_chi <- fitdistr(df.merged$LWout[-which(is.na(df.merged$LWout))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df.merged$LWout[-which(is.na(df.merged$LWout))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm) # best distribution
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  glm_lwout <- glm(log(LWout) ~ PPFDin + airT + SWout + year_ws_sin + year_sa_sin + day_sin + Rnet, 
                   data = df.merged[-which(is.na(df.merged$LWout)), ], 
                   family = gaussian(link = "identity"))
  # summary(glm_lwout)
  
  ## pseudo r2
  # 1 - glm_lwout$deviance / glm_lwout$null.deviance # 0.98
  preds <- exp(predict(glm_lwout, newdata = df.merged[which(is.na(df.merged$LWout)), ]))
  # summary(preds)
  df.merged[which(is.na(df.merged$LWout)), "LWout"] <- unname(preds)
  
  rm(preds, glm_lwout, df_lwout_gaps)
  # rm(preds, glm_lwout, fit_norm, fit_lnorm, fit_chi, fit_gam, df_lwout_gaps)
  
## Soil moisture Main ####
  summary(df.merged$Soil.moisture_main)
  ## smal gaps < 6h interpolating
  df_swmain_gaps <- DetectGaps(df = df.merged$Soil.moisture_main, 12)
  df.merged$Soil.moisture_main <- InterpolFun(df = df.merged$Soil.moisture_main, df_gaps = df_swmain_gaps)
  
  ## Histogram
  # hist(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))])
  
  ## Distribution
  # fit_norm <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "normal")
  # fit_lnorm <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "lognormal")
  # fit_beta <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "beta", 
  #                      start = list(shape1 = 1, shape2 = 1), lower=c(0.1))
  # fit_weibull <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "weibull", 
  #                         start = list(scale = 1, shape = 0.1), lower=c(0.1))
  # fit_chi <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "chi-squared", 
  #                     start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df.merged$Soil.moisture_main[-which(is.na(df.merged$Soil.moisture_main))], "gamma", 
  #                     start = list(shape = 1, rate = 0.1), lower=c(0,0))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm)
  # AIC(fit_beta)
  # AIC(fit_weibull) # Best Distribution, but normal distribution is used due to better visual fit
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  ## Histogram with distributions
  # hist(df.merged$Soil.moisture_main, freq=F, las=1, xlim = c(0,0.6))
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
                 data = df.merged[-which(is.na(df.merged$Soil.moisture_main)), ], 
                 family = gaussian)
  # summary(glm_smm)
  # 1 - glm_smm$deviance / glm_smm$null.deviance # 0.77
  
  preds <- predict(glm_smm, newdata = df.merged[which(is.na(df.merged$Soil.moisture_main)), ])
  summary(preds)
  preds[which(preds < 0)] <- min(df.merged$Soil.moisture_main, na.rm = T)
  df.merged[which(is.na(df.merged$Soil.moisture_main)), "Soil.moisture_main"] <- unname(preds)
  
  # summary(df.merged$Soil.moisture_main)
  
  rm(preds, glm_smm, df_swmain_gaps)
  # rm(preds, glm_smm, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam, df_swmain_gaps)

## Soil moisture 1 ####
  # summary(df.merged$Soil.moisture1)
  
  ## smal gaps < 6h interpolating
  df_sm1_gaps <- DetectGaps(df = df.merged$Soil.moisture1, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm1 <- glm(Soil.moisture1 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df.merged[-which(is.na(df.merged$Soil.moisture1)), ], 
                 family = gaussian)
  # summary(glm_sm1)
  ## pseudo r2
  # 1 - glm_sm1$deviance / glm_sm1$null.deviance # 0.91
  
  preds <- predict(glm_sm1, newdata = df.merged[which(is.na(df.merged$Soil.moisture1)), ])
  # summary(preds)
  df.merged$Soil.moisture1[which(is.na(df.merged$Soil.moisture1))] <- unname(preds)
  
  # summary(df.merged$Soil.moisture1)
  
  rm(preds, glm_sm1, df_sm1_gaps)
  
## Soil moisture 2 ####
  # summary(df.merged$Soil.moisture2)
  
  ## smal gaps < 6h interpolating
  df_sm2_gaps <- DetectGaps(df = df.merged$Soil.moisture2, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm2 <- glm(Soil.moisture2 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df.merged[-which(is.na(df.merged$Soil.moisture2)), ], 
                 family = gaussian)
  # summary(glm_sm2)
  
  ## Pseudo r2
  # 1 - glm_sm2$deviance / glm_sm2$null.deviance # 0.97
  
  preds <- predict(glm_sm2, newdata = df.merged[which(is.na(df.merged$Soil.moisture2)), ])
  # summary(preds)
  df.merged$Soil.moisture2[which(is.na(df.merged$Soil.moisture2))] <- unname(preds)
  
  # summary(df.merged$Soil.moisture2)
  
  rm(preds, glm_sm2, df_sm2_gaps)

## Soil moisture 3 ####
  # summary(df.merged$Soil.moisture3)
  
  ## smal gaps < 6h interpolating
  df_sm3_gaps <- DetectGaps(df = df.merged$Soil.moisture3, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm3 <- glm(Soil.moisture3 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df.merged[-which(is.na(df.merged$Soil.moisture3)), ], 
                 family = gaussian)
  # summary(glm_sm3)
  ## Pseudo r2
  # 1 - glm_sm3$deviance / glm_sm3$null.deviance # 0.99
  
  preds <- predict(glm_sm3, newdata = df.merged[which(is.na(df.merged$Soil.moisture3)), ])
  # summary(preds)
  df.merged$Soil.moisture3[which(is.na(df.merged$Soil.moisture3))] <- unname(preds)
  
  # summary(df.merged$Soil.moisture3)
  
  rm(preds, glm_sm3, df_sm3_gaps)
  
## Soil moisture 4 ####
  # summary(df.merged$Soil.moisture4)
  
  ## smal gaps < 6h interpolating
  df_sm4_gaps <- DetectGaps(df = df.merged$Soil.moisture4, 12)
  
  ## Gaussianregrssion fitting and predicting
  glm_sm4 <- glm(Soil.moisture4 ~ airT + Ts6 + PPFDin + SWout + LWout + precip_30d + Soil.moisture_main, 
                 data = df.merged[-which(is.na(df.merged$Soil.moisture4)), ], 
                 family = gaussian)
  # summary(glm_sm4)
  ## Pseudo r2
  # 1 - glm_sm4$deviance / glm_sm4$null.deviance # 0.92
  
  preds <- predict(glm_sm4, newdata = df.merged[which(is.na(df.merged$Soil.moisture4)), ])
  # summary(preds)
  df.merged$Soil.moisture4[which(is.na(df.merged$Soil.moisture4))] <- unname(preds)
  
  # summary(df.merged$Soil.moisture4)
  
  rm(preds, glm_sm4, df_sm4_gaps)
  
## LWin ####
  # summary(df.merged$LWin)
  ## smal gaps < 6h interpolating
  df_lwin_gaps <- DetectGaps(df = df.merged$LWin, 12)
  df.merged$LWin <- InterpolFun(df = df.merged$LWin, df_gaps = df_lwin_gaps)
  
  ## Histogram
  #hist(df.merged$LWin[-which(is.na(df.merged$LWin))])
  
  ## Distribution
  # fit_norm <- fitdistr(df.merged$LWin[-which(is.na(df.merged$LWin))], "normal")
  # fit_lnorm <- fitdistr(df.merged$LWin[-which(is.na(df.merged$LWin))], "lognormal")
  # fit_chi <- fitdistr(df.merged$LWin[-which(is.na(df.merged$LWin))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df.merged$LWin[-which(is.na(df.merged$LWin))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0.1))
  # 
  # AIC(fit_norm) # best distribution
  # AIC(fit_lnorm) 
  # AIC(fit_chi)
  # AIC(fit_gam)
  
  glm_lwin <- glm(LWin ~ PPFDin + airT + SWout + Soil.moisture_main + LWout + Rnet, 
                  data = df.merged[-which(is.na(df.merged$LWin)), ], 
                  family = gaussian(link = "identity"))
  # summary(glm_lwin)
  
  ## pseudo r2
  # 1 - glm_lwin$deviance / glm_lwin$null.deviance # 0.95
  preds <- predict(glm_lwin, newdata = df.merged[which(is.na(df.merged$LWin)), ])
  # summary(preds)
  preds[which(preds < 0)] <- NA
  df.merged[which(is.na(df.merged$LWin)), "LWin"] <- unname(preds)
  
  # small gaps fill again 
  df_lwin_gaps <- DetectGaps(df = df.merged$LWin, 12)
  df.merged$LWin <- InterpolFun(df = df.merged$LWin, df_gaps = df_lwin_gaps)
  
  # summary(df.merged$LWin)
  
  rm(preds, glm_lwin, fit_norm, fit_lnorm, fit_chi, fit_gam, df_lwin_gaps)
  # rm(preds, glm_lwin, df_lwin_gaps)
  
## Windspeed ####
  # summary(df.merged$WindSpeed)
  ## smal gaps < 6h interpolating
  df_wind_gaps <- DetectGaps(df = df.merged$WindSpeed, 12)
  df.merged$WindSpeed <- InterpolFun(df = df.merged$WindSpeed, df_gaps = df_wind_gaps)
  
  ## Histogram
  # hist(df.merged$WindSpeed[-which(is.na(df.merged$WindSpeed))])
  
  ## GLM
  glm_ws <- glm(WindSpeed ~ airT + PPFDin + wind_speed, data = df.merged[-which(is.na(df.merged$WindSpeed)), ])
  # summary(glm_ws)
  
  ## pseudo r2
  # 1 - glm_ws$deviance / glm_ws$null.deviance # 0.11
  preds <- predict(glm_ws, newdata = df.merged[which(is.na(df.merged$WindSpeed)), ])
  # summary(preds)
  df.merged[which(is.na(df.merged$WindSpeed)), "WindSpeed"] <- unname(preds)
  
  rm(preds, glm_ws, df_wind_gaps)
  
## RH ####
  ## check if significant correlated predictors have common gaps
  # length(which(which(is.na(df.merged$RH)) %in% which(is.na(df.merged$LWout)) == T)) # 3% common gaps
  # length(which(which(is.na(df.merged$RH)) %in% which(is.na(df.merged$PPFDin)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$RH)) %in% which(is.na(df.merged$airT)) == T)) # no common gaps
  # length(which(which(is.na(df.merged$RH)) %in% which(is.na(df.merged$Soil.moisture_main)) == T)) # common gaps
  # length(which(which(is.na(df.merged$RH)) %in% which(is.na(df.merged$CO2)) == T)) # common gaps
  
  ## Histogram 
  # hist(df.merged$RH) # beta distribution
  # plot(RH ~ relative_humidity, df.merged)
  # summary(df.merged$RH)
  
  ## Values need to be in range (0,1) for beta regression
  df.merged$RH <- df.merged$RH / 100
  df.merged$RH[which(df.merged$RH == 1)] <- 1 - 1e-10
  
  ## Check distributions
  # fit_norm <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "normal")
  # fit_lnorm <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "lognormal")
  # fit_beta <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "beta", start = list(shape1 = 1, shape2 = 1), lower=c(0,0))
  # fit_weibull <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "weibull", start = list(scale = 1, shape = 1), lower=c(0,0))
  # fit_chi <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "chi-squared", start=list(df=3), method="Brent", lower=0.1, upper=100)
  # fit_gam <- fitdistr(df.merged$RH[-which(is.na(df.merged$RH))], "gamma", start = list(shape = 1, rate = 0.1), lower=c(0,0))
  # 
  # AIC(fit_norm)
  # AIC(fit_lnorm)
  # AIC(fit_beta) ## Best distribution
  # AIC(fit_weibull)
  # AIC(fit_chi)
  # AIC(fit_gam)
  # 
  # hist(df.merged$RH[-which(is.na(df.merged$RH))], freq=F, las=1, xlim = c(0,2))
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
                        data = df.merged[-which(is.na(df.merged$RH)), ])
  # summary(betareg_rh, type = "pearson")
  ## pseudo r2 0.16
  
  preds <- predict(betareg_rh, newdata = df.merged[which(is.na(df.merged$RH)), ])
  # summary(preds)
  df.merged[which(is.na(df.merged$RH)),"RH"] <- unname(preds)
  
  rm(preds, betareg_rh, fit_gam)
  # rm(preds, betareg_rh, fit_norm, fit_lnorm, fit_beta, fit_weibull, fit_chi, fit_gam)
  
## Precipitation ####
  # summary(df.merged$Precip)
  # # smal gaps < 6h interpolating
  # df_precip_gaps <- DetectGaps(df = df.merged$Precip, 12)
  # df.merged$Precip <- InterpolFun(df = df.merged$Precip, df_gaps = df_precip_gaps)
  # 
  # # Histogram
  # hist(df.merged$Precip[-which(is.na(df.merged$Precip))])
  # summary(df.merged$Precip[-which(is.na(df.merged$Precip))])
  
  #summary(df.merged)
  #### ####

#### ------------------------------------- ####
  ## 6 - Calculate SWin, mean TS, mean MS and fill gaps ####
#### ------------------------------------- ####   
  ## Short wave in ####
  df.merged$SWin <- df.merged$Rnet - (df.merged$LWin - df.merged$LWout) + df.merged$SWout
  df.merged$SWin[which(df.merged$SWin < -5 )] <- NA
  df.merged$SWin[which(df.merged$SWin > 1300 )] <- NA
  df.merged$SWin[which(df.merged$SWin > -5 &  df.merged$SWin < 0 )] <- 0
  # summary(df.merged$SWin)
  
  df.swin.gaps <- DetectGaps(df = df.merged$SWin, 12)
  df.merged$SWin <- InterpolFun(df = df.merged$SWin, df_gaps = df.swin.gaps)
  
  # hist(df.merged$SWin)  
  rm(df.swin.gaps)
  ## mean TS ####
  df.merged$TS_mean <- apply(df.merged[, 8:13], 1, mean, na.rm = T)
  # summary(df.merged$TS_mean)
  
  ## mean MS ####
  df.merged$MS_mean <- apply(df.merged[, 15:18], 1, mean, na.rm = T)
  #s ummary(df.merged$MS_mean)
  
  #### ####
  
#### ------------------------------------- ####
  ## 7 - Sun rise / set ####
#### ------------------------------------- ####    
  
  ## Sun rise / set ####
  df.hel$Datum <- as.character(df.hel$Datum)
  df.hel$Sunrise <- as.character(df.hel$Sunrise)
  df.hel$Sunset <- as.character(df.hel$Sunset)
  
  df.hel.2 <- data.frame(df.hel, as.numeric(do.call(rbind, strsplit(df.hel$Datum,' '))[, 1]))
  names(df.hel.2)[5] <- "day"
  
  df.hel.2 <- data.frame(df.hel.2, as.numeric(do.call(rbind, strsplit(df.hel$Sunset,':'))[, 1]))
  df.hel.2 <- data.frame(df.hel.2, as.numeric(do.call(rbind, strsplit(df.hel$Sunset,':'))[, 2]))
  names(df.hel.2)[6:7] <- paste("set", c("h","m"), sep = "_")
  
  df.hel.2 <- data.frame(df.hel.2, as.numeric(do.call(rbind, strsplit(df.hel$Sunrise,':'))[, 1]))
  df.hel.2 <- data.frame(df.hel.2, as.numeric(do.call(rbind, strsplit(df.hel$Sunrise,':'))[, 2]))
  names(df.hel.2)[8:9] <- paste("rise", c("h","m"), sep = "_")
  
  df.hel.2 <- df.hel.2[, -c(2:4)]
  
  ## flag night time data ####
  FlagNightData <- function(df_flux, df_sun){
    hm_flux <- as.numeric(format(df_flux$dt, "%H")) * 60 + as.numeric(format(df_flux$dt, "%M"))
    hm_sun_r <- df_sun$rise_h * 60 + df_sun$rise_m
    hm_sun_s <- df_sun$set_h * 60 + df_sun$set_m
    
    flag_ <- rep(0, nrow(df_flux))
    
    for (i in 1:12){
      w_m_raw <- which(as.numeric(format(df_flux$dt, "%m")) == i) # which month
      w_m_sun <- which(df_sun$month == i) # which month
      
      for (j in 1:length(w_m_sun)){
        w_d_raw <- which(as.numeric(format(df_flux$dt, "%d")) == j) # which day
        w_raw <- w_m_raw[which(w_m_raw %in% w_d_raw)] # match of month and day
        
        flag_[w_raw][hm_flux[w_raw] < hm_sun_r[w_m_sun][j] | hm_flux[w_raw] > hm_sun_s[w_m_sun][j]] <- 1
      }
    }
    return(flag_)
  }
  
  df.merged$flag_night <- FlagNightData(df_flux = df.merged, df_sun = df.hel.2)
  
  rm(df.hel, df.hel.2, FlagNightData)
  
  #### ####

#### ------------------------------------- ####
  ## 8 - Prepare Data for Respiration Model ####
#### ------------------------------------- ####  
  
  ## Extract Night Data ####
  df.night <- df.merged[df.merged$flag_night == 1, -c(2:7, 38:40, 44)]
  df.night <- df.night[-which(df.night$PPFDin > 5), ]
  
  ## u* correction ####
  # Jassal et al. 2009: 0.19 | Krishnan et al. 2009: 0.16 | Jassal et al. 2010: 0.19 
  df.night$NEE_cor <- df.night$NEE_measure
  df.night$NEE_cor[df.night$ustar < 0.19] <- NA
  df.night <- df.night[, -25]
  # summary(df.night$NEE_measure)
  # summary(df.night$NEE_cor)
  # sum(is.na(df.night$NEE_cor)) / nrow(df.night) ## 78 % gaps
  
  ## data frame with NNE != NA ####
  ## and without precip, pressure, lw, sw, co2
  df.night.model <-  df.night[!is.na(df.night$NEE_cor), ]
  df.night.pred <-  df.night[is.na(df.night$NEE_cor), ]
  
  # summary(df.night_model)
  # summary(df.night_pred)
  
  rm(df.night, df.merged)
  #### ####
  
#### ------------------------------------- ####
  ## 9 - Save data ####
#### ------------------------------------- ####  
  ## save ####
  save(df.night.model, df.night.pred, file = c(paste0(mypath, "/RData/df_model.RData")))
  