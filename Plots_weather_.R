#### ------------------------------------------- ##
#### Code for weather plots ##
#### ------------------------------------------- ##

  ## packages
  library(dplyr)
  library(RColorBrewer)
  library(readr)
  
  mypath <- getwd()
  
#### ----------------------- ##
#### 0 - Weather Plots  ##
#### ----------------------- ##
  
#### load data ####  
  df_comox <- read.csv(paste0(mypath, "/Daten/weatherstats_comox_hourly.csv"))
  df_comox_prec <- read_table2("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Daten_und_Auswertung/Daten/Mappe2.csv", 
                               col_names = FALSE)
  
  
  df_comox_prec <- df_comox_prec[, -c(4, 5)]
  colnames(df_comox_prec) <- c("month", "year", "prec")
  df_comox_prec$year <- substr(df_comox_prec$year, 1, nchar(df_comox_prec$year) - 1)
  df_comox_prec$year <- as.numeric(df_comox_prec$year)
  
  for (i in 2:16){
    df_comox_prec$year[which(df_comox_prec$year == i)] <- as.numeric(2000 + i)
  }
  
  df_comox_prec$month_n <- 12:1
  
#### aggregate and sum / mean ####
  df_comox$year  <- as.integer(substr(as.character(df_comox$date_time_local), 1, 4))
  
  # aggregate by year
  df_weather_c_y <- df_comox %>%
    group_by(year) %>%
    summarize(mean_t = mean(temperature, na.rm = TRUE))
  df_weather_c_y <- df_weather_c_y[-c(1:6, 22:23), ]
  
  df_weather_c_y_p <- df_comox_prec %>%
    group_by(year) %>%
    summarize(prec = sum(prec, na.rm = TRUE))
  
  df_weather_c_y <- cbind(df_weather_c_y, "prec" = df_weather_c_y_p$prec)
  
  # aggregate by month
  df_comox$month  <- as.integer(substr(df_comox$date_time_local, 6, 7))
  
  df_weather_c_m <- df_comox %>%
    group_by(month) %>%
    summarize(mean_t = mean(temperature, na.rm = TRUE))
  
  df_weather_c_m_p <- df_comox_prec %>%
    group_by(month_n) %>%
    summarize(prec = mean(prec, na.rm = TRUE))
  
  df_weather_c_m <- cbind(df_weather_c_m, "prec" = df_weather_c_m_p$prec)
  
#### comox normals ####
  date <- c(1:12)
  mean_t <- c(3.9, 4.3, 6.1, 8.8, 12.4, 15.5, 18.0, 17.9, 14.5, 9.5, 5.7, 3.5)	
  mean_p <- c(159.1, 107.8, 95.7, 64.4, 45.6, 42.8, 26.7, 29.2, 41.8, 122.7, 191.9, 168.9)
  month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  normals <- data.frame(date, mean_t, mean_p)
  
  df_weather_c_y$dif_t <- df_weather_c_y$mean_t - 10
  df_weather_c_m$dif_t <- df_weather_c_m$mean_t - normals$mean_t
  df_weather_c_y$dif_p <- df_weather_c_y$prec - 1153.6
  df_weather_c_m$dif_p <- df_weather_c_m$prec - normals$mean_p
  
#### define cex for plots ####
  cex_fig = 3
  cex_axis = 2.7
  cex_lab = 1.5
  cex_legend <- 2
  familiy_fig = "Times"
  
#### Colors ####
  ## colors precipitation
  ii_y_p <- cut(df_weather_c_y$dif_p, breaks = seq(-550, 550, len = 100), 
              include.lowest = TRUE)
  
  ii_m_p <- cut(df_weather_c_m$dif_p, breaks = seq(-90, 90, len = 100), 
              include.lowest = TRUE)
  
  # Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  colors_y_p <- colorRampPalette(c("orangered", "white", "lightblue"))(99)[ii_y_p]
  colors_m_p <- colorRampPalette(c("orangered", "white", "lightblue"))(99)[ii_m_p]
  
  ## colors temperature
  ii_y_t <- cut(df_weather_c_y$dif_t, breaks = seq(-1.5, 1.5, len = 100), 
                include.lowest = TRUE)
  
  ii_m_t <- cut(df_weather_c_m$dif_t, breaks = seq(-0.4, 0.4, len = 100), 
                include.lowest = TRUE)
  
  # Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  colors_y_t <- colorRampPalette(c("lightblue", "white", "orangered"))(99)[ii_y_t]
  colors_m_t <- colorRampPalette(c("lightblue", "white", "orangered"))(99)[ii_m_t]
  
#### Plots ####
  ## Monthly temperature and precipitation ####
  pdf("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Latex/Plots/Temp_prec_month.pdf",
            family = "Times", width = 16, height = 8, bg = "white")

  par(family = familiy_fig, mfrow = c(3, 1))

  ## firgure for the diff of precipitation
  par(mar = c(1, 7, 5 , 7.5))
  
  # plot
  barplot(df_weather_c_m$dif_p, axes = F, xaxt = 'n', beside = TRUE, 
          las = 1, col = colors_m_p,
          yaxt = 'n', xaxs = "i",
          ylim =c(-20, 40))
  text(x = 0.5, y = 30, labels = "a)", cex = cex_fig)
  abline(h = seq(-20, 40, 10), lty = 3, col = "grey30")
  abline(h = 0)
  axis(3, at = seq(0.75, 13.9, length.out = 12), labels = month, cex.axis = cex_fig)
  axis(2, at = seq(-20, 40, 10), labels = c(-20, "", 0, "", 20, "", 40), las = 2, cex.axis = cex_axis)
  mtext(text = expression(Delta*'precip'*' [mm]'), side = 2, line = 5, cex = cex_legend)
  
  ## figure for the 30year mean
  par(mar = c(1, 7, 0, 7.5))
  
  # precipitation barplot
  barplot(mean_p, space = 0, las = 1, xaxs = "i", 
          ylab = NA, yaxt = "n", col = "steelblue3", ylim = c(0, 300), density = 10) # , ylim = c(0,precp_ylab)
  text(x = 0.3, y = 240, labels = "b)", cex = cex_fig)
  
  # add the 15 year average
  points(df_weather_c_m$prec, x = seq(0.5, 11.5, 1), ylim = c(0, 85), cex = cex_fig, 
         col = "blue")
  lines(df_weather_c_m$prec,x = seq(0.5, 11.5, 1), xaxs ="i", type = "l", 
          ylab = NA,  yaxt = "n", col = "blue", ylim = c(0, 85), lty = 2, lwd = 3)
  abline(h = c(50, 100, 150, 200, 250),lty = 3, col = "grey30")
  axis(2, at = seq(0, 250, 50), labels = c(0, "", 100, "", 200, ""), las = 2,
       cex.axis = cex_axis)
  mtext(text = "precip [mm]", side = 2, line = 5, cex = cex_legend)
  
  # air temp
  par(mar=c(0, 7, 0, 7.5), new = T)
  plot(mean_t, type = "l", yaxt = "n", xaxt = "n", xlab = NA, ylab = NA,
       ylim = c(0, 20), lwd = 3, col = "firebrick", frame.plot = F)
  points(mean_t, xlab = NA, ylab = NA, cex = cex_fig,
         ylim = c(0, 20), lwd = 3, col = "firebrick")
  lines(df_weather_c_m$mean_t, type = "l", lty = 2, col = "red", lwd = 3)
  legend(x = 0.8, y = 22, legend = c("15 year mean"), lty = c(2),
         col = c("blue"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  legend(x = 3, y = 22, legend = c("30 year mean", "15 year mean"), lty = c(1, 2), 
         col = c("firebrick", "red"), bg = F, bty = "n", cex = cex_axis, lwd = c(3, 3))
  axis(4, at = seq(0, 20, 5), labels = seq(0, 20, 5), las = 2,  cex.axis = cex_axis)
  mtext(text = expression("T"[a]*" [°C]"), side = 4, line = 6, cex = cex_legend)
  
  ## firgure for the diff of temperature
  par(mar = c(5, 7, 0, 7.5))
  
  barplot(df_weather_c_m$dif_t, beside = TRUE, axes = F, xaxt = 'n', 
          las = 1, col = colors_m_t,
          yaxt ='n', cex.names = 1.3, xaxs = "i", ylab = NA,
          ylim = c(-0.2, 0.5))
  text(x = 0.5, y = 0.4, labels = "c)", cex = cex_fig)
  abline(h = c(-0.2, 0, 0.2, 0.4), lty = 3, col = "grey30")
  axis(1, at = seq(0.75, 13.9, length.out = 12), labels = rep("", 12), 
       cex.axis = cex_fig)
  axis(1, at = seq(0.75, 13.9, length.out = 12), labels = month, 
       cex.axis = cex_fig, line = 0.7, lwd = 0)
  axis(4, at = c(-0.2, 0, 0.2, 0.4), las = 1,  cex.axis = cex_axis)
  mtext(text = expression(Delta*'T'[a]*' [°C]'), side = 4, line = 6, cex = cex_legend)
  abline(h = 0)
  
  ## box
  par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(3.3, 4.6, 3.3, 4.95))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, add = T, ylim = c(-2, -1))
  
  dev.off()
  
  ## Annual temperature and precipitation ####
  pdf("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Latex/Plots/Temp_prec_year.pdf",
      family = "Times", width = 16, height = 8, bg = "white")
  
  par(family = familiy_fig, mfrow = c(3, 1))
  
  ## figure for the diff of precipitation
  par(mar = c(2, 8, 5, 7.5))
  barplot(df_weather_c_y$dif_p, axes = F, xaxt = 'n', beside = TRUE, 
          las = 1, col = colors_y_p,
          yaxt = 'n', xaxs = "i",
          ylim =c(-400, 520))
  text(x = 0.5, y = 400, labels = "a)", cex = cex_fig)
  abline(h = seq(-400, 500, 200), lty = 3, col = "grey30")
  abline(h = 0)
  axis(3, at = seq(0.75, 17.5, length.out = 15), labels = df_weather_c_y$year, cex.axis = cex_fig)
  axis(2, at = seq(-400, 500, 200), labels = seq(-400, 500, 200), las = 2, cex.axis = cex_axis)
  mtext(text = expression(Delta*'precip'*' [mm]'), side = 2, line = 5.4, cex = cex_legend)
  
  ## figure for the 15 years
  # Precipitation
  par(mar = c(1.5, 8, 0, 7.5))
  plot(df_weather_c_y$prec, las = 1, lwd = 3, xaxt = "n", cex = cex_fig, axes = F,
          ylab = NA, yaxt = "n", col = "blue", ylim = c(0, 1700))
  lines(df_weather_c_y$prec, xaxs ="i", type = "l", 
        ylab = NA,  yaxt = "n", col = "blue", ylim = c(0, 85), lty = 1, lwd = 3)
  abline(h = 1153.6, lty = 2, col = "steelblue3", lwd = 3)
  abline(h = 0)
  abline(h = c(0, 500, 1000, 1500), lty = 3, col = "grey30")
  text(x = 0.7, y = 1500, labels = "b)", cex = cex_fig)
  axis(2, at = c(0, 500, 1000, 1500), labels = c(0, 500, 1000, 1500), las = 2,
       cex.axis = cex_axis)
  mtext(text = "precip [mm]", side = 2, line = 5.8, cex = cex_legend)
  
  # Temperature
  par(mar=c(0, 8, 1, 7.5), new = T)
  plot(df_weather_c_y$mean_t, type = "l", yaxt = "n", xaxt = "n", xlab = NA, ylab = NA,
       ylim = c(9.2, 11.5), lwd = 3, col = "firebrick", frame.plot = F)
  points(df_weather_c_y$mean_t, xlab = NA, ylab = NA, cex = cex_fig,
         ylim = c(0, 20), lwd = 3, col = "firebrick")
  abline(h = 10, lty = 2, col = "red", lwd = 3)
  axis(4, at = seq(9.5, 11.5, 1), labels = seq(9.5, 11.5, 1), las = 2,  cex.axis = cex_axis)
  mtext(text = expression("T"[a]*" [°C]"), side = 4, line = 6.5, cex = cex_legend)
  
  ## figure for the diff of temperature
  par(mar = c(5, 8, 0, 7.5))
  barplot(df_weather_c_y$dif_t, beside = TRUE, axes = F, xaxt = 'n', 
          las = 1, col = colors_y_t,
          yaxt ='n', cex.names = 1.3, xaxs = "i", ylab = NA,
          ylim = c(-0.7, 1.5))
  text(x = 0.5, y = 1.3, labels = "c)", cex = cex_fig)
  abline(h = c(-0.5, 0, 0.5, 1, 1.5), lty = 3, col = "grey30")
  axis(1, at = seq(0.75, 17.5, length.out = 15), labels = rep("", 15), 
       cex.axis = cex_fig)
  axis(1, at = seq(0.75, 17.5, length.out = 15), labels = df_weather_c_y$year, 
       cex.axis = cex_fig, line = 0.7, lwd = 0)
  axis(4, at =  c(-0.5, 0, 0.5, 1, 1.5), las = 1, cex.axis = cex_axis)
  
  legend(x = 6, y = 1.7, legend = c("annual precep", "30 year mean"), lty = c(1, 2),
         col = c("blue", "steelblue3"), bg = F, bty = "n", cex = cex_axis, lwd = c(3, 3))
  legend(x = 9.5, y = 1.7, legend = c("annual mean temp", "30 year mean"), lty = c(1, 2), 
         col = c("firebrick", "red"), bg = F, bty = "n", cex = cex_axis, lwd = c(3, 3))
  
  mtext(text = expression(Delta*'T'[a]*' [°C]'), side = 4, line = 6.5, cex = cex_legend)
  abline(h = 0)
  
  ## box 
  par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(3.3, 5.3, 3.3, 4.95))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, add = T, ylim = c(-2, -1))
  
  dev.off()
