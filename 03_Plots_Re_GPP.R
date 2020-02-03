#### ------------------------------------------- ##
#### Code for Re,GPP and model selection plots ##
#### ------------------------------------------- ##
  
  # This file produces all plots related to the NEE partitioning process.
  # 
  # 
  # author: Ferdinand Briegel
  # last_update: 12.07.2019 
  
#### ----------------------- ####
#### 0 - set path and load packages ####
#### ----------------------- ####
  ## path ####
  mypath <- getwd()
  path <- substr(mypath, 1, nchar(mypath)-27)
  
  ### --- ### Please set here path for thesis folder ### ---- ###
  # path <- "O:/Master/SoSe_2019/Briegel-Ferdinand-2/Thesis_Ferdinand_Briegel/"

  ## packages ####
  library(tidyr)
  library(dplyr)
  library(readr)

  ## settings ####
  cex_fig = 2.2
  cex_axis = 2.1
  cex_lab = 2.2
  cex_legend <- 2.2
  month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#### ------------------------- ####
#### 1. Respiration and GPP - Predictor Relevance ####
#### ------------------------- ####
  ## load data ####
  load(paste0(path, "Data_and_Programming/master/RData/GPP/results_complete_gpp_m0s1_13.06.RData"))
  load(paste0(path, "Data_and_Programming/master/RData/RE/Final_model/results_complete_m0s1_08.04.RData"))
  
  ## creating target data.frames for plots ####
  ##  mse and r2 values extracted by hand - GPP
  names_gpp <- c(results_resp_all_b_gpp_m0s1[[3]]$best_preds_5$predictors, "Ts1", "full")
  names_gpp[c(4,5)] <- c("Ms_main", "Ta")
  names_gpp <- paste0(c("","+ ","+ ","+ ","+ ", ""), names_gpp)
  
  mse_gpp <- c(0.5663879 , 0.3988966 , 0.2949491 , 0.2830734 , 0.2897094, 0.2030991)
  r2_gpp <- c(0.43551837, 0.60313034, 0.70744857, 0.71900228, 0.71258853, 0.79822407)
  
  df_pred_gpp <- data.frame(names_gpp, mse_gpp, r2_gpp)
  
  ## mse and r2 values extracted by hand ## Re
  names_re <- c(results_resp_all_b_m0s1[[3]]$best_preds_5$predictors, "full")
  names_re[c(1,3)] <- c("Ts_1", "Ms_mean")
  names_re <- paste0(c("","+ ","+ ","+ ","+ ", ""), names_re)
  
  mse_re <- c(5.119496, 4.880327, 4.902300, 4.945011, 4.858975, 4.740289)
  r2_re <- c(0.38354402, 0.41252898, 0.41067951, 0.40628517, 0.41557686, 0.4301256)
  
  df_pred_re <- data.frame(names_re, mse_re, r2_re)

  ## Plots ####
  labels_1 <- c(expression("T"[s]*"_1"), expression("LW"[""%down%""]), expression(theta["_mean"]), 
              "year_ws_sin", "year_sa_sin", "full")
  labels_2 <- c(expression("PPFD"[""%down%""]), expression(theta["_main"]), expression("LW"[""%down%""]),
                expression("LW"[""%up%""]), expression("T"[s]*"_1"), "full")
  
  ## plot
  # pdf(pate0(path, "/Latex/Plots/pred_selection.pdf"),
  #     width = 16, height = 8, bg = "white")
  tiff(paste0(path, "Latex/Plots/pred_selection.tiff"),
       width = 16, height = 8, bg = "white", units = 'in', res = 400)
  
  par(mfrow = c(2, 1))
  par(mar = c(4, 7, 2 , 2))
  
  # plot
  barplot(df_pred_re$r2_re, axes = F, xaxt = 'n', beside = TRUE, 
          las = 1, yaxt = 'n', xaxs = "i", ylim = c(0, 1), 
          space = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.7))
  
  abline(h = seq(0, 1, .2), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(h = df_pred_re$r2_re[6], lty = 2, col = "black", lwd = 2)
  
  axis(1, at = seq(0.7, 5.55, length.out = 5), labels = rep("", 5), cex.axis = cex_axis)
  axis(1, at = seq(0.7, 5.55, length.out = 5), labels = labels_1[-6], cex.axis = 2.4, line = 0.7, lwd = 0)
  axis(1, at = 7.25, labels = "", cex.axis = cex_axis)
  axis(1, at = 7.25, labels = labels_1[6], line = 0.7, lwd = 0, cex.axis = 2.4)
  axis(2, at = seq(0, 1, .2), labels = seq(0, 1, .2), las = 2, cex.axis = cex_axis)
  axis(2, at = seq(0.1, 0.9, .2), labels = rep("", 5), las = 2, tck = -0.01)
  
  mtext(text = expression('R'^2), side = 2, line = 4.5, cex = 2.7)
  text(x = 0.6, y = 0.9, labels = expression("(a) R"[e]), cex = cex_fig)
  
  barplot(df_pred_re$r2_re, axes = F, xaxt = 'n', beside = TRUE, 
          las = 1, yaxt = 'n', xaxs = "i", ylim = c(0, 1), 
          space = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.7), add = T)
  
  par(new = T, mar = c(4, 7, 2 , 2))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  # plot  
  par(mar = c(4, 7, 2 , 2))
  barplot(df_pred_gpp$r2_gpp, axes = F, xaxt = 'n', beside = TRUE, las = 1, 
          yaxt = 'n', xaxs = "i", ylim = c(0, 1), 
          space = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.7))
  
  abline(h = seq(0, 1, .2), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(h = df_pred_gpp$r2_gpp[6], lty = 2, col = "black", lwd = 2)
  
  axis(1, at = seq(0.7, 5.55, length.out = 5), labels = rep("", 5), cex.axis = cex_axis)
  axis(1, at = seq(0.7, 5.55, length.out = 5), labels = labels_2[-6], cex.axis = 2.4, line = 0.7, lwd = 0)
  axis(1, at = 7.25, labels = "", cex.axis = cex_axis)
  axis(1, at = 7.25, labels = labels_2[6], line = 0.7, lwd = 0, cex.axis = 2.4)
  axis(2, at = seq(0, 1, .2), labels = seq(0, 1, .2), las = 2, cex.axis = cex_axis)
  axis(2, at = seq(0.1, 0.9, .2), labels = rep("", 5), las = 2, tck = -0.01)
  
  mtext(text = expression('R'^2), side = 2, line = 4.5, cex = 2.7)
  text(x = 0.7, y = 0.9, labels = "(b) GPP", cex = cex_fig)
  
  barplot(df_pred_gpp$r2_gpp, axes = F, xaxt = 'n', beside = TRUE, las = 1, 
          yaxt = 'n', xaxs = "i", ylim = c(0, 1), 
          space = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.7), add = T)
  
  par(new = T, mar = c(4, 7, 2 , 2))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  dev.off()
  
#### ------------------------- ####
#### 2. Respiration and GPP - Yearly and Monthly totals ####
#### ------------------------- ####
#### results as one data.frame ####
  ## load data ####
  load(paste0(path, "Data_and_Programming/master/RData/GPP/results_complete_gpp_m0s1_13.06.RData"))
  load(paste0(path, "Data_and_Programming/master/RData/GPP/results_boots_gpp_m0s1_13.06.RData"))
  
  ## Lee et al. 
  df_daytime <- read_delim(paste0(path, "Data_and_Programming/master/Daten/daytime_method_Annual.csv"), 
                           ";", escape_double = FALSE, trim_ws = TRUE)
  
  df_nighttime <- read_delim(paste0(path, "Data_and_Programming/master/Daten/Nighttime_method_Annual.csv"), 
                             ";", escape_double = FALSE, trim_ws = TRUE)
  
  df_monthly <- read_delim(paste0(path, "Data_and_Programming/master/Daten/Monthly_C.csv")
                            , ";", escape_double = FALSE, 
                           locale = locale(decimal_mark = ",", grouping_mark = "."), 
                           trim_ws = TRUE)
  
  ## create data.frame ####
  df_results <- df_results_boot_gpp_m0s1[[2]]
  df_results$NEE_filled_m0s1 <- df_results$NEE_cor
  
  df_results$NEE_filled_m0s1[which(is.na(df_results$NEE_filled_m0s1))] <- 
    -df_results$GPP_final[which(is.na(df_results$NEE_filled_m0s1))] + 
    df_results$Re_final[which(is.na(df_results$NEE_filled_m0s1))]
  
  df_results$NEE_model_m0s1 <- -df_results$GPP_final + df_results$Re_final
  df_results$NEE_lee <- df_results$NEE
  
  ## NEE CI
  df_results$NEE_ci <- NA
  df_results$GPP_gap_filled_95.conf[is.na(df_results$GPP_gap_filled_95.conf)] <- 0
  df_results$Re_gap_filled_95.conf[is.na(df_results$Re_gap_filled_95.conf)] <- 0
  
  df_results$NEE_ci[which(is.na(df_results$NEE_cor))] <- 
    df_results$GPP_gap_filled_95.conf[which(is.na(df_results$NEE_cor))] + 
    df_results$Re_gap_filled_95.conf[which(is.na(df_results$NEE_cor))]
  
  df_results$GPP_gap_filled_95.conf[which(df_results$GPP_gap_filled_95.conf == 0)] <- NA
  df_results$Re_gap_filled_95.conf[which(df_results$Re_gap_filled_95.conf == 0)] <- NA
  
  summary(df_results)
  summary(df_results$NEE_cor)
  summary(df_results$NEE_model_m0s1)
  summary(df_results$NEE_filled_m0s1)
  summary(df_results$NEE_lee)

  ## function for totals and errors ####
  fun.sum <- function(data){
    sum_ <- sum(data * 1800, na.rm = T) / 10^6 * 12.01
    return(sum_)
  }
  
  fun.error <- function(data, ci = NULL){
    if (is.null(ci)){
      error <- sqrt(sum((abs(data) * 1800)^2, na.rm = T)) * 0.2 / 10^6 * 12.01
    } else {
      error_random <- sqrt(sum((abs(data[which(is.na(ci))]) * 1800)^2, na.rm = T)) * 0.2 / 10^6 * 12.01
      error_ci <- sum(ci * 1800, na.rm = T) / 10^6 * 12.01
      
      error <- error_random + error_ci
    }
    return(error)
  }
  
  ## annual values and sums ####
  df_results_y <- df_results %>%
    group_by(year) %>%
    summarize(# m0s1
              NEE_model_m0s1_sum = fun.sum(NEE_model_m0s1),
              GPP_m0s1_sum = fun.sum(GPP_final), 
              Re_m0s1_sum = fun.sum(Re_final),
              GPP_m0s1_ci = fun.error(GPP_final, GPP_gap_filled_95.conf), 
              Re_m0s1_ci = fun.error(Re_final, Re_gap_filled_95.conf),
              # gap filled
              NEE_gapfilled_sum = fun.sum(NEE_filled_m0s1),
              NEE_gapfilled_ci = fun.error(NEE_filled_m0s1, NEE_ci),
              # lee
              NEE_lee_sum = fun.sum(NEE_lee))
  
  df_results_y <- df_results_y[-16, ]
  
  ## montly values and sums ####
  df_results_m <- df_results %>%
    group_by(year, month) %>%
    summarize(NEE_model_m0s1_sum = fun.sum(NEE_model_m0s1),
              GPP_m0s1_sum = fun.sum(GPP_final), 
              Re_m0s1_sum = fun.sum(Re_final),
              NEE_model_m0s1_ci = fun.error(NEE_model_m0s1),
              GPP_m0s1_ci = fun.error(GPP_final, GPP_gap_filled_95.conf), 
              Re_m0s1_ci = fun.error(Re_final, Re_gap_filled_95.conf),
              # gap filled
              NEE_gapfilled_sum = fun.sum(NEE_filled_m0s1),
              NEE_gapfilled_ci = fun.error(NEE_filled_m0s1, NEE_ci),
              # lee
              NEE_lee_sum = fun.sum(NEE_lee))
  
  df_results_m <- df_results_m[-181, ]
  
  ## Results from Lee
  df_monthly <- df_monthly %>%
    separate(X1, c("month", "year"), "-")
  
  df_monthly$month <- match(df_monthly$month, month.abb)
  df_monthly$year <- as.numeric(paste0(20, df_monthly$year))
  
  ## merge all results ##
  df_month <- merge(df_results_m, df_monthly, by = c("month", "year"))
  
  df_monthly_m <- df_month %>%
    group_by(month) %>%
    summarize(NEE_model_m0s1_sum = mean(NEE_model_m0s1_sum, na.rm = T),
              GPP_m0s1_sum = mean(GPP_m0s1_sum, na.rm = T),
              Re_m0s1_sum = mean(Re_m0s1_sum, na.rm = T),
              NEE_model_m0s1_ci = mean(NEE_model_m0s1_ci, na.rm = T),
              GPP_m0s1_ci = mean(GPP_m0s1_ci, na.rm = T),
              Re_m0s1_ci = mean(Re_m0s1_ci, na.rm = T),
              # gap filled
              NEE_gapfilled_sum = mean(NEE_gapfilled_sum, na.rm = T),
              NEE_gapfilled_ci = mean(NEE_gapfilled_ci, na.rm = T),
              # lee
              Re_lee = mean(Re, na.rm = T),
              GPP_lee = mean(GPP, na.rm = T),
              NEP_lee = mean(NEP, na.rm = T))

#### Plots ####
  ## First Overview Plots year ####
  ## NEE
  plot(df_results_y$NEE_model_m0s1_sum * -1 ~ df_results_y$year, type = "l", xlab = "year", 
       ylab = "g C m-2 y-1", ylim = c(-300, 700))
  lines(df_results_y$NEE_gapfilled_sum * -1 ~ df_results_y$year, type = "l", xlab = "year",
        ylab = "g C m-2 y-1", col = "blue")
  lines(df_results_y$NEE_lee_sum * -1 ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  lines(df_daytime$A_NEP ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey")
  lines(df_nighttime$NEP ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "green")
  title("NEE")
  legend("topleft", legend = c("m0s1", "lee_1", "lee_day", "lee_night"), col = c("black", "red", "grey", "green"), 
         lty = c(1, 1, 1, 1), bty = "n", lwd = 1.5, cex = 1.5)

  ## GPP
  plot(df_results_y$GPP_m0s1_sum ~ df_results_y$year, type = "l", xlab = "year", 
       ylab = "g C m-2 y-1", ylim = c(1000, 2100))
  lines(df_results_y$GPP_m0s1_sum + df_results_y$GPP_m0s1_ci ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey", lty = 2)
  lines(df_results_y$GPP_m0s1_sum - df_results_y$GPP_m0s1_ci ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey", lty = 2)
  lines(df_daytime$A_GEP ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  lines(df_nighttime$GPP ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  title("GPP")
  legend("topleft", legend = c("m0s1", "lee_day", "lee_night"), col = c("black", "grey", "red"), 
         lty = c(1, 1, 1), bty = "n", lwd = 1.5, cex = 1.5)
  
  ## Re
  plot(df_results_y$Re_m0s1_sum ~ df_results_y$year, type = "l", xlab = "year", 
       ylab = "g C m-2 y-1", ylim = c(1000, 1800))
  lines(df_results_y$Re_m0s1_sum + df_results_y$Re_m0s1_ci ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey", lty = 2)
  lines(df_results_y$Re_m0s1_sum - df_results_y$Re_m0s1_ci ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey", lty = 2)
  lines(df_daytime$A_Re ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  lines(df_nighttime$Re ~ df_results_y$year, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  title("Re")
  legend("topleft", legend = c("m0s1", "lee_day", "lee_night"), col = c("black", "grey", "red"), 
         lty = c(1, 1, 1), bty = "n", lwd = 1.5, cex = 1.5)
  
  apply(df_results_y[, c("NEE_model_m0s1_sum", "GPP_m0s1_sum", "GPP_m0s1_ci", "Re_m0s1_sum", "Re_m0s1_ci")],
        2, mean)
  
  ## First Overview Plots month ####
  plot(df_monthly_m$NEE_model_m0s1_sum * -1 ~ df_monthly_m$month, type = "l", xlab = "year", 
       ylab = "g C m-2 y-1", ylim = c(-100, 200))
  lines(df_monthly_m$NEE_gapfilled_sum * -1 ~ df_monthly_m$month, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "blue")
  lines(df_monthly_m$NEP_lee ~ df_monthly_m$month, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "red")
  lines(df_monthly_m$A_NEP ~ df_monthly_m$month, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "grey")
  lines(df_monthly_m$NEP ~ df_monthly_m$month, type = "l", xlab = "year", 
        ylab = "g C m-2 y-1", col = "green")
  title("NEE")
  
  ## Plot PDF year ####
  pdf(paste0(path, "/Latex/Plots/NEE_year.pdf"),
      width = 16, height = 12, bg = "white")
  # tiff(paste0(path, "Latex/Plots/NEE_year.tiff"),
  #      width = 16, height = 12, bg = "white", units = 'in', res = 400)
  
  par(fig = c(0, 1, 0.6, 1), mar = c(5, 8, 3, 8), new = TRUE)
  # plot(NULL, xlim = df_results_y$year, ylim = c(-300, 500), type = "n", xlab = "", ylab = "", axes = FALSE)
  
  plot(df_results_y$NEE_gapfilled_sum * -1 ~ df_results_y$year, type = "l", xlab = "", 
       ylab = "", ylim = c(-800, 810), xaxt = "n", axes = F, lwd = 3)
  
  axis(1, at = df_results_y$year, labels = rep("", 15), 
       cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(-800, 810, 400), labels = seq(-800, 810, 400), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_y$year, labels = df_results_y$year, 
       cex.axis = cex_fig, tck = -0.03)
  axis(4, at = seq(-800, 810, 400), labels = rep("", 5), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  
  mtext(text = expression(bold('NEP')*' (g C m' ^-2*' year' ^-1*')'), 
        side = 2, line = 5.5, cex = cex_lab)
  
  abline(h = seq(-800, 810, 400), lty = 3, col = "darkgrey", lwd = 2.5)
  # abline(v = 2006, lty = 2, col = "black")
  abline(h = 0, lty = 2, lwd = 2, col = "black")  
  text(x = 2001.8, y = 740, labels = "(a)", cex = cex_fig)
  legend(x = 2002, y = 920, legend = c("ANN", "Lee et al. 2020"), lty = c(1, 1), pch = c(1, 2),
         col = c("black", "red"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  
  lines(df_results_y$NEE_gapfilled_sum * -1 ~ df_results_y$year,
        lwd = 3, col = "black")
  lines(df_results_y$NEE_gapfilled_sum * -1 + df_results_y$NEE_gapfilled_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_results_y$NEE_gapfilled_sum * -1 - df_results_y$NEE_gapfilled_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_daytime$A_NEP ~ df_results_y$year, 
        lwd = 3, col = "red")
  
  points(df_results_y$NEE_gapfilled_sum * -1 ~ df_results_y$year, 
         cex = cex_fig, lwd = 3)
  points(df_daytime$A_NEP ~ df_results_y$year, 
         cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  par(new = T, mar = c(5, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  ## GPP
  par(fig = c(0, 1, 0.3, 0.7), mar = c(5, 8, 3, 8), new = TRUE)
  plot(df_results_y$GPP_m0s1_sum ~ df_results_y$year, type = "l", xlab = "", 
       ylab = "", ylim = c(1000, 2000), col = "black", xaxt = "n", axes = F, lwd = 3)
  
  axis(1, at = df_results_y$year, labels = rep("", 15), 
       cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(1000, 2000, 200), labels = rep("", 6), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_y$year, labels = rep("", 15), 
       cex.axis = cex_fig, tck = -0.03)
  axis(4, at = seq(1000, 2000, 200), labels = seq(1000, 2000, 200), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  
  mtext(text = expression(bold('GPP')*' (g C m' ^-2*' year' ^-1*')'), 
        side = 4, line = 7, cex = cex_lab)
  
  abline(h = seq(1000, 2000, 200), lty = 3, lwd = 2.5, col = "darkgrey")
  # abline(v = 2006, lty = 2, col = "black")
  text(x = 2001.8, y = 1900, labels = "(b)", cex = cex_fig)
  # legend(x = 2002, y = 2050, legend = c("GPP", expression("R"[e])), lty = c(1, 1),
  #        col = c("green", "darkgrey"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  
  lines(df_results_y$GPP_m0s1_sum ~ df_results_y$year, type = "l", xlab = "year", 
        col = "black", lwd = 3)
  lines(df_results_y$GPP_m0s1_sum + df_results_y$GPP_m0s1_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_results_y$GPP_m0s1_sum - df_results_y$GPP_m0s1_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_daytime$A_GEP ~ df_results_y$year, 
        lwd = 3, col = "red")
  points(df_results_y$GPP_m0s1_sum ~ df_results_y$year, 
         cex = cex_fig, lwd = 3)
  points(df_daytime$A_GEP ~ df_results_y$year, 
         cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  par(new = T, mar = c(5, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  ## Re 
  par(fig = c(0, 1, 0, 0.4), mar = c(4, 8, 3, 8), new = TRUE)
  plot(df_results_y$Re_m0s1_sum ~ df_results_y$year, type = "l", xlab = "",
       ylab = "", ylim = c(1000, 2200), xaxt = "n", axes = F, lwd = 3)
  
  axis(1, at = df_results_y$year, labels = rep("", 15), 
       cex.axis = cex_fig, tck = -0.03)
  axis(1, at = df_results_y$year, labels = df_results_y$year, 
       cex.axis = cex_fig, line = 0.7, lwd = 0, tck = -0.03)
  axis(2, at = seq(1000, 2200, 200), labels = seq(1000, 2200, 200), las = 2, cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_y$year, labels = rep("", 15), cex.axis = 
         cex_fig, tck = -0.03)
  axis(4, at = seq(1000, 2200, 200), labels = rep("", 7), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  mtext(text = expression(bold('R'[e])*' (g C m' ^-2*' year' ^-1*')'), 
        side = 2, line = 5.5, cex = cex_lab)
  
  abline(h = seq(1000, 2200, 200), lty = 3, lwd = 2.5, col = "darkgrey")
  # abline(v = 2006, lty = 2, col = "black")
  text(x = 2001.8, y = 2100, labels = "(c)", cex = cex_fig)
  # legend(x = 2002, y = 2050, legend = c("GPP", expression("R"[e])), lty = c(2, 2),
  #        col = c("green", "darkgrey"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  
  lines(df_results_y$Re_m0s1_sum ~ df_results_y$year,
        col = "black", lwd = 3)
  lines(df_results_y$Re_m0s1_sum + df_results_y$Re_m0s1_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_results_y$Re_m0s1_sum - df_results_y$Re_m0s1_ci ~ df_results_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_daytime$A_Re ~ df_results_y$year, 
        lwd = 3, col = "red")
  points(df_results_y$Re_m0s1_sum ~ df_results_y$year, 
         cex = cex_fig, lwd = 3)
  points(df_daytime$A_Re ~ df_results_y$year, 
         cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  par(new = T, mar = c(4, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))  
  dev.off()
  
  ## Plot PDF month ####
  pdf(paste0(path, "Latex/Plots/NEE_month.pdf"),
      width = 16, height = 12, bg = "white")
  # tiff(paste0(path, "Latex/Plots/NEE_month.tiff"),
       # width = 16, height = 12, bg = "white", units = 'in', res = 400)
  
  ## NEE 
  par(fig = c(0, 1, 0.6, 1), mar = c(5, 8, 3, 8), new = TRUE)
  plot(df_monthly_m$NEE_gapfilled_sum * -1 ~ df_monthly_m$month, type = "l", xlab = "", 
       ylab = "", ylim = c(-80, 120), xaxt = "n", axes = F, lwd = 3)

  axis(1, at = df_monthly_m$month, labels = rep("", 12), 
       cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(-80, 120, 40), labels = seq(-80, 120, 40), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_monthly_m$month, labels = month, cex.axis = 
         cex_fig, tck = -0.03)
  axis(4, at = seq(-80, 120, 40), labels = rep("", 6), las = 2, cex.axis = 
         cex_axis, tck = -0.03)
  mtext(text = expression(bold('NEP')*' (g C m' ^-2*' month' ^-1*')'), 
        side = 2, line = 5.45, cex = cex_lab)
  
  abline(h = seq(-80, 120, 40), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(h = 0, lty = 2, lwd = 2)
  text(x = 1, y = 110, labels = "(a)", cex = cex_fig)
  legend(x = 8, y = 120, legend = c("ANN", "Lee et al. 2020"), lty = c(1, 1), pch = c(1, 2),
         col = c("black", "red"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  
  lines(df_monthly_m$NEE_gapfilled_sum * -1 ~ df_monthly_m$month, 
        lwd = 3, col = "black")
  lines(df_monthly_m$NEE_gapfilled_sum * -1 + df_monthly_m$NEE_gapfilled_ci ~ df_monthly_m$month, 
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$NEE_gapfilled_sum * -1 - df_monthly_m$NEE_gapfilled_ci ~ df_monthly_m$month, 
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$NEP_lee ~ df_monthly_m$month, 
        lwd = 3, col = "red")
  points(df_monthly_m$NEE_gapfilled_sum * -1 ~ df_monthly_m$month, 
         cex = cex_fig, lwd = 3)
  points(df_monthly_m$NEP_lee ~ df_monthly_m$month, 
         cex = cex_fig, pch = 2, lwd = 3, col = "red")
  
  par(new = T, mar = c(5, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))

  ## GPP
  par(fig = c(0, 1, 0.3, 0.7), mar = c(5, 8, 3, 8), new = TRUE)
  plot(df_monthly_m$GPP_m0s1_sum ~ df_monthly_m$month, type = "l", xlab = "", 
       ylab = "", ylim = c(0, 300), col = "black", xaxt = "n", axes = F, lwd = 3)

  axis(1, at = df_monthly_m$month, labels = rep("", 12), 
       cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(0, 300, 50), labels = rep("", 7), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_monthly_m$month, labels = rep("", 12), cex.axis = 
         cex_fig, tck = -0.03)
  axis(4, at = seq(0, 300, 50), labels = seq(0, 300, 50), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  mtext(text = expression(bold('GPP')*' (g C m' ^-2*' month' ^-1*')'), 
        side = 4, line = 6.55, cex = cex_lab)
  
  abline(h = seq(0, 300, 50), lty = 3, lwd = 2.5, col = "darkgrey")
  text(x = 1, y = 280, labels = "(b)", cex = cex_fig)
  
  lines(df_monthly_m$GPP_m0s1_sum ~ df_monthly_m$month, 
        col = "black", lwd = 3)
  lines(df_monthly_m$GPP_m0s1_sum + df_monthly_m$GPP_m0s1_ci ~ df_monthly_m$month, 
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$GPP_m0s1_sum - df_monthly_m$GPP_m0s1_ci ~ df_monthly_m$month, 
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$GPP_lee ~ df_monthly_m$month, 
        col = "red", lwd = 3)
  points(df_monthly_m$GPP_m0s1_sum ~ df_monthly_m$month, 
         cex = cex_fig, lwd = 3, col = "black")
  points(df_monthly_m$GPP_lee ~ df_monthly_m$month, 
         cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  par(new = T, mar = c(5, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))

  ## Re 
  par(fig = c(0, 1, 0, 0.4), mar = c(4, 8, 3, 8), new = TRUE)
  plot(df_monthly_m$Re_m0s1_sum ~ df_monthly_m$month, type = "l", xlab = "", lty = 1,
       ylab = "", ylim = c(0, 300), col = "black", xaxt = "n", axes = F, lwd = 3)

  axis(1, at = df_monthly_m$month, labels = rep("", 12), 
       cex.axis = cex_fig, tck = -0.03)
  axis(1, at = df_monthly_m$month, labels = month, cex.axis = 
         cex_fig, line = 0.7, lwd = 0, tck = -0.03)
  axis(2, at = seq(0, 300, 50), labels = seq(0, 300, 50), las = 2, 
       cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_monthly_m$month, labels = rep("", 12), cex.axis = 
         cex_fig, tck = -0.03)
  axis(4, at = seq(0, 300, 50), labels = rep("", 7), las = 2, cex.axis = 
         cex_axis, tck = -0.03)
  mtext(text = expression(bold('R'[e])*' (g C m' ^-2*' month' ^-1*')'), 
        side = 2, line = 5.45, cex = cex_lab)
  
  abline(h = seq(0, 300, 50), lty = 3, lwd = 2.5, col = "darkgrey")
  text(x = 1, y = 280, labels = "(c)", cex = cex_fig)
  
  lines(df_monthly_m$Re_m0s1_sum ~ df_monthly_m$month, 
        col = "black", lwd = 3)
  lines(df_monthly_m$Re_m0s1_sum + df_monthly_m$Re_m0s1_ci ~ df_monthly_m$month,
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$Re_m0s1_sum - df_monthly_m$Re_m0s1_ci ~ df_monthly_m$month, 
        col = "black", lty = 2, lwd = 3)
  lines(df_monthly_m$Re_lee ~ df_monthly_m$month, 
        col = "red", lwd = 3)
  points(df_monthly_m$Re_m0s1_sum ~ df_monthly_m$month, 
         cex = cex_fig, lwd = 3, col = "black")
  points(df_monthly_m$Re_lee ~ df_monthly_m$month, 
         cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  par(new = T, mar = c(4, 8, 3, 8))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  dev.off()
  
#### ------------------------- ##
#### 2. Model selection ##
#### ------------------------- ##  
  
  
#### ------------------------- ####
#### 3. Respiration and GPP - Seasonal predictor change ####
#### ------------------------- ####
  ## load data ####
    load(paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Temporal_variability/results_full_season_26.04.RData"))
    
  ## Respiration: list with predictor and ranking ####
    ## target data.frame with predictors and months for ranking
    pred_all <- NULL
    
    ## get the used predictors
    for (i in 1:length(results_resp_all_season)){
      pred_all <- c(pred_all, results_resp_all_season[[i]][[3]]$best_preds_full$predictors)
    }
    
    pred_all <- c(pred_all, "Soil.moisture1", "MS_mean")
    
    unique_pred <- unique(pred_all)
    df_ <- data.frame("predictor" = unique_pred)
    df_[, 2:13] <- NA
    
    ## loop to determine "rank" of every predictor
    for (i in 1:length(results_resp_all_season)){
      colnames(df_)[i + 1] <- paste0("m_", i)
      
      for (j in 1:length(unique_pred)){
        if (length(which(strsplit(as.character(results_resp_all_season[[i]][[2]][nrow(results_resp_all_season[[i]][[2]]), ]$predictors), 
                                  split = '+', fixed = TRUE)[[1]][2:12] == unique_pred[j])) == 0){
          df_[j, i + 1] <- NA
        } else {
          df_[j, i + 1] <- which(strsplit(as.character(results_resp_all_season[[i]][[2]][nrow(results_resp_all_season[[i]][[2]]), ]$predictors), 
                                          split = '+', fixed = TRUE)[[1]][2:12] == unique_pred[j])
        }
      }
    }
    
    df_$predictor <- as.character(df_$predictor)
    df_2 <- df_
    
    ## one soil temperature
    df_2[17, ] <- df_2[2, ]
    df_2[17, c(8, 9)] <- c(4,7)
    df_2$predictor[17] <- "Tsoil"
    
    ## one soil moisture
    df_2[18, ] <- df_2[13, ]
    df_2$predictor[18] <- "Msoil"
    df_2[18, c(2:4, 6, 9, 11:13)] <- c(8, 11, 7, 9, 1, 10, 11, 11)
    
    df_2 <- df_2[-c(2, 8, 11, 13, 14, 15, 16), ]
    
    ## final format
    df_2$relevance <- apply(df_2[, 2:13], 1, mean, na.rm = TRUE)
    df_2 <- df_2[order(df_2$relevance), ]
    
    names <- df_2[, 1]
    df_3 <- as.data.frame(t(df_2[,-1]))
    
    df_3$Month <- rownames(df_3)
    df_3 <- df_3[, c(12, 1:11)]
    
    rownames(df_3) <- NULL
    colnames(df_3) <- NULL
    colnames(df_3) <- c("Month", names)
    
    for (i in 2:12){
      df_3[1:12, i] <- as.integer(df_3[1:12, i])  
    }
    
  ## Respiration: list with predictors andexplained variance ####
  r_2 <- data.frame("r_1" = double(), "r_2" = double(), "r_3" = double(), "r_4" = double(), "r_5" = double())
  for (i in 1:12){
    w_lev_1 <- results_resp_all_season[[i]][[2]][results_resp_all_season[[i]][[2]]$level == 1, ]
    w_lev_2 <- results_resp_all_season[[i]][[2]][results_resp_all_season[[i]][[2]]$level == 2, ]
    w_lev_3 <- results_resp_all_season[[i]][[2]][results_resp_all_season[[i]][[2]]$level == 3, ]
    w_lev_4 <- results_resp_all_season[[i]][[2]][results_resp_all_season[[i]][[2]]$level == 4, ]
    w_lev_5 <- results_resp_all_season[[i]][[2]][results_resp_all_season[[i]][[2]]$level == 5, ]
    r_2[i, ] <-  cbind(max(w_lev_1$r2), max(w_lev_2$r2), max(w_lev_3$r2), max(w_lev_4$r2), max(w_lev_5$r2))
  }
    
  xtable::xtable(r_2)
  ## Respiration: Plot ####
    label_legend <- c(expression(italic("T")[s]), expression(italic("T")[a]), expression(italic("LW"[""%down%""])), 
                      expression(italic(theta)[s]), expression(italic("year_sa_sin")))
    
    pdf(paste0(path, "Latex/Plots/Re_pred_month.pdf"),
        width = 16, height = 8, bg = "white")
    # tiff(paste0(path, "Latex/Plots/Re_pred_month.tiff"),
    #      width = 16, height = 8, bg = "white", units = 'in', res = 400)
    ## r2
    par(fig = c(0, 1, 0.7, 1))
    par(mar = c(0, 5, 3, 5.5))
    plot(r_2$r_1 ~ c(1:12), ylim = c(0, 0.4), xlim = c(0.5, 12.5), type = "l", axes = F, xaxt = 'n', 
         yaxt = 'n', xaxs = "i", xlab = "", ylab = "", lwd = 2.5)

    text(x = 0.75, y = 0.35, labels = "(a)", cex = cex_fig)
    abline(h = seq(0, 0.4, 0.1), lty = 3, lwd = 2.5, col = "darkgrey")
    abline(h = 0)
    axis(3, at = seq(1, 12, length.out = 12), labels = month, cex.axis = cex_fig)
    axis(4, at = seq(0, 0.4, 0.1), labels = rep("", 5), las = 2, cex.axis = cex_axis)
    axis(4, at = seq(0, 0.4, 0.2), labels = seq(0, 0.4, 0.2), las = 2, cex.axis = cex_axis)
    mtext(text = expression(bolditalic("R"^"2")), side = 4, line = 4.45, cex = cex_legend)
    
    lines(r_2$r_1 ~ c(1:12), lwd = 2.5)
    points(r_2$r_1 ~ c(1:12), cex = cex_fig, lwd = 2)
    lines(r_2$r_5 ~ c(1:12), lty = 2, lwd = 2.5)
    points(r_2$r_5 ~ c(1:12), cex = cex_fig, lwd = 2)
    
    legend(x = 0.5, y = 0.16, legend = c("single predictor", "five predictors"), lty = c(1, 2), pch = c(1, 1),
           col = c("black", "black"), bg = F, bty = "n", cex = 2.5, lwd = 3, ncol = 2)
    
    ## rank
    #par(mar = c(4, 5, 3, 3))
    par(fig = c(0, 1, 0, 0.7), new = TRUE)
    par(mar = c(2.5, 5, 0, 5.5))
    plot(df_3$Tsoil[1:12] ~ c(1:12), type = "l", ylim = rev(range(c(0,11))), col = "cyan", lwd = 2.5, xaxt = 'n', xlab = "", 
         ylab = expression(bold("Ranking")), cex.axis = cex_axis, cex.lab = cex_lab, yaxt = 'n')

    abline(h = seq(1, 11, 1), lty = 3, lwd = 2.5, col = "darkgrey")
    text(x = 0.8, y = 0.3, labels = "(b)", cex = cex_fig)
    axis(1, at = seq(1, 12, length.out = 12), labels = month, cex.axis = cex_axis)
    axis(2, at = seq(1, 11, length.out = 11), labels = 1:11, cex.axis = cex_axis, las = 2)
    
    lines(df_3$Tsoil[1:12] ~ c(1:12), col = "cyan", lwd = 2.5)
    points(df_3$Tsoil[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "cyan")
    lines(df_3$LWin[1:12] ~ c(1:12), col = "black", lwd = 2.5)
    points(df_3$LWin[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "black")
    lines(df_3$airT[1:12] ~ c(1:12), col = "blue", lwd = 2.5)
    points(df_3$airT[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "blue")
    lines(df_3$Msoil[1:12] ~ c(1:12), col = "brown", lwd = 2.5)
    points(df_3$Msoil[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "brown")
    # lines(df_3$year_sa_sin[1:12] ~ c(1:12), col = "firebrick1", lwd = 2)
    # points(df_3$year_sa_sin[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "firebrick1")
    
    legend(x = 5.5, y = 8, legend = label_legend[-5], lty = 1, pch = 1, ncol = 2,
           col = c("cyan", "blue", "black", "brown"), bg = F, bty = "n", cex = 2.5, lwd = c(3))
    
    ## box 
    par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(2.5, 5, 3, 5.5))
    plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
    
    dev.off()
    
  ## GPP: list with predictor and ranking ####
    pred_all <- NULL
    
    for (i in 1:length(results_gpp_all_season)){
      pred_all <- c(pred_all, results_gpp_all_season[[i]][[3]]$best_preds_full$predictors)
    }
    
    pred_all <- c(pred_all, "Soil.moisture1", "MS_mean")
    
    unique_pred <- unique(pred_all)
    df_ <- data.frame("predictor" = unique_pred)
    df_[, 2:13] <- NA
    
    for (i in 1:length(results_gpp_all_season)){
      colnames(df_)[i + 1] <- paste0("m_", i)
      
      for (j in 1:length(unique_pred)){
        if (length(which(strsplit(as.character(results_gpp_all_season[[i]][[2]][nrow(results_gpp_all_season[[i]][[2]]), ]$predictors), 
                                  split = '+', fixed = TRUE)[[1]][2:14] == unique_pred[j])) == 0){
          df_[j, i + 1] <- NA
        } else {
          df_[j, i + 1] <- which(strsplit(as.character(results_gpp_all_season[[i]][[2]][nrow(results_gpp_all_season[[i]][[2]]), ]$predictors), 
                                          split = '+', fixed = TRUE)[[1]][2:14] == unique_pred[j])
        }
      }
    }
    
    df_$predictor <- as.character(df_$predictor)
    df_2 <- df_
    
    ## one soil temperature
    df_2[23, ] <- df_2[3, ]
    df_2$predictor[23] <- "Tsoil"
    df_2[23, c(8, 9, 12, 13)] <- c(8, 7, 7, 4)
  
    ## one soil moisture
    df_2[24, ] <- df_2[16, ]
    df_2$predictor[24] <- "Msoil"
    df_2[24, c(2:4, 8, 10:13)] <- c(4, 5, 7, 4, 2, 4, 5, 5)
    
    df_2 <- df_2[-c(3, 4, 11, 15:22), ]
    
    df_2$relevance <- apply(df_2[, 2:13], 1, mean, na.rm = TRUE)
    df_2 <- df_2[order(df_2$relevance), ]
    
    names <- df_2[, 1]
    df_3 <- as.data.frame(t(df_2[,-1]))
    
    df_3$Month <- rownames(df_3)
    df_3 <- df_3[, c(14, 1:13)]
    
    rownames(df_3) <- NULL
    colnames(df_3) <- NULL
    colnames(df_3) <- c("Month", names)
    
    for (i in 2:14){
      df_3[1:12, i] <- as.integer(df_3[1:12, i])  
    }
  
  ## GPP: list with predictors andexplained variance ####
  r_2 <- data.frame("r_1" = double(), "r_2" = double(), "r_3" = double(), "r_4" = double(), "r_5" = double())
  for (i in 1:12){
    w_lev_1 <- results_gpp_all_season[[i]][[2]][results_gpp_all_season[[i]][[2]]$level == 1, ]
    w_lev_2 <- results_gpp_all_season[[i]][[2]][results_gpp_all_season[[i]][[2]]$level == 2, ]
    w_lev_3 <- results_gpp_all_season[[i]][[2]][results_gpp_all_season[[i]][[2]]$level == 3, ]
    w_lev_4 <- results_gpp_all_season[[i]][[2]][results_gpp_all_season[[i]][[2]]$level == 4, ]
    w_lev_5 <- results_gpp_all_season[[i]][[2]][results_gpp_all_season[[i]][[2]]$level == 5, ]
    r_2[i, ] <-  cbind(max(w_lev_1$r2), max(w_lev_2$r2), max(w_lev_3$r2), max(w_lev_4$r2), max(w_lev_5$r2))
  }
  
  ## GPP: Plot ####
    label_legend <- c(expression(italic("Q")), expression(italic("LW"[""%up%""])), 
                      expression(italic("LW"[""%down%""])), expression(italic(theta)[s]), 
                      expression(italic("T")[s]))
    
    # pdf(paste0(path, "Latex/Plots/GPP_pred_month.pdf"),
    #     width = 16, height = 8, bg = "white")
    tiff(paste0(path, "Latex/Plots/GPP_pred_month.tiff"),
         width = 16, height = 8, bg = "white", units = 'in', res = 400)
    
    ## r2
    par(fig = c(0, 1, 0.7, 1))
    par(mar = c(0, 5, 3, 5.5))
    plot(r_2$r_1 ~ c(1:12), ylim = c(0, 1), xlim = c(0.5, 12.5), type = "l", axes = F, xaxt = 'n', 
         yaxt = 'n', xaxs = "i", xlab = "", ylab = "", lwd = 2.5)

    text(x = 0.75, y = 0.9, labels = "(a)", cex = cex_fig)
    abline(h = seq(0, 1, 0.2), lty = 3, lwd = 2.5, col = "darkgrey")
    abline(h = 0)
    axis(3, at = seq(1, 12, length.out = 12), labels = month, cex.axis = cex_fig)
    axis(4, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2, cex.axis = cex_axis)
    mtext(text = expression(bolditalic("R"^"2")), side = 4, line = 4.5, cex = cex_legend)
    
    lines(r_2$r_1 ~ c(1:12), lwd = 2.5)
    points(r_2$r_1 ~ c(1:12), cex = cex_fig, lwd = 2)
    lines(r_2$r_5 ~ c(1:12), lty = 2, lwd = 2.5)
    points(r_2$r_5 ~ c(1:12), cex = cex_fig, lwd = 2)
        
    legend(x = 0.5, y = 0.45, legend = c("single predictor", "five predictors"), lty = c(1, 2), pch = c(1, 1),
           col = c("black", "black"), bg = F, bty = "n", cex = 2.5, lwd = 3, ncol = 2)
    
    ## ranking
    par(fig = c(0, 1, 0, 0.7), new = TRUE)
    par(mar = c(2.5, 5, 0, 5.5))
    plot(df_3$PPFDin[1:12] ~ c(1:12), type = "l", ylim = rev(range(c(0,13))), col = "green", lwd = 2.5, xaxt = 'n', xlab = "", 
         ylab = expression(bold("Ranking")), cex.axis = cex_axis, cex.lab = cex_lab, yaxt = 'n')

    text(x = 0.8, y = 0.3, labels = "(b)", cex = cex_fig)
    axis(1, at = seq(1, 12, length.out = 12), labels = month, cex.axis = cex_axis)
    axis(2, at = seq(1, 13, length.out = 13), labels = 1:13, cex.axis = cex_axis, las = 2)
    abline(h = seq(1, 13, 1), lty = 3, lwd = 2.5, col = "darkgrey")
    
    lines(df_3$PPFDin[1:12] ~ c(1:12), col = "green", lwd = 2.5)
    points(df_3$PPFDin[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "green")
    lines(df_3$LWout[1:12] ~ c(1:12), col = "grey", lwd = 2.5)
    points(df_3$LWout[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "grey")
    lines(df_3$LWin[1:12] ~ c(1:12), col = "black", lwd = 2.5)
    points(df_3$LWin[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "black")
    lines(df_3$Msoil[1:12] ~ c(1:12), col = "brown", lwd = 2.5)
    points(df_3$Msoil[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "brown")
    lines(df_3$Tsoil[1:12] ~ c(1:12), col = "cyan", lwd = 2.5)
    points(df_3$Tsoil[1:12] ~ c(1:12), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "cyan")
    
    legend(x = 1, y = -1, legend = label_legend, lty = 1, pch = 1, ncol = 5,
           col = c("green", "grey", "black", "brown", "cyan"), bg = F, bty = "n", cex = 2.4, lwd = c(3))
    
    ## box 
    par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(2.5, 5, 3, 5.5))
    plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
    
    dev.off()
    
#### ------------------------- ####
#### 4. Respiration and GPP - Annual predictor change ####
#### ------------------------- ####
  ## load data ####
  load(paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Temporal_variability/results_full_year_06.06.RData"))
  
  ## Respiration: list with predictor and ranking ####
  ## target data.frame with predictors and months for ranking
  pred_all <- NULL
  
  ## get the used predictors
  for (i in 1:length(results_resp_all_year)){
    pred_all <- c(pred_all, results_resp_all_year[[i]][[3]]$best_preds_full$predictors)
  }
  
  unique_pred <- unique(pred_all)
  df_ <- data.frame("predictor" = unique_pred)
  df_[, 2:15] <- NA
  
  ## loop to determine "rank" of every predictor
  for (i in 1:length(results_resp_all_year)){
    if (i < 8){
      colnames(df_)[i + 1] <- paste0(200, i + 1, "/", 200, i + 2)
    } else if (i == 8){
      colnames(df_)[i + 1] <- paste0(200, i + 1, "/", 20, i + 2)
      } else {
        colnames(df_)[i + 1] <- paste0(20, i + 1, "/", 20, i + 2)
      }
    
    for (j in 1:length(unique_pred)){
      if (length(which(strsplit(as.character(results_resp_all_year[[i]][[2]][nrow(results_resp_all_year[[i]][[2]]), ]$predictors), 
                                split = '+', fixed = TRUE)[[1]][2:12] == unique_pred[j])) == 0){
        df_[j, i + 1] <- NA
      } else {
        df_[j, i + 1] <- which(strsplit(as.character(results_resp_all_year[[i]][[2]][nrow(results_resp_all_year[[i]][[2]]), ]$predictors), 
                                        split = '+', fixed = TRUE)[[1]][2:12] == unique_pred[j])
      }
    }
  }
  
  df_$predictor <- as.character(df_$predictor)
  df_2 <- df_
  
  # ## one soil temperature
  # df_2[16, ] <- df_2[1, ]
  # df_2[16, 4] <- 1
  # df_2$predictor[16] <- "Tsoil"
  # 
  # ## one soil moisture
  # df_2[17, ] <- df_2[15, ]
  # df_2$predictor[17] <- "Msoil"
  # df_2[17, c(2:5, 9, 11, 12)] <- c(2, 2, 2, 5, 2, 3, 3)
  # 
  # df_2 <- df_2[-c(1, 2, 11, 13, 14, 15), ]
  
  ## final format
  df_2$relevance <- apply(df_2[, 2:13], 1, mean, na.rm = TRUE)
  df_2 <- df_2[order(df_2$relevance), ]
  
  names <- df_2[, 1]
  df_3 <- as.data.frame(t(df_2[, -1]))
  
  df_3$Year <- rownames(df_3)
  df_3 <- df_3[, c(12, 1:11)]
  
  rownames(df_3) <- NULL
  colnames(df_3) <- NULL
  colnames(df_3) <- c("Years", names)
  
  for (i in 2:12){
    df_3[1:12, i] <- as.integer(df_3[1:12, i])  
  }
  
  ## Respiration: list with predictors and explained variance ####
  r_2 <- data.frame("r_1" = double(), "r_2" = double(), "r_3" = double(), "r_4" = double(), "r_5" = double())
  for (i in 1:14){
    w_lev_1 <- results_resp_all_year[[i]][[2]][results_resp_all_year[[i]][[2]]$level == 1, ]
    w_lev_2 <- results_resp_all_year[[i]][[2]][results_resp_all_year[[i]][[2]]$level == 2, ]
    w_lev_3 <- results_resp_all_year[[i]][[2]][results_resp_all_year[[i]][[2]]$level == 3, ]
    w_lev_4 <- results_resp_all_year[[i]][[2]][results_resp_all_year[[i]][[2]]$level == 4, ]
    w_lev_5 <- results_resp_all_year[[i]][[2]][results_resp_all_year[[i]][[2]]$level == 5, ]
    r_2[i, ] <-  cbind(max(w_lev_1$r2), max(w_lev_2$r2), max(w_lev_3$r2), max(w_lev_4$r2), max(w_lev_5$r2))
  }
  
  ## Respiration: Plot ####
  label_legend <- c(expression(italic("T")[s]), expression(theta[s]), expression(italic("LW"[""%down%""])), 
                    expression(italic("year_sa_sin")), expression(italic("year_ws_sin")))
  
  pdf(paste0(path, "Latex/Plots/Re_pred_year.pdf"),
      width = 16, height = 8, bg = "white")
  # tiff(paste0(path, "Latex/Plots/Re_pred_year.tiff"),
  #      width = 16, height = 8, bg = "white", units = 'in', res = 400)
  
  ## r2
  par(fig = c(0, 1, 0.7, 1))
  par(mar = c(0, 5, 3, 5.5))
  plot(r_2$r_1 ~ c(1:14), ylim = c(0, 0.6), xlim = c(0.5, 14.5), type = "l", axes = F, xaxt = 'n', 
       yaxt = 'n', xaxs = "i", xlab = "", ylab = "", lwd = 2.5)

  text(x = 0.8, y = 0.5, labels = "(a)", cex = cex_fig)
  abline(h = seq(0, 0.6, 0.2), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(h = 0)
  axis(3, at = seq(1, 14, length.out = 14), labels = df_3$Years[1:14], cex.axis = cex_fig)
  axis(4, at = seq(0, 0.6, 0.2), labels = seq(0, 0.6, 0.2), las = 2, cex.axis = cex_axis)
  mtext(text = expression(bolditalic("R"^"2")), side = 4, line = 4.5, cex = cex_legend)
  
  lines(r_2$r_1 ~ c(1:14), lty = 1, lwd = 2.5)
  points(r_2$r_1 ~ c(1:14), cex = cex_fig, lwd = 2)
  lines(r_2$r_5 ~ c(1:14), lty = 2, lwd = 2.5)
  points(r_2$r_5 ~ c(1:14), cex = cex_fig, lwd = 2)
  
  legend(x = 0.5, y = 0.3, legend = c("single predictor", "five predictors"), lty = c(1, 2), pch = c(1, 1),
         col = c("black", "black"), bg = F, bty = "n", cex = 2.5, lwd = 3, ncol = 2)
  
  ## ranking
  par(fig = c(0, 1, 0, 0.7), new = TRUE)
  par(mar = c(2.5, 5, 0, 5.5))
  plot(df_3$Ts1[1:14] ~ c(1:14), type = "l", ylim = rev(range(c(0,13))), col = "cyan", lwd = 2.5, xaxt = 'n', xlab = "", 
       ylab = expression(bold("Ranking")), cex.axis = cex_axis, cex.lab = cex_lab, yaxt = 'n')

  abline(h = seq(1, 13, 1), lty = 3, lwd = 2.5, col = "darkgrey")
  text(x = 0.8, y = 0.3, labels = "(b)", cex = cex_fig)
  axis(1, at = seq(1, 14, length.out = 14), labels = df_3$Years[1:14], cex.axis = cex_axis)
  axis(2, at = seq(1, 13, length.out = 13), labels = 1:13, cex.axis = cex_axis, las = 2)
  
  lines(df_3$Ts1[1:14] ~ c(1:14), col = "cyan", lwd = 2.5)
  points(df_3$Ts1[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "cyan")
  lines(df_3$MS_mean[1:14] ~ c(1:14), col = "brown", lwd = 2.5)
  points(df_3$MS_mean[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "brown")
  lines(df_3$LWin[1:14] ~ c(1:14), col = "black", lwd = 2.5)
  points(df_3$LWin[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "black")
  lines(df_3$year_sa_sin[1:14] ~ c(1:14), col = "firebrick1", lwd = 2.5)
  points(df_3$year_sa_sin[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "firebrick1")
  lines(df_3$year_ws_sin[1:14] ~ c(1:14), col = "gold", lwd = 2.5)
  points(df_3$year_ws_sin[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "gold")
  
  legend(x = 1, y = 9.8, legend = label_legend, lty = 1, pch = 1, ncol = 3,
         col = c("cyan", "brown", "black", "firebrick1", "gold"), bg = F, bty = "n", cex = 2.4, lwd = c(3))
  
  ## box 
  par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(2.5, 5, 3, 5.5))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  dev.off()
  
  ## GPP: list with predictor and ranking ####
  ## target data.frame with predictors and months for ranking
  pred_all <- NULL
  
  ## get the used predictors
  for (i in 1:length(results_gpp_all_year)){
    pred_all <- c(pred_all, results_gpp_all_year[[i]][[3]]$best_preds_full$predictors)
  }
  
  unique_pred <- unique(pred_all)
  df_ <- data.frame("predictor" = unique_pred)
  df_[, 2:15] <- NA
  
  ## loop to determine "rank" of every predictor
  for (i in 1:length(results_gpp_all_year)){
    if (i < 8){
      colnames(df_)[i + 1] <- paste0(200, i + 1, "/", 200, i + 2)
    } else if (i == 8){
      colnames(df_)[i + 1] <- paste0(200, i + 1, "/", 20, i + 2)
    } else {
      colnames(df_)[i + 1] <- paste0(20, i + 1, "/", 20, i + 2)
    }
    
    for (j in 1:length(unique_pred)){
      if (length(which(strsplit(as.character(results_gpp_all_year[[i]][[2]][nrow(results_gpp_all_year[[i]][[2]]), ]$predictors), 
                                split = '+', fixed = TRUE)[[1]][2:14] == unique_pred[j])) == 0){
        df_[j, i + 1] <- NA
      } else {
        df_[j, i + 1] <- which(strsplit(as.character(results_gpp_all_year[[i]][[2]][nrow(results_gpp_all_year[[i]][[2]]), ]$predictors), 
                                        split = '+', fixed = TRUE)[[1]][2:14] == unique_pred[j])
      }
    }
  }
  
  df_$predictor <- as.character(df_$predictor)
  df_2 <- df_
  
  # ## one soil temperature
  # df_2$predictor[2] <- "Tsoil"
  # 
  # ## one soil moisture
  # df_2[16, ] <- df_2[4, ]
  # df_2$predictor[16] <- "Msoil"
  # df_2[16, c(10:13)] <- c(4, 2, 2, 6)
  # 
  # df_2 <- df_2[-c(4, 14, 15), ]
  
  ## final format
  df_2$relevance <- apply(df_2[, 2:15], 1, mean, na.rm = TRUE)
  df_2 <- df_2[order(df_2$relevance), ]
  
  names <- df_2[, 1]
  df_3 <- as.data.frame(t(df_2[, -1]))
  
  df_3$Year <- rownames(df_3)
  df_3 <- df_3[, c(14, 1:13)]
  
  rownames(df_3) <- NULL
  colnames(df_3) <- NULL
  colnames(df_3) <- c("Years", names)
  
  for (i in 2:14){
    df_3[1:14, i] <- as.integer(df_3[1:14, i])  
  }
  
  ## GPP: list with predictors and explained variance ####
  r_2 <- data.frame("r_1" = double(), "r_2" = double(), "r_3" = double(), "r_4" = double(), "r_5" = double())
  for (i in 1:14){
    w_lev_1 <- results_gpp_all_year[[i]][[2]][results_gpp_all_year[[i]][[2]]$level == 1, ]
    w_lev_2 <- results_gpp_all_year[[i]][[2]][results_gpp_all_year[[i]][[2]]$level == 2, ]
    w_lev_3 <- results_gpp_all_year[[i]][[2]][results_gpp_all_year[[i]][[2]]$level == 3, ]
    w_lev_4 <- results_gpp_all_year[[i]][[2]][results_gpp_all_year[[i]][[2]]$level == 4, ]
    w_lev_5 <- results_gpp_all_year[[i]][[2]][results_gpp_all_year[[i]][[2]]$level == 5, ]
    r_2[i, ] <-  cbind(max(w_lev_1$r2), max(w_lev_2$r2), max(w_lev_3$r2), max(w_lev_4$r2), max(w_lev_5$r2))
  }
  
  ## GPP: Plot ####
  label_legend <- c(expression(italic("Q")), expression(italic("LW"[""%down%""])), 
                    expression(italic("LW"[""%up%""])), expression(italic(theta)[s]), 
                    expression(italic("T")[s]))
  
  # pdf(paste0(path, "Latex/Plots/GPP_pred_year.pdf"),
  #     width = 16, height = 8, bg = "white")
  tiff(paste0(path, "Latex/Plots/GPP_pred_year.tiff"),
       width = 16, height = 8, bg = "white", units = 'in', res = 400)
  
  ## r2
  par(fig = c(0, 1, 0.7, 1))
  par(mar = c(0, 5, 3, 5.5))
  plot(r_2$r_1 ~ c(1:14), ylim = c(0, 1), xlim = c(0.5, 14.5), type = "l", axes = F, xaxt = 'n', 
       yaxt = 'n', xaxs = "i", xlab = "", ylab = "", lwd = 2.5)
  
  text(x = 0.8, y = 0.9, labels = "(a)", cex = cex_fig)
  abline(h = seq(0, 1, 0.2), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(h = 0)
  axis(3, at = seq(1, 14, length.out = 14), labels = df_3$Years[1:14], cex.axis = cex_fig)
  axis(4, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2), las = 2, cex.axis = cex_axis)
  mtext(text = expression(bolditalic("R"^"2")), side = 4, line = 4.5, cex = cex_legend)
  
  lines(r_2$r_1 ~ c(1:14), lty = 1, lwd = 2.5)
  points(r_2$r_1 ~ c(1:14), cex = cex_fig, lwd = 2)
  lines(r_2$r_5 ~ c(1:14), lty = 2, lwd = 2.5)
  points(r_2$r_5 ~ c(1:14), cex = cex_fig, lwd = 2)
  
  legend(x = 0.5, y = 0.45, legend = c("single predictor", "five predictors"), lty = c(1, 2), pch = c(1, 1),
         col = c("black", "black"), bg = F, bty = "n", cex = 2.5, lwd = 3, ncol = 2)
  
  ## ranking
  par(fig = c(0, 1, 0, 0.7), new = TRUE)
  par(mar = c(2.5, 5, 0, 5.5))
  plot(df_3$PPFDin[1:14] ~ c(1:14), type = "l", ylim = rev(range(c(0,13))), col = "green", lwd = 2.5, xaxt = 'n', xlab = "", 
       ylab = expression(bold("Ranking")), cex.axis = cex_axis, cex.lab = cex_lab, yaxt = 'n')

  abline(h = seq(1, 13, 1), lty = 3, lwd = 2.5, col = "darkgrey")
  text(x = 0.75, y = 0.3, labels = "(b)", cex = cex_fig)
  axis(1, at = seq(1, 14, length.out = 14), labels = df_3$Years[1:14], cex.axis = cex_axis)
  axis(2, at = seq(1, 13, length.out = 13), labels = 1:13, cex.axis = cex_axis, las = 2)
  
  lines(df_3$PPFDin[1:14] ~ c(1:14), col = "green", lwd = 2.5)
  points(df_3$PPFDin[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "green")
  lines(df_3$LWin[1:14] ~ c(1:14), col = "black", lwd = 2.5)
  points(df_3$LWin[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "black")
  lines(df_3$LWout[1:14] ~ c(1:14), col = "grey", lwd = 2.5)
  points(df_3$LWout[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "grey")
  lines(df_3$Soil.moisture_main[1:14] ~ c(1:14), col = "brown", lwd = 2.5)
  points(df_3$Soil.moisture_main[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "brown")
  lines(df_3$Ts1[1:14] ~ c(1:14), col = "cyan", lwd = 2.5)
  points(df_3$Ts1[1:14] ~ c(1:14), xlab = NA, ylab = NA, cex = cex_fig, lwd = 3, col = "cyan")
  
  legend(x = 2.9, y = 11.5, legend = label_legend, lty = 1, pch = 1, ncol = 5, 
         col = c("green", "black", "grey", "brown", "cyan"), bg = F, bty = "n", cex = 2.4, lwd = c(3))
  
  ## box 
  par(new = T, mfrow = c(1, 1), mgp = c(3, 1, 0), mar = c(2.5, 5, 3, 5.5))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  dev.off()


#### ------------------------- ####
#### 5. Respiration and GPP - Fertilization ####
#### ------------------------- ####
  ## load data ####
  load(paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/GPP/results_boots_fert_gpp_m0s1_21.06.RData"))

  ## create data.frame ####
  df_results_fert <- df_results_boot_fert_gpp_m0s1[[2]]
  # df_results_fert$Re_final_m0s1 <- df_results_fert_boot_gpp_m0s1[[2]]$Re_final
  # df_results_fert$GPP_final_m0s1 <- df_results_fert_boot_gpp_m0s1[[2]]$GPP_final
  # 
  # df_results_fert$Re_final_m0s1_ci <- df_results_fert_boot_gpp_m0s1[[2]]$Re_gap_filled_95.conf
  # df_results_fert$GPP_final_m0s1_ci <- df_results_fert_boot_gpp_m0s1[[2]]$GPP_gap_filled_95.conf
  
  ## calculate unfertilizied NEP
  df_results_fert$NEE_unfert <- df_results_fert$NEE_cor
  
  df_results_fert$NEE_unfert[which(is.na(df_results_fert$NEE_unfert))] <- 
    - df_results_fert$GPP_final[which(is.na(df_results_fert$NEE_unfert))] + 
    df_results_fert$Re_final[which(is.na(df_results_fert$NEE_unfert))]
  
  # NEE confidence intervall
  df_results_fert$NEE_unfert_ci <- NA
  df_results_fert$GPP_gap_filled_95.conf[is.na(df_results_fert$GPP_gap_filled_95.conf)] <- 0
  df_results_fert$Re_gap_filled_95.conf[is.na(df_results_fert$Re_gap_filled_95.conf)] <- 0
  
  df_results_fert$NEE_unfert_ci[which(is.na(df_results_fert$NEE_cor))] <- 
    df_results_fert$GPP_gap_filled_95.conf[which(is.na(df_results_fert$NEE_cor))] + 
    df_results_fert$Re_gap_filled_95.conf[which(is.na(df_results_fert$NEE_cor))]
  
  df_results_fert$GPP_gap_filled_95.conf[which(df_results_fert$GPP_gap_filled_95.conf == 0)] <- NA
  df_results_fert$Re_gap_filled_95.conf[which(df_results_fert$Re_gap_filled_95.conf == 0)] <- NA
  
  ## calculate fertilizied NEP 
  df_results_fert$GPP_fert <- df_results$GPP_final
  df_results_fert$GPP_fert_ci <- df_results$GPP_gap_filled_95.conf
  df_results_fert$Re_fert <- df_results$Re_final
  df_results_fert$Re_fert_ci <- df_results$Re_gap_filled_95.conf
  df_results_fert$NEE_fert <- df_results$NEE_filled_m0s1
  
  ## NEE CI
  df_results_fert$NEE_fert_ci <- NA
  df_results_fert$GPP_fert_ci[is.na(df_results_fert$GPP_fert_ci)] <- 0
  df_results_fert$Re_fert_ci[is.na(df_results_fert$Re_fert_ci)] <- 0
  
  df_results_fert$NEE_fert_ci[which(is.na(df_results$NEE_cor))] <- 
    df_results_fert$GPP_fert_ci[which(is.na(df_results$NEE_cor))] + 
    df_results_fert$Re_fert_ci[which(is.na(df_results$NEE_cor))]
  
  df_results_fert$GPP_fert_ci[which(df_results_fert$GPP_fert_ci == 0)] <- NA
  df_results_fert$Re_fert_ci[which(df_results_fert$Re_fert_ci == 0)] <- NA
  
  summary(df_results_fert)
  summary(df_results_fert$NEE_unfert)
  summary(df_results_fert$NEE_fert)
  hist(df_results_fert$GPP_final)
  hist(df_results_fert$GPP_fert)
  
  # ## exchange first years
  # df_results_fert$NEE_unfert[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))] <- 
  #   df_results_fert$NEE_fert[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))]
  # 
  # df_results_fert$GPP_final[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))] <- 
  #   df_results_fert$GPP_fert[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))]
  # 
  # df_results_fert$GPP_gap_filled_95.conf[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))] <- 
  #   df_results_fert$GPP_fert_ci[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))]
  # 
  # df_results_fert$Re_final[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))] <- 
  #   df_results_fert$Re_fert[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))]
  # 
  # df_results_fert$Re_gap_filled_95.conf[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))] <- 
  #   df_results_fert$Re_fert_ci[which(as.numeric(format(df_results_fert$dt,"%Y")) %in% c(2001:2006))]
    
  ## annual values and sums ####
  df_results_fert_y <- df_results_fert %>%
    group_by(year) %>%
    summarize(# fert
      NEE_fert_sum = fun.sum(NEE_fert),
      GPP_fert_sum = fun.sum(GPP_fert), 
      Re_fert_sum = fun.sum(Re_fert),
      NEE_fert_ci = fun.error(NEE_fert, NEE_fert_ci),
      GPP_fert_ci = fun.error(GPP_fert, GPP_fert_ci), 
      Re_fert_ci = fun.error(Re_fert, Re_fert_ci),
      # unfert
      NEE_unfert_sum = fun.sum(NEE_unfert),
      GPP_unfert_sum = fun.sum(GPP_final), 
      Re_unfert_sum = fun.sum(Re_final),
      NEE_unfert_ci = fun.error(NEE_unfert, NEE_unfert_ci),
      GPP_unfert_ci = fun.error(GPP_final, GPP_gap_filled_95.conf), 
      Re_unfert_ci = fun.error(Re_final, Re_gap_filled_95.conf))
  
  df_results_fert_y <- df_results_fert_y[-16, ]
  
  ## Plot PDF year ####
  pdf(paste0(path, "Latex/Plots/NEE_fert.pdf"),
      width = 16, height = 12, bg = "white")
  
  ## NEP 
  par(fig = c(0, 1, 0.6, 1), mar = c(5, 7, 3, 7), new = TRUE)
  plot(df_results_fert_y$NEE_fert_sum * -1 ~ df_results_fert_y$year, type = "l", xlab = "", 
       ylab = "", ylim = c(-1500, 1000), xaxt = "n", axes = F, lwd = 3)
  lines(df_results_fert_y$NEE_fert_sum * -1 + df_results_fert_y$NEE_fert_ci ~ df_results_fert_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_results_fert_y$NEE_fert_sum * -1 - df_results_fert_y$NEE_fert_ci ~ df_results_fert_y$year, 
        col = "black", lty = 2, lwd = 3)
  
  lines(df_results_fert_y$NEE_unfert_sum * -1 ~ df_results_fert_y$year, lwd = 3, col = "red")
  lines(df_results_fert_y$NEE_unfert_sum * -1 + df_results_fert_y$NEE_unfert_ci ~ df_results_fert_y$year, 
        col = "red", lty = 2, lwd = 3)
  lines(df_results_fert_y$NEE_unfert_sum * -1 - df_results_fert_y$NEE_unfert_ci ~ df_results_fert_y$year,
        col = "red", lty = 2, lwd = 3)
  
  points(df_results_fert_y$NEE_fert_sum * -1 ~ df_results_fert_y$year, cex = cex_fig, lwd = 3)
  points(df_results_fert_y$NEE_unfert_sum * -1 ~ df_results_fert_y$year, cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  axis(1, at = df_results_fert_y$year, labels = rep("", 15), cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(-1500, 1000, 500), labels = seq(-1500, 1000, 500), las = 2, cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_fert_y$year, labels = df_results_fert_y$year, cex.axis = cex_fig, tck = -0.03)
  axis(4, at = seq(-1500, 1000, 500), labels = rep("", 6), las = 2, cex.axis = cex_axis, tck = -0.03)
  mtext(text = expression(bold('NEP')*' (g C m' ^-2*' year' ^-1*')'), side = 2, line = 4.5, cex = cex_lab)
  
  abline(h = seq(-1500, 1000, 500), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(v = 2006, lty = 2, col = "black")
  abline(h = 0, lty = 2, lwd = 2, col = "black")  
  text(x = 2001.8, y = 940, labels = "(a)", cex = cex_fig)
  legend(x = 2001.5, y = -740, legend = c("With fertilization", "Without fertilization"), lty = c(1, 1), pch = c(1, 2),
         col = c("black", "red"), bg = F, bty = "n", cex = cex_axis, lwd = c(3))
  
  par(new = T, mar = c(5, 7, 3, 7))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  ## GPP
  par(fig = c(0, 1, 0.3, 0.7), mar = c(5, 7, 3, 7), new = TRUE)
  plot(df_results_fert_y$GPP_fert_sum ~ df_results_fert_y$year, type = "l", xlab = "", 
       ylab = "", ylim = c(700, 2000), col = "black", xaxt = "n", axes = F, lwd = 3)
  lines(df_results_fert_y$GPP_fert_sum + df_results_fert_y$GPP_fert_ci ~ df_results_fert_y$year, 
        col = "black", lty = 2, lwd = 3)
  lines(df_results_fert_y$GPP_fert_sum - df_results_fert_y$GPP_fert_ci ~ df_results_fert_y$year, 
        col = "black", lty = 2, lwd = 3)
  
  lines(df_results_fert_y$GPP_unfert_sum ~ df_results_fert_y$year, lwd = 3, col = "red")
  lines(df_results_fert_y$GPP_unfert_sum + df_results_fert_y$GPP_unfert_ci ~ df_results_fert_y$year, 
        col = "red", lty = 2, lwd = 3)
  lines(df_results_fert_y$GPP_unfert_sum - df_results_fert_y$GPP_unfert_ci ~ df_results_fert_y$year, 
        col = "red", lty = 2, lwd = 3)
  
  points(df_results_fert_y$GPP_fert_sum ~ df_results_fert_y$year, cex = cex_fig, lwd = 3)
  points(df_results_fert_y$GPP_unfert_sum ~ df_results_fert_y$year, cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  axis(1, at = df_results_fert_y$year, labels = rep("", 15), cex.axis = cex_fig, tck = -0.03)
  axis(2, at = seq(700, 2000, 200), labels = rep("", 7), las = 2, cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_fert_y$year, labels = rep("", 15), cex.axis = cex_fig, tck = -0.03)
  axis(4, at = seq(700, 2000, 200), labels = seq(700, 2000, 200), las = 2, cex.axis = cex_axis, tck = -0.03)
  mtext(text = expression(bold('GPP')*' (g C m' ^-2*' year' ^-1*')'), side = 4, line = 6, cex = cex_lab)
  
  abline(h = seq(700, 2000, 200), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(v = 2006, lty = 2, col = "black")
  text(x = 2001.8, y = 1900, labels = "(b)", cex = cex_fig)
 
  par(new = T, mar = c(5, 7, 3, 7))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  ## Re 
  par(fig = c(0, 1, 0, 0.4), mar = c(4, 7, 3, 7), new = TRUE)
  plot(df_results_fert_y$Re_fert_sum ~ df_results_fert_y$year, type = "l", xlab = "",
       ylab = "", ylim = c(1000, 2400), xaxt = "n", axes = F, lwd = 3)
  lines(df_results_fert_y$Re_fert_sum + df_results_fert_y$Re_fert_ci ~ df_results_fert_y$year,
        col = "black", lty = 2, lwd = 3)
  lines(df_results_fert_y$Re_fert_sum - df_results_fert_y$Re_fert_ci ~ df_results_fert_y$year, 
        col = "black", lty = 2, lwd = 3)
  
  lines(df_results_fert_y$Re_unfert_sum ~ df_results_fert_y$year, lwd = 3, col = "red")
  lines(df_results_fert_y$Re_unfert_sum + df_results_fert_y$Re_unfert_ci ~ df_results_fert_y$year,  
        col = "red", lty = 2, lwd = 3)
  lines(df_results_fert_y$Re_unfert_sum - df_results_fert_y$Re_unfert_ci ~ df_results_fert_y$year, 
        col = "red", lty = 2, lwd = 3)
  
  points(df_results_fert_y$Re_fert_sum ~ df_results_fert_y$year, cex = cex_fig, lwd = 3)
  points(df_results_fert_y$Re_unfert_sum ~ df_results_fert_y$year, cex = cex_fig, lwd = 3, pch = 2, col = "red")
  
  axis(1, at = df_results_fert_y$year, labels = rep("", 15), cex.axis = cex_fig, tck = -0.03)
  axis(1, at = df_results_fert_y$year, labels = df_results_fert_y$year, cex.axis = cex_fig, line = 0.7, lwd = 0, tck = -0.03)
  axis(2, at = seq(1000, 2400, 200), labels = seq(1000, 2400, 200), las = 2, cex.axis = cex_axis, tck = -0.03)
  axis(3, at = df_results_fert_y$year, labels = rep("", 15), cex.axis = cex_fig, tck = -0.03)
  axis(4, at = seq(1000, 2400, 200), labels = rep("", 8), las = 2, cex.axis = cex_axis, tck = -0.03)
  mtext(text = expression(bold('R'[e])*' (g C m' ^-2*' year' ^-1*')'), side = 2, line = 4.5, cex = cex_lab)
  
  abline(h = seq(1000, 2400, 200), lty = 3, lwd = 2.5, col = "darkgrey")
  abline(v = 2006, lty = 2, col = "black")
  text(x = 2001.8, y = 2300, labels = "(c)", cex = cex_fig)
  
  par(new = T, mar = c(4, 7, 3, 7))
  plot(0, yaxt = "n", xaxt = "n", ylab = NA , xlab = NA, ylim = c(-2, -1))
  
  dev.off()
  
#### ------------------------- ####
#### 6. Respiration and GPP - Correlation Matrix ####
#### ------------------------- ####
  ## load data ####
  load(paste0(path, "Data_and_Programming/master/RData/GPP/results_boots_gpp_m0s1_13.06.RData"))
  df_results <- df_results_boot_gpp_m0s1[[2]]
  
  ## aggreagate data ####
  df_month <- df_results %>%
    group_by(year, month) %>%
    summarize(# m0s1
      NEE = fun.sum(NEE_final),
      GPP = fun.sum(GPP_final),
      Re = fun.sum(Re_final),
      Ta = mean(airT, na.rm = T),
      Ts = mean(Ts1, na.rm = T),
      ppfd = mean(PPFDin, na.rm = T),
      ms = mean(Soil.moisture_main, na.rm = T),
      prec = sum(Precip, na.rm = T))
  
  df_month_y <- df_results %>%
    group_by(month) %>%
    summarize(# m0s1
      NEE = fun.sum(NEE_final),
      GPP = fun.sum(GPP_final),
      Re = fun.sum(Re_final),
      Ta = mean(airT, na.rm = T),
      ppfd = mean(PPFDin, na.rm = T),
      prec = mean(Precip, na.rm = T))
  
  df_spring <- df_month[df_month$month %in% c(3,4,5), ] %>%
    group_by(year) %>%
    summarize(# m0s1
      NEE = fun.sum(NEE),
      GPP = fun.sum(GPP),
      Re = fun.sum(Re),
      Ta = mean(Ta, na.rm = T),
      Ts = mean(Ts, na.rm = T),
      ppfd = mean(ppfd, na.rm = T),
      prec = mean(prec))
  
  df_latwinter <- df_month[df_month$month %in% c(2,3,4), ] %>%
    group_by(year) %>%
    summarize(# m0s1
      NEE = fun.sum(NEE),
      GPP = fun.sum(GPP),
      Re = fun.sum(Re),
      Ta = mean(Ta, na.rm = T),
      Ts = mean(Ts, na.rm = T),
      ppfd = mean(ppfd, na.rm = T),
      prec = mean(prec))
  
  df_summer <- df_month[df_month$month %in% c(6,7,8,9), ] %>%
    group_by(year) %>%
    summarize(# m0s1
      NEE = fun.sum(NEE),
      GPP = fun.sum(GPP),
      Re = fun.sum(Re),
      Ta = mean(Ta, na.rm = T),
      Ts = mean(Ts, na.rm = T),
      ppfd = mean(ppfd, na.rm = T), 
      ms = mean(ms),
      prec = mean(prec))
  
  df_year <- df_results %>%
    group_by(year) %>%
    summarize(# m0s1
      Ts = mean(Ts1, na.rm = T),
      Ta = mean(airT, na.rm = T),
      soil.m = mean(Soil.moisture_main, na.rm = T),
      ppfd = mean(PPFDin, na.rm = T),
      prec = sum(Precip, na.rm = T),
      LWin = mean(LWin, na.rm = T),
      LWout = mean(LWout, na.rm = T), 
      Re = fun.sum(Re_final))
  
  df_year <- df_year[-16, ]
  
  ## Correlation Matrix ####
  cor(cbind("gpp_fert" = df_results_y$GPP_m0s1_sum, "Re_fert" = df_results_y$Re_m0s1_sum, "nee_fert" = df_results_y$NEE_gapfilled_sum, 
            "ta" = df_year$Ta, "sp_ta" = df_spring$Ta, "su_ta" = df_summer$Ta, "wi_ta" = df_latwinter$Ta,
            "ts" = df_year$Ts, "sp_ts" = df_spring$Ts, "su_ts" = df_summer$Ts, "wi_ts" = df_latwinter$Ts, 
            "ms" = df_year$soil.m, "su_ms" = df_summer$ms,
            "ppfd" = df_year$ppfd, "s_ppfd" = df_spring$ppfd, 
            "prec" = df_year$prec, "sp_prec" = df_spring$prec, "su_prec" = df_summer$prec,
            "Lwin" = df_year$LWin, "LWout" = df_year$LWout,
            "Re_dif" = c(df_results_y$Re_m0s1_sum - df_daytime$A_Re),
            "GPP_dif" = c(df_results_y$GPP_m0s1_sum - df_daytime$A_GEP),
            "NEP_dif" = c(-df_results_y$NEE_model_m0s1_sum - df_daytime$A_NEP),
            "RE_quo" = c(df_results_y$Re_m0s1_sum / df_daytime$A_Re)), method = "spearman")
  