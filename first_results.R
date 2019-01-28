mypath <- getwd()
CheckData()
df_train.1 <- pred_analysis[[1]]
df_final_train <- df_train.1[,c(results_resp_all_b[[3]]$best_preds_full$predictors, "NEE_cor")]
df_final_pred <- df_night_pred[,c(results_resp_all_b[[3]]$best_preds_full$predictors, "NEE_cor")]

model <- BuildModel(df_train = df_final_train, layer = results_resp_all_b[[3]]$best$layer,
                    units = c(results_resp_all_b[[3]]$best$units, results_resp_all_b[[3]]$best$units / 2),
                    optimizer = "adam", lr = 1e-3)

# min and max for mormalization
mins_data <- apply(df_final_train, 2, min, na.rm = T)
maxs_data <- apply(df_final_train, 2, max, na.rm = T)

# predict = x
df_final_train <- scale(df_final_train, center = mins_data,
                        scale = maxs_data - mins_data)
df_final_pred <- scale(df_final_pred, center = mins_data,
                        scale = maxs_data - mins_data)

train_data <- as.matrix(df_final_train[, -ncol(df_final_train)])
train_targets <- as.array(df_final_train[, ncol(df_final_train)])
pred_data <- as.matrix(df_final_pred[, -ncol(df_final_pred)])

## train model
callback_list <- list(callback_early_stopping(patience = 6))

history <- model %>% fit(
  train_data, train_targets, 
  epochs = 100, batch_size = results_resp_all_b[[3]]$best$batch_size, 
  validation_split = 0.2, verbose = 1,
  callbacks = callback_list)

K <- backend()
K$clear_session()

plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 0.05))

test_predictions <- model %>% predict(pred_data)
final_result <- test_predictions[,1] * (maxs_data[10] - mins_data[10]) + mins_data[10]

#df_raw_2$flag_night <- FlagNightData(df_flux = df_raw_2, df_sun = df_hel_2)
df_night <- df_merged[df_merged$flag_night == 1, ]
df_night <- df_night[-which(df_night$PPFDin > 5), ]

df_night$NEE_cor <- df_night$NEE_measure
df_night$NEE_cor[df_night$ustar < 0.19] <- NA
df_night_pred_2 <-  df_night[is.na(df_night$NEE_cor), ]

plot(df_night_pred_2$NEE[1:80000] ~ df_night_pred_2$dt[1:80000], type = "l", ylim = c(-4,13))
lines(final_result[1:80000] ~ df_night_pred_2$dt[1:80000], col = "red")

summary(df_night_pred_2$NEE)
summary(final_result)

hist(df_night_pred_2$NEE, xlim = c(-20, 20),  breaks = 500)
hist(final_result, xlim = c(-20, 20),  breaks = 10)
sum(df_night_pred_2$NEE[1:50000], na.rm = T)
sum(final_result[1:50000], na.rm = T)

