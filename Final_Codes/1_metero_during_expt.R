################################################################################
## Build hourly final underwater PAR from:
## 1) measured water PAR hourly mean
## 2) measured air PAR hourly mean * 0.8
## 3) shortwave radiation -> fitted air PAR -> * 0.8
## 4) Use air PAR * 0.8 first; if missing, then use shortwave-fit * 0.8.
################################################################################
## 0) Packages + path
################################################################################
library(readxl)
library(openxlsx)
library(plyr)
library(ggplot2)
library(lubridate)

################################################################################
## GLOBAL STYLE + COLORS
col_par      <- "#1B1B1B"   # final PAR
col_par_sub  <- "#0072B2"   # scaled PAR
col_airtemp  <- "#D55E00"   # air temperature
col_watertmp <- "#009E73"   # water temperature

###
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

dir.create("../Output_Data", showWarnings = FALSE, recursive = TRUE)
dir.create("../Output_Figures", showWarnings = FALSE, recursive = TRUE)

t_start <- as.POSIXct("2025-09-19 00:00:00", tz = "UTC")
t_end   <- as.POSIXct("2025-09-25 23:59:59", tz = "UTC")

################################################################################
## 1) Read hourly air PAR
################################################################################
air_file <- "../Input_Data/insitu_air_PAR.xlsx"
air_sheets <- excel_sheets(air_file)

air_hourly_list <- list()

for (sh in air_sheets) {
  
  cat("Reading air PAR sheet:", sh, "\n")
  
  df <- openxlsx::read.xlsx(air_file, sheet = sh)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  
  if (!all(c("Date", "Time") %in% names(df))) next
  
  ## Date
  date_chr <- trimws(as.character(df$Date))
  date_num <- suppressWarnings(as.numeric(date_chr))
  date_final <- rep(NA_character_, length(date_chr))
  
  idx_num_date <- !is.na(date_num)
  if (any(idx_num_date)) {
    date_final[idx_num_date] <- as.character(
      as.Date(date_num[idx_num_date], origin = "1899-12-30")
    )
  }
  
  idx_dmy <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", date_chr)
  if (any(idx_dmy)) {
    tmp <- strsplit(date_chr[idx_dmy], "/")
    date_final[idx_dmy] <- sapply(tmp, function(z) {
      sprintf("%04d-%02d-%02d",
              as.numeric(z[3]), as.numeric(z[2]), as.numeric(z[1]))
    })
  }
  
  idx_ymd <- grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", date_chr)
  if (any(idx_ymd)) {
    date_final[idx_ymd] <- substr(date_chr[idx_ymd], 1, 10)
  }
  
  ## Time
  time_chr <- trimws(as.character(df$Time))
  time_num <- suppressWarnings(as.numeric(time_chr))
  time_final <- rep(NA_character_, length(time_chr))
  
  idx_num_time <- !is.na(time_num)
  if (any(idx_num_time)) {
    time_final[idx_num_time] <- format(
      as.POSIXct(time_num[idx_num_time] * 86400, origin = "1970-01-01", tz = "UTC"),
      "%H:%M:%S"
    )
  }
  
  idx_hms <- grepl("^\\d{1,2}:\\d{1,2}:\\d{1,2}$", time_chr)
  if (any(idx_hms)) {
    time_final[idx_hms] <- time_chr[idx_hms]
  }
  
  idx_hm <- grepl("^\\d{1,2}:\\d{1,2}$", time_chr)
  if (any(idx_hm)) {
    time_final[idx_hm] <- paste0(time_chr[idx_hm], ":00")
  }
  
  df$Datetime <- as.POSIXct(
    paste(date_final, time_final),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  clean_names <- tolower(gsub("\\s+", "", names(df)))
  if (!"input2" %in% clean_names) next
  
  val_col <- names(df)[which(clean_names == "input2")[1]]
  df$Air_PAR_value <- suppressWarnings(as.numeric(df[[val_col]]))
  
  df2 <- df[
    !is.na(df$Datetime) &
      !is.na(df$Air_PAR_value) &
      df$Datetime >= t_start &
      df$Datetime <= t_end,
  ]
  
  if (nrow(df2) == 0) next
  
  df2$Date <- format(df2$Datetime, "%Y-%m-%d")
  df2$hour <- format(df2$Datetime, "%H")
  
  hourly <- ddply(df2, .(Date, hour), summarise, 
                  Air_PAR_hour_mean = mean(Air_PAR_value, na.rm = TRUE)
  )
  
  hourly$Datetime <- as.POSIXct(paste(hourly$Date, hourly$hour),
    format = "%Y-%m-%d %H", tz = "UTC")
  
  air_hourly_list[[sh]] <- hourly
}

air_hourly_tmp <- do.call(rbind, air_hourly_list)
rownames(air_hourly_tmp) <- NULL

air_hourly <- ddply(air_hourly_tmp, .(Date, hour), summarise,
  Air_PAR_hour_mean = mean(Air_PAR_hour_mean, na.rm = TRUE))

air_hourly$Datetime <- as.POSIXct(paste(air_hourly$Date, air_hourly$hour),
  format = "%Y-%m-%d %H", tz = "UTC")

air_hourly <- air_hourly[, c("Datetime", "Date", "hour", "Air_PAR_hour_mean")]

################################################################################
## 2) Read hourly water PAR
################################################################################
water_file <- "../Input_Data/insitu_water_par.xlsx"

df <- read.xlsx(water_file, sheet = "Sheet1")
df <- as.data.frame(df, stringsAsFactors = FALSE)
names(df) <- trimws(names(df))

if (is.numeric(df$Datetime_UTC)) {
  df$Datetime <- as.POSIXct(
    df$Datetime_UTC * 86400,
    origin = "1899-12-30",
    tz = "UTC"
  )
} else {
  df$Datetime <- as.POSIXct(df$Datetime_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

df$Water_PAR_value <- suppressWarnings(as.numeric(as.character(df$Water_PAR)))

df2 <- df[
  !is.na(df$Datetime) &
    df$Datetime >= t_start &
    df$Datetime <= t_end,
]

df2$Date <- format(df2$Datetime, "%Y-%m-%d")
df2$hour <- format(df2$Datetime, "%H")

water_hourly <- ddply(df2, .(Date, hour), summarise,
                      Water_PAR_hour_mean = mean(Water_PAR_value, na.rm = TRUE))

water_hourly$Datetime <- as.POSIXct(paste(water_hourly$Date, water_hourly$hour),
  format = "%Y-%m-%d %H", tz = "UTC")

water_hourly <- water_hourly[, c("Datetime", "Date", "hour", "Water_PAR_hour_mean")]

################################################################################
## 3) Read met data
################################################################################
met <- read.xlsx("../Input_Data/metero_djk1.xlsx")
met <- as.data.frame(met, stringsAsFactors = FALSE)
names(met) <- trimws(names(met))

met$Datetime <- as.POSIXct(paste(met$year, met$month, met$day, met$hour),
  format = "%Y %m %d %H", tz = "UTC")

met <- met[met$Datetime >= t_start & met$Datetime <= t_end, ]

met$Date <- format(met$Datetime, "%Y-%m-%d")
met$hour <- format(met$Datetime, "%H")

met_use <- met[, c("Datetime", "Date", "hour", "short_radi_W_m2", "air_temp")]

################################################################################
## 4) Build shortwave -> air PAR relationships
################################################################################
fit_df <- merge(air_hourly, met_use, by = c("Datetime", "Date", "hour"), all = FALSE)

fit_df <- fit_df[
  !is.na(fit_df$Air_PAR_hour_mean) &
    !is.na(fit_df$short_radi_W_m2),
]

fit_df_g1 <- fit_df[fit_df$Date %in% c("2025-09-21", "2025-09-22", "2025-09-23"), ]
fit_df_g2 <- fit_df[fit_df$Date %in% c("2025-09-25"), ]

if (nrow(fit_df_g1) >= 2 && length(unique(fit_df_g1$short_radi_W_m2)) >= 2) {
  fit_g1 <- lm(Air_PAR_hour_mean ~ short_radi_W_m2, data = fit_df_g1)
} else {
  stop("Group 1 fit failed.")
}

if (nrow(fit_df_g2) >= 2 && length(unique(fit_df_g2$short_radi_W_m2)) >= 2) {
  fit_g2 <- lm(Air_PAR_hour_mean ~ short_radi_W_m2, data = fit_df_g2)
} else {
  stop("Group 2 fit failed.")
}

print(summary(fit_g1))
print(summary(fit_g2))

################################################################################
## 5) Build full hourly target grid
################################################################################
target_time <- seq(from = t_start, to = t_end, by = "hour")

target_df <- data.frame(Datetime = target_time, stringsAsFactors = FALSE)

target_df$Date <- format(target_df$Datetime, "%Y-%m-%d")
target_df$hour <- format(target_df$Datetime, "%H")

################################################################################
## 6) Merge all data
################################################################################
final_df <- merge(target_df, water_hourly,by = c("Datetime", "Date", "hour"), all.x = TRUE)
final_df <- merge(final_df, air_hourly,   by = c("Datetime", "Date", "hour"), all.x = TRUE)
final_df <- merge(final_df, met_use,      by = c("Datetime", "Date", "hour"), all.x = TRUE)

################################################################################
## 7) Fill final underwater PAR
################################################################################
final_df$Water_PAR_final  <- NA
final_df$Water_PAR_source <- NA

## normal dates: not 2025-09-25
idx_normal <- final_df$Date != "2025-09-25"

## 1) measured water PAR
idx1 <- idx_normal & !is.na(final_df$Water_PAR_hour_mean)
final_df$Water_PAR_final[idx1]  <- final_df$Water_PAR_hour_mean[idx1]
final_df$Water_PAR_source[idx1] <- "Measured_water_PAR"

## 2) air PAR * 0.8
idx2 <- idx_normal & is.na(final_df$Water_PAR_final) & !is.na(final_df$Air_PAR_hour_mean)

final_df$Water_PAR_final[idx2]  <- final_df$Air_PAR_hour_mean[idx2] * 0.8
final_df$Water_PAR_source[idx2] <- "Filled_by_air_PAR_x_0.8"

## 3a) shortwave fit_g1 * 0.8
dates_g1 <- c("2025-09-19", "2025-09-21", "2025-09-22", "2025-09-23")
dates_g2 <- c("2025-09-20", "2025-09-24")

idx3_g1 <- idx_normal &
  is.na(final_df$Water_PAR_final) &
  final_df$Date %in% dates_g1 &
  !is.na(final_df$short_radi_W_m2)

if (any(idx3_g1)) {
  pred_air_g1 <- predict(fit_g1, newdata = final_df[idx3_g1, ])
  pred_air_g1[pred_air_g1 < 0] <- 0
  
  final_df$Water_PAR_final[idx3_g1]  <- pred_air_g1 * 0.8
  final_df$Water_PAR_source[idx3_g1] <- "Filled_by_shortwave_fit_g1_x_0.8"
}

## 3b) shortwave fit_g2 * 0.8
idx3_g2 <- idx_normal & is.na(final_df$Water_PAR_final) &
  final_df$Date %in% dates_g2 &
  !is.na(final_df$short_radi_W_m2)

if (any(idx3_g2)) {
  pred_air_g2 <- predict(fit_g2, newdata = final_df[idx3_g2, ])
  pred_air_g2[pred_air_g2 < 0] <- 0
  
  final_df$Water_PAR_final[idx3_g2]  <- pred_air_g2 * 0.8
  final_df$Water_PAR_source[idx3_g2] <- "Filled_by_shortwave_fit_g2_x_0.8"
}

## 2025-09-25 only
idx_25 <- final_df$Date == "2025-09-25"

## 1) air PAR * 0.8
idx25_air <- idx_25 & !is.na(final_df$Air_PAR_hour_mean)
final_df$Water_PAR_final[idx25_air]  <- final_df$Air_PAR_hour_mean[idx25_air] * 0.8
final_df$Water_PAR_source[idx25_air] <- "09_25_air_PAR_x_0.8"

## 2) shortwave fit * 0.8
idx25_sw <- idx_25 &
  is.na(final_df$Water_PAR_final) &
  !is.na(final_df$short_radi_W_m2)

if (any(idx25_sw)) {
  pred_air_25 <- predict(fit_g2, newdata = final_df[idx25_sw, ])
  pred_air_25[pred_air_25 < 0] <- 0
  
  final_df$Water_PAR_final[idx25_sw]  <- pred_air_25 * 0.8
  final_df$Water_PAR_source[idx25_sw] <- "09_25_shortwave_fit_x_0.8"
}

################################################################################
## 8) Check missing
################################################################################
cat("Remaining missing final hourly water PAR rows:\n")
print(final_df[is.na(final_df$Water_PAR_final), c("Datetime", "Date", "hour", "short_radi_W_m2", "air_temp")])

cat("Check 2025-09-25:\n")
print(final_df[final_df$Date == "2025-09-25",
           c("Datetime", "Water_PAR_hour_mean", "Air_PAR_hour_mean",
             "short_radi_W_m2", "Water_PAR_final", "Water_PAR_source")]
)

################################################################################
## 9) Export main output
################################################################################
out_file <- "../Output_Data/water_PAR_hourly_final_filled.xlsx"

wb <- createWorkbook()
addWorksheet(wb, "Final_hourly")
writeData(wb, "Final_hourly", final_df)
addWorksheet(wb, "Air_hourly")
writeData(wb, "Air_hourly", air_hourly)
addWorksheet(wb, "Water_hourly")
writeData(wb, "Water_hourly", water_hourly)
addWorksheet(wb, "Met_use")
writeData(wb, "Met_use", met_use)
addWorksheet(wb, "Fit_summary")

fit_summary_out <- data.frame(Model = c("fit_g1_21_22_23", "fit_g2_25"),
  Intercept = c(coef(fit_g1)[1], coef(fit_g2)[1]),
  Slope     = c(coef(fit_g1)[2], coef(fit_g2)[2]),
  R2        = c(summary(fit_g1)$r.squared, summary(fit_g2)$r.squared))

writeData(wb, "Fit_summary", fit_summary_out)

#saveWorkbook(wb, out_file, overwrite = TRUE)

################################################################################
## 10) Prepare plotting data and export
################################################################################
plot_df <- final_df[order(final_df$Datetime), ]
plot_df$Hour_num <- as.numeric(format(plot_df$Datetime, "%H"))

## date-specific scaling
plot_df$Water_PAR_scaled <- NA
idx_09 <- plot_df$Date %in% c("2025-09-19", "2025-09-21", "2025-09-22", "2025-09-23")
idx_02 <- plot_df$Date %in% c("2025-09-20", "2025-09-24", "2025-09-25")
plot_df$Water_PAR_scaled[idx_09] <- plot_df$Water_PAR_final[idx_09] * 0.9
plot_df$Water_PAR_scaled[idx_02] <- plot_df$Water_PAR_final[idx_02] * 0.2

water_plot <- water_hourly[order(water_hourly$Datetime), ]
water_plot$Hour_num <- as.numeric(water_plot$hour)

air_plot <- air_hourly[order(air_hourly$Datetime), ]
air_plot$Hour_num <- as.numeric(air_plot$hour)

temp_plot <- met_use[order(met_use$Datetime), ]
temp_plot$Hour_num <- as.numeric(temp_plot$hour)

plot_data_file <- "../Output_Data/PAR_temperature_plot_data.xlsx"
wb2 <- createWorkbook()
addWorksheet(wb2, "plot_df_final_PAR")
writeData(wb2, "plot_df_final_PAR", plot_df)
addWorksheet(wb2, "water_plot_measured")
writeData(wb2, "water_plot_measured", water_plot)
addWorksheet(wb2, "air_plot_measured")
writeData(wb2, "air_plot_measured", air_plot)
addWorksheet(wb2, "temp_plot_air_temp")
writeData(wb2, "temp_plot_air_temp", temp_plot)

#saveWorkbook(wb2, plot_data_file, overwrite = TRUE)
#write.csv(plot_df,    "../Output_Data/plot_df_final_PAR.csv", row.names = FALSE)
#write.csv(water_plot, "../Output_Data/water_plot_measured.csv", row.names = FALSE)
#write.csv(air_plot,   "../Output_Data/air_plot_measured.csv", row.names = FALSE)
#write.csv(temp_plot,  "../Output_Data/temp_plot_air_temp.csv", row.names = FALSE)

################################################################################
## Global plotting settings
################################################################################
plot_width  <- 10
plot_height <- 4.5

par_family   <- "sans"
par_cex      <- 1.2
par_cex_axis <- 1.2
par_cex_lab  <- 1.2
par_mar      <- c(4, 4, 1, 4) + 0.1

## x-axis positions shared by all time-series plots
day_starts <- as.POSIXct(
  paste(unique(plot_df$Date), "00"),
  format = "%Y-%m-%d %H",
  tz = "UTC"
)

################################################################################
## 11) Plot 1: final PAR + scaled PAR + air temperature
################################################################################
pdf("../Output_Figures/PAR_airtemp.pdf",
    width = plot_width, height = plot_height, family = par_family)

par(mar = par_mar, family = par_family,
    cex = par_cex, cex.axis = par_cex_axis, cex.lab = par_cex_lab)

## left axis
plot(plot_df$Datetime, plot_df$Water_PAR_final,
     type = "n", xlab = "", ylab = "", xaxt = "n", yaxs = "i", ylim = c(0, 2000))

## 左侧纵坐标标题
mtext(expression("Light intensity (" * mu * "mol photons " * m^-2 * " " * s^-1 * ")"),
      side = 2, line = 2.5, cex = 1.3)

date_levels <- unique(plot_df$Date)

for (d in date_levels) {
  
  ## final PAR
  idx1_all <- plot_df$Date == d &
    !is.na(plot_df$Water_PAR_final) &
    plot_df$Water_PAR_final > 0
  
  if (sum(idx1_all) > 0) {
    points(plot_df$Datetime[idx1_all],
           plot_df$Water_PAR_final[idx1_all],
           pch = 16, cex = 0.8, col = col_par)
  }
  
  idx1_day <- plot_df$Date == d &
    !is.na(plot_df$Water_PAR_final) &
    plot_df$Water_PAR_final > 0 &
    plot_df$Hour_num >= 6 &
    plot_df$Hour_num <= 18
  
  if (sum(idx1_day) >= 2) {
    lines(plot_df$Datetime[idx1_day],
          plot_df$Water_PAR_final[idx1_day],
          lwd = 2, lty = 1, col = col_par)
  }
  
  ## scaled PAR
  idx2_all <- plot_df$Date == d &
    !is.na(plot_df$Water_PAR_scaled) &
    plot_df$Water_PAR_scaled > 0
  
  if (sum(idx2_all) > 0) {
    points(plot_df$Datetime[idx2_all],
           plot_df$Water_PAR_scaled[idx2_all],
           pch = 1, cex = 0.8, col = col_par_sub)
  }
  
  idx2_day <- plot_df$Date == d &
    !is.na(plot_df$Water_PAR_scaled) &
    plot_df$Water_PAR_scaled > 0 &
    plot_df$Hour_num >= 6 &
    plot_df$Hour_num <= 18
  
  if (sum(idx2_day) >= 2) {
    lines(plot_df$Datetime[idx2_day],
          plot_df$Water_PAR_scaled[idx2_day],
          lwd = 1.8, lty = 2, col = col_par_sub)
  }
}

axis.POSIXct(1, at = day_starts, format = "%m-%d", las = 2)

## right axis
par(new = TRUE)

plot(temp_plot$Datetime, temp_plot$air_temp,
     type = "n", axes = FALSE, xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(15, 30))

for (d in unique(temp_plot$Date)) {
  idx <- temp_plot$Date == d & !is.na(temp_plot$air_temp)
  if (sum(idx) >= 2) {
    lines(temp_plot$Datetime[idx],
          temp_plot$air_temp[idx],
          col = col_airtemp, lwd = 1.2, lty = 1)
  }
}

axis(4, col.axis = col_airtemp, col = col_airtemp)
mtext("Air temperature (°C)", side = 4, line = 2.5, col = col_airtemp, cex = 1.5)

legend("top",
       legend = c("Below water surface", "Incubation depth", "Air temperature"),
       col = c(col_par, col_par_sub, col_airtemp),
       lty = c(1, 2, 1), lwd = c(2, 1.8, 1.2), pch = c(16, 1, NA), cex = 1.0, bty = "n")

box()
dev.off()
################################################################################
## 12) Plot 2: final PAR + scaled PAR only
################################################################################
#pdf("../Output_Figures/Plot2_final_PAR_and_scaled.pdf",
#    width = plot_width, height = plot_height, family = par_family)
#par(mar = par_mar, family = par_family, cex = par_cex, cex.axis = par_cex_axis, cex.lab = par_cex_lab)
#plot(plot_df$Datetime, plot_df$Water_PAR_final,
#     type = "n", xlab = "", ylab = "", xaxt = "n", yaxs = "i", ylim = c(0, 2000))
#mtext(expression("Light intensity (" * mu * "mol photons " * m^-2 * " " * s^-1 * ")"),
#      side = 2, line = 2.5, cex = 1.3)
#for (d in date_levels) {
#  idx1_all <- plot_df$Date == d &
#    !is.na(plot_df$Water_PAR_final) &
#    plot_df$Water_PAR_final > 0
#  if (sum(idx1_all) > 0) {
#    points(plot_df$Datetime[idx1_all],
#           plot_df$Water_PAR_final[idx1_all],
#           pch = 16, cex = 0.8, col = col_par)
#  }
#idx1_day <- plot_df$Date == d &
#    !is.na(plot_df$Water_PAR_final) &
#    plot_df$Water_PAR_final > 0 &
#    plot_df$Hour_num >= 6 &
#    plot_df$Hour_num <= 18
#  if (sum(idx1_day) >= 2) {
#    lines(plot_df$Datetime[idx1_day],
#          plot_df$Water_PAR_final[idx1_day],
#          lwd = 2, lty = 1, col = col_par)
#  }
#  idx2_all <- plot_df$Date == d &
#    !is.na(plot_df$Water_PAR_scaled) &
#    plot_df$Water_PAR_scaled > 0
#  
#  if (sum(idx2_all) > 0) {
#    points(plot_df$Datetime[idx2_all],
#           plot_df$Water_PAR_scaled[idx2_all],
#           pch = 1, cex = 0.8, col = col_par_sub)
#  }
  
#  idx2_day <- plot_df$Date == d &
#    !is.na(plot_df$Water_PAR_scaled) &
#    plot_df$Water_PAR_scaled > 0 &
#    plot_df$Hour_num >= 6 &
#    plot_df$Hour_num <= 18
#  
#  if (sum(idx2_day) >= 2) {
#    lines(plot_df$Datetime[idx2_day],
#          plot_df$Water_PAR_scaled[idx2_day],
#          lwd = 1.8, lty = 2, col = col_par_sub)
#  }
#}

#axis.POSIXct(1, at = day_starts, format = "%m-%d", las = 2)
#legend("topright", legend = c("Below water surface", "Incubation depth"),
#       col = c(col_par, col_par_sub), lty = c(1, 2), lwd = c(2, 1.8), pch = c(16, 1),
#       cex = 1.0, bty = "n")

#box()
#dev.off()

################################################################################
## 13) Plot 3: measured water PAR
################################################################################
#pdf("../Output_Figures/Plot3_measured_water_PAR_hourly.pdf",
#    width = plot_width, height = plot_height, family = par_family)
#par(mar = par_mar,
#    family = par_family,
#   cex = par_cex,
#    cex.axis = par_cex_axis,
#    cex.lab = par_cex_lab)
#plot(water_plot$Datetime, water_plot$Water_PAR_hour_mean, type = "n", xlab = "",
#     ylab = expression("Light intensity (" * mu * "mol photons " * m^-2 * " " * s^-1 * ")"),
#     xaxt = "n", yaxs = "i")
#for (d in unique(water_plot$Date)) {
#  idx_all <- water_plot$Date == d &
#    !is.na(water_plot$Water_PAR_hour_mean) &
#    water_plot$Water_PAR_hour_mean > 0
#   if (sum(idx_all) > 0) {
#    points(water_plot$Datetime[idx_all],
#           water_plot$Water_PAR_hour_mean[idx_all],
#           pch = 16, cex = 0.8, col = col_par)
#  }
#  idx_day <- water_plot$Date == d &
#    !is.na(water_plot$Water_PAR_hour_mean) &
#    water_plot$Water_PAR_hour_mean > 0 &
#    water_plot$Hour_num >= 6 &
#    water_plot$Hour_num <= 18

 # if (sum(idx_day) >= 2) {
#    lines(water_plot$Datetime[idx_day],
#          water_plot$Water_PAR_hour_mean[idx_day],
#          lwd = 2, lty = 1, col = col_par)
#  }
#}

#axis.POSIXct(1, at = day_starts, format = "%m-%d", las = 2)
#box()
#dev.off()

################################################################################
## 14) Plot 4: measured air PAR
################################################################################
#pdf("../Output_Figures/Plot4_measured_air_PAR_hourly.pdf",
#    width = plot_width, height = plot_height, family = par_family)
#par(mar = par_mar, family = par_family, cex = par_cex, cex.axis = par_cex_axis,
#    cex.lab = par_cex_lab)
#plot(air_plot$Datetime, air_plot$Air_PAR_hour_mean,
#     type = "n", xlab = "",
#     ylab = expression("Light intensity (" * mu * "mol photons " * m^-2 * " " * s^-1 * ")"),
#     xaxt = "n", yaxs = "i")
#for (d in unique(air_plot$Date)) {
#  idx_all <- air_plot$Date == d &
#    !is.na(air_plot$Air_PAR_hour_mean) &
#    air_plot$Air_PAR_hour_mean > 0
#  if (sum(idx_all) > 0) {
#   points(air_plot$Datetime[idx_all],
#           air_plot$Air_PAR_hour_mean[idx_all],
#           pch = 16, cex = 0.8, col = col_par)
#  }
#  idx_day <- air_plot$Date == d &
#    !is.na(air_plot$Air_PAR_hour_mean) &
#    air_plot$Air_PAR_hour_mean > 0 &
#    air_plot$Hour_num >= 6 &
#    air_plot$Hour_num <= 18
#  if (sum(idx_day) >= 2) {
#    lines(air_plot$Datetime[idx_day],
#          air_plot$Air_PAR_hour_mean[idx_day],
#          lwd = 2, lty = 1, col = col_par)
#  }
#}
#axis.POSIXct(1, at = day_starts, format = "%m-%d", las = 2)
#box()
#dev.off()

################################################################################
## 15) Plot 5: air temperature
################################################################################
#pdf("../Output_Figures/Plot5_air_temperature_hourly.pdf",
#    width = plot_width, height = plot_height, family = par_family)
#par(mar = par_mar,
#    family = par_family,
#    cex = par_cex,
#    cex.axis = par_cex_axis,
#    cex.lab = par_cex_lab)

#plot(temp_plot$Datetime, temp_plot$air_temp,
#     type = "n",
#     xlab = "",
#     ylab = expression("Air temperature (" * degree * "C)"),
#     xaxt = "n",
#     yaxs = "i")

#for (d in unique(temp_plot$Date)) {
#  
#  idx_all <- temp_plot$Date == d & !is.na(temp_plot$air_temp)
#  
#  if (sum(idx_all) > 0) {
#    points(temp_plot$Datetime[idx_all],
#           temp_plot$air_temp[idx_all],
#           pch = 16, cex = 0.8, col = col_airtemp)
#  }
#  
#  idx_day <- temp_plot$Date == d & !is.na(temp_plot$air_temp)
#  
#  if (sum(idx_day) >= 2) {
#    lines(temp_plot$Datetime[idx_day],
#          temp_plot$air_temp[idx_day],
#          lwd = 2, lty = 1, col = col_airtemp)
#  }
#}

#axis.POSIXct(1, at = day_starts, format = "%m-%d", las = 2)
#box()
#dev.off()

################################################################################
## 16) Water temperature time-series
################################################################################
temp_raw <- read.xlsx("../Input_Data/insitu_water_temp.xlsx", sheet = 1)
temp_raw <- as.data.frame(temp_raw, stringsAsFactors = FALSE)

names(temp_raw) <- trimws(names(temp_raw))
names(temp_raw)[names(temp_raw) == "Water.temperature.°C"] <- "Water_temp"

temp_raw$Device <- as.character(temp_raw$Device)
temp_raw$Water_temp <- as.numeric(as.character(temp_raw$Water_temp))
temp_raw$Date <- as.POSIXct(temp_raw$Date * 86400, origin = "1899-12-30", tz = "UTC")

temp_raw <- temp_raw[
  !is.na(temp_raw$Device) &
    !is.na(temp_raw$Date) &
    !is.na(temp_raw$Water_temp), ]

temp_raw <- temp_raw[order(temp_raw$Date, temp_raw$Device), ]

temp_mean <- ddply(temp_raw, .(Date), summarise,
                   Water_temp_mean = mean(Water_temp, na.rm = TRUE))
temp_mean <- temp_mean[order(temp_mean$Date), ]

## export
out_xlsx <- "../Output_Data/water_temperature_timeseries.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "raw_data")
#writeData(wb, "raw_data", temp_raw)
addWorksheet(wb, "mean_by_time")
#writeData(wb, "mean_by_time", temp_mean)
#saveWorkbook(wb, out_xlsx, overwrite = TRUE)

#write.csv(temp_raw,  "../Output_Data/water_temperature_raw.csv", row.names = FALSE)
#write.csv(temp_mean, "../Output_Data/water_temperature_mean_by_time.csv", row.names = FALSE)

## plot
pdf("../Output_Figures/Water_temperature_timeseries.pdf",
    width = plot_width, height = plot_height, family = par_family)

par(mar = par_mar,
    family = par_family,
    cex = par_cex,
    cex.axis = par_cex_axis,
    cex.lab = par_cex_lab)

plot(temp_raw$Date, temp_raw$Water_temp,
     type = "n",
     xlab = "",
     ylab = expression("Water temperature (" * degree * "C)"),
     xaxt = "n",
     yaxs = "i",
     ylim = c(23, 30))

day_starts_temp <- seq(
  from = as.POSIXct(format(min(temp_raw$Date), "%Y-%m-%d 00:00:00"), tz = "UTC"),
  to   = as.POSIXct(format(max(temp_raw$Date), "%Y-%m-%d 00:00:00"), tz = "UTC"),
  by   = "1 day"
)

axis.POSIXct(1, at = day_starts_temp, format = "%m-%d", las = 2)

dev_levels <- sort(unique(temp_raw$Device))
dev_cols <- c("1" = "#D55E00", "2" = "#0072B2", "3" = "#009E73")

if (!all(dev_levels %in% names(dev_cols))) {
  auto_cols <- c("#D55E00", "#0072B2", "#009E73", "#E69F00", "#CC79A7")
  dev_cols <- auto_cols[1:length(dev_levels)]
  names(dev_cols) <- dev_levels
}

for (d in dev_levels) {
  sub <- temp_raw[temp_raw$Device == d, ]
  points(sub$Date, sub$Water_temp,
         pch = 16, cex = 0.45, col = dev_cols[d])
}

legend("topright",
       legend = paste("Device", dev_levels),
       col = dev_cols[dev_levels], pch = 16, pt.cex = 0.8, cex = 1.0,  bty = "n")

box()

dev.off()