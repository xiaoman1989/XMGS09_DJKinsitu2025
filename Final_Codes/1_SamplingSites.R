# -----------------------------
# 0) 加载库
# -----------------------------
library(readxl)
library(dplyr)

# -----------------------------
# 0.1) 全局字号/点大小设置
# -----------------------------
cex_axis  <- 1.8  # 坐标轴标题 + 刻度
cex_label <- 2.2
cex_panel <- 1.6   # (A)-(F) panel label
cex_text  <- 2.1   # 图中文字（公式、k、Zeu、legend）
cex_point <- 1.8   # 点大小

# -----------------------------
# 1) 读取数据（Site 2）
# -----------------------------
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

safe_read <- function(path){
  if (file.exists(path)) return(na.omit(read_excel(path)))
  return(NULL)
}

site2_temp <- safe_read("../Input_Data/ysi_site2_temp.xlsx")
site2_chla <- safe_read("../Input_Data/ysi_site2_chla.xlsx")
site2_do   <- safe_read("../Input_Data/ysi_site2_od.xlsx")
site2_cond <- safe_read("../Input_Data/ysi_site2_cond.xlsx")
site2_pH   <- safe_read("../Input_Data/ysi_site2_pH.xlsx")
site2_par  <- safe_read("../Input_Data/Licor_site2_PAR.xlsx")

# 添加 Site 标签
add_site <- function(df, name){
  if(!is.null(df)) df$Site <- name
  df
}

site2_temp <- add_site(site2_temp, "Site 2")
site2_chla <- add_site(site2_chla, "Site 2")
site2_do   <- add_site(site2_do,   "Site 2")
site2_cond <- add_site(site2_cond, "Site 2")
site2_pH   <- add_site(site2_pH,   "Site 2")
site2_par  <- add_site(site2_par,  "Site 2")

# -----------------------------
# 2) 辅助函数
# -----------------------------
prep_profile <- function(df1, df2 = NULL, xcol, ycol = "Vertical_Position_m"){
  
  if(is.null(df1)) return(NULL)
  
  df <- df1[, c(xcol, ycol, "Site")]
  
  if(!is.null(df2)){
    df <- rbind(df, df2[, c(xcol, ycol, "Site")])
  }
  
  df <- df[is.finite(df[[xcol]]) & is.finite(df[[ycol]]), ]
  df <- df[order(df[[ycol]]), ]
  
  df
}

# PAR：按 Time + 深度平均
avg_by_depth_time <- function(df, xcol, ycol = "Vertical_Position_m"){
  
  if(is.null(df)) return(NULL)
  
  aggregate(df[[xcol]],
            by = list(
              Site  = df$Site,
              Time  = df$Time,
              Depth = df[[ycol]]
            ),
            FUN = mean, na.rm = TRUE) |>
    `colnames<-`(c("Site", "Time", ycol, xcol))
}

# PAR专用整理函数：保留 Time
prep_profile_par <- function(df, xcol, ycol = "Vertical_Position_m"){
  
  if(is.null(df)) return(NULL)
  
  df <- df[, c(xcol, ycol, "Site", "Time")]
  df <- df[is.finite(df[[xcol]]) & is.finite(df[[ycol]]), ]
  df <- df[order(df$Time, df[[ycol]]), ]
  
  df
}

add_panel_label <- function(label){
  mtext(label, side = 3, adj  = 0.02, line = -2, cex  = cex_panel, font = 1)
}

# 横坐标使用较少的主刻度，避免大字号下刻度标签过密
make_x_ticks <- function(xlim_use, n = 3){
  ticks <- pretty(xlim_use, n = n)
  ticks[ticks >= xlim_use[1] & ticks <= xlim_use[2]]
}

# -----------------------------
# 3) 单站点绘图函数
# -----------------------------
plot_single_site <- function(df, xcol, xlab, ylab = "",
                             col = "blue", dot_cex = cex_point,
                             x_ticks = NULL){
  
  if(is.null(df) || nrow(df) == 0){
    plot.new()
    text(0.5, 0.5, "No data", cex = cex_text)
    return(invisible(NULL))
  }
  
  ylim_use <- rev(range(df$Vertical_Position_m, na.rm = TRUE))
  xlim_use <- range(df[[xcol]], na.rm = TRUE)
  if(is.null(x_ticks)) x_ticks <- make_x_ticks(xlim_use)
  xlim_plot <- range(c(xlim_use, x_ticks), na.rm = TRUE)
  
  plot(df[[xcol]], df$Vertical_Position_m,
       pch  = 16,
       col  = col,
       cex  = dot_cex,
       xlim = xlim_plot,
       ylim = ylim_use,
       xlab = xlab,
       ylab = ylab,
       xaxt = "n")
  axis(1, at = x_ticks)
}

# -----------------------------
# 4) 光强绘图（区分 morning / afternoon）
# -----------------------------
plot_light_fit_by_time <- function(par_df,
                                   xlab = "Light intensity (PAR)",
                                   ylab = "",
                                   dot_cex = cex_point,
                                   text_cex = cex_text,
                                   x_ticks = NULL){
  
  if(is.null(par_df) || nrow(par_df) == 0){
    plot.new()
    text(0.5, 0.5, "No PAR data", cex = text_cex)
    return(invisible(NULL))
  }
  
  if(!"Time" %in% colnames(par_df)){
    stop("par_df 必须包含 'Time' 列，值为 'morning' 或 'afternoon'")
  }
  
  df <- par_df |>
    mutate(
      Time  = trimws(as.character(Time)),
      Depth = abs(Vertical_Position_m),
      I     = light_intensity
    ) |>
    filter(is.finite(Depth), is.finite(I), I > 0)
  
  if(nrow(df) == 0){
    plot.new()
    text(0.5, 0.5, "No valid PAR data", cex = text_cex)
    return(invisible(NULL))
  }
  
  colors <- c("morning" = "blue", "afternoon" = "red")
  
  ylim_use <- rev(range(df$Depth, na.rm = TRUE))
  xlim_use <- range(df$I, na.rm = TRUE)
  if(is.null(x_ticks)) x_ticks <- make_x_ticks(xlim_use)
  xlim_plot <- range(c(xlim_use, x_ticks), na.rm = TRUE)
  depth_span <- diff(range(df$Depth, na.rm = TRUE))
  
  plot(0, 0, type = "n",
       xlim = xlim_plot,
       ylim = ylim_use,
       xlab = xlab,
       ylab = ylab,
       xaxt = "n")
  axis(1, at = x_ticks)
  
  time_levels <- intersect(c("morning", "afternoon"), unique(df$Time))
  
  for(i in seq_along(time_levels)){
    t <- time_levels[i]
    sub_df <- df[df$Time == t, ]
    
    points(sub_df$I, sub_df$Depth,
           pch = 16, col = colors[t], cex = dot_cex)
    
    if(nrow(sub_df) >= 3){
      
      I0_start <- sub_df$I[which.min(sub_df$Depth)]
      if(!is.finite(I0_start) || I0_start <= 0){
        I0_start <- max(sub_df$I, na.rm = TRUE)
      }
      
      max_depth <- max(sub_df$Depth, na.rm = TRUE)
      k_start <- ifelse(max_depth > 0, 0.5 / max_depth, 0.1)
      
      fit <- try(
        nls(I ~ I0 * exp(-k * Depth),
            data = sub_df,
            start = list(I0 = I0_start, k = k_start),
            control = nls.control(maxiter = 300, warnOnly = TRUE)),
        silent = TRUE
      )
      
      if(!inherits(fit, "try-error")){
        z <- seq(min(sub_df$Depth), max(sub_df$Depth), length.out = 200)
        pred <- predict(fit, data.frame(Depth = z))
        lines(pred, z, col = colors[t], lwd = 2)
        
        k <- coef(fit)["k"]
        zeu <- 4.605 / k
        
       x_text <- xlim_use[2]
        
        # 标注整体下移，避免遮住曲线
        if(t == "morning"){
          y_text <- min(df$Depth, na.rm = TRUE) + 0.32 * depth_span
        } else if(t == "afternoon"){
          y_text <- min(df$Depth, na.rm = TRUE) + 0.46 * depth_span
        } else {
          y_text <- min(df$Depth, na.rm = TRUE) + 0.32 * depth_span
        }
        
        text(x_text, y_text,
             labels = paste0(t,
    #                         "\nk = ", round(k, 2),
                             ", Zeu = ", round(zeu, 2), " m"),
             col = colors[t],
             pos = 2,
             cex = text_cex)
      }
    }
  }
  
  # 公式进一步下移
 # text(xlim_use[2],
 #      min(df$Depth, na.rm = TRUE) + 0.6 * depth_span,
 #      labels = expression(I(z) == I[0] ~ e^{-k * z}),
 #      pos = 2,
 #      cex = text_cex)
}

# -----------------------------
# 5) 构建数据集
# -----------------------------
temp_df <- prep_profile(site2_temp, NULL, "Temp °C")
chla_df <- prep_profile(site2_chla, NULL, "Chlorophyll ug/L")
do_df   <- prep_profile(site2_do,   NULL, "ODO mg/L")
cond_df <- prep_profile(site2_cond, NULL, "Cond µS/cm")
pH_df   <- prep_profile(site2_pH,   NULL, "pH")

# 对光强按 Time + 深度平均
if(!is.null(site2_par)){
  site2_par$Time <- trimws(as.character(site2_par$Time))
  site2_par2 <- avg_by_depth_time(site2_par, "light_intensity")
  par_df <- prep_profile_par(site2_par2, "light_intensity")
} else {
  par_df <- NULL
}

# -----------------------------
# 6) 绘图到 PDF 和 PNG
# -----------------------------
dir.create("../Output_Figures", showWarnings = FALSE, recursive = TRUE)

draw_site2_panels <- function() {
par(family = "sans",
    mfrow    = c(2, 3),
    mar      = c(6.2, 3.5, 2.2, 1.2),
    oma      = c(0, 4.2, 0, 0),
    mgp      = c(4.0, 1.0, 0),
    cex.lab  = cex_label,
    cex.axis = cex_axis)

# (A) Temperature
plot_single_site(
  temp_df,
  "Temp °C",
  expression(paste("Water temperature (", degree, "C)")),
  dot_cex = cex_point,
  x_ticks = c(15, 20, 25)
)
add_panel_label("(A)")

# (B) Chlorophyll-a
plot_single_site(
  chla_df,
  "Chlorophyll ug/L",
  expression(paste("Chlorophyll ", italic(a), " (", mu, "g L"^-1, ")")),
  dot_cex = cex_point,
  x_ticks = c(0, 0.5, 1.0, 1.5)
)
add_panel_label("(B)")

# (C) DO
plot_single_site(
  do_df,
  "ODO mg/L",
  expression(paste("DO (mg L"^-1, ")")),
  dot_cex = cex_point,
  x_ticks = c(2, 4, 6, 8)
)
add_panel_label("(C)")

# (D) Conductivity
plot_single_site(
  cond_df,
  "Cond µS/cm",
  expression(paste("Conductivity (", mu, "S cm"^-1, ")")),
  dot_cex = cex_point,
  x_ticks = c(260, 280, 300)
)
add_panel_label("(D)")

# (E) pH
plot_single_site(
  pH_df,
  "pH",
  "pH",
  dot_cex = cex_point,
  x_ticks = c(7.8, 7.9, 8.0, 8.1)
)
add_panel_label("(E)")

# (F) Light / PAR
plot_light_fit_by_time(
  par_df,
  dot_cex  = cex_point,
  x_ticks = c(0, 500, 1000)
  #,
  #text_cex = cex_text
)
add_panel_label("(F)")

# 全图共用一个纵坐标标题，避免六个面板重复显示
mtext("Water depth (m)", side = 2, outer = TRUE,
      line = 2.0, cex = cex_axis, family = "sans")
}

pdf("../Output_Figures/YSI_Site2_6panels.pdf",
    width = 12, height = 10, family = "sans")
draw_site2_panels()
dev.off()

png("../Output_Figures/YSI_Site2_6panels.png",
    width = 12, height = 10, units = "in", res = 300, bg = "white")
draw_site2_panels()
dev.off()
