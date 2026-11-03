################################################################################
## XMGS09 ONLY
## Growth rate of total cell concentration
## Day0 -> Day2 : dt = 2
## Day2 -> Day6 : dt = 4
## For Day2 -> Day6: Day2_cells * 0.8
################################################################################

library(openxlsx)
library(plyr)
library(ggplot2)
library(patchwork)

################################################################################
## 1) READ DATA
################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

data2 <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
data2 <- as.data.frame(data2, stringsAsFactors = FALSE)

## clean
data2$Treatment    <- trimws(as.character(data2$Treatment))
data2$Replication  <- trimws(as.character(data2$Replication))
data2$Day          <- as.numeric(as.character(data2$Day))
data2$Latin_phylum <- trimws(as.character(data2$Latin_phylum))
data2$Cells_mL     <- as.numeric(as.character(data2$Cells_mL))
data2$Day_label    <- paste0("Day", data2$Day)

################################################################################
## 2) KEEP ONLY WANTED DATA
################################################################################
keep_idx_daily <- (
  (data2$Day_label == "Day0" & data2$Treatment == "Initial") |
    (data2$Day_label == "Day2" &
       data2$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P")) |
    (data2$Day_label == "Day6" &
       data2$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"))
)

df_daily <- data2[keep_idx_daily, , drop = FALSE]

################################################################################
## 3) REPLICATE-LEVEL TOTAL CELLS
################################################################################
xmgs09_total_daily_rep <- ddply(
  df_daily,
  .variables = c("Day_label", "Treatment", "Replication"),
  .fun = function(x){
    data.frame(Total_cells = sum(x$Cells_mL, na.rm = TRUE))
  }
)

################################################################################
## 4) DAY0 -> DAY2 GROWTH RATE (dt = 2)
################################################################################

## mean initial total cells
xmgs09_init_mean <- mean(
  xmgs09_total_daily_rep$Total_cells[
    xmgs09_total_daily_rep$Day_label == "Day0" &
      xmgs09_total_daily_rep$Treatment == "Initial"
  ],
  na.rm = TRUE
)

## Day2 data
xmgs09_Day2_daily <- xmgs09_total_daily_rep[
  xmgs09_total_daily_rep$Day_label == "Day2" &
    xmgs09_total_daily_rep$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"),
  ,
  drop = FALSE
]

## growth rate mu = ln(Day2 / mean Initial) / 2
xmgs09_Day2_daily$Initial_mean_cells <- xmgs09_init_mean
xmgs09_Day2_daily$Growth_rate <- ifelse(
  !is.na(xmgs09_Day2_daily$Initial_mean_cells) &
    xmgs09_Day2_daily$Initial_mean_cells > 0 &
    !is.na(xmgs09_Day2_daily$Total_cells) &
    xmgs09_Day2_daily$Total_cells > 0,
  log(xmgs09_Day2_daily$Total_cells / xmgs09_Day2_daily$Initial_mean_cells) / 2,
  NA
)

## x positions
xmgs09_Day2_daily$x_plot <- NA_real_
xmgs09_Day2_daily$x_plot[xmgs09_Day2_daily$Treatment == "0P"]     <- 0
xmgs09_Day2_daily$x_plot[xmgs09_Day2_daily$Treatment == "0.005P"] <- 0.005
xmgs09_Day2_daily$x_plot[xmgs09_Day2_daily$Treatment == "0.01P"]  <- 0.01
xmgs09_Day2_daily$x_plot[xmgs09_Day2_daily$Treatment == "0.025P"] <- 0.025
xmgs09_Day2_daily$x_plot[xmgs09_Day2_daily$Treatment == "0.05P"]  <- 0.05

## group
xmgs09_Day2_daily$Group <- "Expt2_0N_Day0_2"
xmgs09_Day2_daily$Stage <- "Day2_vs_Day0"

xmgs09_Day2_daily <- xmgs09_Day2_daily[, c(
  "Replication", "Treatment", "Total_cells",
  "Initial_mean_cells", "Growth_rate",
  "x_plot", "Group", "Stage"
)]

################################################################################
## 5) DAY2 -> DAY6 GROWTH RATE (dt = 4)
################################################################################

## Day2 source table for matching
Day2_source <- xmgs09_total_daily_rep[
  xmgs09_total_daily_rep$Day_label == "Day2" &
    xmgs09_total_daily_rep$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"),
  c("Treatment", "Replication", "Total_cells"),
  drop = FALSE
]

names(Day2_source)[names(Day2_source) == "Treatment"]   <- "Day2_Treatment"
names(Day2_source)[names(Day2_source) == "Total_cells"] <- "Day2_cells"

## Day6 table
Day6_target <- xmgs09_total_daily_rep[
  xmgs09_total_daily_rep$Day_label == "Day6" &
    xmgs09_total_daily_rep$Treatment %in% c("0P", "0.005P","0.01P", "0.025P", "0.05P"),
  ,
  drop = FALSE
]

## map Day6 treatment to Day2 treatment
Day6_target$Day2_match <- as.character(Day6_target$Treatment)

## merge Day6 with matched Day2 by treatment + replication
xmgs09_Day6_daily <- merge(
  Day6_target,
  Day2_source,
  by.x = c("Day2_match", "Replication"),
  by.y = c("Day2_Treatment", "Replication"),
  all.x = TRUE
)

## adjusted Day2 denominator
## default = Day2 * 0.8
xmgs09_Day6_daily$Day2_adjusted <- xmgs09_Day6_daily$Day2_cells * 0.8

## growth rate mu = ln(Day6 / adjusted Day2) / 4
xmgs09_Day6_daily$Growth_rate <- ifelse(
  !is.na(xmgs09_Day6_daily$Day2_adjusted) &
    xmgs09_Day6_daily$Day2_adjusted > 0 &
    !is.na(xmgs09_Day6_daily$Total_cells) &
    xmgs09_Day6_daily$Total_cells > 0,
  log(xmgs09_Day6_daily$Total_cells / xmgs09_Day6_daily$Day2_adjusted) / 4,
  NA
)

## x positions
xmgs09_Day6_daily$x_plot <- NA_real_
xmgs09_Day6_daily$x_plot[xmgs09_Day6_daily$Treatment == "0P"]       <- 0
xmgs09_Day6_daily$x_plot[xmgs09_Day6_daily$Treatment == "0.005P"]   <- 0.005
xmgs09_Day6_daily$x_plot[xmgs09_Day6_daily$Treatment == "0.01P"]    <- 0.01
xmgs09_Day6_daily$x_plot[xmgs09_Day6_daily$Treatment == "0.025P"]   <- 0.025
xmgs09_Day6_daily$x_plot[xmgs09_Day6_daily$Treatment == "0.05P"]    <- 0.05

## groups
xmgs09_Day6_daily$Group <- "Expt2_0N_Day2_6"

xmgs09_Day6_daily$Stage <- "Day6_vs_Day2_adjusted"
xmgs09_Day6_daily$Initial_mean_cells <- NA_real_

xmgs09_Day6_daily <- xmgs09_Day6_daily[, c(
  "Replication", "Treatment", "Total_cells",
  "Initial_mean_cells", "Growth_rate",
  "x_plot", "Group", "Stage"
)]

################################################################################
## 6) COMBINE XMGS09 TWO STAGES
################################################################################
daily_all_09 <- rbind(
  xmgs09_Day2_daily,
  xmgs09_Day6_daily
)

daily_all_09 <- as.data.frame(daily_all_09, stringsAsFactors = FALSE)

################################################################################
## SAVE: ALL replication-level growth rates
################################################################################
daily_all_09_new <- daily_all_09
################################################################################
## ADD: numeric P concentration (for modeling)
daily_all_09_new$Treatment_clean <- gsub("_2$", "", daily_all_09_new$Treatment)
daily_all_09_new$P_char <- gsub("P", "", daily_all_09_new$Treatment_clean)
daily_all_09_new$P_conc <- as.numeric(daily_all_09_new$P_char)
selected <- c("P_conc","Growth_rate", "Stage")
daily_all_09_new <- daily_all_09_new[, selected]

write.xlsx(daily_all_09_new, "../Output_Data/XMGS09_growth_rate.xlsx",
           rowNames = FALSE)

################################################################################
## XMGS09 ONLY，Day0–2: Monod fit
## Day2–6: linear fit
################################################################################
################################################################################
## 1) COMBINE TWO STAGES
################################################################################
growth09_rep <- rbind(
  xmgs09_Day2_daily,
  xmgs09_Day6_daily
)

growth09_rep <- as.data.frame(growth09_rep, stringsAsFactors = FALSE)

growth09_rep <- growth09_rep[, c(
  "Replication", "Treatment", "Growth_rate", "x_plot", "Stage"
)]

growth09_rep$Replication <- as.character(growth09_rep$Replication)
growth09_rep$Treatment   <- as.character(growth09_rep$Treatment)
growth09_rep$Growth_rate <- as.numeric(growth09_rep$Growth_rate)
growth09_rep$x_plot      <- as.numeric(growth09_rep$x_plot)
growth09_rep$Stage       <- as.character(growth09_rep$Stage)

growth09_rep$Stage2 <- NA_character_
growth09_rep$Stage2[growth09_rep$Stage == "Day2_vs_Day0"]          <- "Day0–2"
growth09_rep$Stage2[growth09_rep$Stage == "Day6_vs_Day2_adjusted"] <- "Day2–6"

growth09_rep <- growth09_rep[
  !is.na(growth09_rep$Growth_rate) &
    !is.na(growth09_rep$x_plot) &
    !is.na(growth09_rep$Stage2),
  ,
  drop = FALSE
]

growth09_rep$Stage2 <- factor(
  growth09_rep$Stage2,
  levels = c("Day0–2", "Day2–6")
)

################################################################################
## 2) SPLIT DATA
################################################################################
df_day02 <- growth09_rep[growth09_rep$Stage2 == "Day0–2", , drop = FALSE]
df_day26 <- growth09_rep[growth09_rep$Stage2 == "Day2–6", , drop = FALSE]

################################################################################
## 3) MONOD FUNCTION FOR DAY0–2
################################################################################
monod_fun <- function(S, mu_max, K){
  mu_max * S / (K + S)
}

fit_monod_bounded <- function(df_stage,
                              mu_lower = 0,
                              mu_upper = 1,
                              K_lower  = 1e-6,
                              K_upper  = 0.10){
  
  df_stage <- df_stage[
    !is.na(df_stage$x_plot) &
      !is.na(df_stage$Growth_rate),
    ,
    drop = FALSE
  ]
  
  if(nrow(df_stage) < 3){
    return(NULL)
  }
  
  start_mu <- max(df_stage$Growth_rate, na.rm = TRUE)
  if(!is.finite(start_mu) || start_mu <= 0){
    start_mu <- 0.1
  }
  start_mu <- min(max(start_mu, mu_lower + 1e-4), mu_upper - 1e-4)
  
  pos_x <- df_stage$x_plot[df_stage$x_plot > 0]
  start_K <- median(pos_x, na.rm = TRUE)
  if(!is.finite(start_K) || start_K <= 0){
    start_K <- 0.01
  }
  start_K <- min(max(start_K, K_lower + 1e-6), K_upper - 1e-6)
  
  fit_try <- try(
    nls(
      Growth_rate ~ mu_max * x_plot / (K + x_plot),
      data = df_stage,
      start = list(mu_max = start_mu, K = start_K),
      algorithm = "port",
      lower = c(mu_max = mu_lower, K = K_lower),
      upper = c(mu_max = mu_upper, K = K_upper),
      control = nls.control(maxiter = 500, warnOnly = TRUE)
    ),
    silent = TRUE
  )
  
  if(inherits(fit_try, "try-error")){
    return(NULL)
  } else {
    return(fit_try)
  }
}

fit_day02 <- fit_monod_bounded(
  df_stage = df_day02,
  mu_lower = 0,
  mu_upper = 1.0,
  K_lower  = 1e-6,
  K_upper  = 0.05
)

################################################################################
## 4) LINEAR FIT FOR DAY2–6
################################################################################
fit_day26 <- NULL
if(nrow(df_day26) >= 2){
  fit_day26 <- lm(Growth_rate ~ x_plot, data = df_day26)
}

################################################################################
## 5) BUILD PREDICTION CURVES
################################################################################
pred_grid <- data.frame(x_plot = seq(0, 0.05, length.out = 300))

pred_day02 <- NULL
pred_day26 <- NULL

if(!is.null(fit_day02)){
  c1 <- coef(fit_day02)
  pred_day02 <- data.frame(
    x_plot = pred_grid$x_plot,
    Growth_rate = monod_fun(pred_grid$x_plot, c1["mu_max"], c1["K"]),
    Stage2 = "Day0–2"
  )
}

if(!is.null(fit_day26)){
  pred_day26 <- data.frame(
    x_plot = pred_grid$x_plot,
    Growth_rate = predict(fit_day26, newdata = pred_grid),
    Stage2 = "Day2–6"
  )
}

pred_all <- rbind(pred_day02, pred_day26)

################################################################################
## 6) FORMULAS
################################################################################
formula_day02 <- "Day\u00A00–2: Monod fit failed"
formula_day26 <- "Day\u00A02–6: linear fit failed"

if(!is.null(fit_day02)){
  c1 <- coef(fit_day02)
  formula_day02 <- paste0("Day\u00A00–2: \u03bc = ",sprintf("%.2f", c1["mu_max"]),
                          " × DIP / (", sprintf("%.3f", c1["K"]), " + DIP)"
  )
}

if(!is.null(fit_day26)){
  c2 <- coef(fit_day26)
  formula_day26 <- paste0("Day\u00A02–6: \u03bc = ",sprintf("%.2f", c2["(Intercept)"]),
                          ifelse(c2["x_plot"] >= 0, " + ", " - "),
                          sprintf("%.2f", abs(c2["x_plot"])), " × DIP"
  )
}

################################################################################
## 7) EXPORT COEFFICIENTS
################################################################################
coef_out <- data.frame(Stage = c("Day0–2", "Day2–6"),
                       Model = c("Monod", "Linear"),
                       Parameter1 = c(
                         if(!is.null(fit_day02)) round(unname(coef(fit_day02)["mu_max"]), 3) else NA,
                         if(!is.null(fit_day26)) round(unname(coef(fit_day26)["(Intercept)"]), 3) else NA
                       ),
                       Parameter1_name = c("mu_max", "Intercept"),
                       Parameter2 = c(
                         if(!is.null(fit_day02)) round(unname(coef(fit_day02)["K"]), 3) else NA,
                         if(!is.null(fit_day26)) round(unname(coef(fit_day26)["x_plot"]), 3) else NA
                       ),
                       Parameter2_name = c("K", "Slope")
)

write.xlsx(coef_out,"../Output_Data/XMGS09_growth_rate_Monod_linear_coefficients.xlsx",
           rowNames = FALSE)

################################################################################
## 8) EXPORT FITTED CURVES + RAW DATA
################################################################################
write.xlsx(growth09_rep, "../Output_Data/XMGS09_growth_rate_replication_for_Monod_linear.xlsx",
           rowNames = FALSE)
write.xlsx(pred_all,"../Output_Data/XMGS09_growth_rate_Monod_linear_curve_data.xlsx",
           rowNames = FALSE)

################################################################################
## 9) Y LIMIT
################################################################################
ymin_fit <- min(c(growth09_rep$Growth_rate, pred_all$Growth_rate), na.rm = TRUE)
ymax_fit <- max(c(growth09_rep$Growth_rate, pred_all$Growth_rate), na.rm = TRUE)

if(!is.finite(ymin_fit)) ymin_fit <- -0.05
if(!is.finite(ymax_fit)) ymax_fit <- 0.20

if(ymin_fit > 0) ymin_fit <- 0
ymax_fit <- ymax_fit * 1.18

################################################################################
## 10) COLORS
################################################################################
#stage_cols <- c("Day0–2" = "#7DB7E8", "Day2–6" = "#4E79A7")
stage_cols <- c("Day0–2" = "#1F78B4", "Day2–6" = "#08306B")

################################################################################
## 11) PLOT
################################################################################
p_growth_mixfit_09 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) + 
  geom_point(data = growth09_rep, aes(x = x_plot, y = Growth_rate, color = Stage2),
             size = 3.2, alpha = 0.9,
             position = position_jitter(width = 0.0006, height = 0)
  ) +
  geom_line(data = pred_all,aes(x = x_plot, y = Growth_rate, color = Stage2),
            linewidth = 1.2) +
  annotate( "text",  x = 0,  y = ymax_fit * 0.97,
            label = formula_day02,  hjust = 0,  vjust = 1,
            size = 4.8,   family = "sans",  color = stage_cols["Day0–2"]) +
  annotate( "text",  x = 0,  y = ymax_fit * 0.88,
            label = formula_day26,  hjust = 0,  vjust = 0.5,
            size = 4.8,   family = "sans",  color = stage_cols["Day2–6"]) +
  scale_color_manual(values = stage_cols, breaks = c("Day0–2", "Day2–6")) +
  scale_x_continuous(
    breaks = c(0, 0.005, 0.01, 0.025, 0.05),
    labels = c("0", "0.005", "0.01", "0.025", "0.05"),
    expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 0.05), ylim = c(ymin_fit, ymax_fit*1.05)) +
  labs(
    x = expression("DIP addition (" * mg~L^{-1} * ")"),
    y = expression("Growth rate (" * d^-1 * ")"),
    color = NULL) +
  theme_classic(base_size = 18, base_family = "sans") +
  theme(
    legend.position ="none",
    axis.text.x = element_text(color = "black", angle = 0, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(5, 5, 1, 1)
  )
################################################################################
## 12) SAVE FIGURE
################################################################################
ggsave("../Output_Figures/Growth_rate_population_day02_day26.jpg",
       plot = p_growth_mixfit_09, width = 7, height = 4.5, dpi = 300)

################################################################################
## ADD FILAMENTOUS CYANOBACTERIA AND GREEN ALGAE TO THE SAME GROWTH-RATE FIGURE
## Keep the original total-algae calculation, and add filamentous cyanobacteria
## and green algae using the same Day0-2 Monod and Day2-6 linear fitting strategy.
## using the same Day0-2 Monod and Day2-6 linear fitting strategy.
################################################################################

calc_growth_by_cell_type <- function(cell_type_name, cells_fun) {
  rep_daily <- ddply(
    df_daily,
    .variables = c("Day_label", "Treatment", "Replication"),
    .fun = function(x) {
      data.frame(Total_cells = cells_fun(x))
    }
  )
  
  init_mean <- mean(
    rep_daily$Total_cells[
      rep_daily$Day_label == "Day0" &
        rep_daily$Treatment == "Initial"
    ],
    na.rm = TRUE
  )
  
  day2_dat <- rep_daily[
    rep_daily$Day_label == "Day2" &
      rep_daily$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"),
    ,
    drop = FALSE
  ]
  
  day2_dat$Initial_mean_cells <- init_mean
  day2_dat$Growth_rate <- ifelse(
    !is.na(day2_dat$Initial_mean_cells) &
      day2_dat$Initial_mean_cells > 0 &
      !is.na(day2_dat$Total_cells) &
      day2_dat$Total_cells > 0,
    log(day2_dat$Total_cells / day2_dat$Initial_mean_cells) / 2,
    NA
  )
  
  day2_dat$x_plot <- NA_real_
  day2_dat$x_plot[day2_dat$Treatment == "0P"]     <- 0
  day2_dat$x_plot[day2_dat$Treatment == "0.005P"] <- 0.005
  day2_dat$x_plot[day2_dat$Treatment == "0.01P"]  <- 0.01
  day2_dat$x_plot[day2_dat$Treatment == "0.025P"] <- 0.025
  day2_dat$x_plot[day2_dat$Treatment == "0.05P"]  <- 0.05
  
  day2_dat$Stage2 <- "Day0-2"
  day2_dat$Cell_type <- cell_type_name
  
  day2_source <- rep_daily[
    rep_daily$Day_label == "Day2" &
      rep_daily$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"),
    c("Treatment", "Replication", "Total_cells"),
    drop = FALSE
  ]
  names(day2_source)[names(day2_source) == "Treatment"] <- "Day2_Treatment"
  names(day2_source)[names(day2_source) == "Total_cells"] <- "Day2_cells"
  
  day6_dat <- rep_daily[
    rep_daily$Day_label == "Day6" &
      rep_daily$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"),
    ,
    drop = FALSE
  ]
  day6_dat$Day2_match <- as.character(day6_dat$Treatment)
  
  day6_dat <- merge(
    day6_dat,
    day2_source,
    by.x = c("Day2_match", "Replication"),
    by.y = c("Day2_Treatment", "Replication"),
    all.x = TRUE
  )
  
  day6_dat$Day2_adjusted <- day6_dat$Day2_cells * 0.8
  day6_dat$Growth_rate <- ifelse(
    !is.na(day6_dat$Day2_adjusted) &
      day6_dat$Day2_adjusted > 0 &
      !is.na(day6_dat$Total_cells) &
      day6_dat$Total_cells > 0,
    log(day6_dat$Total_cells / day6_dat$Day2_adjusted) / 4,
    NA
  )
  
  day6_dat$x_plot <- NA_real_
  day6_dat$x_plot[day6_dat$Treatment == "0P"]     <- 0
  day6_dat$x_plot[day6_dat$Treatment == "0.005P"] <- 0.005
  day6_dat$x_plot[day6_dat$Treatment == "0.01P"]  <- 0.01
  day6_dat$x_plot[day6_dat$Treatment == "0.025P"] <- 0.025
  day6_dat$x_plot[day6_dat$Treatment == "0.05P"]  <- 0.05
  
  day6_dat$Stage2 <- "Day2-6"
  day6_dat$Cell_type <- cell_type_name
  
  out <- rbind(
    day2_dat[, c("Replication", "Treatment", "Total_cells", "Growth_rate", "x_plot", "Stage2", "Cell_type")],
    day6_dat[, c("Replication", "Treatment", "Total_cells", "Growth_rate", "x_plot", "Stage2", "Cell_type")]
  )
  
  out <- out[
    !is.na(out$Growth_rate) &
      !is.na(out$x_plot) &
      !is.na(out$Stage2),
    ,
    drop = FALSE
  ]
  
  out$Stage2 <- factor(out$Stage2, levels = c("Day0-2", "Day2-6"))
  out$Cell_type <- factor(out$Cell_type, levels = c("Total algae", "Filamentous cyanobacteria", "Green algae"))
  out
}

growth_total_fil <- rbind(
  calc_growth_by_cell_type(
    "Total algae",
    function(x) sum(x$Cells_mL, na.rm = TRUE)
  ),
  calc_growth_by_cell_type(
    "Filamentous cyanobacteria",
    function(x) {
      idx <- x$Latin_phylum == "Cyanophyta" &
        grepl("Filamentous", x$Morphology, ignore.case = TRUE)
      sum(x$Cells_mL[idx], na.rm = TRUE)
    }
  ),
  calc_growth_by_cell_type(
    "Green algae",
    function(x) {
      idx <- x$Latin_phylum == "Chlorophyta"
      sum(x$Cells_mL[idx], na.rm = TRUE)
    }
  )
)

pred_grid2 <- data.frame(x_plot = seq(0, 0.05, length.out = 300))
pred_list2 <- list()
coef_list2 <- list()
idx_pred <- 1
idx_coef <- 1

for (ct in levels(growth_total_fil$Cell_type)) {
  for (st in levels(growth_total_fil$Stage2)) {
    df_fit <- growth_total_fil[
      growth_total_fil$Cell_type == ct &
        growth_total_fil$Stage2 == st,
      ,
      drop = FALSE
    ]
    
    fit_obj <- NULL
    model_name <- NA_character_
    parameter1 <- NA_real_
    parameter2 <- NA_real_
    parameter1_name <- NA_character_
    parameter2_name <- NA_character_
    
    if (st == "Day0-2") {
      fit_obj <- fit_monod_bounded(
        df_stage = df_fit,
        mu_lower = 0,
        mu_upper = 1.5,
        K_lower = 1e-6,
        K_upper = 0.05
      )
      model_name <- "Monod"
      if (!is.null(fit_obj)) {
        cc <- coef(fit_obj)
        pred_list2[[idx_pred]] <- data.frame(
          x_plot = pred_grid2$x_plot,
          Growth_rate = monod_fun(pred_grid2$x_plot, cc["mu_max"], cc["K"]),
          Stage2 = st,
          Cell_type = ct
        )
        idx_pred <- idx_pred + 1
        parameter1 <- unname(cc["mu_max"])
        parameter2 <- unname(cc["K"])
        parameter1_name <- "mu_max"
        parameter2_name <- "K"
      }
    } else {
      if (nrow(df_fit) >= 2) fit_obj <- lm(Growth_rate ~ x_plot, data = df_fit)
      model_name <- "Linear"
      if (!is.null(fit_obj)) {
        pred_list2[[idx_pred]] <- data.frame(
          x_plot = pred_grid2$x_plot,
          Growth_rate = predict(fit_obj, newdata = pred_grid2),
          Stage2 = st,
          Cell_type = ct
        )
        idx_pred <- idx_pred + 1
        cc <- coef(fit_obj)
        parameter1 <- unname(cc["(Intercept)"])
        parameter2 <- unname(cc["x_plot"])
        parameter1_name <- "Intercept"
        parameter2_name <- "Slope"
      }
    }
    
    coef_list2[[idx_coef]] <- data.frame(
      Cell_type = ct,
      Stage = st,
      Model = model_name,
      Parameter1 = round(parameter1, 4),
      Parameter1_name = parameter1_name,
      Parameter2 = round(parameter2, 4),
      Parameter2_name = parameter2_name
    )
    idx_coef <- idx_coef + 1
  }
}

pred_total_fil <- do.call(rbind, pred_list2)
coef_total_fil <- do.call(rbind, coef_list2)

growth_total_fil$Stage2 <- factor(growth_total_fil$Stage2, levels = c("Day0-2", "Day2-6"))
pred_total_fil$Stage2 <- factor(pred_total_fil$Stage2, levels = c("Day0-2", "Day2-6"))
growth_total_fil$Cell_type <- factor(growth_total_fil$Cell_type,
                                     levels = c("Total algae", "Filamentous cyanobacteria", "Green algae"))
pred_total_fil$Cell_type <- factor(pred_total_fil$Cell_type,
                                   levels = c("Total algae", "Filamentous cyanobacteria", "Green algae"))

cell_cols2 <- c("Total algae" = "#08306B",
                "Filamentous cyanobacteria" = "#6BAED6",
                "Green algae" = "#2CA25F")
stage_ltys2 <- c("Day0-2" = "solid", "Day2-6" = "dashed")
cell_shapes2 <- c("Total algae" = 16,
                  "Filamentous cyanobacteria" = 17,
                  "Green algae" = 15)

ymin_fit2 <- min(c(growth_total_fil$Growth_rate, pred_total_fil$Growth_rate), na.rm = TRUE)
ymax_fit2 <- max(c(growth_total_fil$Growth_rate, pred_total_fil$Growth_rate), na.rm = TRUE)
if (!is.finite(ymin_fit2)) ymin_fit2 <- -0.05
if (!is.finite(ymax_fit2)) ymax_fit2 <- 0.20
if (ymin_fit2 > 0) ymin_fit2 <- 0
ymax_fit2 <- ymax_fit2 * 1.15

formula_total_fil <- coef_total_fil


formula_total_fil$Stage_label <- ifelse(
  formula_total_fil$Stage == "Day0-2",
  "Days 0-2",
  "Days 2–6"
)

formula_total_fil$Cell_label <- ifelse(
  formula_total_fil$Cell_type == "Filamentous cyanobacteria",
  "Fil. cyano",
  ifelse(
    formula_total_fil$Cell_type == "Green algae",
    "Chlorophyte",
    "Total phyto."
  )
)
formula_total_fil$Formula <- ifelse(
  formula_total_fil$Model == "Monod" & is.finite(formula_total_fil$Parameter1),
  paste0(
    formula_total_fil$Cell_label, " ", formula_total_fil$Stage_label,
    ": µ=", sprintf("%.2f", formula_total_fil$Parameter1),
    " × DIP/(", sprintf("%.3f", formula_total_fil$Parameter2), " + DIP)"
  ),
  paste0(
    formula_total_fil$Cell_label, " ", formula_total_fil$Stage_label,
    ": µ=", sprintf("%.2f", formula_total_fil$Parameter1),
    ifelse(formula_total_fil$Parameter2 >= 0, " + ", " − "),
    sprintf("%.2f", abs(formula_total_fil$Parameter2)), " × DIP"
  )
)

formula_total_fil <- formula_total_fil[
  order(
    factor(formula_total_fil$Cell_type,
           levels = c("Total algae", "Filamentous cyanobacteria", "Green algae")),
    factor(formula_total_fil$Stage, levels = c("Day0-2", "Day2-6"))
  ),
  ,
  drop = FALSE
]
formula_total_fil$x_line_start <- 0.052
formula_total_fil$x_line_end <- 0.055
formula_total_fil$x_label <- 0.0565
formula_total_fil$y_label <- c(1.13, 0.96, 0.79, 0.62, 0.45, 0.28)

p_growth_mixfit_09_total_fil_main <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_point(data = growth_total_fil,
             aes(x = x_plot, y = Growth_rate, color = Cell_type, shape = Cell_type),
             size = 2.4, alpha = 0.86,
             position = position_jitter(width = 0.00055, height = 0)
  ) +
  geom_line(data = pred_total_fil,
            aes(x = x_plot, y = Growth_rate, color = Cell_type, linetype = Stage2,
                group = interaction(Stage2, Cell_type)),linewidth = 0.9) +
  scale_color_manual(values = cell_cols2,
                     breaks = c("Total algae", "Filamentous cyanobacteria", "Green algae")) +
  scale_linetype_manual(
    values = stage_ltys2, breaks = c("Day0-2", "Day2-6"),
    guide = "none") +
  scale_shape_manual(
    values = cell_shapes2,breaks = c("Total algae", "Filamentous cyanobacteria", "Green algae"),
    guide = "none") +
  scale_x_continuous(
    breaks = c(0, 0.005, 0.01, 0.025, 0.05),
    labels = c("0", "0.005", "0.01", "0.025", "0.05"),expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 0.05), ylim = c(-0.4, 1.2), clip = "on") +
  labs(
    x = expression("DIP addition (" * mg~L^{-1} * ")"),
    y = expression("Net growth rate (" * d^-1 * ")"),
    color = NULL) +
  guides(
    color = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  theme_classic(base_size = 15, base_family = "sans") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(color = "black", size = 15,  angle = 38, vjust = 1,hjust = 1),
    axis.text.y = element_text(color = "black", size = 15),
    axis.title = element_text(color = "black", size = 15),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(5, 4, 1, 1)
  )

p_formula_panel <- ggplot(formula_total_fil) +
  geom_segment(
    aes(x = 0.02, xend = 0.13, y = y_label, yend = y_label,
        color = Cell_type, linetype = Stage),
    linewidth = 0.9, show.legend = FALSE
  ) +
  geom_point(
    aes(x = 0.02, y = y_label, color = Cell_type, shape = Cell_type),
    size = 2.8, stroke = 0.8, show.legend = FALSE
  ) +
  geom_text(
    aes(x = 0.18, y = y_label, label = Formula, color = Cell_type),
    hjust = 0, vjust = 0.5, size = 5, family = "sans", show.legend = FALSE
  ) +
  scale_color_manual(values = cell_cols2) +
  scale_linetype_manual(values = stage_ltys2) +
  scale_shape_manual(values = cell_shapes2) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.4, 1.2), clip = "on") +
  theme_void(base_family = "sans") +
  theme(plot.margin = margin(5, 0, 1, 5))

p_growth_mixfit_09_total_fil <- p_growth_mixfit_09_total_fil_main +
  p_formula_panel +
  plot_layout(widths = c(0.78, 0.8))

ggsave("../Output_Figures/Growth_rate_population_day02_day26_total_filamentous_green.jpg",
       plot = p_growth_mixfit_09_total_fil, width = 12.4, height = 4.6, dpi = 300)
