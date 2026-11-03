################################################################################
## Figure 1. Nutrient concentration trajectories
## Initial -> Day2_pre -> Day2_post -> Day6
################################################################################

library(openxlsx)
library(plyr)
library(tidyr)
library(ggplot2)
library(grid)

################################################################################
## 0) Path
################################################################################
project_dir <- Sys.getenv(
  "XMGS09_PROJECT_DIR",
  unset = "~/My Drive/00_Project/XMGS09_DJKinsitu2"
)
dirName <- file.path(path.expand(project_dir), "Final_Codes")
setwd(dirName)

################################################################################
## 1) Read data
################################################################################
dat <- read.xlsx("../Input_Data/insitu_water_quality.xlsx", sheet = 1)
dat <- as.data.frame(dat, stringsAsFactors = FALSE)

names(dat) <- trimws(names(dat))
dat$Treatment   <- trimws(as.character(dat$Treatment))
dat$Replication <- trimws(as.character(dat$Replication))
dat$Day         <- as.numeric(as.character(dat$Day))
dat$Batch       <- trimws(as.character(dat$Batch))

## Keep the internal variable names used below, while accepting the more common
## column names used in the raw data table.
if (!"DTN_mg_L" %in% names(dat) && "TDN_mg_L" %in% names(dat)) {
  dat$DTN_mg_L <- dat$TDN_mg_L
}
if (!"DTP_mg_L" %in% names(dat) && "TDP_mg_L" %in% names(dat)) {
  dat$DTP_mg_L <- dat$TDP_mg_L
}

################################################################################
## 2) Clean nutrient columns
################################################################################
nutrient_vars <- c("TN_mg_L", "DTN_mg_L", "NOx-N_mg_L", "NH4-N_mg_L",
  "TP_mg_L", "DTP_mg_L", "PO4-P_mg_L")

to_num <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", "", x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "ND", "n.d.", "-", "—")] <- NA
  x <- gsub("^<", "", x)
  x <- gsub(",", "", x)
  as.numeric(x)
}

for (v in nutrient_vars) {
  dat[[v]] <- to_num(dat[[v]])
}

################################################################################
## 3) Convert to long format
################################################################################
dat_long <- pivot_longer(
  dat[, c("Day", "Batch", "Treatment", "Replication", nutrient_vars)],
  cols = all_of(nutrient_vars), names_to = "Nutrient", values_to = "Value")

dat_long <- as.data.frame(dat_long, stringsAsFactors = FALSE)

## Main P-gradient treatments used throughout the manuscript.
treatments_final <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")

################################################################################
## 5) Summary function
################################################################################
sum_fun <- function(x) {
  data.frame(
    Mean = mean(x$Value, na.rm = TRUE),
    SD   = sd(x$Value, na.rm = TRUE),
    n    = sum(!is.na(x$Value))
  )
}

################################################################################
## 6) Initial summary
################################################################################
initial_sum <- ddply(dat_long[dat_long$Day == 0 & dat_long$Treatment == "Initial", ],
  .(Nutrient),  sum_fun)
initial_sum$Treatment <- "Initial"
initial_sum$Timepoint <- "Initial"

################################################################################
## 7) Filtered-water measurements used for the Day2_post calculation
################################################################################
initial_diss_rep <- dat_long[
  dat_long$Day == 0 & dat_long$Treatment == "Initial_diss",
  c("Replication", "Nutrient", "Value")
]
names(initial_diss_rep)[names(initial_diss_rep) == "Value"] <- "Filtered_value"

################################################################################
## 8) Day2_pre summary
################################################################################
## Batch 1 is the measured sample collected immediately before water replacement.
day2_pre_rep <- dat_long[
  dat_long$Day == 2 & dat_long$Batch == "1" &
    dat_long$Treatment %in% treatments_final,
  c("Treatment", "Replication", "Nutrient", "Value")
]

day2_pre <- ddply(day2_pre_rep, .(Treatment, Nutrient), sum_fun)
day2_pre$Timepoint <- "Day2_pre"

################################################################################
## 9) Day2_post summary
################################################################################
## Day2_post was not measured independently. Recalculate it at the replicate level
## from the 80:20 mixture, pairing each bag with the corresponding replicate of
## filtered reservoir water. Summarising the replicate-level calculated values
## gives the correct SD and avoids pooling Batch 1 and Batch 2.
day2_post_calc_rep <- merge(
  day2_pre_rep,
  initial_diss_rep,
  by = c("Replication", "Nutrient"),
  all.x = TRUE,
  sort = FALSE
)
day2_post_calc_rep$Value <-
  0.8 * day2_post_calc_rep$Value + 0.2 * day2_post_calc_rep$Filtered_value

## Validate the recalculation against the stored Batch 2 values in the input file.
day2_post_recorded_rep <- dat_long[
  dat_long$Day == 2 & dat_long$Batch == "2" &
    dat_long$Treatment %in% treatments_final,
  c("Treatment", "Replication", "Nutrient", "Value")
]
names(day2_post_recorded_rep)[names(day2_post_recorded_rep) == "Value"] <-
  "Recorded_value"

day2_post_check <- merge(
  day2_post_calc_rep[, c("Treatment", "Replication", "Nutrient", "Value")],
  day2_post_recorded_rep,
  by = c("Treatment", "Replication", "Nutrient"),
  all = TRUE,
  sort = FALSE
)
day2_post_check$Absolute_difference <-
  abs(day2_post_check$Value - day2_post_check$Recorded_value)

max_post_difference <- max(day2_post_check$Absolute_difference, na.rm = TRUE)
if (!is.finite(max_post_difference) || max_post_difference > 1e-10) {
  stop("Calculated Day2_post values do not match the stored Batch 2 values.")
}

day2_post <- ddply(day2_post_calc_rep, .(Treatment, Nutrient), sum_fun)
day2_post$Timepoint <- "Day2_post"

################################################################################
## 10) Day6 summary
################################################################################
day6_sum <- ddply(
  dat_long[dat_long$Day == 6 & dat_long$Batch == "2", ],
  .(Treatment, Nutrient),
  sum_fun
)
day6_sum$Timepoint <- "Day6"

################################################################################
## 11) Initial as a single group
################################################################################
initial_expand <- initial_sum[, c("Treatment", "Nutrient", "Mean", "SD", "n", "Timepoint")]

################################################################################
## 12) Keep only target treatments for all timepoints
################################################################################
day2_pre  <- day2_pre[day2_pre$Treatment %in% treatments_final, ]
day2_post <- day2_post[day2_post$Treatment %in% treatments_final, ]
day6_sum  <- day6_sum[day6_sum$Treatment %in% treatments_final, ]

################################################################################
## 13) Combine all summaries
################################################################################
plot_df <- rbind(
  initial_expand[, c("Treatment", "Nutrient", "Mean", "SD", "n", "Timepoint")],
  day2_pre[,       c("Treatment", "Nutrient", "Mean", "SD", "n", "Timepoint")],
  day2_post[,      c("Treatment", "Nutrient", "Mean", "SD", "n", "Timepoint")],
  day6_sum[,       c("Treatment", "Nutrient", "Mean", "SD", "n", "Timepoint")]
)

plot_df <- plot_df[!is.na(plot_df$Mean), ]

################################################################################
## 14) Factor order
################################################################################
plot_df$Timepoint <- factor(plot_df$Timepoint, levels = c("Initial", "Day2_pre", "Day2_post", "Day6"))

plot_df$Treatment <- factor(plot_df$Treatment, levels = c("Initial", treatments_final))

plot_df$Nutrient <- factor(plot_df$Nutrient,
  levels = nutrient_vars, labels = c("TN", "TDN", "NOx-N", "NH4-N", "TP", "TDP", "PO4-P")
)

plot_df$Data_type <- "Measured"
plot_df$Data_type[plot_df$Timepoint == "Day2_post"] <- "Calculated after dilution"
plot_df$Data_type[plot_df$Treatment == "Initial"] <- "Measured"

################################################################################
## 15) Export plotting data
################################################################################
write.csv(plot_df,"../Output_Data/Figure1_nutrient_trajectory_plotdata.csv",row.names = FALSE)

wb <- createWorkbook()
addWorksheet(wb, "Figure1_plot_data")
writeData(wb, "Figure1_plot_data", plot_df)
saveWorkbook(wb,  "../Output_Data/Figure1_nutrient_trajectory_plotdata.xlsx", overwrite = TRUE)

################################################################################
## 15b) Day2_pre versus calculated Day2_post comparison
##
## These tests quantify the expected change caused by the 80:20 mixing step.
## Because Day2_post is calculated rather than independently measured, the
## resulting P values must be interpreted as arithmetic dilution comparisons,
## not as independent experimental evidence of a replacement effect.
################################################################################
day2_comparison <- merge(
  day2_pre_rep,
  day2_post_calc_rep[, c("Treatment", "Replication", "Nutrient", "Value")],
  by = c("Treatment", "Replication", "Nutrient"),
  suffixes = c("_pre", "_post"),
  sort = FALSE
)
day2_comparison$Difference <-
  day2_comparison$Value_post - day2_comparison$Value_pre
day2_comparison$Percent_change <-
  100 * day2_comparison$Difference / day2_comparison$Value_pre
day2_comparison$Treatment <- factor(
  day2_comparison$Treatment,
  levels = treatments_final
)

run_pre_post_comparison <- function(x) {
  ## In a balanced design, the intercept under sum contrasts tests the grand
  ## mean paired change, while Treatment tests the replacement-by-treatment
  ## interaction (i.e., whether the calculated change varies among treatments).
  fit <- lm(
    Difference ~ Treatment,
    data = x,
    contrasts = list(Treatment = contr.sum)
  )
  fit_coef <- summary(fit)$coefficients
  fit_anova <- anova(fit)

  data.frame(
    n = nrow(x),
    Day2_pre_mean = mean(x$Value_pre, na.rm = TRUE),
    Day2_post_mean = mean(x$Value_post, na.rm = TRUE),
    Mean_change = mean(x$Difference, na.rm = TRUE),
    Percent_change = 100 * mean(x$Difference, na.rm = TRUE) /
      mean(x$Value_pre, na.rm = TRUE),
    Replacement_F = unname(fit_coef["(Intercept)", "t value"]^2),
    Replacement_df1 = 1,
    Replacement_df2 = df.residual(fit),
    Replacement_P = unname(fit_coef["(Intercept)", "Pr(>|t|)"]),
    Treatment_interaction_F = fit_anova["Treatment", "F value"],
    Treatment_interaction_df1 = fit_anova["Treatment", "Df"],
    Treatment_interaction_df2 = fit_anova["Residuals", "Df"],
    Treatment_interaction_P = fit_anova["Treatment", "Pr(>F)"],
    Residual_Shapiro_P = shapiro.test(residuals(fit))$p.value
  )
}

day2_test_results <- ddply(
  day2_comparison,
  .(Nutrient),
  run_pre_post_comparison
)
day2_test_results$Replacement_P_BH <- p.adjust(
  day2_test_results$Replacement_P,
  method = "BH"
)
day2_test_results$Treatment_interaction_P_BH <- p.adjust(
  day2_test_results$Treatment_interaction_P,
  method = "BH"
)
day2_test_results$Interpretation <-
  "Calculated 80:20 dilution comparison; Day2_post was not independently measured"

day2_by_treatment <- ddply(
  day2_comparison,
  .(Treatment, Nutrient),
  function(x) {
    data.frame(
      n = nrow(x),
      Day2_pre_mean = mean(x$Value_pre, na.rm = TRUE),
      Day2_pre_SD = sd(x$Value_pre, na.rm = TRUE),
      Day2_post_mean = mean(x$Value_post, na.rm = TRUE),
      Day2_post_SD = sd(x$Value_post, na.rm = TRUE),
      Mean_change = mean(x$Difference, na.rm = TRUE),
      Percent_change = 100 * mean(x$Difference, na.rm = TRUE) /
        mean(x$Value_pre, na.rm = TRUE)
    )
  }
)

write.xlsx(
  list(
    Overall_tests = day2_test_results,
    By_treatment = day2_by_treatment,
    Paired_values = day2_comparison,
    Calculation_check = day2_post_check
  ),
  "../Output_Data/Day2_pre_vs_post_nutrient_comparison.xlsx",
  overwrite = TRUE
)

################################################################################
## 16) Make x-offset
################################################################################
plot_df$x_base <- c("Initial" = 1,"Day2_pre"= 2,"Day2_post"= 3,"Day6"= 4)[as.character(plot_df$Timepoint)]
write.xlsx(plot_df, "../Output_Data/Insitu_water_quality_mean.xlsx", rowNames = FALSE)

################################################################################
## 17) Nutrient trajectory figure
################################################################################
plot_df_new <- plot_df

## 2) reset factor levels
plot_df_new$Treatment <- factor(plot_df_new$Treatment,
  levels = c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P"))

## 3) x-offset
offset_map_new <- c(
  "Initial" =  0.00,
  "0P"      = -0.18,
  "0.005P"  = -0.09,
  "0.01P"   =  0.00,
  "0.025P"  =  0.09,
  "0.05P"   =  0.18
)

plot_df_new$x_plot <- plot_df_new$x_base + offset_map_new[as.character(plot_df_new$Treatment)]

## 4) colors
treat_cols_new <- c(
  "Initial" = "black",
  "0P"      = "#7F7F7F",
  "0.005P"  = "#56B4E9",
  "0.01P"   = "#009E73",
  "0.025P"  = "#E69F00",
  "0.05P"   = "#D55E00"
)

## 5) rebuild label positions
label_df_new <- ddply(plot_df_new, .(Nutrient), function(x) {
  ymax1 <- max(x$Mean + x$SD, na.rm = TRUE)
  ymin1 <- min(x$Mean - x$SD, na.rm = TRUE)
  if (!is.finite(ymax1)) ymax1 <- max(x$Mean, na.rm = TRUE)
  if (!is.finite(ymin1)) ymin1 <- min(x$Mean, na.rm = TRUE)
  
  data.frame(x_plot = 1.2, y_lab  = ymin1 + 1.2 * (ymax1 - ymin1))
})
label_df_new$label <- as.character(label_df_new$Nutrient)

################################################################################
## 6) plot new figure
################################################################################
p1_new <- ggplot() +
  geom_line(data = plot_df_new, aes(x = x_plot, y = Mean, group = Treatment,
                                color = Treatment, linetype = Treatment), linewidth = 0.9) +
  scale_linetype_manual(values = c(
    "Initial"= 1,  
    "0P"  = 1, 
    "0.005P"= 2,  
    "0.01P" = 2,
    "0.025P"= 2,  
    "0.05P" = 1)) +
  geom_point(data = plot_df_new[plot_df_new$Treatment == "Initial", ], aes(x = x_plot, y = Mean),
             shape = 16, color = "black",  size = 3, show.legend = FALSE) +
  geom_point(data = plot_df_new[plot_df_new$Treatment != "Initial", ], aes(x = x_plot, y = Mean, 
                                                                           color = Treatment, shape = Data_type),
             size = 3, stroke = 1, show.legend = TRUE) +
  geom_errorbar(data = plot_df_new[plot_df_new$Treatment == "Initial", ],
                aes(x = x_plot, ymin = Mean - SD, ymax = Mean + SD),
                color = "black",width = 0.05, linewidth = 0.5, show.legend = FALSE) +
  geom_errorbar(data = plot_df_new[plot_df_new$Treatment != "Initial", ],
                aes(x = x_plot, ymin = Mean - SD, ymax = Mean + SD, color = Treatment),
                width = 0.05,  linewidth = 0.5) +
  geom_text(data = label_df_new,  aes(x = x_plot, y = y_lab, label = label),
            inherit.aes = FALSE, hjust = 1, vjust = 1, size = 5) +
  facet_wrap(~ Nutrient, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("Initial", "Day2_pre", "Day2_post", "Day6"), limits = c(0.75, 4.25) ) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 3)) +
  scale_color_manual(values = treat_cols_new) +
  scale_shape_manual(values = c("Initial" = 16, "Measured" = 16, "Calculated after dilution" = 1),
                     breaks = "Calculated after dilution", labels = "After dilution", name = NULL) +
  guides(linetype = "none",
         color = guide_legend(nrow = 2, byrow = TRUE, order = 2),
         shape = guide_legend(nrow = 1, order = 1)
  ) +
  labs( x = NULL, 
        y = expression("Concentration (mg L"^-1 * ")"),
        color = NULL, shape = NULL) +
  theme_bw(base_size = 20) +
  theme(
    plot.margin = margin(2, 1, 1, 1),
    text = element_text(family = "sans"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = c(0.73, 0.05),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box = "vertical",
    legend.box.spacing = unit(0.1, "lines"),
    legend.spacing = unit(0.1, "lines"),
    legend.spacing.y = unit(0.5, "lines"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(0.8, "lines")
  )

################################################################################
## 7) save new figure
################################################################################
ggsave("../Output_Figures/Nutrient_trajectory.jpg", p1_new,  width = 10, height = 7, dpi = 300)

################################################################################
## 19) Nutrients versus cumulative P addition
##     - Main P-gradient treatments only
##     - Arrange as: TN/DTN, NOx-N/TP, DTP/PO4-P, TN:TP/legend
##     - Put nutrient labels inside panels to avoid external strip space
################################################################################

## This block reuses the cleaned raw data object `dat` from sections 1-2.
## It is intentionally independent from `plot_df`, because this figure uses
## P addition on the x-axis rather than timepoint on the x-axis.

nutrient_vars_p <- c("TN_mg_L", "DTN_mg_L", "NOx-N_mg_L",
                     "TP_mg_L", "DTP_mg_L", "PO4-P_mg_L", "TN_TP")

## Clean TN:TP as numeric if needed.
dat$TN_TP <- to_num(dat$TN_TP)

## Add cumulative P addition and timepoint labels from the raw table.
dat_p <- dat

dat_p$P_add_mg_L <- NA_real_
dat_p$P_add_mg_L[dat_p$Treatment == "0P"] <- 0
dat_p$P_add_mg_L[dat_p$Treatment == "0.005P"] <- 0.005
dat_p$P_add_mg_L[dat_p$Treatment == "0.01P"] <- 0.01
dat_p$P_add_mg_L[dat_p$Treatment == "0.025P"] <- 0.025
dat_p$P_add_mg_L[dat_p$Treatment == "0.05P"] <- 0.05

dat_p$Timepoint <- NA_character_
dat_p$Timepoint[dat_p$Treatment == "Initial"] <- "Initial"
dat_p$Timepoint[dat_p$Treatment == "Initial_diss"] <- "Initial_diss"
dat_p$Timepoint[dat_p$Day == 2 & as.character(dat_p$Batch) %in% c("1", "pre", "Day2_pre")] <- "Day2_pre"
dat_p$Timepoint[dat_p$Day == 2 & as.character(dat_p$Batch) %in% c("2", "post", "Day2_post")] <- "Day2_post"
dat_p$Timepoint[dat_p$Day == 6] <- "Day6"

## Main gradient only.
dat_p <- dat_p[dat_p$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P") &
                 dat_p$Timepoint %in% c("Day2_pre", "Day2_post", "Day6"), ]

dat_p$Treatment <- factor(dat_p$Treatment,
                          levels = c("0P", "0.005P", "0.01P", "0.025P", "0.05P"))
dat_p$Timepoint <- factor(dat_p$Timepoint,
                          levels = c("Day2_pre", "Day2_post", "Day6"))

nutr_p_long <- pivot_longer(
  dat_p[, c("Treatment", "P_add_mg_L", "Replication", "Timepoint", nutrient_vars_p)],
  cols = all_of(nutrient_vars_p),
  names_to = "Nutrient", values_to = "Value"
)
nutr_p_long <- as.data.frame(nutr_p_long, stringsAsFactors = FALSE)

initial_mean_long <- pivot_longer(
  dat[dat$Treatment == "Initial", c("Treatment", "Replication", nutrient_vars_p)],
  cols = all_of(nutrient_vars_p),
  names_to = "Nutrient", values_to = "Value"
)
initial_mean_long <- as.data.frame(initial_mean_long, stringsAsFactors = FALSE)
initial_mean_df <- ddply(initial_mean_long, .(Nutrient), function(x) {
  data.frame(Initial_mean = mean(x$Value, na.rm = TRUE))
})

nutrient_label_map <- c(
  "TN_mg_L" = "TN",
  "DTN_mg_L" = "TDN",
  "NOx-N_mg_L" = "NOx-N",
  "TP_mg_L" = "TP",
  "DTP_mg_L" = "TDP",
  "PO4-P_mg_L" = "PO4-P",
  "TN_TP" = "TN:TP"
)

mean_se_value <- function(x) {
  x <- x[is.finite(x)]
  data.frame(y = mean(x),
             ymin = mean(x) - sd(x) / sqrt(length(x)),
             ymax = mean(x) + sd(x) / sqrt(length(x)))
}

timepoint_cols <- c(
  "Day2_pre" = "#F8766D",
  "Day2_post" = "#00BA38",
  "Day6" = "#619CFF"
)

nice_four_breaks <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NULL)
  xmin <- min(x)
  xmax <- max(x)
  if (xmin == xmax) {
    pad <- ifelse(xmin == 0, 1, abs(xmin) * 0.1)
    xmin <- xmin - pad
    xmax <- xmax + pad
  }

  raw_step <- (xmax - xmin) / 3
  base <- 10 ^ floor(log10(raw_step))
  steps <- as.vector(outer(c(1, 2, 2.5, 5, 10), base * 10 ^ (-2:2), "*"))
  steps <- sort(unique(steps[steps > 0]))

  candidates <- lapply(steps, function(step) {
    lower <- floor(xmin / step) * step
    upper <- lower + 3 * step
    while (upper < xmax) {
      lower <- lower + step
      upper <- lower + 3 * step
    }
    brks <- lower + step * 0:3
    data.frame(step = step, lower = lower, upper = upper,
               span = upper - lower, covers_zero = lower <= 0 & upper >= 0,
               breaks = I(list(brks)))
  })
  candidates <- do.call(rbind, candidates)
  candidates <- candidates[candidates$lower <= xmin & candidates$upper >= xmax, ]
  if (nrow(candidates) == 0) return(pretty(x, n = 4))

  best <- candidates[order(candidates$span, candidates$step), ][1, ]
  as.numeric(best$breaks[[1]])
}

nice_axis_label <- function(x) {
  ifelse(abs(x) >= 1,
         formatC(x, format = "fg", digits = 3),
         formatC(x, format = "fg", digits = 2))
}

nutr_p_long$Nutrient_label <- factor(
  unname(nutrient_label_map[as.character(nutr_p_long$Nutrient)]),
  levels = c("TN", "TDN", "NOx-N", "TP", "TDP", "PO4-P", "TN:TP")
)

initial_mean_df$Nutrient_label <- factor(
  unname(nutrient_label_map[as.character(initial_mean_df$Nutrient)]),
  levels = levels(nutr_p_long$Nutrient_label)
)

tp_reference <- data.frame(
  Nutrient_label = factor("TP", levels = levels(nutr_p_long$Nutrient_label)),
  intercept = 0,
  slope = 1
)

nutrient_title_df <- data.frame(
  Nutrient_label = factor(levels(nutr_p_long$Nutrient_label),
                          levels = levels(nutr_p_long$Nutrient_label)),
  P_add_mg_L = 0.025,
  Value = Inf,
  label = levels(nutr_p_long$Nutrient_label)
)

p_nutrient_vs_p <- ggplot(nutr_p_long,
                          aes(x = P_add_mg_L, y = Value, color = Timepoint)) +
  geom_hline(data = initial_mean_df,
             aes(yintercept = Initial_mean, linetype = "Initial mean"),
             inherit.aes = FALSE, color = "grey45", linewidth = 0.55) +
  geom_point(position = position_jitter(width = 0.0007, height = 0),
             alpha = 0.75, size = 1.9) +
  stat_summary(fun.data = mean_se_value, geom = "errorbar",
               aes(group = Timepoint), width = 0.0014, linewidth = 0.5) +
  stat_summary(fun = mean, geom = "line",
               aes(group = Timepoint), linewidth = 0.9) +
  geom_text(data = nutrient_title_df,
            aes(x = P_add_mg_L, y = Value, label = label),
            inherit.aes = FALSE, hjust = 0.5, vjust = 1.25,
            size = 5.0, fontface = "plain") +
  facet_wrap(~ Nutrient_label, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.025, 0.05),
                     labels = c("0", "0.005", "0.01", "0.025", "0.05"),
                     limits = c(-0.0025, 0.0525),
                     expand = expansion(mult = c(0.01, 0.02)),
                     guide = guide_axis(n.dodge = 1)) +
  scale_y_continuous(breaks = function(x) pretty(x, n = 4),
                     labels = nice_axis_label,
                     expand = expansion(mult = c(0.05, 0.14))) +
  scale_color_manual(values = timepoint_cols) +
  scale_linetype_manual(name = NULL, values = c("Initial mean" = "dashed")) +
  labs(x = expression("DIP addition (mg L"^-1 * ")"),
       y = expression("Concentration (mg L"^-1 * ") / Mass ratio"),
       color = "Timepoint") +
  guides(color = guide_legend(order = 1, nrow = 1, byrow = TRUE),
         linetype = guide_legend(order = 2,
                                 override.aes = list(color = "grey45", linewidth = 0.55))) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
    panel.border = element_rect(color = "grey25", fill = NA, linewidth = 0.6),
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 45,  hjust = 1,
                               vjust = 1, size = 15,
                               margin = margin(t = 4)),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    legend.position = c(0.75, 0.05),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.background = element_blank(),
    legend.title = element_text(size = 14, margin = margin(b = 4)),
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.4, "lines"),
    legend.key.height = unit(0.9, "lines"),
    legend.spacing.x = unit(0.45, "lines"),
    legend.spacing.y = unit(0.25, "lines"),
    legend.box.spacing = unit(0.1, "lines"),
    legend.margin = margin(2, 2, 2, 2),
    plot.margin = margin(8, 10, 8, 8)
  )

ggsave("../Output_Figures/Nutrient_vs_P_addition.jpg",
       p_nutrient_vs_p, width = 12, height = 9.7, dpi = 300)

write.xlsx(nutr_p_long,"../Output_Data/Nutrient_vs_P_addition_plotdata.xlsx",
           rowNames = FALSE)
