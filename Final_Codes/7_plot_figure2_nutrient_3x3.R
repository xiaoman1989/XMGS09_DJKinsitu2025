library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

################################################################################
## Figure 2. Nutrient trajectories in 3 x 3 layout
################################################################################

dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

input_file <- "../Input_Data/insitu_water_quality.xlsx"
out_dir <- "../Output_Figures/"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

dat <- read.xlsx(input_file, sheet = 1)
dat <- as.data.frame(dat, stringsAsFactors = FALSE)

names(dat) <- trimws(names(dat))
dat$Treatment <- trimws(as.character(dat$Treatment))
dat$Replication <- as.numeric(as.character(dat$Replication))
dat$Day <- as.numeric(as.character(dat$Day))
dat$Batch <- as.numeric(as.character(dat$Batch))

to_num <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", "", x)
  x <- trimws(x)
  x[x %in% c("", "NA", "N/A", "ND", "n.d.", "-", "—")] <- NA
  x <- gsub("^<", "", x)
  x <- gsub(",", "", x)
  as.numeric(x)
}

num_cols <- c("TN_mg_L", "TDN_mg_L", "TP_mg_L", "TDP_mg_L",
              "NH4-N_mg_L", "NOx-N_mg_L", "PO4-P_mg_L", "TN_TP")
for (v in num_cols) dat[[v]] <- to_num(dat[[v]])

dat$PP_mg_L <- dat$TP_mg_L - dat$TDP_mg_L
dat$DIN_mg_L <- dat$`NH4-N_mg_L` + dat$`NOx-N_mg_L`
dat$TDN_TDP <- dat$TDN_mg_L / dat$TDP_mg_L

treat_levels <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")
treat_cols <- c(
  "0P" = "#8F8F8F",
  "0.005P" = "#56B4E9",
  "0.01P" = "#009E73",
  "0.025P" = "#E69F00",
  "0.05P" = "#D55E00"
)

p_add_map <- c("0P" = 0, "0.005P" = 0.005, "0.01P" = 0.01,
               "0.025P" = 0.025, "0.05P" = 0.05)

vars_keep <- c("TP_mg_L", "TDP_mg_L", "PP_mg_L",
               "TN_mg_L", "TDN_mg_L", "DIN_mg_L",
               "TN_TP", "TDN_TDP")

label_map <- c(
  "TP_mg_L" = "TP",
  "TDP_mg_L" = "TDP",
  "PP_mg_L" = "Particulate P",
  "TN_mg_L" = "TN",
  "TDN_mg_L" = "TDN",
  "DIN_mg_L" = "DIN",
  "TN_TP" = "TN:TP",
  "TDN_TDP" = "TDN:TDP"
)

## Initial values.
initial_long <- dat %>%
  dplyr::filter(Treatment == "Initial", Day == 0) %>%
  dplyr::select(Treatment, Replication, dplyr::all_of(vars_keep)) %>%
  pivot_longer(dplyr::all_of(vars_keep), names_to = "Variable", values_to = "Value") %>%
  dplyr::mutate(Timepoint = "Initial", Data_type = "Measured")

## Day2_pre and Day6 measured values.
measured_long <- dat %>%
  dplyr::filter(Treatment %in% treat_levels,
         (Day == 2 & Batch == 1) | Day == 6) %>%
  dplyr::mutate(Timepoint = ifelse(Day == 2, "Day2_pre", "Day6"),
         Data_type = "Measured") %>%
  dplyr::select(Treatment, Replication, Timepoint, Data_type, dplyr::all_of(vars_keep)) %>%
  pivot_longer(dplyr::all_of(vars_keep), names_to = "Variable", values_to = "Value")

## Day2_post calculated after 80:20 replacement with filtered lake water.
initial_diss_mean <- dat %>%
  dplyr::filter(Treatment == "Initial_diss") %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(vars_keep), ~ mean(.x, na.rm = TRUE)))

day2_pre_wide <- dat %>%
  dplyr::filter(Treatment %in% treat_levels, Day == 2, Batch == 1) %>%
  dplyr::select(Treatment, Replication, dplyr::all_of(vars_keep))

day2_post_wide <- day2_pre_wide
for (v in vars_keep) {
  day2_post_wide[[v]] <- 0.8 * day2_pre_wide[[v]] + 0.2 * initial_diss_mean[[v]][1]
}

day2_post_long <- day2_post_wide %>%
  dplyr::mutate(Timepoint = "Day2_post", Data_type = "Calculated after dilution") %>%
  dplyr::select(Treatment, Replication, Timepoint, Data_type, dplyr::all_of(vars_keep)) %>%
  pivot_longer(dplyr::all_of(vars_keep), names_to = "Variable", values_to = "Value")

plot_long <- dplyr::bind_rows(initial_long, measured_long, day2_post_long) %>%
  dplyr::mutate(
    Timepoint = factor(Timepoint, levels = c("Initial", "Day2_pre", "Day2_post", "Day6")),
    Treatment = factor(Treatment, levels = c("Initial", treat_levels)),
    Variable = factor(Variable, levels = vars_keep),
    Label = factor(unname(label_map[as.character(Variable)]),
                   levels = unname(label_map[vars_keep])),
    x_base = as.numeric(Timepoint)
  )

## Use the mean initial value as common starting point for treatment lines.
initial_mean <- plot_long %>%
  dplyr::filter(Timepoint == "Initial") %>%
  dplyr::group_by(Variable, Label) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::crossing(Treatment = factor(treat_levels, levels = c("Initial", treat_levels))) %>%
  dplyr::mutate(Replication = NA_real_, Timepoint = factor("Initial", levels = levels(plot_long$Timepoint)),
         Data_type = "Mean", x_base = 1)

line_mean <- plot_long %>%
  dplyr::filter(Treatment %in% treat_levels) %>%
  dplyr::group_by(Treatment, Timepoint, Variable, Label) %>%
  dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(x_base = as.numeric(Timepoint), Data_type = "Mean", Replication = NA_real_) %>%
  dplyr::bind_rows(initial_mean) %>%
  dplyr::arrange(Variable, Treatment, Timepoint)

mean_sd <- plot_long %>%
  dplyr::group_by(Treatment, Timepoint, Variable, Label, Data_type) %>%
  dplyr::summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    n = sum(is.finite(Value)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(x_base = as.numeric(Timepoint))

title_df <- data.frame(
  Label = factor(unname(label_map[vars_keep]), levels = unname(label_map[vars_keep])),
  x = 2.5,
  y = Inf,
  title_text = unname(label_map[vars_keep])
)

get_group_ylim <- function(vars, lower_zero = FALSE) {
  raw_vals <- plot_long %>%
    dplyr::filter(Variable %in% vars) %>%
    dplyr::pull(Value)

  sd_vals <- mean_sd %>%
    dplyr::filter(Variable %in% vars) %>%
    dplyr::transmute(lo = Mean - SD, hi = Mean + SD)

  yy <- c(raw_vals, sd_vals$lo, sd_vals$hi)
  yy <- yy[is.finite(yy)]
  ymin <- min(yy, na.rm = TRUE)
  ymax <- max(yy, na.rm = TRUE)
  yrng <- ymax - ymin
  if (!is.finite(yrng) || yrng == 0) yrng <- abs(ymax) * 0.1 + 1
  ymin <- ymin - 0.06 * yrng
  ymax <- ymax + 0.14 * yrng
  if (lower_zero) ymin <- min(0, ymin)
  c(ymin, ymax)
}

p_ylim <- c(0, 0.08)
n_ylim <- c(0.1, 1.35)
ratio_ylim <- c(0, 105)

p_breaks <- c(0, 0.025, 0.050, 0.075)
n_breaks <- c(0.1, 0.5, 0.9, 1.3)
ratio_breaks <- c(0, 35, 70, 105)

break_4 <- function(x) {
  seq(x[1], x[2], length.out = 4)
}

make_traj <- function(var, ylab = NULL, show_legend = FALSE, ylim = NULL, ybreaks = NULL) {
  lab_now <- label_map[[var]]
  pd <- plot_long %>% dplyr::filter(Variable == var, Treatment != "Initial")
  init_pd <- plot_long %>% dplyr::filter(Variable == var, Timepoint == "Initial")
  lm <- line_mean %>% dplyr::filter(Variable == var)
  ms <- mean_sd %>% dplyr::filter(Variable == var, Treatment != "Initial")
  init_ms <- mean_sd %>% dplyr::filter(Variable == var, Treatment == "Initial")
  ttl <- title_df %>% dplyr::filter(as.character(Label) == lab_now)

  ggplot() +
    geom_line(data = lm, aes(x = x_base, y = Value, color = Treatment, group = Treatment),
              linewidth = 0.65) +
    geom_point(data = pd, aes(x = x_base, y = Value, color = Treatment,
                              shape = Data_type),
               size = 1.25, alpha = 0.62, stroke = 0.55,
               position = position_jitter(width = 0.055, height = 0)) +
    geom_point(data = init_pd, aes(x = x_base, y = Value),
               color = "black", fill = "black", shape = 16, size = 1.25,
               alpha = 0.62, position = position_jitter(width = 0.055, height = 0)) +
    geom_errorbar(data = ms, aes(x = x_base, ymin = Mean - SD, ymax = Mean + SD, color = Treatment),
                  width = 0.08, linewidth = 0.45) +
    geom_point(data = ms, aes(x = x_base, y = Mean, color = Treatment,
                              shape = Data_type),
               size = 2.45, stroke = 0.75) +
    geom_errorbar(data = init_ms, aes(x = x_base, ymin = Mean - SD, ymax = Mean + SD),
                  color = "black", width = 0.08, linewidth = 0.45) +
    geom_point(data = init_ms, aes(x = x_base, y = Mean),
               color = "black", fill = "black", shape = 16, size = 2.45) +
    geom_text(data = ttl, aes(x = x, y = y, label = title_text),
              inherit.aes = FALSE, hjust = 0.5, vjust = 1.35, size = 4.3) +
    scale_x_continuous(breaks = 1:4,
                       labels = c("Initial", "Day2_pre", "Day2_post", "Day6"),
                       limits = c(0.65, 4.35)) +
    scale_y_continuous(limits = ylim, breaks = ybreaks,
                       expand = expansion(mult = c(0, 0))) +
    scale_color_manual(values = treat_cols, breaks = treat_levels, name = "Cumulative DIP addition") +
    scale_shape_manual(values = c("Measured" = 16, "Calculated after dilution" = 1, "Mean" = 16),
                       breaks = c("Measured", "Calculated after dilution"),
                       labels = c("Measured", "Calculated Day2_post"),
                       name = NULL) +
    labs(x = NULL, y = ylab) +
    theme_classic(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 35, hjust = 1),
      axis.title.y = element_text(size = 13),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE),
           shape = guide_legend(nrow = 1, byrow = TRUE))
}

## Delta particulate P after three P pulses.
initial_pp_mean <- dat %>%
  dplyr::filter(Treatment == "Initial") %>%
  dplyr::summarise(PP = mean(PP_mg_L, na.rm = TRUE)) %>%
  dplyr::pull(PP)

delta_pp_rep <- dat %>%
  dplyr::filter(Treatment %in% treat_levels, Day == 2, Batch == 1) %>%
  dplyr::mutate(P_add = unname(p_add_map[Treatment]),
         Delta_PP = PP_mg_L - initial_pp_mean,
         Treatment = factor(Treatment, levels = treat_levels))

delta_pp <- delta_pp_rep %>%
  dplyr::group_by(Treatment, P_add) %>%
  dplyr::summarise(Mean = mean(Delta_PP, na.rm = TRUE),
            SD = sd(Delta_PP, na.rm = TRUE),
            .groups = "drop")

fit_delta <- lm(Delta_PP ~ P_add, data = delta_pp_rep)
fit_sum <- summary(fit_delta)
p_delta <- pf(fit_sum$fstatistic[1], fit_sum$fstatistic[2], fit_sum$fstatistic[3], lower.tail = FALSE)
lab_delta <- paste0("R\u00b2 = ", sprintf("%.2f", fit_sum$r.squared),
                    "\nP ", ifelse(p_delta < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p_delta))))
delta_y_breaks <- seq(-0.01, 0.05, by = 0.02)

p_delta <- ggplot(delta_pp, aes(x = P_add, y = Mean, color = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey55", linewidth = 0.45) +
  geom_smooth(data = delta_pp_rep, aes(x = P_add, y = Delta_PP, group = 1),
              inherit.aes = FALSE, method = "lm", se = TRUE, color = "black",
              fill = "grey80", linewidth = 0.8) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.0013, linewidth = 0.5) +
  geom_point(size = 2.55, stroke = 0.65) +
  annotate("text", x = 0.006, y = max(delta_pp$Mean + delta_pp$SD, na.rm = TRUE) * 0.9,
           label = lab_delta, hjust = 0, vjust = 1, size = 4.1) +
  scale_color_manual(values = treat_cols, breaks = treat_levels, name = "Cumulative DIP addition") +
  scale_x_continuous(breaks = c(0, 0.005, 0.01, 0.025, 0.05),
                     labels = c("0", "0.005", "0.01", "0.025", "0.05")) +
  scale_y_continuous(limits = c(-0.01, 0.05),
                     breaks = delta_y_breaks,
                     labels = sprintf("%.2f", delta_y_breaks),
                     expand = expansion(mult = c(0.08, 0.14))) +
  labs(x = expression("DIP addition"~"(mg P L"^-1*")"),
       y = expression(Delta*"PP"[pulse]~"(mg P L"^-1*")")) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "none",
    axis.title.x = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(color = "none")

p1 <- make_traj("TP_mg_L", expression("P concentration (mg L"^-1*")"), ylim = p_ylim, ybreaks = p_breaks)
p2 <- make_traj("TDP_mg_L", NULL, ylim = p_ylim, ybreaks = p_breaks)
p3 <- make_traj("PP_mg_L", NULL, ylim = p_ylim, ybreaks = p_breaks)
p4 <- make_traj("TN_mg_L", expression("N concentration (mg L"^-1*")"), ylim = n_ylim, ybreaks = n_breaks)
p5 <- make_traj("TDN_mg_L", NULL, ylim = n_ylim, ybreaks = n_breaks)
p6 <- make_traj("DIN_mg_L", NULL, ylim = n_ylim, ybreaks = n_breaks)
p7 <- make_traj("TN_TP", "N:P mass ratio", ylim = ratio_ylim, ybreaks = ratio_breaks)
p8 <- make_traj("TDN_TDP", NULL, ylim = ratio_ylim, ybreaks = ratio_breaks)

p_all <- ((p1 | p2 | p3) /
  (p4 | p5 | p6) /
  (p7 | p8 | p_delta)) +
  plot_annotation(tag_levels = "A")

p_all <- p_all + plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(0, 0, 0, 0),
        plot.tag = element_text(size = 15, face = "plain"))

out_pdf <- file.path(out_dir, "Figure2_nutrient_PP_ratios_3x3.pdf")
out_png <- file.path(out_dir, "Figure2_nutrient_PP_ratios_3x3.png")

ggsave(out_pdf, p_all, width = 8.2, height = 8.9, device = cairo_pdf)
ggsave(out_png, p_all, width = 8.2, height = 8.9, dpi = 450)

write.xlsx(delta_pp, file.path(out_dir, "Figure2_delta_PP_pulse_plotdata.xlsx"), rowNames = FALSE)

cat("Saved:\n", out_pdf, "\n", out_png, "\n")
