library(openxlsx)
library(dplyr)
library(ggplot2)
library(patchwork)

################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

input_file <- "../Output_Data/Algae_endpoint_driver_from_input/Algae_endpoint_driver_from_input_results.xlsx"
cell_file  <- "../Output_Data/Cell_5_modify_tidy_calibrated.xlsx"
out_dir    <- "../Output_Figures"

dat <- read.xlsx(input_file, sheet = "Analysis_data_long")

treat_levels <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")
treat_cols <- c(
  "0P" = "#4daf4a",
  "0.005P" = "#1b9e77",
  "0.01P" = "#e68613",
  "0.025P" = "#7570b3",
  "0.05P" = "#e7298a"
)
treat_shapes <- c(
  "0P" = 0,
  "0.005P" = 16,
  "0.01P" = 17,
  "0.025P" = 15,
  "0.05P" = 3
)

plot_list <- data.frame(
  panel = c("A", "B", "C", "D", "E"),
  endpoint = c("Day2", "Day2", "Day6", "Day6", "Day6"),
  nutrient_time = c("Day2_pre", "Day2_pre", "Day2_post", "Day2_post", "Day2_post"),
  response_label = c(
    "Total_algae_cells",
    "Filamentous_cyano_cells",
    "Total_algae_cells",
    "Filamentous_cyano_cells",
    "Green_algae_cells"
  ),
  predictor = c("TP_mg_L", "TP_mg_L", "TP_mg_L", "TN_TP", "TP_mg_L"),
  title = c(
    "Day2_pre total cells vs TP",
    "Day2_pre fil. cyano cells vs TP",
    "Day6 total cells vs TP",
    "Day6 fil. cyano cells vs TN:TP",
    "Day6 chlorophyte cells vs TP"
  ),
  stringsAsFactors = FALSE
)

ylim_data <- bind_rows(lapply(seq_len(nrow(plot_list)), function(i) {
  one <- plot_list[i, ]
  dat %>%
    filter(
      endpoint == one$endpoint,
      nutrient_time == one$nutrient_time,
      response_label == one$response_label
    ) %>%
    transmute(y = response_model_value)
}))
y_limits <- range(ylim_data$y, na.rm = TRUE)
y_range <- diff(y_limits)
y_limits <- c(y_limits[1] - y_range * 0.05, y_limits[2] + y_range * 0.25)

p_text <- function(p) {
  if (is.na(p)) return("p = NA")
  if (p < 0.001) return("p < 0.001")
  paste0("p = ", sprintf("%.3f", p))
}

p_expr <- function(p) {
  if (is.na(p)) return("italic(P) == NA")
  if (p < 0.001) return("italic(P) < 0.001")
  paste0("italic(P) == ", sprintf("%.3f", p))
}

plot_data <- bind_rows(lapply(seq_len(nrow(plot_list)), function(i) {
  one <- plot_list[i, ]
  dat %>%
    filter(
      endpoint == one$endpoint,
      nutrient_time == one$nutrient_time,
      response_label == one$response_label
    ) %>%
    mutate(
      Treatment = factor(Treatment, levels = treat_levels),
      panel_id = paste0(one$panel, "  ", one$title),
      x = .data[[one$predictor]],
      y = response_model_value
    ) %>%
    filter(is.finite(x), is.finite(y))
}))

panel_levels <- paste0(plot_list$panel, "  ", plot_list$title)

plot_data$panel_id <- factor(plot_data$panel_id, levels = panel_levels)

label_data <- bind_rows(lapply(seq_len(nrow(plot_list)), function(i) {
  one <- plot_list[i, ]
  dd <- plot_data %>% filter(panel_id == paste0(one$panel, "  ", one$title))
  fit <- lm(y ~ x, data = dd)
  st <- summary(fit)
  f <- st$fstatistic
  model_p <- if (is.null(f)) NA_real_ else pf(f[1], f[2], f[3], lower.tail = FALSE)
  data.frame(
    panel_id = factor(paste0(one$panel, "  ", one$title),
                      levels = levels(plot_data$panel_id)),
    title_lab = paste0(one$panel, "  ", one$title),
    stat_expr = paste0("adj.~R^2 == ", sprintf("%.2f", st$adj.r.squared),
                       "*','~~", p_expr(model_p)),
    stringsAsFactors = FALSE
  )
}))

p_main5 <- ggplot(plot_data, aes(x = x, y = y, color = Treatment, shape = Treatment)) +
  geom_point(size = 2.6, alpha = 0.9) +
  geom_smooth(data = plot_data, aes(x = x, y = y, group = 1),
              inherit.aes = FALSE, method = "lm", se = TRUE, color = "black",
              fill = "grey70", alpha = 0.28, linewidth = 0.6) +
  geom_text(data = label_data, aes(x = -Inf, y = Inf, label = title_lab),
            inherit.aes = FALSE, hjust = -0.03, vjust = 1.45,
            size = 5, color = "black") +
  geom_text(data = label_data, aes(x = -Inf, y = Inf, label = stat_expr),
            inherit.aes = FALSE, parse = TRUE, hjust = -0.03, vjust = 2.5,
            size = 5, color = "black") +
  scale_color_manual(values = treat_cols, drop = FALSE) +
  scale_shape_manual(values = treat_shapes, drop = FALSE) +
  scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0.04))) +
  facet_wrap(~ panel_id, ncol = 2, scales = "free_x") +
  labs(x = expression("TP (mg L"^-1*") or TN:TP"),
       y = expression(ln(cells~mL^-1 + 1)),
       color = "Treatment",
       shape = "Treatment") +
  theme_bw(base_size = 15) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing.x = unit(1.5, "lines"),
    panel.spacing.y = unit(0.9, "lines"),
    legend.position = c(0.74, 0.13),
    legend.justification = c(0.5, 0.5),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.box = "vertical",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.key.width = unit(1.1, "lines"),
    legend.key.height = unit(0.9, "lines"),
    legend.spacing.x = unit(0.35, "lines"),
    legend.spacing.y = unit(0.15, "lines"),
    legend.margin = margin(0, 0, 0, 0),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(size = 12)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE))

out_file <- file.path(out_dir, "Selected_AICc_relationships_main5.png")
tmp_file <- tempfile(fileext = ".png")

ggsave(tmp_file, p_main5, width = 10, height = 8, dpi = 500, device = "png")
file.copy(tmp_file, out_file, overwrite = TRUE)
unlink(tmp_file)

cat("Saved:\n")
cat(out_file, "\n")
