################################################################################
## Shape-based stacked bar plot
## FINAL CLEAN VERSION
################################################################################

library(openxlsx)
library(plyr)
library(ggplot2)
library(grid)
library(cowplot)

################################################################################
## 0) Path
################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

dir.create("../Output_Figures", showWarnings = FALSE, recursive = TRUE)

################################################################################
## 1) Read data
################################################################################
df <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
df <- as.data.frame(df, stringsAsFactors = FALSE)

df <- df[as.character(df$Treatment) != "0.005P_2", ]

df$Treatment    <- trimws(as.character(df$Treatment))
df$Replication  <- trimws(as.character(df$Replication))
df$Day          <- as.numeric(as.character(df$Day))
df$Latin_phylum <- trimws(as.character(df$Latin_phylum))
df$Genus_Latin  <- trimws(as.character(df$Genus_Latin))
df$Cells_mL     <- as.numeric(as.character(df$Cells_mL))
df$Morphology   <- trimws(as.character(df$Morphology))

df$Morphology[is.na(df$Morphology) | df$Morphology == ""] <- "Non_cyano"

df$Day_label <- paste0("Day", df$Day)

################################################################################
## 2) Keep only wanted Day × Treatment
################################################################################
keep_idx <- (
  (df$Day_label == "Day0" & df$Treatment == "Initial") |
    (df$Day_label == "Day2" & df$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P")) |
    (df$Day_label == "Day6" & df$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"))
)

df2 <- df[keep_idx, , drop = FALSE]

################################################################################
## 3) Order and colors
################################################################################
group_order_legend <- c(
  "Filamentous",
  "Colonial",
  "Unicellular-colonial",
  "Other_cyano",
  "Non_cyano"
)

group_order_stack <- c(
  "Non_cyano",
  "Other_cyano",
  "Unicellular-colonial",
  "Colonial",
  "Filamentous"
)

group_cols <- c(
  "Filamentous"          = "red2",
  "Colonial"             = "blue1",
  "Unicellular-colonial" = "lightblue",
  "Other_cyano"          = "purple",
  "Non_cyano"            = "grey80"
)

################################################################################
## 4) Summarise replicate level
################################################################################
sum_rep <- ddply(
  df2,
  .variables = c("Day_label", "Treatment", "Replication", "Morphology"),
  .fun = function(x){
    data.frame(Cells_group = sum(x$Cells_mL, na.rm = TRUE))
  }
)

################################################################################
## 5) Complete missing Morphology within each replicate
################################################################################
rep_keys <- unique(sum_rep[, c("Day_label", "Treatment", "Replication")])

all_rep_groups <- merge(
  rep_keys,
  data.frame(Morphology = group_order_legend, stringsAsFactors = FALSE),
  by = NULL
)

sum_rep2 <- merge(
  all_rep_groups,
  sum_rep,
  by = c("Day_label", "Treatment", "Replication", "Morphology"),
  all.x = TRUE
)

sum_rep2$Cells_group[is.na(sum_rep2$Cells_group)] <- 0

################################################################################
## 6) Percent within each replicate
################################################################################
rep_total <- ddply(
  sum_rep2,
  .variables = c("Day_label", "Treatment", "Replication"),
  .fun = function(x){
    data.frame(Total_cells = sum(x$Cells_group, na.rm = TRUE))
  }
)

sum_rep3 <- merge(
  sum_rep2,
  rep_total,
  by = c("Day_label", "Treatment", "Replication"),
  all.x = TRUE
)

sum_rep3$Percent <- ifelse(
  sum_rep3$Total_cells > 0,
  sum_rep3$Cells_group / sum_rep3$Total_cells * 100,
  0
)

################################################################################
## 7) Mean across replicates
################################################################################
sum_mean <- ddply(
  sum_rep3,
  .variables = c("Day_label", "Treatment", "Morphology"),
  .fun = function(x){
    data.frame(Percent = mean(x$Percent, na.rm = TRUE))
  }
)

################################################################################
## 8) Add all wanted combinations
################################################################################
wanted_keys <- rbind(
  data.frame(Day_label = "Day0", Treatment = "Initial", stringsAsFactors = FALSE),
  data.frame(Day_label = "Day2", Treatment = c("0P", "0.005P", "0.01P", "0.025P", "0.05P"), stringsAsFactors = FALSE),
  data.frame(Day_label = "Day6", Treatment = c("0P", "0.005P", "0.01P", "0.025P", "0.05P"), stringsAsFactors = FALSE)
)

plot_template <- merge(
  wanted_keys,
  data.frame(Morphology = group_order_legend, stringsAsFactors = FALSE),
  by = NULL
)

plot_df <- merge(
  plot_template,
  sum_mean,
  by = c("Day_label", "Treatment", "Morphology"),
  all.x = TRUE
)

plot_df$Percent[is.na(plot_df$Percent)] <- 0

################################################################################
## 9) Factor order
################################################################################
day_order <- c("Day0", "Day2", "Day6")
trt_order_plot <- c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P")

plot_df$Day_label  <- factor(plot_df$Day_label, levels = day_order)
plot_df$Treatment  <- factor(plot_df$Treatment, levels = trt_order_plot)
plot_df$Morphology <- factor(plot_df$Morphology, levels = group_order_stack)

plot_df <- plot_df[order(plot_df$Day_label, plot_df$Treatment, plot_df$Morphology), ]

################################################################################
## 10) Force each bar sum = 100
################################################################################
plot_df <- ddply(
  plot_df,
  .(Day_label, Treatment),
  function(x){
    s <- sum(x$Percent, na.rm = TRUE)
    if(s > 0){
      x$Percent <- x$Percent / s * 100
    }
    x
  }
)

write.xlsx(
  plot_df,
  "../Output_Data/Cyanophyta_shape_percent_plot_data.xlsx",
  rowNames = FALSE
)

################################################################################
## 12) Main shape stacked bar plot
################################################################################
p_shape_bar <- ggplot(plot_df, aes(x = Treatment, y = Percent, fill = Morphology)) +
  geom_col(width = 0.75, color = "black", linewidth = 0.3, position = "stack") +
  facet_grid(. ~ Day_label, scales = "free_x", space = "free_x") +
  scale_fill_manual(
    values = group_cols,
    breaks = group_order_legend,
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80, 100),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = expression("DIP addition (" * mg~L^{-1} * ")"),
    y = "Cell concentration (%)",
    fill = NULL
  ) +
  theme_classic(base_size = 20, base_family = "sans") +
  theme(
    text = element_text(family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans", color = "black"),
    axis.title  = element_text(family = "sans"),
    axis.text   = element_text(family = "sans", color = "black"),
    strip.text = element_text(family = "sans"),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none",
    panel.grid = element_blank()
  )

ggsave(
  "../Output_Figures/Cyanophyta_shape_barplot_final.jpg",
  plot = p_shape_bar,
  width = 10,
  height = 4.5,
  dpi = 300
)

################################################################################
## 13) Legend only
################################################################################
p_legend <- ggplot(plot_df, aes(x = Treatment, y = Percent, fill = Morphology)) +
  geom_col() +
  scale_fill_manual(
    values = group_cols,
    breaks = group_order_legend,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(colour = NA)
    )
  ) +
  theme_void(base_family = "sans") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, family = "sans"),
    legend.key.width = unit(8, "mm"),
    legend.key.height = unit(6, "mm")
  )

legend_grob <- cowplot::get_legend(p_legend)

ggsave(
  "../Output_Figures/Cyanophyta_shape_legend.jpg",
  plot = legend_grob,
  width = 10,
  height = 1,
  dpi = 300
)

################################################################################
## 14) Boxplot: Cyanophyta percentage of total cell concentration
## Initial + Day6 only
################################################################################
keep_idx_box <- (
  (df$Day_label == "Day0" & df$Treatment == "Initial") |
    (df$Day_label == "Day6" & df$Treatment %in% c("0P", "0.005P", "0.01P", "0.025P", "0.05P"))
)

df_box <- df[keep_idx_box, , drop = FALSE]

rep_total_box <- ddply(
  df_box,
  .variables = c("Day_label", "Treatment", "Replication"),
  .fun = function(x){
    data.frame(Total_cells = sum(x$Cells_mL, na.rm = TRUE))
  }
)

rep_cyano_box <- ddply(
  df_box[df_box$Latin_phylum == "Cyanophyta", , drop = FALSE],
  .variables = c("Day_label", "Treatment", "Replication"),
  .fun = function(x){
    data.frame(Cyano_cells = sum(x$Cells_mL, na.rm = TRUE))
  }
)

cyano_pct_rep <- merge(
  rep_total_box,
  rep_cyano_box,
  by = c("Day_label", "Treatment", "Replication"),
  all.x = TRUE
)

cyano_pct_rep$Cyano_cells[is.na(cyano_pct_rep$Cyano_cells)] <- 0

cyano_pct_rep$Cyano_percent <- ifelse(
  cyano_pct_rep$Total_cells > 0,
  cyano_pct_rep$Cyano_cells / cyano_pct_rep$Total_cells * 100,
  0
)

cyano_pct_rep$P_add <- NA_real_
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "Initial"] <- -0.01
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "0P"]      <- 0
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "0.005P"]  <- 0.005
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "0.01P"]   <- 0.01
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "0.025P"]  <- 0.025
cyano_pct_rep$P_add[cyano_pct_rep$Treatment == "0.05P"]   <- 0.05

cyano_pct_rep$Day_group <- as.character(cyano_pct_rep$Day_label)
cyano_pct_rep$Day_group[cyano_pct_rep$Treatment == "Initial"] <- "Initial"
cyano_pct_rep$Day_group <- factor(cyano_pct_rep$Day_group, levels = c("Initial", "Day6"))

cyano_pct_rep <- cyano_pct_rep[order(cyano_pct_rep$P_add, cyano_pct_rep$Day_group), ]

day_cols_box <- c(
  "Initial" = "grey75",
  "Day6"    = "#F4A261"
)

p_cyano_box <- ggplot(
  cyano_pct_rep,
  aes(x = P_add, y = Cyano_percent, fill = Day_group)
) +
  geom_boxplot(
    aes(group = interaction(P_add, Day_group)),
    outlier.shape = NA,
    width = 0.0035,
    color = "black",
    position = position_dodge(width = 0.004)
  ) +
  geom_jitter(
    aes(group = interaction(P_add, Day_group)),
    size = 2,
    alpha = 0.9,
    shape = 21,
    color = "black",
    stroke = 0.3,
    position = position_jitterdodge(
      jitter.width = 0.001,
      dodge.width = 0.004
    )
  ) +
  scale_x_continuous(
    breaks = c(-0.01, 0, 0.005, 0.01, 0.025, 0.05),
    labels = c("Initial", "0", "0.005", "0.01", "0.025", "0.05"),
    expand = c(0.01, 0.01)
  ) +
  scale_fill_manual(values = day_cols_box, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100),
    expand = c(0.02, 0.02)
  ) +
  labs(
    x = expression("DIP addition (" * mg~L^{-1} * ")"),
    y = "Cyanophyta to total\ncell concentration (%)",
    fill = NULL
  ) +
  theme_classic(base_size = 24, base_family = "sans") +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave(
  "../Output_Figures/Cyanophyta_percent_boxplot_day6.jpg",
  plot = p_cyano_box,
  width = 8,
  height = 5.8,
  dpi = 300
)

cat("Run successfully.\n")
