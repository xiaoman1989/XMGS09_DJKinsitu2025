################################################################################
## CYANOBACTERIA GENUS-LEVEL FIGURES
## - Pie plot
## - Genus-level stacked bar plots
## - Percentage stacked bar plots
## - Add significance letters above absolute bar plots
################################################################################

library(plyr)
library(ggplot2)
library(ggforce)
library(openxlsx)
library(ggtext)
library(grid)
library(cowplot)
library(agricolae)

################################################################################
# 0) PATH + READ DATA
################################################################################
dirName <- "~/My Drive/00_Project/XMGS08_NFSField/Final_Codes"
setwd(dirName)

df <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
df <- as.data.frame(df, stringsAsFactors = FALSE)

df <- df[as.character(df$Treatment) != "0.005P_2", ]

df$Treatment       <- trimws(as.character(df$Treatment))
df$Replication     <- trimws(as.character(df$Replication))
df$Day             <- as.numeric(as.character(df$Day))
df$Latin_phylum    <- trimws(as.character(df$Latin_phylum))
df$Genus_Latin     <- trimws(as.character(df$Genus_Latin))
df$Morphology      <- trimws(as.character(df$Morphology))
df$Cells_mL        <- as.numeric(as.character(df$Cells_mL))
df$Biovolume_mm3_L <- as.numeric(as.character(df$Biovolume_mm3_L))

################################################################################
# 1) GLOBAL SETTINGS
################################################################################
panel_levels <- c(
  "Initial",
  "0P_Day2","0.005P_Day2","0.01P_Day2","0.025P_Day2","0.05P_Day2",
  "0P_Day6","0.005P_Day6","0.01P_Day6","0.025P_Day6","0.05P_Day6"
)

treatment_levels <- c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P")
day_group_levels <- c("Day 0", "Day 2", "Day 6")
sig_trt_levels <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")

################################################################################
# 2) HELPER FUNCTIONS
################################################################################
keep_cyano_data <- function(dat){
  dat <- dat[dat$Latin_phylum == "Cyanophyta", , drop = FALSE]
  
  keep_idx <-
    (dat$Treatment == "Initial") |
    (dat$Day == 2 & dat$Treatment %in% sig_trt_levels) |
    (dat$Day == 6 & dat$Treatment %in% sig_trt_levels)
  
  dat <- dat[keep_idx, , drop = FALSE]
  
  dat$Panel <- NA_character_
  dat$Panel[dat$Treatment == "Initial"] <- "Initial"
  dat$Panel[dat$Day == 2 & dat$Treatment %in% sig_trt_levels] <-
    paste0(dat$Treatment[dat$Day == 2 & dat$Treatment %in% sig_trt_levels], "_Day2")
  dat$Panel[dat$Day == 6 & dat$Treatment %in% sig_trt_levels] <-
    paste0(dat$Treatment[dat$Day == 6 & dat$Treatment %in% sig_trt_levels], "_Day6")
  
  dat$Panel <- factor(dat$Panel, levels = panel_levels)
  dat <- dat[!is.na(dat$Panel), , drop = FALSE]
  dat
}

add_panel_layout <- function(dat){
  dat$Day_group <- NA_character_
  dat$Day_group[dat$Panel == "Initial"] <- "Day 0"
  dat$Day_group[grepl("_Day2$", as.character(dat$Panel))] <- "Day 2"
  dat$Day_group[grepl("_Day6$", as.character(dat$Panel))] <- "Day 6"
  dat$Day_group <- factor(dat$Day_group, levels = day_group_levels)
  
  dat$Treatment_clean <- as.character(dat$Panel)
  dat$Treatment_clean[dat$Panel == "Initial"] <- "Initial"
  dat$Treatment_clean <- gsub("_Day2$", "", dat$Treatment_clean)
  dat$Treatment_clean <- gsub("_Day6$", "", dat$Treatment_clean)
  dat$Treatment_clean <- factor(dat$Treatment_clean, levels = treatment_levels)
  
  dat
}

calc_metric_tables <- function(dat, value_col){
  genus_rep <- ddply(dat, .(Panel, Replication, Genus_Latin), function(x){
    data.frame(value_rep = sum(x[[value_col]], na.rm = TRUE))
  })
  
  rep_list <- unique(genus_rep[, c("Panel", "Replication")])
  genus_list <- sort(unique(trimws(as.character(genus_rep$Genus_Latin))))
  full_grid <- merge(rep_list, data.frame(Genus_Latin = genus_list), by = NULL)
  
  genus_full <- merge(full_grid, genus_rep,
                      by = c("Panel", "Replication", "Genus_Latin"),
                      all.x = TRUE)
  genus_full$value_rep[is.na(genus_full$value_rep)] <- 0
  
  total_rep <- ddply(genus_full, .(Panel, Replication), function(x){
    data.frame(Total_rep = sum(x$value_rep, na.rm = TRUE))
  })
  
  total_mean <- ddply(total_rep, .(Panel), function(x){
    yy <- x$Total_rep
    data.frame(
      Total_mean = mean(yy, na.rm = TRUE),
      Total_sd   = ifelse(sum(!is.na(yy)) > 1, sd(yy, na.rm = TRUE), NA_real_)
    )
  })
  total_mean$Panel <- factor(total_mean$Panel, levels = panel_levels)
  
  genus_mean <- ddply(genus_full, .(Panel, Genus_Latin), function(x){
    yy <- x$value_rep
    data.frame(
      Mean_value = mean(yy, na.rm = TRUE),
      SD_value   = ifelse(sum(!is.na(yy)) > 1, sd(yy, na.rm = TRUE), NA_real_)
    )
  })
  genus_mean$Panel <- factor(genus_mean$Panel, levels = panel_levels)
  
  genus_full2 <- merge(genus_full, total_rep, by = c("Panel", "Replication"), all.x = TRUE)
  genus_full2$Percent_rep <- ifelse(genus_full2$Total_rep > 0,
                                    genus_full2$value_rep / genus_full2$Total_rep * 100,
                                    0)
  
  genus_percent_mean <- ddply(genus_full2, .(Panel, Genus_Latin), function(x){
    yy <- x$Percent_rep
    data.frame(
      Percent_mean = mean(yy, na.rm = TRUE),
      Percent_sd   = ifelse(sum(!is.na(yy)) > 1, sd(yy, na.rm = TRUE), NA_real_)
    )
  })
  genus_percent_mean$Panel <- factor(genus_percent_mean$Panel, levels = panel_levels)
  
  sum_check <- ddply(genus_percent_mean, .(Panel), function(x){
    data.frame(SumPercent = sum(x$Percent_mean, na.rm = TRUE))
  })
  
  genus_percent_mean <- merge(genus_percent_mean, sum_check, by = "Panel", all.x = TRUE)
  genus_percent_mean$Percent_fix <- ifelse(genus_percent_mean$SumPercent > 0,
                                           genus_percent_mean$Percent_mean /
                                             genus_percent_mean$SumPercent * 100,
                                           0)
  
  list(
    genus_rep = genus_rep,
    genus_full = genus_full,
    total_rep = total_rep,
    total_mean = total_mean,
    genus_mean = genus_mean,
    genus_percent_mean = genus_percent_mean
  )
}

add_shape_info <- function(dat, shape_df, shape_levels, genus_order){
  dat <- merge(dat, shape_df[, c("Genus_Latin", "Morphology")],
               by = "Genus_Latin", all.x = TRUE)
  
  dat$shape_level <- as.character(dat$Morphology)
  dat$shape_level[is.na(dat$shape_level) | dat$shape_level == ""] <- "Others"
  dat$shape_level <- factor(dat$shape_level, levels = shape_levels)
  
  dat$Genus_Latin <- factor(as.character(dat$Genus_Latin), levels = genus_order)
  dat$FillLegend <- paste(dat$shape_level, dat$Genus_Latin, sep = " | ")
  dat
}

get_sig_letters <- function(total_rep_df){
  total_rep_df <- add_panel_layout(total_rep_df)
  total_rep_df <- total_rep_df[total_rep_df$Day_group %in% c("Day 2", "Day 6"), ]
  
  out <- data.frame()
  
  for(dd in c("Day 2", "Day 6")){
    tmp <- total_rep_df[total_rep_df$Day_group == dd, ]
    tmp$Treatment_clean <- factor(as.character(tmp$Treatment_clean),
                                  levels = sig_trt_levels)
    
    fit <- aov(Total_rep ~ Treatment_clean, data = tmp)
    lsd <- LSD.test(fit, "Treatment_clean", p.adj = "none")
    
    one <- data.frame(
      Treatment_clean = rownames(lsd$groups),
      Letters = as.character(lsd$groups$groups),
      Day_group = dd,
      stringsAsFactors = FALSE
    )
    
    out <- rbind(out, one)
  }
  
  out$Treatment_clean <- factor(out$Treatment_clean, levels = treatment_levels)
  out$Day_group <- factor(out$Day_group, levels = day_group_levels)
  out
}

################################################################################
# 3) PREP CYANOBACTERIA DATA
################################################################################
df_cyano <- keep_cyano_data(df)

################################################################################
# 4) CALCULATE TABLES
################################################################################
cells_obj <- calc_metric_tables(df_cyano, "Cells_mL")
bv_obj    <- calc_metric_tables(df_cyano, "Biovolume_mm3_L")

cyano_mean            <- cells_obj$total_mean
genus_cells_mean      <- cells_obj$genus_mean
genus_percent_mean    <- cells_obj$genus_percent_mean

cyano_bv_mean         <- bv_obj$total_mean
genus_bv_mean         <- bv_obj$genus_mean
genus_bv_percent_mean <- bv_obj$genus_percent_mean

################################################################################
# 5) COLOR MAP + SHAPE MAP
################################################################################
genus_color_df <- read.xlsx("../../../08_Reference/Algae_color.xlsx")
genus_color_df <- as.data.frame(genus_color_df, stringsAsFactors = FALSE)
genus_color_df$Genus_Latin <- trimws(as.character(genus_color_df$Genus_Latin))
genus_color_df$Color       <- trimws(as.character(genus_color_df$Color))
genus_cols <- setNames(genus_color_df$Color, genus_color_df$Genus_Latin)

shape_df <- read.xlsx("../../../08_Reference/Algae_shape.xlsx")
shape_df <- as.data.frame(shape_df, stringsAsFactors = FALSE)
shape_df$Genus_Latin <- trimws(as.character(shape_df$Genus_Latin))
shape_df$Morphology  <- trimws(as.character(shape_df$Morphology))

shape_levels <- unique(shape_df$Morphology)
shape_levels <- shape_levels[!is.na(shape_levels) & shape_levels != ""]
if(!("Others" %in% shape_levels)) shape_levels <- c(shape_levels, "Others")

genus_order <- c()
for(sh in shape_levels){
  g1 <- shape_df$Genus_Latin[shape_df$Morphology == sh]
  g1 <- g1[!is.na(g1) & g1 != ""]
  genus_order <- c(genus_order, g1)
}
genus_order <- unique(genus_order)

missing_g <- setdiff(unique(as.character(genus_cells_mean$Genus_Latin)), genus_order)
if(length(missing_g) > 0) genus_order <- c(genus_order, sort(missing_g))
genus_order <- unique(genus_order)

genus_cells_mean      <- add_shape_info(genus_cells_mean,      shape_df, shape_levels, genus_order)
genus_percent_mean    <- add_shape_info(genus_percent_mean,    shape_df, shape_levels, genus_order)
genus_bv_mean         <- add_shape_info(genus_bv_mean,         shape_df, shape_levels, genus_order)
genus_bv_percent_mean <- add_shape_info(genus_bv_percent_mean, shape_df, shape_levels, genus_order)

heading_map <- setNames(paste0("<b>", shape_levels, "</b>"), shape_levels)

legend_core <- unique(genus_cells_mean[, c("shape_level", "Genus_Latin", "FillLegend")])
legend_core$shape_level <- factor(legend_core$shape_level, levels = shape_levels)
legend_core$Genus_Latin <- factor(as.character(legend_core$Genus_Latin), levels = genus_order)
legend_core <- legend_core[order(legend_core$shape_level, legend_core$Genus_Latin), ]

legend_breaks <- c()
for(sh in shape_levels){
  idx <- which(legend_core$shape_level == sh)
  if(length(idx) == 0) next
  
  legend_breaks <- c(
    legend_breaks,
    paste0("HEADER | ", sh),
    as.character(legend_core$FillLegend[idx])
  )
}

legend_labels <- sapply(legend_breaks, function(b){
  if(grepl("^HEADER \\|", b)){
    sh <- sub("^HEADER \\|\\s*", "", b)
    heading_map[[sh]]
  } else {
    genus <- sub(".*\\|\\s*", "", b)
    paste0("<i>", genus, "</i>")
  }
})

legend_values <- setNames(rep("transparent", length(legend_breaks)), legend_breaks)
for(i in seq_len(nrow(legend_core))){
  g <- trimws(as.character(legend_core$Genus_Latin[i]))
  k <- as.character(legend_core$FillLegend[i])
  
  if(!is.na(genus_cols[g]) && genus_cols[g] != ""){
    legend_values[k] <- genus_cols[g]
  } else {
    legend_values[k] <- "grey80"
  }
}

genus_cells_mean$FillLegend      <- factor(genus_cells_mean$FillLegend,      levels = legend_breaks)
genus_percent_mean$FillLegend    <- factor(genus_percent_mean$FillLegend,    levels = legend_breaks)
genus_bv_mean$FillLegend         <- factor(genus_bv_mean$FillLegend,         levels = legend_breaks)
genus_bv_percent_mean$FillLegend <- factor(genus_bv_percent_mean$FillLegend, levels = legend_breaks)

################################################################################
# 6) EXPORT REPLICATION-LEVEL CYANOBACTERIA RATIOS
################################################################################
keep_idx_all <-
  (df$Treatment == "Initial") |
  (df$Day == 2 & df$Treatment %in% sig_trt_levels) |
  (df$Day == 6 & df$Treatment %in% sig_trt_levels)

df_ratio <- df[keep_idx_all, , drop = FALSE]

df_ratio$Panel <- NA_character_
df_ratio$Panel[df_ratio$Treatment == "Initial"] <- "Initial"
df_ratio$Panel[df_ratio$Day == 2 & df_ratio$Treatment %in% sig_trt_levels] <-
  paste0(df_ratio$Treatment[df_ratio$Day == 2 & df_ratio$Treatment %in% sig_trt_levels], "_Day2")
df_ratio$Panel[df_ratio$Day == 6 & df_ratio$Treatment %in% sig_trt_levels] <-
  paste0(df_ratio$Treatment[df_ratio$Day == 6 & df_ratio$Treatment %in% sig_trt_levels], "_Day6")

df_ratio$Panel <- factor(df_ratio$Panel, levels = panel_levels)
df_ratio <- df_ratio[!is.na(df_ratio$Panel), , drop = FALSE]
df_ratio <- add_panel_layout(df_ratio)

total_algae_rep <- ddply(df_ratio, .(Panel, Day_group, Treatment_clean, Treatment, Replication), function(x){
  data.frame(Total_algae_cells = sum(x$Cells_mL, na.rm = TRUE))
})

cyano_rep_ratio <- ddply(df_ratio[df_ratio$Latin_phylum == "Cyanophyta", ],
                         .(Panel, Day_group, Treatment_clean, Treatment, Replication), function(x){
                           data.frame(Cyano_cells = sum(x$Cells_mL, na.rm = TRUE))
                         })

fila_rep_ratio <- ddply(df_ratio[df_ratio$Latin_phylum == "Cyanophyta" &
                                   !is.na(df_ratio$Morphology) &
                                   df_ratio$Morphology == "Filamentous", ],
                        .(Panel, Day_group, Treatment_clean, Treatment, Replication), function(x){
                          data.frame(Filamentous_cyano_cells = sum(x$Cells_mL, na.rm = TRUE))
                        })

ratio_rep_df <- merge(total_algae_rep, cyano_rep_ratio,
                      by = c("Panel", "Day_group", "Treatment_clean", "Treatment", "Replication"),
                      all.x = TRUE)
ratio_rep_df <- merge(ratio_rep_df, fila_rep_ratio,
                      by = c("Panel", "Day_group", "Treatment_clean", "Treatment", "Replication"),
                      all.x = TRUE)

ratio_rep_df$Cyano_cells[is.na(ratio_rep_df$Cyano_cells)] <- 0
ratio_rep_df$Filamentous_cyano_cells[is.na(ratio_rep_df$Filamentous_cyano_cells)] <- 0

ratio_rep_df$Cyano_pct_total_cells <- ifelse(
  ratio_rep_df$Total_algae_cells > 0,
  ratio_rep_df$Cyano_cells / ratio_rep_df$Total_algae_cells * 100,
  NA_real_
)

ratio_rep_df$Filamentous_cyano_pct_total_cells <- ifelse(
  ratio_rep_df$Total_algae_cells > 0,
  ratio_rep_df$Filamentous_cyano_cells / ratio_rep_df$Total_algae_cells * 100,
  NA_real_
)

ratio_rep_df <- ratio_rep_df[, c(
  "Panel", "Day_group", "Treatment", "Replication",
  "Total_algae_cells", "Cyano_cells", "Filamentous_cyano_cells",
  "Cyano_pct_total_cells", "Filamentous_cyano_pct_total_cells"
)]

ratio_rep_df$Treatment_clean <- gsub("_2$", "", ratio_rep_df$Treatment)
ratio_rep_df$P_char <- gsub("P", "", ratio_rep_df$Treatment_clean)
ratio_rep_df$P_conc <- as.numeric(ratio_rep_df$P_char)

write.xlsx(ratio_rep_df,
           "../Output_Data/XMGS09_replication_cyano_filamentous_ratio.xlsx",
           rowNames = FALSE)

################################################################################
# 7) BAR PLOT TABLES
################################################################################
genus_cells_bar <- add_panel_layout(genus_cells_mean)
genus_prop_bar  <- add_panel_layout(genus_percent_mean)
genus_bv_bar    <- add_panel_layout(genus_bv_mean)
genus_bv_prop   <- add_panel_layout(genus_bv_percent_mean)

bar_total_df <- cyano_mean[, c("Panel", "Total_mean", "Total_sd")]
bar_total_df <- add_panel_layout(bar_total_df)

bar_bv_total_df <- cyano_bv_mean[, c("Panel", "Total_mean", "Total_sd")]
bar_bv_total_df <- add_panel_layout(bar_bv_total_df)

bar_stack_top <- ddply(genus_cells_bar, .(Panel, Day_group, Treatment_clean), function(x){
  data.frame(Stack_mean = sum(x$Mean_value, na.rm = TRUE))
})
bar_stack_top <- merge(bar_stack_top, bar_total_df[, c("Panel", "Total_sd")],
                       by = "Panel", all.x = TRUE)
bar_stack_top$ymin <- bar_stack_top$Stack_mean
bar_stack_top$ymax <- bar_stack_top$Stack_mean + bar_stack_top$Total_sd

bar_bv_stack_top <- ddply(genus_bv_bar, .(Panel, Day_group, Treatment_clean), function(x){
  data.frame(Stack_mean = sum(x$Mean_value, na.rm = TRUE))
})
bar_bv_stack_top <- merge(bar_bv_stack_top, bar_bv_total_df[, c("Panel", "Total_sd")],
                          by = "Panel", all.x = TRUE)
bar_bv_stack_top$ymin <- bar_bv_stack_top$Stack_mean
bar_bv_stack_top$ymax <- bar_bv_stack_top$Stack_mean + bar_bv_stack_top$Total_sd

################################################################################
# 8) SIGNIFICANCE LETTERS FOR TOTAL CYANOBACTERIA
################################################################################
cell_letters <- get_sig_letters(cells_obj$total_rep)
bv_letters   <- get_sig_letters(bv_obj$total_rep)

cell_letter_y <- merge(
  bar_stack_top[, c("Panel", "Day_group", "Treatment_clean", "ymax")],
  cell_letters,
  by = c("Day_group", "Treatment_clean"),
  all.x = FALSE
)
cell_letter_y$y <- cell_letter_y$ymax + 0.04 * max(bar_stack_top$ymax, na.rm = TRUE)

bv_letter_y <- merge(
  bar_bv_stack_top[, c("Panel", "Day_group", "Treatment_clean", "ymax")],
  bv_letters,
  by = c("Day_group", "Treatment_clean"),
  all.x = FALSE
)
bv_letter_y$y <- bv_letter_y$ymax + 0.04 * max(bar_bv_stack_top$ymax, na.rm = TRUE)

################################################################################
# 9) Y LIMITS
################################################################################
y_max_use <- max(cell_letter_y$y, na.rm = TRUE)
if(!is.finite(y_max_use) || y_max_use <= 0) y_max_use <- 1
y_max_use <- y_max_use * 1.06

y_max_bv <- max(bv_letter_y$y, na.rm = TRUE)
if(!is.finite(y_max_bv) || y_max_bv <= 0) y_max_bv <- 1
y_max_bv <- y_max_bv * 1.06

################################################################################
# 10) THEMES
################################################################################
theme_pub_bar <- theme_classic(base_size = 18, base_family = "sans") +
  theme(
    text = element_text(family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"),
    axis.text.y = element_text(family = "sans"),
    axis.title.y = element_text(family = "sans", size = 15),
    axis.title.x = element_text(family = "sans", size = 18),
    strip.text  = element_text(family = "sans"),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    legend.position = "none",
    panel.spacing.x = unit(3, "mm")
  )

theme_pub_prop <- theme_pub_bar

################################################################################
# 11) CELLS STACKED BAR WITH SIGNIFICANCE LETTERS
################################################################################
p_bar <- ggplot(genus_cells_bar,
                aes(x = Treatment_clean, y = Mean_value, fill = FillLegend)) +
  geom_col(width = 0.78, color = "black", linewidth = 0.3) +
  geom_errorbar(
    data = bar_stack_top,
    aes(x = Treatment_clean, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    width = 0.20,
    linewidth = 0.5,
    color = "black",
    na.rm = TRUE
  ) +
  geom_text(
    data = cell_letter_y,
    aes(x = Treatment_clean, y = y, label = Letters),
    inherit.aes = FALSE,
    size = 7,
    family = "sans"
  ) +
  scale_fill_manual(
    values = legend_values,
    breaks = legend_breaks,
    labels = legend_labels,
    na.value = "grey80",
    drop = FALSE
  ) +
  facet_grid(. ~ Day_group, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, y_max_use), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = expression(atop("Cyanobacterial cell concentration", "(cells mL"^-1*")"))
  ) +
  theme_pub_bar

ggsave(
  "../Output_Figures/Cells_cyano_genus_bar.jpg",
  p_bar, width = 8.5, height = 5.2, dpi = 300
)

################################################################################
# 12) SEPARATE LEGEND
################################################################################
legend_plot_df <- genus_cells_bar
legend_plot_df$legend_x <- 1

p_legend <- ggplot(legend_plot_df, aes(x = legend_x, y = Mean_value, fill = FillLegend)) +
  geom_col() +
  scale_fill_manual(
    values = legend_values,
    breaks = legend_breaks,
    labels = legend_labels,
    na.value = "grey80",
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(
      ncol = length(shape_levels),
      byrow = FALSE,
      override.aes = list(colour = NA),
      keywidth  = unit(5.5, "mm"),
      keyheight = unit(5.5, "mm")
    )
  ) +
  theme_void(base_family = "sans") +
  theme(
    text = element_text(family = "sans"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = ggtext::element_markdown(size = 15, family = "sans"),
    legend.key.width  = unit(6.5, "mm"),
    legend.key.height = unit(6.5, "mm"),
    legend.spacing.x  = unit(1.5, "mm"),
    legend.spacing.y  = unit(1.0, "mm"),
    legend.box.spacing = unit(1.5, "mm")
  )

legend_bar <- cowplot::get_legend(p_legend)

ggsave(
  "../Output_Figures/Cyano_genus_legend.jpg",
  legend_bar, width = 12, height = 1.5, dpi = 300, bg = "white"
)

################################################################################
# 13) CELLS PERCENT STACKED BAR
################################################################################
genus_prop_bar$Percent <- genus_prop_bar$Percent_fix

p_genus_percent <- ggplot(
  genus_prop_bar,
  aes(x = Treatment_clean, y = Percent, fill = FillLegend)
) +
  geom_col(position = "stack", width = 0.78, color = "black", linewidth = 0.3) +
  facet_grid(. ~ Day_group, scales = "free_x", space = "free_x") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(
    values = legend_values,
    breaks = legend_breaks,
    labels = legend_labels,
    na.value = "grey80",
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = "Genus-level proportion of\ncyanobacterial cells (%)"
  ) +
  theme_pub_prop

ggsave(
  "../Output_Figures/Cells_cyano_genus_Percent_bar.jpg",
  p_genus_percent, width = 8.5, height = 5.2, dpi = 300
)

################################################################################
# 14) BIOVOLUME STACKED BAR WITH SIGNIFICANCE LETTERS
################################################################################
p_bv_bar <- ggplot(
  genus_bv_bar,
  aes(x = Treatment_clean, y = Mean_value, fill = FillLegend)
) +
  geom_col(width = 0.78, color = "black", linewidth = 0.3) +
  geom_errorbar(
    data = bar_bv_stack_top,
    aes(x = Treatment_clean, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    width = 0.20,
    linewidth = 0.5,
    color = "black",
    na.rm = TRUE
  ) +
  geom_text(
    data = bv_letter_y,
    aes(x = Treatment_clean, y = y, label = Letters),
    inherit.aes = FALSE,
    size = 7,
    family = "sans"
  ) +
  scale_fill_manual(
    values = legend_values,
    breaks = legend_breaks,
    labels = legend_labels,
    na.value = "grey80",
    drop = FALSE
  ) +
  facet_grid(. ~ Day_group, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, y_max_bv), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = expression(atop("Cyanobacterial biovolume", "(mm"^3~L^-1*")"))
  ) +
  theme_pub_bar

ggsave(
  "../Output_Figures/Biovolume_cyano_genus_bar.jpg",
  p_bv_bar, width = 8.5, height = 5.2, dpi = 300
)

################################################################################
# 15) BIOVOLUME PERCENT STACKED BAR
################################################################################
genus_bv_prop$Percent <- genus_bv_prop$Percent_fix

p_bv_percent <- ggplot(
  genus_bv_prop,
  aes(x = Treatment_clean, y = Percent, fill = FillLegend)
) +
  geom_col(position = "stack", width = 0.78, color = "black", linewidth = 0.3) +
  facet_grid(. ~ Day_group, scales = "free_x", space = "free_x") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(
    values = legend_values,
    breaks = legend_breaks,
    labels = legend_labels,
    na.value = "grey80",
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = "Genus-level proportion of\ncyanobacterial biovolume (%)"
  ) +
  theme_pub_prop

ggsave(
  "../Output_Figures/Biovolume_cyano_genus_Percent_bar.jpg",
  p_bv_percent, width = 8.5, height = 5.2, dpi = 300
)

################################################################################
# 16) EXPORT TABLES
################################################################################
write.xlsx(
  list(
    cyano_cell_total_rep = cells_obj$total_rep,
    cyano_cell_total_mean = cyano_mean,
    cyano_cell_genus_mean = genus_cells_mean,
    cyano_cell_genus_percent = genus_percent_mean,
    cyano_bv_total_rep = bv_obj$total_rep,
    cyano_bv_total_mean = cyano_bv_mean,
    cyano_bv_genus_mean = genus_bv_mean,
    cyano_bv_genus_percent = genus_bv_percent_mean,
    cell_letters = cell_letters,
    bv_letters = bv_letters,
    ratio_rep_df = ratio_rep_df
  ),
  "../Output_Data/Cyano_genus_barplot_summary_tables.xlsx",
  overwrite = TRUE
)

cat("Run successfully.\n")
