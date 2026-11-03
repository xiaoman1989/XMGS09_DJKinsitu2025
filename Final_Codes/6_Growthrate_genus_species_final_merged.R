################################################################################
### Growth-rate trend figures at genus and species levels
### Species-level colors inherit the corresponding genus-level colors.
################################################################################

library(openxlsx)
library(ggplot2)
library(grid)
library(patchwork)
################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

input_file <- "../Input_Data/Growthrate_summary.xlsx"
cell_file  <- "../Output_Data/Cell_5_modify_tidy_calibrated.xlsx"
out_dir    <- "../Output_Figures"

parse_sheet <- function(sheet_name) {
  dat <- read.xlsx(input_file, sheet = sheet_name)
  value_cols <- setdiff(names(dat), c("Taxa", "Group"))
  
  rows <- list()
  for (col in value_cols) {
    parts <- strsplit(col, "_")[[1]]
    rows[[length(rows) + 1]] <- data.frame(
      level = sheet_name,
      Taxa = dat$Taxa,
      Group = dat$Group,
      DIP = as.numeric(sub("P$", "", parts[1])),
      period = paste(parts[-1], collapse = "_"),
      growth_rate = as.numeric(dat[[col]]),
      stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$growth_rate), ]
  out$period <- factor(out$period, levels = c("D0_2", "D2_6", "D0_6"))
  out
}

taxa_map <- c(
  "Pseudanabaena属" = "Pseudanabaena",
  "Chroococcus属" = "Chroococcus",
  "Chlorella属" = "Chlorella",
  "尖头藻属" = "Raphidiopsis",
  "Leptolyngbya sp." = "Leptolyngbya"
)

get_genus <- function(x) {
  x <- trimws(as.character(x))
  ifelse(x %in% names(taxa_map), taxa_map[x], sub("\\s+.*$", "", x))
}

period_labels <- c(
  "D0_2" = "Days 0-2: DIP pulses",
  "D2_6" = "Days 2-6: post-replacement",
  "D0_6" = "Days 0-6: net response"
)

selected_genera <- c(
  "Total algae",
  "Chlorella",
  "Chroococcus",
  "Pseudanabaena",
  "Leptolyngbya",
  "Raphidiopsis",
  "Synedra"
)

selected_genera_genus <- setdiff(selected_genera, "Total algae")

genus_cols <- c(
  "Total algae" = "#000000",
  "Chlorella" = "#2E8B57",
  "Chroococcus" = "#4C78A8",
  "Pseudanabaena" = "#E45756",
  "Leptolyngbya" = "#F58518",
  "Raphidiopsis" = "#7A5195",
  "Synedra" = "#666666"
)

genus_linetypes <- c(
  "Total algae" = "solid",
  "Chlorella" = "solid",
  "Chroococcus" = "longdash",
  "Pseudanabaena" = "dotdash",
  "Leptolyngbya" = "dashed",
  "Raphidiopsis" = "twodash",
  "Synedra" = "dotted"
)

genus_shapes <- c(
  "Total algae" = 23,
  "Chlorella" = 16,
  "Chroococcus" = 18,
  "Pseudanabaena" = 17,
  "Leptolyngbya" = 15,
  "Raphidiopsis" = 3,
  "Synedra" = 8
)

growth <- rbind(parse_sheet("species"), parse_sheet("genus"))
growth$Genus_plot <- get_genus(growth$Taxa)
growth$period_plot <- factor(period_labels[as.character(growth$period)], levels = period_labels)

genus <- growth[growth$level == "genus" & growth$Genus_plot %in% selected_genera_genus, ]
species <- growth[growth$level == "species" & growth$Genus_plot %in% selected_genera, ]

################################################################################
### Add total-algae growth rates to the species-level trend figure
################################################################################

calc_total_algae_growth <- function() {
  cell <- read.xlsx(cell_file, sheet = 1)
  cell <- as.data.frame(cell, stringsAsFactors = FALSE)
  cell$Treatment <- trimws(as.character(cell$Treatment))
  cell$Replication <- trimws(as.character(cell$Replication))
  cell$Day <- as.numeric(as.character(cell$Day))
  cell$Cells_mL <- as.numeric(as.character(cell$Cells_mL))
  
  keep_treat <- c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P")
  cell <- cell[cell$Treatment %in% keep_treat & cell$Day %in% c(0, 2, 6), , drop = FALSE]
  
  rep_cells <- aggregate(
    Cells_mL ~ Treatment + Replication + Day,
    data = cell,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  names(rep_cells)[names(rep_cells) == "Cells_mL"] <- "Total_cells"
  
  mean_cells <- aggregate(
    Total_cells ~ Treatment + Day,
    data = rep_cells,
    FUN = mean,
    na.rm = TRUE
  )
  
  get_mean <- function(treatment, day) {
    val <- mean_cells$Total_cells[mean_cells$Treatment == treatment & mean_cells$Day == day]
    if (length(val) == 0) return(NA_real_)
    val[1]
  }
  
  p_map <- c("0P" = 0, "0.005P" = 0.005, "0.01P" = 0.01, "0.025P" = 0.025, "0.05P" = 0.05)
  initial_mean <- get_mean("Initial", 0)
  
  rows <- list()
  for (trt in names(p_map)) {
    day2_mean <- get_mean(trt, 2)
    day6_mean <- get_mean(trt, 6)
    
    rows[[length(rows) + 1]] <- data.frame(
      level = "species",
      Taxa = "Total algae",
      Group = "Total algae",
      DIP = unname(p_map[trt]),
      period = factor("D0_2", levels = c("D0_2", "D2_6", "D0_6")),
      growth_rate = log(day2_mean / initial_mean) / 2,
      stringsAsFactors = FALSE
    )
    
    rows[[length(rows) + 1]] <- data.frame(
      level = "species",
      Taxa = "Total algae",
      Group = "Total algae",
      DIP = unname(p_map[trt]),
      period = factor("D2_6", levels = c("D0_2", "D2_6", "D0_6")),
      growth_rate = log(day6_mean / (day2_mean * 0.8)) / 4,
      stringsAsFactors = FALSE
    )
    
    rows[[length(rows) + 1]] <- data.frame(
      level = "species",
      Taxa = "Total algae",
      Group = "Total algae",
      DIP = unname(p_map[trt]),
      period = factor("D0_6", levels = c("D0_2", "D2_6", "D0_6")),
      growth_rate = log(day6_mean / initial_mean) / 6,
      stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(rbind, rows)
  out$Genus_plot <- "Total algae"
  out$period_plot <- factor(period_labels[as.character(out$period)], levels = period_labels)
  out
}

total_algae <- calc_total_algae_growth()
species <- rbind(species, total_algae)
genus <- rbind(genus, total_algae)

genus$Genus_plot <- factor(genus$Genus_plot, levels = selected_genera)
species$Genus_plot <- factor(species$Genus_plot, levels = selected_genera)
species$Species_plot <- factor(species$Taxa, levels = unique(species$Taxa))

genus_breaks <- selected_genera[
  selected_genera %in% unique(as.character(genus$Genus_plot))
]

display_taxon_label <- function(x) {
  ifelse(
    x == "Total algae", "Total phytoplankton",
    ifelse(x == "Green algae", "Chlorophyte", x)
  )
}

genus_legend_labels <- as.expression(lapply(
  genus_breaks,
  function(x) {
    display_x <- display_taxon_label(x)
    if (x %in% c("Total algae", "Green algae")) {
      bquote(plain(.(display_x)))
    } else {
      bquote(italic(.(display_x)))
    }
  }
))

species_label_key <- unique(species[, c("Genus_plot", "Species_plot")])
species_labels <- setNames(
  as.character(species_label_key$Species_plot),
  as.character(species_label_key$Genus_plot)
)
species_labels <- species_labels[!duplicated(names(species_labels))]
species_labels <- species_labels[selected_genera[selected_genera %in% names(species_labels)]]
species_legend_labels <- as.expression(lapply(
  unname(species_labels),
  function(x) {
    display_x <- display_taxon_label(x)
    if (x %in% c("Total algae", "Green algae")) {
      bquote(plain(.(display_x)))
    } else {
      bquote(italic(.(display_x)))
    }
  }
))

################################################################################
### Genus-level figure
################################################################################

p_genus <- ggplot(
  genus,
  aes(
    x = DIP,
    y = growth_rate,
    color = Genus_plot,
    group = Genus_plot,
    linetype = Genus_plot,
    shape = Genus_plot
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "grey60") +
  geom_line(linewidth = 0.85, lineend = "round") +
  geom_point(size = 2.4, stroke = 0.55) +
  facet_wrap(~ period_plot, nrow = 1) +
  scale_color_manual(
    values = genus_cols,
    breaks = genus_breaks,
    labels = genus_legend_labels,
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = genus_linetypes,
    breaks = genus_breaks,
    labels = genus_legend_labels,
    drop = FALSE
  ) +
  scale_shape_manual(
    values = genus_shapes,
    breaks = genus_breaks,
    labels = genus_legend_labels,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = c(0, 0.005, 0.01, 0.025, 0.05),
    labels = c("0", "0.005", "0.01", "0.025", "0.05")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.10))) +
  theme_classic(base_size = 13, base_family = "sans") +
  labs(
    x = expression("DIP addition (mg L"^-1*")"),
    y = expression("Net growth rate (d"^-1*")"),
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        linetype = unname(genus_linetypes[genus_breaks]),
        shape = unname(genus_shapes[genus_breaks]),
        linewidth = 0.9,
        size = 2.6
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16, color = "black"),
    legend.key.width = unit(1.05, "cm"),
    legend.key.height = unit(0.42, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.45),
    strip.text = element_text(face = "bold", size = 13, margin = margin(4, 3, 4, 3)),
    axis.text.x = element_text(angle = 38, hjust = 1, vjust = 1, color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 13),
    axis.title = element_text(color = "black", size = 13),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.55),
    panel.spacing.x = unit(0.55, "lines"),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )

################################################################################
### Species-level figure
### Data are species-level values, but visual aesthetics are mapped by genus.
### This is appropriate here because each selected genus has only one species.
p_species <- ggplot(
  species,
  aes(
    x = DIP,
    y = growth_rate,
    color = Genus_plot,
    group = Genus_plot,
    linetype = Genus_plot,
    shape = Genus_plot
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "grey60") +
  geom_line(linewidth = 0.9, lineend = "round") +
  geom_point(size = 2.5, stroke = 0.6) +
  facet_wrap(~ period_plot, nrow = 1) +
  scale_color_manual(
  values = genus_cols,
  breaks = names(species_labels),
  labels = species_legend_labels,
  drop = TRUE
) +

scale_linetype_manual(
  values = genus_linetypes,
  breaks = names(species_labels),
  labels = species_legend_labels,
  drop = TRUE
) +

scale_shape_manual(
  values = genus_shapes,
  breaks = names(species_labels),
  labels = species_legend_labels,
  drop = TRUE
) +
  scale_x_continuous(
    breaks = c(0, 0.005, 0.01, 0.025, 0.05),
    labels = c("0", "0.005", "0.01", "0.025", "0.05")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.10))) +
  theme_classic(base_size = 13, base_family = "sans") +
  labs(
    x = expression("DIP addition (mg L"^-1*")"),
    y = expression("Net growth rate (d"^-1*")"),
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        linetype = unname(genus_linetypes[names(species_labels)]),
        shape = unname(genus_shapes[names(species_labels)]),
        linewidth = 0.95,
        size = 2.7
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16, color = "black"),
    legend.key.width = unit(1.05, "cm"),
    legend.key.height = unit(0.42, "cm"),
    legend.spacing.x = unit(0.25, "cm"),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.45),
    strip.text = element_text(face = "bold", size = 13, margin = margin(4, 3, 4, 3)),
    axis.text.x = element_text(angle = 38, hjust = 1, vjust = 1, color = "black", size = 13),
    axis.text.y = element_text(color = "black", size = 13),
    axis.title = element_text(color = "black", size = 13),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.55),
    panel.spacing.x = unit(0.55, "lines"),
    plot.margin = margin(5.5, 8, 5.5, 5.5)
  )

################################################################################
### Additional figures: keep only D0-2 and D2-6
################################################################################

genus_2period <- genus[as.character(genus$period) %in% c("D0_2", "D2_6"), ]
species_2period <- species[as.character(species$period) %in% c("D0_2", "D2_6"), ]

genus_2period$period_plot <- factor(
  period_labels[as.character(genus_2period$period)],
  levels = period_labels[c("D0_2", "D2_6")]
)

species_2period$period_plot <- factor(
  period_labels[as.character(species_2period$period)],
  levels = period_labels[c("D0_2", "D2_6")]
)

period_labels_short <- c(
  "D0_2" = "Days 0-2: DIP addition",
  "D2_6" = "Days 2-6: post-replacement"
)

genus_2period$period_short <- factor(
  period_labels_short[as.character(genus_2period$period)],
  levels = period_labels_short[c("D0_2", "D2_6")]
)

species_2period$period_short <- factor(
  period_labels_short[as.character(species_2period$period)],
  levels = period_labels_short[c("D0_2", "D2_6")]
)

p_genus_2period_right_legend <- p_genus %+% genus_2period +
  facet_wrap(~ period_plot, nrow = 1) +
  labs(x = NULL) +
  guides(
    color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      override.aes = list(
        linetype = unname(genus_linetypes[genus_breaks]),
        shape = unname(genus_shapes[genus_breaks]),
        linewidth = 0.9,
        size = 2.6
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    legend.box.just = "left",
    legend.justification = c(0, 0.5),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, -1),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5.5, 4, 8, 5.5)
  )

p_species_2period_right_legend <- p_species %+% species_2period +
  facet_wrap(~ period_plot, nrow = 1) +
  guides(
    color = guide_legend(
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      override.aes = list(
        linetype = unname(genus_linetypes[names(species_labels)]),
        shape = unname(genus_shapes[names(species_labels)]),
        linewidth = 0.95,
        size = 2.7
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    legend.box.just = "left",
    legend.justification = c(0, 0.5),
    legend.margin = margin(0, 0, 0, 0),
    strip.text = element_blank(),
    strip.background = element_blank(),
    plot.margin = margin(8, 4, 5.5, 5.5)
  )

p_growthrate_genus_species_2period_right_legend <-
  p_genus_2period_right_legend / p_species_2period_right_legend +
  plot_layout(heights = c(1, 1))

p_genus_2period_clean <- p_genus %+% genus_2period +
  facet_wrap(~ period_short, nrow = 1) +
  labs(x = NULL) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        linetype = unname(genus_linetypes[genus_breaks]),
        shape = unname(genus_shapes[genus_breaks]),
        linewidth = 0.9,
        size = 2.6
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    legend.key.width = unit(0.8, "cm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5.5, 8, 2, 5.5)
  )

p_species_2period_clean <- p_species %+% species_2period +
  facet_wrap(~ period_short, nrow = 1) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        linetype = unname(genus_linetypes[names(species_labels)]),
        shape = unname(genus_shapes[names(species_labels)]),
        linewidth = 0.95,
        size = 2.7
      )
    ),
    linetype = "none",
    shape = "none"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12, color = "black"),
    legend.key.width = unit(0.8, "cm"),
    plot.margin = margin(2, 8, 5.5, 5.5)
  )

p_growthrate_genus_species_2period_clean <-
  p_genus_2period_clean / p_species_2period_clean +
  plot_layout(heights = c(1, 1.05))

ggsave(file.path(out_dir, "Growthrate_genus.jpg"),
       p_genus, width = 10.8, height = 4.55, dpi = 600)

ggsave(file.path(out_dir, "Growthrate_species.jpg"),
       p_species, width = 10.8, height = 4.85, dpi = 600)

ggsave(file.path(out_dir, "Growthrate_genus_D0_2_D2_6.jpg"),
       p_genus_2period_clean, width = 8.8, height = 4.2, dpi = 600)

ggsave(file.path(out_dir, "Growthrate_species_D0_2_D2_6.jpg"),
       p_species_2period_clean, width = 8.8, height = 5.3, dpi = 600)

ggsave(file.path(out_dir, "Growthrate_genus_species_D0_2_D2_6_combined_right_legend.jpg"),
       p_growthrate_genus_species_2period_right_legend, width = 10.8, height = 6.6, dpi = 600)

ggsave(file.path(out_dir, "Growthrate_genus_species_D0_2_D2_6_combined_clean.jpg"),
       p_growthrate_genus_species_2period_clean, width = 9.4, height = 8.0, dpi = 600)

color_key <- data.frame(
  Genus = selected_genera,
  Color = genus_cols[selected_genera],
  In_genus_sheet = selected_genera %in% unique(as.character(genus$Genus_plot)),
  In_species_sheet = selected_genera %in% unique(as.character(species$Genus_plot)),
  stringsAsFactors = FALSE
)

write.xlsx(color_key, file.path(out_dir, "Growthrate_color_key.xlsx"), overwrite = TRUE)
