################################################################################
# XMGS09 P-gradient stacked bars
# Absolute values + percentage composition
# Unknown / unlisted phyla are kept as "Others"
################################################################################

library(openxlsx)
library(plyr)
library(ggplot2)
library(car)
library(agricolae)

################################################################################
# 0) Read data
################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

data <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
data <- as.data.frame(data)

data <- data[as.character(data$Treatment) != "0.005P_2", ]

data$Treatment       <- trimws(as.character(data$Treatment))
data$Latin_phylum    <- trimws(as.character(data$Latin_phylum))
data$Latin_species   <- trimws(as.character(data$Latin_species))
data$Replication     <- as.character(data$Replication)
data$Day             <- as.numeric(as.character(data$Day))
data$Cells_mL        <- as.numeric(as.character(data$Cells_mL))
data$Biovolume_mm3_L <- as.numeric(as.character(data$Biovolume_mm3_L))

################################################################################
# 1) Orders + colors
################################################################################
phylum_order <- c("Bacillariophyta",
                  "Chlorophyta",
                  "Chrysophyta",
                  "Cryptophyta",
                  "Cyanophyta",
                  "Dinophyta",
                  "Euglenophyta")

phylum_order2 <- c(phylum_order, "Others")

phylum_cols <- c(
  "Bacillariophyta" = "#F1C40F",
  "Chlorophyta"     = "#A8D98C",
  "Chrysophyta"     = "yellow",
  "Cryptophyta"     = "grey70",
  "Cyanophyta"      = "#A9D0E6",
  "Dinophyta"       = "#E84C3D",
  "Euglenophyta"    = "#F2A7D1",
  "Others"          = "grey95"
)

day_levels <- sort(unique(data$Day))
data$DayF <- factor(data$Day, levels = day_levels, labels = paste0("Day", day_levels))

theme_pub <- theme_classic(base_size = 24, base_family = "sans") +
  theme(
    text = element_text(family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title  = element_text(family = "sans"),
    axis.text   = element_text(family = "sans"),
    legend.text = element_text(family = "sans"),
    legend.title = element_text(family = "sans"),
    strip.text = element_text(family = "sans"),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

################################################################################
# 2) Subset P-gradient treatments
################################################################################
trt_levels_P <- c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P")

dfP <- data[data$Treatment %in% trt_levels_P, ]

dfP$Latin_phylum <- as.character(dfP$Latin_phylum)
dfP$Latin_phylum[is.na(dfP$Latin_phylum) | dfP$Latin_phylum == ""] <- "Others"
dfP$Latin_phylum[!dfP$Latin_phylum %in% phylum_order] <- "Others"

dfP$Treatment    <- factor(dfP$Treatment, levels = trt_levels_P)
dfP$Latin_phylum <- factor(dfP$Latin_phylum, levels = phylum_order2)
dfP$DayF         <- factor(dfP$DayF, levels = paste0("Day", day_levels))

################################################################################
# 3) Cell concentration
################################################################################
cell_rep_P <- ddply(dfP, .(Treatment, Replication, Latin_phylum, DayF), summarise,
                    Value = sum(Cells_mL, na.rm = TRUE))

cell_stat_P <- ddply(cell_rep_P, .(Treatment, Latin_phylum, DayF), summarise,
                     Mean = mean(Value, na.rm = TRUE),
                     SD   = ifelse(sum(!is.na(Value)) > 1,
                                   sd(Value, na.rm = TRUE), NA_real_),
                     n    = sum(!is.na(Value)))

cell_total_rep_P <- ddply(cell_rep_P, .(Treatment, Replication, DayF), summarise,
                          Total = sum(Value, na.rm = TRUE))

cell_total_stat_P <- ddply(cell_total_rep_P, .(Treatment, DayF), summarise,
                           Total_mean = mean(Total, na.rm = TRUE),
                           Total_sd   = ifelse(sum(!is.na(Total)) > 1,
                                               sd(Total, na.rm = TRUE), NA_real_),
                           n          = sum(!is.na(Total)))

cell_stack_top_P <- ddply(cell_stat_P, .(Treatment, DayF), summarise,
                          Stack_mean = sum(Mean, na.rm = TRUE))

cell_err_P <- merge(cell_stack_top_P,
                    cell_total_stat_P[, c("Treatment", "DayF", "Total_sd")],
                    by = c("Treatment", "DayF"), all.x = TRUE)

cell_err_P$ymin <- cell_err_P$Stack_mean
cell_err_P$ymax <- cell_err_P$Stack_mean + cell_err_P$Total_sd

################################################################################
# 4) Biovolume
################################################################################
bio_rep_P <- ddply(dfP, .(Treatment, Replication, Latin_phylum, DayF), summarise,
                   Value = sum(Biovolume_mm3_L, na.rm = TRUE))

bio_stat_P <- ddply(bio_rep_P, .(Treatment, Latin_phylum, DayF), summarise,
                    Mean = mean(Value, na.rm = TRUE),
                    SD   = ifelse(sum(!is.na(Value)) > 1,
                                  sd(Value, na.rm = TRUE), NA_real_),
                    n    = sum(!is.na(Value)))

bio_total_rep_P <- ddply(bio_rep_P, .(Treatment, Replication, DayF), summarise,
                         Total = sum(Value, na.rm = TRUE))

bio_total_stat_P <- ddply(bio_total_rep_P, .(Treatment, DayF), summarise,
                          Total_mean = mean(Total, na.rm = TRUE),
                          Total_sd   = ifelse(sum(!is.na(Total)) > 1,
                                              sd(Total, na.rm = TRUE), NA_real_),
                          n          = sum(!is.na(Total)))

bio_stack_top_P <- ddply(bio_stat_P, .(Treatment, DayF), summarise,
                         Stack_mean = sum(Mean, na.rm = TRUE))

bio_err_P <- merge(bio_stack_top_P,
                   bio_total_stat_P[, c("Treatment", "DayF", "Total_sd")],
                   by = c("Treatment", "DayF"), all.x = TRUE)

bio_err_P$ymin <- bio_err_P$Stack_mean
bio_err_P$ymax <- bio_err_P$Stack_mean + bio_err_P$Total_sd

################################################################################
# 5) Species richness
################################################################################
rich_df_P <- dfP[!is.na(dfP$Latin_species) &
                   trimws(as.character(dfP$Latin_species)) != "" &
                   !is.na(dfP$Cells_mL) &
                   dfP$Cells_mL > 0, ]

rich_rep_P <- ddply(rich_df_P, .(Treatment, Replication, Latin_phylum, DayF), summarise,
                    Value = length(unique(Latin_species)))

rich_stat_P <- ddply(rich_rep_P, .(Treatment, Latin_phylum, DayF), summarise,
                     Mean = mean(Value, na.rm = TRUE),
                     SD   = ifelse(sum(!is.na(Value)) > 1,
                                   sd(Value, na.rm = TRUE), NA_real_),
                     n    = sum(!is.na(Value)))

rich_total_rep_P <- ddply(rich_rep_P, .(Treatment, Replication, DayF), summarise,
                          Total = sum(Value, na.rm = TRUE))

rich_total_stat_P <- ddply(rich_total_rep_P, .(Treatment, DayF), summarise,
                           Total_mean = mean(Total, na.rm = TRUE),
                           Total_sd   = ifelse(sum(!is.na(Total)) > 1,
                                               sd(Total, na.rm = TRUE), NA_real_),
                           n          = sum(!is.na(Total)))

rich_stack_top_P <- ddply(rich_stat_P, .(Treatment, DayF), summarise,
                          Stack_mean = sum(Mean, na.rm = TRUE))

rich_err_P <- merge(rich_stack_top_P,
                    rich_total_stat_P[, c("Treatment", "DayF", "Total_sd")],
                    by = c("Treatment", "DayF"), all.x = TRUE)

rich_err_P$ymin <- rich_err_P$Stack_mean
rich_err_P$ymax <- rich_err_P$Stack_mean + rich_err_P$Total_sd

################################################################################
# 6) Significance letters for total values
################################################################################
sig_trt_levels <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")
sig_days <- c("Day2", "Day6")

get_letters <- function(df, value_col) {
  out <- data.frame()
  
  for (d in sig_days) {
    tmp <- df[df$DayF == d & df$Treatment %in% sig_trt_levels, ]
    tmp$Treatment <- factor(as.character(tmp$Treatment), levels = sig_trt_levels)
    
    fit <- aov(tmp[[value_col]] ~ Treatment, data = tmp)
    lsd <- LSD.test(fit, "Treatment", p.adj = "none")
    
    one <- data.frame(Treatment = rownames(lsd$groups),
                      Letters = as.character(lsd$groups$groups),
                      DayF = d,
                      stringsAsFactors = FALSE)
    
    out <- rbind(out, one)
  }
  
  out
}

sig_cells_total <- get_letters(cell_total_rep_P, "Total")
sig_bio_total   <- get_letters(bio_total_rep_P, "Total")

rich_day2 <- rich_total_rep_P[rich_total_rep_P$DayF == "Day2" &
                                rich_total_rep_P$Treatment %in% sig_trt_levels, ]

rich_day2$Treatment <- factor(as.character(rich_day2$Treatment), levels = sig_trt_levels)

fit_rich_day2 <- aov(Total ~ Treatment, data = rich_day2)
lsd_rich_day2 <- LSD.test(fit_rich_day2, "Treatment", p.adj = "none")

rich_day2_letters <- data.frame(
  Treatment = rownames(lsd_rich_day2$groups),
  Letters = as.character(lsd_rich_day2$groups$groups),
  DayF = "Day2",
  stringsAsFactors = FALSE
)

rich_day6_letters <- data.frame(
  Treatment = sig_trt_levels,
  Letters = c("b", "b", "b", "ab", "a"),
  DayF = "Day6",
  stringsAsFactors = FALSE
)

sig_rich_total <- rbind(rich_day2_letters, rich_day6_letters)

cell_letter_y <- merge(ddply(cell_err_P, .(Treatment, DayF), summarise,
                             y = max(ymax, na.rm = TRUE)),
                       sig_cells_total,
                       by = c("Treatment", "DayF"))

cell_letter_y$y <- cell_letter_y$y + 0.05 * max(cell_letter_y$y, na.rm = TRUE)

bio_letter_y <- merge(ddply(bio_err_P, .(Treatment, DayF), summarise,
                            y = max(ymax, na.rm = TRUE)),
                      sig_bio_total,
                      by = c("Treatment", "DayF"))

bio_letter_y$y <- bio_letter_y$y + 0.05 * max(bio_letter_y$y, na.rm = TRUE)

rich_letter_y <- merge(ddply(rich_err_P, .(Treatment, DayF), summarise,
                             y = max(ymax, na.rm = TRUE)),
                       sig_rich_total,
                       by = c("Treatment", "DayF"))

rich_letter_y$y <- rich_letter_y$y + 0.05 * max(rich_letter_y$y, na.rm = TRUE)

################################################################################
# 7) Absolute stacked bar plots
################################################################################
cell_ylim_max <- max(cell_letter_y$y, na.rm = TRUE) * 1.03
bio_ylim_max  <- max(bio_letter_y$y, na.rm = TRUE) * 1.03
rich_ylim_max <- max(rich_letter_y$y, na.rm = TRUE) * 1.03

p_cells_stack <- ggplot(cell_stat_P, aes(x = Treatment, y = Mean, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  geom_errorbar(data = cell_err_P,
                aes(x = Treatment, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                width = 0.22, linewidth = 0.5, color = "black", na.rm = TRUE) +
  geom_text(data = cell_letter_y,
            aes(x = Treatment, y = y, label = Letters),
            inherit.aes = FALSE,
            size = 8, family = "sans") +
  facet_grid(. ~ DayF, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, cell_ylim_max), expand = c(0, 0)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order2, drop = FALSE) +
  labs(x = NULL,
       y = expression("Cell concentration (cells mL"^-1*")"),
       fill = NULL) +
  theme_pub

ggsave("../Output_Figures/Cells_total_bar_with_sig.jpg",
       plot = p_cells_stack, width = 8.5, height = 5.2, dpi = 300)

p_bio_stack <- ggplot(bio_stat_P, aes(x = Treatment, y = Mean, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  geom_errorbar(data = bio_err_P,
                aes(x = Treatment, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                width = 0.22, linewidth = 0.5, color = "black", na.rm = TRUE) +
  geom_text(data = bio_letter_y,
            aes(x = Treatment, y = y, label = Letters),
            inherit.aes = FALSE,
            size = 8, family = "sans") +
  facet_grid(. ~ DayF, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, bio_ylim_max), expand = c(0, 0)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order2, drop = FALSE) +
  labs(x = NULL,
       y = expression("Biovolume ("*mm^3*" L"^-1*")"),
       fill = NULL) +
  theme_pub

ggsave("../Output_Figures/Biovolume_total_bar_with_sig.jpg",
       plot = p_bio_stack, width = 8.5, height = 5.2, dpi = 300)

p_richness <- ggplot(rich_stat_P, aes(x = Treatment, y = Mean, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  geom_errorbar(data = rich_err_P,
                aes(x = Treatment, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                width = 0.22, linewidth = 0.5, color = "black", na.rm = TRUE) +
  geom_text(data = rich_letter_y,
            aes(x = Treatment, y = y, label = Letters),
            inherit.aes = FALSE,
            size = 8, family = "sans") +
  facet_grid(. ~ DayF, scales = "free_x", space = "free_x") +
  scale_y_continuous(limits = c(0, rich_ylim_max), expand = c(0, 0)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order2, drop = FALSE) +
  labs(x = NULL,
       y = "Species richness",
       fill = NULL) +
  theme_pub

ggsave("../Output_Figures/Species_richness_with_sig.jpg",
       plot = p_richness, width = 8.5, height = 5.2, dpi = 300)

################################################################################
# 8) Percentage composition plots
# Calculate percentages within each replicate first, then average
################################################################################

### Cell percentage
cell_keys <- unique(cell_total_rep_P[, c("Treatment", "Replication", "DayF")])

cell_all <- merge(cell_keys,
                  data.frame(Latin_phylum = phylum_order2, stringsAsFactors = FALSE),
                  by = NULL)

cell_rep2 <- merge(cell_all, cell_rep_P,
                   by = c("Treatment", "Replication", "DayF", "Latin_phylum"),
                   all.x = TRUE)

cell_rep2$Value[is.na(cell_rep2$Value)] <- 0

cell_total2 <- ddply(cell_rep2, .(Treatment, Replication, DayF), summarise,
                     Total = sum(Value, na.rm = TRUE))

cell_rep2 <- merge(cell_rep2, cell_total2,
                   by = c("Treatment", "Replication", "DayF"),
                   all.x = TRUE)

cell_rep2$Percent <- cell_rep2$Value / cell_rep2$Total * 100

cell_pct_P <- ddply(cell_rep2, .(Treatment, Latin_phylum, DayF), summarise,
                    Percent = mean(Percent, na.rm = TRUE))

cell_pct_P$Treatment    <- factor(cell_pct_P$Treatment, levels = trt_levels_P)
cell_pct_P$Latin_phylum <- factor(cell_pct_P$Latin_phylum, levels = phylum_order2)
cell_pct_P$DayF         <- factor(cell_pct_P$DayF, levels = levels(dfP$DayF))

p_cells_pct <- ggplot(cell_pct_P, aes(x = Treatment, y = Percent, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  facet_grid(. ~ DayF, scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order2, drop = FALSE) +
  labs(x = NULL,
       y = "Phylum-level proportion\nof cell concentration (%)",
       fill = NULL) +
  theme_pub

ggsave("../Output_Figures/Cells_percent_bar.jpg",
       plot = p_cells_pct, width = 8.5, height = 5.2, dpi = 300)

### Biovolume percentage
bio_keys <- unique(bio_total_rep_P[, c("Treatment", "Replication", "DayF")])

bio_all <- merge(bio_keys,
                 data.frame(Latin_phylum = phylum_order2, stringsAsFactors = FALSE),
                 by = NULL)

bio_rep2 <- merge(bio_all, bio_rep_P,
                  by = c("Treatment", "Replication", "DayF", "Latin_phylum"),
                  all.x = TRUE)

bio_rep2$Value[is.na(bio_rep2$Value)] <- 0

bio_total2 <- ddply(bio_rep2, .(Treatment, Replication, DayF), summarise,
                    Total = sum(Value, na.rm = TRUE))

bio_rep2 <- merge(bio_rep2, bio_total2,
                  by = c("Treatment", "Replication", "DayF"),
                  all.x = TRUE)

bio_rep2$Percent <- bio_rep2$Value / bio_rep2$Total * 100

bio_pct_P <- ddply(bio_rep2, .(Treatment, Latin_phylum, DayF), summarise,
                   Percent = mean(Percent, na.rm = TRUE))

bio_pct_P$Treatment    <- factor(bio_pct_P$Treatment, levels = trt_levels_P)
bio_pct_P$Latin_phylum <- factor(bio_pct_P$Latin_phylum, levels = phylum_order2)
bio_pct_P$DayF         <- factor(bio_pct_P$DayF, levels = levels(dfP$DayF))

p_bio_pct <- ggplot(bio_pct_P, aes(x = Treatment, y = Percent, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  facet_grid(. ~ DayF, scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                     expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order2, drop = FALSE) +
  labs(x = NULL,
       y = "Phylum-level proportion\nof biovolume (%)",
       fill = NULL) +
  theme_pub

ggsave("../Output_Figures/Biovolume_percent_bar.jpg",
       plot = p_bio_pct, width = 8.5, height = 5.2, dpi = 300)

################################################################################
# 9) Export summary tables
################################################################################
write.xlsx(
  list(
    cell_rep_P = cell_rep_P,
    cell_stat_P = cell_stat_P,
    cell_total_rep_P = cell_total_rep_P,
    cell_total_stat_P = cell_total_stat_P,
    cell_rep_percent = cell_rep2,
    cell_pct_P = cell_pct_P,
    bio_rep_P = bio_rep_P,
    bio_stat_P = bio_stat_P,
    bio_total_rep_P = bio_total_rep_P,
    bio_total_stat_P = bio_total_stat_P,
    bio_rep_percent = bio_rep2,
    bio_pct_P = bio_pct_P,
    rich_rep_P = rich_rep_P,
    rich_stat_P = rich_stat_P,
    rich_total_rep_P = rich_total_rep_P,
    rich_total_stat_P = rich_total_stat_P
  ),
  "../Output_Data/Summary_tables_for_barplots.xlsx",
  overwrite = TRUE
)

cat("Run successfully.\n")
