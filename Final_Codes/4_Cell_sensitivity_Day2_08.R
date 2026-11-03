################################################################################
# Packages
library(openxlsx)
library(plyr)
library(ggplot2)

################################################################################
# 0) Read data
################################################################################
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

data <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
data <- as.data.frame(data)

###remove data from 0.005P_2 treatment
data <- data[as.character(data$Treatment) != "0.005P_2",]
# Clean types
data$Treatment       <- trimws(as.character(data$Treatment))
data$Latin_phylum    <- trimws(as.character(data$Latin_phylum))
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

phylum_cols <- c(
  "Bacillariophyta" = "#F1C40F",
  "Chlorophyta"     = "#A8D98C",
  "Chrysophyta"     = "yellow",
  "Cryptophyta"     = "grey70",
  "Cyanophyta"      = "#A9D0E6",
  "Dinophyta"       = "#E84C3D",
  "Euglenophyta"    = "#F2A7D1"
)

################################################################################
# 2) Day factor
################################################################################
day_levels <- sort(unique(data$Day))
data$DayF <- factor(data$Day, levels = day_levels, labels = paste0("Day", day_levels))

################################################################################
# 3) Unified theme
################################################################################
theme_pub <- theme_classic(base_size = 24, base_family = "sans") +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"),
        axis.title  = element_text(family = "sans"),
        axis.text   = element_text(family = "sans"),
        legend.text = element_text(family = "sans"),
        legend.title = element_text(family = "sans"),
        strip.text = element_text(family = "sans"),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        legend.position = "none")

################################################################################
# 4) Subset for P-gradient treatments
################################################################################
trt_levels_P <- c("Initial","0P","0.005P","0.01P","0.025P","0.05P")
dfP <- data[data$Treatment %in% trt_levels_P, ]
dfP <- as.data.frame(dfP)

dfP <- dfP[dfP$Latin_phylum %in% phylum_order, ]

dfP$Treatment    <- factor(dfP$Treatment, levels = trt_levels_P)
dfP$Latin_phylum <- factor(dfP$Latin_phylum, levels = phylum_order)
dfP$DayF         <- factor(dfP$DayF, levels = paste0("Day", day_levels))

################################################################################
# 5) Sensitivity plots with Day2 original vs Day2×0.8 vs Day6 (custom x)
################################################################################
dfS <- dfP[dfP$DayF %in% c("Day0","Day2","Day6"), ]
dfS <- as.data.frame(dfS)

dfS_08 <- dfS[dfS$DayF == "Day2", ]
dfS_08$Cells_mL        <- dfS_08$Cells_mL * 0.8
dfS_08$Biovolume_mm3_L <- dfS_08$Biovolume_mm3_L * 0.8
dfS_08$DayF <- "Day2x0.8"

dfS_all <- rbind(dfS, dfS_08)

dfS_all$Xcat <- NA_character_
dfS_all$Xcat[dfS_all$Treatment == "Initial" & dfS_all$DayF == "Day0"] <- "Day 0 (Initial)"

make_label <- function(trt, dayflag){
  if(dayflag == "Day2")     return(paste0(trt, " (Day 2)"))
  if(dayflag == "Day2x0.8") return(paste0(trt, " (Day 2×0.8)"))
  if(dayflag == "Day6")     return(paste0(trt, " (Day 6)"))
  return(NA_character_)
}

idx_norm <- dfS_all$Treatment %in% c("0P","0.005P","0.01P","0.025P","0.05P") &
  dfS_all$DayF %in% c("Day2","Day2x0.8","Day6")

dfS_all$Xcat[idx_norm] <- mapply(make_label,
                                 trt = dfS_all$Treatment[idx_norm],
                                 dayflag = dfS_all$DayF[idx_norm])

dfS_all <- dfS_all[!is.na(dfS_all$Xcat), ]

x_levels <- c("Day 0 (Initial)",
              "0P (Day 2)","0P (Day 2×0.8)","0P (Day 6)",
              "0.005P (Day 2)","0.005P (Day 2×0.8)","0.005P (Day 6)",
              "0.01P (Day 2)","0.01P (Day 2×0.8)","0.01P (Day 6)",
              "0.025P (Day 2)","0.025P (Day 2×0.8)","0.025P (Day 6)",
              "0.05P (Day 2)","0.05P (Day 2×0.8)","0.05P (Day 6)"
)

dfS_all$Xcat <- factor(dfS_all$Xcat, levels = x_levels)
dfS_all$Latin_phylum <- factor(dfS_all$Latin_phylum, levels = phylum_order)

theme_pub_x <- theme_pub +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"))

################################################################################
# 6) CELLS stacked
################################################################################
cell_rep_S <- ddply(dfS_all, .(Xcat, Replication, Latin_phylum), summarise,
                    Value = sum(Cells_mL, na.rm = TRUE))

cell_stat_S <- ddply(cell_rep_S, .(Xcat, Latin_phylum), summarise,
                     Mean = mean(Value, na.rm = TRUE),
                     SD   = ifelse(sum(!is.na(Value)) > 1,
                                   sd(Value, na.rm = TRUE), NA_real_))

cell_total_rep_S <- ddply(cell_rep_S, .(Xcat, Replication), summarise,
                          Total = sum(Value, na.rm = TRUE))

cell_total_stat_S <- ddply(cell_total_rep_S, .(Xcat), summarise,
                           Total_sd = ifelse(sum(!is.na(Total)) > 1,
                                             sd(Total, na.rm = TRUE), NA_real_))

cell_stack_top_S <- ddply(cell_stat_S, .(Xcat), summarise,
                          Stack_mean = sum(Mean, na.rm = TRUE))

cell_err_S <- merge(cell_stack_top_S, cell_total_stat_S[, c("Xcat","Total_sd")],
                    by = "Xcat", all.x = TRUE)
cell_err_S$ymin <- cell_err_S$Stack_mean
cell_err_S$ymax <- cell_err_S$Stack_mean + cell_err_S$Total_sd

cell_stat_S$Xcat <- factor(cell_stat_S$Xcat, levels = x_levels)
cell_err_S$Xcat  <- factor(cell_err_S$Xcat, levels = x_levels)

p_cells_stack_S <- ggplot(cell_stat_S, aes(x = Xcat, y = Mean, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  geom_errorbar(data = cell_err_S,
                aes(x = Xcat, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                width = 0.22, linewidth = 0.5, color = "black", na.rm = TRUE) +
  geom_vline(xintercept = c(1.5, 4.5, 7.5, 10.5, 13.5),
             linetype = "dashed", linewidth = 0.6, colour = "black") +
  scale_y_continuous(limits = c(0, 65000), breaks = seq(0, 65000, by = 15000), expand = c(0, 0)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order, drop = FALSE) +
  labs(x = NULL,
       y = expression("Cell concentration (cells mL"^-1*")"),
       fill = NULL) +
  theme_pub_x
ggsave("../Output_Figures/Cells_total_stacked_Day2_vs_Day2x0.8_vs_Day6.jpg",
       plot = p_cells_stack_S, width = 12.5, height = 6.5, dpi = 300)

################################################################################
# 7) SENSITIVITY: BIOVOLUME stacked
################################################################################
bio_rep_S <- ddply(dfS_all, .(Xcat, Replication, Latin_phylum), summarise,
                   Value = sum(Biovolume_mm3_L, na.rm = TRUE))

bio_stat_S <- ddply(bio_rep_S, .(Xcat, Latin_phylum), summarise,
                    Mean = mean(Value, na.rm = TRUE),
                    SD   = ifelse(sum(!is.na(Value)) > 1,
                                  sd(Value, na.rm = TRUE), NA_real_))

bio_total_rep_S <- ddply(bio_rep_S, .(Xcat, Replication), summarise,
                         Total = sum(Value, na.rm = TRUE))

bio_total_stat_S <- ddply(bio_total_rep_S, .(Xcat), summarise,
                          Total_sd = ifelse(sum(!is.na(Total)) > 1,
                                            sd(Total, na.rm = TRUE), NA_real_))

bio_stack_top_S <- ddply(bio_stat_S, .(Xcat), summarise,
                         Stack_mean = sum(Mean, na.rm = TRUE))

bio_err_S <- merge(bio_stack_top_S,
                   bio_total_stat_S[, c("Xcat","Total_sd")],
                   by = "Xcat", all.x = TRUE)
bio_err_S$ymin <- bio_err_S$Stack_mean
bio_err_S$ymax <- bio_err_S$Stack_mean + bio_err_S$Total_sd

bio_stat_S$Xcat <- factor(bio_stat_S$Xcat, levels = x_levels)
bio_err_S$Xcat  <- factor(bio_err_S$Xcat, levels = x_levels)

p_bio_stack_S <- ggplot(bio_stat_S, aes(x = Xcat, y = Mean, fill = Latin_phylum)) +
  geom_col(position = "stack", width = 0.75, color = "black", linewidth = 0.3) +
  geom_errorbar(data = bio_err_S,
                aes(x = Xcat, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                width = 0.22, linewidth = 0.5, color = "black", na.rm = TRUE) +
  geom_vline(xintercept = c(1.5, 4.5, 7.5, 10.5, 13.5),
             linetype = "dashed", linewidth = 0.6, colour = "black") +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  scale_fill_manual(values = phylum_cols, breaks = phylum_order, drop = FALSE) +
  labs(x = NULL,
       y = expression("Biovolume ("*mm^3*" L"^-1*")"),
       fill = NULL) +
  theme_pub_x

ggsave("../Output_Figures/Biovolume_total_stacked_Day2_vs_Day2x0.8_vs_Day6.jpg",
       plot = p_bio_stack_S, width = 12.5, height = 6.5, dpi = 300)
