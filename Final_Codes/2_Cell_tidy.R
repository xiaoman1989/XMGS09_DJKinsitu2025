################################################################################
## 0) Packages + paths
################################################################################
library(openxlsx)
library(tidyr)
library(plyr)
library(readxl)

dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

dir.create("../Output_Data", showWarnings = FALSE, recursive = TRUE)

################################################################################
## 1) Helper functions
################################################################################
clean_key <- function(x){
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)   # NBSP
  x <- gsub("[[:space:]]+", " ", x)           
  x <- trimws(x)
  x
}

get_tank <- function(x){
  x <- as.character(x)
  as.integer(sub("#.*$", "", x))
}

get_treatment <- function(tank){
  trt <- rep("Unknown", length(tank))
  trt[tank %in% 1:4]   <- "Initial"
  trt[tank %in% 5:8]   <- "0P"
  trt[tank %in% 9:12]  <- "0.005P"
  trt[tank %in% 13:16] <- "0.01P"
  trt[tank %in% 17:20] <- "0.025P"
  trt[tank %in% 21:24] <- "0.05P"
  trt[tank %in% 25:28] <- "0P"
  trt[tank %in% 29:32] <- "0.005P"
  trt[tank %in% 33:36] <- "0.005P_2"
  trt[tank %in% 37:40] <- "0.01P"
  trt[tank %in% 41:44] <- "0.025P"
  trt[tank %in% 45:48] <- "0.05P"
  trt
}

get_replication <- function(tank){
  repn <- rep(NA_integer_, length(tank))
  repn[tank %in% 1:4]   <- tank[tank %in% 1:4]   - 0L
  repn[tank %in% 5:8]   <- tank[tank %in% 5:8]   - 4L
  repn[tank %in% 9:12]  <- tank[tank %in% 9:12]  - 8L
  repn[tank %in% 13:16] <- tank[tank %in% 13:16] - 12L
  repn[tank %in% 17:20] <- tank[tank %in% 17:20] - 16L
  repn[tank %in% 21:24] <- tank[tank %in% 21:24] - 20L
  repn[tank %in% 25:28] <- tank[tank %in% 25:28] - 24L
  repn[tank %in% 29:32] <- tank[tank %in% 29:32] - 28L
  repn[tank %in% 33:36] <- tank[tank %in% 33:36] - 32L
  repn[tank %in% 37:40] <- tank[tank %in% 37:40] - 36L
  repn[tank %in% 41:44] <- tank[tank %in% 41:44] - 40L
  repn[tank %in% 45:48] <- tank[tank %in% 45:48] - 44L
  repn
}

get_day <- function(tank){
  day <- rep(NA_real_, length(tank))
  day[tank %in% 1:4]   <- 0
  day[tank %in% 5:24]  <- 2
  day[tank %in% 25:48] <- 6
  day
}

################################################################################
## 2) Function: tidy one workbook
################################################################################
make_tidy_data <- function(file_path){
  
  df_cell <- read.xlsx(file_path, sheet = 1)
  df_bio  <- read.xlsx(file_path, sheet = 2)
  
  ## long format
  cell_long <- pivot_longer(df_cell, cols = matches("^\\d+#$"),
    names_to = "Number", values_to = "Cell_conc_10_4_cell_L")
  
  bio_long <- pivot_longer(df_bio, cols = matches("^\\d+#$"),
    names_to = "Number",  values_to = "Biovolume_mm3_L")
  
  ## clean key
  cell_long$中文名   <- clean_key(cell_long$中文名)
  cell_long$拉丁文名 <- clean_key(cell_long$拉丁文名)
  cell_long$门       <- clean_key(cell_long$门)
  cell_long$Number   <- clean_key(cell_long$Number)
  
  bio_long$中文名   <- clean_key(bio_long$中文名)
  bio_long$拉丁文名 <- clean_key(bio_long$拉丁文名)
  bio_long$门       <- clean_key(bio_long$门)
  bio_long$Number   <- clean_key(bio_long$Number)
  
  ## merge
  dat <- merge(cell_long, bio_long, by = c("中文名","拉丁文名","门","Number"), all = TRUE)
  
  names(dat)[names(dat) == "中文名"]   <- "Chinese_species"
  names(dat)[names(dat) == "拉丁文名"] <- "Latin_species"
  names(dat)[names(dat) == "门"]       <- "Chinese_phylum"
  
  ## extra metadata
  phylum_map <- c(
    "蓝藻" = "Cyanophyta",
    "绿藻" = "Chlorophyta",
    "硅藻" = "Bacillariophyta",
    "隐藻" = "Cryptophyta",
    "甲藻" = "Dinophyta",
    "裸藻" = "Euglenophyta",
    "金藻" = "Chrysophyta"
  )
  
  dat$Tank            <- get_tank(dat$Number)
  dat$Treatment       <- get_treatment(dat$Tank)
  dat$Replication     <- get_replication(dat$Tank)
  dat$Day             <- get_day(dat$Tank)
  dat$Latin_phylum    <- unname(phylum_map[as.character(dat$Chinese_phylum)])
  dat$Expt_Number     <- "XMGS09"
  dat$Cells_mL        <- suppressWarnings(as.numeric(dat$Cell_conc_10_4_cell_L)) * 10
  dat$Biovolume_mm3_L <- suppressWarnings(as.numeric(dat$Biovolume_mm3_L))
  
  final_data <- dat[, c("Expt_Number","Tank","Treatment","Replication","Day",
                        "Latin_phylum","Chinese_phylum",
                        "Latin_species","Chinese_species",
                        "Number","Cells_mL","Biovolume_mm3_L")]
  
  ## remove rows where Cells_mL is 0 or NA  —— 整行直接删掉
  final_data <- final_data[!is.na(final_data$Cells_mL) & final_data$Cells_mL > 0, ]
  
  ## order
  trt_levels <- c("Initial","0P","0.005P","0.005P_2","0.01P","0.025P","0.05P")
  final_data$Treatment <- factor(final_data$Treatment, levels = trt_levels, ordered = TRUE)
  final_data$Day       <- factor(final_data$Day, levels = c(0,2,6), ordered = TRUE)
  
  final_data$Latin_species <- clean_key(final_data$Latin_species)
  final_data$Genus_Latin   <- sub(" .*", "", final_data$Latin_species)
  
  ord <- order(final_data$Treatment, final_data$Day, final_data$Replication, final_data$Tank)
  final_data <- final_data[ord, ]
  rownames(final_data) <- NULL
  
  final_data
}

################################################################################
## 3) Tidy both files
################################################################################
tidy_non <- make_tidy_data("../Input_Data/Cell_5_non_modify.xlsx")
tidy_mod <- make_tidy_data("../Input_Data/Cell_5_modify.xlsx")

################################################################################
## 4) Add morphology to tidy tables
################################################################################
shape_df <- read_excel("/Users/manxiao/My Drive/08_Reference/Algae_shape.xlsx")
shape_df <- as.data.frame(shape_df)

shape_df$Genus_Latin <- clean_key(shape_df$Genus_Latin)
shape_df$Genus_key   <- tolower(shape_df$Genus_Latin)

tidy_non$Genus_key <- tolower(tidy_non$Genus_Latin)
tidy_mod$Genus_key <- tolower(tidy_mod$Genus_Latin)

shape_sub <- shape_df[, c("Genus_key","Morphology")]

tidy_non <- merge(tidy_non, shape_sub, by = "Genus_key", all.x = TRUE, sort = FALSE)
tidy_mod <- merge(tidy_mod, shape_sub, by = "Genus_key", all.x = TRUE, sort = FALSE)

tidy_non$Genus_key <- NULL
tidy_mod$Genus_key <- NULL

################################################################################
## 5) Export tidy tables
################################################################################
#write.xlsx(tidy_non, "../Output_Data/Cell_5_non_modify_tidy.xlsx", rowNames = FALSE)
#write.xlsx(tidy_mod, "../Output_Data/Cell_5_modify_tidy.xlsx", rowNames = FALSE)

################################################################################
## 6) Calculate species-level coefficient from NON-MODIFY
##    only use true paired observations
################################################################################
coef_input <- tidy_non[!is.na(tidy_non$Biovolume_mm3_L) &  tidy_non$Biovolume_mm3_L > 0, ]

coef_input$coef_obs <- coef_input$Biovolume_mm3_L / coef_input$Cells_mL

coef_species <- ddply(coef_input, .(Latin_species, Chinese_species, Latin_phylum), summarise,
  coef_mean  = mean(coef_obs, na.rm = TRUE),  coef_sd    = sd(coef_obs, na.rm = TRUE),
  coef_ratio = sum(Biovolume_mm3_L, na.rm = TRUE) / sum(Cells_mL, na.rm = TRUE),
  n_pair     = length(coef_obs)
)

## sort
coef_species <- coef_species[order(coef_species$Latin_phylum, coef_species$Latin_species), ]
rownames(coef_species) <- NULL

#write.xlsx(coef_species,"../Output_Data/species_conversion_coefficient_from_non_modify.xlsx", rowNames = FALSE)

################################################################################
## 7) Use NON-MODIFY coefficient to calibrate MODIFY biovolume
##    only keep ONE calibrated biovolume column
################################################################################
tidy_mod_calib <- merge(tidy_mod, coef_species[, c("Latin_species","coef_ratio")],
  by = "Latin_species",  all.x = TRUE, sort = FALSE)

## keep original biovolume temporarily for checking if needed
names(tidy_mod_calib)[names(tidy_mod_calib) == "Biovolume_mm3_L"] <- "Biovolume_mm3_L_original"

## calibrated biovolume (single final column)
tidy_mod_calib$Biovolume_mm3_L <- tidy_mod_calib$Cells_mL * tidy_mod_calib$coef_ratio

## if a species has no coefficient in non_modify, keep original modify biovolume
idx_na <- is.na(tidy_mod_calib$Biovolume_mm3_L)
tidy_mod_calib$Biovolume_mm3_L[idx_na] <- tidy_mod_calib$Biovolume_mm3_L_original[idx_na]

## remove helper column
tidy_mod_calib$coef_ratio <- NULL


################################################################################
## 8) Re-order calibrated tidy table
################################################################################
trt_levels <- c("Initial","0P","0.005P","0.005P_2","0.01P","0.025P","0.05P")
tidy_mod_calib$Treatment <- factor(tidy_mod_calib$Treatment, levels = trt_levels, ordered = TRUE)
tidy_mod_calib$Day       <- factor(tidy_mod_calib$Day, levels = c(0, 2, 6), ordered = TRUE)

ord2 <- order(tidy_mod_calib$Treatment, tidy_mod_calib$Day,
              tidy_mod_calib$Replication, tidy_mod_calib$Tank)
tidy_mod_calib <- tidy_mod_calib[ord2, ]
rownames(tidy_mod_calib) <- NULL

################################################################################
## 9) Export calibrated tidy data
################################################################################
write.xlsx(tidy_mod_calib, "../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", rowNames = FALSE)


