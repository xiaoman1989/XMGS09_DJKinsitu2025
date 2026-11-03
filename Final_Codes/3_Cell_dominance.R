################################################################################
# McNaughton dominance (TRUE only) by Treatment × Day
# OUTPUT ORDER STRICT:
#   Day0:  Initial
#   Day2:  0P, 0.005P, 0.01P, 0.025P, 0.05P
#   Day6:  0P, 0.005P, 0.01P, 0.025P, 0.05P
################################################################################

library(openxlsx)
library(plyr)

Cell_tidy <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)

################################################################################
# 0) Prep data
################################################################################
data <- Cell_tidy

###remove data from 0.005P_2 treatment
data <- data[as.character(data$Treatment) != "0.005P_2",]

# make sure Treatment is character before edits
data$Treatment   <- as.character(data$Treatment)
data$Replication <- as.factor(data$Replication)
data$Genus_Latin <- as.factor(data$Genus_Latin)
data$Latin_species <- as.factor(data$Latin_species)
data$Latin_phylum  <- as.factor(data$Latin_phylum)

data$Cells_mL <- suppressWarnings(as.numeric(data$Cells_mL))
data$Cells_mL[is.na(data$Cells_mL)] <- 0

# Day to numeric, then fix Initial -> 1
data$Day <- suppressWarnings(as.numeric(as.character(data$Day)))
data$Day[data$Treatment == "Initial"] <- 0

# If you have duplicate 0.005P groups, map tanks/replicates BEFORE this script.
# Here we assume Treatment already contains "0.005P_2" where appropriate.
data <- data[is.na(data$Day) | data$Day %in% c(0,2,6), ]

# composite key
data$TrtDay <- paste0(data$Treatment, "_Day", data$Day)

################################################################################
# 1) STRICT orders (Day-first, then Treatment order)
################################################################################
trt_order <- c("Initial","0P","0.005P","0.01P","0.025P","0.05P")
day_order <- c(0,2,6)

trtday_order <- c(
  "Initial_Day0",
  paste0(trt_order[-1], "_Day2"),
  paste0(trt_order[-1], "_Day6")
)
trtday_header <- trtday_order

phylum_order <- c("Bacillariophyta","Chlorophyta","Cyanophyta",
                  "Cryptophyta","Dinophyta","Euglenophyta")

################################################################################
################################################################################
# Genus-level McNaughton dominance (TRUE only)  BY Treatment × Day
################################################################################
tax_info_g <- unique(data[, c("Genus_Latin","Latin_phylum")])

# ----------------------- Method A (Genus) -----------------------
result_A_g <- data.frame()

for(td in trtday_order){   # ✅ use strict order, not unique()
  sub_td <- data[data$TrtDay == td, ]
  if(nrow(sub_td) == 0) next
  
  reps <- unique(sub_td$Replication)
  R <- length(reps)
  if(R == 0) next
  
  for(g in unique(sub_td$Genus_Latin)){
    rel_vec <- c()
    occ <- 0
    
    for(r in reps){
      sub_rep <- sub_td[sub_td$Replication == r, ]
      N_r  <- sum(sub_rep$Cells_mL, na.rm = TRUE)
      ni_r <- sum(sub_rep$Cells_mL[sub_rep$Genus_Latin == g], na.rm = TRUE)
      
      if(ni_r > 0) occ <- occ + 1
      rel_vec <- c(rel_vec, ifelse(N_r > 0, ni_r / N_r, 0))
    }
    
    Y <- mean(rel_vec) * (occ / R)
    
    if(Y > 0.02){
      result_A_g <- rbind(result_A_g,
                          data.frame(TrtDay = td,
                                     Genus  = as.character(g),
                                     Y_A    = Y))
    }
  }
}

result_A_g <- merge(result_A_g, tax_info_g,
                    by.x = "Genus", by.y = "Genus_Latin", all.x = TRUE)

# ----------------------- Method B (Genus) -----------------------
result_B_g <- data.frame()

for(td in trtday_order){   # ✅ strict order
  sub_td <- data[data$TrtDay == td, ]
  if(nrow(sub_td) == 0) next
  
  reps <- unique(sub_td$Replication)
  R <- length(reps)
  if(R == 0) next
  
  N_total <- sum(sub_td$Cells_mL, na.rm = TRUE)
  
  for(g in unique(sub_td$Genus_Latin)){
    ni_total <- sum(sub_td$Cells_mL[sub_td$Genus_Latin == g], na.rm = TRUE)
    
    occ <- length(unique(sub_td$Replication[
      sub_td$Genus_Latin == g & sub_td$Cells_mL > 0
    ]))
    
    Y <- ifelse(N_total > 0, (ni_total / N_total) * (occ / R), 0)
    
    if(Y > 0.02){
      result_B_g <- rbind(result_B_g,
                          data.frame(TrtDay = td,
                                     Genus  = as.character(g),
                                     Y_B    = Y))
    }
  }
}

result_B_g <- merge(result_B_g, tax_info_g,
                    by.x = "Genus", by.y = "Genus_Latin", all.x = TRUE)

# ----------------------- Wide (Genus) -----------------------
make_wide_g_day <- function(df, value_col){
  
  tmp <- df[, c("Latin_phylum","Genus","TrtDay", value_col)]
  tmp <- tmp[!is.na(tmp$Latin_phylum) & !is.na(tmp$Genus) & !is.na(tmp$TrtDay), ]
  
  wide <- reshape(tmp,
                  idvar = c("Latin_phylum","Genus"),
                  timevar = "TrtDay",
                  direction = "wide")
  
  names(wide) <- gsub(paste0("^", value_col, "\\."), "", names(wide))
  
  # add missing columns (keep strict order)
  for(t in trtday_order){
    if(!(t %in% names(wide))) wide[[t]] <- NA
  }
  
  wide <- wide[, c("Latin_phylum","Genus", trtday_order)]
  names(wide) <- c("Phylum","Genus", trtday_header)
  
  wide[, 3:ncol(wide)] <- round(wide[, 3:ncol(wide)], 3)
  
  # sort rows by phylum order then genus
  wide$Phylum <- factor(wide$Phylum, levels = phylum_order)
  wide <- wide[order(wide$Phylum, wide$Genus), ]
  wide$Phylum <- as.character(wide$Phylum)
  
  wide
}

wide_A_g <- make_wide_g_day(result_A_g, "Y_A")
wide_B_g <- make_wide_g_day(result_B_g, "Y_B")

write.xlsx(list(Method_A_TRUE = wide_A_g, Method_B_TRUE = wide_B_g),
           file = "../Output_Data/Genus_Dominance_TRUE_only_BY_TreatmentDay.xlsx",
           overwrite = TRUE)

################################################################################
################################################################################
# Species-level McNaughton dominance (TRUE only)  BY Treatment × Day
################################################################################
data_sp <- data[!is.na(data$Latin_species) & as.character(data$Latin_species) != "", ]
tax_sp  <- unique(data_sp[, c("Latin_species","Latin_phylum","Genus_Latin")])

# ----------------------- Method A (Species) -----------------------
result_A_sp <- data.frame()

for(td in trtday_order){   # ✅ strict order
  sub_td <- data_sp[data_sp$TrtDay == td, ]
  if(nrow(sub_td) == 0) next
  
  reps <- unique(sub_td$Replication)
  R <- length(reps)
  if(R == 0) next
  
  for(sp in unique(sub_td$Latin_species)){
    rel_vec <- c()
    occ <- 0
    
    for(r in reps){
      sub_rep <- sub_td[sub_td$Replication == r, ]
      N_r  <- sum(sub_rep$Cells_mL, na.rm = TRUE)
      ni_r <- sum(sub_rep$Cells_mL[sub_rep$Latin_species == sp], na.rm = TRUE)
      
      if(ni_r > 0) occ <- occ + 1
      rel_vec <- c(rel_vec, ifelse(N_r > 0, ni_r / N_r, 0))
    }
    
    Y <- mean(rel_vec) * (occ / R)
    Y <- round(Y, 3)
    
    if(Y > 0.02){
      result_A_sp <- rbind(result_A_sp,
                           data.frame(TrtDay = td,
                                      Latin_species = as.character(sp),
                                      Y_A = Y))
    }
  }
}

result_A_sp <- merge(result_A_sp, tax_sp, by = "Latin_species", all.x = TRUE)

# ----------------------- Method B (Species) -----------------------
result_B_sp <- data.frame()

for(td in trtday_order){   # ✅ strict order
  sub_td <- data_sp[data_sp$TrtDay == td, ]
  if(nrow(sub_td) == 0) next
  
  reps <- unique(sub_td$Replication)
  R <- length(reps)
  if(R == 0) next
  
  N_total <- sum(sub_td$Cells_mL, na.rm = TRUE)
  
  for(sp in unique(sub_td$Latin_species)){
    ni_total <- sum(sub_td$Cells_mL[sub_td$Latin_species == sp], na.rm = TRUE)
    
    occ <- length(unique(sub_td$Replication[
      sub_td$Latin_species == sp & sub_td$Cells_mL > 0
    ]))
    
    Y <- ifelse(N_total > 0, (ni_total / N_total) * (occ / R), 0)
    Y <- round(Y, 3)
    
    if(Y > 0.02){
      result_B_sp <- rbind(result_B_sp,
                           data.frame(TrtDay = td,
                                      Latin_species = as.character(sp),
                                      Y_B = Y))
    }
  }
}

result_B_sp <- merge(result_B_sp, tax_sp, by = "Latin_species", all.x = TRUE)

# ----------------------- Wide (Species) -----------------------
make_wide_sp_day <- function(df, value_col){
  
  tmp <- df[, c("Latin_phylum","Genus_Latin","Latin_species","TrtDay", value_col)]
  tmp <- tmp[!is.na(tmp$Latin_phylum) & !is.na(tmp$Genus_Latin) &
               !is.na(tmp$Latin_species) & !is.na(tmp$TrtDay), ]
  
  wide <- reshape(tmp,
                  idvar = c("Latin_phylum","Genus_Latin","Latin_species"),
                  timevar = "TrtDay",
                  direction = "wide")
  
  names(wide) <- gsub(paste0("^", value_col, "\\."), "", names(wide))
  
  for(t in trtday_order){
    if(!(t %in% names(wide))) wide[[t]] <- NA
  }
  
  wide <- wide[, c("Latin_phylum","Genus_Latin","Latin_species", trtday_order)]
  names(wide) <- c("Phylum","Genus","Species", trtday_header)
  
  wide[, 4:ncol(wide)] <- round(wide[, 4:ncol(wide)], 3)
  
  wide$Phylum <- factor(wide$Phylum, levels = phylum_order)
  wide <- wide[order(wide$Phylum, wide$Genus, wide$Species), ]
  wide$Phylum <- as.character(wide$Phylum)
  
  wide
}

wide_A_sp <- make_wide_sp_day(result_A_sp, "Y_A")
wide_B_sp <- make_wide_sp_day(result_B_sp, "Y_B")

write.xlsx(list(Method_A_TRUE = wide_A_sp, Method_B_TRUE = wide_B_sp),
           file = "../Output_Data/Species_Dominance_TRUE_only_A_vs_B_BY_TreatmentDay.xlsx",
           overwrite = TRUE)

################################################################################
# Sanity check: show available TrtDay & strict order
################################################################################
print(sort(unique(data$TrtDay)))
print(trtday_order)
print(table(data$Treatment, data$Day, useNA = "ifany"))

