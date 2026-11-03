################################################################################
### 1) Day 2: compare treatments for total cells, biovolume, and richness
### 2) Day 6: compare treatments for total cells, biovolume, and richness
### 3) Within each treatment: Day2 * 0.8 vs Day6
###
### raw ANOVA/t-test -> if assumptions fail, log10(x + 1) -> if still fail,
### non-parametric test.
################################################################################

library(openxlsx)
library(plyr)
library(car)
library(emmeans)
library(multcomp)

# ==============================================================================
# 1) Read data
# ==============================================================================
dirName <- "~/My Drive/00_Project/XMGS09_DJKinsitu2/Final_Codes"
setwd(dirName)

data <- read.xlsx("../Output_Data/Cell_5_modify_tidy_calibrated.xlsx", sheet = 1)
data <- as.data.frame(data)

# remove no-exchange 0.005P_2 treatment
data <- data[as.character(data$Treatment) != "0.005P_2", ]

data$Treatment       <- trimws(as.character(data$Treatment))
data$Latin_species   <- trimws(as.character(data$Latin_species))
data$Replication     <- as.character(data$Replication)
data$Day             <- as.numeric(as.character(data$Day))
data$Cells_mL        <- as.numeric(as.character(data$Cells_mL))
data$Biovolume_mm3_L <- as.numeric(as.character(data$Biovolume_mm3_L))

trt_levels_P <- c("Initial", "0P", "0.005P", "0.01P", "0.025P", "0.05P")
df <- data[data$Treatment %in% trt_levels_P, ]
df <- as.data.frame(df)

# ==============================================================================
# 2) Helper functions
# ==============================================================================
get_cld_letters <- function(emm_obj){
  cld_res <- multcomp::cld(
    emm_obj,
    Letters = letters,
    adjust = "tukey"
  )
  cld_res <- as.data.frame(cld_res)
  if(".group" %in% names(cld_res)){
    cld_res$.group <- gsub(" ", "", cld_res$.group)
  }
  cld_res
}

safe_shapiro_p <- function(x){
  out <- tryCatch(shapiro.test(x)$p.value, error = function(e) NA)
  out
}

safe_levene_p <- function(formula_use, data_use){
  out <- tryCatch(car::leveneTest(formula_use, data = data_use)[1, "Pr(>F)"],
                  error = function(e) NA)
  as.numeric(out)
}

run_group_test <- function(dat, response_col, label_text, group_levels = trt_levels_P){
  
  cat("\n====================================================\n")
  cat(label_text, "\n")
  cat("====================================================\n")
  
  dat <- as.data.frame(dat)
  dat$Treatment <- factor(dat$Treatment, levels = group_levels)
  dat <- dat[!is.na(dat[[response_col]]) & !is.na(dat$Treatment), ]
  
  letters_out <- data.frame()
  
  form_raw <- as.formula(paste(response_col, "~ Treatment"))
  fit_raw <- aov(form_raw, data = dat)
  shapiro_raw <- safe_shapiro_p(residuals(fit_raw))
  levene_raw  <- safe_levene_p(form_raw, dat)
  
  cat("\nRaw assumption tests\n")
  print(shapiro_raw)
  print(levene_raw)
  
  if(!is.na(shapiro_raw) && !is.na(levene_raw) &&
     shapiro_raw > 0.05 && levene_raw > 0.05){
    
    cat("\nUse raw ANOVA\n")
    print(summary(fit_raw))
    
    emm_raw <- emmeans(fit_raw, ~ Treatment)
    print(pairs(emm_raw, adjust = "tukey"))
    
    letters_out <- get_cld_letters(emm_raw)
    print(letters_out)
    
    return(letters_out)
  }
  
  log_col <- paste0(response_col, "_log")
  dat[[log_col]] <- log10(dat[[response_col]] + 1)
  form_log <- as.formula(paste(log_col, "~ Treatment"))
  fit_log <- aov(form_log, data = dat)
  shapiro_log <- safe_shapiro_p(residuals(fit_log))
  levene_log  <- safe_levene_p(form_log, dat)
  
  cat("\nLog10(x+1) assumption tests\n")
  print(shapiro_log)
  print(levene_log)
  
  if(!is.na(shapiro_log) && !is.na(levene_log) &&
     shapiro_log > 0.05 && levene_log > 0.05){
    
    cat("\nUse log10(x+1) ANOVA\n")
    print(summary(fit_log))
    
    emm_log <- emmeans(fit_log, ~ Treatment)
    print(pairs(emm_log, adjust = "tukey"))
    
    letters_out <- get_cld_letters(emm_log)
    print(letters_out)
    
  } else {
    
    cat("\nUse Kruskal-Wallis\n")
    print(kruskal.test(form_raw, data = dat))
    print(pairwise.wilcox.test(dat[[response_col]],
                               dat$Treatment,
                               p.adjust.method = "BH"))
    
    letters_out <- data.frame(
      Treatment = levels(dat$Treatment),
      .group = NA
    )
  }
  
  letters_out
}

run_within_test <- function(dat, response_col, response_name){
  
  trts_compare <- unique(as.character(dat$Treatment))
  results <- data.frame()
  
  for(trt in trts_compare){
    
    subdat <- dat[dat$Treatment == trt, , drop = FALSE]
    subdat <- subdat[!is.na(subdat[[response_col]]) & !is.na(subdat$Time), , drop = FALSE]
    subdat$Time <- factor(subdat$Time, levels = c("day2_0.8", "day6"))
    
    cat("\n-----------------------------\n")
    cat(response_name, "- Treatment:", trt, "\n")
    cat("-----------------------------\n")
    
    if(length(unique(subdat$Time)) < 2){
      cat("Skipped: only one Time level\n")
      next
    }
    
    form_raw <- as.formula(paste(response_col, "~ Time"))
    fit_raw <- aov(form_raw, data = subdat)
    shapiro_raw <- safe_shapiro_p(residuals(fit_raw))
    levene_raw  <- safe_levene_p(form_raw, subdat)
    
    if(!is.na(shapiro_raw) && !is.na(levene_raw) &&
       shapiro_raw > 0.05 && levene_raw > 0.05){
      
      test_res <- t.test(form_raw, data = subdat, var.equal = TRUE)
      method <- "t-test_raw"
      
    } else {
      
      log_col <- paste0(response_col, "_log")
      subdat[[log_col]] <- log10(subdat[[response_col]] + 1)
      form_log <- as.formula(paste(log_col, "~ Time"))
      fit_log <- aov(form_log, data = subdat)
      shapiro_log <- safe_shapiro_p(residuals(fit_log))
      levene_log  <- safe_levene_p(form_log, subdat)
      
      if(!is.na(shapiro_log) && !is.na(levene_log) &&
         shapiro_log > 0.05 && levene_log > 0.05){
        
        test_res <- t.test(form_log, data = subdat, var.equal = TRUE)
        method <- "t-test_log10x1"
        
      } else {
        
        test_res <- wilcox.test(form_raw, data = subdat, exact = FALSE)
        method <- "wilcox"
      }
    }
    
    print(test_res)
    
    results <- rbind(results, data.frame(
      Treatment = trt,
      Response = response_name,
      Method = method,
      P_value = test_res$p.value,
      stringsAsFactors = FALSE
    ))
  }
  
  results
}

make_within_letters <- function(within_results, response_name){
  
  letters_df <- data.frame()
  
  for(i in 1:nrow(within_results)){
    trt <- within_results$Treatment[i]
    pval <- within_results$P_value[i]
    
    if(is.na(pval)){
      l1 <- NA
      l2 <- NA
    } else if(pval < 0.05){
      l1 <- "a"
      l2 <- "b"
    } else {
      l1 <- "a"
      l2 <- "a"
    }
    
    tmp <- data.frame(
      Treatment = trt,
      Time = c("day2_0.8", "day6"),
      Response = response_name,
      Letters = c(l1, l2),
      stringsAsFactors = FALSE
    )
    
    letters_df <- rbind(letters_df, tmp)
  }
  
  letters_df
}

merge_letters <- function(summary_df, letters_df){
  
  if(nrow(letters_df) == 0) return(summary_df)
  
  if(".group" %in% names(letters_df)){
    names(letters_df)[names(letters_df) == ".group"] <- "Letters"
  }
  
  merge(summary_df, letters_df[, c("Treatment", "Letters")],
        by = "Treatment", all.x = TRUE, sort = FALSE)
}

# ==============================================================================
# 3) Create datasets
# ==============================================================================
df_day2 <- df[df$Day == 2, ]
df_day6 <- df[df$Day == 6, ]

df_day2_08 <- df_day2
df_day2_08$Cells_mL        <- df_day2_08$Cells_mL * 0.8
df_day2_08$Biovolume_mm3_L <- df_day2_08$Biovolume_mm3_L * 0.8
df_day2_08$Time            <- "day2_0.8"

df_day6_time <- df_day6
df_day6_time$Time <- "day6"

df_compare <- rbind(df_day2_08, df_day6_time)
df_compare$Time <- factor(df_compare$Time, levels = c("day2_0.8", "day6"))

# ==============================================================================
# 4) Replicate-level totals
# ==============================================================================
cell_day2_rep <- ddply(df_day2, .(Treatment, Replication), summarise,
                       Total_cells = sum(Cells_mL, na.rm = TRUE))

bio_day2_rep <- ddply(df_day2, .(Treatment, Replication), summarise,
                      Total_bio = sum(Biovolume_mm3_L, na.rm = TRUE))

cell_day6_rep <- ddply(df_day6, .(Treatment, Replication), summarise,
                       Total_cells = sum(Cells_mL, na.rm = TRUE))

bio_day6_rep <- ddply(df_day6, .(Treatment, Replication), summarise,
                      Total_bio = sum(Biovolume_mm3_L, na.rm = TRUE))

cell_compare_rep <- ddply(df_compare, .(Treatment, Time, Replication), summarise,
                          Total_cells = sum(Cells_mL, na.rm = TRUE))

bio_compare_rep <- ddply(df_compare, .(Treatment, Time, Replication), summarise,
                         Total_bio = sum(Biovolume_mm3_L, na.rm = TRUE))

df_rich <- df[!is.na(df$Latin_species) &
                trimws(as.character(df$Latin_species)) != "" &
                !is.na(df$Cells_mL) & df$Cells_mL > 0, ]
df_rich <- as.data.frame(df_rich)

rich_day2_rep <- ddply(df_rich[df_rich$Day == 2, ], .(Treatment, Replication), summarise,
                       Richness = length(unique(Latin_species)))

rich_day6_rep <- ddply(df_rich[df_rich$Day == 6, ], .(Treatment, Replication), summarise,
                       Richness = length(unique(Latin_species)))

# ==============================================================================
# 5) Summary tables
# ==============================================================================
cell_day2_summary <- ddply(cell_day2_rep, .(Treatment), summarise,
                           Mean_cells = mean(Total_cells, na.rm = TRUE),
                           SD_cells   = sd(Total_cells, na.rm = TRUE),
                           n          = sum(!is.na(Total_cells)))

bio_day2_summary <- ddply(bio_day2_rep, .(Treatment), summarise,
                          Mean_bio = mean(Total_bio, na.rm = TRUE),
                          SD_bio   = sd(Total_bio, na.rm = TRUE),
                          n        = sum(!is.na(Total_bio)))

cell_day6_summary <- ddply(cell_day6_rep, .(Treatment), summarise,
                           Mean_cells = mean(Total_cells, na.rm = TRUE),
                           SD_cells   = sd(Total_cells, na.rm = TRUE),
                           n          = sum(!is.na(Total_cells)))

bio_day6_summary <- ddply(bio_day6_rep, .(Treatment), summarise,
                          Mean_bio = mean(Total_bio, na.rm = TRUE),
                          SD_bio   = sd(Total_bio, na.rm = TRUE),
                          n        = sum(!is.na(Total_bio)))

cell_compare_summary <- ddply(cell_compare_rep, .(Treatment, Time), summarise,
                              Mean_cells = mean(Total_cells, na.rm = TRUE),
                              SD_cells   = sd(Total_cells, na.rm = TRUE),
                              n          = sum(!is.na(Total_cells)))

bio_compare_summary <- ddply(bio_compare_rep, .(Treatment, Time), summarise,
                             Mean_bio = mean(Total_bio, na.rm = TRUE),
                             SD_bio   = sd(Total_bio, na.rm = TRUE),
                             n        = sum(!is.na(Total_bio)))

rich_day2_summary <- ddply(rich_day2_rep, .(Treatment), summarise,
                           Mean_richness = mean(Richness, na.rm = TRUE),
                           SD_richness   = sd(Richness, na.rm = TRUE),
                           n             = sum(!is.na(Richness)))

rich_day6_summary <- ddply(rich_day6_rep, .(Treatment), summarise,
                           Mean_richness = mean(Richness, na.rm = TRUE),
                           SD_richness   = sd(Richness, na.rm = TRUE),
                           n             = sum(!is.na(Richness)))

# ==============================================================================
# 6) Between-treatment tests
# ==============================================================================
letters_day2_cell <- run_group_test(cell_day2_rep, "Total_cells",
                                    "ANALYSIS 1A: Day 2 total cell concentration among treatments")

letters_day2_bio <- run_group_test(bio_day2_rep, "Total_bio",
                                   "ANALYSIS 1B: Day 2 total biovolume among treatments")

letters_day2_rich <- run_group_test(rich_day2_rep, "Richness",
                                    "ANALYSIS 1C: Day 2 richness among treatments")

letters_day6_cell <- run_group_test(cell_day6_rep, "Total_cells",
                                    "ANALYSIS 2A: Day 6 total cell concentration among treatments")

letters_day6_bio <- run_group_test(bio_day6_rep, "Total_bio",
                                   "ANALYSIS 2B: Day 6 total biovolume among treatments")

letters_day6_rich <- run_group_test(rich_day6_rep, "Richness",
                                    "ANALYSIS 2C: Day 6 richness among treatments")

# ==============================================================================
# 7) Within-treatment tests: Day2*0.8 vs Day6
# ==============================================================================
cat("\n====================================================\n")
cat("ANALYSIS 3A: Within each treatment, Day2*0.8 vs Day6 for Total_cells\n")
cat("====================================================\n")

within_cell_results <- run_within_test(cell_compare_rep, "Total_cells", "Total_cells")
within_cell_letters <- make_within_letters(within_cell_results, "Total_cells")

cat("\n====================================================\n")
cat("ANALYSIS 3B: Within each treatment, Day2*0.8 vs Day6 for Total_bio\n")
cat("====================================================\n")

within_bio_results <- run_within_test(bio_compare_rep, "Total_bio", "Total_bio")
within_bio_letters <- make_within_letters(within_bio_results, "Total_bio")

# ==============================================================================
# 8) Merge letters into summary tables
# ==============================================================================
cell_day2_summary <- merge_letters(cell_day2_summary, letters_day2_cell)
bio_day2_summary  <- merge_letters(bio_day2_summary,  letters_day2_bio)
rich_day2_summary <- merge_letters(rich_day2_summary, letters_day2_rich)

cell_day6_summary <- merge_letters(cell_day6_summary, letters_day6_cell)
bio_day6_summary  <- merge_letters(bio_day6_summary,  letters_day6_bio)
rich_day6_summary <- merge_letters(rich_day6_summary, letters_day6_rich)

# ==============================================================================
# 9) Save input and summary tables
# ==============================================================================
write.xlsx(
  list(
    "cell_day2_rep"        = cell_day2_rep,
    "bio_day2_rep"         = bio_day2_rep,
    "rich_day2_rep"        = rich_day2_rep,
    "cell_day6_rep"        = cell_day6_rep,
    "bio_day6_rep"         = bio_day6_rep,
    "rich_day6_rep"        = rich_day6_rep,
    "cell_compare_rep"     = cell_compare_rep,
    "bio_compare_rep"      = bio_compare_rep,
    "cell_day2_summary"    = cell_day2_summary,
    "bio_day2_summary"     = bio_day2_summary,
    "rich_day2_summary"    = rich_day2_summary,
    "cell_day6_summary"    = cell_day6_summary,
    "bio_day6_summary"     = bio_day6_summary,
    "rich_day6_summary"    = rich_day6_summary,
    "cell_compare_summary" = cell_compare_summary,
    "bio_compare_summary"  = bio_compare_summary,
    "within_cell_results"  = within_cell_results,
    "within_bio_results"   = within_bio_results,
    "within_cell_letters"  = within_cell_letters,
    "within_bio_letters"   = within_bio_letters
  ),
  "../Output_Data/Stat_tables.xlsx",
  overwrite = TRUE
)
