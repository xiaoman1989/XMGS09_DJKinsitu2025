################################################################################
## Nutrient and Chl-a significance analysis
## Same source data and same statistical workflow for N, P, TN:TP, and Chl-a
## Source data: insitu_water_quality.xlsx
################################################################################

library(openxlsx)
library(car)
library(multcompView)
library(ggplot2)
library(rstatix)

################################################################################
## 0) Path
################################################################################
project_dir <- Sys.getenv(
  "XMGS09_PROJECT_DIR",
  unset = "~/My Drive/00_Project/XMGS09_DJKinsitu2"
)
dirName <- file.path(path.expand(project_dir), "Final_Codes")
setwd(dirName)

input_file <- "../Input_Data/insitu_water_quality.xlsx"
output_data_dir <- "../Output_Data"
output_fig_dir  <- "../Output_Figures"

dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_fig_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
## 1) Read and clean data
################################################################################
d <- read.xlsx(input_file, sheet = 1, check.names = FALSE)
d <- as.data.frame(d, stringsAsFactors = FALSE)

names(d) <- trimws(names(d))
d$Treatment <- trimws(as.character(d$Treatment))
d$Replication <- as.integer(d$Replication)
d$Day <- as.integer(d$Day)
d$Batch <- trimws(as.character(d$Batch))

## Keep common internal names.
if (!"TDN_mg_L" %in% names(d) && "DTN_mg_L" %in% names(d)) d$TDN_mg_L <- d$DTN_mg_L
if (!"TDP_mg_L" %in% names(d) && "DTP_mg_L" %in% names(d)) d$TDP_mg_L <- d$DTP_mg_L
if (!"Chla_ug_L" %in% names(d) && "Chla_µg_L" %in% names(d)) d$Chla_ug_L <- d$`Chla_µg_L`

response_vars <- c(
  "TN_mg_L",
  "TDN_mg_L",
  "NOx-N_mg_L",
  "NH4-N_mg_L",
  "TP_mg_L",
  "TDP_mg_L",
  "PO4-P_mg_L",
  "TN_TP",
  "Chla_ug_L"
)

for (v in response_vars) {
  d[[v]] <- as.numeric(as.character(d[[v]]))
}

## Recalculate TN:TP from replicate-level TN and TP to avoid rounding/source issues.
d$TN_TP <- ifelse(
  is.finite(d$TN_mg_L) & is.finite(d$TP_mg_L) & d$TP_mg_L > 0,
  d$TN_mg_L / d$TP_mg_L,
  NA_real_
)

################################################################################
## 2) Keep analysis data
################################################################################
treatment_levels <- c("0P", "0.005P", "0.01P", "0.025P", "0.05P")

## Day2: after three P pulses, before water replacement.
## Day6: final measurement.
analysis_data <- d[
  ((d$Day == 2 & d$Batch == "1") |
     (d$Day == 6 & d$Batch == "2")) &
    d$Treatment %in% treatment_levels,
  ,
  drop = FALSE
]

## 0.005P_2 is the no-exchange treatment and is not part of the main P gradient.
excluded_data <- d[d$Treatment == "0.005P_2", , drop = FALSE]

analysis_data$Treatment <- factor(analysis_data$Treatment, levels = treatment_levels)

################################################################################
## 3) Significance analysis function
################################################################################
run_significance <- function(dat, response_col, response_name) {
  dat <- dat[!is.na(dat[[response_col]]), , drop = FALSE]
  
  summary_list <- list()
  assumption_list <- list()
  anova_list <- list()
  pairwise_list <- list()
  letter_list <- list()
  
  for (day_value in c(2, 6)) {
    x <- dat[dat$Day == day_value, , drop = FALSE]
    x$Treatment <- droplevels(x$Treatment)
    
    if (nrow(x) < 3 || length(unique(x$Treatment)) < 2) next
    
    summary_day <- do.call(
      rbind,
      lapply(split(x[[response_col]], x$Treatment), function(v) {
        data.frame(
          n = length(v),
          mean = mean(v, na.rm = TRUE),
          sd = sd(v, na.rm = TRUE),
          se = sd(v, na.rm = TRUE) / sqrt(length(v)),
          median = median(v, na.rm = TRUE),
          min = min(v, na.rm = TRUE),
          max = max(v, na.rm = TRUE)
        )
      })
    )
    
    summary_day$Treatment <- rownames(summary_day)
    rownames(summary_day) <- NULL
    summary_day$Day <- day_value
    summary_day$Response <- response_name
    
    names(x)[names(x) == response_col] <- "Response_value"
    initial_fit <- lm(Response_value ~ Treatment, data = x)
    shapiro_result <- shapiro.test(residuals(initial_fit))
    levene_result <- car::leveneTest(Response_value ~ Treatment, data = x)

    normality_pass <- shapiro_result$p.value >= 0.05
    equal_variance_pass <- levene_result[1, "Pr(>F)"] >= 0.05

    ## Select the omnibus and post-hoc procedures from the diagnostics.
    ## - Normal residuals + equal variances: ordinary ANOVA + Tukey HSD
    ## - Normal residuals + unequal variances: Welch ANOVA + Games-Howell
    ## - Non-normal residuals: Kruskal-Wallis + Dunn (Holm adjustment)
    if (!normality_pass) {
      omnibus <- kruskal.test(Response_value ~ Treatment, data = x)
      dunn_result <- rstatix::dunn_test(
        x,
        Response_value ~ Treatment,
        p.adjust.method = "holm"
      )

      treatment_means_tmp <- tapply(
        x$Response_value,
        x$Treatment,
        mean,
        na.rm = TRUE
      )
      pairwise_df <- data.frame(
        Response = response_name,
        Day = day_value,
        Method = "Dunn test (Holm-adjusted)",
        Comparison = paste(dunn_result$group2, dunn_result$group1, sep = "-"),
        Difference = unname(
          treatment_means_tmp[dunn_result$group2] -
            treatment_means_tmp[dunn_result$group1]
        ),
        Lower_CI = NA_real_,
        Upper_CI = NA_real_,
        P_adjusted = dunn_result$p.adj,
        Significant = dunn_result$p.adj < 0.05,
        row.names = NULL
      )

      omnibus_method <- "Kruskal-Wallis"
      statistic_name <- "Chi-square"
      statistic_value <- unname(omnibus$statistic)
      df_between <- unname(omnibus$parameter)
      df_within <- NA_real_
      omnibus_p <- omnibus$p.value
    } else if (!equal_variance_pass) {
      omnibus <- oneway.test(
        Response_value ~ Treatment,
        data = x,
        var.equal = FALSE
      )
      gh_result <- rstatix::games_howell_test(
        x,
        Response_value ~ Treatment
      )

      pairwise_df <- data.frame(
        Response = response_name,
        Day = day_value,
        Method = "Games-Howell",
        Comparison = paste(gh_result$group2, gh_result$group1, sep = "-"),
        Difference = gh_result$estimate,
        Lower_CI = gh_result$conf.low,
        Upper_CI = gh_result$conf.high,
        P_adjusted = gh_result$p.adj,
        Significant = gh_result$p.adj < 0.05,
        row.names = NULL
      )

      omnibus_method <- "Welch ANOVA"
      statistic_name <- "F"
      statistic_value <- unname(omnibus$statistic)
      df_between <- unname(omnibus$parameter[1])
      df_within <- unname(omnibus$parameter[2])
      omnibus_p <- omnibus$p.value
    } else {
      fit <- aov(Response_value ~ Treatment, data = x)
      anova_result <- summary(fit)[[1]]
      tukey_result <- TukeyHSD(fit)$Treatment

      pairwise_df <- data.frame(
        Response = response_name,
        Day = day_value,
        Method = "Tukey HSD",
        Comparison = rownames(tukey_result),
        Difference = tukey_result[, "diff"],
        Lower_CI = tukey_result[, "lwr"],
        Upper_CI = tukey_result[, "upr"],
        P_adjusted = tukey_result[, "p adj"],
        Significant = tukey_result[, "p adj"] < 0.05,
        row.names = NULL
      )

      omnibus_method <- "One-way ANOVA"
      statistic_name <- "F"
      statistic_value <- anova_result["Treatment", "F value"]
      df_between <- anova_result["Treatment", "Df"]
      df_within <- anova_result["Residuals", "Df"]
      omnibus_p <- anova_result["Treatment", "Pr(>F)"]
    }

    p_values <- pairwise_df$P_adjusted
    names(p_values) <- pairwise_df$Comparison
    letters0 <- multcompLetters(p_values, threshold = 0.05)$Letters
    
    ## Reorder letters so that the highest mean starts with "a".
    treatment_means <- tapply(x$Response_value, x$Treatment, mean, na.rm = TRUE)
    old_letters <- unique(unlist(strsplit(unname(letters0), "")))
    old_letter_rank <- sapply(old_letters, function(letter) {
      groups_with_letter <- names(letters0)[grepl(letter, letters0, fixed = TRUE)]
      max(treatment_means[groups_with_letter], na.rm = TRUE)
    })
    ordered_old_letters <- names(sort(old_letter_rank, decreasing = TRUE))
    new_letter_map <- setNames(base::letters[seq_along(ordered_old_letters)],
                               ordered_old_letters)
    letters1 <- vapply(
      letters0,
      function(label) {
        old_parts <- unlist(strsplit(label, ""))
        new_parts <- unname(new_letter_map[old_parts])
        paste(sort(new_parts), collapse = "")
      },
      character(1)
    )
    
    letter_df <- data.frame(
      Response = response_name,
      Day = day_value,
      Treatment = names(letters1),
      Letter = unname(letters1),
      row.names = NULL
    )
    
    assumption_list[[as.character(day_value)]] <- data.frame(
      Response = response_name,
      Day = day_value,
      Shapiro_W = unname(shapiro_result$statistic),
      Shapiro_p = shapiro_result$p.value,
      Levene_F = levene_result[1, "F value"],
      Levene_p = levene_result[1, "Pr(>F)"],
      Normality_pass = normality_pass,
      Equal_variance_pass = equal_variance_pass,
      Selected_omnibus_method = omnibus_method,
      Selected_posthoc_method = unique(pairwise_df$Method)
    )
    
    anova_list[[as.character(day_value)]] <- data.frame(
      Response = response_name,
      Day = day_value,
      Method = omnibus_method,
      Statistic_name = statistic_name,
      Statistic = statistic_value,
      Df_between = df_between,
      Df_within = df_within,
      P_value = omnibus_p,
      Significant_unadjusted = omnibus_p < 0.05
    )
    
    summary_list[[as.character(day_value)]] <- summary_day
    pairwise_list[[as.character(day_value)]] <- pairwise_df
    letter_list[[as.character(day_value)]] <- letter_df
  }
  
  if (length(summary_list) == 0) return(NULL)
  
  summary_all <- do.call(rbind, summary_list)
  letters_all <- do.call(rbind, letter_list)
  summary_all <- merge(
    summary_all,
    letters_all,
    by = c("Response", "Day", "Treatment"),
    all.x = TRUE,
    sort = FALSE
  )
  
  summary_all$Treatment <- factor(summary_all$Treatment, levels = treatment_levels)
  summary_all <- summary_all[
    order(summary_all$Response, summary_all$Day, summary_all$Treatment),
    c("Response", "Day", "Treatment", "n", "mean", "sd", "se",
      "median", "min", "max", "Letter"),
    drop = FALSE
  ]
  summary_all$Treatment <- as.character(summary_all$Treatment)
  
  list(
    summary = summary_all,
    assumptions = do.call(rbind, assumption_list),
    anova = do.call(rbind, anova_list),
    pairwise = do.call(rbind, pairwise_list)
  )
}

################################################################################
## 4) Run analyses
################################################################################
response_map <- c(
  "TN_mg_L" = "TN",
  "TDN_mg_L" = "TDN",
  "NOx-N_mg_L" = "NOx-N",
  "NH4-N_mg_L" = "NH4-N",
  "TP_mg_L" = "TP",
  "TDP_mg_L" = "TDP",
  "PO4-P_mg_L" = "PO4-P",
  "TN_TP" = "TN:TP",
  "Chla_ug_L" = "Chl-a"
)

result_list <- list()
for (v in names(response_map)) {
  result_list[[v]] <- run_significance(analysis_data, v, response_map[[v]])
}
result_list <- result_list[!sapply(result_list, is.null)]

summary_all <- do.call(rbind, lapply(result_list, function(x) x$summary))
assumptions_all <- do.call(rbind, lapply(result_list, function(x) x$assumptions))
anova_all <- do.call(rbind, lapply(result_list, function(x) x$anova))
pairwise_all <- do.call(rbind, lapply(result_list, function(x) x$pairwise))

## Control the false-discovery rate across all omnibus response tests conducted
## within the same sampling day. Post-hoc P values are already adjusted within
## each response by Tukey, Games-Howell, or Holm, as appropriate.
anova_all$P_BH_by_day <- ave(
  anova_all$P_value,
  anova_all$Day,
  FUN = function(x) p.adjust(x, method = "BH")
)
anova_all$Significant_BH <- anova_all$P_BH_by_day < 0.05

################################################################################
## 5) Export Excel
################################################################################
output_file <- file.path(
  output_data_dir,
  "Nutrient_Chla_significance_corrected.xlsx"
)

method_notes <- data.frame(
  Item = c(
    "Day 2 definition",
    "Day 6 definition",
    "Test selection",
    "Omnibus multiplicity",
    "Post-hoc multiplicity",
    "NH4-N interpretation",
    "Day2_post exclusion"
  ),
  Description = c(
    "Day 2 analyses use measured Day2_pre samples only (Batch 1).",
    "Day 6 analyses use Batch 2 samples from the main P-gradient treatments.",
    paste(
      "One-way ANOVA and Tukey HSD were used when residual normality and",
      "variance homogeneity passed; Welch ANOVA and Games-Howell were used",
      "for unequal variances; Kruskal-Wallis and Holm-adjusted Dunn tests",
      "were used for non-normal residuals."
    ),
    "Omnibus P values were Benjamini-Hochberg adjusted across responses within each day.",
    "Post-hoc P values were adjusted within each response by the selected procedure.",
    "Most NH4-N observations were below the stated 0.01 mg L-1 detection limit; interpret inferential results cautiously.",
    "Calculated Day2_post values are not included in these among-treatment tests."
  ),
  stringsAsFactors = FALSE
)

wb <- createWorkbook()
sheet_data <- list(
  Summary_with_letters = summary_all,
  Omnibus_tests = anova_all,
  Assumption_tests = assumptions_all,
  Pairwise_tests = pairwise_all,
  Analysis_data = analysis_data,
  Excluded_0.005P_2 = excluded_data,
  Method_notes = method_notes
)

for (sheet_name in names(sheet_data)) {
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  writeData(wb, sheet_name, sheet_data[[sheet_name]])
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name,
               cols = seq_len(ncol(sheet_data[[sheet_name]])),
               widths = "auto")
}

saveWorkbook(wb, output_file, overwrite = TRUE)

################################################################################
## 6) Plot TN:TP significance
################################################################################
plot_df <- summary_all[summary_all$Response == "TN:TP", ]
plot_df$Treatment <- factor(plot_df$Treatment, levels = treatment_levels)
plot_df$Day_label <- factor(paste("Day", plot_df$Day),
                            levels = c("Day 2", "Day 6"))
plot_df$label_y <- plot_df$mean + plot_df$sd + 2.5

p_tntp <- ggplot(plot_df, aes(x = Treatment, y = mean, group = 1)) +
  geom_line(color = "#386CB0", linewidth = 0.8) +
  geom_point(shape = 21, size = 3.2,
             fill = "#7DB7E8", color = "black", stroke = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.12, linewidth = 0.6) +
  geom_text(aes(y = label_y, label = Letter),
            size = 4.5, fontface = "bold") +
  facet_wrap(~ Day_label, nrow = 1, scales = "free_y") +
  labs(x = "P treatment", y = "TN:TP (mass ratio)") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.16))) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.6)
  )

ggsave(file.path(output_fig_dir, "TNTP_significance_by_day_corrected.png"),
       p_tntp, width = 8, height = 4.2, dpi = 600)
