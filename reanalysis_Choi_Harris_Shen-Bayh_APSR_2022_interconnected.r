setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

rm(list = ls())
library(Matching)
library(foreign)
library(xtable)
library(ggplot2)
library(reshape2)
library(readstata13)
library(estimatr)
library(stargazer)
library(lubridate)
library(readr)
library(stringr)
library(lfe)
library(foreach)
library(rdrobust)
library(tidyverse)
library(forcats)
library(ggplot2)
library(doMC)
library(tidyverse)
library(broom)
library(kableExtra)
library(text2vec)
library(quanteda)

registerDoMC(24)

load('analysis_final.Rdata')

fdata <- fdata %>% filter(!is.na(match1))
fdata <- fdata %>% filter(!is.na(jfordef))

shot_numbers <- c(0, 2, 5, 10)
study_name <- 'Choi_Harris_Shen-Bayh_APSR_2022'
all_results <- list()

for (shot in shot_numbers) {
  file_name <- file.path(ANNOTATION_DIR, paste0(shot, '_shot_Choi_Harris_Shen-Bayh_APSR_2022', ANNOTATION_SUFFIX))
  if (!file.exists(file_name)) {
    warning(paste('文件不存在:', file_name))
    next
  }
  shot_df <- read.csv(file_name)
  model_cols <- setdiff(colnames(shot_df), c('original_row_id', 'original_label'))
  if (shot == 0) {
    model_cols <- c(model_cols, 'original_label')
  }
  for (model_col in model_cols) {
    model_vec <- shot_df[[model_col]]
    model_num <- suppressWarnings(as.numeric(as.character(model_vec)))
    valid_idx <- which(model_num %in% c(0, 1) & !is.na(model_num))
    n_total <- length(model_vec)
    n_valid <- length(valid_idx)
    n_invalid <- n_total - n_valid
    cat(sprintf('shot=%s, model=%s: 有效行=%d, 无效行=%d\n', shot, model_col, n_valid, n_invalid))
    if (n_valid == 0) next
    temp_df <- fdata[valid_idx, ]
    temp_df$model <- model_num[valid_idx]
    # c1
    c1 <- tryCatch({felm(model ~ match1 | 0 | 0 | 0, data=temp_df)}, error=function(e) NULL)
    # c2
    c2 <- tryCatch({felm(model ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | 0 | 0 | 0, data=temp_df)}, error=function(e) NULL)
    # c3
    c3 <- tryCatch({felm(model ~ match1 | cyfe | 0 | cyfe, data=temp_df)}, error=function(e) NULL)
    # c4
    c4 <- tryCatch({felm(model ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe | 0 | cyfe, data=temp_df)}, error=function(e) NULL)
    # c5
    c5 <- tryCatch({felm(model ~ match1 | cyfe + factor(uid) | 0 | uid, data=temp_df)}, error=function(e) NULL)
    # c6
    c6 <- tryCatch({felm(model ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe + factor(uid) | 0 | uid, data=temp_df)}, error=function(e) NULL)
    model_list <- list(c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5, c6 = c6)
    for (i in seq_along(model_list)) {
      reg <- model_list[[i]]
      if (is.null(reg)) next
      tidy_res <- broom::tidy(reg)
      tidy_res <- tidy_res[tidy_res$term == 'match1', ]
      if (nrow(tidy_res) == 0) next
      if (model_col == 'original_label') {
        model_name <- 'original study'
        in_context <- 'original'
      } else {
        model_name <- model_col
        in_context <- paste0(shot, '-shot')
      }
      estimate_name <- paste0('Coethnic Match (M', i, ')')
      result_row <- data.frame(
        model = model_name,
        estimate_name = estimate_name,
        estimate = tidy_res$estimate,
        se = tidy_res$std.error,
        in_context = in_context,
        study = study_name,
        stringsAsFactors = FALSE
      )
      all_results[[length(all_results)+1]] <- result_row
    }
  }
}

final_results <- do.call(rbind, all_results)
final_results$shot_number <- ifelse(final_results$model == 'original study', -1, as.numeric(gsub('-shot', '', final_results$in_context)))
final_results <- final_results %>%
  arrange(shot_number, model, estimate_name)
final_results <- bind_rows(
  final_results %>% filter(model == 'original study'),
  final_results %>% filter(model != 'original study')
)

final_results$shot_number <- NULL

write.csv(final_results, 'Choi_Harris_Shen-Bayh_APSR_2022_estimates.csv', row.names = FALSE)
