packages <- c('openxlsx', 'dplyr', 'data.table', 'lubridate', 'zoo', 'bit64', 'ggplot2')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
rm(packages, new.packages)

library(here)
library(readr)
library(fixest)
library(readxl)
library(openxlsx)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

maindir <- if (exists("STUDY_DIR")) STUDY_DIR else here()

load("dat_stacked.RData")

filter_and_clean_labels <- function(label_vec) {
    valid_digits <- as.character(1:6)
    invalid_contents <- c()
    valid_idx <- rep(TRUE, length(label_vec))
    cleaned_labels <- vector("character", length(label_vec))
    for (i in seq_along(label_vec)) {
        x <- label_vec[i]
        items <- unlist(strsplit(as.character(x), ","))
        items <- trimws(items)
        items <- gsub("\\.0$", "", items)
        if (length(items) == 0 || any(!items %in% valid_digits)) {
            valid_idx[i] <- FALSE
            invalid_contents <- c(invalid_contents, x)
            cleaned_labels[i] <- NA
        } else {
            items <- sort(items)
            cleaned_labels[i] <- paste(items, collapse=",")
        }
    }
    list(valid_idx=valid_idx, cleaned_labels=cleaned_labels, invalid=unique(invalid_contents))
}

# Figure 5: Predicted relative attributions of economic news
n_shot <- c(0, 2, 5, 10)
estimates <- data.frame()
estimate_names <- c('Vladimir Putin', 'Russian officials', 'Foreign economy', 'Foreign governments')

for (i in seq_along(n_shot)){
    shot <- n_shot[i]
    csv_path <- file.path(ANNOTATION_DIR, paste0(shot, '_shot_Rozenas_Stukal_JOP_2018', ANNOTATION_SUFFIX))
    if (file.exists(csv_path)) {
        df_llm <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
    } else {
        cat(paste0('未找到标注文件，跳过：', csv_path, '\n'))
        next
    }
    exclude_cols <- c("original_row_id", "original_label",
                      grep("^outcome_type", names(df_llm), value = TRUE))
    llm_cols <- setdiff(names(df_llm), exclude_cols)
    if (shot == 0 && "original_label" %in% names(df_llm)) {
        llm_cols <- c("original_label", llm_cols)
        df_llm$original_label <- as.character(df_llm$original_label)
    }
    for (k in llm_cols) {
        res <- filter_and_clean_labels(as.character(df_llm[[k]]))
        cat("shot=", shot, ", model=", k, ": 过滤了", sum(!res$valid_idx), "行\n")
        cat("无效内容:", paste0("[", paste(unique(res$invalid), collapse=","), "]\n"))
        valid_idx <- res$valid_idx
        cleaned_labels <- res$cleaned_labels
        df_llm_model <- df_llm[valid_idx, ]
        df_llm_model[[k]] <- cleaned_labels[valid_idx]
        if (k == "original_label") {
            df_llm_model$putin_bin <- grepl("1", df_llm_model[[k]])*1
            df_llm_model$rugov_bin <- grepl("2", df_llm_model[[k]])*1
            df_llm_model$forec_bin <- grepl("5", df_llm_model[[k]])*1
            df_llm_model$forgov_bin <- grepl("4", df_llm_model[[k]])*1
            merge_cols <- c("original_row_id", "putin_bin", "rugov_bin", "forec_bin", "forgov_bin")
        } else {
            model <- gsub("_", "", k)
            df_llm_model[[paste0("putin_bin_", model)]]    <- grepl("1", df_llm_model[[k]])*1
            df_llm_model[[paste0("rugov_bin_", model)]]    <- grepl("2", df_llm_model[[k]])*1
            df_llm_model[[paste0("forec_bin_", model)]]    <- grepl("5", df_llm_model[[k]])*1
            df_llm_model[[paste0("forgov_bin_", model)]]   <- grepl("4", df_llm_model[[k]])*1
            merge_cols <- c("original_row_id", paste0("putin_bin_", model), paste0("rugov_bin_", model), paste0("forec_bin_", model), paste0("forgov_bin_", model))
        }
        dat_temp <- dat_stacked |> mutate(original_row_id = 1:nrow(dat_stacked)) |> left_join(df_llm_model[, merge_cols], by="original_row_id")
        if (k == "original_label") {
            for (v in c("putin_bin", "rugov_bin", "forec_bin", "forgov_bin")) {
                if (paste0(v, ".y") %in% names(dat_temp)) {
                    dat_temp[[v]] <- dat_temp[[paste0(v, ".y")]]
                }
            }
            dat_temp <- dat_temp[!is.na(dat_temp$putin_bin), ]
            spec1 <- as.formula("putin_bin ~ pos | year+month+weekdays")
            spec2 <- as.formula("rugov_bin ~ pos | year+month+weekdays")
            spec3 <- as.formula("forec_bin ~ pos | year+month+weekdays")
            spec4 <- as.formula("forgov_bin ~ pos | year+month+weekdays")
            m1 <- tryCatch(feols(spec1, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m2 <- tryCatch(feols(spec2, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m3 <- tryCatch(feols(spec3, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m4 <- tryCatch(feols(spec4, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m <- list(m1, m2, m3, m4)
            for (j in 1:4){
                if (is.null(m[[j]])) next
                llm_est <- data.frame(
                    model="original study", estimate_name=estimate_names[j],
                    estimate=unname(coef(m[[j]])),
                    se=unname(se(m[[j]])),
                    in_context="original")
                estimates <- bind_rows(estimates, llm_est)
            }
        } else {
            dat_temp <- dat_temp[!is.na(dat_temp[[paste0('putin_bin_', model)]]), ]
            spec1 <- as.formula(paste0("putin_bin_", model, "~ pos | year+month+weekdays"))
            spec2 <- as.formula(paste0("rugov_bin_", model, "~ pos | year+month+weekdays"))
            spec3 <- as.formula(paste0("forec_bin_", model, "~ pos | year+month+weekdays"))
            spec4 <- as.formula(paste0("forgov_bin_", model, "~ pos | year+month+weekdays"))
            m1 <- tryCatch(feols(spec1, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m2 <- tryCatch(feols(spec2, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m3 <- tryCatch(feols(spec3, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m4 <- tryCatch(feols(spec4, data = dat_temp, cluster=c('year_month')), error = function(e) NULL)
            m <- list(m1, m2, m3, m4)
            for (j in 1:4){
                if (is.null(m[[j]])) next
                llm_est <- data.frame(
                    model=k, estimate_name=estimate_names[j],
                    estimate=unname(coef(m[[j]])),
                    se=unname(se(m[[j]])),
                    in_context=paste0(shot, '-shot'))
                estimates <- bind_rows(estimates, llm_est)
            }
        }
    }
}

estimates <- estimates |> 
    mutate(
        estimate_name = factor(estimate_name, levels=c('Vladimir Putin', 'Russian officials', 'Foreign economy', 'Foreign governments')),
        model = factor(model))

estimates$study <- 'Rozenas_Stukal_JOP_2018'

write.csv(estimates, file.path(maindir, 'Rozenas_Stukal_JOP_2018_estimates.csv'), row.names = FALSE, fileEncoding = 'UTF-8')

p1 <- ggplot(estimates, aes(x=estimate_name, y=estimate, color=model, shape=model)) + 
    scale_shape_manual(values = c("original study" = 16, "llama_8b" = 2, "gemma_12b"=0, "mistral_24b"=8, "gemma_27b"=4, "llama_70b"=5, "qwen_72b"=6)) +
    geom_pointrange(aes(ymin = estimate - 1.96*se, ymax=estimate + 1.96*se), position = position_dodge(width=0.4), size=0.6, linewidth=0.8) +
    theme_bw() + geom_hline(yintercept = 0, linetype = 'longdash', color = 'grey') + 
    labs(x = '', y='Estimates') + theme(text = element_text(size = 20))

ggsave(file.path(maindir, 'Rozenas_Stukal_JOP_2018.png'), p1, width=12, height=8)
