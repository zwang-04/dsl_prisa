library(tidyverse)
library(lfe)
library(lubridate)
library(ggthemes)
library(broom)
library(data.table)
library(magrittr)
library(here)
library(dsl)
library(plm)

setwd(here::here())

# ========= Config =========
input_rdata <- "ad_content_humanonly_base.RData"
sample_size <- 100

# ========= Helper Functions =========
options(contrasts = c("contr.treatment", "contr.treatment"))

if (getRversion() >= "2.15.1") utils::globalVariables(
  c("temp_numeric", "temp_char", ".", "i.ad_tone_attack", "i.ad_tone_contrast", 
    "i.ad_tone_promote", "spend", "primary_date", "general", "weeks_to_election")
)

# ========= Load prerequisites =========
load("ad_dataset_clean.RData")  # for fb_codes_clean, tv_codes_clean

# issues list
composite_issue <- c("iss_drugs","iss_fiscal","iss_econ","iss_mil","iss_edu","iss_laworder","iss_foreign","iss_healthcare","iss_env","iss_goodgovt")
low_kappa_issues <- c("issue101","issue12","issue13","issue14","issue15","issue17","issue19","issue20","issue21","issue210","issue212","issue23","issue32","issue33","issue34","issue35","issue36","issue38","issue39","issue40","issue41","issue42","issue44","issue45","issue52","issue57","issue58","issue60","issue61","issue63","issue66","issue71","issue74","issue75","issue80","issue83","issue84","issue90","issue91","issue92","issue94","issue96","issue97","issue98","issue99")
detail_issue_retain <- c("issue210","issue41","issue54","issue106","issue22","issue10","issue24","issue11","issue12","issue20","issue18","issue16","issue17","issue14","issue23","issue19","issue15","issue21","issue50","issue52","issue80","issue83","issue90","issue91","issue60","issue62","issue70","issue53","issue59","issue58","issue55","issue40","issue45","issue43","issue200","issue95","issue30","issue32","issue37","issue102","issue56","issue57","issue31","issue101","issue39","issue93","issue96","issue98","issue103","issue104","issue97")
detail_issue_grouped <- c("issue210","issue41","issue106","issue44","issue33","issue10","issue11","issue12","issue22","issue13","issue20","issue18","issue16","issue14","issue17","issue19","issue23","issue60","issue62","issue50","issue51","issue40","issue45","issue43","issue42","issue212","issue200","issue61","issue63","issue64","issue68","issue69","issue66","issue67","issue70","issue71","issue72","issue73","issue74","issue75","issue76","issue65","issue53","issue54","issue59","issue58","issue55","issue80","issue82","issue83","issue84","issue90","issue91","issue92","issue93","issue39","issue34")
detail_issue_ungrouped <- setdiff(setdiff(detail_issue_retain, detail_issue_grouped), low_kappa_issues)
issue_to_run <- c(composite_issue, detail_issue_ungrouped)

# ========= Load prebuilt base =========
if (!file.exists(input_rdata)) stop("Missing ad_content_humanonly_base.RData.")
load(input_rdata)
setkey(ad_content_humanonly_base, page_id, snapshot_id)
cat("Loaded base subset rows:", nrow(ad_content_humanonly_base), "\n")

# ========= DSL Bootstrap Setup =========
shot <- 0
model <- "llama_70b"
in_context <- paste0(shot, "-shot")

results_file <- paste0("DSL_Fowler_et_al_APSR_2021_bootstrap_", sample_size, ".csv")

column_headers <- c("bootstrap_iter", "shot", "model", "estimate_name", 
                    "basic_n_labeled", "n_labeled", "n_total", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "p_value")

write.table(t(column_headers), 
            file = results_file, 
            sep = ",", 
            col.names = FALSE, 
            row.names = FALSE, 
            quote = FALSE)

cat("创建结果文件:", results_file, "\n")
cat("样本大小设置为:", sample_size, "\n")

# ========= Load shot file =========
shot_file <- paste0(shot, "_shot_Fowler_et_al_APSR_2021_labeled.csv")
if (!file.exists(shot_file)) stop("Shot file not found: ", shot_file)

shot_labels <- read.csv(shot_file)
cat("Loaded", nrow(shot_labels), "rows\n")

# Check columns
if (!(model %in% names(shot_labels))) {
  stop("Model column not found: ", model)
}
if (!("original_label" %in% names(shot_labels))) {
  stop("original_label column not found")
}

# ========= Filter valid labels (0/1/2/3) =========
num_labels_llm <- suppressWarnings(as.numeric(shot_labels[[model]]))
num_labels_human <- suppressWarnings(as.numeric(shot_labels[["original_label"]]))

valid_idx <- (num_labels_llm %in% c(0,1,2,3)) & (num_labels_human %in% c(0,1,2,3))
invalid_n <- sum(!valid_idx, na.rm = TRUE)

cat("Total rows:", length(valid_idx), "\n")
cat("Valid rows:", sum(valid_idx), "\n")
cat("Invalid rows removed:", invalid_n, "\n")

shot_labels_valid <- shot_labels[valid_idx, ]
num_labels_llm_valid <- num_labels_llm[valid_idx]
num_labels_human_valid <- num_labels_human[valid_idx]

# ========= Process LLM predictions =========
shot_labels_llm <- shot_labels_valid %>% mutate(
  ad_tone_attack_llm = case_when(num_labels_llm_valid == 3 ~ 1, TRUE ~ 0),
  ad_tone_contrast_llm = case_when(num_labels_llm_valid == 1 ~ 1, TRUE ~ 0),
  ad_tone_promote_llm = case_when(num_labels_llm_valid == 2 ~ 1, TRUE ~ 0)
)

fb_codes_llm <- fb_codes_clean %>% inner_join(
  shot_labels_llm %>% filter(grepl("^fb_", original_row_id)) %>%
    select(original_row_id, ad_tone_attack_llm, ad_tone_contrast_llm, ad_tone_promote_llm),
  by = c("row_id" = "original_row_id")
)

tv_codes_llm <- tv_codes_clean %>% inner_join(
  shot_labels_llm %>% filter(grepl("^tv_", original_row_id)) %>%
    select(original_row_id, ad_tone_attack_llm, ad_tone_contrast_llm, ad_tone_promote_llm),
  by = c("row_id" = "original_row_id")
)

human_tone_fb_llm <- fb_codes_llm %>% 
  select(page_id, snapshot_id, ad_tone_attack_llm, ad_tone_contrast_llm, ad_tone_promote_llm) %>% 
  as.data.table
human_tone_tv_llm <- tv_codes_llm %>% mutate(page_id = "TV") %>% 
  select(page_id, snapshot_id, ad_tone_attack_llm, ad_tone_contrast_llm, ad_tone_promote_llm) %>% 
  as.data.table
human_tone_all_llm <- rbind(human_tone_fb_llm, human_tone_tv_llm)
setkey(human_tone_all_llm, page_id, snapshot_id)

# Fix: Use merge instead of bracket join for this case
ad_content_llm <- merge(ad_content_humanonly_base, human_tone_all_llm, 
                        by = c("page_id", "snapshot_id"), all = FALSE)

cat("LLM matched rows:", nrow(ad_content_llm), "\n")

# ========= Process Human labels =========
shot_labels_human <- shot_labels_valid %>% mutate(
  ad_tone_attack_human = case_when(num_labels_human_valid == 3 ~ 1, TRUE ~ 0),
  ad_tone_contrast_human = case_when(num_labels_human_valid == 1 ~ 1, TRUE ~ 0),
  ad_tone_promote_human = case_when(num_labels_human_valid == 2 ~ 1, TRUE ~ 0)
)

fb_codes_human <- fb_codes_clean %>% inner_join(
  shot_labels_human %>% filter(grepl("^fb_", original_row_id)) %>%
    select(original_row_id, ad_tone_attack_human, ad_tone_contrast_human, ad_tone_promote_human),
  by = c("row_id" = "original_row_id")
)

tv_codes_human <- tv_codes_clean %>% inner_join(
  shot_labels_human %>% filter(grepl("^tv_", original_row_id)) %>%
    select(original_row_id, ad_tone_attack_human, ad_tone_contrast_human, ad_tone_promote_human),
  by = c("row_id" = "original_row_id")
)

human_tone_fb_human <- fb_codes_human %>% 
  select(page_id, snapshot_id, original_row_id = row_id, ad_tone_attack_human, ad_tone_contrast_human, ad_tone_promote_human) %>% 
  as.data.table
human_tone_tv_human <- tv_codes_human %>% mutate(page_id = "TV") %>% 
  select(page_id, snapshot_id, original_row_id = row_id, ad_tone_attack_human, ad_tone_contrast_human, ad_tone_promote_human) %>% 
  as.data.table
human_tone_all_human <- rbind(human_tone_fb_human, human_tone_tv_human)
setkey(human_tone_all_human, page_id, snapshot_id)

# Fix: Use merge instead of bracket join
ad_content_human <- merge(ad_content_humanonly_base, human_tone_all_human, 
                          by = c("page_id", "snapshot_id"), all = FALSE)

cat("Human matched rows:", nrow(ad_content_human), "\n")

# ========= Aggregate LLM =========
# Merge with templates
cand_ads_fb_llm <- ad_content_llm[cand_ads_fb_template, nomatch=0]
cand_ads_fb_llm[,`:=` (fb=1, officetype = ifelse(office %in% c("us_house", "us_sen"), "federal", "state"))]

cand_ads_tv_llm <- ad_content_llm[cand_ads_tv_template, nomatch=0]
cand_ads_tv_llm[,`:=`(fb=0, officetype= ifelse(office %in% c("US House", "US Senate"), "federal", "state"))]

cand_ads_llm <- rbind(cand_ads_tv_llm, cand_ads_fb_llm, fill=T)
cand_ads_llm %<>% .[pr_dates_clean, on =c("state", "officetype"), nomatch=0]

cand_ad_avg_llm <- cand_ads_llm[!is.na(spend), 
                                .(ad_tone_attack_llm = weighted.mean(ad_tone_attack_llm, spend, na.rm=T),
                                  ad_tone_contrast_llm = weighted.mean(ad_tone_contrast_llm, spend, na.rm=T),
                                  ad_tone_promote_llm = weighted.mean(ad_tone_promote_llm, spend, na.rm=T),
                                  total_spend = sum(spend, na.rm=T),
                                  n_ads = .N), 
                                by=.(cand_id, fb)]

cat("LLM aggregated observations:", nrow(cand_ad_avg_llm), "\n")

# ========= Aggregate Human =========
cand_ads_fb_human <- ad_content_human[cand_ads_fb_template, nomatch=0]
cand_ads_fb_human[,`:=` (fb=1, officetype = ifelse(office %in% c("us_house", "us_sen"), "federal", "state"))]

cand_ads_tv_human <- ad_content_human[cand_ads_tv_template, nomatch=0]
cand_ads_tv_human[,`:=`(fb=0, officetype= ifelse(office %in% c("US House", "US Senate"), "federal", "state"))]

cand_ads_human <- rbind(cand_ads_tv_human, cand_ads_fb_human, fill=T)
cand_ads_human %<>% .[pr_dates_clean, on =c("state", "officetype"), nomatch=0]

# 基于原子单位 original_row_id 构建 组→原子 映射，并计算每组去重后的 basic_n
ad_group_map <- unique(cand_ads_human[!is.na(spend), .(original_row_id, cand_id, fb)])
basic_n_by_group <- ad_group_map[, .(basic_n = uniqueN(original_row_id)), by = .(cand_id, fb)]

cand_ad_avg_human <- cand_ads_human[!is.na(spend), 
                                    .(ad_tone_attack_human = weighted.mean(ad_tone_attack_human, spend, na.rm=T),
                                      ad_tone_contrast_human = weighted.mean(ad_tone_contrast_human, spend, na.rm=T),
                                      ad_tone_promote_human = weighted.mean(ad_tone_promote_human, spend, na.rm=T)), 
                                    by=.(cand_id, fb)]

# 合并去重后的 basic_n
cand_ad_avg_human <- merge(cand_ad_avg_human, basic_n_by_group, by = c("cand_id", "fb"), all.x = TRUE)

cat("Human aggregated observations:", nrow(cand_ad_avg_human), "\n")

# ========= Merge LLM and Human =========
cand_ad_merged <- merge(cand_ad_avg_llm, cand_ad_avg_human, 
                        by = c("cand_id", "fb"), all.x = TRUE)
cand_ad_merged <- as.data.frame(cand_ad_merged)

cat("Merged observations:", nrow(cand_ad_merged), "\n")
cat("Observations with human labels:", sum(!is.na(cand_ad_merged$ad_tone_attack_human)), "\n")

# ========= Bootstrap Loop =========
tone_vars <- c("attack", "contrast", "promote")

for (bootstrap_iter in 1:100) {
  cat("\n=== Bootstrap迭代", bootstrap_iter, "===\n")
  
  # Find available labeled observations
  labeled_pool <- which(!is.na(cand_ad_merged$ad_tone_attack_human))
  available_labeled <- length(labeled_pool)
  
  cat("Available labeled observations:", available_labeled, "\n")
  
  if (available_labeled < sample_size) {
    cat("警告: 可用样本数", available_labeled, "小于请求的样本数", sample_size, "\n")
    actual_sample_size <- available_labeled
  } else {
    actual_sample_size <- sample_size
  }
  
  if (actual_sample_size == 0) {
    cat("跳过: 没有可用样本\n")
    next
  }
  
  # Sample labeled observations
  set.seed(123 + bootstrap_iter + sample_size * 1000)
  labeled_indices <- sample(labeled_pool, actual_sample_size)
  
  # Create current dataset
  current_df <- cand_ad_merged
  current_df$is_labeled <- 0
  current_df$is_labeled[labeled_indices] <- 1
  
  # Calculate basic_n_labeled 基于被抽中的组全局去重 original_row_id
  current_labeled_groups <- unique(as.data.table(current_df[labeled_indices, c("cand_id", "fb")]))
  labeled_original_ids <- unique(ad_group_map[current_labeled_groups, on = c("cand_id", "fb")]$original_row_id)
  basic_n_labeled <- length(labeled_original_ids)
  
  cat("basic_n_labeled:", basic_n_labeled, "\n")
  
  # Convert to factors
  current_df$cand_id <- as.factor(current_df$cand_id)
  current_df$fb <- as.numeric(current_df$fb)
  
  # Loop through three tone variables
  for (tone in tone_vars) {
    cat("\n--- 处理tone:", tone, "---\n")
    
    tone_llm_col <- paste0("ad_tone_", tone, "_llm")
    tone_human_col <- paste0("ad_tone_", tone, "_human")
    tone_labeled_col <- paste0("ad_tone_", tone, "_labeled")
    
    # Create labeled variable
    current_df[[tone_labeled_col]] <- ifelse(current_df$is_labeled == 1,
                                             current_df[[tone_human_col]],
                                             NA)
    
    current_results <- data.frame()
    
    # Try DSL
    tryCatch({
      out <- dsl(
        model = "felm",
        formula = as.formula(paste0(tone_labeled_col, " ~ fb")),
        predicted_var = tone_labeled_col,
        prediction = tone_llm_col,
        fixed_effect = "oneway",
        index = "cand_id",
        cluster = "cand_id",
        data = current_df
      )
      
      n_labeled_actual <- tryCatch({ out$internal$num_expert }, error = function(e) actual_sample_size)
      n_total <- tryCatch({ out$internal$num_data }, error = function(e) nrow(current_df))
      
      summ <- tryCatch({ summary(out) }, error = function(e) NULL)
      coef_tbl <- NULL
      if (!is.null(summ)) {
        if (is.data.frame(summ) || is.matrix(summ)) {
          coef_tbl <- as.data.frame(summ)
        } else {
          coef_tbl <- tryCatch({ summ$coefficients }, error = function(e) NULL)
          if (is.null(coef_tbl)) coef_tbl <- tryCatch({ summ$coef }, error = function(e) NULL)
        }
      }
      
      est <- se <- ciL <- ciU <- pval <- NA
      if (!is.null(coef_tbl)) {
        rn <- rownames(coef_tbl)
        idx <- NA_integer_
        if (!is.null(rn) && "fb" %in% rn) {
          idx <- match("fb", rn)
        } else if (!is.null(rn)) {
          non_intercept <- setdiff(rn, "(Intercept)")
          if (length(non_intercept) == 1) idx <- match(non_intercept, rn)
          if (is.na(idx) && length(rn) == 1) idx <- 1L
        }
        if (!is.na(idx)) {
          est <- suppressWarnings(as.numeric(coef_tbl[idx, "Estimate"]))
          se  <- suppressWarnings(as.numeric(coef_tbl[idx, "Std. Error"]))
          ciL <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Lower"]))
          ciU <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Upper"]))
          pval <- suppressWarnings(as.numeric(coef_tbl[idx, "p value"]))
        }
      }
      
      current_results <- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model,
        estimate_name = paste0(toupper(substring(tone, 1, 1)), substring(tone, 2), " (Candidate)"),
        basic_n_labeled = basic_n_labeled,
        n_labeled = n_labeled_actual,
        n_total = n_total,
        method = "dsl",
        estimate = est,
        se = se,
        ci_lower = ciL,
        ci_upper = ciU,
        p_value = pval
      ))
      
      cat("  ✓ DSL成功\n")
      
    }, error = function(e) {
      cat("  ✗ DSL错误:", e$message, "\n")
      current_results <- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model,
        estimate_name = paste0(toupper(substring(tone, 1, 1)), substring(tone, 2), " (Candidate)"),
        basic_n_labeled = basic_n_labeled,
        n_labeled = actual_sample_size,
        n_total = nrow(current_df),
        method = "dsl",
        estimate = NA,
        se = NA,
        ci_lower = NA,
        ci_upper = NA,
        p_value = NA
      ))
    })
    
    # Write results
    if (nrow(current_results) > 0) {
      write.table(current_results, 
                  file = results_file, 
                  sep = ",", 
                  append = TRUE, 
                  col.names = FALSE, 
                  row.names = FALSE)
    } else {
      cat("  警告: 没有生成任何结果，创建NA行\n")
      na_results <- data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model,
        estimate_name = paste0(toupper(substring(tone, 1, 1)), substring(tone, 2), " (Candidate)"),
        basic_n_labeled = basic_n_labeled,
        n_labeled = actual_sample_size,
        n_total = nrow(current_df),
        method = "dsl",
        estimate = NA,
        se = NA,
        ci_lower = NA,
        ci_upper = NA,
        p_value = NA
      )
      write.table(na_results, 
                  file = results_file, 
                  sep = ",", 
                  append = TRUE, 
                  col.names = FALSE, 
                  row.names = FALSE)
    }
  }
}

cat("\nDSL分析完成! 结果已保存到:", results_file, "\n")