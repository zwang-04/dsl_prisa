# 有时候报错：task {xx} failed - "empty model"
# Bootstrap的部分任务失败

library(here)
setwd(here::here())

library(tidyverse)
library(tm)
library(Rtsne)
library(geometry)
library(rsvd)
library(dplyr)
library(openxlsx)
library(tidytext)
library(quanteda)
library(plm)
library(lmtest)
library(dotwhisker)
library(pbkrtest)
library(car)
library(rstatix)
library(ggpubr)
library(broom)
library(fixest)
library(dsl)
utils::globalVariables(c("temp_numeric", "temp_char", "."))

sample_size <- 100
# =============================================
shot <- 0
model <- "llama_70b"
all_results <- list()

load("parl_speech.Rdata")
df_analysis <- ws_parl
df_analysis <- df_analysis %>% mutate(id = row_number())
names(df_analysis)[144] <- "original_row_id"

file_name <- paste0(shot, "_shot_Widmann_JOP_2025_labeled.csv")
if (!file.exists(file_name)) {
  stop(paste("文件不存在：", file_name))
}
shot_df <- read.csv(file_name)
model_cols <- setdiff(colnames(shot_df), c("original_row_id", "original_label"))
if (shot == 0) {
  model_cols <- c(model_cols, "original_label")
}
if (nrow(df_analysis) != nrow(shot_df)) {
  stop(paste("行数不一致：", file_name))
}
merged <- bind_cols(df_analysis, shot_df)
merged$date3 <- as.numeric(merged$date2) - 17434
merged$months <- NA
k <- 1; j <- 0
for (i in seq(from=30, to=1200, by=30)){
  merged$months[merged$date3 < i & merged$date3 > j] <- k
  j <- i
  k <- k + 1
}
merged2 <- merged[!is.na(merged$wkindirekt),]
merged2 <- merged2[merged2$topic == 56 | merged2$topic == 16,]
merged2$party <- gsub(" ", "", merged2$party)
merged2$party <- gsub("(*UCP)\\s*", "", merged2$party, perl = TRUE)

if (model == "original_label") {
  model_name <- "original study"
  emotion_raw <- merged2[[model]]
} else {
  model_name <- model
  emotion_raw <- merged2[[model]]
}

# 筛选有效行
emotion_num <- suppressWarnings(as.numeric(as.character(emotion_raw)))
valid_idx <- which(!is.na(emotion_num) & emotion_num %in% c(0,1,2,3))
n_total <- length(emotion_num)
n_valid <- length(valid_idx)
n_invalid <- n_total - n_valid
cat("Total relevant rows:", n_total, "\n")
cat(sprintf("[shot=%s, model=%s] 有效行: %d, 无效行: %d\n", shot, model_name, n_valid, n_invalid))
# Total relevant rows: 33056
# [shot=0, model=llama_70b] 有效行: 33055, 无效行: 1

if (n_valid == 0) {
  stop("没有有效数据")
}
merged2_valid <- merged2[valid_idx, ]

# 定义配置
subdatasets <- list(
  list(
    name = "AfD_disgust",
    party = "AfD",
    emotion_var = "disgust",
    emotion_condition = c(2, 3),
    estimate_name = "AfD's disgust appeal"
  ),
  list(
    name = "Greens_anger", 
    party = "Greens",
    emotion_var = "anger",
    emotion_condition = c(1, 3),
    estimate_name = "Green party's anger appeal"
  ),
  list(
    name = "TheLeft_anger",
    party = "TheLeft", 
    emotion_var = "anger",
    emotion_condition = c(1, 3),
    estimate_name = "Left-wing parties' anger appeal"
  )
)

# 子数据集
subdataset_results <- list()

for (i in seq_along(subdatasets)) {
  config <- subdatasets[[i]]
  cat("\n=== 处理子数据集:", config$name, "===\n")

  party <- config$party
  df_party <- merged2_valid[!(merged2_valid$party != party & merged2_valid$treat_indirect == 1),]

  n_before_agg <- nrow(df_party)
  cat("聚合前样本数:", n_before_agg, "\n")

  emotion_var <- config$emotion_var
  emotion_condition <- config$emotion_condition

  df_party[[paste0(emotion_var, "_llm")]] <- ifelse(as.integer(df_party[[model]]) %in% emotion_condition, 1, 0)

  df_party[[paste0(emotion_var, "_human")]] <- ifelse(as.integer(df_party[["original_label"]]) %in% emotion_condition, 1, 0)

  df_agg_llm <- aggregate(df_party[[paste0(emotion_var, "_llm")]], 
                         list(df_party$name, df_party$months, df_party$count, df_party$party), 
                         mean)
  colnames(df_agg_llm) <- c("name", "date", "treat_indirect", "party", paste0(emotion_var, "_llm"))
  
  df_agg_human <- aggregate(df_party[[paste0(emotion_var, "_human")]], 
                           list(df_party$name, df_party$months, df_party$count, df_party$party), 
                           mean)
  colnames(df_agg_human) <- c("name", "date", "treat_indirect", "party", paste0(emotion_var, "_human"))

  df_agg <- df_agg_llm %>%
    left_join(df_agg_human, by = c("name", "date", "treat_indirect", "party"))
  df_agg <- df_agg[order(df_agg$name, -abs(df_agg$treat_indirect)), ]
  df_agg <- df_agg %>% distinct(name, date, .keep_all = T)

  n_after_agg <- nrow(df_agg)
  cat("聚合后样本数:", n_after_agg, "\n")

  basic_counts <- df_party %>%
    group_by(name, months, count, party) %>%
    summarise(basic_n = dplyr::n(), .groups = "drop") %>%
    mutate(name = as.character(name), months = as.numeric(months))

  df_agg_with_counts <- df_agg %>%
    left_join(basic_counts, by = c("name", "date" = "months", "treat_indirect" = "count", "party"))

  available_labeled <- n_after_agg
  if (available_labeled < sample_size) {
    cat("警告: 可用样本数", available_labeled, "小于请求的样本数", sample_size, "\n")
    actual_sample_size <- available_labeled
  } else {
    actual_sample_size <- sample_size
  }
  
  subdataset_results[[config$name]] <- list(
    config = config,
    data = df_agg_with_counts,
    n_before_agg = n_before_agg,
    n_after_agg = n_after_agg,
    sample_size = actual_sample_size
  )
}

# === 处理子数据集: AfD_disgust ===
#   聚合前样本数: 16924 
# 聚合后样本数: 433 
# 
# === 处理子数据集: Greens_anger ===
#   聚合前样本数: 17714 
# 聚合后样本数: 449 
# 
# === 处理子数据集: TheLeft_anger ===
#   聚合前样本数: 17650 
# 聚合后样本数: 432 

# 定义PRISA模型函数
fn_true_model <- function(df) {
  fit_true <- plm(emotion_labeled ~ treat_indirect, model="within", index = c("name", "date"), effect = "twoways", data = df)
  coefs <- coef(fit_true)
  return(coefs["treat_indirect"])
}

fn_proxy_model <- function(df) {
  fit_proxy <- plm(emotion_llm ~ treat_indirect, model="within", index = c("name", "date"), effect = "twoways", data = df)
  coefs <- coef(fit_proxy)
  return(coefs["treat_indirect"])
}

# output file (DSL, keep basic_n_labeled)
results_file <- paste0("DSL_Widmann_JOP_2025_bootstrap_", sample_size, ".csv")
in_context <- paste0(shot, "-shot")
column_headers <- c("bootstrap_iter", "shot", "model", "estimate_name", "subdataset", "basic_n_labeled", "n_labeled", "n_total", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "p_value")

write.table(t(column_headers), 
           file = results_file, 
           sep = ",", 
           col.names = FALSE, 
           row.names = FALSE, 
           quote = FALSE)

cat("创建结果文件:", results_file, "\n")
cat("样本大小设置为:", sample_size, "\n")

# ============ Bootstrap ============
for (bootstrap_iter in 1:100) {
  cat("\n=== Bootstrap迭代", bootstrap_iter, "===\n")
  
  for (subdataset_name in names(subdataset_results)) {
    subdataset_info <- subdataset_results[[subdataset_name]]
    config <- subdataset_info$config
    data <- subdataset_info$data
    actual_sample_size <- subdataset_info$sample_size
    
    cat("\n--- 处理子数据集:", subdataset_name, "---\n")
    
    if (actual_sample_size == 0) {
      cat("跳过", subdataset_name, ": 没有可用样本\n")
      next
    }

    emotion_var <- config$emotion_var
    data$emotion_llm <- data[[paste0(emotion_var, "_llm")]]
    data$emotion_human <- data[[paste0(emotion_var, "_human")]]
    data$emotion_labeled <- NA  # 将在bootstrap中设置

    data$name <- as.factor(data$name)
    data$date <- as.numeric(data$date)
    
    set.seed(123 + bootstrap_iter + sample_size * 1000)
    
    # 采样
    labeled_pool <- which(!is.na(data$emotion_human))
    labeled_indices <- sample(labeled_pool, actual_sample_size)
    
    current_df <- data
    current_df$is_labeled <- 0
    current_df$is_labeled[labeled_indices] <- 1
    current_df$emotion_labeled <- ifelse(current_df$is_labeled == 1, current_df$emotion_human, NA)

    basic_n_labeled <- current_df %>%
      dplyr::filter(is_labeled == 1) %>%
      summarise(total_basic = sum(basic_n, na.rm = TRUE)) %>%
      pull(total_basic)
    
    current_results <- data.frame()
    
    tryCatch({
      out <- dsl(
        model = "felm",
        formula = emotion_labeled ~ treat_indirect,
        predicted_var = "emotion_labeled",
        prediction = "emotion_llm",
        fixed_effect = "twoways",
        index = c("name", "date"),
        cluster = "name",
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
        if (!is.null(rn) && "treat_indirect" %in% rn) {
          idx <- match("treat_indirect", rn)
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
        model = model_name,
        estimate_name = paste0(config$estimate_name),
        subdataset = subdataset_name,
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

    }, error = function(e) {
      cat("  ✗ DSL错误:", e$message, "\n")
      current_results <- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model_name,
        estimate_name = paste0(config$estimate_name),
        subdataset = subdataset_name,
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
        model = model_name,
        estimate_name = paste0(config$estimate_name),
        subdataset = subdataset_name,
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

cat("\nPRISA分析完成! 结果已保存到:", results_file, "\n")
