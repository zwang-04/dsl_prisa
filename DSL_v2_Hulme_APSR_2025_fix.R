setwd(here::here())

rm(list = ls())

library(readr)
library(dplyr)
library(doBy)
library(readxl)
library(writexl)
library(MASS)
library(sandwich)
library(lmtest)
library(stargazer)
library(car)
library(dsl)

dataset <- read_csv("dataset.csv")
CSUMF_Crises <- read_csv("Data_files/CSUMF_Crises.csv")
Covariates <- read_csv("Data_files/Covariates.csv")

label_files <- list.files(pattern = "^0_shot_Hulme_APSR_2025_labeled\\.csv$")
cat("找到的0-shot标注文件：", label_files, "\n")

if (length(label_files) == 0) {
  stop("未找到0-shot标注文件（需要文件名：0_shot_Hulme_APSR_2025_labeled.csv）")
}

# Bootstrap
sample_size <- 200

for (shot_file in label_files) {
  cat("\n=== 正在处理文件：", shot_file, "===\n")
  
  label_data <- read_csv(shot_file)
  cat("总行数：", nrow(label_data), "\n")

  parse_and_validate_label <- function(x) {
    if (is.na(x) || x == "NA" || x == "") {
      return(list(valid = FALSE, values = rep(NA, 8)))
    }
    
    values <- strsplit(x, ",")[[1]]
    if (length(values) != 8) {
      return(list(valid = FALSE, values = rep(NA, 8)))
    }
    
    numeric_values <- rep(NA, 8)
    for (i in 1:8) {
      if (values[i] != "NA" && !is.na(values[i])) {
        num_val <- as.numeric(values[i])
        if (!is.na(num_val) && (num_val == 0 || num_val == 1)) {
          numeric_values[i] <- num_val
        } else {
          return(list(valid = FALSE, values = rep(NA, 8)))
        }
      }
    }
    
    return(list(valid = TRUE, values = numeric_values))
  }
  
  # 只需跑llama_70b；original_label在下面单独解析一次供bootstrap使用
  models_to_process <- c("llama_70b")
  
  processed_data_list <- list()
  
  for (model_col in models_to_process) {
    cat("\n--- 处理列：", model_col, "---\n")
    
    if (!model_col %in% colnames(label_data)) {
      cat("警告：未找到列", model_col, "，跳过\n")
      next
    }
    
    # 解析
    parsed_results <- lapply(label_data[[model_col]], parse_and_validate_label)
    
    valid_indices <- sapply(parsed_results, function(x) x$valid)
    valid_count <- sum(valid_indices)
    total_count <- length(valid_indices)
    invalid_count <- total_count - valid_count
    
    cat("  数据质量统计：\n")
    cat("    总行数：", total_count, "\n")
    cat("    有效行数：", valid_count, "\n")
    cat("    无效行数：", invalid_count, "\n")
    cat("    有效率：", round(valid_count/total_count*100, 1), "%\n")
    
    if (valid_count == 0) {
      cat("  警告：没有有效数据，跳过此列\n")
      next
    }

    label_data_valid <- label_data[valid_indices, ]

    label_data_valid$Advocates_for_Use_of_American_Military_Force <- sapply(parsed_results[valid_indices], function(x) x$values[1])
    label_data_valid$Advocates_for_Use_of_American_Ground_Troops <- sapply(parsed_results[valid_indices], function(x) x$values[2])
    label_data_valid$Advocates_for_Use_of_American_Air_Assets <- sapply(parsed_results[valid_indices], function(x) x$values[3])
    label_data_valid$Advocates_for_Use_of_American_Naval_Assets <- sapply(parsed_results[valid_indices], function(x) x$values[4])
    label_data_valid$Advocates_against_Use_of_American_Military_Force <- sapply(parsed_results[valid_indices], function(x) x$values[5])
    label_data_valid$Advocates_against_Use_of_American_Ground_Troops <- sapply(parsed_results[valid_indices], function(x) x$values[6])
    label_data_valid$Advocates_against_Use_of_American_Air_Assets <- sapply(parsed_results[valid_indices], function(x) x$values[7])
    label_data_valid$Advocates_against_Use_of_American_Naval_Assets <- sapply(parsed_results[valid_indices], function(x) x$values[8])
    
    label_data_selected <- label_data_valid %>%
      dplyr::select(original_row_id, 
                    Advocates_for_Use_of_American_Military_Force,
                    Advocates_for_Use_of_American_Ground_Troops,
                    Advocates_for_Use_of_American_Air_Assets,
                    Advocates_for_Use_of_American_Naval_Assets,
                    Advocates_against_Use_of_American_Military_Force,
                    Advocates_against_Use_of_American_Ground_Troops,
                    Advocates_against_Use_of_American_Air_Assets,
                    Advocates_against_Use_of_American_Naval_Assets)

    CSUMF_speeches <- dataset %>%
      left_join(label_data_selected, by = "original_row_id")

    handcoded <- subset(CSUMF_speeches, CSUMF_speeches$human_labeled == 1)
    handcoded <- subset(handcoded, handcoded$tweet == 0)

    handcoded$aggregate_support <- handcoded$Advocates_for_Use_of_American_Military_Force + 
      handcoded$Advocates_for_Use_of_American_Ground_Troops + 
      handcoded$Advocates_for_Use_of_American_Air_Assets + 
      handcoded$Advocates_for_Use_of_American_Naval_Assets
    handcoded$aggregate_opposition <- handcoded$Advocates_against_Use_of_American_Military_Force + 
      handcoded$Advocates_against_Use_of_American_Ground_Troops + 
      handcoded$Advocates_against_Use_of_American_Air_Assets + 
      handcoded$Advocates_against_Use_of_American_Naval_Assets

    # 第一次聚合：按危机-议员
    collapse2 <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID,
                           FUN=sum, data=handcoded)
    collapse2$individual_agg_support <- collapse2$aggregate_support.sum /
      (collapse2$aggregate_support.sum + collapse2$aggregate_opposition.sum)

    # 与reanalysis一致：NA行整行剔除，不填0
    collapse2 <- subset(collapse2, !is.na(collapse2$individual_agg_support))
    collapse2$count <- 1

    collapse2_complete <- collapse2
    
    # 第二次聚合：按危机
    collapse2b <- summaryBy(individual_agg_support + count ~ crisname + crisno, 
                            FUN=sum, data=collapse2)
    collapse2b$avg_agg_support <- collapse2b$individual_agg_support.sum/collapse2b$count.sum
    SPEAKERS_FULL <- collapse2b[c("crisno","count.sum")]
    SPEAKERS_FULL$Full_set_speakers <- SPEAKERS_FULL$count.sum
    SPEAKERS_FULL <- SPEAKERS_FULL[c("crisno","Full_set_speakers")]
    CSUMF_CSS <- collapse2b
    CSUMF_CSS <- CSUMF_CSS[c(1,2,5)]
    CSUMF_CSS <- merge(CSUMF_CSS, SPEAKERS_FULL, by = "crisno")

    CSUMF_CSS$avg_agg_support <- CSUMF_CSS$avg_agg_support - 0.5
    CSUMF_CSS$avg_agg_support_5adjust <- ifelse(CSUMF_CSS$Full_set_speakers > 4,
                                                CSUMF_CSS$avg_agg_support, 
                                                CSUMF_CSS$avg_agg_support*CSUMF_CSS$Full_set_speakers*.2)

    USA_crises <- CSUMF_Crises[c("crisno","crisname","Vietnam War","US_high_act")]
    CSUMF_CSS <- merge(USA_crises, CSUMF_CSS, by = c("crisno"), all.x = T)
    CSUMF_CSS <- CSUMF_CSS[, !colnames(CSUMF_CSS) %in% "crisname.y"]
    colnames(CSUMF_CSS)[colnames(CSUMF_CSS) == "crisname.x"] <- "crisname"

    CSUMF_CSS$continuing_war <- 0
    CSUMF_CSS$continuing_war <- ifelse(CSUMF_CSS$crisname == "KOREAN WAR II" | 
                                         CSUMF_CSS$crisname == "KOREAN WAR III" |
                                         CSUMF_CSS$crisname == "PLEIKU" |
                                         CSUMF_CSS$crisname == "Vietnam Escalation" |
                                         CSUMF_CSS$crisname == "Vietnam 67 Key Votes" |
                                         CSUMF_CSS$crisname == "TET OFFENSIVE" |
                                         CSUMF_CSS$crisname == "VIETNAM SPRING OFF." |
                                         CSUMF_CSS$crisname == "INVASION OF CAMBODIA" |
                                         CSUMF_CSS$crisname == "INVASION OF LAOS II" |
                                         CSUMF_CSS$crisname == "VIETNAM PORTS MINING" |
                                         CSUMF_CSS$crisname == "CHRISTMAS BOMBING" |
                                         CSUMF_CSS$crisname == "Iraq Surge" |
                                         CSUMF_CSS$crisname == "Afghan Surge", 1, 0)
    
    CSUMF_CSS$US_high_act <- ifelse(CSUMF_CSS$crisname == "GULF OF TONKIN", 15, CSUMF_CSS$US_high_act)
    
    # 计算党派支持分数
    collapse2_for_party <- collapse2_complete  # 使用完整副本
    collapse2_for_party$party <- collapse2_for_party$MasterID
    
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
    collapse2_for_party$sixth <- substr(substrRight(collapse2_for_party$MasterID,6),1,1)
    collapse2_for_party$seventh <- substr(substrRight(collapse2_for_party$MasterID,7),1,1)
    collapse2_for_party$party <- ifelse(collapse2_for_party$sixth == 0, collapse2_for_party$seventh, collapse2_for_party$sixth)
    collapse2_for_party$party <- ifelse(collapse2_for_party$party == 1, 100, collapse2_for_party$party)
    collapse2_for_party$party <- ifelse(collapse2_for_party$party == 2, 200, collapse2_for_party$party)
    collapse2_for_party <- subset(collapse2_for_party, collapse2_for_party$party == 100 | collapse2_for_party$party == 200)
    collapse2b <- summaryBy(individual_agg_support + count ~ crisname + crisno + party, 
                            FUN=sum, data=collapse2_for_party)
    collapse2b$avg_agg_support <- collapse2b$individual_agg_support.sum/collapse2b$count.sum
    
    Repub_speakers <- subset(collapse2b, collapse2b$party == 200)
    Repub_speakers <- Repub_speakers[c(2,5)] 
    colnames(Repub_speakers)[colnames(Repub_speakers) == "count.sum"] <- "Repub_speakers_full"
    
    Dem_speakers <- subset(collapse2b, collapse2b$party == 100)
    Dem_speakers <- Dem_speakers[c(2,5)] 
    colnames(Dem_speakers)[colnames(Dem_speakers) == "count.sum"] <- "Dem_speakers_full"
    
    CSUMF_CSS_party <- collapse2b
    CSUMF_CSS_party <- CSUMF_CSS_party[c(1,2,3,6)]
    
    CSUMF_CSS_Dems <- subset(CSUMF_CSS_party, CSUMF_CSS_party$party == 100)
    CSUMF_CSS_Dems$avg_agg_support_Dems <- CSUMF_CSS_Dems$avg_agg_support
    CSUMF_CSS_Dems <- CSUMF_CSS_Dems[c(2,5)]
    
    CSUMF_CSS_Repubs <- subset(CSUMF_CSS_party, CSUMF_CSS_party$party == 200)
    CSUMF_CSS_Repubs$avg_agg_support_Repubs <- CSUMF_CSS_Repubs$avg_agg_support
    CSUMF_CSS_Repubs <- CSUMF_CSS_Repubs[c(2,5)]
    
    CSUMF_CSS <- merge(CSUMF_CSS, CSUMF_CSS_Dems, by = "crisno", all.x = T)
    CSUMF_CSS <- merge(CSUMF_CSS, CSUMF_CSS_Repubs, by = "crisno", all.x = T)
    CSUMF_CSS <- merge(CSUMF_CSS, Repub_speakers, by = "crisno", all.x = T)
    CSUMF_CSS <- merge(CSUMF_CSS, Dem_speakers, by = "crisno", all.x = T)
    
    CSUMF_CSS$avg_agg_support_Repubs <- CSUMF_CSS$avg_agg_support_Repubs - 0.5
    CSUMF_CSS$avg_agg_support_3adjust_Repubs <- ifelse(CSUMF_CSS$Repub_speakers > 2,
                                                       CSUMF_CSS$avg_agg_support_Repubs, 
                                                       CSUMF_CSS$avg_agg_support_Repubs*CSUMF_CSS$Repub_speakers/3)
    
    CSUMF_CSS$avg_agg_support_Dems <- CSUMF_CSS$avg_agg_support_Dems - 0.5
    CSUMF_CSS$avg_agg_support_3adjust_Dem <- ifelse(CSUMF_CSS$Dem_speakers > 2,
                                                    CSUMF_CSS$avg_agg_support_Dems, 
                                                    CSUMF_CSS$avg_agg_support_Dems*CSUMF_CSS$Dem_speakers/3)
    
    CSUMF_CSS <- CSUMF_CSS %>%
      mutate(across(c("avg_agg_support", "Full_set_speakers","avg_agg_support_5adjust",
                      "avg_agg_support_Dems","avg_agg_support_Repubs","Repub_speakers_full",
                      "Dem_speakers_full","avg_agg_support_3adjust_Repubs","avg_agg_support_3adjust_Dem"), 
                    ~ ifelse(is.na(.), 0, .)))

    Early_Speeches_Only <- CSUMF_Crises
    Early_Speeches_Only <- Early_Speeches_Only[c(1:10,15)]
    
    DF_Early_Speeches_Only <- merge(handcoded, Early_Speeches_Only, by = "crisno")
    DF_Early_Speeches_Only <- subset(DF_Early_Speeches_Only, 
                                     as.Date(as.character(DF_Early_Speeches_Only$date), "%Y-%m-%d") < 
                                       as.Date(as.character(DF_Early_Speeches_Only$US_force_init), "%Y%m%d"))
    
    DF_Early_Speeches_Only$aggregate_support <- DF_Early_Speeches_Only$Advocates_for_Use_of_American_Military_Force + 
      DF_Early_Speeches_Only$Advocates_for_Use_of_American_Ground_Troops + 
      DF_Early_Speeches_Only$Advocates_for_Use_of_American_Air_Assets + 
      DF_Early_Speeches_Only$Advocates_for_Use_of_American_Naval_Assets
    DF_Early_Speeches_Only$aggregate_opposition <- DF_Early_Speeches_Only$Advocates_against_Use_of_American_Military_Force + 
      DF_Early_Speeches_Only$Advocates_against_Use_of_American_Ground_Troops + 
      DF_Early_Speeches_Only$Advocates_against_Use_of_American_Air_Assets + 
      DF_Early_Speeches_Only$Advocates_against_Use_of_American_Naval_Assets
    
    collapse2_early <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID, 
                                 FUN=sum, data=DF_Early_Speeches_Only)
    collapse2_early$individual_agg_support <- collapse2_early$aggregate_support.sum/(collapse2_early$aggregate_support.sum+collapse2_early$aggregate_opposition.sum)

    # 同上：整行剔除NA
    collapse2_early <- subset(collapse2_early, !is.na(collapse2_early$individual_agg_support))
    collapse2_early$count <- 1
    
    collapse2b_early <- summaryBy(individual_agg_support + count ~ crisname + crisno, 
                                  FUN=sum, data=collapse2_early)
    collapse2b_early$avg_agg_support <- collapse2b_early$individual_agg_support.sum/collapse2b_early$count.sum
    
    SPEAKERS_Pre_Init <- collapse2b_early[c("crisno","count.sum")]
    SPEAKERS_Pre_Init$Pre_init_speakers <- SPEAKERS_Pre_Init$count.sum
    SPEAKERS_Pre_Init <- SPEAKERS_Pre_Init[c("crisno","Pre_init_speakers")]
    
    DF_Early_Speeches_Only2 <- collapse2b_early
    DF_Early_Speeches_Only2 <- DF_Early_Speeches_Only2[c(1,4)]
    DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY <- DF_Early_Speeches_Only2$avg_agg_support
    DF_Early_Speeches_Only2 <- DF_Early_Speeches_Only2[c(1,3)]
    DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY <- DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY - 0.5
    DF_Early_Speeches_Only2 <- merge(DF_Early_Speeches_Only2, SPEAKERS_Pre_Init, by = "crisno")
    
    CSUMF_CSS <- merge(CSUMF_CSS, DF_Early_Speeches_Only2, by = "crisno", all.x = T)
    CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY <- ifelse(is.na(CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY),
                                                              CSUMF_CSS$avg_agg_support,
                                                              CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY)
    CSUMF_CSS$Pre_init_speakers <- ifelse(is.na(CSUMF_CSS$Pre_init_speakers),
                                          CSUMF_CSS$Full_set_speakers,
                                          CSUMF_CSS$Pre_init_speakers)
    
    CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust <- ifelse(CSUMF_CSS$Pre_init_speakers > 4,
                                                                      CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY, 
                                                                      CSUMF_CSS$Pre_init_speakers*.2*CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY)
    
    CSUMF_CSS <- CSUMF_CSS %>%
      mutate(across(c("avg_agg_support", "Full_set_speakers","avg_agg_support_5adjust",
                      "avg_agg_support_Dems","avg_agg_support_Repubs","Repub_speakers_full",
                      "Dem_speakers_full","avg_agg_support_3adjust_Repubs","avg_agg_support_3adjust_Dem"), 
                    ~ ifelse(is.na(.), 0, .)))

    Multivariate_DF <- CSUMF_CSS[c("crisno","avg_agg_support_Pre_Use_of_Force_ONLY_5adjust","avg_agg_support_5adjust")]
    Multivariate_DF <- merge(Covariates, Multivariate_DF, by = "crisno")

    # basic_n：join一次，诊断打印和total_basic_n复用
    mapped_all <- label_data_valid %>%
      dplyr::select(original_row_id) %>%
      dplyr::left_join(dataset %>% dplyr::select(original_row_id, crisno, tweet), by = "original_row_id")
    zero_shot_valid <- mapped_all %>%
      filter(!is.na(crisno), tweet == 0)
    collapse2_with_basic <- zero_shot_valid %>%
      group_by(crisno) %>%
      summarise(total_basic_n = dplyr::n(), .groups = "drop")

    Multivariate_DF <- Multivariate_DF %>%
      left_join(collapse2_with_basic, by = "crisno")

    {
      cat("\n==== 基数校验 (", model_col, ") ====\n", sep = "")
      cat("0-shot CSV总行:", nrow(label_data), "\n")
      cat("0-shot 解析有效行:", valid_count, "\n")
      cat("有效且能映射到dataset的行:", nrow(mapped_all), "\n")
      cat("其中tweet==0的行:", sum(mapped_all$tweet == 0, na.rm = TRUE), "\n")
      cat("tweet==0且crisno非缺失的行:", sum(mapped_all$tweet == 0 & !is.na(mapped_all$crisno), na.rm = TRUE), "\n")
      cat("按crisno聚合后的总和(用于basic_n): ",
          zero_shot_valid %>% dplyr::count(crisno) %>% dplyr::pull(n) %>% sum(),
          "\n", sep = "")

      cat("==== 覆盖校验 (", model_col, ") ====\n", sep = "")
      cov_crisno <- unique(Covariates$crisno)
      kept_rows <- zero_shot_valid %>% dplyr::filter(crisno %in% cov_crisno)
      cat("0-shot有效tweet==0总行:", nrow(zero_shot_valid), "\n")
      cat("0-shot crisno个数:", dplyr::n_distinct(zero_shot_valid$crisno), "\n")
      cat("能在Covariates中找到crisno的0-shot行:", nrow(kept_rows), "\n")
      cat("差额(被内连接丢掉的0-shot行):", nrow(zero_shot_valid) - nrow(kept_rows), "\n")
    }

    Multivariate_DF$crisno_factor <- as.factor(Multivariate_DF$crisno)

    processed_data_list[[model_col]] <- Multivariate_DF
  }

  if (!"llama_70b" %in% names(processed_data_list)) {
    cat("错误：未能成功处理 llama_70b\n")
    next
  }

  if (!"original_label" %in% colnames(label_data)) {
    cat("错误：未找到 original_label 列\n")
    next
  }

  # 解析original_label，供Bootstrap重抽样使用
  parsed_results_human <- lapply(label_data[["original_label"]], parse_and_validate_label)
  valid_indices_human <- sapply(parsed_results_human, function(x) x$valid)
  label_data_valid_human <- label_data[valid_indices_human, ]

  if (nrow(label_data_valid_human) == 0) {
    cat("错误：original_label 没有解析出有效数据\n")
    next
  }

  label_data_valid_human$Advocates_for_Use_of_American_Military_Force <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[1])
  label_data_valid_human$Advocates_for_Use_of_American_Ground_Troops <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[2])
  label_data_valid_human$Advocates_for_Use_of_American_Air_Assets <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[3])
  label_data_valid_human$Advocates_for_Use_of_American_Naval_Assets <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[4])
  label_data_valid_human$Advocates_against_Use_of_American_Military_Force <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[5])
  label_data_valid_human$Advocates_against_Use_of_American_Ground_Troops <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[6])
  label_data_valid_human$Advocates_against_Use_of_American_Air_Assets <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[7])
  label_data_valid_human$Advocates_against_Use_of_American_Naval_Assets <- sapply(parsed_results_human[valid_indices_human], function(x) x$values[8])

  label_data_selected_human <- label_data_valid_human %>%
    dplyr::select(original_row_id,
                  Advocates_for_Use_of_American_Military_Force,
                  Advocates_for_Use_of_American_Ground_Troops,
                  Advocates_for_Use_of_American_Air_Assets,
                  Advocates_for_Use_of_American_Naval_Assets,
                  Advocates_against_Use_of_American_Military_Force,
                  Advocates_against_Use_of_American_Ground_Troops,
                  Advocates_against_Use_of_American_Air_Assets,
                  Advocates_against_Use_of_American_Naval_Assets)

  df_llm <- processed_data_list[["llama_70b"]]
  
  cat("\nLLM数据集大小：", nrow(df_llm), "个危机\n")

  # 使用LLM数据作为基础结构（包含所有危机和协变量）
  dsl_data <- df_llm %>%
    dplyr::select(crisno, crisno_factor, avg_agg_support_Pre_Use_of_Force_ONLY_5adjust, 
                  effcaprat, yrtrig, AVERAGE_POLAR_BTWN_CHAMBERS_D1, 
                  net_approval, UNRATE, Cold_War, distance_USA, ONGOING_WAR, 
                  continuing_war, theta2_mean, prez_party, YM_HSM,
                  prez_copartisans, percent_repub.mean, US_high_act)
  
  # LLM预测值：保留NA，不填0，和reanalysis依赖lm()的na.omit行为一致
  dsl_data$congressional_support_llm <- df_llm$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust

  cat("  包含NA的congressional_support_llm数：", sum(is.na(dsl_data$congressional_support_llm)), "\n")
  cat("  数据集大小：", nrow(dsl_data), "\n")
  
  dsl_data$us_high_act_numeric <- as.numeric(as.character(dsl_data$US_high_act))
  dsl_data$congressional_support_score_llm <- dsl_data$congressional_support_llm

  dsl_data <- dsl_data %>%
    rename(
      average_polar_btwn_chambers_d1 = AVERAGE_POLAR_BTWN_CHAMBERS_D1,
      unrate = UNRATE,
      cold_war = Cold_War,
      distance_usa = distance_USA,
      ongoing_war = ONGOING_WAR,
      ym_hsm = YM_HSM,
      percent_repubmean = percent_repub.mean
    )
  
  # 输出
  results_file <- paste0("DSL_Hulme_APSR_2025_bootstrap_", sample_size, ".csv")
  in_context <- "0-shot"
  column_headers <- c("bootstrap_iter", "shot", "model", "estimate_name", 
                      "basic_n_labeled", "n_labeled", "n_total", "method",
                      "estimate", "se", "ci_lower", "ci_upper", "p_value")
  
  write.table(t(column_headers), 
              file = results_file, 
              sep = ",", 
              col.names = FALSE, 
              row.names = FALSE, 
              quote = FALSE)
  
  cat("\n创建结果文件:", results_file, "\n")
  cat("样本大小设置为:", sample_size, "\n")
  
  Early_Speeches_Only <- CSUMF_Crises[c(1:10,15)]
  
  # Bootstrap循环
  for (bootstrap_iter in 1:100) {
    cat("\n=== Bootstrap迭代", bootstrap_iter, "===\n")

    # 基础层抽样：tweet==0且human_labeled==1，与handcoded口径一致
    base_speeches <- dataset %>%
      filter(tweet == 0, human_labeled == 1) %>%
      left_join(label_data_valid_human %>% dplyr::select(original_row_id), by = "original_row_id") %>%
      filter(!is.na(original_row_id))
    
    available_labeled <- nrow(base_speeches)
    cat("可用基础演讲数:", available_labeled, "\n")
    
    if (available_labeled < sample_size) {
      cat("警告: 可用基础演讲数", available_labeled, "小于请求的样本数", sample_size, "，跳过\n")
      next
    }
    
    # 基础层抽样
    set.seed(123 + bootstrap_iter + sample_size * 1000)
    sampled_indices <- sample(seq_len(nrow(base_speeches)), sample_size, replace = FALSE)
    
    base_speeches$is_labeled <- 0L
    base_speeches$is_labeled[sampled_indices] <- 1L
    
    # 基础层均匀抽样概率
    p_basic <- sample_size / available_labeled
    base_speeches$basic_sample_prob <- p_basic
    
    # 聚合到危机级并计算 agg_sample_prob
    crisis_agg <- base_speeches %>%
      group_by(crisno) %>%
      summarise(
        agg_sample_prob = 1 - prod(1 - basic_sample_prob),
        .groups = "drop"
      )
    
    # 聚合人类标注
    sampled_speeches <- base_speeches[base_speeches$is_labeled == 1L, ]
    
    # 只对抽中的
    if (nrow(sampled_speeches) > 0) {
      handcoded_sampled <- sampled_speeches %>%
        left_join(label_data_selected_human, by = "original_row_id") %>%
        filter(human_labeled == 1)
      
      if (nrow(handcoded_sampled) > 0) {
        handcoded_sampled$aggregate_support <- handcoded_sampled$Advocates_for_Use_of_American_Military_Force + 
          handcoded_sampled$Advocates_for_Use_of_American_Ground_Troops + 
          handcoded_sampled$Advocates_for_Use_of_American_Air_Assets + 
          handcoded_sampled$Advocates_for_Use_of_American_Naval_Assets
        handcoded_sampled$aggregate_opposition <- handcoded_sampled$Advocates_against_Use_of_American_Military_Force + 
          handcoded_sampled$Advocates_against_Use_of_American_Ground_Troops + 
          handcoded_sampled$Advocates_against_Use_of_American_Air_Assets + 
          handcoded_sampled$Advocates_against_Use_of_American_Naval_Assets
        
        # 重新聚合到危机级（NA行剔除，与主流程一致）
        collapse2_sampled <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID,
                                       FUN=sum, data=handcoded_sampled)
        collapse2_sampled$individual_agg_support <- collapse2_sampled$aggregate_support.sum /
          (collapse2_sampled$aggregate_support.sum + collapse2_sampled$aggregate_opposition.sum)
        collapse2_sampled <- subset(collapse2_sampled, !is.na(collapse2_sampled$individual_agg_support))
        collapse2_sampled$count <- 1

        collapse2b_sampled <- summaryBy(individual_agg_support + count ~ crisname + crisno,
                                        FUN=sum, data=collapse2_sampled)
        collapse2b_sampled$avg_agg_support <- collapse2b_sampled$individual_agg_support.sum/collapse2b_sampled$count.sum
        collapse2b_sampled$avg_agg_support <- collapse2b_sampled$avg_agg_support - 0.5
        Full_set_speakers_sampled <- collapse2b_sampled[c("crisno", "count.sum")]
        colnames(Full_set_speakers_sampled)[colnames(Full_set_speakers_sampled) == "count.sum"] <- "Full_set_speakers_sampled"

        # 计算 Pre_Use_of_Force_ONLY 版本
        DF_Early_Speeches_Only_sampled <- merge(handcoded_sampled, Early_Speeches_Only, by = "crisno")
        DF_Early_Speeches_Only_sampled <- subset(DF_Early_Speeches_Only_sampled,
                                                 as.Date(as.character(DF_Early_Speeches_Only_sampled$date), "%Y-%m-%d") <
                                                   as.Date(as.character(DF_Early_Speeches_Only_sampled$US_force_init), "%Y%m%d"))

        if (nrow(DF_Early_Speeches_Only_sampled) > 0) {
          DF_Early_Speeches_Only_sampled$aggregate_support <- DF_Early_Speeches_Only_sampled$Advocates_for_Use_of_American_Military_Force +
            DF_Early_Speeches_Only_sampled$Advocates_for_Use_of_American_Ground_Troops +
            DF_Early_Speeches_Only_sampled$Advocates_for_Use_of_American_Air_Assets +
            DF_Early_Speeches_Only_sampled$Advocates_for_Use_of_American_Naval_Assets
          DF_Early_Speeches_Only_sampled$aggregate_opposition <- DF_Early_Speeches_Only_sampled$Advocates_against_Use_of_American_Military_Force +
            DF_Early_Speeches_Only_sampled$Advocates_against_Use_of_American_Ground_Troops +
            DF_Early_Speeches_Only_sampled$Advocates_against_Use_of_American_Air_Assets +
            DF_Early_Speeches_Only_sampled$Advocates_against_Use_of_American_Naval_Assets

          collapse2_early_sampled <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID,
                                               FUN=sum, data=DF_Early_Speeches_Only_sampled)
          collapse2_early_sampled$individual_agg_support <- collapse2_early_sampled$aggregate_support.sum/(collapse2_early_sampled$aggregate_support.sum+collapse2_early_sampled$aggregate_opposition.sum)
          collapse2_early_sampled <- subset(collapse2_early_sampled, !is.na(collapse2_early_sampled$individual_agg_support))
          collapse2_early_sampled$count <- 1

          collapse2b_early_sampled <- summaryBy(individual_agg_support + count ~ crisname + crisno,
                                                FUN=sum, data=collapse2_early_sampled)
          collapse2b_early_sampled$avg_agg_support <- collapse2b_early_sampled$individual_agg_support.sum/collapse2b_early_sampled$count.sum
          collapse2b_early_sampled$avg_agg_support <- collapse2b_early_sampled$avg_agg_support - 0.5

          Pre_init_speakers_sampled <- collapse2b_early_sampled[c("crisno", "count.sum")]
          colnames(Pre_init_speakers_sampled)[colnames(Pre_init_speakers_sampled) == "count.sum"] <- "Pre_init_speakers_sampled"

          # 合并早期发言均值与人数
          collapse2b_sampled <- merge(collapse2b_sampled,
                                      collapse2b_early_sampled[c("crisno", "avg_agg_support")],
                                      by = "crisno", all.x = TRUE)
          collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY <- collapse2b_sampled$avg_agg_support.y
          collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY <- ifelse(is.na(collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY),
                                                                             collapse2b_sampled$avg_agg_support.x,
                                                                             collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY)
          collapse2b_sampled <- merge(collapse2b_sampled, Pre_init_speakers_sampled, by = "crisno", all.x = TRUE)
        } else {
          collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY <- collapse2b_sampled$avg_agg_support
          collapse2b_sampled$Pre_init_speakers_sampled <- NA_real_
        }

        # 缺失时回退到Full_set_speakers
        collapse2b_sampled <- merge(collapse2b_sampled, Full_set_speakers_sampled, by = "crisno", all.x = TRUE)
        collapse2b_sampled$Pre_init_speakers_sampled <- ifelse(is.na(collapse2b_sampled$Pre_init_speakers_sampled),
                                                                collapse2b_sampled$Full_set_speakers_sampled,
                                                                collapse2b_sampled$Pre_init_speakers_sampled)

        # 5adjust：与llm端保持同一套收缩公式
        collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust <- ifelse(
          collapse2b_sampled$Pre_init_speakers_sampled > 4,
          collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY,
          collapse2b_sampled$Pre_init_speakers_sampled * .2 * collapse2b_sampled$avg_agg_support_Pre_Use_of_Force_ONLY
        )

        # 创建危机级标注数据
        crisis_labeled <- collapse2b_sampled[c("crisno", "avg_agg_support_Pre_Use_of_Force_ONLY_5adjust")]
        crisis_labeled$congressional_support_score_labeled <- crisis_labeled$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust

      } else {
        # 没有抽中的标注演讲
        crisis_labeled <- data.frame(crisno = character(0), congressional_support_score_labeled = numeric(0))
      }
    } else {
      crisis_labeled <- data.frame(crisno = character(0), congressional_support_score_labeled = numeric(0))
    }
    
    current_df <- dsl_data %>%
      left_join(crisis_labeled, by = "crisno") %>%
      left_join(crisis_agg, by = "crisno")

    basic_n_labeled_sum <- sum(base_speeches$is_labeled, na.rm = TRUE)

    models_config <- list(
      list(
        name = "M1",
        formula = "us_high_act_numeric ~ congressional_support_score_labeled + effcaprat + yrtrig + average_polar_btwn_chambers_d1 + net_approval + unrate + cold_war + distance_usa + ongoing_war + continuing_war",
        estimate_name = "Informal Cong. Sentiment (M1)"
      ),
      list(
        name = "M2",
        formula = "us_high_act_numeric ~ congressional_support_score_labeled + effcaprat + yrtrig + average_polar_btwn_chambers_d1 + net_approval + unrate + cold_war + distance_usa + ongoing_war + continuing_war + theta2_mean + prez_party + ym_hsm",
        estimate_name = "Informal Cong. Sentiment (M2)"
      ),
      list(
        name = "M7",
        formula = "us_high_act_numeric ~ congressional_support_score_labeled + effcaprat + yrtrig + prez_copartisans + average_polar_btwn_chambers_d1 + net_approval + percent_repubmean + unrate + cold_war + distance_usa + ongoing_war + continuing_war",
        estimate_name = "Informal Cong. Sentiment (M7)"
      ),
      list(
        name = "M8",
        formula = "us_high_act_numeric ~ congressional_support_score_labeled + effcaprat + yrtrig + prez_copartisans + average_polar_btwn_chambers_d1 + net_approval + percent_repubmean + unrate + cold_war + distance_usa + ongoing_war + continuing_war + theta2_mean + prez_party + ym_hsm",
        estimate_name = "Informal Cong. Sentiment (M8)"
      )
    )

    for (model_config in models_config) {
      cat("\n--- 处理模型：", model_config$name, "---\n")
      
      tryCatch({
        cat("  开始运行DSL...\n")

        out <- dsl(
          model = "lm",
          formula = as.formula(model_config$formula),
          predicted_var = "congressional_support_score_labeled",
          prediction = "congressional_support_score_llm",
          sample_prob = "agg_sample_prob",
          data = current_df
        )

        n_labeled_actual <- tryCatch({ out$internal$num_expert }, error = function(e) sample_size)
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
          if (!is.null(rn) && "congressional_support_score_labeled" %in% rn) {
            idx <- match("congressional_support_score_labeled", rn)
          }
          
          if (!is.na(idx)) {
            est <- suppressWarnings(as.numeric(coef_tbl[idx, "Estimate"]))
            se  <- suppressWarnings(as.numeric(coef_tbl[idx, "Std. Error"]))

            if ("CI Lower" %in% colnames(coef_tbl)) {
              ciL <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Lower"]))
              ciU <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Upper"]))
            } else {
              ciL <- est - 1.96 * se
              ciU <- est + 1.96 * se
            }

            if ("p value" %in% colnames(coef_tbl)) {
              pval <- suppressWarnings(as.numeric(coef_tbl[idx, "p value"]))
            } else if ("Pr(>|t|)" %in% colnames(coef_tbl)) {
              pval <- suppressWarnings(as.numeric(coef_tbl[idx, "Pr(>|t|)"]))
            }
          }
        }

        result_row <- data.frame(
          bootstrap_iter = bootstrap_iter,
          shot = in_context,
          model = "llama_70b",
          estimate_name = model_config$estimate_name,
          basic_n_labeled = basic_n_labeled_sum,
          n_labeled = n_labeled_actual,
          n_total = n_total,
          method = "dsl",
          estimate = est,
          se = se,
          ci_lower = ciL,
          ci_upper = ciU,
          p_value = pval,
          stringsAsFactors = FALSE
        )
        
        # 写入
        write.table(result_row, 
                    file = results_file, 
                    sep = ",", 
                    append = TRUE, 
                    col.names = FALSE, 
                    row.names = FALSE)
        
        cat("  ✓ DSL成功，结果已写入文件\n")
        
      }, error = function(e) {
        cat("  ✗ DSL错误:", e$message, "\n")

        na_result <- data.frame(
          bootstrap_iter = bootstrap_iter,
          shot = in_context,
          model = "llama_70b",
          estimate_name = model_config$estimate_name,
          basic_n_labeled = basic_n_labeled_sum,
          n_labeled = sample_size,
          n_total = nrow(current_df),
          method = "dsl",
          estimate = NA,
          se = NA,
          ci_lower = NA,
          ci_upper = NA,
          p_value = NA,
          stringsAsFactors = FALSE
        )
        
        write.table(na_result, 
                    file = results_file, 
                    sep = ",", 
                    append = TRUE, 
                    col.names = FALSE, 
                    row.names = FALSE)
        
        cat("  NA结果已写入文件\n")
      })
    }
  }
  
  cat("\nDSL分析完成! 结果已保存到:", results_file, "\n")
}

cat("\n所有Bootstrap迭代已完成。输出文件：", results_file, "\n")