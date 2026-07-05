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
sample_size <- 50
bootstrap_iterations <- 100

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
  
  # original_label 和 llama_70b 
  models_to_process <- c("original_label", "llama_70b")
  
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

    handcoded_with_basic_n <- handcoded %>%
      group_by(crisname, crisno, MasterID) %>%
      summarise(basic_n = dplyr::n(), .groups = "drop")
    
    # 第一次聚合：按危机-议员
    collapse2 <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID, 
                           FUN=sum, data=handcoded)
    collapse2$individual_agg_support <- collapse2$aggregate_support.sum / 
      (collapse2$aggregate_support.sum + collapse2$aggregate_opposition.sum)

    collapse2$individual_agg_support <- ifelse(is.na(collapse2$individual_agg_support), 
                                               0, 
                                               collapse2$individual_agg_support)
    collapse2$count <- 1

    collapse2 <- collapse2 %>%
      left_join(handcoded_with_basic_n, by = c("crisname", "crisno", "MasterID"))

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
    
    # collapse2_early <- subset(collapse2_early, !is.na(collapse2_early$individual_agg_support))
    collapse2_early$individual_agg_support <- ifelse(is.na(collapse2_early$individual_agg_support), 
                                                     0, 
                                                     collapse2_early$individual_agg_support)
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

    # basic_n
    zero_shot_map <- dataset %>%
      dplyr::select(original_row_id, crisno, tweet)
    zero_shot_valid <- label_data_valid %>%
      dplyr::select(original_row_id) %>%
      left_join(zero_shot_map, by = "original_row_id") %>%
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
      mapped_all <- label_data_valid %>%
        dplyr::select(original_row_id) %>%
        dplyr::left_join(dataset %>% dplyr::select(original_row_id, crisno, tweet), by = "original_row_id")
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

  if (!"original_label" %in% names(processed_data_list) || !"llama_70b" %in% names(processed_data_list)) {
    cat("错误：未能成功处理 original_label 或 llama_70b\n")
    next
  }

  df_human <- processed_data_list[["original_label"]]
  df_llm <- processed_data_list[["llama_70b"]]
  
  cat("\n处理前数据集大小：\n")
  cat("  df_human:", nrow(df_human), "个危机\n")
  cat("  df_llm:", nrow(df_llm), "个危机\n")

  all_crisno <- union(df_human$crisno, df_llm$crisno)
  cat("  总危机数（并集）：", length(all_crisno), "\n")
  
  human_only <- setdiff(df_human$crisno, df_llm$crisno)
  llm_only <- setdiff(df_llm$crisno, df_human$crisno)
  
  if (length(human_only) > 0) {
    cat("  只在human中的危机数：", length(human_only), "\n")
  }
  if (length(llm_only) > 0) {
    cat("  只在llm中的危机数：", length(llm_only), "\n")
  }

  full_crisno_df <- data.frame(crisno = all_crisno)

  df_human_full <- full_crisno_df %>%
    left_join(df_human, by = "crisno") %>%
    arrange(crisno)
  
  df_llm_full <- full_crisno_df %>%
    left_join(df_llm, by = "crisno") %>%
    arrange(crisno)
  
  cat("\n对齐后的数据集大小：", nrow(df_human_full), "个危机\n")

  if (nrow(df_human_full) != nrow(df_llm_full)) {
    stop("错误：全连接后行数仍不一致！")
  }

  dsl_data <- df_human_full %>%
    dplyr::select(crisno, crisno_factor, avg_agg_support_Pre_Use_of_Force_ONLY_5adjust, 
                  effcaprat, yrtrig, AVERAGE_POLAR_BTWN_CHAMBERS_D1, 
                  net_approval, UNRATE, Cold_War, distance_USA, ONGOING_WAR, 
                  continuing_war, theta2_mean, prez_party, YM_HSM,
                  prez_copartisans, percent_repub.mean, total_basic_n, US_high_act)  # 添加US_high_act
  
  # 添加LLM预测
  dsl_data$congressional_support_llm <- df_llm_full$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust
  
  # 添加人工标注
  dsl_data$congressional_support_human <- df_human_full$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust
  
  # 对于缺失的值，用0填充
  dsl_data <- dsl_data %>%
    mutate(
      congressional_support_llm = ifelse(is.na(congressional_support_llm), 0, congressional_support_llm),
      congressional_support_human = ifelse(is.na(congressional_support_human), 0, congressional_support_human),
      total_basic_n = ifelse(is.na(total_basic_n), 0, total_basic_n)
    )
  
  dsl_data <- dsl_data %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .)))
  
  cat("  包含NA的congressional_support_human数：", sum(is.na(df_human_full$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust)), "\n")
  cat("  包含NA的congressional_support_llm数：", sum(is.na(df_llm_full$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust)), "\n")
  cat("  填充后的最终数据集大小：", nrow(dsl_data), "\n")
  
  dsl_data$us_high_act_numeric <- as.numeric(as.character(dsl_data$US_high_act))

  dsl_data$congressional_support_score_llm <- dsl_data$congressional_support_llm
  dsl_data$congressional_support_score_human <- dsl_data$congressional_support_human

  dsl_data <- dsl_data %>%
    rename(
      average_polar_btwn_chambers_d1 = AVERAGE_POLAR_BTWN_CHAMBERS_D1,
      unrate = UNRATE,
      cold_war = Cold_War,
      distance_usa = distance_USA,  # 修复大小写
      ongoing_war = ONGOING_WAR,
      ym_hsm = YM_HSM,
      percent_repubmean = percent_repub.mean,
      basic_n_labeled = total_basic_n
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
  cat("Bootstrap迭代次数:", bootstrap_iterations, "\n")
  
  # Bootstrap循环
  for (bootstrap_iter in 1:bootstrap_iterations) {
    cat("\n=== Bootstrap迭代", bootstrap_iter, "===\n")

    available_labeled <- nrow(dsl_data)
    if (available_labeled < sample_size) {
      cat("警告: 可用样本数", available_labeled, "小于请求的样本数", sample_size, "\n")
      actual_sample_size <- available_labeled
    } else {
      actual_sample_size <- sample_size
    }

    set.seed(123 + bootstrap_iter + sample_size * 1000)
    labeled_indices <- sample(1:nrow(dsl_data), actual_sample_size, replace = FALSE)

    current_df <- dsl_data
    current_df$is_labeled <- 0
    current_df$is_labeled[labeled_indices] <- 1

    current_df$congressional_support_score_labeled <- ifelse(current_df$is_labeled == 1, 
                                                             current_df$congressional_support_score_human, 
                                                             NA)

    basic_n_labeled_sum <- current_df %>%
      filter(is_labeled == 1) %>%
      dplyr::select(crisno, basic_n_labeled) %>%
      dplyr::distinct(crisno, .keep_all = TRUE) %>%
      summarise(total = sum(basic_n_labeled, na.rm = TRUE)) %>%
      pull(total)

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
          n_labeled = actual_sample_size,
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