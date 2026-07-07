setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

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

dataset <- read_csv("dataset.csv")
CSUMF_Crises <- read_csv("Data_files/CSUMF_Crises.csv")
Covariates <- read_csv("Data_files/Covariates.csv")

suffix_esc <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
label_files <- list.files(path    = ANNOTATION_DIR,
                          pattern = paste0(".*_shot_Hulme_APSR_2025", suffix_esc, "$"))
cat("找到的标注文件：", label_files, "\n")

all_results <- list()

for (shot_file in label_files) {
  cat("\n=== 正在处理文件：", shot_file, "===\n")

  shot_num <- as.numeric(gsub("(\\d+)_shot.*", "\\1", shot_file))
  cat("Shot数量：", shot_num, "\n")

  label_data <- read_csv(file.path(ANNOTATION_DIR, shot_file))
  cat("总行数：", nrow(label_data), "\n")

  llm_columns <- setdiff(colnames(label_data), c("original_row_id", "original_label"))
  cat("LLM模型数量：", length(llm_columns), "\n")

  if (shot_num == 0) {
    columns_to_process <- c("original_label", llm_columns)
    cat("0-shot模式：将处理original_label +", length(llm_columns), "个LLM模型\n")
  } else {
    columns_to_process <- llm_columns
    cat(shot_num, "-shot模式：将处理", length(llm_columns), "个LLM模型\n")
  }

  for (col_name in columns_to_process) {
    cat("\n  --- 处理模型：", col_name, "---\n")
    tryCatch({

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
    
    parsed_results <- lapply(label_data[[col_name]], parse_and_validate_label)
    
    valid_indices <- sapply(parsed_results, function(x) x$valid)
    valid_count <- sum(valid_indices)
    total_count <- length(valid_indices)
    invalid_count <- total_count - valid_count
    
    cat("    数据质量统计：\n")
    cat("      总行数：", total_count, "\n")
    cat("      有效行数：", valid_count, "\n")
    cat("      无效行数：", invalid_count, "\n")
    cat("      有效率：", round(valid_count/total_count*100, 1), "%\n")
    cat("    继续处理模型", col_name, "\n")
    
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

    # 覆盖校验：标注CSV到Covariates的覆盖差额
    {
      cat("\n==== 基数校验 (", col_name, ") ====\n", sep = "")
      cat("标注CSV总行:", nrow(label_data), "\n")
      cat("解析有效行:", sum(valid_indices), "\n")
      mapped_all <- label_data_valid %>%
        dplyr::select(original_row_id) %>%
        dplyr::left_join(dataset %>% dplyr::select(original_row_id, crisno, tweet), by = "original_row_id")
      cat("有效且能映射到dataset的行:", nrow(mapped_all), "\n")
      cat("其中tweet==0的行:", sum(mapped_all$tweet == 0, na.rm = TRUE), "\n")
      cat("tweet==0且crisno非缺失的行:", sum(mapped_all$tweet == 0 & !is.na(mapped_all$crisno), na.rm = TRUE), "\n")
      zero_shot_valid <- mapped_all %>% dplyr::filter(tweet == 0, !is.na(crisno))
      cat("按crisno聚合后的总和(用于basic_n): ",
          zero_shot_valid %>% dplyr::count(crisno) %>% dplyr::pull(n) %>% sum(),
          "\n", sep = "")

      cat("==== 覆盖校验 (", col_name, ") ====\n", sep = "")
      cov_crisno <- unique(Covariates$crisno)
      kept_rows <- zero_shot_valid %>% dplyr::filter(crisno %in% cov_crisno)
      cat("有效tweet==0总行:", nrow(zero_shot_valid), "\n")
      cat("crisno个数:", dplyr::n_distinct(zero_shot_valid$crisno), "\n")
      cat("能在Covariates中找到crisno的行:", nrow(kept_rows), "\n")
      cat("差额(被内连接丢掉的行):", nrow(zero_shot_valid) - nrow(kept_rows), "\n")
    }

CSUMF_speeches <- dataset %>%
  left_join(label_data_selected, by = "original_row_id")

handcoded <- subset(CSUMF_speeches, CSUMF_speeches$human_labeled == 1)
handcoded <- subset(handcoded, handcoded$tweet == 0)

handcoded$aggregate_support <- handcoded$Advocates_for_Use_of_American_Military_Force + handcoded$Advocates_for_Use_of_American_Ground_Troops + handcoded$Advocates_for_Use_of_American_Air_Assets + handcoded$Advocates_for_Use_of_American_Naval_Assets
handcoded$aggregate_opposition <- handcoded$Advocates_against_Use_of_American_Military_Force + handcoded$Advocates_against_Use_of_American_Ground_Troops + handcoded$Advocates_against_Use_of_American_Air_Assets + handcoded$Advocates_against_Use_of_American_Naval_Assets

collapse2 <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID, FUN=sum, data=handcoded)
collapse2$individual_agg_support <- collapse2$aggregate_support.sum/(collapse2$aggregate_support.sum+collapse2$aggregate_opposition.sum)
collapse2 <- subset(collapse2,!is.na(collapse2$individual_agg_support))
collapse2$count <- 1
collapse2b <- summaryBy(individual_agg_support + count ~ crisname + crisno, FUN=sum, data=collapse2)
collapse2b$avg_agg_support <- collapse2b$individual_agg_support.sum/collapse2b$count.sum
SPEAKERS_FULL <- collapse2b[c("crisno","count.sum")]
SPEAKERS_FULL$Full_set_speakers <- SPEAKERS_FULL$count.sum
SPEAKERS_FULL <- SPEAKERS_FULL[c("crisno","Full_set_speakers")]
CSUMF_CSS <- collapse2b
CSUMF_CSS <- CSUMF_CSS[c(1,2,5)]
CSUMF_CSS <- merge(CSUMF_CSS,SPEAKERS_FULL, by = "crisno")

CSUMF_CSS$avg_agg_support <- CSUMF_CSS$avg_agg_support-0.5
CSUMF_CSS$avg_agg_support_5adjust <- ifelse(CSUMF_CSS$Full_set_speakers > 4,CSUMF_CSS$avg_agg_support, CSUMF_CSS$avg_agg_support*CSUMF_CSS$Full_set_speakers*.2)

    USA_crises <- CSUMF_Crises[c("crisno","crisname","Vietnam War","US_high_act")]
    CSUMF_CSS <- merge(USA_crises,CSUMF_CSS, by = c("crisno"), all.x = T)
    CSUMF_CSS <- CSUMF_CSS[, !colnames(CSUMF_CSS) %in% "crisname.y"]
    colnames(CSUMF_CSS)[colnames(CSUMF_CSS) == "crisname.x"] <- "crisname"
    
    CSUMF_CSS$continuing_war <- 0
    CSUMF_CSS$continuing_war <- ifelse(CSUMF_CSS$crisname == "KOREAN WAR II"| CSUMF_CSS$crisname == "KOREAN WAR III"|CSUMF_CSS$crisname == "PLEIKU"|CSUMF_CSS$crisname == "Vietnam Escalation"|CSUMF_CSS$crisname == "Vietnam 67 Key Votes"|CSUMF_CSS$crisname == "TET OFFENSIVE"|CSUMF_CSS$crisname == "VIETNAM SPRING OFF."|CSUMF_CSS$crisname == "INVASION OF CAMBODIA"|CSUMF_CSS$crisname == "INVASION OF LAOS II"|CSUMF_CSS$crisname == "VIETNAM PORTS MINING"|CSUMF_CSS$crisname == "CHRISTMAS BOMBING"
                                       |CSUMF_CSS$crisname == "Iraq Surge"|CSUMF_CSS$crisname == "Afghan Surge",1,0)
    
    CSUMF_CSS$US_high_act <- ifelse(CSUMF_CSS$crisname == "GULF OF TONKIN",  15, CSUMF_CSS$US_high_act)
    
    # 添加党派支持分数计算（与原始版本保持一致）
collapse2$party <- collapse2$MasterID

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

collapse2$sixth <- substr(substrRight(collapse2$MasterID,6),1,1)
collapse2$seventh <- substr(substrRight(collapse2$MasterID,7),1,1)
collapse2$party <- ifelse(collapse2$sixth == 0,collapse2$seventh,collapse2$sixth)
collapse2$party <- ifelse(collapse2$party == 1, 100,collapse2$party)
collapse2$party <- ifelse(collapse2$party == 2, 200,collapse2$party)
collapse2 <- subset(collapse2,collapse2$party == 100 |collapse2$party == 200)
collapse2b <- summaryBy(individual_agg_support + count ~ crisname + crisno + party, FUN=sum, data=collapse2)
collapse2b$avg_agg_support <- collapse2b$individual_agg_support.sum/collapse2b$count.sum

Repub_speakers <- subset(collapse2b, collapse2b$party == 200)
Repub_speakers <- Repub_speakers[c(2,5)] 
colnames(Repub_speakers)[colnames(Repub_speakers) == "count.sum"] <- "Repub_speakers_full"

Dem_speakers <- subset(collapse2b, collapse2b$party == 100)
Dem_speakers <- Dem_speakers[c(2,5)] 
colnames(Dem_speakers)[colnames(Dem_speakers) == "count.sum"] <- "Dem_speakers_full"

CSUMF_CSS_party <- collapse2b
CSUMF_CSS_party <- CSUMF_CSS_party[c(1,2,3,6)]

CSUMF_CSS_Dems <- subset(CSUMF_CSS_party,CSUMF_CSS_party$party == 100)
CSUMF_CSS_Dems$avg_agg_support_Dems <- CSUMF_CSS_Dems$avg_agg_support
CSUMF_CSS_Dems <- CSUMF_CSS_Dems[c(2,5)]

CSUMF_CSS_Repubs <- subset(CSUMF_CSS_party,CSUMF_CSS_party$party == 200)
CSUMF_CSS_Repubs$avg_agg_support_Repubs <- CSUMF_CSS_Repubs$avg_agg_support
CSUMF_CSS_Repubs <- CSUMF_CSS_Repubs[c(2,5)]

CSUMF_CSS <- merge(CSUMF_CSS,CSUMF_CSS_Dems, by = "crisno", all.x = T)
CSUMF_CSS <- merge(CSUMF_CSS,CSUMF_CSS_Repubs, by = "crisno", all.x = T)
CSUMF_CSS <- merge(CSUMF_CSS,Repub_speakers, by = "crisno", all.x = T)
CSUMF_CSS <- merge(CSUMF_CSS,Dem_speakers, by = "crisno", all.x = T)

CSUMF_CSS$avg_agg_support_Repubs <- CSUMF_CSS$avg_agg_support_Repubs-0.5
CSUMF_CSS$avg_agg_support_3adjust_Repubs <- ifelse(CSUMF_CSS$Repub_speakers > 2,CSUMF_CSS$avg_agg_support_Repubs, CSUMF_CSS$avg_agg_support_Repubs*CSUMF_CSS$Repub_speakers/3)

CSUMF_CSS$avg_agg_support_Dems <- CSUMF_CSS$avg_agg_support_Dems-0.5
CSUMF_CSS$avg_agg_support_3adjust_Dem <- ifelse(CSUMF_CSS$Dem_speakers > 2,CSUMF_CSS$avg_agg_support_Dems, CSUMF_CSS$avg_agg_support_Dems*CSUMF_CSS$Dem_speakers/3)

    # NA值替换为0（与原始版本保持一致）
CSUMF_CSS <- CSUMF_CSS %>%
  mutate(across(c("avg_agg_support", "Full_set_speakers","avg_agg_support_5adjust","avg_agg_support_Dems","avg_agg_support_Repubs","Repub_speakers_full","Dem_speakers_full","avg_agg_support_3adjust_Repubs","avg_agg_support_3adjust_Dem"), ~ ifelse(is.na(.), 0, .)))

    # 添加动用前的支持分数计算（与原始版本保持一致）
Early_Speeches_Only <- CSUMF_Crises
Early_Speeches_Only <-  Early_Speeches_Only[c(1:10,15)]

DF_Early_Speeches_Only <- merge(handcoded,Early_Speeches_Only, by = "crisno")
DF_Early_Speeches_Only <- subset(DF_Early_Speeches_Only, as.Date(as.character(DF_Early_Speeches_Only$date), "%Y-%m-%d") < as.Date(as.character(DF_Early_Speeches_Only$US_force_init), "%Y%m%d"))

DF_Early_Speeches_Only$aggregate_support <- DF_Early_Speeches_Only$Advocates_for_Use_of_American_Military_Force + DF_Early_Speeches_Only$Advocates_for_Use_of_American_Ground_Troops + DF_Early_Speeches_Only$Advocates_for_Use_of_American_Air_Assets + DF_Early_Speeches_Only$Advocates_for_Use_of_American_Naval_Assets
DF_Early_Speeches_Only$aggregate_opposition <- DF_Early_Speeches_Only$Advocates_against_Use_of_American_Military_Force + DF_Early_Speeches_Only$Advocates_against_Use_of_American_Ground_Troops + DF_Early_Speeches_Only$Advocates_against_Use_of_American_Air_Assets + DF_Early_Speeches_Only$Advocates_against_Use_of_American_Naval_Assets

collapse2 <- summaryBy(aggregate_support + aggregate_opposition ~ crisname + crisno + MasterID, FUN=sum, data=DF_Early_Speeches_Only)
collapse2$individual_agg_support <- collapse2$aggregate_support.sum/(collapse2$aggregate_support.sum+collapse2$aggregate_opposition.sum)
collapse2 <- subset(collapse2,!is.na(collapse2$individual_agg_support))
collapse2$count <- 1

collapse2b <- summaryBy(individual_agg_support + count ~ crisname + crisno, FUN=sum, data=collapse2)
collapse2b$avg_agg_support <- collapse2b$individual_agg_support.sum/collapse2b$count.sum

SPEAKERS_Pre_Init <- collapse2b[c("crisno","count.sum")]
SPEAKERS_Pre_Init$Pre_init_speakers <- SPEAKERS_Pre_Init$count.sum
SPEAKERS_Pre_Init <- SPEAKERS_Pre_Init[c("crisno","Pre_init_speakers")]

DF_Early_Speeches_Only2 <- collapse2b
DF_Early_Speeches_Only2 <- DF_Early_Speeches_Only2[c(1,4)]
DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY <- DF_Early_Speeches_Only2$avg_agg_support
DF_Early_Speeches_Only2 <- DF_Early_Speeches_Only2[c(1,3)]
DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY <- DF_Early_Speeches_Only2$avg_agg_support_Pre_Use_of_Force_ONLY-0.5
DF_Early_Speeches_Only2 <- merge(DF_Early_Speeches_Only2,SPEAKERS_Pre_Init, by = "crisno")

CSUMF_CSS <- merge(CSUMF_CSS,DF_Early_Speeches_Only2, by = "crisno", all.x = T)
CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY <- ifelse(is.na(CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY),CSUMF_CSS$avg_agg_support,CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY)
CSUMF_CSS$Pre_init_speakers <- ifelse(is.na(CSUMF_CSS$Pre_init_speakers),CSUMF_CSS$Full_set_speakers,CSUMF_CSS$Pre_init_speakers)

    # 动用前的支持分数调整
CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust <- ifelse(CSUMF_CSS$Pre_init_speakers > 4,CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY, CSUMF_CSS$Pre_init_speakers*.2*CSUMF_CSS$avg_agg_support_Pre_Use_of_Force_ONLY)

    # 最后的NA值替换（与原始版本保持一致）
    CSUMF_CSS <- CSUMF_CSS %>%
      mutate(across(c("avg_agg_support", "Full_set_speakers","avg_agg_support_5adjust","avg_agg_support_Dems","avg_agg_support_Repubs","Repub_speakers_full","Dem_speakers_full","avg_agg_support_3adjust_Repubs","avg_agg_support_3adjust_Dem"), ~ ifelse(is.na(.), 0, .)))
    
Multivariate_DF <- CSUMF_CSS[c("crisno","avg_agg_support_Pre_Use_of_Force_ONLY_5adjust","avg_agg_support_5adjust")]
Multivariate_DF <- merge(Covariates,Multivariate_DF, by = "crisno")
    
    data <- Multivariate_DF
data$congressional_support_score <- data$avg_agg_support_Pre_Use_of_Force_ONLY_5adjust
data$us_high_act <- data$US_high_act
data$average_polar_btwn_chambers_d1 <- data$AVERAGE_POLAR_BTWN_CHAMBERS_D1
data$unrate <- data$UNRATE
data$ongoing_war <- data$ONGOING_WAR
data$cold_war <- data$Cold_War
data$distance_usa <- data$distance_USA
data$ym_hsm <- data$YM_HSM
data$percent_repubmean <- data$percent_repub.mean
data$us_high_act_numeric <- as.numeric(as.character(data$us_high_act))
data$us_high_act <- as.factor(data$us_high_act)

run_ols <- function(formula, data) {
  model <- lm(formula, data = data)
  vcov_robust <- sandwich::vcovHC(model, type = "HC1")
  return(list(model = model, vcov_robust = vcov_robust))
}

    models_with_congressional <- c(1, 2, 7, 8)
    model_names <- paste0("M", models_with_congressional)
    
    for (i in seq_along(models_with_congressional)) {
      model_idx <- models_with_congressional[i]
      model_name <- model_names[i]
      
      if (model_idx == 1) {
        formula <- us_high_act_numeric ~ congressional_support_score + effcaprat + yrtrig + 
  average_polar_btwn_chambers_d1 + net_approval + unrate + cold_war + 
  distance_usa + ongoing_war + continuing_war
      } else if (model_idx == 2) {
        formula <- us_high_act_numeric ~ congressional_support_score + effcaprat + yrtrig + 
  average_polar_btwn_chambers_d1 + net_approval + unrate + cold_war + 
  distance_usa + ongoing_war + continuing_war + theta2_mean + prez_party + ym_hsm
      } else if (model_idx == 7) {
        formula <- us_high_act_numeric ~ congressional_support_score + effcaprat + yrtrig + 
  prez_copartisans + average_polar_btwn_chambers_d1 + net_approval + 
  percent_repubmean + unrate + cold_war + distance_usa + ongoing_war + continuing_war
      } else if (model_idx == 8) {
        formula <- us_high_act_numeric ~ congressional_support_score + effcaprat + yrtrig + 
  prez_copartisans + average_polar_btwn_chambers_d1 + net_approval + 
  percent_repubmean + unrate + cold_war + distance_usa + ongoing_war + 
  continuing_war + theta2_mean + prez_party + ym_hsm
      }
      
      current_model <- run_ols(formula, data)
      
      coef_robust <- coef(current_model$model)
      se_robust <- sqrt(diag(current_model$vcov_robust))
      
      if ("congressional_support_score" %in% names(coef_robust)) {
        estimate <- coef_robust["congressional_support_score"]
        se <- se_robust["congressional_support_score"]
        
        if (col_name == "original_label") {
          model_value <- "original study"
          in_context_value <- "original"
        } else {
          model_value <- col_name
          in_context_value <- paste0(shot_num, "-shot")
        }
        
         all_results[[length(all_results) + 1]] <- data.frame(
           model = model_value,
           estimate_name = paste0("Informal Cong. Sentiment (", model_name, ")"),
           estimate = estimate,
           se = se,
           in_context = in_context_value,
           study = "Hulme_APSR_2025",
           stringsAsFactors = FALSE
         )
         
         cat("      成功提取", model_name, "的估计值\n")
      }
    }

    }, error = function(e) {
      cat("    跳过模型", col_name, "（错误：", conditionMessage(e), "）\n")
    })
  }
}

if (length(all_results) > 0) {
  estimates_df <- do.call(rbind, all_results)

  estimates_df$is_original <- estimates_df$model == "original study"
  estimates_df <- estimates_df[order(-estimates_df$is_original, estimates_df$in_context, estimates_df$model, estimates_df$estimate_name), ]
  estimates_df$is_original <- NULL

  write.csv(estimates_df, "Hulme_APSR_2025_estimates.csv", row.names = FALSE)
  
  cat("\n", paste(rep("=", 50), collapse=""), "\n")
  cat("分析完成！\n")
  cat("处理总结：\n")
  cat("   - 处理的文件数量：", length(label_files), "\n")
  cat("   - 成功提取的估计值数量：", nrow(estimates_df), "\n")
  cat("   - 输出文件：Hulme_APSR_2025_estimates.csv\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
} else {
  cat("\n没有找到有效的估计值！\n")
} 
