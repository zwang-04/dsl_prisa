setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

library(readxl)
library(dplyr)
library(broom)

suffix_esc <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
shot_files <- list.files(path    = ANNOTATION_DIR,
                         pattern = paste0("_shot_Pan_Chen_APSR_2018", suffix_esc, "$"))
results <- data.frame()

for (shot_file in shot_files) {
  number <- gsub("_shot.*", "", shot_file)
  in_context <- paste0(number, "-shot")
  labeled_data <- read.csv(file.path(ANNOTATION_DIR, shot_file))
  posts_data <- read_excel("posts_2014.xlsx", .name_repair = "unique")
  
  llm_columns <- setdiff(colnames(labeled_data), c("original_row_id", "original_label"))
  if (number == "0" && "original_label" %in% colnames(labeled_data)) {
    llm_columns <- c(llm_columns, "original_label")
  }
  
  for (model_col in llm_columns) {
    valid_data <- labeled_data %>%
      dplyr::select(original_row_id, !!sym(model_col)) %>%
      mutate(
        encoded_value = as.numeric(as.character(!!sym(model_col))),
        is_valid = encoded_value %in% c(0, 1, 2, 3)
      ) %>%
      filter(is_valid) %>%
      dplyr::select(original_row_id, encoded_value)
    colnames(valid_data)[colnames(valid_data) == "encoded_value"] <- model_col
    if (nrow(valid_data) == 0) next
    
    model_data <- valid_data %>%
      mutate(
        prefectureWrongdoing = case_when(
          !!sym(model_col) %in% c(1, 3) ~ 1,
          TRUE ~ 0
        ),
        countyWrongdoing = case_when(
          !!sym(model_col) %in% c(2, 3) ~ 1,
          TRUE ~ 0
        )
      ) %>%
      dplyr::select(original_row_id, prefectureWrongdoing, countyWrongdoing)
    
    merged_data <- posts_data %>%
      left_join(model_data, by = c("email_id_unique" = "original_row_id"))
    
    merged_data$prefectureWrongdoing <- factor(merged_data$prefectureWrongdoing)
    merged_data$connect3 <- merged_data$w_county + merged_data$l_county + merged_data$x_county
    merged_data$connect2 <- merged_data$w_county + merged_data$l_county + merged_data$x_county + merged_data$d_county + merged_data$y_county + merged_data$h_county
    merged_data$connect2b <- ifelse(merged_data$connect2 >= 1, 1, 0)
    merged_data$prefj <- ifelse(merged_data$jurisdiction == "prefectureIssue", 1, 0)
    merged_data$countyj <- ifelse(merged_data$jurisdiction == "countyIssue", 1, 0)
    merged_data$regionj <- ifelse(merged_data$jurisdiction == "regionalIssue", 1, 0)
    merged_data$personal_experience <- ifelse(merged_data$post_individual_experience=="direct",1,0)
    
    # Table 3
    m1 <- glm(SendOrNot ~ prefectureWrongdoing, data = merged_data, family = binomial(link="logit"))
    m2 <- glm(SendOrNot ~ prefectureWrongdoing + countyWrongdoing * connect2b, data = merged_data, family = binomial(link="logit"))
    m3 <- glm(SendOrNot ~ prefectureWrongdoing + countyWrongdoing * connect2b + prevalence + regionj + groupIssue + realWorldCollectiveAction + petitioning + sentiment_indico + personal_experience, data = merged_data, family = binomial(link="logit"))
    
    # Table 4
    m4 <- glm(censorship ~ prefectureWrongdoing + prefectureCensorthipAuthority + prevalence + groupIssue + sentiment_indico + personal_experience + realWorldCollectiveAction + petitioning + regionj, data = merged_data, family = binomial(link="logit"))
    m5 <- glm(censorship ~ prefectureWrongdoing * prefectureCensorthipAuthority + prevalence + groupIssue + sentiment_indico + personal_experience + realWorldCollectiveAction + petitioning + regionj, data = merged_data, family = binomial(link="logit"))
    
    # 提取
    # Table 3
    # prefectureWrongdoing1
    for (i in 1:3) {
      model_obj <- list(m1, m2, m3)[[i]]
      model_name <- paste0("Prefecture on Reporting (M", i, ")")
      if ("prefectureWrongdoing1" %in% rownames(coef(summary(model_obj)))) {
        est <- coef(summary(model_obj))["prefectureWrongdoing1", "Estimate"]
        se <- coef(summary(model_obj))["prefectureWrongdoing1", "Std. Error"]
        results <- rbind(results, data.frame(
          model = model_col,
          estimate_name = model_name,
          estimate = est,
          se = se,
          in_context = in_context,
          study = "Pan_Chen_APSR_2018",
          stringsAsFactors = FALSE
        ))
      }
    }
    # countyWrongdoing
    for (i in 2:3) {
      model_obj <- list(m2, m3)[[i-1]]
      model_name <- paste0("County on Reporting (M", i, ")")
      if ("countyWrongdoing" %in% rownames(coef(summary(model_obj)))) {
        est <- coef(summary(model_obj))["countyWrongdoing", "Estimate"]
        se <- coef(summary(model_obj))["countyWrongdoing", "Std. Error"]
        results <- rbind(results, data.frame(
          model = model_col,
          estimate_name = model_name,
          estimate = est,
          se = se,
          in_context = in_context,
          study = "Pan_Chen_APSR_2018",
          stringsAsFactors = FALSE
        ))
      }
    }
    # Table 4
    for (i in 1:2) {
      model_obj <- list(m4, m5)[[i]]
      model_name <- paste0("Prefecture on Censorship (M", i, ")")
      if ("prefectureWrongdoing1" %in% rownames(coef(summary(model_obj)))) {
        est <- coef(summary(model_obj))["prefectureWrongdoing1", "Estimate"]
        se <- coef(summary(model_obj))["prefectureWrongdoing1", "Std. Error"]
        results <- rbind(results, data.frame(
          model = model_col,
          estimate_name = model_name,
          estimate = est,
          se = se,
          in_context = in_context,
          study = "Pan_Chen_APSR_2018",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

results$shot_number <- as.integer(gsub("-shot", "", results$in_context))
is_original <- (results$model == 'original_label') & (results$in_context == '0-shot')
results$in_context[is_original] <- 'original'
results$model[is_original] <- 'original study'
results_original <- results[is_original, ]
results_rest <- results[!(results$model %in% c('original_label', 'original study')), ]
results_rest <- results_rest[order(results_rest$shot_number, results_rest$model, results_rest$estimate_name), ]
results_final <- rbind(results_original, results_rest)
results_final$shot_number <- NULL

write.csv(results_final, "Pan_Chen_APSR_2018_estimates.csv", row.names = FALSE)
cat("Results saved to Pan_Chen_APSR_2018_estimates.csv\n")
