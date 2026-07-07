setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

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

shot_numbers <- c(0, 2, 5, 10)
all_results <- list()

load("parl_speech.Rdata")
df_analysis <- ws_parl
df_analysis <- df_analysis %>% mutate(id = row_number())
names(df_analysis)[144] <- "original_row_id"

for (shot in shot_numbers) {
  file_name <- file.path(ANNOTATION_DIR, paste0(shot, "_shot_Widmann_JOP_2025", ANNOTATION_SUFFIX))
  if (!file.exists(file_name)) next
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
  for (model in model_cols) {
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
    cat(sprintf("[shot=%s, model=%s] 有效行: %d, 无效行: %d\n", shot, model_name, n_valid, n_invalid))
    if (n_valid == 0) next
    merged2_valid <- merged2[valid_idx, ]
    # AfD的disgust
    party <- "AfD"
    df_party <- merged2_valid[!(merged2_valid$party != party & merged2_valid$treat_indirect == 1),]
    df_party$disgust <- ifelse(as.integer(df_party[[model]]) %in% c(2, 3), 1, 0)
    df_agg <- aggregate(df_party$disgust, list(df_party$name, df_party$months, df_party$count, df_party$party), mean)
    colnames(df_agg) <- c("name", "date", "treat_indirect", "party", "disgust")
    df_agg <- df_agg[order(df_agg$name, -abs(df_agg$treat_indirect)), ]
    df_agg <- df_agg %>% distinct(name, date, .keep_all = T)
    fixed1 <- tryCatch({
      plm(disgust ~ treat_indirect, model="within", index = c("name", "date"), effect = "twoways", data = df_agg)
    }, error=function(e) NULL)
    if (!is.null(fixed1)) {
      model1 <- coeftest(fixed1, vcov=vcovHC(fixed1, type="HC0", cluster="group"))
      tidy_res <- broom::tidy(model1)
      est_row <- tidy_res[tidy_res$term == "treat_indirect",]
      if (nrow(est_row) > 0) {
        all_results[[length(all_results)+1]] <- data.frame(
          model = model_name,
          estimate_name = "AfD's disgust appeal",
          estimate = est_row$estimate,
          se = est_row$std.error,
          in_context = paste0(shot, "-shot"),
          study = "Widmann_JOP_2025"
        )
      }
    }
    # Greens的anger
    party <- "Greens"
    df_party <- merged2_valid[!(merged2_valid$party != party & merged2_valid$treat_indirect == 1),]
    df_party$anger <- ifelse(as.integer(df_party[[model]]) %in% c(1, 3), 1, 0)
    df_agg2 <- aggregate(df_party$anger, list(df_party$name, df_party$months, df_party$count, df_party$party), mean)
    colnames(df_agg2) <- c("name", "date", "treat_indirect", "party", "anger")
    df_agg2 <- df_agg2[order(df_agg2$name, -abs(df_agg2$treat_indirect)), ]
    df_agg2 <- df_agg2 %>% distinct(name, date, .keep_all = T)
    fixed2 <- tryCatch({
      plm(anger ~ treat_indirect, model="within", index = c("name", "date"), effect = "twoways", data = df_agg2)
    }, error=function(e) NULL)
    if (!is.null(fixed2)) {
      model2 <- coeftest(fixed2, vcov=vcovHC(fixed2, type="HC0", cluster="group"))
      tidy_res2 <- broom::tidy(model2)
      est_row2 <- tidy_res2[tidy_res2$term == "treat_indirect",]
      if (nrow(est_row2) > 0) {
        all_results[[length(all_results)+1]] <- data.frame(
          model = model_name,
          estimate_name = "Green party's anger appeal",
          estimate = est_row2$estimate,
          se = est_row2$std.error,
          in_context = paste0(shot, "-shot"),
          study = "Widmann_JOP_2025"
        )
      }
    }
    # The Left的anger
    party <- "TheLeft"
    df_party <- merged2_valid[!(merged2_valid$party != party & merged2_valid$treat_indirect == 1),]
    df_party$anger <- ifelse(as.integer(df_party[[model]]) %in% c(1, 3), 1, 0)
    df_agg3 <- aggregate(df_party$anger, list(df_party$name, df_party$months, df_party$count, df_party$party), mean)
    colnames(df_agg3) <- c("name", "date", "treat_indirect", "party", "anger")
    df_agg3 <- df_agg3[order(df_agg3$name, -abs(df_agg3$treat_indirect)), ]
    df_agg3 <- df_agg3 %>% distinct(name, date, .keep_all = T)
    fixed3 <- tryCatch({
      plm(anger ~ treat_indirect, model="within", index = c("name", "date"), effect = "twoways", data = df_agg3)
    }, error=function(e) {cat("plm error:", e$message, "\n"); NULL})
    if (!is.null(fixed3)) {
      model3 <- coeftest(fixed3, vcov=vcovHC(fixed3, type="HC0", cluster="group"))
      tidy_res3 <- broom::tidy(model3)
      est_row3 <- tidy_res3[tidy_res3$term == "treat_indirect",]
      if (nrow(est_row3) > 0) {
        all_results[[length(all_results)+1]] <- data.frame(
          model = model_name,
          estimate_name = "Left-wing parties' anger appeal",
          estimate = est_row3$estimate,
          se = est_row3$std.error,
          in_context = paste0(shot, "-shot"),
          study = "Widmann_JOP_2025"
        )
      }
    }
  }
}

if (length(all_results) > 0) {
  results_df <- do.call(rbind, all_results)
  results_df$in_context[results_df$model == "original study"] <- "original"
  results_df$shot_number <- NA
  is_shot <- grepl("^[0-9]+-shot$", results_df$in_context)
  results_df$shot_number[is_shot] <- as.numeric(sub("-shot", "", results_df$in_context[is_shot]))
  results_df$shot_number[results_df$in_context == "original"] <- -1
  results_df <- results_df %>%
    arrange(shot_number, model, estimate_name)
  original_rows <- results_df[results_df$model == "original study", ]
  other_rows <- results_df[results_df$model != "original study", ]
  results_df <- rbind(original_rows, other_rows)
  results_df$shot_number <- NULL
  write.csv(results_df, "Widmann_JOP_2025_estimates.csv", row.names = FALSE)
}



