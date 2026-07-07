setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

library(TMB)
library(glmmTMB)
library(dplyr)

target_vars <- c("Partisan.Euroscepticism", "Public.Euroscepticism", "Issue.Salience")
model_names <- c(
  "Partisan Euroscepticism (M1)" = "Partisan.Euroscepticism|table3.1",
  "Partisan Euroscepticism (M2)" = "Partisan.Euroscepticism|table3.2",
  "Public Euroscepticism (M1)" = "Public.Euroscepticism|table3.1",
  "Public Euroscepticism (M2)" = "Public.Euroscepticism|table3.2",
  "Issue Salience (M3)" = "Issue.Salience|table3.3",
  "Issue Salience (M4)" = "Issue.Salience|table3.4"
)

process_model <- function(organized, model_name, shot_num, results, in_context, study, is_original, shot_number) {
  style_raw <- as.character(organized$Style)
  organized$Style <- as.numeric(style_raw)
  valid_styles <- c(1, 2, 3, 4)
  valid_mask <- (organized$Style %in% valid_styles) | is.na(organized$Style)
  organized <- organized[valid_mask, ]
  organized$Claim <- ifelse(organized$Style == 1, 1, 0)
  organized$Share <- ifelse(organized$Style == 2, 1, 0)
  organized$Blame <- ifelse(organized$Style == 3, 1, 0)
  organized$Claim[is.na(organized$Style)] <- NA
  organized$Share[is.na(organized$Style)] <- NA
  organized$Blame[is.na(organized$Style)] <- NA

  table3.1 <- glm(Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, data = organized, family = "binomial")
  table3.2 <- glm(Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, data = organized, family = "binomial")
  table3.3 <- glm(Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, data = organized, family = "binomial")
  table3.4 <- glm(Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, data = organized, family = "binomial")
  table3.5 <- glmmTMB(Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country + (1 | Head.of.Government), data = organized, family = "binomial")
  table3.6 <- glmmTMB(Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country + (1 | Head.of.Government), data = organized, family = "binomial")

  for (key in names(model_names)) {
    split_info <- strsplit(model_names[[key]], "\\|")[[1]]
    var <- split_info[1]
    model_obj <- get(split_info[2])
    if (var %in% names(coef(model_obj))) {
      est <- coef(model_obj)[var]
      se <- sqrt(diag(vcov(model_obj)))[var]
      results <- rbind(results, data.frame(
        model = model_name,
        estimate_name = key,
        estimate = est,
        se = se,
        in_context = in_context,
        study = study,
        is_original = is_original,
        shot_number = shot_number,
        stringsAsFactors = FALSE
      ))
    }
  }
  return(results)
}

cat("加载EUCO.RData...\n")
load("EUCO.RData")

results <- data.frame()

suffix_esc <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
shot_files <- list.files(path    = ANNOTATION_DIR,
                         pattern = paste0("^[0-9]+_shot_Hunter_JOP_2025", suffix_esc, "$"))
cat(sprintf("\n找到以下shot文件：\n%s\n", paste(shot_files, collapse = "\n")))

for (file in shot_files) {
  shot_num <- gsub("_shot_Hunter_JOP_2025.*$", "", file)
  cat(sprintf("\n处理 %s-shot 数据...\n", shot_num))
  new_labels <- read.csv(file.path(ANNOTATION_DIR, file))
  llm_columns <- setdiff(names(new_labels), c("original_row_id", "original_label"))
  # 0-shot时加入original_label
  if (shot_num == "0" && "original_label" %in% names(new_labels)) {
    llm_columns <- c(llm_columns, "original_label")
  }
  for (model_name in llm_columns) {
    cat(sprintf("\n开始处理模型：%s\n", model_name))
    organized$Style <- new_labels[[model_name]][match(organized$original_row_id, new_labels$original_row_id)]
    is_original <- (model_name == "original_label" && shot_num == "0")
    results <- process_model(
      organized, 
      ifelse(is_original, "original study", model_name), 
      shot_num, 
      results, 
      ifelse(is_original, "original", paste0(shot_num, "-shot")), 
      "Hunter_JOP_2025", 
      is_original, 
      as.integer(shot_num)
    )
  }
}

results <- results[order(-results$is_original, results$shot_number, results$model, results$estimate_name), ]
results$is_original <- NULL
results$shot_number <- NULL

write.csv(results, "Hunter_JOP_2025_estimates.csv", row.names = FALSE)
cat("Results saved to Hunter_JOP_2025_estimates.csv\n")
