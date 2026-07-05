library(here)
setwd(here::here())
library(readxl)
library(dplyr)
library(broom)
library(dsl)
library(readr)

# ================================ Initialization ================================
sample_size <- 500
results_file <- paste0("DSL_Pan_Chen_APSR_2018_bootstrap_", sample_size, ".csv")

column_headers <- c("bootstrap_iter", "shot", "model", "estimate_name", "n_labeled", "n_total", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "p_value")

write.table(t(column_headers), 
            file = results_file, 
            sep = ",", 
            col.names = FALSE, 
            row.names = FALSE, 
            quote = FALSE)

cat("Created results file with headers:", results_file, "\n")

# ================================ 1. Data Processing ================================
shot_num <- 0
  shot_file <- paste0(shot_num, "_shot_Pan_Chen_APSR_2018_labeled.csv")
  if (!file.exists(shot_file)) {
  stop("File not found: ", shot_file)
  }
  
  cat("\n=== Processing", shot_num, "shot ===\n")
  in_context <- paste0(shot_num, "-shot")
  
  llm_labels <- read.csv(shot_file)
  posts_data <- read_excel("posts_2014.xlsx", .name_repair = "unique")
  
model_name <- "llama_70b"
cat("LLM model fixed to:", model_name, "\n")
    
    llm_data <- llm_labels %>%
      select(original_row_id, !!sym(model_name), original_label) %>%
      mutate(
    pred_encoded = as.numeric(as.character(!!sym(model_name))),
        original_encoded = as.numeric(as.character(original_label)),
    pred_valid = pred_encoded %in% c(0, 1, 2, 3),
        original_valid = original_encoded %in% c(0, 1, 2, 3)
      ) %>%
  filter(pred_valid) %>%
      mutate(
    pred_prefecWrong = case_when(
      pred_encoded %in% c(1, 3) ~ 1,
          TRUE ~ 0
        ),
    pred_countyWrong = case_when(
      pred_encoded %in% c(2, 3) ~ 1,
          TRUE ~ 0
        ),
    human_prefecWrong = case_when(
          original_valid & original_encoded %in% c(1, 3) ~ 1,
          original_valid ~ 0,
          TRUE ~ NA_real_
        ),
    human_countyWrong = case_when(
          original_valid & original_encoded %in% c(2, 3) ~ 1,
          original_valid ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
  select(original_row_id, starts_with("pred_"), starts_with("human_"))
    
    merged_data <- posts_data %>%
      left_join(llm_data, by = c("email_id_unique" = "original_row_id")) %>%
  filter(!is.na(pred_prefecWrong))
    
    merged_data$connect3 <- merged_data$w_county + merged_data$l_county + merged_data$x_county
    merged_data$connect2 <- merged_data$w_county + merged_data$l_county + merged_data$x_county + 
      merged_data$d_county + merged_data$y_county + merged_data$h_county
    merged_data$connect2b <- ifelse(merged_data$connect2 >= 1, 1, 0)
    merged_data$prefj <- ifelse(merged_data$jurisdiction == "prefectureIssue", 1, 0)
    merged_data$countyj <- ifelse(merged_data$jurisdiction == "countyIssue", 1, 0)
    merged_data$regionj <- ifelse(merged_data$jurisdiction == "regionalIssue", 1, 0)
    merged_data$personal_experience <- ifelse(merged_data$post_individual_experience=="direct",1,0)
    
    cat("Total merged samples:", nrow(merged_data), "\n")
    available_labeled <- sum(!is.na(merged_data$human_prefecWrong) & !is.na(merged_data$human_countyWrong))
    cat("Samples with human labels:", available_labeled, "\n")
if (sample_size > available_labeled) {
  stop("Requested sample_size=", sample_size, ", but only ", available_labeled, " labeled samples available")
}

cat("Sample size set to:", sample_size, "\n")

# ================================ 2. Bootstrap Loop ================================
for (bootstrap_iter in 1:100) {
  cat("\n\n\n========== Bootstrap Iteration", bootstrap_iter, "==========")
  
  # Set seed
  set.seed(123 + bootstrap_iter + sample_size * 1000)
  labeled_indices <- sample(which(!is.na(merged_data$human_prefecWrong) & !is.na(merged_data$human_countyWrong)), sample_size)
      
      current_df <- merged_data
      current_df$is_labeled <- 0
      current_df$is_labeled[labeled_indices] <- 1

  current_df$prefecWrong <- ifelse(current_df$is_labeled == 1, 
                                   current_df$human_prefecWrong, 
                                   NA)
  current_df$countyWrong <- ifelse(current_df$is_labeled == 1, 
                                   current_df$human_countyWrong, 
                                   NA)
  
  if (length(unique(current_df$prefecWrong[!is.na(current_df$prefecWrong)])) < 2) {
    cat("Insufficient variation in prefecture labels, skipping iteration", bootstrap_iter, "\n")
        next
      }
      
  if (length(unique(current_df$countyWrong[!is.na(current_df$countyWrong)])) < 2) {
    cat("Insufficient variation in county labels, skipping iteration", bootstrap_iter, "\n")
        next
      }
      
  dsl_df <- current_df
      
      current_results <- data.frame()
      
  # 模型公式
  models_info <- list(
    list(
      names = c("Prefecture on Reporting (M1)"),
      targets = c("prefecWrong"),
      formula = SendOrNot ~ prefecWrong
    ),
    list(
      names = c("Prefecture on Reporting (M2)", "County on Reporting (M2)"),
      targets = c("prefecWrong", "countyWrong"),
      formula = SendOrNot ~ prefecWrong + countyWrong * connect2b
    ),
    list(
      names = c("Prefecture on Reporting (M3)", "County on Reporting (M3)"),
      targets = c("prefecWrong", "countyWrong"),
      formula = SendOrNot ~ prefecWrong + countyWrong * connect2b + prevalence + regionj + groupIssue + realWorldCollectiveAction + petitioning + sentiment_indico + personal_experience
    ),
    list(
      names = c("Prefecture on Censorship (M1)"),
      targets = c("prefecWrong"),
      formula = censorship ~ prefecWrong + prefectureCensorthipAuthority + prevalence + groupIssue + sentiment_indico + personal_experience + realWorldCollectiveAction + petitioning + regionj
    ),
    list(
      names = c("Prefecture on Censorship (M2)"),
      targets = c("prefecWrong"),
      formula = censorship ~ prefecWrong * prefectureCensorthipAuthority + prevalence + groupIssue + sentiment_indico + personal_experience + realWorldCollectiveAction + petitioning + regionj
    )
  )

  for (i in seq_along(models_info)) {
    model_info <- models_info[[i]]
    cat("\n=====  Running DSL for:", paste(model_info$names, collapse = "; "), "=====\n")

    # 动态确定公式中包含的变量
    formula_vars <- all.vars(model_info$formula)
    available_vars <- c()
    available_preds <- c()
    
    if ("prefecWrong" %in% formula_vars) {
      available_vars <- c(available_vars, "prefecWrong")
      available_preds <- c(available_preds, "pred_prefecWrong")
    }
    
    if ("countyWrong" %in% formula_vars) {
      available_vars <- c(available_vars, "countyWrong")
      available_preds <- c(available_preds, "pred_countyWrong")
    }
    
    cat("  Formula variables:", paste(formula_vars, collapse = ", "), "\n")
    cat("  Available predicted vars:", paste(available_vars, collapse = ", "), "\n")
    cat("  Available predictions:", paste(available_preds, collapse = ", "), "\n")

    out <- tryCatch({
      dsl(
        model = "logit",
        formula = model_info$formula,
        predicted_var = available_vars,
        prediction = available_preds,
        data = dsl_df
      )
    }, error = function(e) {
      cat("  ✗ Error in", paste(model_info$names, collapse = "; "), ":", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(out)) {
      cat("  ✓ DSL completed for:", paste(model_info$names, collapse = "; "), "\n")
      cat("  DSL Results:\n")
      print(summary(out))
      cat("\n")
    } else {
      cat("  ⚠ DSL failed for:", paste(model_info$names, collapse = "; "), "- using NA values\n")
    }

    # 提取样本信息，如果out为NULL则使用默认值
    n_labeled_actual <- tryCatch({ 
      if (!is.null(out)) out$internal$num_expert else sample_size 
    }, error = function(e) sample_size)
    n_total <- tryCatch({ 
      if (!is.null(out)) out$internal$num_data else nrow(dsl_df) 
    }, error = function(e) nrow(dsl_df))

    # 提取系数表
    coef_tbl <- NULL
    if (!is.null(out)) {
      summ <- tryCatch({ summary(out) }, error = function(e) NULL)
      if (!is.null(summ)) {
        if (is.data.frame(summ) || is.matrix(summ)) {
          coef_tbl <- as.data.frame(summ)
          cat("  ✓ Extracted coefficients from summary\n")
        } else {
          coef_tbl <- tryCatch({ summ$coefficients }, error = function(e) NULL)
          if (is.null(coef_tbl)) {
            coef_tbl <- tryCatch({ summ$coef }, error = function(e) NULL)
          }
        }
      }
      
      if (is.null(coef_tbl)) {
        cat("  ⚠ Fallback to manual calculation\n")
        coef_vec <- tryCatch({ out$coefficients }, error = function(e) NULL)
        se_vec <- tryCatch({ out$standard_errors }, error = function(e) NULL)
        if (!is.null(coef_vec) && !is.null(se_vec)) {
          z_scores <- coef_vec / se_vec
          p_values <- 2 * (1 - pnorm(abs(z_scores)))
          
          coef_tbl <- data.frame(
            Estimate = coef_vec,
            `Std. Error` = se_vec,
            `CI Lower` = coef_vec - 1.96 * se_vec,
            `CI Upper` = coef_vec + 1.96 * se_vec,
            `p value` = p_values,
            check.names = FALSE
          )
        }
      }
    }

    for (j in seq_along(model_info$targets)) {
      est_name <- model_info$names[j]
      target_coef <- model_info$targets[j]

      if (!is.null(coef_tbl) && target_coef %in% rownames(coef_tbl)) {
        est <- suppressWarnings(as.numeric(coef_tbl[target_coef, "Estimate"]))
        se  <- suppressWarnings(as.numeric(coef_tbl[target_coef, "Std. Error"]))
        ciL <- suppressWarnings(as.numeric(coef_tbl[target_coef, "CI Lower"]))
        ciU <- suppressWarnings(as.numeric(coef_tbl[target_coef, "CI Upper"]))
        pval <- suppressWarnings(as.numeric(coef_tbl[target_coef, "p value"]))
      } else {
        est <- se <- ciL <- ciU <- pval <- NA
      }

      result_row <- data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model_name,
        estimate_name = est_name,
        n_labeled = n_labeled_actual,
        n_total = n_total,
        method = "dsl",
        estimate = est,
        se = se,
        ci_lower = ciL,
        ci_upper = ciU,
        p_value = pval
      )

      current_results <- rbind(current_results, result_row)
    }
  }
      
      if (nrow(current_results) > 0) {
        write.table(current_results, 
                    file = results_file, 
                    sep = ",", 
                    append = TRUE, 
                    col.names = FALSE, 
                    row.names = FALSE)
  }
}

# ================================ 3. Summary ================================
if (file.exists(results_file) && file.size(results_file) > 0) {
  final_results <- read_csv(results_file)
  cat("\n=== Final Results Summary ===\n")
  cat("Total results:", nrow(final_results), "\n")
  cat("DSL results:", nrow(final_results[final_results$method == "dsl", ]), "\n")
  
  bootstrap_summary <- final_results %>%
    filter(method %in% c("dsl")) %>%
    group_by(bootstrap_iter, shot, model, method) %>%
    summarise(n_estimates = n(), .groups = 'drop')
  
  cat("\nResults by bootstrap iteration and method:\n")
  print(head(bootstrap_summary, 20))  # Show first 20 rows
  
} else {
  cat("No results file found or file is empty\n")
}

cat("\nBootstrap analysis completed!\n")