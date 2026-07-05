library(tidyverse)
library(data.table)
library(lmtest)
library(sandwich)
library(MASS)
library(prisa)
library(here)
setwd(here::here())

sample_size <- 500

results_file <- paste0("PRISA_Gohdes_Steinert_AJPS_2024_bootstrap_", sample_size, ".csv")

column_headers <- c("bootstrap_iter", "model", "estimate_name", "n_labeled", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "elss")

write.table(t(column_headers), 
           file = results_file, 
           sep = ",", 
           col.names = FALSE, 
           row.names = FALSE, 
           quote = FALSE)

cat("Created results file with headers:", results_file, "\n")
cat("Sample size set to:", sample_size, "\n")

dataset <- fread("dataset.csv", select = setdiff(names(fread("dataset.csv", nrows = 0)), "original_label"))

shot_file <- "0_shot_Gohdes_Steinert_AJPS_2024_labeled.csv"
if (!file.exists(shot_file)) {
  stop("Error: File ", shot_file, " not found")
}

llm_labels <- fread(shot_file)
model_name <- "llama_70b"

cat("LLM model:", model_name, "\n")

new_dataset <- dataset
new_dataset$LLM_label <- llm_labels[[model_name]][match(new_dataset$original_row_id, llm_labels$original_row_id)]
new_dataset$original_label <- llm_labels$original_label[match(new_dataset$original_row_id, llm_labels$original_row_id)]

cat("Model: ", model_name, "Matching Validation\n")
cat("      Dataset:", nrow(dataset), "\n")
cat("      LLM_label rows:", sum(!is.na(new_dataset$LLM_label)), "\n")
cat("      Original_label rows:", sum(!is.na(new_dataset$original_label)), "\n")

new_dataset$LLM_label_numeric <- as.numeric(as.character(new_dataset$LLM_label))
valid_labels <- c(-1, 0, 1)
is_valid <- new_dataset$LLM_label_numeric %in% valid_labels & !is.na(new_dataset$LLM_label_numeric)
new_dataset <- new_dataset[is_valid, ]
cat("      Valid LLM_label rows:", nrow(new_dataset), "\n")
cat("      Removed", sum(!is_valid), "rows with invalid LLM labels\n")

cat("      LLM_label Distribution:\n")
label_table <- table(new_dataset$LLM_label_numeric, useNA = "ifany")
for (i in seq_along(label_table)) {
  cat("    ", names(label_table)[i], ":", label_table[i], "\n")
}

new_dataset$llm_negative <- as.integer(new_dataset$LLM_label_numeric == -1)
new_dataset$llm_neutral <- as.integer(new_dataset$LLM_label_numeric == 0)
new_dataset$llm_positive <- as.integer(new_dataset$LLM_label_numeric == 1)

siege_end <- as.Date('2016-12-15')
dfl <- new_dataset %>%
  filter(created_at <= siege_end+31) %>%
  filter(aleppo_account == 1 | matched_account == 1) %>%
  mutate(user.id = as.character(user.id))

dfl.panel <- dfl %>% filter(sample == "panel")
dfl.geotag <- dfl %>% filter(sample == "geotag")

# Check available labeled sample count for each subsample
available_labeled_panel <- sum(!is.na(dfl.panel$original_label))
available_labeled_geotag <- sum(!is.na(dfl.geotag$original_label))

cat("Available labeled samples:\n")
cat("  Panel:", available_labeled_panel, "\n")
cat("  Geotag:", available_labeled_geotag, "\n")

# ================================ Bootstrap Loop ================================
for (bootstrap_iter in 1:100) {
  cat("\n=== Bootstrap Iteration", bootstrap_iter, "===\n")
  
  for (df_name in c("panel", "geotag")) {
    current_df <- if(df_name == "panel") dfl.panel else dfl.geotag
    
    # Check if this subsample has enough labeled data
    available_labeled <- sum(!is.na(current_df$original_label))
    if (sample_size > available_labeled) {
      cat("\nSkipping", df_name, "- insufficient labeled samples (", available_labeled, ")\n")
      next
    }
    
    cat("\nProcessing", df_name, "\n")
    
    # Set seed for reproducibility
    set.seed(123 + bootstrap_iter + which(c("panel", "geotag") == df_name) * 100)
    labeled_indices <- sample(which(!is.na(current_df$original_label)), sample_size)
    
    current_df$is_labeled <- 0
    current_df$is_labeled[labeled_indices] <- 1
    
    current_df$human_negative_labeled <- ifelse(current_df$is_labeled == 1, 
                                               as.integer(current_df$original_label == -1), 
                                               NA)
    current_df$human_positive_labeled <- ifelse(current_df$is_labeled == 1, 
                                               as.integer(current_df$original_label == 1), 
                                               NA)
    
    if (length(unique(current_df$human_positive_labeled[!is.na(current_df$human_positive_labeled)])) < 2 && 
        length(unique(current_df$human_negative_labeled[!is.na(current_df$human_negative_labeled)])) < 2) {
      cat("Insufficient variation in labels, skipping iteration", bootstrap_iter, "for", df_name, "\n")
      next
    }
    
    # 人工 positive
    fn_true_lm_positive <- function(df) {
      fit_pos1 <- glm(human_positive_labeled ~ time*Aleppo, data=df, family=binomial(link='logit'))
      return(c(positive = coef(fit_pos1)["time:Aleppo"]))
    }
    
    # 人工 negative
    fn_true_lm_negative <- function(df) {
      fit_neg1 <- glm(human_negative_labeled ~ time*Aleppo, data=df, family=binomial(link='logit'))
      return(c(negative = coef(fit_neg1)["time:Aleppo"]))
    }
    
    # LLM positive
    fn_proxy_lm_positive <- function(df) {
      fit_pos2 <- glm(llm_positive ~ time*Aleppo, data=df, family=binomial(link='logit'))
      return(c(positive = coef(fit_pos2)["time:Aleppo"]))
    }
    
    # LLM negative
    fn_proxy_lm_negative <- function(df) {
      fit_neg2 <- glm(llm_negative ~ time*Aleppo, data=df, family=binomial(link='logit'))
      return(c(negative = coef(fit_neg2)["time:Aleppo"]))
    }
    
    # PRISA positive
    cat("      Running PRISA for positive sentiment...\n")
    positive_prisa <- data.frame()
    positive_labeled_only <- data.frame()
    estimates_positive <- data.frame()
    
    tryCatch({
      fit_result_positive <- prisa(
        main_model = fn_true_lm_positive,
        proxy_model = fn_proxy_lm_positive,
        data = current_df,
        labeled_set_var_name = "is_labeled",
        options = SetOptions(
          n_boot = 100,
          use_full = TRUE,
          use_parallel = FALSE
        )
      )
      
      estimates_positive <- get_estimates(fit_result_positive)
      
      positive_prisa <- estimates_positive %>% 
        filter(variable == "positive.time:Aleppo", estimator == "prisa")
      positive_labeled_only <- estimates_positive %>% 
        filter(grepl("positive.time:Aleppo", variable), estimator == "labeled_only")
      
      cat("      Positive sentiment PRISA completed successfully\n")
    }, error = function(e) {
      cat("      Error in positive sentiment PRISA:", e$message, "\n")
      positive_prisa <- data.frame()
      positive_labeled_only <- data.frame()
    })
    
    # PRISA negative
    cat("      Running PRISA for negative sentiment...\n")
    negative_prisa <- data.frame()
    negative_labeled_only <- data.frame()
    estimates_negative <- data.frame()
    
    tryCatch({
      fit_result_negative <- prisa(
        main_model = fn_true_lm_negative,
        proxy_model = fn_proxy_lm_negative,
        data = current_df,
        labeled_set_var_name = "is_labeled",
        options = SetOptions(
          n_boot = 100,
          use_full = TRUE,
          use_parallel = FALSE
        )
      )
      
      estimates_negative <- get_estimates(fit_result_negative)
      
      negative_prisa <- estimates_negative %>% 
        filter(variable == "negative.time:Aleppo", estimator == "prisa")
      negative_labeled_only <- estimates_negative %>% 
        filter(variable == "negative.time:Aleppo", estimator == "labeled_only")
      
      cat("      Negative sentiment PRISA completed successfully\n")
    }, error = function(e) {
      cat("      Error in negative sentiment PRISA:", e$message, "\n")
      negative_prisa <- data.frame()
      negative_labeled_only <- data.frame()
    })

    current_results <- data.frame()
    
    # positive
    current_results <- rbind(current_results, data.frame(
      bootstrap_iter = bootstrap_iter,
      model = model_name,
      estimate_name = paste0("moderation positive sentiment (", ifelse(df_name == "panel", "panel", "geo"), ")"),
      n_labeled = sample_size,
      method = "prisa",
      estimate = ifelse(nrow(positive_prisa) > 0, positive_prisa$estimate, NA),
      se = ifelse(nrow(positive_prisa) > 0, positive_prisa$std_err, NA),
      ci_lower = ifelse(nrow(positive_prisa) > 0, positive_prisa$ci_lower_95, NA),
      ci_upper = ifelse(nrow(positive_prisa) > 0, positive_prisa$ci_upper_95, NA),
      elss = ifelse(nrow(positive_prisa) > 0, positive_prisa$elss, NA)
    ))

    current_results <- rbind(current_results, data.frame(
      bootstrap_iter = bootstrap_iter,
      model = model_name,
      estimate_name = paste0("moderation positive sentiment (", ifelse(df_name == "panel", "panel", "geo"), ")"),
      n_labeled = sample_size,
      method = "labeled_only",
      estimate = ifelse(nrow(positive_labeled_only) > 0, positive_labeled_only$estimate, NA),
      se = ifelse(nrow(positive_labeled_only) > 0, positive_labeled_only$std_err, NA),
      ci_lower = ifelse(nrow(positive_labeled_only) > 0, positive_labeled_only$ci_lower_95, NA),
      ci_upper = ifelse(nrow(positive_labeled_only) > 0, positive_labeled_only$ci_upper_95, NA),
      elss = ifelse(nrow(positive_labeled_only) > 0, positive_labeled_only$elss, NA)
    ))
    
    # negative
    current_results <- rbind(current_results, data.frame(
      bootstrap_iter = bootstrap_iter,
      model = model_name,
      estimate_name = paste0("moderation negative sentiment (", ifelse(df_name == "panel", "panel", "geo"), ")"),
      n_labeled = sample_size,
      method = "prisa",
      estimate = ifelse(nrow(negative_prisa) > 0, negative_prisa$estimate, NA),
      se = ifelse(nrow(negative_prisa) > 0, negative_prisa$std_err, NA),
      ci_lower = ifelse(nrow(negative_prisa) > 0, negative_prisa$ci_lower_95, NA),
      ci_upper = ifelse(nrow(negative_prisa) > 0, negative_prisa$ci_upper_95, NA),
      elss = ifelse(nrow(negative_prisa) > 0, negative_prisa$elss, NA)
    ))

    current_results <- rbind(current_results, data.frame(
      bootstrap_iter = bootstrap_iter,
      model = model_name,
      estimate_name = paste0("moderation negative sentiment (", ifelse(df_name == "panel", "panel", "geo"), ")"),
      n_labeled = sample_size,
      method = "labeled_only",
      estimate = ifelse(nrow(negative_labeled_only) > 0, negative_labeled_only$estimate, NA),
      se = ifelse(nrow(negative_labeled_only) > 0, negative_labeled_only$std_err, NA),
      ci_lower = ifelse(nrow(negative_labeled_only) > 0, negative_labeled_only$ci_lower_95, NA),
      ci_upper = ifelse(nrow(negative_labeled_only) > 0, negative_labeled_only$ci_upper_95, NA),
      elss = ifelse(nrow(negative_labeled_only) > 0, negative_labeled_only$elss, NA)
    ))
    
    if (nrow(current_results) > 0) {
      write.table(current_results, 
                 file = results_file, 
                 sep = ",", 
                 append = TRUE, 
                 col.names = FALSE, 
                 row.names = FALSE)
    }
  }
}

# ================================ Summary ================================
if (file.exists(results_file) && file.size(results_file) > 0) {
  final_results <- read.csv(results_file)
  cat("\n=== Final Results Summary ===\n")
  cat("Total results:", nrow(final_results), "\n")
  cat("PRISA results:", nrow(final_results[final_results$method == "prisa", ]), "\n")
  cat("Labeled-only results:", nrow(final_results[final_results$method == "labeled_only", ]), "\n")
  
  bootstrap_summary <- final_results %>%
    filter(method %in% c("prisa", "labeled_only")) %>%
    group_by(bootstrap_iter, model, method) %>%
    summarise(n_estimates = n(), .groups = 'drop')
  
  cat("\nResults by bootstrap iteration and method:\n")
  print(head(bootstrap_summary, 20))  # Show first 20 rows
  
} else {
  cat("No results file found or file is empty\n")
}

cat("\nBootstrap analysis completed!\n")