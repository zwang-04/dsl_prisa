rm(list = ls())
library(lfe)      # Used for felm() function
library(prisa)     # Used for prisa() and related functions  
library(here)      # Used for here::here()
library(tidyverse) # Used for filter(), %>%, group_by(), summarise()
library(readr)

setwd(here::here())

# ================================ Initialization ================================
sample_size <- 500

results_file <- paste0("PRISA_Choi_Harris_Shen-Bayh_APSR_2022_bootstrap_", sample_size, ".csv")
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

load('analysis_final.Rdata')
fdata <- fdata %>% filter(!is.na(match1))
fdata <- fdata %>% filter(!is.na(jfordef))

cat("Original data loaded. Sample size:", nrow(fdata), "\n")

# Load the labeled data (assuming single file with llama_70b model)
shot_file <- "0_shot_Choi_Harris_Shen-Bayh_APSR_2022_labeled.csv"
if (!file.exists(shot_file)) {
  stop("Error: File ", shot_file, " not found")
}

shot_df <- read_csv(shot_file)
model_name <- "llama_70b"

cat("LLM model:", model_name, "\n")

# Prepare LLM and human labels
model_vec_llm <- shot_df[[model_name]]
model_vec_human <- shot_df$original_label

model_num_llm <- suppressWarnings(as.numeric(as.character(model_vec_llm)))
model_num_human <- suppressWarnings(as.numeric(as.character(model_vec_human)))

valid_llm_idx <- which(model_num_llm %in% c(0, 1) & !is.na(model_num_llm))
valid_human_idx <- which(model_num_human %in% c(0, 1) & !is.na(model_num_human))

if (length(valid_llm_idx) == 0) {
  stop("No valid LLM labels for ", model_name)
}

cat("Valid LLM samples:", length(valid_llm_idx), "\n")
cat("Valid human samples:", length(valid_human_idx), "\n")

# Create complete dataset (all valid LLM labels)
temp_df <- fdata[valid_llm_idx, ]
temp_df$llm_label <- model_num_llm[valid_llm_idx]

temp_df$human_label <- NA
human_overlap_idx <- intersect(valid_llm_idx, valid_human_idx)
if (length(human_overlap_idx) > 0) {
  temp_df$human_label[match(human_overlap_idx, valid_llm_idx)] <- model_num_human[human_overlap_idx]
}

cat("Samples with both LLM and human labels:", sum(!is.na(temp_df$human_label)), "\n")

# Check available labeled sample count
available_labeled <- sum(!is.na(temp_df$human_label))
if (sample_size > available_labeled) {
  stop("Requested sample size ", sample_size, " exceeds available labeled samples ", available_labeled)
}

# ================================ Bootstrap Loop ================================
for (bootstrap_iter in 1:100) {
  cat("\n=== Bootstrap Iteration", bootstrap_iter, "===\n")
  
  # Set seed for reproducibility
  set.seed(123 + bootstrap_iter + sample_size * 1000)
  labeled_indices <- sample(which(!is.na(temp_df$human_label)), sample_size)
  
  current_df <- temp_df
  current_df$is_labeled <- 0
  current_df$is_labeled[labeled_indices] <- 1
  
  current_df$human_labeled <- ifelse(current_df$is_labeled == 1, 
                                    current_df$human_label, 
                                    NA)
  
  if (length(unique(current_df$human_labeled[!is.na(current_df$human_labeled)])) < 2) {
    cat("Insufficient variation in labels, skipping iteration", bootstrap_iter, "\n")
    next
  }
  
  # Model 1
  fn_true_m1 <- function(df) {
    fit1_true <- felm(human_labeled ~ match1 | 0 | 0 | 0, data=df)
    return(c(m1 = coef(fit1_true)["match1"]))
  }
  
  fn_proxy_m1 <- function(df) {
    fit1_proxy <- felm(llm_label ~ match1 | 0 | 0 | 0, data=df)
    return(c(m1 = coef(fit1_proxy)["match1"]))
  }
  
  # Model 2
  fn_true_m2 <- function(df) {
    fit2_true <- felm(human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | 0 | 0 | 0, data=df)
    return(c(m2 = coef(fit2_true)["match1"]))
  }
  
  fn_proxy_m2 <- function(df) {
    fit2_proxy <- felm(llm_label ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | 0 | 0 | 0, data=df)
    return(c(m2 = coef(fit2_proxy)["match1"]))
  }
  
  # Model 3
  fn_true_m3 <- function(df) {
    fit3_true <- felm(human_labeled ~ match1 | cyfe | 0 | cyfe, data=df)
    return(c(m3 = coef(fit3_true)["match1"]))
  }
  
  fn_proxy_m3 <- function(df) {
    fit3_proxy <- felm(llm_label ~ match1 | cyfe | 0 | cyfe, data=df)
    return(c(m3 = coef(fit3_proxy)["match1"]))
  }
  
  # Model 4
  fn_true_m4 <- function(df) {
    fit4_true <- felm(human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe | 0 | cyfe, data=df)
    return(c(m4 = coef(fit4_true)["match1"]))
  }
  
  fn_proxy_m4 <- function(df) {
    fit4_proxy <- felm(llm_label ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe | 0 | cyfe, data=df)
    return(c(m4 = coef(fit4_proxy)["match1"]))
  }
  
  # Model 5
  fn_true_m5 <- function(df) {
    fit5_true <- felm(human_labeled ~ match1 | cyfe + factor(uid) | 0 | uid, data=df)
    return(c(m5 = coef(fit5_true)["match1"]))
  }
  
  fn_proxy_m5 <- function(df) {
    fit5_proxy <- felm(llm_label ~ match1 | cyfe + factor(uid) | 0 | uid, data=df)
    return(c(m5 = coef(fit5_proxy)["match1"]))
  }
  
  # Model 6
  fn_true_m6 <- function(df) {
    fit6_true <- felm(human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe + factor(uid) | 0 | uid, data=df)
    return(c(m6 = coef(fit6_true)["match1"]))
  }
  
  fn_proxy_m6 <- function(df) {
    fit6_proxy <- felm(llm_label ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke | cyfe + factor(uid) | 0 | uid, data=df)
    return(c(m6 = coef(fit6_proxy)["match1"]))
  }
  
  current_results <- data.frame()
  
  models_info <- list(
    list(fn_true = fn_true_m1, fn_proxy = fn_proxy_m1, 
         variable = "m1.match1", name = "Coethnic Match (M1)"),
    list(fn_true = fn_true_m2, fn_proxy = fn_proxy_m2, 
         variable = "m2.match1", name = "Coethnic Match (M2)"),
    list(fn_true = fn_true_m3, fn_proxy = fn_proxy_m3, 
         variable = "m3.match1", name = "Coethnic Match (M3)"),
    list(fn_true = fn_true_m4, fn_proxy = fn_proxy_m4, 
         variable = "m4.match1", name = "Coethnic Match (M4)"),
    list(fn_true = fn_true_m5, fn_proxy = fn_proxy_m5, 
         variable = "m5.match1", name = "Coethnic Match (M5)"),
    list(fn_true = fn_true_m6, fn_proxy = fn_proxy_m6, 
         variable = "m6.match1", name = "Coethnic Match (M6)")
  )
  
  for (i in 1:length(models_info)) {
    model_info <- models_info[[i]]
    
    tryCatch({
      fit_result <- suppressMessages(prisa(
        main_model = model_info$fn_true,
        proxy_model = model_info$fn_proxy,
        data = current_df,
        labeled_set_var_name = "is_labeled",
        options = SetOptions(
          n_boot = 100,
          use_full = TRUE,
          use_parallel = FALSE
        )
      ))
      
      estimates <- get_estimates(fit_result)
      
      prisa_result <- estimates %>% 
        filter(variable == model_info$variable, estimator == "prisa")
      labeled_only_result <- estimates %>% 
        filter(variable == model_info$variable, estimator == "labeled_only")
      
      if (nrow(prisa_result) > 0) {
        current_results <- rbind(current_results, data.frame(
          bootstrap_iter = bootstrap_iter,
          model = model_name,
          estimate_name = model_info$name,
          n_labeled = sample_size,
          method = "prisa",
          estimate = prisa_result$estimate,
          se = prisa_result$std_err,
          ci_lower = prisa_result$ci_lower_95,
          ci_upper = prisa_result$ci_upper_95,
          elss = prisa_result$elss
        ))
      } else {
        current_results <- rbind(current_results, data.frame(
          bootstrap_iter = bootstrap_iter,
          model = model_name,
          estimate_name = model_info$name,
          n_labeled = sample_size,
          method = "prisa",
          estimate = NA,
          se = NA,
          ci_lower = NA,
          ci_upper = NA,
          elss = NA
        ))
      }
      
      if (nrow(labeled_only_result) > 0) {
        current_results <- rbind(current_results, data.frame(
          bootstrap_iter = bootstrap_iter,
          model = model_name,
          estimate_name = model_info$name,
          n_labeled = sample_size,
          method = "labeled_only",
          estimate = labeled_only_result$estimate,
          se = labeled_only_result$std_err,
          ci_lower = labeled_only_result$ci_lower_95,
          ci_upper = labeled_only_result$ci_upper_95,
          elss = labeled_only_result$elss
        ))
      } else {
        current_results <- rbind(current_results, data.frame(
          bootstrap_iter = bootstrap_iter,
          model = model_name,
          estimate_name = model_info$name,
          n_labeled = sample_size,
          method = "labeled_only",
          estimate = NA,
          se = NA,
          ci_lower = NA,
          ci_upper = NA,
          elss = NA
        ))
      }
            
    }, error = function(e) {
      cat("  ✗ Error in", model_info$name, ":", e$message, "\n")
      
      current_results <<- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        model = model_name,
        estimate_name = model_info$name,
        n_labeled = sample_size,
        method = "prisa",
        estimate = NA,
        se = NA,
        ci_lower = NA,
        ci_upper = NA,
        elss = NA
      ))
      
      current_results <<- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        model = model_name,
        estimate_name = model_info$name,
        n_labeled = sample_size,
        method = "labeled_only",
        estimate = NA,
        se = NA,
        ci_lower = NA,
        ci_upper = NA,
        elss = NA
      ))
      
    })
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

# ================================ Summary ================================
if (file.exists(results_file) && file.size(results_file) > 0) {
  final_results <- read_csv(results_file)
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