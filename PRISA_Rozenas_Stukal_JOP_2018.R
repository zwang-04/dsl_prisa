packages <- c('dplyr', 'data.table', 'lubridate', 'zoo', 'bit64', 'lfe', 'prisa', 'tidyverse')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}
rm(packages, new.packages)

library(here)
library(readr)
library(fixest)
library(data.table)
library(dplyr)
library(lubridate)
library(lfe)      # Used for felm() function
library(prisa)     # Used for prisa() and related functions  
library(tidyverse) # Used for filter(), %>%, group_by(), summarise()

maindir <- here()

# ================================ Initialization ================================
sample_size <- 500

results_file <- paste0("PRISA_Rozenas_Stukal_JOP_2018_bootstrap_", sample_size, ".csv")
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

load("dat_stacked.RData")

filter_and_clean_labels <- function(label_vec) {
    valid_digits <- as.character(1:6)
    invalid_contents <- c()
    valid_idx <- rep(TRUE, length(label_vec))
    cleaned_labels <- vector("character", length(label_vec))
    for (i in seq_along(label_vec)) {
        x <- label_vec[i]
        items <- unlist(strsplit(as.character(x), ","))
        items <- trimws(items)
        items <- gsub("\\.0$", "", items)
        if (length(items) == 0 || any(!items %in% valid_digits)) {
            valid_idx[i] <- FALSE
            invalid_contents <- c(invalid_contents, x)
            cleaned_labels[i] <- NA
        } else {
            items <- sort(items)
            cleaned_labels[i] <- paste(items, collapse=",")
        }
    }
    list(valid_idx=valid_idx, cleaned_labels=cleaned_labels, invalid=unique(invalid_contents))
}

# ================================ Data Loading and Preparation ================================
estimate_names <- c('Vladimir Putin', 'Russian officials', 'Foreign economy', 'Foreign governments')

# Load the labeled data
shot <- 0
csv_path <- file.path(maindir, paste0(shot, '_shot_Rozenas_Stukal_JOP_2018_labeled.csv'))
if (file.exists(csv_path)) {
    df_llm <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8")
    cat("Successfully loaded CSV file:", csv_path, "\n")
} else {
    stop(paste0('未找到', csv_path, '，请检查文件路径！'))
}

model_name <- "llama_70b"
cat("LLM model:", model_name, "\n")
cat("Original data loaded. Sample size:", nrow(dat_stacked), "\n")

# Process LLM and human labels
if (!(model_name %in% names(df_llm))) {
    stop("Error: Model ", model_name, " not found in data")
}

# Process LLM labels
res_llm <- filter_and_clean_labels(as.character(df_llm[[model_name]]))
cat("LLM labels: 过滤了", sum(!res_llm$valid_idx), "行\n")
cat("无效内容:", paste0("[", paste(unique(res_llm$invalid), collapse=","), "]\n"))

# Process human labels (original_label)
if ("original_label" %in% names(df_llm)) {
    res_human <- filter_and_clean_labels(as.character(df_llm$original_label))
    cat("Human labels: 过滤了", sum(!res_human$valid_idx), "行\n")
    cat("无效内容:", paste0("[", paste(unique(res_human$invalid), collapse=","), "]\n"))
} else {
    stop("Error: original_label not found in data")
}

# Create valid indices for both LLM and human labels
valid_llm_idx <- which(res_llm$valid_idx)
valid_human_idx <- which(res_human$valid_idx)

if (length(valid_llm_idx) == 0) {
    stop("No valid LLM labels for ", model_name)
}

cat("Valid LLM samples:", length(valid_llm_idx), "\n")
cat("Valid human samples:", length(valid_human_idx), "\n")

# Create complete dataset with both LLM and human labels
temp_df <- dat_stacked[valid_llm_idx, ]
temp_df$original_row_id <- valid_llm_idx

# Add LLM labels
temp_df$putin_bin_llm <- grepl("1", res_llm$cleaned_labels[valid_llm_idx]) * 1
temp_df$rugov_bin_llm <- grepl("2", res_llm$cleaned_labels[valid_llm_idx]) * 1
temp_df$forec_bin_llm <- grepl("5", res_llm$cleaned_labels[valid_llm_idx]) * 1
temp_df$forgov_bin_llm <- grepl("4", res_llm$cleaned_labels[valid_llm_idx]) * 1

# Add human labels where available
temp_df$putin_bin_human <- NA
temp_df$rugov_bin_human <- NA
temp_df$forec_bin_human <- NA
temp_df$forgov_bin_human <- NA

human_overlap_idx <- intersect(valid_llm_idx, valid_human_idx)
if (length(human_overlap_idx) > 0) {
    overlap_positions <- match(human_overlap_idx, valid_llm_idx)
    temp_df$putin_bin_human[overlap_positions] <- grepl("1", res_human$cleaned_labels[human_overlap_idx]) * 1
    temp_df$rugov_bin_human[overlap_positions] <- grepl("2", res_human$cleaned_labels[human_overlap_idx]) * 1
    temp_df$forec_bin_human[overlap_positions] <- grepl("5", res_human$cleaned_labels[human_overlap_idx]) * 1
    temp_df$forgov_bin_human[overlap_positions] <- grepl("4", res_human$cleaned_labels[human_overlap_idx]) * 1
}

cat("Samples with both LLM and human labels:", sum(!is.na(temp_df$putin_bin_human)), "\n")

# Check available labeled sample count
available_labeled <- sum(!is.na(temp_df$putin_bin_human))
if (sample_size > available_labeled) {
    stop("Requested sample size ", sample_size, " exceeds available labeled samples ", available_labeled)
}

# ================================ Bootstrap Loop ================================
for (bootstrap_iter in 1:100) {
  cat("\n=== Bootstrap Iteration", bootstrap_iter, "===\n")
  
  # Set seed for reproducibility
  set.seed(123 + bootstrap_iter + sample_size * 1000)
  labeled_indices <- sample(which(!is.na(temp_df$putin_bin_human)), sample_size)
  
  current_df <- temp_df
  current_df$is_labeled <- 0
  current_df$is_labeled[labeled_indices] <- 1
  
  # Create labeled versions of human labels
  current_df$putin_bin_labeled <- ifelse(current_df$is_labeled == 1, 
                                        current_df$putin_bin_human, 
                                        NA)
  current_df$rugov_bin_labeled <- ifelse(current_df$is_labeled == 1, 
                                        current_df$rugov_bin_human, 
                                        NA)
  current_df$forec_bin_labeled <- ifelse(current_df$is_labeled == 1, 
                                        current_df$forec_bin_human, 
                                        NA)
  current_df$forgov_bin_labeled <- ifelse(current_df$is_labeled == 1, 
                                         current_df$forgov_bin_human, 
                                         NA)
  
  # Check if we have sufficient variation in labels
  if (length(unique(current_df$putin_bin_labeled[!is.na(current_df$putin_bin_labeled)])) < 2) {
    cat("Insufficient variation in labels, skipping iteration", bootstrap_iter, "\n")
    next
  }
  
  # Define model functions for each outcome variable
  model_functions <- list(
    list(
      name = "Vladimir Putin",
      fn_true_1 = function(df) {
        fit <- feols(putin_bin_labeled ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(putin = coef(fit)["pos"]))
      },
      fn_proxy_1 = function(df) {
        fit <- feols(putin_bin_llm ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(putin = coef(fit)["pos"]))
      },
      variable = "putin.pos"
    ),
    list(
      name = "Russian officials",
      fn_true_2 = function(df) {
        fit <- feols(rugov_bin_labeled ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(rugov = coef(fit)["pos"]))
      },
      fn_proxy_2 = function(df) {
        fit <- feols(rugov_bin_llm ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(rugov = coef(fit)["pos"]))
      },
      variable = "rugov.pos"
    ),
    list(
      name = "Foreign economy",
      fn_true_3 = function(df) {
        fit <- feols(forec_bin_labeled ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(forec = coef(fit)["pos"]))
      },
      fn_proxy_3 = function(df) {
        fit <- feols(forec_bin_llm ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(forec = coef(fit)["pos"]))
      },
      variable = "forec.pos"
    ),
    list(
      name = "Foreign governments",
      fn_true_4 = function(df) {
        fit <- feols(forgov_bin_labeled ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(forgov = coef(fit)["pos"]))
      },
      fn_proxy_4 = function(df) {
        fit <- feols(forgov_bin_llm ~ pos | year+month+weekdays, data=df, cluster=c('year_month'))
        return(c(forgov = coef(fit)["pos"]))
      },
      variable = "forgov.pos"
    )
  )
  
  current_results <- data.frame()
  
  for (i in seq_along(model_functions)) {
    model_info <- model_functions[[i]]
    
    tryCatch({
      # Get the correct function names based on model index
      fn_true_name <- paste0("fn_true_", i)
      fn_proxy_name <- paste0("fn_proxy_", i)
      
      fit_result <- suppressMessages(prisa(
        main_model = model_info[[fn_true_name]],
        proxy_model = model_info[[fn_proxy_name]],
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