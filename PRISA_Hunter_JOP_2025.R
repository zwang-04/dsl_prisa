library(TMB)
library(glmmTMB)
library(dplyr)
library(prisa)
library(here)
library(broom)
library(readr)
setwd(here::here())

sample_size <- 500

results_file <- paste0("PRISA_Hunter_JOP_2025_bootstrap_", sample_size, ".csv")
column_headers <- c("bootstrap_iter", "model", "estimate_name", "n_labeled", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "elss")

write.table(t(column_headers), 
            file = results_file, 
            sep = ",", 
            col.names = FALSE, 
            row.names = FALSE, 
            quote = FALSE)

cat("Created results file with headers:", results_file, "\n")
cat("Using sample size:", sample_size, "\n")

load("EUCO.RData")

# ================================ PRISA ================================

create_prisa_functions <- function() {
  # Model 1: Partisan, Public
  fn_true_m1 <- function(df) {
    fit1_true <- glm(human_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                     data = df, family = "binomial")
    return(c(m1_par = coef(fit1_true)["Partisan.Euroscepticism"],
             m1_pub = coef(fit1_true)["Public.Euroscepticism"]))
  }
  
  fn_proxy_m1 <- function(df) {
    fit1_proxy <- glm(llm_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                      data = df, family = "binomial")
    return(c(m1_par = coef(fit1_proxy)["Partisan.Euroscepticism"],
             m1_pub = coef(fit1_proxy)["Public.Euroscepticism"]))
  }
  
  # Model 2: Partisan, Public
  fn_true_m2 <- function(df) {
    fit2_true <- glm(human_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                     data = df, family = "binomial")
    return(c(m2_par = coef(fit2_true)["Partisan.Euroscepticism"],
             m2_pub = coef(fit2_true)["Public.Euroscepticism"]))
  }
  
  fn_proxy_m2 <- function(df) {
    fit2_proxy <- glm(llm_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                      data = df, family = "binomial")
    return(c(m2_par = coef(fit2_proxy)["Partisan.Euroscepticism"],
             m2_pub = coef(fit2_proxy)["Public.Euroscepticism"]))
  }
  
  # Model 3: Issue
  fn_true_m3 <- function(df) {
    fit3_true <- glm(human_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                     data = df, family = "binomial")
    return(c(m3_iss = coef(fit3_true)["Issue.Salience"]))
  }
  
  fn_proxy_m3 <- function(df) {
    fit3_proxy <- glm(llm_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                      data = df, family = "binomial")
    return(c(m3_iss = coef(fit3_proxy)["Issue.Salience"]))
  }
  
  # Model 4: Issue
  fn_true_m4 <- function(df) {
    fit4_true <- glm(human_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                     data = df, family = "binomial")
    return(c(m4_iss = coef(fit4_true)["Issue.Salience"]))
  }
  
  fn_proxy_m4 <- function(df) {
    fit4_proxy <- glm(llm_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country, 
                      data = df, family = "binomial")
    return(c(m4_iss = coef(fit4_proxy)["Issue.Salience"]))
  }
  
  return(list(
    list(fn_true = fn_true_m1, fn_proxy = fn_proxy_m1, 
         variables = c("m1_par.Partisan.Euroscepticism", "m1_pub.Public.Euroscepticism"), 
         names = c("Partisan Euroscepticism (M1)", "Public Euroscepticism (M1)")),
    list(fn_true = fn_true_m2, fn_proxy = fn_proxy_m2, 
         variables = c("m2_par.Partisan.Euroscepticism", "m2_pub.Public.Euroscepticism"), 
         names = c("Partisan Euroscepticism (M2)", "Public Euroscepticism (M2)")),
    list(fn_true = fn_true_m3, fn_proxy = fn_proxy_m3, 
         variables = c("m3_iss.Issue.Salience"), 
         names = c("Issue Salience (M3)")),
    list(fn_true = fn_true_m4, fn_proxy = fn_proxy_m4, 
         variables = c("m4_iss.Issue.Salience"), 
         names = c("Issue Salience (M4)"))
  ))
}

models_info <- create_prisa_functions()

shot_num <- 0
shot_file <- paste0(shot_num, "_shot_Hunter_JOP_2025_labeled.csv")
if (!file.exists(shot_file)) {
  stop("Error: File ", shot_file, " not found")
}

shot_df <- read.csv(shot_file)
model_name <- "llama_70b"
cat("LLM model:", model_name, "\n")

# Prepare LLM and human labels (align by original_row_id)
idx_in_shot <- match(organized$original_row_id, shot_df$original_row_id)
model_vec_llm <- shot_df[[model_name]][idx_in_shot]
model_vec_human <- shot_df$original_label[idx_in_shot]

model_num_llm <- suppressWarnings(as.numeric(as.character(model_vec_llm)))
model_num_human <- suppressWarnings(as.numeric(as.character(model_vec_human)))

valid_llm_mask <- model_num_llm %in% c(1, 2, 3, 4) & !is.na(model_num_llm)
valid_human_mask <- model_num_human %in% c(1, 2, 3, 4) & !is.na(model_num_human)

if (sum(valid_llm_mask) == 0) {
  stop("No valid LLM labels for ", model_name)
}

cat("Valid LLM samples:", sum(valid_llm_mask), "\n")
cat("Valid human samples:", sum(valid_human_mask), "\n")

# Create complete dataset (all valid LLM labels)
temp_df <- organized[valid_llm_mask, ]
temp_df$llm_style <- model_num_llm[valid_llm_mask]
temp_df$human_style <- model_num_human[valid_llm_mask]  # NA where invalid

cat("Samples with both LLM and human labels:", sum(!is.na(temp_df$human_style)), "\n")

# Derive binary tasks from LLM styles (always available)
temp_df$llm_Claim <- ifelse(temp_df$llm_style == 1, 1, 0)
temp_df$llm_Share <- ifelse(temp_df$llm_style == 2, 1, 0)
temp_df$llm_Blame <- ifelse(temp_df$llm_style == 3, 1, 0)

# Initialize human binary tasks as NA (will be filled only for labeled samples)
temp_df$human_Claim <- NA
temp_df$human_Share <- NA
temp_df$human_Blame <- NA

# Check available labeled sample count
available_labeled <- sum(!is.na(temp_df$human_style))
if (sample_size > available_labeled) {
  stop("Requested sample size ", sample_size, " exceeds available labeled samples ", available_labeled)
}
    
# ================================ Outer Bootstrap Loop ================================
for (bootstrap_iter in 1:100) {
  set.seed(123 + bootstrap_iter + sample_size * 1000)
  labeled_indices <- sample(which(!is.na(temp_df$human_style)), sample_size)

  current_df <- temp_df
  current_df$is_labeled <- 0
  current_df$is_labeled[labeled_indices] <- 1
  
  # Only set human labels for labeled samples
  current_df$human_Claim <- NA
  current_df$human_Share <- NA
  current_df$human_Blame <- NA
  
  # Fill human labels only for labeled samples
  labeled_mask <- current_df$is_labeled == 1
  current_df$human_Claim[labeled_mask] <- ifelse(current_df$human_style[labeled_mask] == 1, 1, 0)
  current_df$human_Share[labeled_mask] <- ifelse(current_df$human_style[labeled_mask] == 2, 1, 0)
  current_df$human_Blame[labeled_mask] <- ifelse(current_df$human_style[labeled_mask] == 3, 1, 0)

  cat("========== Bootstrap", bootstrap_iter, ", Labeled samples for PRISA:", sum(current_df$is_labeled), "========== \n")

  labeled_styles <- current_df$human_style[current_df$is_labeled == 1]
  if (length(unique(labeled_styles[!is.na(labeled_styles)])) < 2) {
    cat("Insufficient variation in labels, skipping", model_name, "\n")
    next
  }

  current_results <- data.frame()

  for (i in seq_along(models_info)) {
    model_info <- models_info[[i]]
    cat("Running PRISA for Model", i, "...\n")

    tryCatch({
      fit_result <- prisa(
        main_model = model_info$fn_true,
        proxy_model = model_info$fn_proxy,
        data = current_df,
        labeled_set_var_name = "is_labeled",
        options = SetOptions(
          n_boot = 100,
          use_full = TRUE,
          use_parallel = FALSE
        )
      )

      estimates <- get_estimates(fit_result)

      for (j in seq_along(model_info$variables)) {
        var_name <- model_info$variables[j]
        estimate_name <- model_info$names[j]

        prisa_result <- estimates %>% 
          filter(variable == var_name, estimator == "prisa")
        labeled_only_result <- estimates %>% 
          filter(variable == var_name, estimator == "labeled_only")

        if (nrow(prisa_result) > 0) {
          current_results <- rbind(current_results, data.frame(
            bootstrap_iter = bootstrap_iter,
            model = model_name,
            estimate_name = estimate_name,
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
            estimate_name = estimate_name,
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
            estimate_name = estimate_name,
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
            estimate_name = estimate_name,
            n_labeled = sample_size,
            method = "labeled_only",
            estimate = NA,
            se = NA,
            ci_lower = NA,
            ci_upper = NA,
            elss = NA
          ))
        }
      }

      cat("✓ Model", i, "completed successfully\n")

    }, error = function(e) {
      cat("✗ Error in Model", i, ":", e$message, "\n")

      for (j in seq_along(model_info$variables)) {
        estimate_name <- model_info$names[j]

        current_results <<- rbind(current_results, data.frame(
          bootstrap_iter = bootstrap_iter,
          model = model_name,
          estimate_name = estimate_name,
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
          estimate_name = estimate_name,
          n_labeled = sample_size,
          method = "labeled_only",
          estimate = NA,
          se = NA,
          ci_lower = NA,
          ci_upper = NA,
          elss = NA
        ))
      }

      cat("    Added NA records for both prisa and labeled_only methods\n")
    })
  }

  if (nrow(current_results) > 0) {
    write.table(current_results, 
                file = results_file, 
                sep = ",", 
                append = TRUE, 
                col.names = FALSE, 
                row.names = FALSE)

    cat("Results saved:", nrow(current_results), "entries for", model_name, "n=", sample_size, "\n")
  }
}

# ================================ Summary ================================
if (file.exists(results_file) && file.size(results_file) > 0) {
  final_results <- read_csv(results_file, show_col_types = FALSE)
  cat("\n=== Final Results Summary ===\n")
  cat("Total results:", nrow(final_results), "\n")
  cat("PRISA results:", nrow(final_results[final_results$method == "prisa", ]), "\n")
  cat("Labeled-only results:", nrow(final_results[final_results$method == "labeled_only", ]), "\n")
  
  bootstrap_summary <- final_results %>%
    filter(method %in% c("prisa", "labeled_only")) %>%
    group_by(bootstrap_iter, model, method) %>%
    summarise(n_estimates = n(), .groups = 'drop')
  
  cat("\nResults by bootstrap iteration and method:\n")
  print(head(bootstrap_summary, 20))
  
} else {
  cat("No results file found or file is empty\n")
}

cat("\nBootstrap analysis completed!\n")