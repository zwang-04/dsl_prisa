# M3, M4 n_label not equal to that of M1, M2

library(TMB)
library(glmmTMB)
library(dplyr)
library(dsl)
library(here)
library(broom)
library(readr)
setwd(here::here())

sample_size <- 500

results_file <- paste0("DSL_Hunter_JOP_2025_bootstrap_", sample_size, ".csv")
shot_num <- 0
in_context <- paste0(shot_num, "-shot")
column_headers <- c("bootstrap_iter", "shot", "model", "estimate_name", "n_labeled", "n_total", "method",
                    "estimate", "se", "ci_lower", "ci_upper", "p_value")

write.table(t(column_headers), 
            file = results_file, 
            sep = ",", 
            col.names = FALSE, 
            row.names = FALSE, 
            quote = FALSE)

cat("Created results file with headers:", results_file, "\n")
cat("Using sample size:", sample_size, "\n")

load("EUCO.RData")

# ================================ DSL ================================

create_dsl_models <- function() {
  return(list(
    list(outcome = "human_Claim", pred = "llm_Claim",
         formula = as.formula("human_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country"),
         target_coefs = c("Partisan.Euroscepticism", "Public.Euroscepticism"),
         names = c("Partisan Euroscepticism (M1)", "Public Euroscepticism (M1)")),
    list(outcome = "human_Share", pred = "llm_Share",
         formula = as.formula("human_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country"),
         target_coefs = c("Partisan.Euroscepticism", "Public.Euroscepticism"),
         names = c("Partisan Euroscepticism (M2)", "Public Euroscepticism (M2)")),
    list(outcome = "human_Claim", pred = "llm_Claim",
         formula = as.formula("human_Claim ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country"),
         target_coefs = c("Issue.Salience"),
         names = c("Issue Salience (M3)")),
    list(outcome = "human_Share", pred = "llm_Share",
         formula = as.formula("human_Share ~ Partisan.Euroscepticism + Public.Euroscepticism + Issue.Salience + Gov.Trust + Gov.EU.position + Gov.EU.division + EU.presidency + unemployment + election.year + Country"),
         target_coefs = c("Issue.Salience"),
         names = c("Issue Salience (M4)"))
  ))
}

models_info <- create_dsl_models()

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

  cat("========== Bootstrap", bootstrap_iter, ", Labeled samples:", sum(current_df$is_labeled), "========== \n")

  labeled_styles <- current_df$human_style[current_df$is_labeled == 1]
  if (length(unique(labeled_styles[!is.na(labeled_styles)])) < 2) {
    cat("Insufficient variation in labels, skipping", model_name, "\n")
    next
  }

  current_results <- data.frame()

  for (i in seq_along(models_info)) {
    model_info <- models_info[[i]]
    cat("Running DSL for Model", i, "...\n")

    out <- tryCatch({
      dsl(
        model = "logit",
        formula = model_info$formula,
        predicted_var = model_info$outcome,
        prediction = model_info$pred,
        data = current_df
      )
    }, error = function(e) { cat("✗ DSL error in Model", i, ":", e$message, "\n"); NULL })

    n_labeled_actual <- tryCatch({ if (!is.null(out)) out$internal$num_expert else sample_size }, error = function(e) sample_size)
    n_total <- tryCatch({ if (!is.null(out)) out$internal$num_data else nrow(current_df) }, error = function(e) nrow(current_df))

    coef_tbl <- NULL
    if (!is.null(out)) {
      summ <- tryCatch({ summary(out) }, error = function(e) NULL)
      if (!is.null(summ)) {
        if (is.data.frame(summ) || is.matrix(summ)) {
          coef_tbl <- as.data.frame(summ)
        } else {
          coef_tbl <- tryCatch({ summ$coefficients }, error = function(e) NULL)
          if (is.null(coef_tbl)) coef_tbl <- tryCatch({ summ$coef }, error = function(e) NULL)
        }
      }
    }

    for (j in seq_along(model_info$target_coefs)) {
      coef_name <- model_info$target_coefs[j]
      estimate_name <- model_info$names[j]

      if (!is.null(coef_tbl) && coef_name %in% rownames(coef_tbl)) {
        est <- suppressWarnings(as.numeric(coef_tbl[coef_name, "Estimate"]))
        se  <- suppressWarnings(as.numeric(coef_tbl[coef_name, "Std. Error"]))
        ciL <- suppressWarnings(as.numeric(coef_tbl[coef_name, "CI Lower"]))
        ciU <- suppressWarnings(as.numeric(coef_tbl[coef_name, "CI Upper"]))
        pval <- suppressWarnings(as.numeric(coef_tbl[coef_name, "p value"]))
      } else {
        est <- se <- ciL <- ciU <- pval <- NA
      }

      current_results <- rbind(current_results, data.frame(
        bootstrap_iter = bootstrap_iter,
        shot = in_context,
        model = model_name,
        estimate_name = estimate_name,
        n_labeled = n_labeled_actual,
        n_total = n_total,
        method = "dsl",
        estimate = est,
        se = se,
        ci_lower = ciL,
        ci_upper = ciU,
        p_value = pval
      ))
    }
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
  cat("DSL results:", nrow(final_results[final_results$method == "dsl", ]), "\n")
  
  bootstrap_summary <- final_results %>%
    filter(method %in% c("dsl")) %>%
    group_by(bootstrap_iter, model, method) %>%
    summarise(n_estimates = n(), .groups = 'drop')
  
  cat("\nResults by bootstrap iteration and method:\n")
  print(head(bootstrap_summary, 20))
  
} else {
  cat("No results file found or file is empty\n")
}

cat("\nBootstrap analysis completed!\n")