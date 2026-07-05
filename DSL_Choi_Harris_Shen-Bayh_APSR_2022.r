# M5, M6
# ERROR: LU factorization of .gCMatrix failed: out of memory or near-singular 


rm(list = ls())
library(lfe)      # Used for felm() function
library(prisa)     # Used for prisa() and related functions  
library(dsl)
library(here)      # Used for here::here()
library(tidyverse) # Used for filter(), %>%, group_by(), summarise()
library(readr)

setwd(here::here())

# ================================ Initialization ================================
sample_size <- 500

results_file <- paste0("DSL_Choi_Harris_Shen-Bayh_APSR_2022_bootstrap_", sample_size, ".csv")
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
  
  # ================= DSL models =================
  current_results <- data.frame()
  
  dsl_models <- list(
    # M1: no FE -> use lm
    list(name = "Coethnic Match (M1)", model = "lm",
         formula = as.formula("human_labeled ~ match1"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = NULL, index = NULL, cluster = NULL,
         target_coef = "match1"),
    # M2: no FE -> use lm with covariates
    list(name = "Coethnic Match (M2)", model = "lm",
         formula = as.formula("human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = NULL, index = NULL, cluster = NULL,
         target_coef = "match1"),
    # M3: oneway FE cyfe with cluster cyfe
    list(name = "Coethnic Match (M3)", model = "felm",
         formula = as.formula("human_labeled ~ match1"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = "oneway", index = c("cyfe"), cluster = "cyfe",
         target_coef = "match1"),
    # M4: oneway FE cyfe with covariates, cluster cyfe
    list(name = "Coethnic Match (M4)", model = "felm",
         formula = as.formula("human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = "oneway", index = c("cyfe"), cluster = "cyfe",
         target_coef = "match1"),
    # M5: twoways FE cyfe + uid, cluster uid
    list(name = "Coethnic Match (M5)", model = "felm",
         formula = as.formula("human_labeled ~ match1"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = "twoways", index = c("cyfe", "uid"), cluster = "uid",
         target_coef = "match1"),
    # M6: twoways FE cyfe + uid with covariates, cluster uid
    list(name = "Coethnic Match (M6)", model = "felm",
         formula = as.formula("human_labeled ~ match1 + murder + manslaughter + violence + vehicle + arson + drug + theft + public_order + death + prison + stroke"),
         predicted_var = "human_labeled", prediction = "llm_label",
         fe = "twoways", index = c("cyfe", "uid"), cluster = "uid",
         target_coef = "match1")
  )

  for (i in seq_along(dsl_models)) {
    m <- dsl_models[[i]]
    cat("Running DSL for", m$name, "...\n")

    out <- tryCatch({
      if (m$model == "lm") {
        dsl(model = "lm",
            formula = m$formula,
            predicted_var = m$predicted_var,
            prediction = m$prediction,
            data = current_df)
      } else {
        dsl(model = "felm",
            formula = m$formula,
            predicted_var = m$predicted_var,
            prediction = m$prediction,
            fixed_effect = m$fe,
            index = m$index,
            cluster = m$cluster,
            data = current_df)
      }
    }, error = function(e) { cat("  ✗ DSL error:", e$message, "\n"); NULL })

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

    if (!is.null(coef_tbl)) {
      rn <- rownames(coef_tbl)
      idx <- NA_integer_
      if (!is.null(rn) && m$target_coef %in% rn) {
        idx <- match(m$target_coef, rn)
      } else if (!is.null(rn) && length(rn) == 1) {
        idx <- 1L
      } else if (!is.null(rn)) {
        non_intercept <- setdiff(rn, "(Intercept)")
        if (length(non_intercept) == 1) idx <- match(non_intercept, rn)
      }
      if (!is.na(idx)) {
        est <- suppressWarnings(as.numeric(coef_tbl[idx, "Estimate"]))
        se  <- suppressWarnings(as.numeric(coef_tbl[idx, "Std. Error"]))
        ciL <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Lower"]))
        ciU <- suppressWarnings(as.numeric(coef_tbl[idx, "CI Upper"]))
        pval <- suppressWarnings(as.numeric(coef_tbl[idx, "p value"]))
      } else {
        est <- se <- ciL <- ciU <- pval <- NA
      }
    } else {
      est <- se <- ciL <- ciU <- pval <- NA
    }

    current_results <- rbind(current_results, data.frame(
      bootstrap_iter = bootstrap_iter,
      shot = in_context,
      model = model_name,
      estimate_name = m$name,
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
  cat("DSL results:", nrow(final_results[final_results$method == "dsl", ]), "\n")
  
  bootstrap_summary <- final_results %>%
    filter(method %in% c("dsl")) %>%
    group_by(bootstrap_iter, model, method) %>%
    summarise(n_estimates = n(), .groups = 'drop')
  
  cat("\nResults by bootstrap iteration and method:\n")
  print(head(bootstrap_summary, 20))  # Show first 20 rows
  
} else {
  cat("No results file found or file is empty\n")
}

cat("\nBootstrap analysis completed!\n")