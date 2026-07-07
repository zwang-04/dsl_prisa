library(tidyverse)
library(lfe)
library(lubridate)
library(ggthemes)
library(broom)
library(data.table)
library(magrittr)
library(here)

setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

# ========= Config =========
input_rdata <- "ad_content_humanonly_base.RData"
output_file <- "Fowler_et_al_APSR_2021_estimates.csv"

#### REGRESSIONS ####
base_fe_reg <- function(y, df) {
  felm(as.formula(paste(y, "~ fb | cand_id | 0 | cand_id", sep="")), data=df)
}

options(contrasts = c("contr.treatment", "contr.treatment"))

# ========= Load prerequisites =========
load("ad_dataset_clean.RData")  # for fb_codes_clean, tv_codes_clean

# issues list (replicate from main script)
composite_issue <- c("iss_drugs","iss_fiscal","iss_econ","iss_mil","iss_edu","iss_laworder","iss_foreign","iss_healthcare","iss_env","iss_goodgovt")
low_kappa_issues <- c("issue101","issue12","issue13","issue14","issue15","issue17","issue19","issue20","issue21","issue210","issue212","issue23","issue32","issue33","issue34","issue35","issue36","issue38","issue39","issue40","issue41","issue42","issue44","issue45","issue52","issue57","issue58","issue60","issue61","issue63","issue66","issue71","issue74","issue75","issue80","issue83","issue84","issue90","issue91","issue92","issue94","issue96","issue97","issue98","issue99")
detail_issue_retain <- c("issue210","issue41","issue54","issue106","issue22","issue10","issue24","issue11","issue12","issue20","issue18","issue16","issue17","issue14","issue23","issue19","issue15","issue21","issue50","issue52","issue80","issue83","issue90","issue91","issue60","issue62","issue70","issue53","issue59","issue58","issue55","issue40","issue45","issue43","issue200","issue95","issue30","issue32","issue37","issue102","issue56","issue57","issue31","issue101","issue39","issue93","issue96","issue98","issue103","issue104","issue97")
detail_issue_grouped <- c("issue210","issue41","issue106","issue44","issue33","issue10","issue11","issue12","issue22","issue13","issue20","issue18","issue16","issue14","issue17","issue19","issue23","issue60","issue62","issue50","issue51","issue40","issue45","issue43","issue42","issue212","issue200","issue61","issue63","issue64","issue68","issue69","issue66","issue67","issue70","issue71","issue72","issue73","issue74","issue75","issue76","issue65","issue53","issue54","issue59","issue58","issue55","issue80","issue82","issue83","issue84","issue90","issue91","issue92","issue93","issue39","issue34")
detail_issue_ungrouped <- setdiff(setdiff(detail_issue_retain, detail_issue_grouped), low_kappa_issues)
issue_to_run <- c(composite_issue, detail_issue_ungrouped)

# ========= Load prebuilt base =========
if (!file.exists(input_rdata)) stop("Missing ad_content_humanonly_base.RData. Run build script first.")
load(input_rdata)  # provides ad_content_humanonly_base, cand_ads_fb_template, cand_ads_tv_template, pr_dates_clean
setkey(ad_content_humanonly_base, page_id, snapshot_id)
cat("Loaded base subset rows:", nrow(ad_content_humanonly_base), "\n")

# ========= Prepare shot file list =========
suffix_esc <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
shot_files <- list.files(path    = ANNOTATION_DIR,
                         pattern = paste0(".*_shot_Fowler_et_al_APSR_2021", suffix_esc, "$"))
cat("Found shot files:", paste(shot_files, collapse = ", "), "\n")

if (file.exists(output_file)) {
  file.remove(output_file)
  cat("Removed existing output file:", output_file, "\n")
}

all_results <- tibble()

# ========= Loop over files and models =========
for (shot_file in shot_files) {
  cat("\n===== Processing file:", shot_file, "=====\n")
  n_shot <- str_extract(shot_file, "^\\d+")
  cat("N-shot:", n_shot, "\n")

  shot_labels <- read.csv(file.path(ANNOTATION_DIR, shot_file))
  cat("Loaded", nrow(shot_labels), "rows\n")

  llm_columns <- setdiff(names(shot_labels), c("original_row_id", "original_label"))
  cat("Found LLM models:", paste(llm_columns, collapse = ", "), "\n")

  if (n_shot == "0") {
    all_columns_to_process <- c("original_label", llm_columns)
  } else {
    all_columns_to_process <- llm_columns
  }

  for (model_col in all_columns_to_process) {
    cat("\n----- Processing model:", model_col, "-----\n")

    # Filter labels 0/1/2/3 and recode tones
    shot_labels_filtered <- shot_labels %>% filter(.data[[model_col]] %in% c(0,1,2,3))
    if (nrow(shot_labels_filtered) == 0) { cat("No valid labels, skip\n"); next }

    shot_labels_clean <- shot_labels_filtered %>% mutate(
      ad_tone_attack = case_when(.data[[model_col]] == 3 ~ 1, TRUE ~ 0),
      ad_tone_contrast = case_when(.data[[model_col]] == 1 ~ 1, TRUE ~ 0),
      ad_tone_promote = case_when(.data[[model_col]] == 2 ~ 1, TRUE ~ 0)
    )

    fb_codes <- fb_codes_clean %>% inner_join(
      shot_labels_clean %>% filter(grepl("^fb_", original_row_id)) %>%
        select(original_row_id, ad_tone_attack, ad_tone_contrast, ad_tone_promote),
      by = c("row_id" = "original_row_id")
    )

    tv_codes <- tv_codes_clean %>% inner_join(
      shot_labels_clean %>% filter(grepl("^tv_", original_row_id)) %>%
        select(original_row_id, ad_tone_attack, ad_tone_contrast, ad_tone_promote),
      by = c("row_id" = "original_row_id")
    )

    if (nrow(fb_codes) == 0 && nrow(tv_codes) == 0) { cat("No matching codes, skip\n"); next }

    human_tone_fb <- fb_codes %>% select(page_id, snapshot_id, ad_tone_attack, ad_tone_contrast, ad_tone_promote) %>% as.data.table
    human_tone_tv <- tv_codes %>% mutate(page_id = "TV") %>% select(page_id, snapshot_id, ad_tone_attack, ad_tone_contrast, ad_tone_promote) %>% as.data.table
    human_tone_all <- rbind(human_tone_fb, human_tone_tv)
    setkey(human_tone_all, page_id, snapshot_id)

    # Keep only matched rows and overwrite tones
    ad_content_humanonly <- ad_content_humanonly_base[human_tone_all, on = c("page_id","snapshot_id"), nomatch=0]
    ad_content_humanonly[, `:=`(ad_tone_attack = i.ad_tone_attack,
                                ad_tone_contrast = i.ad_tone_contrast,
                                ad_tone_promote = i.ad_tone_promote)]
    cat("Replaced rows:", nrow(ad_content_humanonly), "\n")

    # merge w/ ad metadata (using preprocessed templates)
    cand_ads_fb <- ad_content_humanonly[cand_ads_fb_template, nomatch=0]
    cand_ads_fb[,`:=` (fb=1, officetype = ifelse(office %in% c("us_house", "us_sen"), "federal", "state"))]

    cand_ads_tv <- ad_content_humanonly[cand_ads_tv_template, nomatch=0]
    cand_ads_tv[,`:=`(fb=0, officetype= ifelse(office %in% c("US House", "US Senate"), "federal", "state"))]

    cand_ads <- rbind(cand_ads_tv, cand_ads_fb, fill=T)
    cat("cand_ads rows:", nrow(cand_ads), "\n")

    cand_ads %<>% .[pr_dates_clean, on =c("state", "officetype"), nomatch=0] %>% .[,`:=` (weeks_to_election = as.numeric(mdy("11/06/2018") - date_start) %/% 7, general = as.numeric(date_start > primary_date))]

    cand_ads[,iss_max := apply(cbind(.SD),MARGIN=1,FUN=max), .SDcols=issue_to_run]

    cand_ad_avg <- cand_ads[!is.na(spend), map(.SD, ~ weighted.mean(., spend, na.rm=T)), by=.(cand_id, fb), .SDcols=grep("iss|tone|hat", colnames(cand_ads), value=T)]
    cand_ad_avg[, iss_hhi := apply(cbind(.SD)^2,MARGIN=1,FUN=sum), .SDcols=issue_to_run]
    cand_ad_avg[, iss_hhi0 := apply((cbind(.SD) / apply(.SD, MARGIN=1, FUN =sum))^2,MARGIN=1,FUN=sum), .SDcols=issue_to_run]

    cand_ad_dev <- cand_ads[!is.na(spend), .(party_hat_sd = sqrt(weighted.mean((party_hat - weighted.mean(party_hat,spend,na.rm=T))^2, spend,na.rm=T)), cfscore_hat_sd = sqrt(weighted.mean((cfscore_hat - weighted.mean(cfscore_hat,spend,na.rm=T))^2, spend,na.rm=T)), party_hat_extrem = weighted.mean(abs(party_hat - 0.5), spend, na.rm=T), cfscore_hat_extrem = weighted.mean(abs(cfscore_hat), spend, na.rm=T)), by=.(cand_id, fb)]
    cand_ad_avg <- cand_ad_avg[cand_ad_dev, on = c("cand_id", "fb")]

    m_tone <- c("ad_tone_attack", "ad_tone_contrast", "ad_tone_promote") %>% map(base_fe_reg, df=cand_ad_avg) %>% set_names(c("Attack", "Contrast", "Promote"))
    m_tone_coef <- m_tone %>% map(broom::tidy) %>% bind_rows(.id="Variable") %>% mutate(ci_ub = estimate + 1.96 * std.error, ci_lb = estimate - 1.96 * std.error) %>% mutate(`Fixed Effects` = "Candidate")

    if (model_col == "original_label") {
      tone_results_formatted <- m_tone_coef %>% mutate(model = "original study", estimate_name = paste0(Variable, " (", `Fixed Effects`, ")"), estimate = estimate, se = std.error, in_context = "original", study = "Fowler_et_al_APSR_2021") %>% select(model, estimate_name, estimate, se, in_context, study)
    } else {
      tone_results_formatted <- m_tone_coef %>% mutate(model = model_col, estimate_name = paste0(Variable, " (", `Fixed Effects`, ")"), estimate = estimate, se = std.error, in_context = paste0(n_shot, "-shot"), study = "Fowler_et_al_APSR_2021") %>% select(model, estimate_name, estimate, se, in_context, study)
    }

    all_results <- bind_rows(all_results, tone_results_formatted)

    file_exists <- file.exists(output_file)
    write.table(tone_results_formatted, file = output_file, sep = ",", row.names = FALSE, col.names = !file_exists, append = file_exists, qmethod = "double")
    cat("Appended", nrow(tone_results_formatted), "rows to", output_file, "\n")
  }
}

all_results <- all_results %>% arrange(model != "original study", in_context, model, estimate_name)
cat("\n=== FINAL SUMMARY ===\n")
cat("Total estimates generated:", nrow(all_results), "\n")
cat("Unique models:", length(unique(all_results$model)), "\n")
cat("Unique contexts:", paste(unique(all_results$in_context), collapse = ", "), "\n")
cat("Incremental results written to:", output_file, "\n")



