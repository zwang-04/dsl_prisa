library(tidyverse)
library(data.table)
library(magrittr)
library(here)

setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

# ========= Config =========
output_rdata <- "ad_content_humanonly_base.RData"
if (file.exists(output_rdata)) file.remove(output_rdata)

# ========= Load data required =========
load("tv_ads.RData")
load("fb_ads_meta.RData")
load("cands.RData")
load("dropout_predictions.RData")
load("dropout_predictions_ideology.RData")
load("ad_dataset_clean.RData")
load("primary_dates_2018.RData")

# ========= Prepare base content with ideology =========
base_ad_content <- rbind(X_tagged_pred, X_untagged_pred) %>% as.data.table(keep.rownames="id", sorted = F)
base_ad_content[,c("page_id","snapshot_id"):=split(str_split_fixed(id, fixed("_"),n=2),rep(1:2,each=nrow(base_ad_content)))]
base_ad_content[,id:=NULL]
setkey(base_ad_content, page_id, snapshot_id)

base_ideo_pred <- copy(ideo_pred)
base_ideo_pred[,c("page_id","snapshot_id"):=split(str_split_fixed(ad_id, fixed("_"),n=2),rep(1:2,each=nrow(base_ideo_pred)))]
base_ideo_pred[,ad_id:=NULL]
setkey(base_ideo_pred, page_id, snapshot_id)

base_ad_content_with_ideo <- base_ad_content[base_ideo_pred]

# ========= Find 0-shot file and build unified index =========
suffix_esc   <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
shot_files   <- list.files(path    = ANNOTATION_DIR,
                            pattern = paste0(".*_shot_Fowler_et_al_APSR_2021", suffix_esc, "$"))
cat("Found shot files:", paste(shot_files, collapse = ", "), "\n")

preferred_shots <- c("^0", "^2", "^5", "^10")
index_file <- NA
for (pat in preferred_shots) {
  candidates <- shot_files[str_detect(shot_files, pat)]
  if (length(candidates) > 0) { index_file <- candidates[1]; break }
}
if (is.na(index_file)) stop("No shot file found for Fowler_et_al_APSR_2021.")
zero_shot_file <- index_file

cat("Using 0-shot file for unified index:", zero_shot_file, "\n")
shot_labels_0 <- read.csv(file.path(ANNOTATION_DIR, zero_shot_file))
shot_labels_0_filtered <- shot_labels_0 %>% filter(original_label %in% c(0,1,2,3))

human_union_fb <- fb_codes_clean %>%
  inner_join(
    shot_labels_0_filtered %>%
      filter(grepl("^fb_", original_row_id)) %>%
      dplyr::select(original_row_id),
    by = c("row_id" = "original_row_id")
  ) %>%
  dplyr::select(page_id, snapshot_id)

human_union_tv <- tv_codes_clean %>%
  inner_join(
    shot_labels_0_filtered %>%
      filter(grepl("^tv_", original_row_id)) %>%
      dplyr::select(original_row_id),
    by = c("row_id" = "original_row_id")
  ) %>%
  transmute(page_id = "TV", snapshot_id)

human_coded_index_base <- bind_rows(human_union_fb, human_union_tv) %>% distinct() %>% as.data.table

ad_content_humanonly_base <- base_ad_content_with_ideo[human_coded_index_base, nomatch=0]
setkey(ad_content_humanonly_base, page_id, snapshot_id)
cat("Unified ad_content_humanonly_base rows:", nrow(ad_content_humanonly_base), "\n")

# ========= Prepare metadata for matching =========
# Prepare FB ads metadata
cand_ads_fb_template <- stack_ads %>%
  mutate(spend = (spend.lb + spend.ub) / 2,
         impressions = (impressions.lb + impressions.ub) / 2) %>%
  setDT %>% 
  .[,
    map(.SD, mean, na.rm=T),
    by=.(cand_id, page_id, snapshot_id, date_start, date_stop, office, state), 
    .SDcols = c("spend", "impressions", grep("male", colnames(stack_ads), value=T))]
setkey(cand_ads_fb_template, page_id, snapshot_id)

# Prepare TV ads metadata  
cand_ads_tv_template <- tv_ads %>%
  mutate(page_id = "TV", date_stop=date) %>%
  select(creative, page_id, date_start=date, date_stop, snapshot_id=alt, cand_id, spend=estcost, office, state) %>% 
  setDT(key=c("page_id", "snapshot_id"))

# Prepare primary dates
pr_dates_clean <- pr_dates %>% as.data.table %>% .[,.(officetype, primary_date, state)]

# ========= Save =========
save(ad_content_humanonly_base, cand_ads_fb_template, cand_ads_tv_template, pr_dates_clean, file = output_rdata)
cat("Saved:", output_rdata, "\n")


