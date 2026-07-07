setwd(if (exists("STUDY_DIR")) STUDY_DIR else here::here())

library(tidyverse)
library(data.table)
library(lmtest)
library(sandwich)
library(MASS)

results <- data.frame()

dataset <- fread("dataset.csv")
suffix_esc <- gsub(".", "\\.", ANNOTATION_SUFFIX, fixed = TRUE)
shot_files <- list.files(path    = ANNOTATION_DIR,
                         pattern = paste0("[0-9]+_shot_Gohdes_Steinert_AJPS_2024", suffix_esc, "$"))

clustered_se <- function(logit.model, df) {
  robust.model <- coeftest(logit.model,
                           vcov. = vcovCL(logit.model,
                                          cluster = df[,'user.id'], type = "HC0"))
  return(robust.model)
}

runTopicRegressions <- function(df, type='tweets'){
  if(type=='tweets'){
    m_assadpro <- glm(Assad_pro ~ time*Aleppo, data=df,
                                   family=binomial(link='logit'))
    m_assadanti <- glm(Assad_anti ~ time*Aleppo, data=df,
                                    family=binomial(link='logit'))
    m_camel_pos <- glm(positive ~ time*Aleppo,
                                    data=df, family=binomial(link='logit'))
    m_camel_neg <- glm(negative ~ time*Aleppo,
                                    data=df, family=binomial(link='logit'))
    m_camel_neut <- glm(neutral ~ time*Aleppo,
                                     data=df, family=binomial(link='logit'))
    return(list(m_assadpro=m_assadpro, m_assadanti=m_assadanti, 
                m_camel_pos=m_camel_pos, m_camel_neg=m_camel_neg,
                m_camel_neut=m_camel_neut))
  }
}

var_map <- list(
  "Sentiment: Negative" = "moderation negative sentiment",
  "Sentiment: Positive" = "moderation positive sentiment"
)
sample_map <- list(
  "Geotagged" = "(geo)",
  "Panel" = "(panel)"
)

for (shot_file in shot_files) {
  shot_num <- gsub("_shot_Gohdes_Steinert_AJPS_2024.*$", "", shot_file)
  in_context <- paste0(shot_num, "-shot")
  llm_labels <- fread(file.path(ANNOTATION_DIR, shot_file))
  model_names <- setdiff(names(llm_labels), c("original_row_id", "original_label"))
  if (shot_num == "0" && "original_label" %in% names(llm_labels)) {
    model_names <- c(model_names, "original_label")
  }
  for (model_name in model_names) {
    new_dataset <- dataset
    new_dataset$LLM_label <- llm_labels[[model_name]][match(new_dataset$original_row_id, llm_labels$original_row_id)]
    new_dataset$LLM_label_char <- as.character(new_dataset$LLM_label)
    new_dataset$LLM_label_numeric <- as.numeric(new_dataset$LLM_label_char)
    valid_labels <- c(-1, 0, 1)
    is_valid <- new_dataset$LLM_label_numeric %in% valid_labels
    new_dataset <- new_dataset[is_valid, ]
    new_dataset$negative <- as.integer(new_dataset$LLM_label_numeric == -1)
    new_dataset$neutral <- as.integer(new_dataset$LLM_label_numeric == 0)
    new_dataset$positive <- as.integer(new_dataset$LLM_label_numeric == 1)
    siege_end <- as.Date('2016-12-15')
    dfl <- new_dataset %>%
      filter(created_at <= siege_end+31) %>%
      filter(aleppo_account == 1 | matched_account == 1) %>%
      mutate(user.id = as.character(user.id))
    dfl.panel <- dfl %>% filter(sample == "panel")
    dfl.geotag <- dfl %>% filter(sample == "geotag")
    # Panel
    pl <- runTopicRegressions(dfl.panel)
    pl.nopro <- runTopicRegressions(dfl.panel %>% filter(ProAssad_nongov == 0))
    # Geotag
    tl <- runTopicRegressions(dfl.geotag)
    tl.nopro <- runTopicRegressions(dfl.geotag %>% filter(ProAssad_nongov == 0))
    for (s in list(
      list(obj=tl$m_camel_pos, model="Sentiment: Positive", sample="Geotagged", df=dfl.geotag),
      list(obj=tl$m_camel_neg, model="Sentiment: Negative", sample="Geotagged", df=dfl.geotag),
      list(obj=pl$m_camel_pos, model="Sentiment: Positive", sample="Panel", df=dfl.panel),
      list(obj=pl$m_camel_neg, model="Sentiment: Negative", sample="Panel", df=dfl.panel)
    )) {
      model_obj <- s$obj
      model_type <- s$model
      sample_type <- s$sample
      df_used <- s$df
      # 提取
      coef_names <- names(coef(model_obj))
      idx <- which(coef_names == "time:Aleppo")
      if (length(idx) == 1) {
        model.se <- clustered_se(model_obj, df_used)
        est <- model.se["time:Aleppo", 1]
        se <- model.se["time:Aleppo", 2]
        var_label <- paste(var_map[[model_type]], sample_map[[strsplit(sample_type, ",")[[1]][1]]])
        if (model_name == "original_label" && shot_num == "0") {
          results <- rbind(results, data.frame(
            model = "original study",
            estimate_name = var_label,
            estimate = est,
            se = se,
            in_context = "original",
            study = "Gohdes_Steinert_AJPS_2024",
            stringsAsFactors = FALSE
          ))
        } else if (model_name != "original_label") {
          results <- rbind(results, data.frame(
            model = model_name,
            estimate_name = var_label,
            estimate = est,
            se = se,
            in_context = in_context,
            study = "Gohdes_Steinert_AJPS_2024",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
}

results$is_original <- results$model == 'original study'
results$shot_number <- as.integer(str_match(results$in_context, "^(\\d+)-shot")[,2])
results <- results %>% arrange(desc(is_original), shot_number, model, estimate_name)
results$is_original <- NULL
results$shot_number <- NULL
write.csv(results, "Gohdes_Steinert_AJPS_2024_estimates.csv", row.names = FALSE)
cat("Results saved to Gohdes_Steinert_AJPS_2024_estimates.csv\n") 
