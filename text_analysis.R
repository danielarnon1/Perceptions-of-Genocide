# =============================================================================
# TEXT ANALYSIS — real_world_describe & genocide_open
# Van Nostrand, Schoner & Arnon (2026)
# Run after analysis.R (requires objects: d, OUT_DIR)
# Packages: tidytext, stm, SnowballC, scales, textdata
# =============================================================================

library(tidyverse)
library(tidytext)
library(stm)
library(SnowballC)
library(scales)

# Extra stopwords beyond the standard tidytext list
EXTRA_STOPS <- c(
  "people", "group", "country", "government", "situation",
  "attack", "just", "like", "really", "think", "know",
  "dont", "can", "also", "one", "get", "make", "way",
  "things", "thing", "lot", "much", "many", "even", "still",
  "it", "its", "im", "ive", "id", "thats", "theyre", "theyd"
)

clean_tokens <- function(text_vec, doc_ids) {
  tibble(doc_id = doc_ids, text = text_vec) %>%
    filter(!is.na(text), nchar(trimws(text)) > 2) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% EXTRA_STOPS) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    mutate(word_stem = wordStem(word, language = "en"))
}

# =============================================================================
# A. WORD FREQUENCY BAR CHARTS
# =============================================================================

plot_top_words <- function(tokens_df, title, filename, top_n = 25) {
  df <- tokens_df %>%
    count(word, sort = TRUE) %>%
    slice_head(n = top_n) %>%
    mutate(word = fct_reorder(word, n))

  p <- ggplot(df, aes(x = n, y = word)) +
    geom_col(fill = "#2166ac", alpha = 0.85) +
    geom_text(aes(label = n), hjust = -0.2, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = title, x = "Frequency", y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  ggsave(file.path(OUT_DIR, filename), p, width = 8, height = 8, dpi = 300)
  message("Saved: ", filename)
  p
}

tokens_rw <- clean_tokens(d$real_world_describe, d$case_id)
tokens_go <- clean_tokens(d$genocide_open,       d$case_id)

cat("\nreal_world_describe: ", nrow(tokens_rw), "tokens across",
    n_distinct(tokens_rw$doc_id), "docs\n")
cat("genocide_open:       ", nrow(tokens_go), "tokens across",
    n_distinct(tokens_go$doc_id), "docs\n")

plot_top_words(tokens_rw,
  "Top Words: Real-World Event Associations (real_world_describe)",
  "text_freq_real_world.png")

plot_top_words(tokens_go,
  "Top Words: Open-Ended Genocide Definition (genocide_open)",
  "text_freq_genocide_open.png")

# =============================================================================
# B. SENTIMENT / POLARITY SCORES  (AFINN: numeric -5 to +5 per word)
# =============================================================================

# Download AFINN directly (bypasses textdata's interactive consent prompt)
afinn_url  <- "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-111.txt"
afinn_file <- file.path(tempdir(), "AFINN-111.txt")
if (!file.exists(afinn_file)) {
  download.file(afinn_url, afinn_file, quiet = TRUE, method = "libcurl")
}
afinn <- read_tsv(afinn_file, col_names = c("word", "value"),
                  col_types = cols(word = col_character(), value = col_double()))

score_sentiment <- function(tokens_df, doc_ids) {
  tokens_df %>%
    inner_join(afinn, by = "word") %>%
    group_by(doc_id) %>%
    summarise(
      polarity     = mean(value, na.rm = TRUE),
      n_sent_words = n(),
      .groups      = "drop"
    )
}

sent_rw <- score_sentiment(tokens_rw, d$case_id)
sent_go <- score_sentiment(tokens_go, d$case_id)

cat("\n--- Polarity summary: real_world_describe ---\n")
print(summary(sent_rw$polarity))
cat("\n--- Polarity summary: genocide_open ---\n")
print(summary(sent_go$polarity))

plot_polarity_dist <- function(sent_df, title, filename) {
  df <- sent_df %>% filter(!is.na(polarity))
  mu <- mean(df$polarity, na.rm = TRUE)

  p <- ggplot(df, aes(x = polarity)) +
    geom_histogram(binwidth = 0.25, fill = "#4393c3", color = "white", alpha = 0.85) +
    geom_vline(xintercept = 0,  linetype = "dashed", color = "red",  linewidth = 0.6) +
    geom_vline(xintercept = mu, linetype = "solid",  color = "navy", linewidth = 0.7) +
    annotate("text", x = mu + 0.1, y = Inf,
             label = sprintf("mean = %.2f", mu),
             hjust = 0, vjust = 1.5, color = "navy", size = 3.5) +
    labs(title = title,
         x = "AFINN polarity score (mean per response)",
         y = "Count") +
    theme_bw(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))

  ggsave(file.path(OUT_DIR, filename), p, width = 7, height = 4.5, dpi = 300)
  message("Saved: ", filename)
  p
}

plot_polarity_dist(sent_rw,
  "Polarity Distribution: Real-World Event Associations",
  "polarity_dist_real_world.png")

plot_polarity_dist(sent_go,
  "Polarity Distribution: Open-Ended Genocide Definition",
  "polarity_dist_genocide_open.png")

# =============================================================================
# C. STM TOPIC MODELLING — K = 5 topics, each column separately
#    Prevalence covariate: treatment condition
# =============================================================================

build_stm <- function(tokens_df, all_docs, K = 5, label) {

  cat("\n\n========== STM:", label, "| K =", K, "==========\n")

  # Sparse document-term matrix (stemmed words)
  dtm <- tokens_df %>%
    count(doc_id, word_stem) %>%
    cast_sparse(doc_id, word_stem, n)

  # Align metadata rows to DTM row order
  meta <- tibble(doc_id = rownames(dtm)) %>%
    left_join(
      all_docs %>% select(doc_id = case_id, cond),
      by = "doc_id"
    ) %>%
    mutate(cond_int = as.integer(factor(cond)))

  cat("Docs in DTM:", nrow(dtm), "| Vocabulary:", ncol(dtm), "\n")

  # Fit STM
  fit <- stm(
    documents  = dtm,
    K          = K,
    prevalence = ~ cond_int,
    data       = meta,
    init.type  = "Spectral",
    verbose    = FALSE
  )

  # Top words
  top_w <- labelTopics(fit, n = 10)
  cat("\nTop words per topic (FREX):\n")
  for (k in seq_len(K)) {
    cat(sprintf("  Topic %d: %s\n", k, paste(top_w$frex[k, ], collapse = ", ")))
  }

  # Topic proportions
  theta      <- as.data.frame(fit$theta)
  colnames(theta) <- paste0("topic_", seq_len(K))
  theta$doc_id    <- rownames(dtm)

  theta_long <- theta %>%
    pivot_longer(-doc_id, names_to = "topic", values_to = "proportion") %>%
    left_join(meta %>% select(doc_id, cond), by = "doc_id")

  # Overall topic frequency
  topic_freq <- theta_long %>%
    group_by(topic) %>%
    summarise(mean_prop = mean(proportion), .groups = "drop") %>%
    arrange(desc(mean_prop))

  cat("\nOverall topic frequency:\n")
  print(topic_freq)

  list(fit = fit, theta = theta, theta_long = theta_long,
       topic_freq = topic_freq, meta = meta, dtm = dtm,
       top_words = top_w)
}

# Metadata frame with doc_id and condition
doc_meta <- d %>% select(case_id, cond)

set.seed(42)
stm_rw <- build_stm(tokens_rw, doc_meta, K = 5, "real_world_describe")
stm_go <- build_stm(tokens_go, doc_meta, K = 5, "genocide_open")

# =============================================================================
# D. ASSIGN DOMINANT TOPIC + MERGE POLARITY
# =============================================================================

dominant_topic <- function(theta_long) {
  theta_long %>%
    group_by(doc_id) %>%
    slice_max(proportion, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(doc_id, dominant_topic = topic, cond)
}

dom_rw <- dominant_topic(stm_rw$theta_long) %>%
  left_join(sent_rw, by = "doc_id")

dom_go <- dominant_topic(stm_go$theta_long) %>%
  left_join(sent_go, by = "doc_id")

# =============================================================================
# E. PLOTS: Topic frequency bar + polarity distribution by topic
# =============================================================================

plot_topic_freq <- function(topic_freq_df, top_words, K, title, filename) {
  frex_labels <- sapply(seq_len(K), function(k)
    paste0("T", k, ": ", paste(top_words$frex[k, 1:4], collapse = ", ")))

  df <- topic_freq_df %>%
    mutate(
      topic_num = as.integer(str_extract(topic, "\\d+")),
      label     = frex_labels[topic_num],
      label     = fct_reorder(label, mean_prop)
    )

  p <- ggplot(df, aes(x = mean_prop, y = label)) +
    geom_col(fill = "#2166ac", alpha = 0.85) +
    geom_text(aes(label = percent(mean_prop, accuracy = 0.1)),
              hjust = -0.1, size = 3.2) +
    scale_x_continuous(labels = percent,
                       expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "Mean topic proportion", y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )

  ggsave(file.path(OUT_DIR, filename), p, width = 9, height = 4.5, dpi = 300)
  message("Saved: ", filename)
  p
}

plot_polarity_by_topic <- function(dom_df, top_words, K, title, filename) {
  frex_labels <- sapply(seq_len(K), function(k)
    paste0("T", k, ": ", paste(top_words$frex[k, 1:4], collapse = ", ")))

  df <- dom_df %>%
    filter(!is.na(polarity)) %>%
    mutate(
      topic_num = as.integer(str_extract(dominant_topic, "\\d+")),
      label     = factor(frex_labels[topic_num], levels = frex_labels)
    )

  summ <- df %>%
    group_by(label) %>%
    summarise(mean_pol = mean(polarity), n = n(), .groups = "drop")

  p <- ggplot(df, aes(x = polarity, fill = label)) +
    geom_histogram(binwidth = 0.3, color = "white", alpha = 0.85,
                   show.legend = FALSE) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "red", linewidth = 0.4) +
    geom_vline(data = summ, aes(xintercept = mean_pol),
               color = "navy", linetype = "solid", linewidth = 0.7) +
    geom_text(data = summ,
              aes(x = mean_pol + 0.05, y = Inf,
                  label = sprintf("mu=%.2f\nn=%d", mean_pol, n)),
              hjust = 0, vjust = 1.4, size = 2.8,
              color = "navy", inherit.aes = FALSE) +
    facet_wrap(~ label, ncol = 2, scales = "free_y") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title    = title,
      subtitle = "AFINN polarity per response; navy = topic mean; red dashed = 0",
      x = "Polarity score", y = "Count"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey92"),
      panel.grid.minor = element_blank()
    )

  ggsave(file.path(OUT_DIR, filename), p, width = 10, height = 7, dpi = 300)
  message("Saved: ", filename)
  p
}

plot_topic_freq(stm_rw$topic_freq, stm_rw$top_words, 5,
  "Topic Frequency: Real-World Event Associations",
  "stm_freq_real_world.png")

plot_polarity_by_topic(dom_rw, stm_rw$top_words, 5,
  "Polarity by Topic: Real-World Event Associations",
  "stm_polarity_real_world.png")

plot_topic_freq(stm_go$topic_freq, stm_go$top_words, 5,
  "Topic Frequency: Open-Ended Genocide Definition",
  "stm_freq_genocide_open.png")

plot_polarity_by_topic(dom_go, stm_go$top_words, 5,
  "Polarity by Topic: Open-Ended Genocide Definition",
  "stm_polarity_genocide_open.png")

# =============================================================================
# F. NUMERICAL SUMMARY TABLE
# =============================================================================

summarise_topics <- function(dom_df, top_words, K, col_label) {
  frex_labels <- sapply(seq_len(K), function(k)
    paste(top_words$frex[k, 1:6], collapse = ", "))

  dom_df %>%
    mutate(topic_num = as.integer(str_extract(dominant_topic, "\\d+"))) %>%
    group_by(topic_num) %>%
    summarise(
      n_docs        = n(),
      pct_docs      = n() / nrow(dom_df),
      mean_polarity = mean(polarity, na.rm = TRUE),
      sd_polarity   = sd(polarity,   na.rm = TRUE),
      n_with_sent   = sum(!is.na(polarity)),
      .groups = "drop"
    ) %>%
    mutate(
      column     = col_label,
      frex_words = frex_labels[topic_num]
    ) %>%
    select(column, topic_num, frex_words, n_docs, pct_docs,
           mean_polarity, sd_polarity, n_with_sent) %>%
    arrange(topic_num)
}

summ_rw <- summarise_topics(dom_rw, stm_rw$top_words, 5, "real_world_describe")
summ_go <- summarise_topics(dom_go, stm_go$top_words, 5, "genocide_open")

cat("\n\n========== TOPIC SUMMARY: real_world_describe ==========\n")
print(summ_rw, width = 140)

cat("\n========== TOPIC SUMMARY: genocide_open ==========\n")
print(summ_go, width = 140)

tryCatch(
  write_csv(bind_rows(summ_rw, summ_go),
            file.path(OUT_DIR, "text_topic_polarity_summary.csv")),
  error = function(e) warning("Could not write CSV (file may be open): ", e$message)
)

cat("\nText analysis complete. All figures saved to:", OUT_DIR, "\n")
