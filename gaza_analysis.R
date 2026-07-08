# =============================================================================
# GAZA / ISRAEL / PALESTINE DUMMY + INTERACTION ANALYSIS
# Van Nostrand, Schoner & Arnon (2026)
# Run after analysis.R and text_analysis.R (requires: d, OUT_DIR, sent_rw,
#   tokens_rw, CONTROLS, DVS, DVLABS, COND_LABELS)
# =============================================================================

library(tidyverse)
library(estimatr)
library(broom)
library(ggplot2)
library(forcats)
library(glmnet)

# =============================================================================
# A. GAZA / ISRAEL / PALESTINE DUMMY
# =============================================================================

GAZA_TERMS <- c(
  "israel", "israeli", "isreal",   # common misspelling in data
  "palestine", "palestinian", "palestin",
  "gaza", "west bank", "hamas", "idf",
  "oct 7", "october 7"
)

# Build pattern from the stem/word token list AND raw text search
gaza_pattern <- paste(
  c("israel", "isreal", "palestin", "gaza", "hamas", "idf",
    "west bank", "oct 7", "october 7"),
  collapse = "|"
)

d <- d %>%
  mutate(
    gaza_dummy = as.integer(
      str_detect(tolower(coalesce(real_world_describe, "")), gaza_pattern)
    )
  )

cat("\nGaza dummy distribution:\n")
print(table(d$gaza_dummy, useNA = "always"))
cat(sprintf("  %.1f%% of respondents mentioned Gaza/Israel/Palestine\n",
            100 * mean(d$gaza_dummy, na.rm = TRUE)))

# Histogram
p_hist <- ggplot(d, aes(x = factor(gaza_dummy,
                                    labels = c("0 — no mention", "1 — mentioned")))) +
  geom_bar(fill = c("#4393c3", "#d6604d"), alpha = 0.85, width = 0.5) +
  geom_text(stat = "count",
            aes(label = after_stat(paste0(count, "\n(",
                                          round(count / nrow(d) * 100, 1), "%)"))),
            vjust = -0.4, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title   = "Gaza / Israel / Palestine Mention in real_world_describe",
    x       = NULL, y = "Count"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(OUT_DIR, "hist_gaza_dummy.png"), p_hist,
       width = 6, height = 4.5, dpi = 300)
message("Saved: hist_gaza_dummy.png")

# =============================================================================
# B. MERGE POLARITY SCORE ONTO d
# =============================================================================

d <- d %>%
  left_join(
    sent_rw %>% rename(polarity_rw = polarity,
                       n_sent_words_rw = n_sent_words),
    by = c("case_id" = "doc_id")
  )

cat("\nPolarity (real_world_describe) merged. Non-NA:", sum(!is.na(d$polarity_rw)), "\n")

# =============================================================================
# C. INTERACTION PLOTS: treatment × Gaza dummy for each MAIN DV
#    Model: DV ~ cond * gaza_dummy  (with controls)
# =============================================================================

run_interaction <- function(data, dv, dv_label, controls = CONTROLS) {
  sub <- data %>%
    filter(!is.na(cond), !is.na(.data[[dv]]), !is.na(gaza_dummy)) %>%
    mutate(cond = droplevels(cond),
           gaza_f = factor(gaza_dummy, levels = c(0, 1),
                           labels = c("No Gaza mention", "Gaza mentioned")))

  if (nlevels(sub$cond) < 2) return(NULL)

  ctrl_in <- controls[controls %in% names(sub) &
                        sapply(controls[controls %in% names(sub)],
                               function(v) length(unique(na.omit(sub[[v]]))) > 1)]

  f_int <- as.formula(
    paste(dv, "~ cond * gaza_f +", paste(ctrl_in, collapse = " + "))
  )
  fit <- lm_robust(f_int, data = sub, se_type = "HC2")

  # Marginal means: predict at each cond × gaza_f combination
  pred_grid <- expand.grid(
    cond   = levels(sub$cond),
    gaza_f = levels(sub$gaza_f),
    stringsAsFactors = FALSE
  )
  # Set controls to their means/modes
  for (v in ctrl_in) {
    pred_grid[[v]] <- mean(sub[[v]], na.rm = TRUE)
  }
  pred_grid$cond   <- factor(pred_grid$cond,   levels = levels(sub$cond))
  pred_grid$gaza_f <- factor(pred_grid$gaza_f, levels = levels(sub$gaza_f))

  preds <- predict(fit, newdata = pred_grid, se.fit = TRUE)
  pred_grid$estimate <- preds$fit
  pred_grid$se       <- preds$se.fit
  pred_grid$lo       <- pred_grid$estimate - 1.96 * pred_grid$se
  pred_grid$hi       <- pred_grid$estimate + 1.96 * pred_grid$se
  pred_grid$dv       <- dv_label

  pred_grid
}

all_int <- map2_dfr(DVS, DVLABS, ~ run_interaction(d, .x, .y))

# Plot
plot_interactions <- function(int_df, dv_label) {
  df <- int_df %>%
    filter(dv == dv_label) %>%
    mutate(cond = factor(cond, levels = levels(d$cond)))

  ggplot(df, aes(x = cond, y = estimate, color = gaza_f, group = gaza_f)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = gaza_f),
                alpha = 0.12, color = NA) +
    scale_color_manual(values = c("No Gaza mention" = "#2166ac",
                                  "Gaza mentioned"  = "#d6604d"),
                       name = NULL) +
    scale_fill_manual(values  = c("No Gaza mention" = "#2166ac",
                                  "Gaza mentioned"  = "#d6604d"),
                      name = NULL, guide = "none") +
    labs(title = dv_label,
         x = "Treatment condition", y = "Predicted value (95% CI)") +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x     = element_text(angle = 30, hjust = 1, size = 8),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold")
    )
}

int_plots <- map(DVLABS, ~ plot_interactions(all_int, .x))

library(patchwork)
p_int_grid <- wrap_plots(int_plots, ncol = 3) +
  plot_annotation(
    title    = "Treatment × Gaza/Israel/Palestine Mention — Interaction Effects",
    subtitle = "Predicted means with 95% CIs; controls at their sample means",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10))
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "fig_interaction_gaza.png"), p_int_grid,
       width = 15, height = 9, dpi = 300)
message("Saved: fig_interaction_gaza.png")

# =============================================================================
# D. POLARITY AS OUTCOME — regressions across all DVs
#    (treatment + polarity as DV; then polarity ~ treatment * Gaza)
# =============================================================================

# D1. Polarity ~ cond (with controls), with and without Gaza dummy
run_polarity_models <- function(data, controls = CONTROLS) {
  sub <- data %>%
    filter(!is.na(cond), !is.na(polarity_rw)) %>%
    mutate(cond = droplevels(cond))

  ctrl_in <- controls[controls %in% names(sub) &
                        sapply(controls[controls %in% names(sub)],
                               function(v) length(unique(na.omit(sub[[v]]))) > 1)]

  f_no   <- as.formula("polarity_rw ~ cond")
  f_ctrl <- as.formula(paste("polarity_rw ~ cond +", paste(ctrl_in, collapse = " + ")))

  tidy_fit <- function(fit, lbl) {
    tidy(fit) %>%
      filter(str_starts(term, "cond")) %>%
      mutate(condition = str_remove(term, "^cond"),
             model     = lbl) %>%
      select(model, condition, estimate, std.error, p.value, conf.low, conf.high)
  }

  bind_rows(
    tidy_fit(lm_robust(f_no,   data = sub, se_type = "HC2"), "No controls"),
    tidy_fit(lm_robust(f_ctrl, data = sub, se_type = "HC2"), "With controls")
  )
}

pol_results <- run_polarity_models(d)
cat("\n--- Polarity ~ treatment (reference = control) ---\n")
print(as.data.frame(pol_results))

# Coefficient plot for polarity outcome
pol_plot_df <- pol_results %>%
  mutate(
    model     = factor(model, levels = c("No controls", "With controls")),
    sig       = p.value < 0.05,
    condition = factor(condition, levels = rev(COND_LABELS[-1]))
  )

p_pol <- ggplot(pol_plot_df,
                aes(x = estimate, y = condition, color = model, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.55)) +
  scale_color_manual(values = c("No controls"   = "#2166ac",
                                "With controls" = "#d6604d"),
                     name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "p >= 0.05"),
                     name   = NULL) +
  labs(
    title    = "Effect of Treatment on Sentiment Polarity (real_world_describe)",
    subtitle = "OLS with HC2 SEs. Reference = no tactic (control).",
    x        = "Estimated effect on AFINN polarity score",
    y        = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "fig_polarity_treatment.png"), p_pol,
       width = 8, height = 5, dpi = 300)
message("Saved: fig_polarity_treatment.png")

# D2. Interaction plot: polarity ~ cond × Gaza dummy
pol_int <- run_interaction(d, "polarity_rw", "Sentiment polarity\n(real_world_describe)")
p_pol_int <- plot_interactions(pol_int, "Sentiment polarity\n(real_world_describe)") +
  labs(title = "Polarity ~ Treatment × Gaza Mention",
       y     = "Predicted AFINN polarity (95% CI)")

ggsave(file.path(OUT_DIR, "fig_polarity_interaction_gaza.png"), p_pol_int,
       width = 8, height = 5, dpi = 300)
message("Saved: fig_polarity_interaction_gaza.png")

# =============================================================================
# E. BEST FIVE PREDICTORS OF POLARITY (real_world_describe)
#    Candidate features: demographics + survey variables (no treatments)
# =============================================================================

# Candidate predictors: already-recoded numeric demographics + media use + Gaza dummy
# Social media / news cols: find soc_med_* and pre_news_* that are numeric
soc_cols <- names(d)[str_detect(names(d), "^soc_med_")] %>%
  keep(~ is.numeric(d[[.x]]) && sum(!is.na(d[[.x]])) > 1500)

all_pred_vars <- c(
  "ideology_num", "party_dem", "party_rep",
  "educ_num", "news_num", "age_num", "female",
  "gaza_dummy",
  soc_cols
)
all_pred_vars <- intersect(all_pred_vars, names(d))

cat("\nCandidate predictors (", length(all_pred_vars), "):\n",
    paste(all_pred_vars, collapse = ", "), "\n")

# Build modelling dataset: rows with non-NA polarity; median-impute other NAs
pol_df <- d %>%
  filter(!is.na(polarity_rw)) %>%
  select(polarity_rw, all_of(all_pred_vars)) %>%
  mutate(across(
    where(is.numeric),
    ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))
  ))

cat("\nPrediction dataset: n =", nrow(pol_df), "| predictors =",
    ncol(pol_df) - 1, "\n")

y <- pol_df$polarity_rw

# Build X: drop columns that are constant or all-NA after imputation
X_raw <- pol_df %>% select(-polarity_rw)
ok_cols <- sapply(X_raw, function(v) {
  v2 <- na.omit(v)
  length(v2) > 0 && length(unique(v2)) > 1
})
X_raw <- X_raw[, ok_cols, drop = FALSE]
# Final NA imputation pass (handles all-NA cols that survived by being dropped above)
X_raw <- X_raw %>%
  mutate(across(everything(),
                ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))))
X <- as.matrix(X_raw)
cat("X matrix:", nrow(X), "rows x", ncol(X), "cols\n")

# LASSO for variable selection (alpha = 1)
set.seed(42)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
best_lambda <- cv_lasso$lambda.min

lasso_coefs <- coef(cv_lasso, s = "lambda.min")
lasso_df <- tibble(
  predictor = rownames(lasso_coefs)[-1],
  coef      = as.numeric(lasso_coefs)[-1]
) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))

cat("\nLASSO selected", nrow(lasso_df), "non-zero predictors:\n")
print(as.data.frame(lasso_df), digits = 4)

top5 <- lasso_df %>% slice_head(n = 5)

cat("\n\n===== TOP 5 PREDICTORS OF POLARITY (real_world_describe) =====\n")
print(as.data.frame(top5), digits = 4)

# OLS with just the top-5 for interpretable coefficients
if (nrow(top5) >= 1) {
  top5_vars <- top5$predictor
  # Map back to original column names (model.matrix uses numeric suffixes for factors)
  top5_orig <- intersect(top5_vars, names(pol_df))
  if (length(top5_orig) < length(top5_vars)) {
    # Some are dummy-expanded factor levels — identify parent columns
    for (v in setdiff(top5_vars, top5_orig)) {
      match_col <- names(pol_df)[sapply(names(pol_df), function(cn) str_starts(v, cn))]
      top5_orig <- union(top5_orig, match_col)
    }
  }
  top5_orig <- intersect(top5_orig, names(pol_df))

  if (length(top5_orig) >= 1) {
    f_top5 <- as.formula(paste("polarity_rw ~",
                                paste(top5_orig, collapse = " + ")))
    fit_top5 <- lm_robust(f_top5, data = pol_df, se_type = "HC2")
    cat("\nOLS with top-5 predictors:\n")
    print(tidy(fit_top5))
  }
}

# Coefficient plot for LASSO selected predictors
p_lasso <- ggplot(lasso_df %>% mutate(predictor = fct_reorder(predictor, abs(coef))),
                  aes(x = coef, y = predictor,
                      fill = ifelse(coef > 0, "Positive", "Negative"))) +
  geom_col(alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = c("Positive" = "#2166ac", "Negative" = "#d6604d"),
                    name = "Direction") +
  labs(
    title    = "LASSO Predictors of Sentiment Polarity (real_world_describe)",
    subtitle = sprintf("Lambda = %.4f (10-fold CV); %d predictors selected",
                       best_lambda, nrow(lasso_df)),
    x = "LASSO coefficient", y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

ggsave(file.path(OUT_DIR, "fig_lasso_polarity_predictors.png"), p_lasso,
       width = 8, height = max(4, 0.4 * nrow(lasso_df) + 2), dpi = 300)
message("Saved: fig_lasso_polarity_predictors.png")

# Save results
write_csv(pol_results,
          file.path(OUT_DIR, "polarity_treatment_effects.csv"))
write_csv(lasso_df,
          file.path(OUT_DIR, "polarity_lasso_predictors.csv"))

# =============================================================================
# F. TREATMENT × GAZA DUMMY INTERACTION — genocide_binary & genocide_fit
#    Full OLS with HC2 SEs; extract interaction terms; marginal-means plot
# =============================================================================

MAIN_DVS  <- c("genocide_binary", "genocide_fit")
MAIN_LABS <- c("Genocide classification (binary)", "Genocide fit (1–7 scale)")

run_gaza_interaction <- function(data, dv, dv_label, controls = CONTROLS) {
  sub <- data %>%
    filter(!is.na(cond), !is.na(.data[[dv]]), !is.na(gaza_dummy)) %>%
    mutate(
      cond   = droplevels(cond),
      gaza_f = factor(gaza_dummy, levels = c(0, 1),
                      labels = c("No Gaza mention", "Gaza mentioned"))
    )
  if (nlevels(sub$cond) < 2) return(NULL)

  ctrl_in <- controls[controls %in% names(sub) &
    sapply(controls[controls %in% names(sub)],
           function(v) length(unique(na.omit(sub[[v]]))) > 1)]

  f_int <- as.formula(
    paste(dv, "~ cond * gaza_f +", paste(ctrl_in, collapse = " + "))
  )
  fit <- lm_robust(f_int, data = sub, se_type = "HC2")

  # --- marginal means grid ---
  grid <- expand.grid(
    cond   = levels(sub$cond),
    gaza_f = levels(sub$gaza_f),
    stringsAsFactors = FALSE
  )
  for (v in ctrl_in) grid[[v]] <- mean(sub[[v]], na.rm = TRUE)
  grid$cond   <- factor(grid$cond,   levels = levels(sub$cond))
  grid$gaza_f <- factor(grid$gaza_f, levels = levels(sub$gaza_f))

  pr           <- predict(fit, newdata = grid, se.fit = TRUE)
  grid$pred    <- pr$fit
  grid$pred_lo <- pr$fit - 1.96 * pr$se.fit
  grid$pred_hi <- pr$fit + 1.96 * pr$se.fit
  grid$dv      <- dv_label

  # --- interaction coefficients (cond:gaza_f terms) ---
  int_coefs <- tidy(fit) %>%
    filter(str_detect(term, ":")) %>%
    mutate(
      condition = str_remove(term, "^cond") %>%
                  str_remove(":gaza_fGaza mentioned"),
      dv        = dv_label
    ) %>%
    select(dv, condition, estimate, std.error, p.value, conf.low, conf.high)

  list(grid = grid, int_coefs = int_coefs, fit = fit)
}

res_binary <- run_gaza_interaction(d, "genocide_binary", MAIN_LABS[1])
res_fit    <- run_gaza_interaction(d, "genocide_fit",    MAIN_LABS[2])

# Print interaction coefficients
cat("\n--- Interaction coefficients: genocide_binary ~ cond × Gaza ---\n")
print(as.data.frame(res_binary$int_coefs))
cat("\n--- Interaction coefficients: genocide_fit ~ cond × Gaza ---\n")
print(as.data.frame(res_fit$int_coefs))

# Write interaction terms
write_csv(
  bind_rows(res_binary$int_coefs, res_fit$int_coefs),
  file.path(OUT_DIR, "gaza_interaction_coefs.csv")
)

# --- Marginal means plot per DV ---
make_marginal_plot <- function(grid_df, dv_label, y_label, filename) {
  df <- grid_df %>%
    mutate(cond = factor(cond, levels = levels(d$cond)))

  ggplot(df, aes(x = cond, y = pred,
                 color = gaza_f, group = gaza_f)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = pred_lo, ymax = pred_hi, fill = gaza_f),
                alpha = 0.13, color = NA) +
    scale_color_manual(
      values = c("No Gaza mention" = "#2166ac", "Gaza mentioned" = "#d6604d"),
      name   = NULL
    ) +
    scale_fill_manual(
      values = c("No Gaza mention" = "#2166ac", "Gaza mentioned" = "#d6604d"),
      name   = NULL, guide = "none"
    ) +
    labs(
      title    = dv_label,
      subtitle = "OLS predicted means ± 95% CI (HC2 SEs); controls at sample means",
      x        = "Treatment condition",
      y        = y_label
    ) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x     = element_text(angle = 30, hjust = 1, size = 9),
      legend.position = "bottom",
      plot.title      = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

p_bin <- make_marginal_plot(
  res_binary$grid,
  "Treatment × Gaza Mention: Genocide Classification",
  "Pr(classified as genocide)", "fig_int_genocide_binary.png"
)
p_fit <- make_marginal_plot(
  res_fit$grid,
  "Treatment × Gaza Mention: Genocide Fit Scale",
  "Genocide fit (1–7)", "fig_int_genocide_fit.png"
)

ggsave(file.path(OUT_DIR, "fig_int_genocide_binary.png"), p_bin,
       width = 9, height = 5.5, dpi = 300)
message("Saved: fig_int_genocide_binary.png")

ggsave(file.path(OUT_DIR, "fig_int_genocide_fit.png"), p_fit,
       width = 9, height = 5.5, dpi = 300)
message("Saved: fig_int_genocide_fit.png")

# --- Coefficient plot of interaction terms (both DVs together) ---
int_all <- bind_rows(res_binary$int_coefs, res_fit$int_coefs) %>%
  mutate(
    sig       = p.value < 0.05,
    dv        = factor(dv, levels = MAIN_LABS),
    condition = factor(condition, levels = rev(COND_LABELS[-1]))
  )

p_int_coef <- ggplot(int_all,
                     aes(x = estimate, y = condition,
                         color = dv, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.3,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6)) +
  scale_color_manual(
    values = c("Genocide classification (binary)" = "#2166ac",
               "Genocide fit (1–7 scale)"         = "#d6604d"),
    name = "Outcome"
  ) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 1),
    labels = c(`TRUE` = "p < 0.05", `FALSE` = "p >= 0.05"),
    name   = NULL
  ) +
  labs(
    title    = "Interaction: Treatment × Gaza Mention on Genocide Perceptions",
    subtitle = "Coefficient on cond × Gaza_mentioned; reference = no-tactic control × no mention",
    x        = "Interaction estimate (HC2 SEs, 95% CI)",
    y        = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "fig_int_coefs_both_dvs.png"), p_int_coef,
       width = 9, height = 6, dpi = 300)
message("Saved: fig_int_coefs_both_dvs.png")

cat("\nGaza analysis complete. All outputs saved to:", OUT_DIR, "\n")
