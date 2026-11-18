# =============================================================================
# Genocide Perceptions — Pre-Analysis Plan Analysis (Experiment 2: Tactic × Severity)
# Van Nostrand, Schoner & Arnon (2026)
# =============================================================================
# This file contains data from Experiment 2 (Tactic × Severity).
#
# Dependent variables (PAP Table 6):
#   1. exp_1_genocide_force  — Binary genocide classification (Yes=1/No=2 → 1/0)
#   2. exp_1_genocide_leve   — Genocide fit scale (1–5)
#   3. exp1_takeaction       — Support for action (Yes=1/No=2 → 1/0)
#   4–9. exp1_actiontype     — Most-supported response (select one), split into
#                              6 indicators: diplomatic, economic, humanitarian,
#                              legal, military, none
#
# Treatment assignment is encoded in fl_6_do_fl_30 … fl_6_do_fl_36.
# Whichever of those is 1 gives tr_num (1 = fl_30, 2 = fl_31, … 7 = fl_36).
# Condition mapping (user-confirmed):
# =============================================================================

library(tidyverse)
library(estimatr)   # lm_robust for HC2 SEs
library(broom)
library(ggplot2)
library(forcats)

# -----------------------------------------------------------------------------
# 0. Load data
# -----------------------------------------------------------------------------
DATA_PATH <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\Data\Exp1 - Full Data\[EXT] Re_ Item shared with you_ _2026-138a_files.zip_\2026-138a_client.csv)"

d_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

cat("Raw N:", nrow(d_raw), "\n")

# -----------------------------------------------------------------------------
# 1. Derive tr_num from fl_6_do_fl_30 … fl_6_do_fl_36
#    These are 7 binary columns; tr_num = which one equals 1 (1-indexed).
# -----------------------------------------------------------------------------
fl_cols <- paste0("fl_6_do_fl_", 30:36)

d_raw <- d_raw %>%
  mutate(
    tr_num = {
      mat <- across(all_of(fl_cols), ~ as.integer(!is.na(.) & . == 1))
      apply(mat, 1, function(row) {
        idx <- which(row == 1)
        if (length(idx) == 1) idx else NA_integer_
      })
    }
  )

cat("\ntr_num distribution:\n")
print(table(d_raw$tr_num, useNA = "always"))

# -----------------------------------------------------------------------------
# 2. Map tr_num to condition labels (user-confirmed mapping)
# -----------------------------------------------------------------------------
d_raw <- d_raw %>%
  mutate(
    condition = case_when(
      tr_num == 1 ~ "tactic_baseline",
      tr_num == 2 ~ "tactic_kill_low",
      tr_num == 3 ~ "tactic_kill_high",
      tr_num == 4 ~ "tactic_sex_low",
      tr_num == 5 ~ "tactic_sex_high",
      tr_num == 6 ~ "tactic_infra_low",
      tr_num == 7 ~ "tactic_infra_high",
      TRUE        ~ NA_character_
    )
  )

cat("\nCondition distribution:\n")
print(table(d_raw$condition, useNA = "always"))

# Cross-check against treatment_condition column if present
if ("treatment_condition" %in% names(d_raw)) {
  cat("\nCross-check tr_num-derived vs treatment_condition:\n")
  print(table(derived = d_raw$condition,
              original = d_raw$treatment_condition, useNA = "always"))
}

# -----------------------------------------------------------------------------
# 3. Attention check exclusion
#    PAP: "Participants who fail one, or both, of these attention checks will
#    be excluded." attention == 1 = passed.
# -----------------------------------------------------------------------------
cat("\nAttention check values:\n")
print(table(d_raw$attention, useNA = "always"))

d <- d_raw %>% filter(attention == 1)

cat("N after attention filter:", nrow(d), "\n")

# -----------------------------------------------------------------------------
# 4. Recode dependent variables
# -----------------------------------------------------------------------------
d <- d %>%
  mutate(
    # DV1: Binary genocide classification (1=Yes, 2=No → 1/0)
    genocide_binary = case_when(
      exp_1_genocide_force == 1 ~ 1L,
      exp_1_genocide_force == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # DV2: Genocide fit scale (1–5)
    genocide_fit = as.numeric(exp_1_genocide_leve),
    # DV3: Support for international action (1=Yes, 2=No → 1/0)
    support_action = case_when(
      exp1_takeaction == 1 ~ 1L,
      exp1_takeaction == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # DV4–9: Most-supported response (exp1_actiontype, select one) → one indicator per option
    act_diplomatic   = as.integer(exp1_actiontype == 1),
    act_economic     = as.integer(exp1_actiontype == 2),
    act_humanitarian = as.integer(exp1_actiontype == 3),
    act_legal        = as.integer(exp1_actiontype == 4),
    act_military     = as.integer(exp1_actiontype == 5),
    act_none         = as.integer(exp1_actiontype == 6)
  )

# -----------------------------------------------------------------------------
# 5. Ordered factor for treatment condition (control first)
# -----------------------------------------------------------------------------
COND_LEVELS <- c(
  "tactic_baseline",
  "tactic_kill_low",
  "tactic_kill_high",
  "tactic_sex_low",
  "tactic_sex_high",
  "tactic_infra_low",
  "tactic_infra_high"
)

COND_LABELS <- c(
  "No tactic (control)",
  "Killing — hundreds",
  "Killing — tens of thousands",
  "Sexual violence — hundreds",
  "Sexual violence — tens of thousands",
  "Infrastructure — hundreds",
  "Infrastructure — tens of thousands"
)

d <- d %>%
  mutate(
    cond = factor(condition, levels = COND_LEVELS, labels = COND_LABELS)
  )

cat("\nFinal condition × label distribution:\n")
print(table(d$cond, useNA = "always"))

# -----------------------------------------------------------------------------
# 6. Pre-treatment controls (per PAP)
# -----------------------------------------------------------------------------
d <- d %>%
  mutate(
    # Raw CSV stores numeric codes (see 2026-138a_codebook.xlsx)
    ideology_num = as.numeric(ideology),                       # 1 = very liberal … 5
    party_dem    = as.integer(party == 2),                     # 1 Rep, 2 Dem, 3 Ind, 4 Other
    party_rep    = as.integer(party == 1),
    educ_num     = as.numeric(educ),                           # 1–7
    news_num     = 6 - as.numeric(pre_news),                   # recoded so 5 = extremely closely
    age_num      = as.numeric(age),                            # 1 = 18–29 … 8 = 90+
    female       = case_when(gender == 2 ~ 1L, gender == 1 ~ 0L, TRUE ~ NA_integer_)
  )

CONTROLS <- c("ideology_num", "party_dem", "party_rep", "educ_num",
               "news_num", "age_num", "female")

# =============================================================================
# 7. Estimation: OLS with HC2 robust SEs, with and without controls
# =============================================================================
ACT_DVS <- c("act_diplomatic", "act_economic", "act_humanitarian",
             "act_legal", "act_military", "act_none")
DVS    <- c("genocide_binary", "genocide_fit", "support_action", ACT_DVS)
DVLABS <- c("Genocide classification\n(binary)",
            "Genocide fit\n(1–5 scale)",
            "Support for\ninternational action",
            "Most supported: Diplomatic pressure",
            "Most supported: Economic sanctions",
            "Most supported: Humanitarian assistance",
            "Most supported: Legal accountability",
            "Most supported: Military intervention",
            "Most supported: None of the above")

run_models <- function(data, dv, dv_label, controls = CONTROLS) {
  sub <- data %>%
    filter(!is.na(cond), !is.na(.data[[dv]])) %>%
    mutate(cond = droplevels(cond))   # drop levels absent in this subset

  if (nlevels(sub$cond) < 2) {
    message("Skipping ", dv_label, ": fewer than 2 condition levels in data.")
    return(NULL)
  }

  f_no   <- as.formula(paste(dv, "~ cond"))
  ctrl_in <- controls[controls %in% names(sub) &
                        sapply(controls[controls %in% names(sub)],
                               function(v) length(unique(na.omit(sub[[v]]))) > 1)]
  f_ctrl <- as.formula(paste(dv, "~ cond +",
                              paste(ctrl_in, collapse = " + ")))

  tidy_fit <- function(fit, model_type) {
    tidy(fit) %>%
      filter(str_starts(term, "cond")) %>%
      mutate(
        condition = str_remove(term, "^cond"),
        dv        = dv_label,
        model     = model_type
      ) %>%
      select(dv, model, condition, estimate, std.error,
             statistic, p.value, conf.low, conf.high)
  }

  bind_rows(
    tidy_fit(lm_robust(f_no,   data = sub, se_type = "HC2"), "No controls"),
    tidy_fit(lm_robust(f_ctrl, data = sub, se_type = "HC2"), "With controls")
  )
}

all_results <- map2_dfr(DVS, DVLABS, ~ run_models(d, .x, .y))

cat("\nEstimation complete. Rows:", nrow(all_results), "\n")
print(as.data.frame(all_results))

# =============================================================================
# 8. Figure: Tactic × Severity effects on all 6 DVs
# =============================================================================
OUT_DIR <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\Figures)"
dir.create(OUT_DIR, showWarnings = FALSE)

plot_df <- all_results %>%
  mutate(
    model = factor(model, levels = c("No controls", "With controls")),
    sig   = p.value < 0.05,
    dv    = factor(dv, levels = DVLABS),
    condition = factor(condition, levels = rev(COND_LABELS[-1]))  # exclude control (reference)
  )

p <- ggplot(plot_df,
            aes(x = estimate, y = condition, color = model, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 2.5,
             position = position_dodge(width = 0.55)) +
  scale_color_manual(
    values = c("No controls" = "#2166ac", "With controls" = "#d6604d"),
    name   = NULL
  ) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 1),
    labels = c(`TRUE` = "p < 0.05", `FALSE` = "p ≥ 0.05"),
    name   = NULL
  ) +
  facet_wrap(~ dv, scales = "free_x", ncol = 3) +
  labs(
    title    = "Experiment 2: Effect of Tactic × Severity on Genocide Perceptions",
    subtitle = "OLS estimates with 95% CIs (HC2 robust SEs). Reference = no tactic (control).",
    x        = "Estimated effect vs. control",
    y        = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background  = element_rect(fill = "grey92"),
    legend.position   = "bottom",
    plot.title        = element_text(face = "bold", size = 12),
    panel.grid.minor  = element_blank(),
    axis.text.y       = element_text(size = 9)
  )

ggsave(file.path(OUT_DIR, "fig_tactic_severity.png"), p,
       width = 14, height = 11, dpi = 300)
message("Saved: fig_tactic_severity.png")

# =============================================================================
# 9. Summary means by condition
# =============================================================================
summary_table <- d %>%
  filter(!is.na(genocide_binary)) %>%
  group_by(cond) %>%
  summarise(
    n             = n(),
    mean_genocide = mean(genocide_binary, na.rm = TRUE),
    se_genocide   = sd(genocide_binary,   na.rm = TRUE) / sqrt(n()),
    mean_fit      = mean(genocide_fit,    na.rm = TRUE),
    mean_action   = mean(support_action,  na.rm = TRUE),
    across(all_of(ACT_DVS), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    .groups = "drop"
  )

print(summary_table)

write_csv(all_results,    file.path(OUT_DIR, "exp2_treatment_effects.csv"))
write_csv(summary_table,  file.path(OUT_DIR, "exp2_summary_means.csv"))

cat("\nAll outputs saved to:", OUT_DIR, "\n")


