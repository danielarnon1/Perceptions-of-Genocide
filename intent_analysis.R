# =============================================================================
# Experiment 1 (Intent): treatment effects on the main DVs
# Mirrors analysis.R (Exp 2): OLS, HC2 robust SEs, reference = no-intent control.
# Both specifications adjust for failed_recall (vignette recall-check failures
# are retained rather than excluded). Outputs go to Figures/intent/.
# =============================================================================

source(r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\intent_prep.R)")
suppressMessages({
  library(estimatr)
  library(broom)
  library(scales)
})

d <- intent_all
DEMOG <- setdiff(INTENT_CONTROLS, "failed_recall")

DVS <- tribble(
  ~dv,                ~label,
  "genocide_binary",  "Genocide classification (binary)",
  "genocide_fit",     "Genocide fit (1–5 scale)",
  "support_action",   "Obligation to respond (Yes vs. No)",
  "act_diplomatic",   "Respond: Diplomatic pressure",
  "act_economic",     "Respond: Economic sanctions",
  "act_humanitarian", "Respond: Humanitarian assistance",
  "act_legal",        "Respond: Legal accountability",
  "act_military",     "Respond: Military intervention",
  "act_none",         "Respond: None of the above"
)
MODELS <- c("No controls" = "failed_recall",
            "With controls" = paste(c("failed_recall", DEMOG), collapse = " + "))

tidy_arm <- function(fit) {
  tidy(fit) %>% filter(str_starts(term, "arm")) %>%
    mutate(condition = str_remove(term, "^arm")) %>%
    select(condition, estimate, std.error, statistic, p.value, conf.low, conf.high)
}

run_dv <- function(dv, label, data = d, ref = "No intent (control)") {
  data <- mutate(data, arm = relevel(arm, ref))
  imap_dfr(MODELS, function(rhs, model) {
    fit <- lm_robust(as.formula(paste(dv, "~ arm +", rhs)), data = data, se_type = "HC2")
    tidy_arm(fit) %>% mutate(dv = label, model = model, reference = ref, n = nobs(fit), .before = 1)
  })
}

# -----------------------------------------------------------------------------
# 1. Effects vs. control
# -----------------------------------------------------------------------------
effects <- pmap_dfr(DVS, run_dv) %>% select(-reference)
write_csv(effects, file.path(INTENT_OUT, "intent_treatment_effects.csv"))

cat("\n== Treatment effects vs. no-intent control ==\n")
effects %>%
  mutate(est = sprintf("%6.3f [%6.3f, %6.3f]%s", estimate, conf.low, conf.high,
                       case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", TRUE ~ "")),
         condition = recode(condition, !!!setNames(ARM_SHORT, ARM_LABELS))) %>%
  select(dv, model, condition, est) %>%
  pivot_wider(names_from = condition, values_from = est) %>%
  as.data.frame() %>% print(right = FALSE)

# -----------------------------------------------------------------------------
# 2. Contrasts among treatment arms (no-controls spec)
# -----------------------------------------------------------------------------
contrasts <- map_dfr(c("Denies intent", "Denies intent + security justification"), function(ref) {
  pmap_dfr(DVS, function(dv, label) run_dv(dv, label, ref = ref)) %>%
    filter(model == "No controls", condition != "No intent (control)")
}) %>%
  filter(!(reference == "Denies intent + security justification" & condition == "Denies intent")) %>%
  mutate(contrast = paste(recode(condition, !!!setNames(ARM_SHORT, ARM_LABELS)), "vs.",
                          recode(reference, !!!setNames(ARM_SHORT, ARM_LABELS))))
write_csv(contrasts, file.path(INTENT_OUT, "intent_arm_contrasts.csv"))

cat("\n== Contrasts among treatment arms (no controls) ==\n")
contrasts %>%
  transmute(dv, contrast,
            est =sprintf("%6.3f [%6.3f, %6.3f]", estimate, conf.low, conf.high), p = round(p.value, 3)) %>%
  as.data.frame() %>% print(right = FALSE)

# -----------------------------------------------------------------------------
# 3. Robustness: obligation DV excluding "Not sure"; unadjusted for recall flag
# -----------------------------------------------------------------------------
rob <- bind_rows(
  tidy_arm(lm_robust(I(exp1_takeaction == 1) ~ arm, data = d, se_type = "HC2")) %>%
    mutate(check = "Obligation, Not sure coded as No"),
  tidy_arm(lm_robust(genocide_binary ~ arm, data = d, se_type = "HC2")) %>%
    mutate(check = "Genocide binary, no recall adjustment"),
  tidy_arm(lm_robust(genocide_binary ~ arm, data = filter(d, passed_recall), se_type = "HC2")) %>%
    mutate(check = "Genocide binary, recall failures excluded (PAP rule)")
) %>% as_tibble()
cat("\n== Robustness checks ==\n")
print(rob %>% select(check, condition, estimate, conf.low, conf.high, p.value) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf, width = 200)
write_csv(rob, file.path(INTENT_OUT, "intent_robustness.csv"))

# -----------------------------------------------------------------------------
# 4. Group means by arm
# -----------------------------------------------------------------------------
means <- d %>%
  group_by(arm, arm_s) %>%
  summarise(n = n(), across(all_of(DVS$dv), list(mean = ~ mean(.x, na.rm = TRUE),
                                                 se = ~ sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x))))),
            .groups = "drop")
write_csv(means, file.path(INTENT_OUT, "intent_summary_means.csv"))

means_long <- means %>%
  pivot_longer(-c(arm, arm_s, n), names_to = c("dv", ".value"), names_pattern = "(.*)_(mean|se)$") %>%
  left_join(DVS, by = "dv") %>%
  mutate(label = factor(label, DVS$label), arm_s = factor(arm_s, rev(ARM_SHORT)))

# -----------------------------------------------------------------------------
# 5. Figures
# -----------------------------------------------------------------------------
INK <- "#1b2433"; MUTED <- "#6a7890"; GRID <- "#e3e8f0"
ARM_COLS <- c("Control" = "#7a8a9a", "Denial" = "#5b7fb0",
              "Denial + justify" = "#b08a3a", "Stated intent" = "#c05020")
theme_intent <- function(base = 10) {
  theme_minimal(base_size = base) +
    theme(text = element_text(color = INK),
          plot.title = element_text(face = "bold", size = base + 3),
          plot.subtitle = element_text(color = MUTED, size = base),
          plot.caption = element_text(color = MUTED, size = base - 2, hjust = 0),
          strip.text = element_text(face = "bold", hjust = 0, size = base),
          axis.text = element_text(color = MUTED),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = GRID),
          panel.spacing = unit(1.2, "lines"), legend.position = "top")
}

plot_eff <- effects %>%
  mutate(condition = factor(recode(condition, !!!setNames(ARM_SHORT, ARM_LABELS)), rev(ARM_SHORT[-1])),
         dv = factor(dv, DVS$label),
         sig = factor(p.value < .05, c(TRUE, FALSE), c("p < .05", "p ≥ .05")))

p_eff <- ggplot(plot_eff, aes(estimate, condition, color = model, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = MUTED, linewidth = .35) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2,
                 position = position_dodge(width = .55)) +
  geom_point(size = 2.3, fill = "white", position = position_dodge(width = .55)) +
  scale_color_manual(values = c("No controls" = "#2e7ab4", "With controls" = "#c05020"), name = NULL) +
  scale_shape_manual(values = c("p < .05" = 16, "p ≥ .05" = 21), name = NULL, drop = FALSE) +
  facet_wrap(~ dv, scales = "free_x", ncol = 3) +
  labs(title = "Experiment 1: Effect of intent statements on genocide perceptions",
       subtitle = "OLS estimates with 95% CIs (HC2 robust SEs). Reference = no intent statement (control).",
       x = "Estimated effect vs. control", y = NULL,
       caption = sprintf("N = %s. Both specifications adjust for vignette recall-check failure; 'With controls' adds ideology, party, education, news attention, age, and gender.",
                         comma(nrow(d)))) +
  theme_intent()
ggsave(file.path(INTENT_OUT, "fig_intent_effects.png"), p_eff, width = 13, height = 9.5, dpi = 300, bg = "white")

fmt_mean <- function(dv, m) if (dv == "genocide_fit") sprintf("%.2f", m) else percent(m, .1)
p_means <- ggplot(means_long, aes(mean, arm_s, fill = arm_s)) +
  geom_col(width = .68) +
  geom_errorbarh(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se), height = .2, color = INK, linewidth = .3) +
  geom_text(aes(x = mean + 1.96 * se, label = map2_chr(dv, mean, fmt_mean)),
            hjust = -0.2, size = 2.9, color = INK) +
  scale_fill_manual(values = ARM_COLS, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .25))) +
  facet_wrap(~ label, scales = "free_x", ncol = 3) +
  labs(title = "Experiment 1: Outcome means by intent condition",
       subtitle = "Raw group means with 95% CIs. Shares for binary outcomes; 1–5 mean for genocide fit.",
       x = NULL, y = NULL, caption = sprintf("N = %s (including recall-check failures).", comma(nrow(d)))) +
  theme_intent()
ggsave(file.path(INTENT_OUT, "fig_intent_means.png"), p_means, width = 13, height = 8.5, dpi = 300, bg = "white")

cat("\n== Group means ==\n")
print(means %>% select(arm_s, n, ends_with("_mean")) %>% mutate(across(where(is.double), ~ round(.x, 3))),
      width = 250)
cat("\nOutputs written to", INTENT_OUT, "\n")
