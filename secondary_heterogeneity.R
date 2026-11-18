# =============================================================================
# Secondary (post-treatment attitude) DVs and treatment-effect heterogeneity,
# Experiment 1 (Intent) and Experiment 2 (Tactic × Severity).
#   A. Harmonize both surveys (item order and code direction differ)
#   B. Treatment effects on the "Additional post-treatment questions"
#   C. Heterogeneity: DV ~ treatment × moderator, joint robust Wald test of the
#      interaction terms, for every DV × moderator; BH-adjusted within survey
#   D. Subgroup treatment effects on genocide classification
# Outputs: Figures/secondary/. Local only (not wired into the dashboard).
# =============================================================================

ROOT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions)"
invisible(capture.output(source(file.path(ROOT, "analysis.R"))))   # Exp 2: d
source(file.path(ROOT, "intent_prep.R"))                             # Exp 1: intent_all
suppressMessages({ library(estimatr); library(broom); library(scales); library(patchwork) })
OUT <- file.path(ROOT, "Figures", "secondary"); dir.create(OUT, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# A. Harmonized moderators and secondary DVs
# -----------------------------------------------------------------------------
# Item codes that differ: Exp 2 news items run 1 = extremely closely → 5 = not at all
# (Exp 1 is the reverse); "prefer not" is 8/9/4 in Exp 2 and 99 in Exp 1.
harmonize <- function(df, survey) {
  e2 <- survey == "tactic"
  news    <- if (e2) 6 - as.numeric(df$pre_news)     else as.numeric(df$pre_news)
  warnews <- if (e2) 6 - as.numeric(df$pre_news_war) else as.numeric(df$pre_news_war)
  inc     <- as.numeric(df$income); inc[inc %in% c(8, 99)] <- NA
  rel     <- as.numeric(df$religion); rel[rel %in% c(9, 99)] <- NA
  pty     <- as.numeric(df$party)
  df %>% mutate(
    m_news     = factor(if_else(news >= 4, "Follows closely", "Less closely"), c("Less closely", "Follows closely")),
    m_warnews  = factor(if_else(warnews >= 4, "Follows closely", "Less closely"), c("Less closely", "Follows closely")),
    m_facebook = factor(if_else(soc_med_1 == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
    m_x        = factor(if_else(soc_med_2 == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
    m_instagram= factor(if_else(soc_med_3 == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
    m_tiktok   = factor(if_else(soc_med_4 == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
    m_youtube  = factor(if_else(soc_med_5 == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
    m_age      = cut(as.numeric(age), c(0, 2, 4, 8), labels = c("18–39", "40–59", "60+")),
    m_educ     = factor(if_else(as.numeric(educ) >= 5, "4-year degree+", "No 4-year degree"), c("No 4-year degree", "4-year degree+")),
    m_gender   = factor(case_when(gender == 1 ~ "Male", gender == 2 ~ "Female"), c("Male", "Female")),
    m_income   = cut(inc, c(0, 2, 4, 7), labels = c("Under $50k", "$50k–$100k", "$100k+")),
    m_religion = factor(case_when(rel == 1 ~ "Protestant/Christian", rel == 2 ~ "Catholic", rel == 8 ~ "None",
                                  !is.na(rel) ~ "Other religion"),
                        c("Protestant/Christian", "Catholic", "Other religion", "None")),
    m_religiosity = factor(if_else(as.numeric(religiosity) >= 4, "Monthly or more", "Less than monthly"),
                           c("Less than monthly", "Monthly or more")),
    m_party    = factor(case_when(pty == 1 ~ "Republican", pty == 2 ~ "Democrat", pty == 3 ~ "Independent",
                                  !is.na(pty) ~ "Something else"),
                        c("Democrat", "Republican", "Independent", "Something else")),
    m_ideology = cut(as.numeric(ideology), c(0, 2, 3, 5), labels = c("Liberal", "Moderate", "Conservative"))
  )
}

MODS <- tribble(
  ~mod,            ~mod_label,
  "m_news",        "Follows international news",
  "m_warnews",     "Follows war/conflict news",
  "m_facebook",    "Facebook user",
  "m_x",           "Twitter/X user",
  "m_instagram",   "Instagram user",
  "m_tiktok",      "TikTok user",
  "m_youtube",     "YouTube user",
  "m_age",         "Age",
  "m_educ",        "Education",
  "m_gender",      "Gender",
  "m_income",      "Income",
  "m_religion",    "Religion",
  "m_religiosity", "Religious attendance",
  "m_party",       "Party ID",
  "m_ideology",    "Ideology"
)

tactic_df <- d %>%
  transmute(arm = droplevels(cond), genocide_binary, genocide_fit, support_action,
            reserve = as.numeric(term_reserve), commit = as.numeric(action_1), moral = as.numeric(action_2),
            symbolic = as.numeric(action_3), avoid = as.numeric(action_4),
            ideology_num, party_dem, party_rep, educ_num, news_num, age_num, female,
            pre_news, pre_news_war, soc_med_1, soc_med_2, soc_med_3, soc_med_4, soc_med_5,
            age, educ, gender, income, religion, religiosity, party, ideology) %>%
  harmonize("tactic")

intent_df <- intent_all %>%
  transmute(arm, failed_recall, genocide_binary, genocide_fit, support_action,
            reserve = action_1, commit = action_2, moral = action_3, symbolic = action_4, avoid = action_5,
            therm_victims = feeling_actors_1, therm_perps = feeling_actors_2,
            anger = feelings_broad_1, fear = feelings_broad_2, disgust = feelings_broad_3,
            sadness = feelings_broad_4, contempt = feelings_broad_5,
            ideology_num, party_dem, party_rep, educ_num, news_num, age_num = age_cat, female,
            pre_news, pre_news_war, soc_med_1, soc_med_2, soc_med_3, soc_med_4, soc_med_5,
            age, educ, gender, income, religion, religiosity, party, ideology) %>%
  harmonize("intent")

DV_LABELS <- c(
  genocide_binary = "Genocide classification (0/1)", genocide_fit = "Genocide fit (1–5)",
  support_action = "Obligation to respond (0/1)",
  reserve  = "“Genocide” should be reserved for the worst cases",
  commit   = "Calling it genocide should commit countries to act",
  moral    = "Countries have a moral obligation to intervene",
  symbolic = "“Genocide” is used symbolically, not to prompt action",
  avoid    = "Avoid the term unless prepared to act",
  therm_victims = "Thermometer: victims (0–100)", therm_perps = "Thermometer: perpetrators (0–100)",
  anger = "Anger", fear = "Fear", disgust = "Disgust", sadness = "Sadness", contempt = "Contempt")
SECONDARY <- list(tactic = c("reserve", "commit", "moral", "symbolic", "avoid"),
                  intent = c("reserve", "commit", "moral", "symbolic", "avoid",
                             "therm_victims", "therm_perps", "anger", "fear", "disgust", "sadness", "contempt"))
MAIN <- c("genocide_binary", "genocide_fit", "support_action")
SURVEYS <- list(tactic = list(df = tactic_df, extra = NULL, name = "Exp 2: Tactic × Severity"),
                intent = list(df = intent_df, extra = "failed_recall", name = "Exp 1: Intent"))
DEMOG <- c("ideology_num", "party_dem", "party_rep", "educ_num", "news_num", "age_num", "female")

short_arm <- function(x) x %>%
  str_replace("No tactic \\(control\\)|No intent \\(control\\)", "Control") %>%
  str_replace("Killing — hundreds", "Kill (100s)") %>% str_replace("Killing — tens of thousands", "Kill (10Ks)") %>%
  str_replace("Sexual violence — hundreds", "Sex viol (100s)") %>% str_replace("Sexual violence — tens of thousands", "Sex viol (10Ks)") %>%
  str_replace("Infrastructure — hundreds", "Infra (100s)") %>% str_replace("Infrastructure — tens of thousands", "Infra (10Ks)") %>%
  str_replace("Denies intent \\+ security justification", "Denial + justify") %>%
  str_replace("Denies intent", "Denial") %>% str_replace("States intent to destroy", "Stated intent")

# -----------------------------------------------------------------------------
# B. Treatment effects on secondary DVs
# -----------------------------------------------------------------------------
sec_effects <- imap_dfr(SURVEYS, function(s, sv) {
  map_dfr(SECONDARY[[sv]], function(dv) {
    cm <- mean(s$df[[dv]][s$df$arm == levels(s$df$arm)[1]], na.rm = TRUE)
    specs <- list("No controls" = s$extra, "With controls" = c(s$extra, DEMOG))
    imap_dfr(specs, function(rhs, model) {
      f <- as.formula(paste(dv, "~", paste(c("arm", rhs), collapse = " + ")))
      fit <- lm_robust(f, data = s$df, se_type = "HC2")
      tidy(fit) %>% filter(str_starts(term, "arm")) %>%
        transmute(survey = sv, dv, model, condition = str_remove(term, "^arm"),
                  estimate, std.error, p.value, conf.low, conf.high, n = nobs(fit))
    }) %>% mutate(control_mean = cm)
  })
})
write_csv(sec_effects, file.path(OUT, "secondary_treatment_effects.csv"))

sec_means <- imap_dfr(SURVEYS, function(s, sv) {
  s$df %>% group_by(arm) %>%
    summarise(across(all_of(SECONDARY[[sv]]), ~ mean(.x, na.rm = TRUE)), n = n(), .groups = "drop") %>%
    mutate(survey = sv, .before = 1)
})
write_csv(sec_means, file.path(OUT, "secondary_means.csv"))

INK <- "#1b2433"; MUTED <- "#6a7890"; GRID <- "#e3e8f0"
theme_s <- function(base = 10) theme_minimal(base_size = base) +
  theme(text = element_text(color = INK), plot.title = element_text(face = "bold", size = base + 3),
        plot.subtitle = element_text(color = MUTED, size = base), plot.caption = element_text(color = MUTED, size = base - 2, hjust = 0),
        strip.text = element_text(face = "bold", hjust = 0, size = base - 0.5), axis.text = element_text(color = MUTED),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = GRID),
        panel.spacing = unit(1.1, "lines"), legend.position = "top")

plot_sec <- function(sv, ncol) {
  df <- sec_effects %>% filter(survey == sv) %>%
    mutate(condition = short_arm(condition),
           condition = factor(condition, rev(unique(short_arm(levels(SURVEYS[[sv]]$df$arm)[-1])))),
           facet = factor(sprintf("%s\n(control mean %.2f)", str_wrap(DV_LABELS[dv], 34), control_mean),
                          unique(sprintf("%s\n(control mean %.2f)", str_wrap(DV_LABELS[dv], 34), control_mean))),
           sig = factor(p.value < .05, c(TRUE, FALSE), c("p < .05", "p ≥ .05")))
  ggplot(df, aes(estimate, condition, color = model, shape = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = MUTED, linewidth = .35) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = .2, position = position_dodge(width = .55)) +
    geom_point(size = 2.1, fill = "white", position = position_dodge(width = .55)) +
    scale_color_manual(values = c("No controls" = "#2e7ab4", "With controls" = "#c05020"), name = NULL) +
    scale_shape_manual(values = c("p < .05" = 16, "p ≥ .05" = 21), name = NULL, drop = FALSE) +
    facet_wrap(~ facet, scales = "free_x", ncol = ncol) +
    labs(title = sprintf("%s: treatment effects on post-treatment attitudes", SURVEYS[[sv]]$name),
         subtitle = "OLS with 95% CIs (HC2). Agree/disagree items on 1–5 scales; thermometers 0–100; emotions 1–5. Reference = control.",
         x = "Effect vs. control (scale points)", y = NULL,
         caption = if (sv == "intent") "Thermometer and emotion items ask about “the event you just described” (the real-world event respondents recalled), not the vignette. Both models adjust for vignette-recall failure." else NULL) +
    theme_s()
}
ggsave(file.path(OUT, "fig_secondary_effects_intent.png"), plot_sec("intent", 4), width = 15, height = 10.5, dpi = 300, bg = "white")
ggsave(file.path(OUT, "fig_secondary_effects_tactic.png"), plot_sec("tactic", 3), width = 13, height = 8.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# C. Heterogeneity: joint test of treatment × moderator for every DV × moderator
# -----------------------------------------------------------------------------
wald_inter <- function(fit) {
  k <- which(str_detect(names(coef(fit)), "^arm.*:m_|^m_.*:arm"))
  k <- k[!is.na(coef(fit)[k])]
  b <- coef(fit)[k]; V <- vcov(fit)[k, k, drop = FALSE]
  stat <- as.numeric(t(b) %*% solve(V, b))
  c(stat = stat, df = length(k), p = pchisq(stat, length(k), lower.tail = FALSE))
}

het <- imap_dfr(SURVEYS, function(s, sv) {
  dvs <- c(MAIN, SECONDARY[[sv]])
  map_dfr(dvs, function(dv) map_dfr(MODS$mod, function(m) {
    sub <- s$df %>% filter(!is.na(.data[[m]]), !is.na(.data[[dv]])) %>% mutate(across(all_of(m), droplevels))
    f <- as.formula(paste(dv, "~", paste(c(paste0("arm * ", m), s$extra), collapse = " + ")))
    fit <- lm_robust(f, data = sub, se_type = "HC2")
    w <- wald_inter(fit)
    tibble(survey = sv, dv, mod = m, chi2 = w[["stat"]], df = w[["df"]], p = w[["p"]], n = nobs(fit))
  }))
}) %>%
  group_by(survey) %>% mutate(q_bh = p.adjust(p, "BH")) %>% ungroup() %>%
  left_join(MODS, by = "mod")
write_csv(het, file.path(OUT, "heterogeneity_tests.csv"))

cat("\n== Heterogeneity tests: how many joint interaction tests reject? ==\n")
het %>% group_by(survey) %>%
  summarise(tests = n(), `p < .05` = sum(p < .05), expected_by_chance = round(.05 * n()),
            `p < .01` = sum(p < .01), `BH q < .10` = sum(q_bh < .10), min_q = round(min(q_bh), 3)) %>% print()
cat("\nAll tests with p < .05:\n")
het %>% filter(p < .05) %>% arrange(survey, p) %>%
  transmute(survey, dv, moderator = mod_label, chi2 = round(chi2, 1), df, p = round(p, 4), q_bh = round(q_bh, 3)) %>%
  print(n = Inf)

plot_het <- function(sv) {
  dvs <- c(MAIN, SECONDARY[[sv]])
  df <- het %>% filter(survey == sv) %>%
    mutate(dv_lab = factor(str_wrap(DV_LABELS[dv], 15), str_wrap(DV_LABELS[dvs], 15)),
           mod_label = factor(mod_label, rev(MODS$mod_label)),
           lp = pmin(-log10(p), 4),
           lab = case_when(q_bh < .10 ~ sprintf("%.3f\n(q=%.2f)", p, q_bh), p < .05 ~ sprintf("%.3f", p), TRUE ~ ""))
  ggplot(df, aes(dv_lab, mod_label, fill = lp)) +
    geom_tile(color = "white", linewidth = .6) +
    geom_text(aes(label = lab), size = 2.6, color = INK, lineheight = .85) +
    scale_fill_gradientn(colours = c("#f4f6fa", "#dfe6f1", "#f2c9a8", "#d9783f", "#a8452c"),
                         values = rescale(c(0, -log10(.2), -log10(.05), -log10(.01), 4)), limits = c(0, 4),
                         breaks = -log10(c(.5, .05, .01, .001)), labels = c(".5", ".05", ".01", ".001"),
                         name = "Joint interaction p") +
    scale_x_discrete(position = "top") +
    labs(title = sprintf("%s: is there heterogeneity in treatment effects?", SURVEYS[[sv]]$name),
         subtitle = sprintf("Each cell: joint robust Wald test that all treatment × moderator terms are zero (%d tests). Labeled if p < .05; q = BH-adjusted across all cells.",
                            nrow(df)),
         x = NULL, y = NULL,
         caption = sprintf("Under no heterogeneity, about %d cells would reach p < .05 by chance. Model: DV ~ treatment × moderator%s, HC2 SEs.",
                           round(.05 * nrow(df)), if (sv == "intent") " + recall failure" else "")) +
    theme_s(9) + theme(panel.grid = element_blank(), axis.text.x = element_text(size = 7.5, color = INK, vjust = 0),
                       axis.text.y = element_text(color = INK), legend.position = "bottom",
                       legend.key.width = unit(1.4, "cm"))
}
ggsave(file.path(OUT, "fig_heterogeneity_intent.png"), plot_het("intent"), width = 14, height = 7.5, dpi = 300, bg = "white")
ggsave(file.path(OUT, "fig_heterogeneity_tactic.png"), plot_het("tactic"), width = 10, height = 7.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# D. Subgroup treatment effects on genocide classification
# -----------------------------------------------------------------------------
subgroup <- imap_dfr(SURVEYS, function(s, sv) map_dfr(MODS$mod, function(m) {
  lv <- levels(s$df[[m]])
  map_dfr(lv, function(l) {
    sub <- filter(s$df, .data[[m]] == l)
    fit <- lm_robust(as.formula(paste("genocide_binary ~", paste(c("arm", s$extra), collapse = " + "))), data = sub, se_type = "HC2")
    tidy(fit) %>% filter(str_starts(term, "arm")) %>%
      transmute(survey = sv, mod = m, level = l, condition = str_remove(term, "^arm"),
                estimate, conf.low, conf.high, p.value, n = nobs(fit))
  })
})) %>% left_join(MODS, by = "mod")
write_csv(subgroup, file.path(OUT, "subgroup_effects_genocide_binary.csv"))

plot_sub <- function(sv) {
  df <- subgroup %>% filter(survey == sv) %>%
    left_join(het %>% filter(survey == sv, dv == "genocide_binary") %>% select(mod, p_int = p), by = "mod") %>%
    mutate(condition = factor(short_arm(condition), unique(short_arm(levels(SURVEYS[[sv]]$df$arm)[-1]))),
           row = factor(paste0(mod_label, ": ", level), rev(unique(paste0(mod_label, ": ", level)))),
           grp = factor(mod_label, MODS$mod_label),
           lab = sprintf("%+.0f", 100 * estimate))
  ggplot(df, aes(condition, row, fill = estimate)) +
    geom_tile(color = "white", linewidth = .5) +
    geom_text(aes(label = lab, fontface = ifelse(p.value < .05, "bold", "plain")), size = 2.7, color = INK) +
    scale_fill_gradient2(low = "#5b7fb0", mid = "#f4f6fa", high = "#c05020", midpoint = 0,
                         labels = function(x) paste0(round(100 * x), " pts"), name = "Effect on Pr(genocide)") +
    facet_grid(grp ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = labeller(grp = function(x) {
                 p <- df %>% distinct(grp, p_int)
                 sprintf("p_int = %.2f", p$p_int[match(x, p$grp)])
               })) +
    scale_x_discrete(position = "top") +
    labs(title = sprintf("%s: treatment effect on genocide classification by subgroup", SURVEYS[[sv]]$name),
         subtitle = "Percentage-point effect vs. control within each subgroup (bold = p < .05).
p_int = joint test that the treatment effects differ across that moderator's groups.",
         x = NULL, y = NULL) +
    theme_s(9) + theme(panel.grid = element_blank(), strip.placement = "outside",
                       strip.text.y.left = element_text(angle = 0, size = 7.5, color = MUTED, face = "plain"),
                       axis.text.y = element_text(color = INK), axis.text.x = element_text(color = INK),
                       legend.position = "bottom", legend.key.width = unit(1.4, "cm"))
}
ggsave(file.path(OUT, "fig_subgroups_genocide_intent.png"), plot_sub("intent"), width = 9, height = 12, dpi = 300, bg = "white")
ggsave(file.path(OUT, "fig_subgroups_genocide_tactic.png"), plot_sub("tactic"), width = 11, height = 12, dpi = 300, bg = "white")

cat("\n== Secondary DV effects with p < .05 (no controls) ==\n")
sec_effects %>% as_tibble() %>% filter(model == "No controls", p.value < .05) %>%
  transmute(survey, dv, condition = short_arm(condition), est = round(estimate, 3),
            ci = sprintf("[%.2f, %.2f]", conf.low, conf.high), p = round(p.value, 4), control_mean = round(control_mean, 2)) %>%
  print(n = Inf)
cat("\nCount of secondary-DV effects tested / p < .05 (no controls):\n")
sec_effects %>% filter(model == "No controls") %>% group_by(survey) %>%
  summarise(tests = n(), sig = sum(p.value < .05), expected = round(.05 * n(), 1)) %>% print()
cat("\nOutputs in", OUT, "\n")

# -----------------------------------------------------------------------------
# E. Pooled descriptives of the post-treatment attitude items (for the dashboard)
# -----------------------------------------------------------------------------
LIKERT_ITEMS  <- c("reserve", "commit", "moral", "symbolic", "avoid")
EMOTION_ITEMS <- c("anger", "fear", "disgust", "sadness", "contempt")
THERM_ITEMS   <- c("therm_victims", "therm_perps")

att_desc <- imap_dfr(SURVEYS, function(s, sv) {
  items <- intersect(c(LIKERT_ITEMS, EMOTION_ITEMS), names(s$df))
  map_dfr(items, function(it) {
    x <- s$df[[it]][!is.na(s$df[[it]])]
    tibble(survey = sv, item = it, level = 1:5, n = as.vector(table(factor(x, 1:5))),
           share = n / length(x), mean = mean(x), n_valid = length(x))
  })
})
write_csv(att_desc, file.path(OUT, "attitudes_descriptives.csv"))

therm_bins <- map_dfr(THERM_ITEMS, function(it) {
  x <- intent_df[[it]][!is.na(intent_df[[it]])]
  b <- pmin(floor(x / 10), 9)
  tibble(survey = "intent", item = it, bin = 0:9, n = as.vector(table(factor(b, 0:9))),
         share = n / length(x), mean = mean(x), median = median(x), n_valid = length(x))
})
write_csv(therm_bins, file.path(OUT, "thermometer_bins.csv"))
cat("\n== Attitude item means ==\n")
print(att_desc %>% distinct(survey, item, mean, n_valid) %>% mutate(mean = round(mean, 2)), n = Inf)
print(therm_bins %>% distinct(item, mean, median, n_valid))
