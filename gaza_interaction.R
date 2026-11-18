# =============================================================================
# Treatment × Gaza-mention interaction on genocide classification, both surveys
# gaza = 1 if the respondent was thinking of a real-world event and named
# Israel / Gaza / Hamas / Palestine (broadly construed, incl. misspellings);
# 0 for everyone else (other event, no event, not sure).
# Model: genocide_binary ~ treatment * gaza (OLS, HC2). Intent adds failed_recall.
# =============================================================================

ROOT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions)"
source(file.path(ROOT, "analysis.R"))      # Exp 2 (tactic): d, cond
source(file.path(ROOT, "intent_prep.R"))   # Exp 1 (intent): intent_all, arm
suppressMessages({ library(estimatr); library(broom) })

GAZA_RX <- regex(paste(
  "israel", "isreal", "isral", "pal[aei]?s?t[aei]+n", "gaza", "hamas", "\\bidf\\b",
  "west bank", "netanyahu", "zionis", "oct(ober|\\.)? ?7", "7(th)? (of )?oct",
  sep = "|"), ignore_case = TRUE)

code_gaza <- function(df) {
  df %>% mutate(
    real_world_yes = as.integer(as.numeric(real_world) == 1),
    gaza = as.integer(real_world_yes == 1 & str_detect(coalesce(real_world_describe, ""), GAZA_RX)),
    gaza_f = factor(gaza, 0:1, c("Did not think of Israel/Gaza", "Thought of Israel/Gaza"))
  )
}

run_gaza <- function(df, tr, extra = NULL, survey) {
  df <- df %>% filter(!is.na(.data[[tr]]), !is.na(genocide_binary))
  rhs <- paste(c(paste0(tr, " * gaza_f"), extra), collapse = " + ")
  fit <- lm_robust(as.formula(paste("genocide_binary ~", rhs)), data = df, se_type = "HC2")

  grid <- expand_grid(!!tr := factor(levels(df[[tr]]), levels(df[[tr]])), gaza_f = factor(levels(df$gaza_f), levels(df$gaza_f)))
  for (v in extra) grid[[v]] <- mean(df[[v]], na.rm = TRUE)
  pr <- predict(fit, newdata = grid, interval = "confidence", se.fit = TRUE)
  preds <- grid %>% mutate(fit = pr$fit[, "fit"], lwr = pr$fit[, "lwr"], upr = pr$fit[, "upr"]) %>%
    rename(condition = all_of(tr)) %>%
    left_join(df %>% count(condition = .data[[tr]], gaza_f, name = "n"), by = c("condition", "gaza_f")) %>%
    mutate(survey = survey, .before = 1)

  # Treatment effect within each Gaza group, and the difference (interaction term)
  simple <- map_dfr(levels(df$gaza_f), function(g) {
    sub <- filter(df, gaza_f == g)
    f <- lm_robust(as.formula(paste("genocide_binary ~", paste(c(tr, extra), collapse = " + "))), data = sub, se_type = "HC2")
    tidy(f) %>% filter(str_starts(term, tr)) %>% mutate(condition = str_remove(term, paste0("^", tr)), group = g)
  })
  inter <- tidy(fit) %>% filter(str_detect(term, ":")) %>%
    mutate(condition = str_remove(str_remove(term, paste0("^", tr)), ":gaza_f.*$"), group = "Difference (interaction)")
  main_gaza <- tidy(fit) %>% filter(term == "gaza_fThought of Israel/Gaza")

  effects <- bind_rows(simple, inter) %>%
    select(group, condition, estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(survey = survey, .before = 1)

  summ <- tibble(survey = survey, n = nrow(df), n_gaza = sum(df$gaza), share_gaza = mean(df$gaza),
                 n_real_world = sum(df$real_world_yes, na.rm = TRUE),
                 share_gaza_of_real_world = sum(df$gaza) / sum(df$real_world_yes, na.rm = TRUE),
                 gaza_main_est = main_gaza$estimate, gaza_main_p = main_gaza$p.value,
                 joint_interaction_p = {
                   fr <- lm_robust(as.formula(paste("genocide_binary ~", paste(c(tr, "gaza_f", extra), collapse = " + "))),
                                   data = df, se_type = "HC2")
                   k <- which(str_detect(names(coef(fit)), ":"))
                   b <- coef(fit)[k]; V <- vcov(fit)[k, k]
                   pchisq(as.numeric(t(b) %*% solve(V) %*% b), length(k), lower.tail = FALSE)
                 })
  list(preds = preds, effects = effects, summary = summ)
}

tactic <- run_gaza(code_gaza(d), "cond", survey = "tactic")
intentg <- run_gaza(code_gaza(intent_all), "arm", extra = "failed_recall", survey = "intent")

OUT <- file.path(ROOT, "Figures")
write_csv(bind_rows(tactic$preds, intentg$preds), file.path(OUT, "gaza_interaction_preds.csv"))
write_csv(bind_rows(tactic$effects, intentg$effects), file.path(OUT, "gaza_interaction_effects.csv"))
write_csv(bind_rows(tactic$summary, intentg$summary), file.path(OUT, "gaza_interaction_summary.csv"))

cat("\n== Gaza coding summary ==\n"); print(bind_rows(tactic$summary, intentg$summary), width = 200)
cat("\n== Predicted Pr(genocide) ==\n")
print(bind_rows(tactic$preds, intentg$preds) %>% mutate(across(c(fit, lwr, upr), ~ round(.x, 3))), n = Inf, width = 200)
cat("\n== Treatment effects by Gaza group and interaction ==\n")
print(bind_rows(tactic$effects, intentg$effects) %>% as_tibble() %>% mutate(across(where(is.double), ~ round(.x, 3))), n = Inf, width = 200)
