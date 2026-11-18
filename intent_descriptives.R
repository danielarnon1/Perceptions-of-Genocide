# =============================================================================
# Experiment 1 (Intent): randomization balance and pooled descriptives of DVs
# Outputs go to Figures/intent/. Not wired into the dashboard.
# =============================================================================

source(r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\intent_prep.R)")
suppressMessages({
  library(estimatr)
  library(nnet)
  library(patchwork)
  library(gridExtra)
  library(scales)
})

INK <- "#1b2433"; MUTED <- "#6a7890"; GRID <- "#e3e8f0"
ACCENT <- "#2e7ab4"; WARN <- "#c05020"
ARM_COLS <- c("Control" = "#7a8a9a", "Denial" = "#5b7fb0",
              "Denial + justify" = "#b08a3a", "Stated intent" = "#c05020")

theme_intent <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(text = element_text(color = INK),
          plot.title = element_text(face = "bold", size = base + 1),
          plot.subtitle = element_text(color = MUTED, size = base - 1),
          plot.caption = element_text(color = MUTED, size = base - 3, hjust = 0),
          axis.text = element_text(color = MUTED),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = GRID))
}

# -----------------------------------------------------------------------------
# 1. Sample flow and differential exclusion
# -----------------------------------------------------------------------------
flow <- intent_all %>%
  group_by(arm_s) %>%
  summarise(n_raw = n(), failed_recall = sum(!passed_recall),
            pct_failed = mean(!passed_recall), .groups = "drop")
cat("\n== Sample by arm ==\n"); print(flow)
p_alloc_raw <- chisq.test(flow$n_raw)$p.value
p_fail      <- chisq.test(table(intent_all$arm_s, intent_all$passed_recall))$p.value
cat(sprintf("Equal allocation p = %.3f | recall failure differs by arm p = %.3f\n", p_alloc_raw, p_fail))
write_csv(flow, file.path(INTENT_OUT, "intent_sample_flow.csv"))

# -----------------------------------------------------------------------------
# 2. Covariate balance (analytic sample)
# -----------------------------------------------------------------------------
smd <- function(x, g, ref = "Control") {
  r <- x[g == ref]
  sapply(setdiff(levels(g), ref), function(a) {
    t <- x[g == a]
    (mean(t, na.rm = TRUE) - mean(r, na.rm = TRUE)) /
      sqrt((var(t, na.rm = TRUE) + var(r, na.rm = TRUE)) / 2)
  })
}

bal <- pmap_dfr(BAL_VARS, function(var, label, type) {
  x <- intent[[var]]; g <- intent$arm_s
  stats <- tapply(x, g, function(v) sprintf("%.2f (%.2f)", mean(v, na.rm = TRUE), sd(v, na.rm = TRUE)))
  p <- if (type == "bin") suppressWarnings(chisq.test(table(g, x))$p.value) else
    summary(aov(x ~ g))[[1]][["Pr(>F)"]][1]
  s <- smd(x, g)
  tibble(Variable = label, !!!as.list(stats), p = p,
         `Max |SMD|` = max(abs(s)), smd_denial = s[1], smd_justify = s[2], smd_stated = s[3])
})

cat("\n== Covariate balance (mean (SD); p = omnibus ANOVA / chi-square) ==\n")
print(bal %>% select(-starts_with("smd_")) %>% mutate(across(c(p, `Max |SMD|`), ~ round(.x, 3))),
      n = Inf, width = 200)
write_csv(bal, file.path(INTENT_OUT, "intent_balance_table.csv"))

# Joint tests: (a) multinomial logit of arm on all covariates vs intercept-only
covs <- BAL_VARS$var
cc <- intent %>% select(arm_s, all_of(covs)) %>% drop_na()
m1 <- multinom(arm_s ~ ., data = cc, trace = FALSE)
m0 <- multinom(arm_s ~ 1, data = cc, trace = FALSE)
lr <- 2 * (as.numeric(logLik(m1)) - as.numeric(logLik(m0)))
df_lr <- length(coef(m1)) - length(coef(m0))
p_joint <- pchisq(lr, df_lr, lower.tail = FALSE)
cat(sprintf("\nJoint multinomial LR test: chi2(%d) = %.1f, p = %.3f (complete cases N = %d)\n",
            df_lr, lr, p_joint, nrow(cc)))

# (b) each arm vs control: LPM of arm indicator on covariates, robust joint F
pair_tests <- map_dfr(c("Denial", "Denial + justify", "Stated intent"), function(a) {
  sub <- cc %>% filter(arm_s %in% c("Control", a)) %>% mutate(tr = as.integer(arm_s == a))
  fit <- lm_robust(reformulate(covs, "tr"), data = sub, se_type = "HC2")
  f <- summary(fit)$fstatistic
  tibble(comparison = paste(a, "vs Control"), F = f[["value"]], df1 = f[["numdf"]], df2 = f[["dendf"]],
         p = pf(f[["value"]], f[["numdf"]], f[["dendf"]], lower.tail = FALSE), n = nrow(sub))
})
cat("\n== Pairwise joint balance tests (HC2 robust F) ==\n"); print(pair_tests)
write_csv(bind_rows(tibble(comparison = "All arms (multinomial LR)", F = lr, df1 = df_lr, df2 = NA,
                           p = p_joint, n = nrow(cc)), pair_tests),
          file.path(INTENT_OUT, "intent_balance_joint_tests.csv"))

# Love plot
love <- bal %>%
  select(Variable, smd_denial, smd_justify, smd_stated) %>%
  pivot_longer(-Variable, names_to = "arm", values_to = "smd") %>%
  mutate(arm = recode(arm, smd_denial = "Denial", smd_justify = "Denial + justify", smd_stated = "Stated intent"),
         Variable = factor(Variable, levels = rev(BAL_VARS$label)))

p_love <- ggplot(love, aes(smd, Variable, color = arm, shape = arm)) +
  annotate("rect", xmin = -0.1, xmax = 0.1, ymin = -Inf, ymax = Inf, fill = GRID, alpha = .6) +
  geom_vline(xintercept = 0, color = MUTED, linewidth = .3) +
  geom_point(size = 2.4, position = position_dodge(width = .5)) +
  scale_color_manual(values = ARM_COLS[-1], name = NULL) +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  scale_x_continuous(limits = c(-0.25, 0.25)) +
  labs(title = "Covariate balance across intent arms",
       subtitle = sprintf("Standardized mean difference vs. control. Shaded band = |SMD| < 0.1.  Joint multinomial LR test p = %.2f", p_joint),
       x = "Standardized mean difference (treatment − control)", y = NULL,
       caption = sprintf("Full sample N = %s, including vignette-recall failures. Panel-profile items from Verasight.",
                         comma(nrow(intent)))) +
  theme_intent() + theme(legend.position = "top")
ggsave(file.path(INTENT_OUT, "intent_balance_loveplot.png"), p_love, width = 8.5, height = 7, dpi = 300, bg = "white")

# Balance table as an image
tab_img <- bal %>%
  transmute(Variable, Control, Denial, `Denial + justify`, `Stated intent`,
            p = sprintf("%.3f", p), `Max |SMD|` = sprintf("%.3f", `Max |SMD|`))
n_row <- c("N", as.character(as.vector(table(intent$arm_s))), "", "")
tab_img <- rbind(n_row, tab_img)
tg <- tableGrob(tab_img, rows = NULL,
                theme = ttheme_minimal(base_size = 9,
                                       core = list(fg_params = list(hjust = 0, x = 0.04, col = INK)),
                                       colhead = list(fg_params = list(hjust = 0, x = 0.04, fontface = "bold", col = INK))))
png(file.path(INTENT_OUT, "intent_balance_table.png"), width = 9.5, height = 6.6, units = "in", res = 300, bg = "white")
grid::grid.draw(arrangeGrob(tg, top = grid::textGrob("Experiment 1 (Intent): covariate balance — mean (SD)",
                                                    gp = grid::gpar(fontface = "bold", fontsize = 12)),
                            bottom = grid::textGrob("p: omnibus ANOVA (continuous) or chi-square (binary) across the four arms. Max |SMD|: largest standardized difference of any arm vs. control.",
                                                    gp = grid::gpar(fontsize = 8, col = MUTED))))
dev.off()

# -----------------------------------------------------------------------------
# 3. Pooled descriptives of the main DVs (no treatment breakdown)
# -----------------------------------------------------------------------------
wprop <- function(x, w) sum(w * x, na.rm = TRUE) / sum(w[!is.na(x)])
ci <- function(p, n) 1.96 * sqrt(p * (1 - p) / n)
N <- nrow(intent)

dA <- intent %>% count(val = if_else(genocide_binary == 1, "Yes", "No")) %>%
  mutate(p = n / sum(n), w = c(wprop(intent$genocide_binary == 0, intent$weight),
                               wprop(intent$genocide_binary == 1, intent$weight)),
         val = factor(val, c("No", "Yes")))
fit_labs <- c("Not well at all", "Slightly well", "Moderately well", "Very well", "Extremely well")
dB <- intent %>% count(val = genocide_fit) %>%
  mutate(p = n / sum(n), w = sapply(val, function(k) wprop(intent$genocide_fit == k, intent$weight)),
         lab = factor(fit_labs[val], fit_labs))
dC <- intent %>% count(val = takeaction_cat) %>%
  mutate(p = n / sum(n), w = sapply(as.character(val), function(k) wprop(intent$takeaction_cat == k, intent$weight)))
act_map <- c(act_humanitarian = "Humanitarian assistance", act_diplomatic = "Diplomatic pressure",
             act_economic = "Economic sanctions", act_legal = "Legal accountability",
             act_military = "Military intervention", act_none = "None of the above")
dD <- tibble(var = names(act_map), lab = act_map) %>%
  mutate(p = map_dbl(var, ~ mean(intent[[.x]])), w = map_dbl(var, ~ wprop(intent[[.x]], intent$weight)),
         se = ci(p, N), lab = fct_reorder(lab, p))

bar_layer <- function(fillcol) list(
  geom_col(fill = fillcol, width = .7),
  geom_point(aes(x = w), shape = 23, size = 2.2, fill = "white", color = INK, stroke = .6),
  scale_x_continuous(labels = percent_format(1), expand = expansion(mult = c(0, .22))),
  theme_intent(10)
)

pA <- ggplot(dA, aes(p, val)) + bar_layer(ACCENT) +
  geom_text(aes(label = percent(p, .1)), hjust = -0.25, size = 3.2, color = INK) +
  labs(title = "A. Would you describe this as genocide?", x = NULL, y = NULL)
pB <- ggplot(dB, aes(p, lab)) + bar_layer(ACCENT) +
  geom_text(aes(label = percent(p, .1)), hjust = -0.25, size = 3.2, color = INK) +
  labs(title = "B. How well does “genocide” describe it?",
       subtitle = sprintf("Mean %.2f on a 1–5 scale (SD %.2f)", mean(intent$genocide_fit), sd(intent$genocide_fit)),
       x = NULL, y = NULL)
pC <- ggplot(dC, aes(p, fct_rev(val))) + bar_layer(ACCENT) +
  geom_text(aes(label = percent(p, .1)), hjust = -0.25, size = 3.2, color = INK) +
  labs(title = "C. Governments obligated to respond?", x = NULL, y = NULL)
pD <- ggplot(dD, aes(p, lab)) + bar_layer(WARN) +
  geom_errorbarh(aes(xmin = p - se, xmax = p + se), height = .2, color = INK, linewidth = .3) +
  geom_text(aes(x = p + se, label = percent(p, .1)), hjust = -0.25, size = 3.2, color = INK) +
  labs(title = "D. Should governments respond in these ways?",
       subtitle = sprintf("Select all that apply. Median options chosen: %d", median(intent$n_actions)),
       x = "Share of respondents", y = NULL)

p_all <- (pA + pC) / pB / pD + plot_layout(heights = c(1, 1.5, 1.9)) +
  plot_annotation(
    title = "Experiment 1 (Intent): responses on the main outcomes, all arms pooled",
    caption = sprintf("Bars: unweighted shares, full sample N = %s (incl. recall-check failures). White diamonds: survey-weighted shares (Verasight CPS weights). Panel D error bars: 95%% CI.",
                      comma(N)),
    theme = theme(plot.title = element_text(face = "bold", size = 13, color = INK),
                  plot.caption = element_text(color = MUTED, size = 8, hjust = 0)))
ggsave(file.path(INTENT_OUT, "intent_dv_descriptives.png"), p_all, width = 9, height = 10, dpi = 300, bg = "white")

desc <- bind_rows(
  dA %>% transmute(outcome = "Genocide classification", response = as.character(val), n, share = p, share_weighted = w),
  dB %>% transmute(outcome = "Genocide fit (1–5)", response = as.character(lab), n, share = p, share_weighted = w),
  dC %>% transmute(outcome = "Obligation to respond", response = as.character(val), n, share = p, share_weighted = w),
  dD %>% transmute(outcome = "Response types (select all)", response = as.character(lab), n = round(p * N), share = p, share_weighted = w)
)
write_csv(desc, file.path(INTENT_OUT, "intent_dv_descriptives.csv"))
cat("\n== Pooled DV descriptives ==\n"); print(desc %>% mutate(across(starts_with("share"), ~ round(.x, 3))), n = Inf)

# Classification vs. fit: how the two genocide measures line up
cat("\n== Genocide fit by binary classification ==\n")
print(intent %>% group_by(genocide_binary) %>% summarise(n = n(), mean_fit = mean(genocide_fit),
                                                         share_fit_4plus = mean(genocide_fit >= 4)))
cat("\nOutputs written to", INTENT_OUT, "\n")
