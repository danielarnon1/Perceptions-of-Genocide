# =============================================================================
# Education / income × news attention, and their joint relationship with the
# genocide designation (binary). Both surveys. Outputs: Figures/crosstabs/ses_news_*.
# Local only.
# =============================================================================

ROOT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions)"
invisible(capture.output(source(file.path(ROOT, "crosstabs.R"))))   # dat, SURVEY_COL, OUT, theme helpers
suppressMessages({ library(estimatr); library(broom) })

# Collapsed groups so joint cells have workable n
x <- dat %>%
  mutate(
    educ3 = factor(case_when(as.integer(educ) <= 2 ~ "High school or less",
                             as.integer(educ) <= 4 ~ "Some college / associate",
                             as.integer(educ) == 5 ~ "Bachelor's degree",
                             as.integer(educ) >= 6 ~ "Postgraduate"),
                   c("High school or less", "Some college / associate", "Bachelor's degree", "Postgraduate")),
    inc4 = factor(case_when(as.integer(income) <= 2 ~ "Under $50k", as.integer(income) <= 4 ~ "$50–100k",
                            as.integer(income) == 5 ~ "$100–150k", as.integer(income) >= 6 ~ "$150k+"),
                  c("Under $50k", "$50–100k", "$100–150k", "$150k+")),
    news3 = factor(case_when(as.integer(news) <= 2 ~ "Not closely", as.integer(news) == 3 ~ "Somewhat closely",
                             as.integer(news) >= 4 ~ "Very / extremely closely"),
                   c("Not closely", "Somewhat closely", "Very / extremely closely")),
    war3 = factor(case_when(as.integer(warnews) <= 2 ~ "Not closely", as.integer(warnews) == 3 ~ "Somewhat closely",
                            as.integer(warnews) >= 4 ~ "Very / extremely closely"),
                  c("Not closely", "Somewhat closely", "Very / extremely closely")),
    news_close = as.integer(as.integer(news) >= 4), war_close = as.integer(as.integer(warnews) >= 4),
    sv_arm = paste(survey, arm)
  )

ses <- c(educ3 = "Education", inc4 = "Individual income")
nws <- c(news3 = "Follows international news", war3 = "Follows news on wars and violence against civilians")

# -----------------------------------------------------------------------------
# 1. Composition: who follows the news closely?
# -----------------------------------------------------------------------------
comp <- map_dfr(names(ses), function(s) x %>% filter(!is.na(.data[[s]])) %>%
  group_by(survey, level = .data[[s]]) %>%
  summarise(n = n(), across(c(news_close, war_close), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(ses = ses[[s]])) %>%
  pivot_longer(c(news_close, war_close), names_to = "item", values_to = "share") %>%
  mutate(item = recode(item, news_close = "Follows international news very/extremely closely",
                       war_close = "Follows war/conflict news very/extremely closely"),
         se = sqrt(share * (1 - share) / n))
write_csv(comp, file.path(OUT, "ses_news_composition.csv"))

p_comp <- ggplot(comp, aes(share, fct_rev(level), color = survey)) +
  geom_errorbarh(aes(xmin = share - 1.96 * se, xmax = share + 1.96 * se), height = 0, position = position_dodge(width = .55)) +
  geom_point(size = 2.4, position = position_dodge(width = .55)) +
  geom_text(aes(x = share + 1.96 * se, label = sprintf("%.0f%%", 100 * share)), hjust = -0.3, size = 2.7, show.legend = FALSE,
            position = position_dodge(width = .55)) +
  facet_grid(ses ~ item, scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = SURVEY_COL, name = NULL) +
  scale_x_continuous(labels = percent_format(1), limits = c(0, .8)) +
  labs(title = "Who follows the news? Close news attention by education and income",
       subtitle = "Share answering “very closely” or “extremely closely” (asked before the vignette), with 95% CIs.",
       x = NULL, y = NULL) +
  theme_x() + theme(legend.position = "top", strip.placement = "outside",
                    strip.text = element_text(face = "bold", color = INK, size = 9.5),
                    strip.text.y.left = element_text(angle = 0))
ggsave(file.path(OUT, "ses_news_composition.png"), p_comp, width = 12, height = 6.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 2. Joint cross tabs: genocide rate by SES group × news attention (pooled surveys)
# -----------------------------------------------------------------------------
joint <- map_dfr(names(ses), function(s) map_dfr(names(nws), function(nv) {
  x %>% filter(!is.na(.data[[s]]), !is.na(.data[[nv]]), !is.na(genocide)) %>%
    group_by(ses_level = .data[[s]], news_level = .data[[nv]]) %>%
    summarise(n = n(), p = mean(genocide), .groups = "drop") %>%
    mutate(ses = ses[[s]], news = nws[[nv]], se = sqrt(p * (1 - p) / n))
}))
write_csv(joint, file.path(OUT, "ses_news_joint_pooled.csv"))
joint_sv <- map_dfr(names(ses), function(s) map_dfr(names(nws), function(nv) {
  x %>% filter(!is.na(.data[[s]]), !is.na(.data[[nv]]), !is.na(genocide)) %>%
    group_by(survey, ses_level = .data[[s]], news_level = .data[[nv]]) %>%
    summarise(n = n(), p = mean(genocide), .groups = "drop") %>% mutate(ses = ses[[s]], news = nws[[nv]])
}))
write_csv(joint_sv, file.path(OUT, "ses_news_joint_by_survey.csv"))

overall <- mean(x$genocide, na.rm = TRUE)
heat <- function(s_lbl, n_lbl) {
  df <- joint %>% filter(ses == s_lbl, news == n_lbl)
  ggplot(df, aes(news_level, fct_rev(ses_level), fill = p - overall)) +
    geom_tile(color = "white", linewidth = .8) +
    geom_text(aes(label = sprintf("%.0f%%\nn=%d", 100 * p, n)), size = 3, color = INK, lineheight = .9) +
    scale_fill_gradient2(low = "#5b7fb0", mid = "#f4f6fa", high = "#c05020", midpoint = 0,
                         limits = c(-.2, .2), oob = scales::squish,
                         labels = function(v) sprintf("%+.0f pts", 100 * v), name = "vs. overall rate") +
    scale_x_discrete(position = "top", labels = function(l) str_wrap(l, 14)) +
    labs(title = sprintf("%s × %s", s_lbl, str_to_lower(str_remove(n_lbl, "^Follows "))), x = NULL, y = NULL) +
    theme_x(9.5) + theme(panel.grid = element_blank(), axis.text.x = element_text(color = INK), legend.position = "bottom",
                         legend.key.width = unit(1.2, "cm"), plot.title = element_text(size = 10.5))
}
p_heat <- (heat("Education", nws[["war3"]]) | heat("Individual income", nws[["war3"]])) /
          (heat("Education", nws[["news3"]]) | heat("Individual income", nws[["news3"]])) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p_heat <- p_heat + plot_annotation(
  title = "Genocide designation by education/income × news attention",
  subtitle = sprintf("Share saying “genocide”, both surveys pooled (N = %s), all treatment arms. Color = difference from the overall rate (%.1f%%). Read across a row for the news gradient within an education/income group; down a column for the education/income gradient at a given level of news attention.",
                     comma(sum(!is.na(x$genocide))), 100 * overall) %>% str_wrap(150),
  theme = theme(plot.title = element_text(face = "bold", size = 14, color = INK),
                plot.subtitle = element_text(color = MUTED, size = 9)))
ggsave(file.path(OUT, "ses_news_heatmaps.png"), p_heat, width = 12, height = 9.5, dpi = 300, bg = "white")


# -----------------------------------------------------------------------------
# 3. Regression: do they hold up jointly? (LPM, HC2, survey × arm fixed effects)
# -----------------------------------------------------------------------------
reg <- x %>% filter(!is.na(genocide), !is.na(educ3), !is.na(inc4), !is.na(war3), !is.na(news3))
m_each <- list(
  "Education only"      = lm_robust(genocide ~ educ3 + sv_arm, data = reg, se_type = "HC2"),
  "Income only"         = lm_robust(genocide ~ inc4 + sv_arm, data = reg, se_type = "HC2"),
  "War news only"       = lm_robust(genocide ~ war3 + sv_arm, data = reg, se_type = "HC2"),
  "All three"           = lm_robust(genocide ~ educ3 + inc4 + war3 + sv_arm, data = reg, se_type = "HC2"),
  "All three + demogr." = lm_robust(genocide ~ educ3 + inc4 + war3 + gender + age + party + sv_arm,
                                    data = filter(reg, !is.na(gender), !is.na(age), !is.na(party)), se_type = "HC2")
)
coefs <- imap_dfr(m_each, ~ tidy(.x) %>% filter(str_detect(term, "^(educ3|inc4|war3)")) %>%
  transmute(model = .y, term, estimate, conf.low, conf.high, p.value, n = nobs(.x)))
write_csv(coefs, file.path(OUT, "ses_news_regression.csv"))

joint_wald <- function(fit, pattern) {
  k <- which(str_detect(names(coef(fit)), pattern) & !is.na(coef(fit)))
  b <- coef(fit)[k]; V <- vcov(fit)[k, k, drop = FALSE]
  pchisq(as.numeric(t(b) %*% solve(V, b)), length(k), lower.tail = FALSE)
}
int_e <- lm_robust(genocide ~ educ3 * war3 + sv_arm, data = reg, se_type = "HC2")
int_i <- lm_robust(genocide ~ inc4 * war3 + sv_arm, data = reg, se_type = "HC2")

cat("\n== Composition: share following news very/extremely closely ==\n")
comp %>% mutate(cell = sprintf("%.0f%%", 100 * share), s = ifelse(str_detect(survey, "Intent"), "E1", "E2"),
                item = ifelse(str_detect(item, "war"), "war", "intl")) %>%
  select(ses, level, item, s, cell) %>% pivot_wider(names_from = c(item, s), values_from = cell) %>% print(n = Inf)
cat("\n== Pooled joint cross tab: genocide % (n) — SES × war-news attention ==\n")
joint %>% filter(news == nws[["war3"]]) %>% mutate(cell = sprintf("%.0f%% (%d)", 100 * p, n)) %>%
  select(ses, ses_level, news_level, cell) %>% pivot_wider(names_from = news_level, values_from = cell) %>% print(n = Inf)
cat("\n== Pooled joint cross tab: genocide % (n) — SES × international-news attention ==\n")
joint %>% filter(news == nws[["news3"]]) %>% mutate(cell = sprintf("%.0f%% (%d)", 100 * p, n)) %>%
  select(ses, ses_level, news_level, cell) %>% pivot_wider(names_from = news_level, values_from = cell) %>% print(n = Inf)
cat("\n== LPM coefficients (ref: HS or less / under $50k / war news not closely); survey×arm FE ==\n")
coefs %>% mutate(est = sprintf("%+.3f [%+.3f, %+.3f]%s", estimate, conf.low, conf.high,
                               case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", p.value < .05 ~ "*", TRUE ~ ""))) %>%
  select(model, term, est) %>% pivot_wider(names_from = model, values_from = est) %>% print(n = Inf, width = 250)
cat(sprintf("\nJoint tests in 'All three' model: education p = %.4f | income p = %.4f | war news p = %.4f\n",
            joint_wald(m_each[["All three"]], "^educ3"), joint_wald(m_each[["All three"]], "^inc4"),
            joint_wald(m_each[["All three"]], "^war3")))
cat(sprintf("Interaction tests: education × war news p = %.3f | income × war news p = %.3f\n",
            joint_wald(int_e, "^educ3.*:war3"), joint_wald(int_i, "^inc4.*:war3")))

# -----------------------------------------------------------------------------
# 4. Arrow chart: how much close war-news attention raises "genocide" in each group
# -----------------------------------------------------------------------------
p_int <- c("Education" = joint_wald(int_e, "^educ3.*:war3"), "Individual income" = joint_wald(int_i, "^inc4.*:war3"))
arr <- joint %>% filter(news == nws[["war3"]], news_level != "Somewhat closely") %>%
  mutate(end = if_else(news_level == "Not closely", "lo", "hi")) %>%
  select(ses, ses_level, end, p, n) %>%
  pivot_wider(names_from = end, values_from = c(p, n)) %>%
  mutate(gain = p_hi - p_lo,
         panel = factor(sprintf("%s   (interaction p = %.3f)", ses, p_int[ses]),
                        sprintf("%s   (interaction p = %.3f)", names(p_int), p_int)),
         ses_level = fct_rev(ses_level))
XMAX <- .80
p_arrow <- ggplot(arr, aes(y = ses_level)) +
  geom_vline(xintercept = overall, linetype = "dotted", color = MUTED) +
  annotate("text", x = overall, y = 0.5, label = sprintf("overall %.0f%%", 100 * overall), hjust = -0.08,
           size = 2.8, color = MUTED) +
  geom_segment(aes(x = p_lo, xend = p_hi, yend = ses_level), color = "#c05020", linewidth = 1.1,
               arrow = arrow(length = unit(.14, "in"), type = "closed")) +
  geom_point(aes(x = p_lo), shape = 21, size = 3.4, fill = "white", color = "#5b7fb0", stroke = 1.3) +
  geom_text(aes(x = p_lo, label = sprintf("%.0f%%", 100 * p_lo)), nudge_y = .3, size = 3, color = "#5b7fb0") +
  geom_text(aes(x = p_hi, label = sprintf("%.0f%%", 100 * p_hi)), nudge_y = .3, size = 3, color = "#c05020") +
  geom_text(aes(x = XMAX, label = sprintf("%+.0f pts", 100 * gain)), hjust = 1, size = 3.4,
            fontface = "bold", color = INK) +
  facet_wrap(~ panel, ncol = 2, scales = "free_y") +
  scale_x_continuous(labels = percent_format(1), limits = c(.2, XMAX), breaks = seq(.2, .7, .1)) +
  scale_y_discrete(expand = expansion(add = c(.5, .8))) +
  coord_cartesian(clip = "off") +
  labs(title = "Following war news raises “genocide” in every group, but education and income gaps remain",
       subtitle = "Share saying “genocide” among those who follow news on wars and violence against civilians not closely (○) vs. very or extremely closely (●).\nThe arrow is the gain from close attention (right column). Interaction p tests whether that gain differs across the groups in the panel. Both surveys pooled, all treatment arms.",
       x = "Share saying “genocide”", y = NULL,
       caption = str_wrap("Interaction p: joint test of education (or income) × war-news attention in a linear probability model with survey × treatment-arm fixed effects (HC2). The middle category (“somewhat closely”) is omitted for clarity; full tables in ses_news_joint_pooled.csv.", 190)) +
  theme_x(10.5) +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 10.5, color = INK, lineheight = 1.1),
        panel.grid.major.x = element_line(color = GRID), panel.spacing = unit(2, "lines"),
        plot.caption = element_text(color = MUTED, size = 8, hjust = 0), plot.subtitle = element_text(lineheight = 1.15))
ggsave(file.path(OUT, "ses_news_gradients.png"), p_arrow, width = 12, height = 5.4, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 5. Per-survey interaction tests (for the per-survey dashboard view)
# -----------------------------------------------------------------------------
int_sv <- map_dfr(unique(as.character(reg$survey)), function(s) {
  r <- filter(reg, survey == s)
  tibble(survey = s,
         p_educ = joint_wald(lm_robust(genocide ~ educ3 * war3 + sv_arm, data = r, se_type = "HC2"), "^educ3.*:war3"),
         p_inc  = joint_wald(lm_robust(genocide ~ inc4 * war3 + sv_arm, data = r, se_type = "HC2"), "^inc4.*:war3"))
})
write_csv(int_sv, file.path(OUT, "ses_news_interaction_by_survey.csv"))
cat("\n== Per-survey interaction tests ==\n"); print(int_sv)
cat("\n== Per-survey arrows (war news: not closely -> very/extremely) ==\n")
joint_sv %>% filter(news == nws[["war3"]], news_level != "Somewhat closely") %>%
  mutate(cell = sprintf("%.0f%% (%d)", 100 * p, n), s = ifelse(str_detect(survey, "Intent"), "E1", "E2")) %>%
  select(s, ses_level, news_level, cell) %>% pivot_wider(names_from = c(s, news_level), values_from = cell) %>% print(n = Inf, width = 200)
