# =============================================================================
# Cross tabs: share classifying the vignette as genocide (binary DV) by
# demographic, political, religious and pre-treatment media characteristics.
# Both surveys, pooled across treatment arms (randomization balances arms within
# each group). Outputs: Figures/crosstabs/. Local only.
# =============================================================================

ROOT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions)"
invisible(capture.output(source(file.path(ROOT, "analysis.R"))))   # Exp 2: d
source(file.path(ROOT, "intent_prep.R"))                             # Exp 1: intent_all
suppressMessages({ library(scales); library(patchwork) })
OUT <- file.path(ROOT, "Figures", "crosstabs"); dir.create(OUT, showWarnings = FALSE)

SURVEY_LBL <- c(intent = "Exp 1: Intent", tactic = "Exp 2: Tactic × Severity")
SURVEY_COL <- c("Exp 1: Intent" = "#a8452c", "Exp 2: Tactic × Severity" = "#2e7ab4")

# Harmonize raw codes (Exp 2 news items run 1 = extremely closely; "prefer not"
# codes differ between surveys: 8/9/4 in Exp 2, 99 in Exp 1).
lv <- function(x, levels, labels) factor(x, levels, labels)
code_vars <- function(df, sv) {
  e2 <- sv == "tactic"
  news    <- as.numeric(df$pre_news);     if (e2) news    <- 6 - news
  warnews <- as.numeric(df$pre_news_war); if (e2) warnews <- 6 - warnews
  rel <- as.numeric(df$religion)
  tibble(
    survey = SURVEY_LBL[[sv]],
    genocide = df$genocide_binary,
    age = lv(pmin(as.numeric(df$age), 6), 1:6, c("18–29", "30–39", "40–49", "50–59", "60–69", "70+")),
    gender = lv(as.numeric(df$gender), 1:2, c("Male", "Female")),
    income = lv(as.numeric(df$income), 1:7, c("< $25k", "$25–50k", "$50–75k", "$75–100k", "$100–150k", "$150–200k", "$200k+")),
    educ = lv(as.numeric(df$educ), 1:7, c("Less than high school", "High school", "Some college", "Associate degree",
                                          "Bachelor's degree", "Some postgraduate", "Postgraduate degree")),
    party = lv(as.numeric(df$party), c(2, 3, 1, if (e2) 4 else 99), c("Democrat", "Independent", "Republican", "Something else")),
    ideology = lv(as.numeric(df$ideology), 1:5, c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative")),
    religion = factor(case_when(rel == 1 ~ "Protestant / Christian", rel == 2 ~ "Catholic", rel == 3 ~ "Jewish",
                                rel == 4 ~ "Muslim", rel %in% 5:7 ~ "Other religion", rel == 8 ~ "No religion"),
                      c("Protestant / Christian", "Catholic", "Jewish", "Muslim", "Other religion", "No religion")),
    religiosity = lv(as.numeric(df$religiosity), 1:6, c("Never", "Less than once a year", "A few times a year",
                                                         "Once or twice a month", "Once a week", "More than once a week")),
    news = lv(news, 1:5, c("Not at all closely", "Not too closely", "Somewhat closely", "Very closely", "Extremely closely")),
    warnews = lv(warnews, 1:5, c("Not at all closely", "Not too closely", "Somewhat closely", "Very closely", "Extremely closely")),
    sm_facebook = df$soc_med_1, sm_x = df$soc_med_2, sm_instagram = df$soc_med_3, sm_tiktok = df$soc_med_4,
    sm_youtube = df$soc_med_5, sm_other = df$soc_med_6, sm_none = df$soc_med_7,
    n_platforms = lv(pmin(df$soc_med_1 + df$soc_med_2 + df$soc_med_3 + df$soc_med_4 + df$soc_med_5 + df$soc_med_6, 4),
                     0:4, c("0", "1", "2", "3", "4+"))
  )
}
dat <- bind_rows(code_vars(d, "tactic"), code_vars(intent_all, "intent")) %>%
  mutate(survey = factor(survey, SURVEY_LBL))

VARS <- c(age = "Age", gender = "Gender", income = "Individual income", educ = "Education",
          party = "Party identification", ideology = "Ideology", religion = "Religion",
          religiosity = "Religious service attendance", news = "Follows international news",
          warnews = "Follows news on wars and violence against civilians",
          n_platforms = "Number of social media platforms used regularly")

xtab <- function(var) {
  dat %>% filter(!is.na(.data[[var]]), !is.na(genocide)) %>%
    group_by(survey, level = .data[[var]]) %>%
    summarise(n = n(), yes = sum(genocide), .groups = "drop") %>%
    mutate(var = var, p = yes / n, se = sqrt(p * (1 - p) / n), lo = pmax(p - 1.96 * se, 0), hi = pmin(p + 1.96 * se, 1))
}
chi_p <- function(var) dat %>% filter(!is.na(.data[[var]]), !is.na(genocide)) %>%
  group_by(survey) %>%
  summarise(p_chi = suppressWarnings(chisq.test(table(.data[[var]], genocide))$p.value), .groups = "drop") %>%
  mutate(var = var)

tabs  <- map_dfr(names(VARS), xtab)
chis  <- map_dfr(names(VARS), chi_p)
overall <- dat %>% group_by(survey) %>% summarise(rate = mean(genocide, na.rm = TRUE), n = sum(!is.na(genocide)))

# Social media: users vs non-users of each platform
PLAT <- c(sm_facebook = "Facebook", sm_x = "Twitter/X", sm_instagram = "Instagram", sm_tiktok = "TikTok",
          sm_youtube = "YouTube", sm_other = "Other platform", sm_none = "No social media")
plat <- map_dfr(names(PLAT), function(v) {
  dat %>% filter(!is.na(genocide)) %>% group_by(survey, uses = .data[[v]]) %>%
    summarise(n = n(), yes = sum(genocide), .groups = "drop") %>%
    mutate(platform = PLAT[[v]], var = v, p = yes / n, se = sqrt(p * (1 - p) / n),
           lo = pmax(p - 1.96 * se, 0), hi = pmin(p + 1.96 * se, 1))
})
plat_chi <- map_dfr(names(PLAT), function(v) dat %>% filter(!is.na(genocide)) %>% group_by(survey) %>%
  summarise(p_chi = suppressWarnings(chisq.test(table(.data[[v]], genocide))$p.value), .groups = "drop") %>%
  mutate(platform = PLAT[[v]]))

write_csv(bind_rows(tabs %>% mutate(level = as.character(level)),
                    plat %>% transmute(survey, level = paste(platform, if_else(uses == 1, "user", "non-user")),
                                       n, yes, var, p, se, lo, hi)),
          file.path(OUT, "crosstabs_genocide_binary.csv"))
write_csv(bind_rows(chis, plat_chi %>% rename(var = platform)), file.path(OUT, "crosstabs_chisq.csv"))

# -----------------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------------
INK <- "#1b2433"; MUTED <- "#6a7890"; GRID <- "#e3e8f0"
theme_x <- function(base = 10) theme_minimal(base_size = base) +
  theme(text = element_text(color = INK), plot.title = element_text(face = "bold", size = base + 1),
        plot.subtitle = element_text(color = MUTED, size = base - 1.5),
        axis.text = element_text(color = MUTED), axis.text.y = element_text(color = INK),
        panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = GRID), legend.position = "none")

pfmt <- function(p) ifelse(p < .001, "p < .001", sprintf("p = %.3f", p))
panel <- function(var) {
  df <- filter(tabs, var == !!var) %>% mutate(level = fct_rev(level))
  cp <- filter(chis, var == !!var)
  sub <- paste0("χ² test — ", paste0(c("Exp 1 ", "Exp 2 "), pfmt(cp$p_chi[match(SURVEY_LBL, cp$survey)]), collapse = " · "))
  ggplot(df, aes(p, level, color = survey)) +
    geom_vline(data = overall, aes(xintercept = rate, color = survey), linetype = "dotted", linewidth = .5, alpha = .8) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = .5, position = position_dodge(width = .6)) +
    geom_point(aes(size = n), position = position_dodge(width = .6)) +
    geom_text(aes(x = hi, label = sprintf("%.0f%%", 100 * p)), hjust = -0.25, size = 2.6,
              position = position_dodge(width = .6), show.legend = FALSE) +
    scale_color_manual(values = SURVEY_COL) +
    scale_size_area(max_size = 3.2, guide = "none") +
    scale_x_continuous(labels = percent_format(1), limits = c(0, 1.05), breaks = seq(0, 1, .25)) +
    labs(title = VARS[[var]], subtitle = sub, x = NULL, y = NULL) +
    theme_x()
}

legend_note <- function() {
  ov <- overall %>% mutate(txt = sprintf("%s: %.1f%% overall (N = %s)", survey, 100 * rate, comma(n)))
  paste0("Share answering “Yes” to “Would you describe this situation as a genocide?”, pooled across treatment arms, with 95% CIs. ",
         "Dot size ∝ group n; dotted lines = each survey's overall rate.\n",
         paste(ov$txt, collapse = " · "),
         ". Levels differ between surveys because the vignettes differ; compare patterns across groups within each survey.")
}
key <- ggplot(tibble(s = factor(SURVEY_LBL, SURVEY_LBL), x = 1:2, y = 1), aes(x, y, color = s)) +
  geom_point(size = 3) + geom_text(aes(label = s), hjust = -0.15, color = INK, size = 3.4) +
  scale_color_manual(values = SURVEY_COL) + xlim(0.9, 2.9) + theme_void() + theme(legend.position = "none")

assemble <- function(vars, title, file, ncol = 2, height = 9.5) {
  p <- wrap_plots(lapply(vars, panel), ncol = ncol)
  full <- (key / p) + plot_layout(heights = c(.05, 1)) +
    plot_annotation(title = title, caption = legend_note(),
                    theme = theme(plot.title = element_text(face = "bold", size = 14, color = INK),
                                  plot.caption = element_text(color = MUTED, size = 8, hjust = 0)))
  ggsave(file.path(OUT, file), full, width = 12, height = height, dpi = 300, bg = "white")
}

assemble(c("age", "gender", "income", "educ"), "Genocide classification by demographics", "xtab_demographics.png", height = 10)
assemble(c("party", "ideology", "religion", "religiosity"), "Genocide classification by politics and religion", "xtab_politics_religion.png", height = 10)
assemble(c("news", "warnews"), "Genocide classification by attention to news (pre-treatment)", "xtab_news.png", height = 5.6)

# Social media figure: user vs non-user for each platform, plus number of platforms
plat_df <- plat %>%
  mutate(uses = factor(if_else(uses == 1, "Uses", "Does not use"), c("Does not use", "Uses")),
         y = match(platform, rev(PLAT)) + if_else(survey == SURVEY_LBL[["tactic"]], .17, -.17))
seg <- plat_df %>% select(survey, platform, y, uses, p) %>%
  pivot_wider(names_from = uses, values_from = p) %>%
  left_join(plat_chi, by = c("survey", "platform")) %>%
  mutate(lab = ifelse(p_chi < .001, "p<.001", sprintf("p=%.3f", p_chi)), sig = p_chi < .05)
p_plat <- ggplot(plat_df) +
  geom_segment(data = seg, aes(x = `Does not use`, xend = Uses, y = y, yend = y, color = survey),
               linewidth = .7, alpha = .45) +
  geom_point(aes(p, y, color = survey, shape = uses, size = n), fill = "white", stroke = 1) +
  geom_text(data = seg, aes(x = .80, y = y, label = lab, color = survey, fontface = ifelse(sig, "bold", "plain")),
            hjust = 1, size = 2.6, show.legend = FALSE) +
  scale_color_manual(values = SURVEY_COL, guide = "none") +
  scale_shape_manual(values = c("Does not use" = 21, "Uses" = 16), name = NULL) +
  scale_size_area(max_size = 3.2, guide = "none") +
  scale_y_continuous(breaks = seq_along(PLAT), labels = rev(PLAT), expand = expansion(add = .5)) +
  scale_x_continuous(labels = percent_format(1), limits = c(.25, .80), breaks = seq(.3, .7, .1)) +
  labs(title = "Platform users vs. non-users",
       subtitle = "Filled = uses the platform, hollow = does not; χ² p-value at right (bold if p < .05)",
       x = NULL, y = NULL) +
  theme_x() + theme(legend.position = "top")
sm <- (key / (p_plat | panel("n_platforms"))) + plot_layout(heights = c(.06, 1)) +
  plot_annotation(title = "Genocide classification by social media use (pre-treatment)", caption = legend_note(),
                  theme = theme(plot.title = element_text(face = "bold", size = 14, color = INK),
                                plot.caption = element_text(color = MUTED, size = 8, hjust = 0)))
ggsave(file.path(OUT, "xtab_social_media.png"), sm, width = 13, height = 6.5, dpi = 300, bg = "white")

cat("\n== Chi-square tests (genocide binary × characteristic) ==\n")
bind_rows(chis %>% mutate(var = VARS[var]), plat_chi %>% rename(var = platform)) %>%
  pivot_wider(names_from = survey, values_from = p_chi) %>% mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  print(n = Inf)
cat("\n== Cross tabs (share Yes) ==\n")
tabs %>% transmute(var, level, survey = ifelse(survey == SURVEY_LBL[["intent"]], "E1", "E2"), cell = sprintf("%.0f%% (n=%d)", 100 * p, n)) %>%
  pivot_wider(names_from = survey, values_from = cell) %>% print(n = Inf)
plat %>% transmute(platform, uses, survey = ifelse(survey == SURVEY_LBL[["intent"]], "E1", "E2"), cell = sprintf("%.0f%% (n=%d)", 100 * p, n)) %>%
  pivot_wider(names_from = survey, values_from = cell) %>% print(n = Inf)
cat("\nOutputs in", OUT, "\n")
