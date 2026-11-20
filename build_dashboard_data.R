# =============================================================================
# Builds the data block for dashboard.html from the analysis outputs.
# Run after analysis.R, intent_analysis.R and gaza_interaction.R.
# Replaces everything between the "// <<DATA" and "// DATA>>" lines.
# =============================================================================

suppressMessages({ library(tidyverse); library(jsonlite) })
ROOT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions)"
FIG  <- file.path(ROOT, "Figures")

DV_KEYS  <- c("genocide_binary", "genocide_fit", "support_action", "act_diplomatic", "act_economic",
              "act_humanitarian", "act_legal", "act_military", "act_none")
MODEL_KEYS <- c("No controls" = "no", "With controls" = "ctrl")

eff_block <- function(eff, cond_order) {
  dv_order <- unique(eff$dv)
  stopifnot(length(dv_order) == length(DV_KEYS))
  setNames(lapply(dv_order, function(lbl) {
    sub <- filter(eff, dv == lbl)
    setNames(lapply(names(MODEL_KEYS), function(m) {
      s <- sub %>% filter(model == m) %>% slice(match(cond_order, condition))
      stopifnot(nrow(s) == length(cond_order))
      transmute(s, e = estimate, lo = conf.low, hi = conf.high, p = p.value)
    }), MODEL_KEYS)
  }), DV_KEYS)
}

gaza_block <- function(sv, conds) {
  pr <- read_csv(file.path(FIG, "gaza_interaction_preds.csv"), show_col_types = FALSE) %>% filter(survey == sv)
  ef <- read_csv(file.path(FIG, "gaza_interaction_effects.csv"), show_col_types = FALSE) %>% filter(survey == sv)
  sm <- read_csv(file.path(FIG, "gaza_interaction_summary.csv"), show_col_types = FALSE) %>% filter(survey == sv)
  grp <- function(g) pr %>% filter(gaza_f == g) %>% slice(match(conds, condition)) %>%
    transmute(e = fit, lo = lwr, hi = upr, n)
  egrp <- function(g) ef %>% filter(group == g) %>% slice(match(conds[-1], condition)) %>%
    transmute(e = estimate, lo = conf.low, hi = conf.high, p = p.value)
  list(pred = list(no = grp("Did not think of Israel/Gaza"), yes = grp("Thought of Israel/Gaza")),
       eff  = list(no = egrp("Did not think of Israel/Gaza"), yes = egrp("Thought of Israel/Gaza"),
                   diff = egrp("Difference (interaction)")),
       n = sm$n, n_gaza = sm$n_gaza, share = sm$share_gaza, n_rw = sm$n_real_world,
       share_rw = sm$share_gaza_of_real_world, joint_p = sm$joint_interaction_p,
       main_e = sm$gaza_main_est, main_p = sm$gaza_main_p)
}

# --- Experiment 2: tactic × severity -----------------------------------------
t_means <- read_csv(file.path(FIG, "exp2_summary_means.csv"), show_col_types = FALSE)
t_conds <- t_means$cond
tactic <- list(
  n = t_means$n,
  means = list(genocide_binary = t_means$mean_genocide, genocide_fit = t_means$mean_fit,
               support_action = t_means$mean_action) %>%
    c(setNames(lapply(DV_KEYS[4:9], function(k) t_means[[paste0("mean_", k)]]), DV_KEYS[4:9])),
  eff = eff_block(read_csv(file.path(FIG, "exp2_treatment_effects.csv"), show_col_types = FALSE), t_conds[-1]),
  gaza = gaza_block("tactic", t_conds)
)

# --- Experiment 1: intent -----------------------------------------------------
i_means <- read_csv(file.path(FIG, "intent", "intent_summary_means.csv"), show_col_types = FALSE)
i_conds <- i_means$arm
intent <- list(
  n = i_means$n,
  means = setNames(lapply(DV_KEYS, function(k) i_means[[paste0(k, "_mean")]]), DV_KEYS),
  eff = eff_block(read_csv(file.path(FIG, "intent", "intent_treatment_effects.csv"), show_col_types = FALSE), i_conds[-1]),
  gaza = gaza_block("intent", i_conds)
)

# --- Post-treatment attitude descriptives (pooled across conditions) ---------
att_desc   <- read_csv(file.path(FIG, "secondary", "attitudes_descriptives.csv"), show_col_types = FALSE)
therm_bins <- read_csv(file.path(FIG, "secondary", "thermometer_bins.csv"), show_col_types = FALSE)
att_items <- function(sv, items) {
  lapply(intersect(items, unique(att_desc$item[att_desc$survey == sv])), function(it) {
    a <- att_desc %>% filter(survey == sv, item == it) %>% arrange(level)
    list(k = it, mean = a$mean[1], n = a$n_valid[1], shares = a$share)
  })
}
att_block <- function(sv) {
  out <- list(likert = att_items(sv, c("reserve", "commit", "moral", "symbolic", "avoid")),
              emotions = att_items(sv, c("anger", "fear", "disgust", "sadness", "contempt")))
  tb <- filter(therm_bins, survey == sv)
  if (nrow(tb)) out$therm <- lapply(unique(tb$item), function(it) {
    a <- tb %>% filter(item == it) %>% arrange(bin)
    list(k = it, mean = a$mean[1], median = a$median[1], n = a$n_valid[1], bins = a$share)
  })
  out
}
tactic$att <- att_block("tactic")
intent$att <- att_block("intent")

# --- Cross tabs: share saying "genocide" by respondent characteristic --------
xt  <- read_csv(file.path(FIG, "crosstabs", "crosstabs_genocide_binary.csv"), show_col_types = FALSE)
xtc <- read_csv(file.path(FIG, "crosstabs", "crosstabs_chisq.csv"), show_col_types = FALSE)
XT_VARS <- c("age", "gender", "income", "educ", "party", "ideology", "religion", "religiosity",
             "news", "warnews", "n_platforms")
XT_PLAT <- c(sm_facebook = "Facebook", sm_x = "Twitter/X", sm_instagram = "Instagram", sm_tiktok = "TikTok",
             sm_youtube = "YouTube", sm_other = "Other platform", sm_none = "No social media")
xtab_block <- function(sv_label, overall) {
  x <- filter(xt, survey == sv_label); cc <- filter(xtc, survey == sv_label)
  vars <- setNames(lapply(XT_VARS, function(v) {
    r <- filter(x, var == v)
    list(p_chi = cc$p_chi[cc$var == v], rows = transmute(r, level, n, p, lo, hi))
  }), XT_VARS)
  plat <- lapply(names(XT_PLAT), function(v) {
    r <- filter(x, var == v); u <- r[str_ends(r$level, " user"), ]; nu <- r[str_ends(r$level, "non-user"), ]
    list(platform = XT_PLAT[[v]], p_chi = cc$p_chi[cc$var == XT_PLAT[[v]]],
         user = list(p = u$p, lo = u$lo, hi = u$hi, n = u$n), non = list(p = nu$p, lo = nu$lo, hi = nu$hi, n = nu$n))
  })
  list(overall = overall, vars = vars, platforms = plat)
}
ses_comp <- read_csv(file.path(FIG, "crosstabs", "ses_news_composition.csv"), show_col_types = FALSE)
ses_jnt  <- read_csv(file.path(FIG, "crosstabs", "ses_news_joint_by_survey.csv"), show_col_types = FALSE)
ses_int  <- read_csv(file.path(FIG, "crosstabs", "ses_news_interaction_by_survey.csv"), show_col_types = FALSE)
ses_block <- function(sv_label) {
  WAR <- "Follows news on wars and violence against civilians"
  grp <- function(lbl) {
    comp <- filter(ses_comp, survey == sv_label, ses == lbl)
    j <- filter(ses_jnt, survey == sv_label, ses == lbl, news == WAR)
    lapply(unique(comp$level), function(l) {
      cc <- filter(comp, level == l); a <- filter(j, ses_level == l)
      end <- function(k) { r <- filter(a, news_level == k); list(p = r$p, n = r$n) }
      list(level = l,
           intl = cc$share[str_detect(cc$item, "international")], war = cc$share[str_detect(cc$item, "war")], n = cc$n[1],
           lo = end("Not closely"), hi = end("Very / extremely closely"))
    })
  }
  ip <- filter(ses_int, survey == sv_label)
  list(educ = grp("Education"), inc = grp("Individual income"), p_educ = ip$p_educ, p_inc = ip$p_inc)
}
tactic$ses <- ses_block("Exp 2: Tactic × Severity")
intent$ses <- ses_block("Exp 1: Intent")

tactic$xtab <- xtab_block("Exp 2: Tactic × Severity", weighted.mean(t_means$mean_genocide, t_means$n))
intent$xtab <- xtab_block("Exp 1: Intent", weighted.mean(i_means$genocide_binary_mean, i_means$n))

js <- paste0("const DATA = ", toJSON(list(intent = intent, tactic = tactic), auto_unbox = TRUE, digits = NA,
                                      dataframe = "rows"), ";")

p <- file.path(ROOT, "dashboard.html")
h <- readLines(p, encoding = "UTF-8")
a <- grep("^// <<DATA", h); b <- grep("^// DATA>>", h)
stopifnot(length(a) == 1, length(b) == 1, b > a)
h2 <- c(h[1:a], js, h[b:length(h)])
con <- file(p, open = "wb"); writeLines(enc2utf8(h2), con, useBytes = TRUE); close(con)
cat("Wrote DATA block (", nchar(js), "chars ) into dashboard.html\n")
