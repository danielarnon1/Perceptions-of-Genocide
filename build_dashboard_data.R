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

js <- paste0("const DATA = ", toJSON(list(intent = intent, tactic = tactic), auto_unbox = TRUE, digits = NA,
                                      dataframe = "rows"), ";")

p <- file.path(ROOT, "dashboard.html")
h <- readLines(p, encoding = "UTF-8")
a <- grep("^// <<DATA", h); b <- grep("^// DATA>>", h)
stopifnot(length(a) == 1, length(b) == 1, b > a)
h2 <- c(h[1:a], js, h[b:length(h)])
con <- file(p, open = "wb"); writeLines(enc2utf8(h2), con, useBytes = TRUE); close(con)
cat("Wrote DATA block (", nchar(js), "chars ) into dashboard.html\n")
