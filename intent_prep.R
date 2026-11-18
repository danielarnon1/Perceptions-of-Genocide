# =============================================================================
# Genocide Perceptions — Experiment 1 (Intent): data load and recoding
# Van Nostrand, Schoner & Arnon (2026). Verasight #2026-241, fielded Nov 4–12, 2026.
# Sourced by intent_descriptives.R and the experiment analysis.
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(haven)
})

INTENT_DIR <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\Surveys\Genocide Perceptions  - Survey II Intent - Results\2026-241_files)"
INTENT_OUT <- r"(C:\Users\danielarnon\Dropbox\My PC (LAPTOP-V9QNAF2J)\Documents\daniel\Universities\Arizona\Research\Genocide Perceptions\Figures\intent)"
dir.create(INTENT_OUT, showWarnings = FALSE, recursive = TRUE)

ARM_LEVELS <- c("intent_baseline", "intent_denial", "intent_justify", "intent_stated")
ARM_LABELS <- c("No intent (control)", "Denies intent",
                "Denies intent + security justification", "States intent to destroy")
ARM_SHORT  <- c("Control", "Denial", "Denial + justify", "Stated intent")

intent_raw <- readRDS(file.path(INTENT_DIR, "2026-241_client.rds")) %>% zap_labels()

# Verasight already removed respondents failing the in-survey instructed-response
# check (attention_1 is blank for everyone). attention_2 is the vignette recall
# check; the correct answer is "Country B" (code 2). Recall failures are kept
# (deviation from PAP exclusion rule) and adjusted for via failed_recall.
na99 <- function(x) if_else(x == 99, NA_real_, as.numeric(x))

intent <- intent_raw %>%
  mutate(
    passed_recall = attention_2 == 2,
    failed_recall = as.integer(attention_2 != 2),
    arm   = factor(treatment_condition, levels = ARM_LEVELS, labels = ARM_LABELS),
    arm_s = factor(treatment_condition, levels = ARM_LEVELS, labels = ARM_SHORT),

    # Main DVs
    genocide_binary = as.integer(exp_1_genocide_force == 1),
    genocide_fit    = as.numeric(exp_1_genocide_leve),            # 1–5
    takeaction_cat  = factor(exp1_takeaction, 1:3, c("Yes", "No", "Not sure")),
    support_action  = case_when(exp1_takeaction == 1 ~ 1L, exp1_takeaction == 2 ~ 0L,
                                TRUE ~ NA_integer_),              # Not sure → NA, as in Exp 2
    act_diplomatic   = exp1_actiontype_1,
    act_economic     = exp1_actiontype_2,
    act_humanitarian = exp1_actiontype_3,
    act_legal        = exp1_actiontype_4,
    act_military     = exp1_actiontype_5,
    act_none         = exp1_actiontype_6,
    n_actions = exp1_actiontype_1 + exp1_actiontype_2 + exp1_actiontype_3 +
                exp1_actiontype_4 + exp1_actiontype_5,

    # Demographics (end of survey)
    age_cat      = as.numeric(age),                                # 1 = 18–29 … 8 = 90+
    female       = case_when(gender == 2 ~ 1, gender == 1 ~ 0, TRUE ~ NA_real_),
    educ_num     = as.numeric(educ),                               # 1–7
    college      = as.numeric(educ >= 5),
    income_num   = na99(income),                                   # 1–7
    employ_ft    = as.numeric(employ == 5),
    retired      = as.numeric(employ == 1),
    rel_protestant = if_else(religion == 99, NA_real_, as.numeric(religion == 1)),
    rel_catholic   = if_else(religion == 99, NA_real_, as.numeric(religion == 2)),
    rel_none       = if_else(religion == 99, NA_real_, as.numeric(religion == 8)),
    religiosity_num = as.numeric(religiosity),                     # 1–6
    party_rep    = as.numeric(party == 1),
    party_dem    = as.numeric(party == 2),
    party_ind    = as.numeric(party == 3),
    ideology_num = as.numeric(ideology),                           # 1 = very liberal … 5
    # Pre-treatment attention to news
    news_num     = as.numeric(pre_news),
    news_war_num = as.numeric(pre_news_war),
    # Verasight panel profile
    white    = as.numeric(vs_race == "White"),
    hispanic = as.numeric(vs_hispanic == "Yes"),
    metro    = case_when(vs_metro == "Metropolitan" ~ 1, vs_metro == "Non-metro" ~ 0, TRUE ~ NA_real_)
  )

intent_all <- intent

# Pre-treatment controls for the experiment models, plus the recall-failure flag
INTENT_CONTROLS <- c("ideology_num", "party_dem", "party_rep", "educ_num",
                     "news_num", "age_cat", "female", "failed_recall")

BAL_VARS <- tribble(
  ~var,             ~label,                          ~type,
  "age_cat",        "Age (category, 1–8)",           "num",
  "female",         "Female",                        "bin",
  "educ_num",       "Education (1–7)",               "num",
  "college",        "4-year degree or more",         "bin",
  "income_num",     "Income (1–7)",                  "num",
  "employ_ft",      "Employed full time",            "bin",
  "retired",        "Retired",                       "bin",
  "rel_protestant", "Protestant / Christian",        "bin",
  "rel_catholic",   "Catholic",                      "bin",
  "rel_none",       "No religion",                   "bin",
  "religiosity_num","Religious attendance (1–6)",    "num",
  "party_dem",      "Democrat",                      "bin",
  "party_rep",      "Republican",                    "bin",
  "party_ind",      "Independent",                   "bin",
  "ideology_num",   "Ideology (1 = very lib, 5)",    "num",
  "news_num",       "Follows intl. news (1–5)",      "num",
  "news_war_num",   "Follows war news (1–5)",        "num",
  "white",          "White (panel profile)",         "bin",
  "hispanic",       "Hispanic (panel profile)",      "bin",
  "metro",          "Metropolitan (panel profile)",  "bin"
)
