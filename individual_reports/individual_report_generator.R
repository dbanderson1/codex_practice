# -----------------------------------------------------------------------
# Title: Individualized Report Generator
# Author: Derek Anderson
# Description: Scipt To Generate Individualized Wellness Reports
# Created: Tuesday, 18 April 2023

# -----------------------------------------------------------------------

#load libraries
library(psych)

#Load data
Data <- read.csv("data/pft.csv", row.names = 1)
Data <- na.omit(Data)

# Shared category helpers -------------------------------------------------

age_band <- function(age, breaks) {
  for (rule in breaks) {
    min_age <- rule[[1]]
    max_age <- rule[[2]]
    band <- rule[[3]]

    if (!is.na(min_age) && age < min_age) next
    if (!is.na(max_age) && age > max_age) next
    return(band)
  }
  NA
}

categorize_upper <- function(value, upper_bounds, labels) {
  if (is.na(value)) return(NA)

  for (i in seq_along(upper_bounds)) {
    if (value <= upper_bounds[i]) return(labels[i])
  }

  labels[length(labels)]
}

categorize_lower <- function(value, lower_bounds, labels) {
  if (is.na(value)) return(NA)

  for (i in seq_along(lower_bounds):1) {
    if (value >= lower_bounds[i]) return(labels[i])
  }

  labels[1]
}

get_thresholds <- function(sex, age, male_thresholds, female_thresholds, breaks) {
  if (is.na(sex) || is.na(age)) return(NULL)

  band <- age_band(age, breaks)
  if (is.na(band)) return(NULL)

  if (sex == 0 && band %in% names(male_thresholds)) return(male_thresholds[[band]])
  if (sex == 1 && band %in% names(female_thresholds)) return(female_thresholds[[band]])

  NULL
}

# Create Labels for Categories -------------------------------------------

#### BMI Categories ####
# These are standardized BMI cut-off categories #

bmi_upper_bounds <- c(18.5, 24.9, 29.9, 34.9, 39.9, Inf)
bmi_labels <- c(
  "Underweight",
  "Normal",
  "Overweight",
  "Obesity Class I",
  "Obestity Class II",
  "Obestity Class III"
)

Data$BMI_cat <- vapply(
  Data$BMI,
  categorize_upper,
  character(1),
  upper_bounds = bmi_upper_bounds,
  labels = bmi_labels
)

#### Body Fat % Categories ####
# These are the ACSM Body Fat % chart, ACSM Health Related Physical Fitness Assessment Manual, 2 edition, 2008
# https://www.scribd.com/document/331711396/ACSM-Body-Composition

bf_labels <- c("Essential Fat", "Excellent", "Good", "Average", "Below Average", "Poor")
bf_breaks <- list(
  list(20, 29, "20-29"),
  list(30, 39, "30-39"),
  list(40, 49, "40-49"),
  list(50, 59, "50-59"),
  list(60, NA, "60+")
)

male_bf_thresholds <- list(
  `20-29` = c(7.1, 9.4, 14.1, 17.4, 22.4),
  `30-39` = c(11.3, 13.9, 17.5, 20.5, 24.2),
  `40-49` = c(13.6, 16.3, 19.6, 22.5, 26.1),
  `50-59` = c(15.3, 17.9, 21.3, 24.1, 27.5),
  `60+` = c(15.3, 18.4, 22.0, 25.0, 28.5)
)

female_bf_thresholds <- list(
  `20-29` = c(14.5, 17.1, 20.6, 23.7, 27.7),
  `30-39` = c(15.5, 18.0, 21.6, 24.9, 29.3),
  `40-49` = c(18.5, 21.3, 24.9, 28.1, 32.1),
  `50-59` = c(21.6, 25.0, 28.5, 31.6, 35.6),
  `60+` = c(21.1, 25.1, 29.3, 32.5, 36.6)
)

assign_bf_category <- function(sex, age, body_fat_perc) {
  thresholds <- get_thresholds(sex, age, male_bf_thresholds, female_bf_thresholds, bf_breaks)
  if (is.null(thresholds) || is.na(body_fat_perc)) return(NA)
  categorize_lower(body_fat_perc, thresholds, bf_labels)
}

Data$BFperc_cat <- mapply(assign_bf_category, Data$Sex, Data$Age, Data$BodyFatPer)

#### VO2MAX Scales ####


vo2_labels <- c("Very Poor", "Poor", "Fair", "Good", "Excellent", "Superior")
vo2_breaks <- list(
  list(20, 29, "20-29"),
  list(30, 39, "30-39"),
  list(40, 49, "40-49"),
  list(50, 59, "50-59"),
  list(60, 69, "60-69")
)

male_vo2_thresholds <- list(
  `20-29` = c(35.5, 43.4, 49.1, 55.2, 61.9),
  `30-39` = c(32.8, 38.6, 43.9, 49.3, 56.6),
  `40-49` = c(29.1, 34.7, 39.0, 45.1, 52.2),
  `50-59` = c(24.3, 29.6, 33.9, 39.8, 45.7),
  `60-69` = c(21.3, 25.8, 29.2, 34.6, 40.4)
)

female_vo2_thresholds <- list(
  `20-29` = c(26.3, 33.7, 39.0, 44.8, 51.4),
  `30-39` = c(22.6, 27.5, 31.3, 36.2, 41.5)
)

assign_vo2_category <- function(sex, age, vo2_max) {
  thresholds <- get_thresholds(sex, age, male_vo2_thresholds, female_vo2_thresholds, vo2_breaks)
  if (is.null(thresholds) || is.na(vo2_max)) return(NA)
  categorize_lower(vo2_max, thresholds, vo2_labels)
}

Data$VO2_cat <- mapply(assign_vo2_category, Data$Sex, Data$Age, Data$Est.Vo2.M)

#### Grip Strength Categories ####

gs_labels <- c("Poor", "Fair", "Good", "Very Good", "Excellent")
gs_breaks <- list(
  list(20, 39, "20-39"),
  list(40, 49, "40-49"),
  list(50, 59, "50-59"),
  list(60, 69, "60-69")
)

male_gs_thresholds <- list(
  `20-39` = c(83, 94, 103, 114),
  `40-49` = c(79, 87, 96, 107),
  `50-59` = c(75, 83, 91, 100),
  `60-69` = c(72, 83, 90, 99)
)

female_gs_thresholds <- list(
  `20-29` = c(51, 57, 62, 69),
  `30-39` = c(50, 57, 62, 70)
)

female_gs_breaks <- list(
  list(20, 29, "20-29"),
  list(30, 39, "30-39")
)

assign_gs_category <- function(sex, age, gs) {
  if (is.na(sex) || is.na(age) || is.na(gs)) return(NA)

  if (sex == 0) {
    band <- age_band(age, gs_breaks)
    if (is.na(band) || !band %in% names(male_gs_thresholds)) return(NA)
    return(categorize_upper(gs, c(male_gs_thresholds[[band]], Inf), gs_labels))
  }

  if (sex == 1) {
    band <- age_band(age, female_gs_breaks)
    if (is.na(band) || !band %in% names(female_gs_thresholds)) return(NA)
    return(categorize_upper(gs, c(female_gs_thresholds[[band]], Inf), gs_labels))
  }

  NA
}

Data$GS_cat <- mapply(assign_gs_category, Data$Sex, Data$Age, Data$GripSum)

#### Bench Press Categories ####
# YMCA Bench Press

bp_labels <- c("Very Poor", "Poor", "Below Average", "Average", "Above Average", "Good", "Excellent")
bp_breaks <- list(
  list(18, 25, "18-25"),
  list(26, 35, "26-35"),
  list(36, 45, "36-45"),
  list(46, 55, "46-55"),
  list(56, 65, "56-65")
)

male_bp_thresholds <- list(
  `18-25` = c(11, 19, 23, 28, 33, 43),
  `26-35` = c(10, 16, 20, 25, 29, 40),
  `36-45` = c(7, 13, 17, 21, 25, 35),
  `46-55` = c(3, 8, 11, 15, 20, 27),
  `56-65` = c(0, 4, 8, 11, 16, 23)
)

female_bp_thresholds <- list(
  `18-25` = c(7, 15, 19, 24, 29, 41),
  `26-35` = c(4, 11, 15, 20, 25, 32)
)

assign_bp_category <- function(sex, age, bp) {
  if (is.na(sex) || is.na(age) || is.na(bp)) return(NA)

  thresholds <- get_thresholds(sex, age, male_bp_thresholds, female_bp_thresholds, bp_breaks)
  if (is.null(thresholds)) return(NA)

  categorize_upper(bp, c(thresholds, Inf), bp_labels)
}

Data$BP_cat <- mapply(assign_bp_category, Data$Sex, Data$Age, Data$BP)

# Vertical Jump -----------------------------------------------------------

vj_labels <- c("Very Poor", "Poor", "Below Average", "Average", "Above Average", "Good", "Excellent")
male_vj_thresholds <- c(8, 12, 16, 20, 24, 28, Inf)
female_vj_thresholds <- c(4, 8, 12, 16, 20, 24, Inf)

assign_vj_category <- function(sex, vj) {
  if (is.na(sex) || is.na(vj)) return(NA)
  if (sex == 0) return(categorize_upper(vj, male_vj_thresholds, vj_labels))
  if (sex == 1) return(categorize_upper(vj, female_vj_thresholds, vj_labels))
  NA
}

Data$VJ_cat <- mapply(assign_vj_category, Data$Sex, Data$VJHgtM)

# Plank All ---------------------------------------------------------------

plank_labels <- c("Very Poor", "Poor", "Below Average", "Average")
plank_thresholds <- c(15, 30, 59.9, Inf)

Data$Plank_cat <- vapply(
  Data$Plank,
  categorize_upper,
  character(1),
  upper_bounds = plank_thresholds,
  labels = plank_labels
)

# Sit and Reach -----------------------------------------------------------
# YMCA cut off values

sr_labels <- c("Very Poor", "Poor", "Fair", "Good", "Excellent", "Superior")
sr_breaks <- list(
  list(NA, 19, "<20"),
  list(20, 29, "20-29"),
  list(30, 39, "30-39"),
  list(40, 49, "40-49"),
  list(50, 59, "50-59"),
  list(60, NA, "60+")
)

male_sr_thresholds <- list(
  `<20` = c(10.5, 15.5, 18.0, 20.7, 22.6),
  `20-29` = c(12.3, 15.5, 17.5, 19.5, 21.8),
  `30-39` = c(11.0, 14.5, 16.5, 18.5, 21.0),
  `40-49` = c(10.0, 13.3, 15.3, 17.5, 20.0),
  `50-59` = c(8.5, 12.0, 14.5, 16.5, 19.0),
  `60+` = c(8.0, 11.3, 13.5, 15.5, 19.0)
)

female_sr_thresholds <- list(
  `<20` = c(14.5, 18.5, 21.0, 22.0, 24.3),
  `20-29` = c(15.4, 18.3, 20.0, 21.5, 23.8),
  `30-39` = c(14.4, 17.3, 19.0, 20.5, 22.5),
  `40-49` = c(13.0, 16.5, 18.0, 19.8, 21.5),
  `50-59` = c(13.0, 15.5, 17.9, 19.3, 21.0),
  `60+` = c(11.5, 14.4, 16.4, 17.5, 21.8)
)

assign_sr_category <- function(sex, age, sit_reach_score) {
  thresholds <- get_thresholds(sex, age, male_sr_thresholds, female_sr_thresholds, sr_breaks)
  if (is.null(thresholds) || is.na(sit_reach_score)) return(NA)
  categorize_lower(sit_reach_score, thresholds, sr_labels)
}

Data$SR_cat <- mapply(assign_sr_category, Data$Sex, Data$Age, Data$S.Rmax)

# END WITH SAVE AND COMMITT
