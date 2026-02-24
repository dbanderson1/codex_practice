# -----------------------------------------------------------------------
# Title: Individualized Report Generator
# Author: Derek Anderson
# Description: Scipt To Generate Individualized Wellness Reports 
# Created: Tuesday, 18 April 2023

# -----------------------------------------------------------------------

#load libraries 
library(psych)

#Load data
Data <- read.csv("data/pft.csv", row.names=1)
Data <- na.omit(Data)


# Create Labels for Categories  #
# 1. BMI Categories
# 2. Body Fat % Categories
# 3. VO2 Max Categories

#### BMI Categories ####

Data$BMI_cat <- NA
for (n in 1:nrow(Data)) 
  if (Data$BMI[n] < 18.5) {
    Data$BMI_cat[n] = "Underweight"
  } else if (Data$BMI[n] >= 18.5 & (Data$BMI[n] <= 24.9)) {
    Data$BMI_cat[n] = "Normal"
  }else if (Data$BMI[n] >= 25 & (Data$BMI[n] <= 29.9)) {
    Data$BMI_cat[n] = "Overweight"
  }else if (Data$BMI[n] >= 30 & (Data$BMI[n] <= 34.9)) {
    Data$BMI_cat[n] = "Obesity Class I"
  }else if (Data$BMI[n] >= 35 & (Data$BMI[n] <= 39.9)) {
    Data$BMI_cat[n] = "Obestity Class II"
  }else if (Data$BMI[n] <= 40) {
    Data$BMI_cat[n] = "Obestity Class III"
  }


#### Body Fat % Categories ####

Data$BFperc_cat <- NA

assign_bf_category <- function(sex, age, body_fat_perc) {
  if (is.na(sex) || is.na(age) || is.na(body_fat_perc)) {
    return(NA)
  }

  # Thresholds follow ACSM body-fat standards and are organized as:
  # c(essential_min, essential_max, excellent_min, good_min,
  #   average_min, below_average_min, poor_min)
  male_thresholds <- list(
    `20-29` = c(2, 5, 7.1, 9.4, 14.1, 17.4, 22.4),
    `30-39` = c(2, 5, 11.3, 13.9, 17.5, 20.5, 24.2),
    `40-49` = c(2, 5, 13.6, 16.3, 19.6, 22.5, 26.1),
    `50-59` = c(2, 5, 15.3, 17.9, 21.3, 24.1, 27.5),
    `60+` = c(2, 5, 15.3, 18.4, 22.0, 25.0, 28.5)
  )

  female_thresholds <- list(
    `20-29` = c(10, 13, 14.5, 17.1, 20.6, 23.7, 27.7),
    `30-39` = c(10, 13, 15.5, 18.0, 21.6, 24.9, 29.3),
    `40-49` = c(10, 13, 18.5, 21.3, 24.9, 28.1, 32.1),
    `50-59` = c(10, 13, 21.6, 25.0, 28.5, 31.6, 35.6),
    `60+` = c(10, 13, 21.1, 25.1, 29.3, 32.5, 36.6)
  )

  age_band <- NA
  if (age >= 20 & age <= 29) {
    age_band <- "20-29"
  } else if (age >= 30 & age <= 39) {
    age_band <- "30-39"
  } else if (age >= 40 & age <= 49) {
    age_band <- "40-49"
  } else if (age >= 50 & age <= 59) {
    age_band <- "50-59"
  } else if (age >= 60) {
    age_band <- "60+"
  }

  if (is.na(age_band)) {
    return(NA)
  }

  thresholds <- NULL
  if (sex == 0 && age_band %in% names(male_thresholds)) {
    thresholds <- male_thresholds[[age_band]]
  } else if (sex == 1 && age_band %in% names(female_thresholds)) {
    thresholds <- female_thresholds[[age_band]]
  } else {
    return(NA)
  }

  if (body_fat_perc > thresholds[7]) {
    return("Poor")
  } else if (body_fat_perc >= thresholds[6]) {
    return("Below Average")
  } else if (body_fat_perc >= thresholds[5]) {
    return("Average")
  } else if (body_fat_perc >= thresholds[4]) {
    return("Good")
  } else if (body_fat_perc >= thresholds[3]) {
    return("Excellent")
  }

  return("Essential Fat")
}

for (n in 1:nrow(Data)) {
  Data$BFperc_cat[n] <- assign_bf_category(Data$Sex[n], Data$Age[n], Data$BodyFatPer[n])
}

#### VO2MAX Scales ####               
Data$VO2_cat <- NA

assign_vo2_category <- function(sex, age, vo2_max) {
  if (is.na(sex) || is.na(age) || is.na(vo2_max)) {
    return(NA)
  }

  age_band <- NA
  if (age >= 20 & age <= 29) {
    age_band <- "20-29"
  } else if (age >= 30 & age <= 39) {
    age_band <- "30-39"
  } else if (age >= 40 & age <= 49) {
    age_band <- "40-49"
  } else if (age >= 50 & age <= 59) {
    age_band <- "50-59"
  } else if (age >= 60 & age <= 69) {
    age_band <- "60-69"
  }

  if (is.na(age_band)) {
    return(NA)
  }

  # Thresholds are lower bounds for categories in this order:
  # c(poor_min, fair_min, good_min, excellent_min, superior_min)
  male_thresholds <- list(
    `20-29` = c(35.5, 43.4, 49.1, 55.2, 61.9),
    `30-39` = c(32.8, 38.6, 43.9, 49.3, 56.6),
    `40-49` = c(29.1, 34.7, 39.0, 45.1, 52.2),
    `50-59` = c(24.3, 29.6, 33.9, 39.8, 45.7),
    `60-69` = c(21.3, 25.8, 29.2, 34.6, 40.4)
  )

  female_thresholds <- list(
    `20-29` = c(26.3, 33.7, 39.0, 44.8, 51.4),
    `30-39` = c(22.6, 27.5, 31.3, 36.2, 41.5)
  )

  thresholds <- NULL
  if (sex == 0 && age_band %in% names(male_thresholds)) {
    thresholds <- male_thresholds[[age_band]]
  } else if (sex == 1 && age_band %in% names(female_thresholds)) {
    thresholds <- female_thresholds[[age_band]]
  } else {
    return(NA)
  }

  if (vo2_max >= thresholds[5]) {
    return("Superior")
  } else if (vo2_max >= thresholds[4]) {
    return("Excellent")
  } else if (vo2_max >= thresholds[3]) {
    return("Good")
  } else if (vo2_max >= thresholds[2]) {
    return("Fair")
  } else if (vo2_max >= thresholds[1]) {
    return("Poor")
  }

  return("Very Poor")
}

for (n in 1:nrow(Data)) {
  Data$VO2_cat[n] <- assign_vo2_category(Data$Sex[n], Data$Age[n], Data$Est.Vo2.M[n])
}

#### Grip Strength Categories ####

Data$GS_cat <- NA
for (n in 1:nrow(Data)) {
  gs <- Data$GripSum[n]
  age <- Data$Age[n]
  sex <- Data$Sex[n]

  if (sex == 0 && age >= 20 && age <= 39) {
    if (gs <= 83) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 94) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 103) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 114) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 40 && age <= 49) {
    if (gs <= 79) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 87) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 96) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 107) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 50 && age <= 59) {
    if (gs <= 75) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 83) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 91) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 100) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 60 && age <= 69) {
    if (gs <= 72) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 83) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 90) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 99) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  } else if (sex == 1 && age >= 20 && age <= 29) {
    if (gs <= 51) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 57) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 62) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 69) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  } else if (sex == 1 && age >= 30 && age <= 39) {
    if (gs <= 50) {
      Data$GS_cat[n] <- "Poor"
    } else if (gs <= 57) {
      Data$GS_cat[n] <- "Fair"
    } else if (gs <= 62) {
      Data$GS_cat[n] <- "Good"
    } else if (gs <= 70) {
      Data$GS_cat[n] <- "Very Good"
    } else {
      Data$GS_cat[n] <- "Excellent"
    }
  }
}

#### Bench Press Categories ####

Data$BP_cat <- NA
for (n in 1:nrow(Data)) {
  bp <- Data$BP[n]
  age <- Data$Age[n]
  sex <- Data$Sex[n]

  if (sex == 0 && age >= 18 && age <= 25) {
    if (bp < 12) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 19) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 23) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 28) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 33) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 43) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 26 && age <= 35) {
    if (bp < 11) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 16) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 20) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 25) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 29) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 40) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 36 && age <= 45) {
    if (bp < 8) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 13) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 17) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 21) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 25) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 35) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 46 && age <= 55) {
    if (bp < 4) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 8) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 11) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 15) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 20) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 27) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 0 && age >= 56 && age <= 65) {
    if (bp < 1) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 4) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 8) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 11) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 16) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 23) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 1 && age >= 18 && age <= 25) {
    if (bp < 8) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 15) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 19) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 24) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 29) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 41) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  } else if (sex == 1 && age >= 26 && age <= 35) {
    if (bp < 5) {
      Data$BP_cat[n] <- "Very Poor"
    } else if (bp <= 11) {
      Data$BP_cat[n] <- "Poor"
    } else if (bp <= 15) {
      Data$BP_cat[n] <- "Below Average"
    } else if (bp <= 20) {
      Data$BP_cat[n] <- "Average"
    } else if (bp <= 25) {
      Data$BP_cat[n] <- "Above Average"
    } else if (bp <= 32) {
      Data$BP_cat[n] <- "Good"
    } else {
      Data$BP_cat[n] <- "Excellent"
    }
  }
}

# Vertical Jump ----------------------------------------------------

Data$VJ_cat <- NA
for (n in 1:nrow(Data)) {
  vj <- Data$VJHgtM[n]
  sex <- Data$Sex[n]

  if (sex == 0) {
    if (vj < 8) {
      Data$VJ_cat[n] <- "Very Poor"
    } else if (vj <= 12) {
      Data$VJ_cat[n] <- "Poor"
    } else if (vj <= 16) {
      Data$VJ_cat[n] <- "Below Average"
    } else if (vj <= 20) {
      Data$VJ_cat[n] <- "Average"
    } else if (vj <= 24) {
      Data$VJ_cat[n] <- "Above Average"
    } else if (vj <= 28) {
      Data$VJ_cat[n] <- "Good"
    } else {
      Data$VJ_cat[n] <- "Excellent"
    }
  } else if (sex == 1) {
    if (vj < 4) {
      Data$VJ_cat[n] <- "Very Poor"
    } else if (vj <= 8) {
      Data$VJ_cat[n] <- "Poor"
    } else if (vj <= 12) {
      Data$VJ_cat[n] <- "Below Average"
    } else if (vj <= 16) {
      Data$VJ_cat[n] <- "Average"
    } else if (vj <= 20) {
      Data$VJ_cat[n] <- "Above Average"
    } else if (vj <= 24) {
      Data$VJ_cat[n] <- "Good"
    } else {
      Data$VJ_cat[n] <- "Excellent"
    }
  }
}

# Plank All ---------------------------------------------------------------

Data$Plank_cat <- NA
for (n in 1:nrow(Data)) {
  plank <- Data$Plank[n]
  if (plank < 15) {
    Data$Plank_cat[n] <- "Very Poor"
  } else if (plank <= 30) {
    Data$Plank_cat[n] <- "Poor"
  } else if (plank <= 59.9) {
    Data$Plank_cat[n] <- "Below Average"
  } else {
    Data$Plank_cat[n] <- "Average"
  }
}

# Sit and Reach ---------------------------------------------------------------

Data$SR_cat <- NA

assign_sr_category <- function(sex, age, sit_reach_score) {
  if (is.na(sex) || is.na(age) || is.na(sit_reach_score)) {
    return(NA)
  }

  age_band <- NA
  if (age < 20) {
    age_band <- "<20"
  } else if (age >= 20 & age <= 29) {
    age_band <- "20-29"
  } else if (age >= 30 & age <= 39) {
    age_band <- "30-39"
  } else if (age >= 40 & age <= 49) {
    age_band <- "40-49"
  } else if (age >= 50 & age <= 59) {
    age_band <- "50-59"
  } else if (age >= 60) {
    age_band <- "60+"
  }

  if (is.na(age_band)) {
    return(NA)
  }

  # Lower bounds are ordered as:
  # c(poor_min, fair_min, good_min, excellent_min, superior_min)
  male_thresholds <- list(
    `<20` = c(10.5, 15.5, 18.0, 20.7, 22.6),
    `20-29` = c(12.3, 15.5, 17.5, 19.5, 21.8),
    `30-39` = c(11.0, 14.5, 16.5, 18.5, 21.0),
    `40-49` = c(10.0, 13.3, 15.3, 17.5, 20.0),
    `50-59` = c(8.5, 12.0, 14.5, 16.5, 19.0),
    `60+` = c(8.0, 11.3, 13.5, 15.5, 19.0)
  )

  female_thresholds <- list(
    `<20` = c(14.5, 18.5, 21.0, 22.0, 24.3),
    `20-29` = c(15.4, 18.3, 20.0, 21.5, 23.8),
    `30-39` = c(14.4, 17.3, 19.0, 20.5, 22.5),
    `40-49` = c(13.0, 16.5, 18.0, 19.8, 21.5),
    `50-59` = c(13.0, 15.5, 17.9, 19.3, 21.0),
    `60+` = c(11.5, 14.4, 16.4, 17.5, 21.8)
  )

  thresholds <- NULL
  if (sex == 0 && age_band %in% names(male_thresholds)) {
    thresholds <- male_thresholds[[age_band]]
  } else if (sex == 1 && age_band %in% names(female_thresholds)) {
    thresholds <- female_thresholds[[age_band]]
  } else {
    return(NA)
  }

  if (sit_reach_score >= thresholds[5]) {
    return("Superior")
  } else if (sit_reach_score >= thresholds[4]) {
    return("Excellent")
  } else if (sit_reach_score >= thresholds[3]) {
    return("Good")
  } else if (sit_reach_score >= thresholds[2]) {
    return("Fair")
  } else if (sit_reach_score >= thresholds[1]) {
    return("Poor")
  }

  return("Very Poor")
}

for (n in 1:nrow(Data)) {
  Data$SR_cat[n] <- assign_sr_category(Data$Sex[n], Data$Age[n], Data$S.Rmax[n])
}

# END WITH SAVE AND COMMITT 
