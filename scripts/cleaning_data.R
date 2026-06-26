#load library
library(tidyverse)

data <- read.csv(file.choose())


#create y for heroin
data <- data %>%
  mutate(
    Y_HEROIN = case_when(
      HERFLAG == 1 & HERMON == 1 ~ 1,
      HERFLAG == 1 & HERMON == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

#create y for coc
data <- data %>%
  mutate(
    Y_COCAINE = case_when(
      COCFLAG == 1 & COCMON == 1 ~ 1,
      COCFLAG == 1 & COCMON == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

data <- data %>%
  mutate(
    Y_METH = case_when(
      meth_ever & meth_month ~ 1,
      meth_ever & !meth_month ~ 0,
      TRUE ~ NA_real_
    )
  )
# ------------------------------------------------------------
# Meth - helper variables
# ------------------------------------------------------------

data <- data %>%
  mutate(
    meth_ever  = IRMETHAMREC %in% c(1, 2, 3),
    meth_month = IRMETHAMREC == 1
  )

# ------------------------------------------------------------
# Meth - outcome variable
# ------------------------------------------------------------

data <- data %>%
  mutate(
    Y_METH = case_when(
      meth_ever & meth_month  ~ 1,
      meth_ever & !meth_month ~ 0,
      TRUE ~ NA_real_
    )
  )
# =====================================================
# Employment Index
# Higher score = stronger labor market attachment
# =====================================================

data <- data %>%
  mutate(
    
    # Currently employed
    emp_current = case_when(
      IRWRKSTAT18 == 1 ~ 1,      # full-time
      IRWRKSTAT18 == 2 ~ 0.75,   # part-time
      IRWRKSTAT18 == 3 ~ 0.25,   # unemployed
      IRWRKSTAT18 == 4 ~ 0,      # not in labor force
      TRUE ~ NA_real_
    ),
    
    # Usually works 35+ hours
    emp_fulltime = case_when(
      WRK35WKUS == 1 ~ 1,
      WRK35WKUS == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Worked during the last 12 months
    emp_worked_year = case_when(
      WRKDPSTYR == 1 ~ 1,
      WRKDPSTYR == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Worked recently
    emp_recent = case_when(
      WRKLASTYR2 == 2024 ~ 1,
      WRKLASTYR2 == 2023 ~ 0.8,
      WRKLASTYR2 == 2022 ~ 0.6,
      WRKLASTYR2 == 2021 ~ 0.4,
      WRKLASTYR2 < 2021 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  
  mutate(
    EMPLOYMENT_INDEX =
      rowMeans(
        cbind(
          emp_current,
          emp_fulltime,
          emp_worked_year,
          emp_recent
        ),
        na.rm = TRUE
      )
  )
# =====================================================
# Economic Status Index
# Higher score = better economic status
# =====================================================

data <- data %>%
  mutate(
    
    income_norm =
      (IRFAMIN3 - 1) / (7 - 1),
    
    poverty_norm =
      (POVERTY3 - 1) / (3 - 1)
    
  ) %>%
  
  mutate(
    
    ECONOMIC_STATUS_INDEX =
      rowMeans(
        cbind(
          income_norm,
          poverty_norm
        ),
        na.rm = TRUE
      )
  )
# =====================================================
# Economic Assistance Index
# Higher score = more government / family assistance
# =====================================================

data <- data %>%
  mutate(
    
    assist_govt =
      ifelse(GOVTPROG == 1, 1, 0),
    
    assist_ssi =
      ifelse(IRFAMSSI == 1, 1, 0),
    
    assist_payment =
      ifelse(IRFAMPMT == 1, 1, 0),
    
    assist_social =
      ifelse(IRFAMSOC == 1, 1, 0),
    
    assist_services =
      ifelse(IRFAMSVC == 1, 1, 0)
  ) %>%
  
  mutate(
    
    ECONOMIC_ASSISTANCE_INDEX =
      rowMeans(
        cbind(
          assist_govt,
          assist_ssi,
          assist_payment,
          assist_social,
          assist_services
        ),
        na.rm = TRUE
      )
  )
# =====================================================
# Mental Health Index
# Higher score = greater mental health burden
# =====================================================

data <- data %>%
  mutate(
    
    treatment_history = case_when(
      MHTRTPY2 == 1 ~ 1,
      MHTRTPY2 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    distress_norm =
      (KSSLR6MONED - min(KSSLR6MONED, na.rm = TRUE)) /
      (max(KSSLR6MONED, na.rm = TRUE) -
         min(KSSLR6MONED, na.rm = TRUE)),
    
    anxiety_norm =
      (AGADTOTSC - min(AGADTOTSC, na.rm = TRUE)) /
      (max(AGADTOTSC, na.rm = TRUE) -
         min(AGADTOTSC, na.rm = TRUE))
  ) %>%
  
  mutate(
    
    MENTAL_HEALTH_INDEX =
      rowMeans(
        cbind(
          distress_norm,
          anxiety_norm,
          treatment_history
        ),
        na.rm = TRUE
      )
  )
