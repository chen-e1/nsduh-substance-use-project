# ==========================================================
# NSDUH Data Cleaning Pipeline
# Final reproducible cleaning script
# ==========================================================

# This script combines:
# 1. Reconstruction / initial cleaning
# 2. Variable reduction for the final project dataset
#
# Output:
# nsduh_project_final_reduced.csv
# ==========================================================

# NSDUH – Reconstructed Cleaning Pipeline

library(dplyr)

df_clean <- df

# Adults only (18+)
if ("CATAG3" %in% names(df_clean)) {
  df_clean <- df_clean %>%
    filter(CATAG3 != 1)
}

# Remove military / combat variables
military_cols <- grep("MIL|ARM|VET|COMBAT|DEPLOY|ACTIVE|RESERVE|GUARD|SERVICE",
                      names(df_clean), value=TRUE, ignore.case=TRUE)
df_clean <- df_clean %>% select(-any_of(military_cols))

# Remove tobacco / nicotine
tobacco_cols <- grep("TOB|NIC|CIG|VAP|SMK",
                     names(df_clean), value=TRUE)
df_clean <- df_clean %>% select(-any_of(tobacco_cols))

# Remove prescription drug families
rx_cols <- grep("PNR|TRQ|SED|STM|OXC|RXOP|FENT|BZO",
                names(df_clean), value=TRUE)
df_clean <- df_clean %>% select(-any_of(rx_cols))

# Remove youth-only variables
youth_remove <- c(
"YESCHFLT","YESCHWRK","YESCHIMP","YESCHINT",
"YETCGJOB","YESCHACT",
"YOFAMDOC","YOSOCWRK","YOOTHHLP","YSDSWRK"
)
df_clean <- df_clean %>% select(-any_of(youth_remove))

# Remove detailed education/work variables
demo_remove <- c(
"EDUHIGHCAT","ENRLCOLLFT2","ENRLCOLLST2","ANYEDUC3",
"WRKSTATWK2","WRKDPSTWK","WRKDHRSWK2","WRK35WKUS",
"WRKRSNNOT","WRKRSNJOB","WRKEFFORT","WRKDPSTYR",
"WRKNJBPYR","WRKNJBWKS","WRKSICKMO","WRKSKIPMO",
"IRWRKSTAT18"
)
df_clean <- df_clean %>% select(-any_of(demo_remove))

# Remove detailed personality variables
personality_cols <- grep("^IMP|^IRIMP|IMPRESP|IMPGOUT|IMPPEOP|IMPHHLD",
                         names(df_clean), value=TRUE)
df_clean <- df_clean %>% select(-any_of(personality_cols))

# Keep only WHODAS total score
whodas_remove <- c("WHODASSCED","WHODASDAED","WHODASDASC")
df_clean <- df_clean %>% select(-any_of(whodas_remove))

# Remove youth mental-health questionnaires
mh_youth <- grep("^YGAD|^YMDE", names(df_clean), value=TRUE)
df_clean <- df_clean %>% select(-any_of(mh_youth))

# Additional manual cleaning performed interactively:
# - Removed detailed alcohol diagnostic families
# - Removed treatment-detail families
# - Removed many UD variables for drugs outside study scope
# - Kept focus on cannabis, cocaine, heroin,
#   methamphetamine and hallucinogens
# - Removed many administrative and duplicate variables

cat("Rows:", nrow(df_clean), "\n")
cat("Columns:", ncol(df_clean), "\n")


# ==========================================================
# Load libraries
# ==========================================================

# ==========================================================
# Load libraries
# ==========================================================

library(tidyverse)

# ==========================================================
# Load data
# ==========================================================

data <- read.csv(file.choose())

# ==========================================================
# Create derived variables and indices
# ==========================================================

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

# ==========================================================
# Remove cannabis-related variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("MJ"),
    -starts_with("MR"),
    -starts_with("BLNT"),
    -starts_with("CBD"),
    -starts_with("MKM")
  )

# ==========================================================
# Remove LSD variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("LSD"),
    -starts_with("LSDFLAG"),
    -starts_with("LSDYR"),
    -starts_with("LSDMON")
  )

# ==========================================================
# Remove alcohol variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("ALC"),
    -starts_with("BNGDRK"),
    -starts_with("HVYDRK")
  )

# ==========================================================
# Remove hallucinogens and unrelated drug variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("HALL"),
    -starts_with("INH"),
    -starts_with("PIP")
  )

# Remove specific drug variables
data <- data %>%
  select(
    -contains("SALVIA"),
    -contains("KET")
  )

# ==========================================================
# Remove drug risk perception variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("RSK"),
    -starts_with("GRSK"),
    -starts_with("DIFGET"),
    -starts_with("DIFOBT"),
    -starts_with("APPDRG")
  )

# ==========================================================
# Remove youth questionnaire variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("YO"),
    -starts_with("YE"),
    -starts_with("PR"),
    -starts_with("PAR"),
    -starts_with("STND")
  )

# ==========================================================
# Remove physical health variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("BMI"),
    -starts_with("PREG"),
    -starts_with("HRTCOND"),
    -starts_with("CIRROS"),
    -starts_with("KIDNY")
  )

# ==========================================================
# Remove depression questionnaire variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("AD")
  )

# ==========================================================
# Remove treatment questionnaire variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("SUNT"),
    -starts_with("MHNT"),
    -starts_with("RCV")
  )

# ==========================================================
# Save reduced dataset
# ==========================================================

write.csv(
  data,
  "nsduh_project_reduced.csv",
  row.names = FALSE
)

# ==========================================================
# Remove Crack variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("CRK"),
    -starts_with("CRAK"),
    -starts_with("CRTOT"),
    -starts_with("CRFQ"),
    -starts_with("CRBST"),
    -starts_with("CRDAY"),
    -contains("CR30EST")
  )

# ==========================================================
# Remove route of administration / needle variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("GNND"),
    -starts_with("OTDG"),
    -starts_with("ANYNDL"),
    -starts_with("CHMNDL"),
    -starts_with("COCNEED"),
    -starts_with("HERNEED"),
    -starts_with("HERSNI"),
    -starts_with("HERSMO"),
    -starts_with("IRNPCOLD"),
    -starts_with("HRNDL"),
    -starts_with("CONDL"),
    -starts_with("HEOT")
  )

# ==========================================================
# Remove DSM diagnosis variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("PYUD"),
    -starts_with("IRPYUD"),
    -starts_with("IRPYSEV")
  )

# ==========================================================
# Remove survey summary variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("SVYR")
  )

# ==========================================================
# Remove detailed cocaine history
# ==========================================================

data <- data %>%
  select(
    -starts_with("CC"),
    -contains("COCAGE"),
    -contains("COCMFU"),
    -contains("COCREC"),
    -contains("COCYRTOT"),
    -contains("COCUS30A"),
    -contains("COCYDAYS"),
    -contains("COCMDAYS"),
    -contains("COCYLU"),
    -contains("COCMLU"),
    -contains("COCAGLST"),
    -contains("COCYRBFR")
  )

# ==========================================================
# Remove detailed heroin history
# ==========================================================

data <- data %>%
  select(
    -starts_with("HR"),
    -contains("HERAGE"),
    -contains("HERMFU"),
    -contains("HERREC"),
    -contains("HERYRTOT"),
    -contains("HER30USE"),
    -contains("HERYDAYS"),
    -contains("HERMDAYS"),
    -contains("HERYLU"),
    -contains("HERMLU"),
    -contains("HERAGLST")
  )

# ==========================================================
# Save reduced dataset
# ==========================================================

write.csv(
  data,
  "nsduh_project_reduced_v3.csv",
  row.names = FALSE
)

# ==========================================================
# Remove height / weight variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("HT"),
    -starts_with("WT")
  )

# ==========================================================
# Remove general health variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("HLT"),
    -starts_with("HLC"),
    -starts_with("HLN"),
    -starts_with("HLL")
  )
# ==========================================================
# Remove fentanyl and prescription misuse variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("FENT"),
    -starts_with("TQSD"),
    -starts_with("PSY"),
    -starts_with("CNS"),
    -starts_with("OXY"),
    -starts_with("NOOPPR")
  )

# ==========================================================
# Remove illicit drug summary variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("ILL"),
    -starts_with("CDUFLAG")
  )

# ==========================================================
# Remove inhalants / solvents / aerosols
# ==========================================================

data <- data %>%
  select(
    -starts_with("SOLVE"),
    -starts_with("NITOXI"),
    -starts_with("FLTMRK"),
    -starts_with("SPAINT"),
    -starts_with("AIRDU"),
    -starts_with("OTHAER")
  )

# ==========================================================
# Remove nicotine / dependence variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("NDSS"),
    -starts_with("FTND")
  )

# ==========================================================
# Remove miscellaneous drug summary variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("CG30"),
    -starts_with("CI30"),
    -starts_with("MXMJ")
  )

# ==========================================================
# Remove Kratom variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("KRAT"),
    -starts_with("IRKRAT")
  )
# ==========================================================
# Remove general health variables
# ==========================================================

data <- data %>%
  select(
    -HEALTH,
    -HEALTH2
  )

# ==========================================================
# Remove disability variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("LVLDIF")
  )

# ==========================================================
# Remove English language variable
# ==========================================================

data <- data %>%
  select(
    -SPEAKENGL
  )

# ==========================================================
# Remove residential mobility
# ==========================================================

data <- data %>%
  select(
    -MOVSINPYR2
  )

# ==========================================================
# Remove college enrollment variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("ENRLCOLL")
  )

# ==========================================================
# Remove household size variables
# ==========================================================

data <- data %>%
  select(
    -IRHHSIZ2,
    -IRHH65_2
  )

# ==========================================================
# Remove parents variables
# ==========================================================

data <- data %>%
  select(
    -EDFAM18,
    -IMOTHER,
    -IFATHER
  )

# ==========================================================
# Remove combined sex-race variable
# ==========================================================

data <- data %>%
  select(
    -SEXRACE
  )

# ==========================================================
# Keep only general criminal history
# ==========================================================

data <- data %>%
  select(
    -NOBOOKY2,
    -starts_with("BK")
  )

# ==========================================================
# Remove remaining cannabis-specific variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("CAB"),
    -starts_with("CAF"),
    -starts_with("EIB"),
    -MEDMJPA2,
    -BLRECFL2,
    -RSNOMRJ,
    -RSNMRJMO
  )

# ==========================================================
# Remove hallucinogen variables
# ==========================================================

data <- data %>%
  select(
    -HALTOTFG,
    -HALFQFLG,
    -IRPIPLF,
    -IRPIPMN,
    -IRCBDHMPREC,
    -IRHALLUCREC,
    -IRPCPRC,
    -IRPSILCYREC,
    -IRHALLUCYFQ,
    -IRHALLUCAGE,
    -IRPCPAGE
  )

# ==========================================================
# Remove OTC flag
# ==========================================================

data <- data %>%
  select(
    -OTCFLAG
  )

# ==========================================================
# Remove youth / school / religion variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("SNY"),
    -starts_with("SNA"),
    -starts_with("SNR"),
    -starts_with("YUSU"),
    -SCHFELT,
    -TCHGJOB,
    -AVGGRADE,
    -ARGUPAR,
    -TALKPROB,
    -YTHACT2,
    -RLGIMPT,
    -RLGDCSN,
    -RLGFRND
  )

# ==========================================================
# Remove raw depression / impairment questions
# ==========================================================

data <- data %>%
  select(
    -starts_with("DST"),
    -starts_with("IRDST"),
    -starts_with("IMP"),
    -starts_with("IRIMP"),
    -starts_with("IRYGAD"),
    -starts_with("IRAGAD"),
    -starts_with("AMDE"),
    -starts_with("IRAMDE"),
    -starts_with("ASDS"),
    -starts_with("ATXMDE"),
    -starts_with("ARXMDE"),
    -starts_with("AOMD"),
    -starts_with("APSY"),
    -starts_with("ASOC"),
    -starts_with("ACOUN"),
    -starts_with("AOMHM"),
    -starts_with("ANURS"),
    -starts_with("AREL"),
    -starts_with("AHBC"),
    -starts_with("AHLT"),
    -starts_with("AALT")
  )

# ==========================================================
# Remove detailed youth mental health variables
# ==========================================================

data <- data %>%
  select(
    -starts_with("YMI"),
    -starts_with("YMS"),
    -starts_with("YTX"),
    -starts_with("YRX"),
    -starts_with("YDOC"),
    -starts_with("YPSY"),
    -starts_with("YSOC"),
    -starts_with("YCOUN"),
    -starts_with("YNURS"),
    -starts_with("YREL"),
    -starts_with("YHB"),
    -starts_with("YALT"),
    -starts_with("YSDS"),
    -starts_with("YMIM"),
    -MDEIMPY
  )

# ==========================================================
# Remove detailed substance use disorder questions
# ==========================================================

data <- data %>%
  select(
    -starts_with("UDAL"),
    -starts_with("UDMJ"),
    -starts_with("UDHA")
  )

# ==========================================================
# Save reduced dataset
# ==========================================================

write.csv(
  data,
  "nsduh_project_reduced_v5.csv",
  row.names = FALSE
)
