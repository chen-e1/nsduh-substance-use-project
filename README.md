# NSDUH Substance Use Project

## Project Overview

This project analyzes socioeconomic, demographic, and mental-health factors associated with active past-month use of heroin, cocaine, and methamphetamine among adults with a lifetime history of use.

The analysis is based on data from the National Survey on Drug Use and Health (NSDUH). The project focuses on adults aged 18 and older who reported lifetime use of heroin, cocaine, or methamphetamine.

The main goal is to identify broad and consistent patterns associated with active past-month use among lifetime users, rather than to make causal claims or provide precise individual-level prediction.

---

## Research Question

Which socioeconomic factors are associated with active past-month use among individuals with a lifetime history of heroin, cocaine, or methamphetamine use?

In this project, active past-month use refers to self-reported use in the past 30 days among respondents who also reported lifetime use of the relevant substance.

---

## Data Source

The project uses data from the National Survey on Drug Use and Health (NSDUH).

The broad analytic sample includes adults aged 18 and older with a lifetime history of heroin, cocaine, or methamphetamine use. The broad analytic sample contains 7,326 respondents.

Because each model uses a different substance-specific outcome and complete-case filtering, the final sample size varies across model specifications.

The raw NSDUH data are not stored directly in this repository. To reproduce the analysis, place the cleaned dataset in the following path:

```text
data/nsduh_clean_with_indexes_geo_id.csv
```

Additional information about the data file is available in:

```text
data/README_data.md
```

---

## Outcome Definition

The main outcome variable measures active past-month use among lifetime users.

For each relevant substance-specific model:

```text
Y = 1
```

The respondent reported lifetime use of the relevant substance and also reported use in the past 30 days.

```text
Y = 0
```

The respondent reported lifetime use of the relevant substance but did not report use in the past 30 days.

Respondents without lifetime use of the relevant substance were not included in that substance-specific outcome.

Importantly, this outcome does not directly measure relapse, treatment failure, or cessation. It captures active past-month use among individuals with a lifetime history of use.

---

## Data Cleaning and Feature Engineering

The data cleaning process included:

* Restricting the sample to adults aged 18 and older.
* Identifying respondents with lifetime heroin, cocaine, or methamphetamine use.
* Creating past-month active-use outcome variables.
* Recoding demographic and socioeconomic variables.
* Constructing composite measures for economic status, employment, government assistance, and mental-health vulnerability.
* Preparing cleaned variables for logistic regression, hierarchical modeling, and Random Forest classification.

The main data-cleaning script is:

```text
scripts/01_NSDUH_Final_Cleaning.R
```

---

## Modeling Strategy

The project used three main modeling approaches.

### 1. Stepwise Logistic Regression

Stepwise logistic regression was used to identify which predictors were retained in the final logistic models for different substance-use outcomes.

This approach provides interpretable statistical results, including direction of association, significance, and odds-ratio-based interpretation.

Relevant scripts:

```text
scripts/02_stepwise_regression_model.R
scripts/03_stepwise_separation_drugs.R
```

---

### 2. Hierarchical Logistic Regression

Hierarchical logistic regression was used as an additional structured modeling approach. Predictors were entered in theoretical blocks:

1. Substance-use profile
2. Socioeconomic variables
3. Demographic variables
4. Mental-health variables
5. Treatment and legal-history variables

This approach allowed us to examine whether each block of variables added explanatory value beyond previously included predictors.

Relevant script:

```text
scripts/04_hierarchical_logistic_models.R
```

Exported result tables from this analysis are saved in:

```text
outputs/hierarchical_logistic/
```

---

### 3. Random Forest

Random Forest models were used as a complementary classification-oriented approach.

Unlike logistic regression, Random Forest does not provide p-values or odds ratios. Instead, it ranks variables according to their contribution to classification performance.

Random Forest was used to examine whether the same broad variables identified in the logistic models also appeared as important for classification.

Relevant script:

```text
scripts/05_random_forest_models.Rmd
```

Supplementary substance-specific Random Forest models may be included separately if retained in the final repository.

---

## Model Evaluation

Model performance was evaluated using several metrics, including:

* Accuracy
* Balanced Accuracy
* Sensitivity / Recall
* Specificity
* Precision
* F1-score
* ROC-AUC
* PR-AUC

Because the outcome was imbalanced, the analysis did not rely only on accuracy. Precision, recall, F1-score, and PR-AUC were especially important for evaluating how well the models identified the positive class: respondents with active past-month use.

Overall, model evaluation indicated moderate classification performance. Therefore, the models are interpreted as tools for identifying broad population-level patterns rather than as precise individual-level prediction tools.

---

## Main Findings

Across modeling approaches, several patterns appeared consistently:

* Mental-health vulnerability was one of the most stable and informative factors across models.
* Socioeconomic factors, including economic status, employment, and education, were repeatedly associated with active past-month use.
* Some variables, such as health insurance and government assistance, appeared more substance-specific.
* Random Forest results broadly supported the logistic regression findings by ranking mental health and socioeconomic variables as important for classification.

Overall, the findings suggest that active past-month use among lifetime users is associated with a combination of mental-health, socioeconomic, and substance-use-profile factors.

These findings should be interpreted as associations, not causal effects.

---

## Repository Structure

```text
.
├── README.md
├── data/
│   └── README_data.md
├── scripts/
│   ├── 01_NSDUH_Final_Cleaning.R
│   ├── 02_stepwise_regression_model.R
│   ├── 03_stepwise_separation_drugs.R
│   ├── 04_hierarchical_logistic_models.R
│   └── 05_random_forest_models.Rmd
├── outputs/
│   └── hierarchical_logistic/
├── figures/
├── appendices/
└── report/
```

### Folder Descriptions

```text
data/
```

Contains a README explaining where the cleaned dataset should be placed. The dataset itself is not stored directly in the repository.

```text
scripts/
```

Contains the R and R Markdown files used for data cleaning, modeling, and evaluation.

```text
outputs/
```

Contains exported result tables, mainly from the hierarchical logistic regression analysis.

```text
figures/
```

Contains final figures used in the report, presentation, or appendices.

```text
appendices/
```

Contains supplementary project materials and extended model summaries.

```text
report/
```

Contains the final project report.

---

## How to Reproduce the Analysis

1. Clone or download this repository.

2. Place the cleaned NSDUH dataset in the following path:

```text
data/nsduh_clean_with_indexes_geo_id.csv
```

3. Run the data-cleaning script:

```text
scripts/01_NSDUH_Final_Cleaning.R
```

4. Run the modeling scripts:

```text
scripts/02_stepwise_regression_model.R
scripts/03_stepwise_separation_drugs.R
scripts/04_hierarchical_logistic_models.R
```

5. Knit or run the Random Forest R Markdown file:

```text
scripts/05_random_forest_models.Rmd
```

6. Review exported results in:

```text
outputs/hierarchical_logistic/
```

---

## Required R Packages

The analysis uses the following R packages:

```r
tidyverse
dplyr
ggplot2
caret
pROC
PRROC
randomForest
ResourceSelection
pscl
broom
knitr
rmarkdown
```

Depending on the local R environment, additional dependencies may be installed automatically.

---

## Limitations

This project has several limitations.

First, the outcome is based on lifetime-use and past-month-use reports. It does not directly measure cessation, relapse, or treatment outcomes.

Second, NSDUH is cross-sectional, so the analysis can identify associations but cannot establish causal relationships.

Third, the data are self-reported and may be affected by reporting bias.

Finally, some high-risk populations are not fully represented in NSDUH, so the findings should be generalized with caution.

---

## Authors

This project was completed as part of the CIS2601 final project.

Team members:

```text
Yali Gamliel, Lee Mizrahi, Chen Einy, Bar Hana Yehezkel
```

---

## Links

Code repository:

```text
https://github.com/chen-e1/nsduh-substance-use-project/tree/main
```

Data folder:

```text
https://github.com/chen-e1/nsduh-substance-use-project/tree/main/data
```
