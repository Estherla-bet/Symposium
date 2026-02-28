#Estherla Twene
#Advanced Data Analysis
#Loading libraries
library(haven)    
library(tidyverse) 
library(mice)     
library(broom)
library(dplyr)
library(gtsummary)
library(flextable)
library(officer)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(car)
install.packages("ResourceSelection")
library(ResourceSelection)

#Setting working directory
getwd()
setwd("/Users/estherlatwene/Desktop/class1_survey_project")

#Loading dataset
haalsi <- read_csv("1ADA_DATA.csv")


#Checking variables
dim(haalsi)           
names(haalsi)[1:40] 

#Checking variable names
names(haalsi)


#Creating the outcome variable
haalsi <- haalsi %>%
  mutate(
    W2C_PF_CADL_num = as.numeric(W2C_PF_CADL),
    func_limit = case_when(
      W2C_PF_CADL_num == 1 ~ 1L,   # has limitation
      W2C_PF_CADL_num == 0 ~ 0L,   # no limitation
      W2C_PF_CADL_num %in% c(-99, -98, -97) ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    
    func_limit = factor(
      func_limit,
      levels = c(0, 1),
      labels = c("No limitation", "Has limitation")
    )
  )

#Checking the outcome variable
table(haalsi$func_limit, useNA = "ifany")

# 419 study participants have functional limitation,3717 no functional limitation and 923 with missing values(NA)

#Creating the exposure variables and defining diabetes
haalsi <- haalsi %>%
  mutate(
    diag_num = as.numeric(W2C_CM_DIABETES_SRDIAG),
    trt_num  = as.numeric(W2C_CM_DIABETES_SRTRT),
    
    diag_num = ifelse(diag_num %in% c(-99, -98, -97), NA, diag_num),
    trt_num  = ifelse(trt_num %in% c(-99, -98, -97), NA, trt_num),
    
    # Diabetes definition:
    # YES if ever diagnosed OR treated
    diabetes_num = case_when(
      diag_num == 1 | trt_num == 1 ~ 1L,  # any YES → Diabetes
      diag_num == 2 & trt_num == 2 ~ 0L,  # both NO → No diabetes
      TRUE ~ NA_integer_             # everything else → missing
    ),
    
    diabetes = factor(
      diabetes_num,
      levels = c(0, 1),
      labels = c("No diabetes", "Diabetes")
    )
  )

#Assessing the distribution of the diabetes variable
table(haalsi$diabetes, useNA = "ifany")
#There are 2,695 participants with no diabetes, 581 with diabetes and 1783 NA

#Covariates
##Renaming the covariates
haalsi <- haalsi %>%
  rename(
    sex        = W2C_RSEX,
    educ       = W1C_BD_EDUC4,
    wealth     = W2C_WEALTHINDEX,
    bmi_cat    = W2C_BS_BMICAT,
    marital    = W2C_BD_MAR,
    smoke      = W2_C_CURRENT_SMOKE,
    hyper      = W2_C_EVER_HYPER,
    stroke     = W2_C_STROKE,
    angina     = W3C_EVER_ANGINA
  )


haalsi <- haalsi %>%
  mutate(
    sex     = as_factor(sex),
    bmi_cat = as_factor(bmi_cat),
    marital = as_factor(marital),
    smoke   = as_factor(smoke),
  )


# Adding age variable
haalsi <- haalsi %>%
  mutate(
    age_years = as.numeric(W2C_RAGE_CALC)
  )

# Age distribution
summary(haalsi$age_years)

# Recoding age into groups 10-year gaps, 40-49, 40-59, 60-69, 70-79 and 80+
haalsi <- haalsi %>%
  mutate(
    age_group = cut(
      age_years,
      breaks = c(40, 50, 60, 70, 80, Inf),   # 40–49, 50–59, 60–69, 70–79, 80+
      labels = c("40–49", "50–59", "60–69", "70–79", "80+"),
      right = FALSE
    )
  )

# Checking age group distribution
table(haalsi$age_group, useNA = "ifany")

# Wealth quintiles
haalsi <- haalsi %>%
  mutate(
    wealth = factor(wealth,
                    levels = 1:5,
                    labels = c("Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)"))
  )

# Recode alcohol (W2CM069: 1 = Yes, 2 = No, -97/-98/-99 = missing)
haalsi <- haalsi %>%
  mutate(
    alcohol_num = as.numeric(W2CM069),
    alcohol_num = ifelse(alcohol_num %in% c(-99, -98, -97), NA, alcohol_num),
    alcohol = factor(
      alcohol_num,
      levels = c(2, 1),          # 2 = No, 1 = Yes
      labels = c("No", "Yes")
    )
  )

table(haalsi$alcohol, useNA = "ifany")

#Recoding educational
haalsi <- haalsi %>%
  mutate(
    # numeric versions (keep these for models if you want)
    hyper_num  = as.numeric(hyper),
    stroke_num = as.numeric(stroke),
    angina_num = as.numeric(angina),
    
    # set special missing codes to NA
    hyper_num  = ifelse(hyper_num  %in% c(-97, -98, -99), NA, hyper_num),
    stroke_num = ifelse(stroke_num %in% c(-97, -98, -99), NA, stroke_num),
    angina_num = ifelse(angina_num %in% c(-97, -98, -99), NA, angina_num),
    
    # recode to 0/1: here I’m assuming 1 = Yes, 2 = No
    hyper_num  = dplyr::case_when(
      hyper_num == 1 ~ 1L,
      hyper_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    stroke_num = dplyr::case_when(
      stroke_num == 1 ~ 1L,
      stroke_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    angina_num = dplyr::case_when(
      angina_num == 1 ~ 1L,
      angina_num == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # labelled factors for descriptives and models
    hyper  = factor(hyper_num,
                    levels = c(0, 1),
                    labels = c("No hypertension", "Hypertension")),
    stroke = factor(stroke_num,
                    levels = c(0, 1),
                    labels = c("No stroke", "Stroke")),
    angina = factor(angina_num,
                    levels = c(0, 1),
                    labels = c("No angina", "Angina"))
  )

table(haalsi$hyper, useNA="ifany")
table(haalsi$stroke, useNA="ifany")
table(haalsi$angina, useNA="ifany")


# Recoding variable labels in dataset
# Sex
haalsi$sex <- fct_recode(
  haalsi$sex,
  "Male"   = "1",
  "Female" = "2"
)

# BMI
haalsi$bmi_cat <- fct_recode(
  haalsi$bmi_cat,
  "Underweight" = "0",
  "Normal"      = "1",
  "Overweight"  = "2",
  "Obese"       = "3"
)

# Marital Status
haalsi$marital <- fct_recode(
  haalsi$marital,
  "Never married"      = "0",
  "Married"            = "1",
  "Separated/Divorced" = "2",
  "Widowed"            = "3"
)

# Smoking
haalsi$smoke <- fct_recode(
  haalsi$smoke,
  "Yes" = "1",
  "No"  = "2"
)

# Alcohol
haalsi$alcohol <- fct_recode(
  haalsi$alcohol,
  "No"  = "No",
  "Yes" = "Yes"
)


#Re-coding educational variable
haalsi <- haalsi %>%
  mutate(
    educ_num = as.numeric(educ),
    educ = case_when(
      educ_num == 1 ~ "No formal education",
      educ_num == 2 ~ "Some primary (1–7 years)",
      educ_num == 3 ~ "Some secondary (8–11 years)",
      educ_num == 4 ~ "Secondary or more (12+ years)",
      educ_num %in% c(-97, -98, -99) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    educ = factor(
      educ,
      levels = c(
        "No formal education",
        "Some primary (1–7 years)",
        "Some secondary (8–11 years)",
        "Secondary or more (12+ years)"))
  )

table(haalsi$educ, useNA="ifany")
table(haalsi$educ, haalsi$diabetes, useNA="ifany")


#Variables to be included in MICE
log_vars <- c(
  "func_limit", "diabetes",
  "sex", "educ", "wealth",
  "bmi_cat", "marital",
  "smoke", "alcohol",
  "hyper", "stroke", "angina", "age_group"
)


mice_vars <- c(
  "func_limit", "diabetes",
  "sex", "wealth",
  "bmi_cat", "marital",
  "smoke", "alcohol", "age_group"
)

#Assessing missingness
# Create data set for MICE
haalsi_mice <- haalsi[, mice_vars]

#Running multiple imputation
imp <- mice(haalsi_mice, m = 10, seed = 123)

# Descriptive statistics
table1_diabetes <- haalsi %>%
  filter(!is.na(diabetes)) %>% 
  tbl_summary(
    by = diabetes,
    include = c(
      func_limit,
      age_group,
      sex, educ, wealth,
      bmi_cat, marital,
      smoke, alcohol,
      hyper, stroke, angina
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    label = list(
      func_limit ~ "Functional limitation",
      age_group ~ "Age group",
      sex ~ "Sex",
      educ ~ "Education level",
      wealth ~ "Wealth quintile",
      bmi_cat ~ "BMI category",
      marital ~ "Marital status",
      smoke ~ "Smoking status",
      alcohol ~ "Alcohol use"
    )
  ) %>%
  modify_caption("Table 1. Participant Characteristics by Diabetes Status") %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

table1_diabetes

table1_flex <- as_flex_table(table1_diabetes)

doc <- read_docx()
doc <- body_add_par(doc, "Table 1. Participant Characteristics by Diabetes Status", style = "heading 1")
doc <- body_add_flextable(doc, table1_flex)
print(doc, target = "Table1_Descriptives_By_Diabetes.docx")

#Checking assumptions
#Multicolinearity
cc <- haalsi %>%
  select(func_limit, diabetes, sex, wealth, bmi_cat, marital,
         smoke, alcohol, age_group) %>%
  na.omit()

model_check <- glm(
  func_limit ~ diabetes + sex + wealth + bmi_cat +
    marital + smoke + alcohol + age_group,
  data = cc,
  family = binomial
)


vif(model_check)
## Variance inflation factors (VIFs) were examined to assess multi-collinearity among predictors.No multi-collinearity

#Goodness of fit
cc$func_limit_num <- ifelse(cc$func_limit == "Has limitation", 1, 0)
model_check <- glm(
  func_limit_num ~ diabetes + sex + wealth + bmi_cat +
    marital + smoke + alcohol + age_group,
  data = cc,
  family = binomial
)

hoslem.test(cc$func_limit_num, fitted(model_check), g = 10)
##p-value >0.05, indication good model fit


#Un-adjusted model with only diabetes and functional limitation
logistic_unadj <- with(
  imp,
  glm(func_limit ~ diabetes, family = binomial)
)

pooled_unadj <- pool(logistic_unadj)

unadj_sum <- summary(pooled_unadj, conf.int = TRUE, small = TRUE)

APA_unadj <- unadj_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_unadj

# Logistic regression, adjusting for confounders
logistic_mi <- with(
  imp,
  glm(
    func_limit ~ diabetes + sex + wealth +
      bmi_cat + marital + smoke + alcohol + age_group ,
    family = binomial
  )
)

pooled_results <- pool(logistic_mi)


# Pooled summary with CIs and p-values
adj_sum <- summary(pooled_results, conf.int = TRUE, small = TRUE)

# APA-style adjusted results
APA_adj <- adj_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_adj


#Assessing EMM by sex (Male and Female)
# Diabetes × Sex Interaction
logistic_int_sex <- with(
  imp,
  glm(
    func_limit ~ diabetes * sex +
      age_group + wealth + bmi_cat + marital +
      smoke + alcohol,
    family = binomial
  )
)

pooled_int_sex <- pool(logistic_int_sex)

int_sex_sum <- summary(pooled_int_sex, conf.int = TRUE, small = TRUE)

APA_int_sex <- int_sex_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

APA_int_sex


#Want clean labels 
clean_labels <- function(table) {
  table %>%
    mutate(
      term_chr = as.character(term),
      
      Variable = dplyr::case_when(
        # Diabetes
        term_chr == "diabetesDiabetes" ~ "Diabetes (yes)",
        
        # Sex
        term_chr == "sex2" ~ "Female",
        
        # BMI
        term_chr == "bmi_cat0" ~ "Underweight (BMI)",
        term_chr == "bmi_cat1" ~ "Normal BMI",
        term_chr == "bmi_cat2" ~ "Overweight (BMI)",
        term_chr == "bmi_cat3" ~ "Obese (BMI)",
        
        # Marital status
        term_chr == "marital0" ~ "Never married",
        term_chr == "marital1" ~ "Separated/divorced",
        term_chr == "marital2" ~ "Widowed",
        term_chr == "marital3" ~ "Currently married",
        
        # Smoking
        term_chr == "smoke1" ~ "Current smoker",
        
        # Alcohol
        term_chr == "alcoholYes" ~ "Alcohol use (yes)",
        
        # Age groups
        term_chr == "age_group40–49" ~ "Age 40–49",
        term_chr == "age_group50–59" ~ "Age 50–59",
        term_chr == "age_group60–69" ~ "Age 60–69",
        term_chr == "age_group70–79" ~ "Age 70–79",
        term_chr == "age_group80+"   ~ "Age 80+",
        
        # Interaction terms (Sex)
        term_chr == "diabetesDiabetes:sex2"           ~ "Diabetes × Female",
        
        TRUE ~ term_chr
      )
    ) %>%
    select(Variable, OR, lower_CI, upper_CI, p_value)
}



##Re-running my models after cleaning labels
clean_APA_adj <- clean_labels(APA_adj)
clean_APA_adj

clean_APA_sex <- clean_labels(APA_int_sex)
clean_APA_sex


#Creating table 2
table2_logistic <- clean_APA_adj %>%
  mutate(
    OR = sprintf("%.2f", OR),
    CI = sprintf("%.2f–%.2f", lower_CI, upper_CI),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(Variable, OR, CI, p_value)


table2_flex <- flextable(table2_logistic) %>%
  autofit() %>%
  bold(part = "header") %>%        # bold header
  align(align = "center", part = "all") %>% 
  set_caption("Table 2. Adjusted Logistic Regression of Diabetes and Functional Limitation")

doc <- read_docx()
doc <- body_add_par(doc, "Table 2. Adjusted Logistic Regression of Diabetes and Functional Limitation", style = "heading 1")
doc <- body_add_flextable(doc, table2_flex)
print(doc, target = "Table2_Logistic_Regression.docx")


# UNADJUSTED
# Men
logistic_unadj_male <- with(
  imp,
  glm(
    func_limit ~ diabetes,
    family = binomial,
    subset = sex == "Male"
  )
)
pooled_unadj_male <- pool(logistic_unadj_male)

unadj_male_sum <- summary(pooled_unadj_male, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Male",
    Model    = "Unadjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Women
logistic_unadj_female <- with(
  imp,
  glm(
    func_limit ~ diabetes,
    family = binomial,
    subset = sex == "Female"
  )
)
pooled_unadj_female <- pool(logistic_unadj_female)

unadj_female_sum <- summary(pooled_unadj_female, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Female",
    Model    = "Unadjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# ADJUSTED

# Men
logistic_adj_male <- with(
  imp,
  glm(
    func_limit ~ diabetes + wealth + bmi_cat + marital +
      smoke + alcohol + age_group,
    family = binomial,
    subset = sex == "Male"
  )
)
pooled_adj_male <- pool(logistic_adj_male)

adj_male_sum <- summary(pooled_adj_male, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Male",
    Model    = "Adjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Women
logistic_adj_female <- with(
  imp,
  glm(
    func_limit ~ diabetes + wealth + bmi_cat + marital +
      smoke + alcohol + age_group,
    family = binomial,
    subset = sex == "Female"
  )
)
pooled_adj_female <- pool(logistic_adj_female)

adj_female_sum <- summary(pooled_adj_female, conf.int = TRUE, small = TRUE) %>%
  dplyr::filter(term == "diabetesDiabetes") %>%
  dplyr::mutate(
    Sex      = "Female",
    Model    = "Adjusted",
    OR       = exp(estimate),
    lower_CI = exp(`2.5 %`),
    upper_CI = exp(`97.5 %`),
    p_value  = p.value
  ) %>%
  dplyr::select(Sex, Model, OR, lower_CI, upper_CI, p_value)

# Combine all four rows
emm_by_sex <- bind_rows(
  unadj_male_sum,
  unadj_female_sum,
  adj_male_sum,
  adj_female_sum
)

emm_by_sex_formatted <- emm_by_sex %>%
  dplyr::mutate(
    OR      = sprintf("%.2f", OR),
    CI      = sprintf("%.2f–%.2f", lower_CI, upper_CI),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  dplyr::select(Sex, Model, OR, CI, p_value)

emm_by_sex_formatted

emm_sex_OR_flex <- flextable(emm_by_sex_formatted) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table X. Unadjusted and Adjusted Association Between Diabetes and Functional Limitation, Stratified by Sex")

doc <- read_docx()
doc <- body_add_par(doc, "Table X. Diabetes and Functional Limitation by Sex", style = "heading 1")
doc <- body_add_flextable(doc, emm_sex_OR_flex)

print(doc, target = "TableX_Diabetes_Sex_Stratified.docx")


# Assessing Effect Measure Modification (EMM) by Sex using interaction terms
emm_flex <- flextable(emm_by_sex_formatted) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table 3. Effect Measure Modification (EMM) by Sex for the Association Between Diabetes and Functional Limitation")

doc <- read_docx()
doc <- body_add_par(doc, "Table 3. Effect Measure Modification (EMM) by Sex", style = "heading 1")
doc <- body_add_flextable(doc, emm_flex)

print(doc, target = "Table3_EMM_Sex.docx")

#Flow chart. CONSORT-style flowchart for HAALSI analytic sample
figure1 <- grViz("
digraph flowchart {

  graph [layout = dot, rankdir = TB]

  node [fontname = Helvetica, shape = rectangle, fontsize = 15]

  # nodes
  node1 [label = '@@1']
  node2 [label = '@@2']
  node3 [label = '@@3']

  # flow
  node1 -> node2 -> node3
}

[1]: 'HAALSI Wave 2 participants aged ≥40 years\\n n = 5,059'
[2]: 'Excluding 923 participants with missing data on functional limitation\\nAnalytic sample with functional limitation information\\n n = 4,136'
[3]: 'Excluding 860 participants with missing diabetes status\\nFinal analytic sample for descriptive and regression analyses\\n n = 3,276'
")

figure1

# Export to PDF as Figure 1
figure1 %>%
  DiagrammeRsvg::export_svg() %>%  # convert to SVG text
  charToRaw() %>%                  # convert to raw vector
  rsvg::rsvg_pdf("Figure1_Flowchart.pdf") # save as PDF







############################################################
# NEW EQUITY-FOCUSED QUESTION:
# Socioeconomic differences in functional aging, with and without diabetes
# SES measures: Wealth quintile (asset index) + Education
############################################################

# 0) IMPORTANT: include education in MICE data used for models
mice_vars_equity <- c(
  "func_limit", "diabetes",
  "sex", "age_group",
  "wealth", "educ",
  "bmi_cat", "marital",
  "smoke", "alcohol"
)

haalsi_mice2 <- haalsi[, mice_vars_equity]

# Run MICE again
imp2 <- mice(haalsi_mice2, m = 10, seed = 123)

############################################################
# 1) DESCRIPTIVES: Functional limitation by SES (wealth + education)
############################################################

# Table: participant characteristics by wealth quintile
table_by_wealth <- haalsi %>%
  filter(!is.na(wealth), !is.na(func_limit)) %>%
  tbl_summary(
    by = wealth,
    include = c(
      func_limit, diabetes, age_group, sex, educ,
      bmi_cat, marital, smoke, alcohol
    ),
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no",
    label = list(
      func_limit ~ "Functional limitation",
      diabetes ~ "Diabetes status",
      age_group ~ "Age group",
      sex ~ "Sex",
      educ ~ "Education level",
      bmi_cat ~ "BMI category",
      marital ~ "Marital status",
      smoke ~ "Smoking status",
      alcohol ~ "Alcohol use"
    )
  ) %>%
  modify_caption("Table A. Participant Characteristics by Wealth Quintile") %>%
  bold_labels()

table_by_wealth

# Export Table A
table_by_wealth_flex <- as_flex_table(table_by_wealth)
doc <- read_docx()
doc <- body_add_par(doc, "Table A. Participant Characteristics by Wealth Quintile", style = "heading 1")
doc <- body_add_flextable(doc, table_by_wealth_flex)
print(doc, target = "TableA_Descriptives_By_Wealth.docx")


############################################################
# 2) MAIN EQUITY MODELS
# (A) Wealth + diabetes + education as predictors of functional limitation
############################################################

# Model 1: SES + diabetes (adjusted)
model_equity_main <- with(
  imp2,
  glm(
    func_limit ~ diabetes + wealth + educ +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)

pooled_equity_main <- pool(model_equity_main)
equity_main_sum <- summary(pooled_equity_main, conf.int = TRUE, small = TRUE)

equity_main_APA <- equity_main_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

equity_main_APA


############################################################
# 3) EFFECT MODIFICATION (Equity question):
# Does diabetes association differ across WEALTH quintiles?
############################################################

model_int_wealth <- with(
  imp2,
  glm(
    func_limit ~ diabetes * wealth + educ +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)

pooled_int_wealth <- pool(model_int_wealth)
int_wealth_sum <- summary(pooled_int_wealth, conf.int = TRUE, small = TRUE)

int_wealth_APA <- int_wealth_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

int_wealth_APA


############################################################
# 4) OPTIONAL: Effect modification by EDUCATION (if you want)
############################################################

model_int_educ <- with(
  imp2,
  glm(
    func_limit ~ diabetes * educ + wealth +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)

pooled_int_educ <- pool(model_int_educ)
int_educ_sum <- summary(pooled_int_educ, conf.int = TRUE, small = TRUE)

int_educ_APA <- int_educ_sum %>%
  mutate(
    OR       = round(exp(estimate), 2),
    lower_CI = round(exp(`2.5 %`), 2),
    upper_CI = round(exp(`97.5 %`), 2),
    p_value  = p.value
  ) %>%
  select(term, OR, lower_CI, upper_CI, p_value)

int_educ_APA


############################################################
# 5) EXPORT a clean regression table for the equity main model
############################################################

# Convert to readable regression table (simple version)
equity_table <- equity_main_APA %>%
  mutate(
    OR = sprintf("%.2f", OR),
    CI = sprintf("%.2f–%.2f", lower_CI, upper_CI),
    p_value = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(term, OR, CI, p_value)

equity_table_flex <- flextable(equity_table) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table B. Equity-Focused Model: Diabetes, Wealth, Education and Functional Limitation")

doc <- read_docx()
doc <- body_add_par(doc, "Table B. Equity-Focused Model Results", style = "heading 1")
doc <- body_add_flextable(doc, equity_table_flex)
print(doc, target = "TableB_Equity_Model.docx")





############################################################
# EQUITY-FOCUSED ADD-ON (Recommended):
# 1) Diabetes-focused model (already done) + interpret SES as context
# 2) SES-focused model with WEALTH as exposure (avoid Table 2 fallacy)
# 3) Effect modification: Diabetes × Wealth (interaction)
# 4) Export Table C (wealth exposure) + Table D (interaction terms)
############################################################

# If you already created imp2 earlier (MICE including educ + wealth), skip this.
# Otherwise, create an imputation object that includes educ:
mice_vars_equity <- c(
  "func_limit", "diabetes",
  "sex", "age_group",
  "wealth", "educ",
  "bmi_cat", "marital",
  "smoke", "alcohol"
)

haalsi_mice2 <- haalsi[, mice_vars_equity]
imp2 <- mice(haalsi_mice2, m = 10, seed = 123)

############################################################
# Helper: format pooled model into OR (95% CI), p-value
############################################################
format_pooled_or <- function(pooled_obj, model_name) {
  s <- summary(pooled_obj, conf.int = TRUE, small = TRUE)
  s %>%
    mutate(
      Model = model_name,
      OR = exp(estimate),
      lower = exp(`2.5 %`),
      upper = exp(`97.5 %`),
      p = p.value
    ) %>%
    transmute(
      Model,
      term,
      OR = sprintf("%.2f", OR),
      CI = sprintf("%.2f–%.2f", lower, upper),
      p_value = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
    )
}

############################################################
# (A) SES-focused models: WEALTH as the primary exposure
############################################################

# Model C1: Wealth + Education + covariates (NO diabetes)
m_wealth_c1 <- with(
  imp2,
  glm(
    func_limit ~ wealth + educ +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)
p_wealth_c1 <- pool(m_wealth_c1)

# Model C2: Add diabetes (assess attenuation)
m_wealth_c2 <- with(
  imp2,
  glm(
    func_limit ~ wealth + educ + diabetes +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)
p_wealth_c2 <- pool(m_wealth_c2)

# Combine and pivot wide for a clean table
t_c1 <- format_pooled_or(p_wealth_c1, "Model 1: SES + covariates")
t_c2 <- format_pooled_or(p_wealth_c2, "Model 2: + diabetes")

tableC_long <- bind_rows(t_c1, t_c2)

tableC_wide <- tableC_long %>%
  tidyr::pivot_wider(
    names_from = Model,
    values_from = c(OR, CI, p_value)
  ) %>%
  arrange(term)

tableC_wide

# Export Table C to Word
ftC <- flextable(tableC_wide) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table C. Wealth (Asset Index) and Education as Predictors of Functional Limitation: Models With and Without Diabetes")

doc <- read_docx()
doc <- body_add_par(doc, "Table C. Wealth and Education Models (Wealth as Exposure)", style = "heading 1")
doc <- body_add_flextable(doc, ftC)
print(doc, target = "TableC_Wealth_Exposure_TwoModels.docx")

############################################################
# (B) Effect modification (Interaction): Diabetes × Wealth
############################################################

m_int_wealth <- with(
  imp2,
  glm(
    func_limit ~ diabetes * wealth + educ +
      sex + age_group +
      bmi_cat + marital + smoke + alcohol,
    family = binomial
  )
)

p_int_wealth <- pool(m_int_wealth)

# Full interaction output (all terms)
tableD_int <- format_pooled_or(p_int_wealth, "Diabetes × Wealth interaction")
tableD_int

# Export Table D to Word
ftD <- flextable(tableD_int) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table D. Interaction Model: Diabetes × Wealth (Adjusted for Education and Covariates)")

doc <- read_docx()
doc <- body_add_par(doc, "Table D. Diabetes × Wealth Interaction Model", style = "heading 1")
doc <- body_add_flextable(doc, ftD)
print(doc, target = "TableD_Diabetes_Wealth_Interaction.docx")

############################################################
# (Optional) GLOBAL TEST: Are the interaction terms jointly significant?
# Uses one completed dataset just for a quick Wald test (approximation).
# If you want a strict pooled global test, tell me and I’ll give that version.
############################################################
comp1 <- complete(imp2, 1)
fit_no_int <- glm(
  func_limit ~ diabetes + wealth + educ +
    sex + age_group + bmi_cat + marital + smoke + alcohol,
  data = comp1, family = binomial
)

fit_with_int <- glm(
  func_limit ~ diabetes * wealth + educ +
    sex + age_group + bmi_cat + marital + smoke + alcohol,
  data = comp1, family = binomial
)

car::Anova(fit_with_int, type = 3)  # shows Wald tests including interaction block
anova(fit_no_int, fit_with_int, test = "Chisq")  # LR test in one completed dataset

ftC <- flextable(tableC_wide) %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("Table C. Wealth (Asset Index) and Education as Predictors of Functional Limitation: Models With and Without Diabetes")




############################################################
# POSTER FIGURES: Bar charts + Forest plots
# Assumes you already ran MICE and have:
#   - imp  (your original mice object)
#   - pooled_results (from your adjusted diabetes model)
#   - p_int_wealth (from your diabetes*wealth interaction model, if you ran it)
############################################################

library(dplyr)
library(ggplot2)
library(mice)

# Colorblind-friendly palette (Okabe–Ito)
okabe_ito <- c(
  blue = "#0072B2",
  red  = "#D55E00"
)


# Make sure diabetes is a factor with expected labels
prev_df <- comp1 %>%
  filter(!is.na(func_limit), !is.na(diabetes)) %>%
  mutate(diabetes = factor(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(
    prevalence = mean(func_limit == "Has limitation") * 100,
    .groups = "drop"
  )

# Check the exact labels (important)
p_prev_labeled <- ggplot(prev_df,
                         aes(x = diabetes, y = prevalence, fill = diabetes)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c(
    "No diabetes" = "#0072B2",
    "Diabetes"    = "#D55E00"
  )) +
  geom_text(
    aes(label = sprintf("%.1f%%", prevalence)),
    vjust = -0.6,
    size = 6,
    fontface = "bold"
  ) +
  labs(
    x = NULL,
    y = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold")
  )

p_prev_labeled <- p_prev_labeled +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt")
  )

ggsave(
  "Fig1_FL_by_Diabetes_labeled.png",
  p_prev_labeled,
  width = 6,
  height = 5,
  dpi = 300
)

#Checking disease prevalence



dm_prev <- comp1 %>%
  filter(!is.na(diabetes)) %>%
  summarise(
    diabetes_prevalence = mean(diabetes == "Diabetes") * 100
  )

dm_prev

# Overall prevalence of functional limitation
fl_prev_overall <- comp1 %>%
  filter(!is.na(func_limit)) %>%
  summarise(
    prevalence = mean(func_limit == "Has limitation") * 100
  )

fl_prev_overall


dm_prev_overall <- comp1 %>%
  filter(!is.na(diabetes)) %>%
  summarise(
    prevalence = mean(diabetes == "Diabetes") * 100
  )

dm_prev_overall


fl_prev_by_dm <- comp1 %>%
  filter(!is.na(func_limit), !is.na(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(
    prevalence = mean(func_limit == "Has limitation") * 100,
    .groups = "drop"
  )

fl_prev_by_dm

############################################################
# FIGURE 2: Forest plot from NON-interaction adjusted model
# This should match diabetes aOR = 2.04 (1.50–2.79) if that's in pooled_results
############################################################

library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# pooled_results is from:
# logistic_mi <- with(imp2, glm(func_limit ~ diabetes + sex + wealth + educ + ...))
# pooled_results <- pool(logistic_mi)

# helper to extract pooled ORs
format_pooled_or <- function(pooled_obj, model_name = "Model") {
  s <- summary(pooled_obj, conf.int = TRUE, exponentiate = TRUE)
  s %>%
    transmute(
      Model = model_name,
      term  = term,
      OR    = estimate,
      LCL   = `2.5 %`,
      UCL   = `97.5 %`,
      p     = p.value
    )
}

plot_df <- format_pooled_or(pooled_results, "Adjusted") %>%
  filter(
    term %in% c("diabetesDiabetes") |
      str_detect(term, "^wealth") |
      str_detect(term, "^educ")
  ) %>%
  mutate(
    group = case_when(
      term == "diabetesDiabetes" ~ "Diabetes",
      str_detect(term, "^wealth") ~ "SES (Wealth)",
      str_detect(term, "^educ") ~ "SES (Education)",
      TRUE ~ "Other"
    ),
    label = case_when(
      term == "diabetesDiabetes" ~ "Diabetes (vs no diabetes)",
      
      term == "wealthQ2" ~ "Wealth Q2 (vs Q1 poorest)",
      term == "wealthQ3" ~ "Wealth Q3 (vs Q1 poorest)",
      term == "wealthQ4" ~ "Wealth Q4 (vs Q1 poorest)",
      term == "wealthQ5 (Richest)" ~ "Wealth Q5 richest (vs Q1 poorest)",
      
      term == "educSome primary (1–7 years)" ~ "Education: Some primary (vs none)",
      term == "educSome secondary (8–11 years)" ~ "Education: Some secondary (vs none)",
      term == "educSecondary or more (12+ years)" ~ "Education: ≥12 years (vs none)",
      
      TRUE ~ term
    )
  )

# order
plot_df <- plot_df %>%
  mutate(
    label = factor(
      label,
      levels = c(
        "Diabetes (vs no diabetes)",
        "Wealth Q2 (vs Q1 poorest)",
        "Wealth Q3 (vs Q1 poorest)",
        "Wealth Q4 (vs Q1 poorest)",
        "Wealth Q5 richest (vs Q1 poorest)",
        "Education: Some primary (vs none)",
        "Education: Some secondary (vs none)",
        "Education: ≥12 years (vs none)"
      )
    )
  )

# colors + shapes (Diabetes orange, Wealth blue circle, Education blue triangle)
cols <- c(
  "Diabetes" = "#D55E00",
  "SES (Wealth)" = "#0072B2",
  "SES (Education)" = "#0072B2"
)

shapes <- c(
  "Diabetes" = 16,
  "SES (Wealth)" = 16,
  "SES (Education)" = 17
)

p_forest <- ggplot(plot_df, aes(x = OR, y = fct_rev(label), color = group, shape = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.9, color = "black") +
  geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.18, linewidth = 1.1) +
  geom_point(size = 4) +
  scale_x_log10(limits = c(0.2, 4)) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = shapes) +
  labs(x = "Adjusted odds ratio (log scale)", y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_forest

ggsave("Fig2_Forest_DM_Wealth_Education_MAINMODEL.png",
       p_forest, width = 11, height = 6.5, dpi = 300)


############################################################
# Education-only model (complete case, NO imputation)
############################################################

cc_educ <- haalsi %>%
  filter(
    !is.na(func_limit),
    !is.na(diabetes),
    !is.na(educ),
    !is.na(sex),
    !is.na(age_group),
    !is.na(bmi_cat),
    !is.na(marital),
    !is.na(smoke),
    !is.na(alcohol)
  )

educ_model <- glm(
  func_limit ~ educ + diabetes +
    sex + age_group + bmi_cat + marital + smoke + alcohol,
  data = cc_educ,
  family = binomial
)

educ_df <- broom::tidy(educ_model, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(grepl("^educ", term)) %>%
  mutate(
    label = case_when(
      term == "educSome primary (1–7 years)" ~ "Education: Some primary (vs none)",
      term == "educSome secondary (8–11 years)" ~ "Education: Some secondary (vs none)",
      term == "educSecondary or more (12+ years)" ~ "Education: ≥12 years (vs none)",
      TRUE ~ term
    )
  )

p_educ <- ggplot(educ_df, aes(x = estimate, y = fct_rev(label))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.18) +
  geom_point(size = 4, color = "#0072B2") +
  scale_x_log10(limits = c(0.2, 2)) +
  labs(
    x = "Adjusted odds ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 16)

p_educ

ggsave("Fig_Education_FunctionalLimitation_CC.png",
       p_educ, width = 8, height = 4.5, dpi = 300)


############################################################
# MODEL INCLUDING EDUCATION + WEALTH
############################################################

m_equity <- with(
  imp2,
  glm(
    func_limit ~ diabetes + wealth + educ +
      sex + age_group + bmi_cat +
      marital + smoke + alcohol,
    family = binomial
  )
)

p_equity <- pool(m_equity)

equity_results <- summary(p_equity, conf.int = TRUE) %>%
  mutate(
    OR = exp(estimate),
    lower = exp(`2.5 %`),
    upper = exp(`97.5 %`)
  )


forest_equity <- equity_results %>%
  filter(grepl("wealth|educ", term)) %>%
  mutate(
    term = case_when(
      term == "wealthQ2" ~ "Wealth Q2",
      term == "wealthQ3" ~ "Wealth Q3",
      term == "wealthQ4" ~ "Wealth Q4",
      term == "wealthQ5 (Richest)" ~ "Wealth Q5 (Richest)",
      term == "educSome primary (1–7 years)" ~ "Primary (1–7y)",
      term == "educSome secondary (8–11 years)" ~ "Secondary (8–11y)",
      term == "educSecondary or more (12+ years)" ~ "≥12 years",
      TRUE ~ term
    )
  )


library(ggplot2)

p_forest <- ggplot(forest_equity,
                   aes(x = OR, y = reorder(term, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2,
                 color = "#0072B2") +
  scale_x_log10() +
  labs(
    x = "Adjusted Odds Ratio (log scale)",
    y = "",
    title = "Socioeconomic Status and Functional Limitation"
  ) +
  theme_minimal(base_size = 14)

p_forest


ggsave("Fig2_SES_ForestPlot.png",
       p_forest,
       width = 7,
       height = 5,
       dpi = 300)





library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)

# Assumes you already have:
# m_equity <- with(imp2, glm(func_limit ~ diabetes + wealth + educ + sex + age_group + bmi_cat + marital + smoke + alcohol, family=binomial))
# p_equity <- pool(m_equity)

equity_results <- summary(p_equity, conf.int = TRUE) %>%
  mutate(
    OR    = exp(estimate),
    lower = exp(`2.5 %`),
    upper = exp(`97.5 %`),
    sig   = p.value < 0.05
  )

forest_df <- equity_results %>%
  filter(
    term == "diabetesDiabetes" |
      str_detect(term, "^wealth") |
      str_detect(term, "^educ")
  ) %>%
  mutate(
    group = case_when(
      term == "diabetesDiabetes" ~ "Diabetes",
      str_detect(term, "^educ")  ~ "Education",
      str_detect(term, "^wealth") ~ "Wealth",
      TRUE ~ "Other"
    ),
    label = case_when(
      term == "diabetesDiabetes" ~ "Diabetes (vs no diabetes)",
      
      term == "educSome primary (1–7 years)" ~ "Education: Primary (1–7y) vs none",
      term == "educSome secondary (8–11 years)" ~ "Education: Secondary (8–11y) vs none",
      term == "educSecondary or more (12+ years)" ~ "Education: ≥12y vs none",
      
      term == "wealthQ2" ~ "Wealth: Q2 vs Q1 (poorest)",
      term == "wealthQ3" ~ "Wealth: Q3 vs Q1 (poorest)",
      term == "wealthQ4" ~ "Wealth: Q4 vs Q1 (poorest)",
      term == "wealthQ5 (Richest)" ~ "Wealth: Q5 (richest) vs Q1 (poorest)",
      
      TRUE ~ term
    ),
    # Optional: add * for significant terms (safe emphasis)
    label = ifelse(sig, paste0(label, " *"), label)
  )

# Order rows for readability
forest_df <- forest_df %>%
  mutate(
    group = factor(group, levels = c("Diabetes", "Education", "Wealth")),
    label = fct_relevel(
      label,
      "Diabetes (vs no diabetes) *", "Diabetes (vs no diabetes)",
      "Education: Primary (1–7y) vs none *", "Education: Primary (1–7y) vs none",
      "Education: Secondary (8–11y) vs none *", "Education: Secondary (8–11y) vs none",
      "Education: ≥12y vs none *", "Education: ≥12y vs none",
      "Wealth: Q2 vs Q1 (poorest) *", "Wealth: Q2 vs Q1 (poorest)",
      "Wealth: Q3 vs Q1 (poorest) *", "Wealth: Q3 vs Q1 (poorest)",
      "Wealth: Q4 vs Q1 (poorest) *", "Wealth: Q4 vs Q1 (poorest)",
      "Wealth: Q5 (richest) vs Q1 (poorest) *", "Wealth: Q5 (richest) vs Q1 (poorest)"
    )
  )

# Colors: Diabetes red (same as before), Education separate, Wealth separate
cols <- c(
  "Diabetes"  = "#D55E00",  # red/orange-red you used earlier
  "Education" = "#CC79A7",  # purple
  "Wealth"    = "#0072B2"   # blue
)

p_forest <- ggplot(forest_df, aes(x = OR, y = fct_rev(label), color = group)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.9, color = "gray40") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.18, linewidth = 1.1) +
  geom_point(size = 4) +
  scale_x_log10() +
  scale_color_manual(values = cols) +
  labs(x = "Adjusted odds ratio (log scale)", y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_forest

ggsave("Fig_SES_Forest_Diabetes_Education_Wealth.png",
       p_forest, width = 11, height = 6.5, dpi = 300)








# Add spacing between blocks
forest_df <- forest_df %>%
  mutate(
    block = case_when(
      group == "Diabetes"  ~ 1,
      group == "Education" ~ 2,
      group == "Wealth"    ~ 3
    )
  ) %>%
  arrange(block, desc(OR)) %>%
  mutate(row_id = row_number())

# Custom colors (keep yours)
cols <- c(
  "Diabetes"  = "#D55E00",  # red
  "Education" = "#CC79A7",  # purple
  "Wealth"    = "#0072B2"   # blue
)

p_forest_clean <- ggplot(forest_df,
                         aes(x = OR, y = fct_rev(label), color = group)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             linewidth = 1,
             color = "gray40") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.2,
                 linewidth = 1.2) +
  geom_point(aes(size = group)) +
  scale_size_manual(values = c(
    "Diabetes" = 5,
    "Education" = 4,
    "Wealth" = 3.5
  )) +
  scale_x_log10(limits = c(0.2, 4)) +
  scale_color_manual(values = cols) +
  labs(
    x = "Adjusted odds ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

p_forest_clean

