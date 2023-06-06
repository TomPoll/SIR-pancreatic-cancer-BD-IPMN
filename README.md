---
title: "SIR-panc-cancer-BDIPMN"
author: "Tommaso Pollini"
date: "2023-03-24"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(readr)
library(knitr)
library(tidyr)
library(dplyr)
library(lubridate)
library(kableExtra)
library(tidyverse)
library(formatR)
opts_chunk$set(tidy.opts=list(width.cutoff=60), tidy=TRUE)
```

## Introduction

The aim of the study is to identify a subset of patients affected by a BD-IPMN of the pancreas where surveillance discontinuation could be considered. To compare the incidence of pancreatic cancer in our cohort with that of the general population we are going to calculate the Standardized Incidence Ratio (SIR) of pancreatic cancer.

In summary, first we will calculate the number of person-year that every patients has contributed to every age group. Afterwards, we will get the sum of the element wise multiplication between person-year and the crude rate of pancreatic cancer, which equals the number of expected cases. Finally we will calculate SIR and its 95% confidence interval.


```{r Dataset read, echo=TRUE, message=FALSE, warning=FALSE}
# The first thing that we need is to read the dataset, and assigning it 
# to the variable df. For the purpose of this markdown a randomly
# generated dataset will be used

df <- tibble(
  DOB = ymd(paste0(round(seq(1930, 1980, length=2000)), "/1/1")), 
  DSTART = DOB + runif(35, 1, 7500),
  DLAST = DSTART + runif(35, 100, 7500),
  Institution = rep(c("SGH", "B", "C", "D", "E"),400),
  ID = c(1:2000),
  Sex = sample(c("MALE", "FEMALE"), size = 2000, replace = TRUE),
  OBS = sample(c(0,1), size = 2000, replace = TRUE, prob = c(0.997, 0.003)))

# With the prob argument of the OBS variable, we have set the rate of observation 
# at 0.3% as an example, changing this number alter the final SIR significantly

```

```{r echo=FALSE}
Male <- c(0.02,0.02,0.04,0.02,0.12,0.19,0.43,1.0,2.7,5.7,11.5,20.1,33.3,48.1,62.0,82.4,98.2,106.9)
Female <- c(0.02,0.00,0.02,0.09,0.17,0.27,0.38,0.83,1.8,3.8,7.5,13.2,22.5,35.7,52.4,67.4,79.4,93.2)
sinMale <- c(0.0,0.0,0.0,0.2,0.0,0.3,0.3,1.6,3.0,3.1,7.9,17.7,21.0,45.1,50.8,61.0,72.5,71.8)
sinFemale <- c(0.0,0.0,0.0,0.2,0.0,0.1,0.3,0.6,2.0,4.4,4.7,10.4,16.4,26.6,38.2,63.6,52.5,83.7)
agegroup <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

cruderate <- data.frame(agegroup, Male, Female, sinMale, sinFemale)
kable(cruderate, col.names = c("Age (years)", "Male", "Female", "Male", "Female"), booktabs = TRUE, caption = "Crude rate (per 100,000 people) of Pancreatic Cancer") %>%
  add_header_above(c(" " = 1, "List A countries" = 2, "Singapore" = 2)) %>%
  kable_styling(latex_options = "HOLD_position", full_width = F) %>%
  add_footnote(c("List A countries: Germany, Italy, Republic of Korea, The Netherlands, Spain, UK, England and Wales, and the USA")) %>%
  column_spec(1, width = "5em") %>%
  column_spec(2, width = "3,3em") %>%
  column_spec(3, width = "3,3em") %>%
  column_spec(4, width = "3,3em") %>%
  column_spec(5, width = "3,3em")
```

The crude rate of pancreatic cancer are inputted into different tibbles, that will be used later to calculate the number of expected cases

```{r - Cancer Rates tibble, echo=TRUE, tidy=TRUE}
# Crude rate (per 100.000) for male per age group
RaM <- tibble(group_1=0.02, group_2=0.02, group_3=0.04, group_4=0.02, group_5=0.12, group_6=0.19, group_7=0.43, group_8=1.0, group_9=2.7, group_10=5.7, group_11=11.5, group_12=20.1, group_13=33.3, group_14=48.1, group_15=62.0, group_16=82.4, group_17=98.2, group_18=106.9)

# Crude rate (per 100.000) for female per age group
RaF <- tibble(group_1=0.02, group_2=0, group_3=0.02, group_4=0.09, group_5=0.17, group_6=0.27, group_7=0.38, group_8=0.83, group_9=1.8, group_10=3.8, group_11=7.5, group_12=13.2, group_13=22.5, group_14=35.7, group_15=52.4, group_16=67.4, group_17=79.4, group_18=93.2)

# Crude rate (per 100.000) for male in SINGAPORE per age group
sRaM <- tibble(group_1=0, group_2=0, group_3=0, group_4=0.2, group_5=0, group_6=0.3, group_7=0.3, group_8=1.6, group_9=3.0, group_10=3.1, group_11=7.9, group_12=17.7, group_13=21.0, group_14=45.1, group_15=50.8, group_16=61.0, group_17=72.5, group_18=71.8)

# Crude rate (per 100.000) for female in SINGAPORE per age group
sRaF <- tibble(group_1=0, group_2=0, group_3=0, group_4=0.2, group_5=0, group_6=0.1, group_7=0.3, group_8=0.6, group_9=2.0, group_10=4.4, group_11=4.7, group_12=10.4, group_13=16.4, group_14=26.6, group_15=38.2, group_16=63.6, group_17=52.5, group_18=83.7)

```

## Overall Person-Year

To calculate SIR, we need to measure the number of expected cases, given the number of person-years stratified by age group and the incidence of pancreatic cancer in the respective age group. Crude rates of pancreatic cancer in the general population were obtained from the IARC dataset for Germany, Italy, Republic of Korea, The Netherlands, Spain, UK, England and Wales, and the USA. Data for Singapore was extracted from the singaporean national registry of disease. Therefore, the number of expected cases was measured separately for patients from SGH (Singapore General Hospital) from patients from all other institutions in the study.

Using the dplyr package in R, we create a new column "s" with a seq.Date object from the date of the first observation (DSTART) to the last date of surveillance or the date when a pancreatic malignancy was identified (DLAST), with days increment.

```{r}
# The following is to calculate person-years of patients
# from all institution excepts Singapore
person_year <- df %>%
  filter(Institution != "SGH") %>%
  rowwise() %>%
  mutate(s = list(seq.Date(DSTART, DLAST, by="days"))) %>% 
  unnest(s) %>% 
  # create a new column with the corresponding age group
  # for every date in seq.Date
  mutate(age = s-DOB, 
         group = as.numeric(floor((age/365.25)/5)+1), 
         group = ifelse(group > 18, 18, group)) %>% 
  group_by(ID, DOB, DSTART, DLAST, group, Sex, OBS) %>% 
  summarise(n = round(n()/365.25, 2)) %>% 
  ungroup %>% 
  arrange(group) %>%
  pivot_wider(names_from = "group", names_prefix = "group_", values_from="n", values_fill=0.00)

```

```{r}
# The following is to calculate person-years of patients
# from Singapore
singapore_person_year <- df %>%
  filter(Institution == "SGH") %>%
  rowwise() %>%
  mutate(s = list(seq.Date(DSTART, DLAST, by="days"))) %>% 
  unnest(s) %>% 
  # create a new column with the corresponding age group
  # for every date in seq.Date
  mutate(age = s-DOB, 
         group = as.numeric(floor((age/365.25)/5)+1), 
         group = ifelse(group > 18, 18, group)) %>% 
  group_by(ID, DOB, DSTART, DLAST, group, Sex, OBS) %>% 
  summarise(n = round(n()/365.25, 2)) %>% 
  ungroup %>% 
  arrange(group) %>%
  pivot_wider(names_from = "group", names_prefix = "group_", values_from="n", values_fill=0.00)
```
```{r total person-year}
totalpy <- person_year %>%
  select(starts_with("group_")) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(., cols = starts_with('group_'))
totpy <- sum(totalpy$value)

sin_totalpy <- singapore_person_year %>%
  select(starts_with("group_")) %>%
  summarise(across(where(is.numeric), sum)) %>%
  pivot_longer(., cols = starts_with('group_'))
sin_totpy <- sum(totalpy$value)

py <- totpy+sin_totpy
```

```{r person year table, echo=FALSE}
kable(person_year[1:6, 1:10], booktabs = TRUE, align = "l", caption = "Person year ") %>%
  kableExtra::footnote(general = "A random sample of 6 cases is reported") %>%
  kable_styling(latex_options = "HOLD_position") 
```

## Person-year by Sex

```{r Female Person-years}
fem_pry <- person_year %>%
  filter(Sex == "FEMALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))
```

```{r Men Person-years}
men_pry <- person_year %>%
  filter(Sex == "MALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))
```

```{r}
#Same calculations is made in patients from Singapore

singapore_fem_pry <- singapore_person_year %>%
  filter(Sex == "FEMALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

singapore_men_pry <- singapore_person_year %>%
  filter(Sex == "MALE")%>%
  summarise(across(OBS:last_col(), ~sum(., is.na=0, 0)))

```


Given $n$ age categories, the number of expected cases is equal to $E=\left(P_{1}I_{1}+P_{2}I_{2}+P_{3}I_{3}...P_{n}I_{n}\right)$ with $P$ being the number of person-years and $I$ the incidence of pancreatic cancer in the $n_{th}$ age group

```{r Female expected cases}
FemaleEx <- bind_rows(RaF, fem_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_female_expected = (rowSums(., na.rm = TRUE)/100000))
```


```{r Male expected cases}
MaleEx <- bind_rows(RaM, men_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_male_expected = (rowSums(., na.rm = TRUE)/100000))
```

```{r}
singapore_FemaleEx <- bind_rows(sRaF, singapore_fem_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_female_expected = (rowSums(., na.rm = TRUE)/100000))

singapore_MaleEx <- bind_rows(sRaM, singapore_men_pry) %>%
  summarise(across(group_1:group_18 & !OBS, ~prod(.))) %>%
  replace(is.na(.), 0) %>%
  mutate(total_male_expected = (rowSums(., na.rm = TRUE)/100000))

```


## Standardized Incidence Ratio

After we obtained the number of expected cases, we can calculate the SIR, defined as:

\begin{align*}
\displaystyle SIR=\frac{{\sum_{k = 1}^{M} D_{k}}}{\sum_{k = 1}^{M} t_{k}\lambda_{k}^{*}}
\end{align*}

where the total number of events observed in the cohort is $D=\sum_{k = 1}^{M} D_{k}$ and the total number of expected events is $E^*=\sum_{k = 1}^{M} E_{k}^*=\sum_{k = 1}^{M} t_{k}\lambda_{k}^{*}$

To approximate the 95% confidence interval (95%CI) we can use the Wilson and Hilferty approximation of the chi-square percentiles:

\begin{align*}
\chi_{v, a}=v\left(1-\frac{2}{9v}+Z_{\alpha}\sqrt{\frac{2}{9v}}\right)^3
\end{align*}

Therefore, the lower limit of the 95%CI is equal to $SIR_{L}=\frac{D}{{E}^*}\left(1-\frac{1}{9D}+\frac{Z_{\alpha/2}}{3\sqrt{D}}\right)^3$ with Z equal to -1.96 while the upper limit is equal to $SIR_{U}=\frac{D+1}{{E}^*}\left(1-\frac{1}{9(D+1)}+\frac{z_{1-\alpha/2}}{3\sqrt{D+1}}\right)^3$ with Z equal to 1.96


```{r SIR}
mEx <- MaleEx$total_male_expected
mObs <- men_pry$OBS

fEx <- FemaleEx$total_female_expected
fObs <- fem_pry$OBS

sin_mEx <- singapore_MaleEx$total_male_expected
sin_mObs <- singapore_men_pry$OBS

sin_fEx <- singapore_FemaleEx$total_female_expected
sin_fObs <- singapore_fem_pry$OBS

totEx <- mEx+fEx+sin_mEx+sin_fEx
totObs <- mObs+fObs+sin_mObs+sin_fObs

SIR <- totObs/totEx
CIl <- SIR*(((1-(1/(9*totObs)))-(1.96/(3*sqrt(totObs))))^3)
CIu <- SIR*((totObs+1)/totObs)*((1-(1/(9*(totObs+1)))+(1.96/(3*sqrt(totObs+1))))^3)

mSIR <- (mObs+sin_mObs) / (mEx+sin_mEx)
mCIl <- mSIR*(((1-(1/(9*(mObs+sin_mObs))))-(1.96/(3*sqrt(mObs+sin_mObs))))^3)
mCIu <- mSIR*(((mObs+sin_mObs)+1)/(mObs+sin_mObs))*((1-(1/(9*((mObs+sin_mObs)+1)))+(1.96/(3*sqrt((mObs+sin_mObs)+1))))^3)

fSIR <- (fObs+sin_fObs) / (fEx+sin_fEx)
fCIl <- fSIR*(((1-(1/(9*(fObs+sin_fObs))))-(1.96/(3*sqrt(fObs+sin_fObs))))^3)
fCIu <- fSIR*(((fObs+sin_fObs)+1)/(fObs+sin_fObs))*((1-(1/(9*((fObs+sin_fObs)+1)))+(1.96/(3*sqrt((fObs+sin_fObs)+1))))^3)
```

```{r}
options(width = 80)
print(paste("Total number of person-year is", py))
print(paste("The number of expected cases is", round(totEx, 2), "while observed cases are", round(totObs, 2)))
print(paste("The number of expected cases in male patients is", round((mEx+sin_mEx), 2), "while observed cases are", round((mObs+sin_mObs), 2)))
print(paste("The number of expected cases in female patients is", round((fEx+sin_fEx), 2), "while observed cases are", round((fObs+sin_fObs), 2)))
```

```{r}
options(width = 80)
print(paste("The SIR for patients with a trivial BD-IPMN is:", round(SIR, 2), "with a 95%CI of",round(CIl, 2),"-",round(CIu,2)))
print(paste("The SIR for male patients is:", round(mSIR, 2), "with a 95%CI of",round(mCIl, 2),"-",round(mCIu,2)))
print(paste("The SIR for female patients is:", round(fSIR, 2), "with a 95%CI of", round(fCIl, 2), "-", round(fCIu, 2)))
```

## Appendix

The session info are below

```{r echo=FALSE}
sessionInfo()
```
