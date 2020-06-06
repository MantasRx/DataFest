SomeScribbling
================

``` r
library(tidyverse)
library(janitor)
library(broom)
```

``` r
april_srvy <- read_csv("AP-Survey/01_April_30_covid_impact_survey.csv")

may_srvy <- read_csv("AP-Survey/02_May_12_covid_impact_survey.csv")
```

``` r
soc_answers <- april_srvy %>%
  select(SOC5A) %>%
  group_by(SOC5A) %>%
  summarise(n = n()) %>%
  select(SOC5A)

april_srvy <- april_srvy %>%
  mutate(SOC5AScore = case_when(
    SOC5A == soc_answers[[1]][[1]] ~ 0,
    SOC5A == soc_answers[[1]][[2]] ~ 1,
    SOC5A == soc_answers[[1]][[3]] ~ 2,
    SOC5A == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

april_srvy <- april_srvy %>%
  mutate(SOC5BScore = case_when(
    SOC5B == soc_answers[[1]][[1]] ~ 0,
    SOC5B == soc_answers[[1]][[2]] ~ 1,
    SOC5B == soc_answers[[1]][[3]] ~ 2,
    SOC5B == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

april_srvy <- april_srvy %>%
  mutate(SOC5CScore = case_when(
    SOC5C == soc_answers[[1]][[1]] ~ 0,
    SOC5C == soc_answers[[1]][[2]] ~ 1,
    SOC5C == soc_answers[[1]][[3]] ~ 2,
    SOC5C == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

april_srvy <- april_srvy %>%
  mutate(SOC5DScore = case_when(
    SOC5D == soc_answers[[1]][[1]] ~ 0,
    SOC5D == soc_answers[[1]][[2]] ~ 1,
    SOC5D == soc_answers[[1]][[3]] ~ 2,
    SOC5D == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

april_srvy <- april_srvy %>%
  mutate(SOC5EScore = case_when(
    SOC5E == soc_answers[[1]][[1]] ~ 0,
    SOC5E == soc_answers[[1]][[2]] ~ 1,
    SOC5E == soc_answers[[1]][[3]] ~ 2,
    SOC5E == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

may_srvy <- may_srvy %>%
  mutate(SOC5AScore = case_when(
    SOC5A == soc_answers[[1]][[1]] ~ 0,
    SOC5A == soc_answers[[1]][[2]] ~ 1,
    SOC5A == soc_answers[[1]][[3]] ~ 2,
    SOC5A == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

may_srvy <- may_srvy %>%
  mutate(SOC5BScore = case_when(
    SOC5B == soc_answers[[1]][[1]] ~ 0,
    SOC5B == soc_answers[[1]][[2]] ~ 1,
    SOC5B == soc_answers[[1]][[3]] ~ 2,
    SOC5B == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

may_srvy <- may_srvy %>%
  mutate(SOC5CScore = case_when(
    SOC5C == soc_answers[[1]][[1]] ~ 0,
    SOC5C == soc_answers[[1]][[2]] ~ 1,
    SOC5C == soc_answers[[1]][[3]] ~ 2,
    SOC5C == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

may_srvy <- may_srvy %>%
  mutate(SOC5DScore = case_when(
    SOC5D == soc_answers[[1]][[1]] ~ 0,
    SOC5D == soc_answers[[1]][[2]] ~ 1,
    SOC5D == soc_answers[[1]][[3]] ~ 2,
    SOC5D == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

may_srvy <- may_srvy %>%
  mutate(SOC5EScore = case_when(
    SOC5E == soc_answers[[1]][[1]] ~ 0,
    SOC5E == soc_answers[[1]][[2]] ~ 1,
    SOC5E == soc_answers[[1]][[3]] ~ 2,
    SOC5E == soc_answers[[1]][[4]] ~ 3,
    TRUE ~ NaN
  ))

happiness_calculator <- function(socialAScore, socialBScore, socialCScore, socialDScore, socialEScore) {
  happiness <- socialAScore + socialBScore + socialCScore + socialDScore + socialEScore
}

may_srvy <- may_srvy %>% mutate(happiness = happiness_calculator(SOC5AScore, SOC5BScore, SOC5CScore, SOC5DScore, SOC5EScore))

april_srvy <- april_srvy %>% mutate(happiness = happiness_calculator(SOC5AScore, SOC5BScore, SOC5CScore, SOC5DScore, SOC5EScore))

# Result check april_srvy %>% select(happiness) %>% group_by(happiness) %>% summarise(n = n())

#Write the new files with happiness variable as new .csv's into the directory
may_happiness <- write.csv(may_srvy, "AP-Survey/may_happiness.csv")
april_happiness <- write.csv(april_srvy, "AP-Survey/april_happiness.csv")
```
