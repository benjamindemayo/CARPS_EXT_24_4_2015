CARPS Reproducibility Report
================
Benny deMayo
2018-10-19

-   [Report Details](#report-details)
-   [Methods summary:](#methods-summary)
    -   [Authors tested whether physical experience enhances science learning. Students completed a pretest assessing their understanding the physical concept of angular momentum and then were divided into an 'observation' group and an experimental group in which they experienced the physical consequences of conservation of angular momentum. Authors predicted that physical exposure to the concept of angular momentum would boost learning and thus cause higher scores at posttest.](#authors-tested-whether-physical-experience-enhances-science-learning.-students-completed-a-pretest-assessing-their-understanding-the-physical-concept-of-angular-momentum-and-then-were-divided-into-an-observation-group-and-an-experimental-group-in-which-they-experienced-the-physical-consequences-of-conservation-of-angular-momentum.-authors-predicted-that-physical-exposure-to-the-concept-of-angular-momentum-would-boost-learning-and-thus-cause-higher-scores-at-posttest.)
-   [Target outcomes:](#target-outcomes)
-   [Step 1: Load packages and prepare report object](#step-1-load-packages-and-prepare-report-object)
-   [Step 2: Load data](#step-2-load-data)
-   [Step 3: Tidy data](#step-3-tidy-data)
-   [Step 4: Run analysis](#step-4-run-analysis)
    -   [Pre-processing](#pre-processing)
    -   [Inferential statistics](#inferential-statistics)
-   [Step 5: Conclusion](#step-5-conclusion)
-   [Session information](#session-information)

\[PILOT/COPILOT - TEXT IN SQUARE BRACKETS IS HERE FOR GUIDANCE. COPILOT PLEASE DELETE BEFORE KNITTING THE FINAL REPORT\]

``` r
source('reproCheck.R')
```

Report Details
==============

\[PILOT/COPILOT ENTER RELEVANT REPORT DETAILS HERE\]

``` r
articleID <- 'EXT_24_2_2015' # insert the article ID code here e.g., "10-3-2015_PS"
reportType <- 'pilot' # specify whether this is the 'pilot' report or 'final' report
pilotNames <- 'Benjamin deMayo' # insert the pilot's name here e.g., "Tom Hardwicke".  If there are multiple cpilots enter both names in a character string e.g., "Tom Hardwicke, Bob Dylan"
copilotNames <- NA # # insert the co-pilot's name here e.g., "Michael Frank". If there are multiple co-pilots enter both names in a character string e.g., "Tom Hardwicke, Bob Dylan"
pilotTTC <- 240 # insert the pilot's estimated time to complete (in minutes, fine to approximate) e.g., 120
copilotTTC <- NA # insert the co-pilot's estimated time to complete (in minutes, fine to approximate) e.g., 120
pilotStartDate <- as.Date("10/19/18", format = "%m/%d/%y")  # insert the pilot's start date in US format e.g., as.Date("01/25/18", format = "%m/%d/%y")
copilotStartDate <- NA # insert the co-pilot's start date in US format e.g., as.Date("01/25/18", format = "%m/%d/%y")
completionDate <- NA # copilot insert the date of final report completion (after any necessary rounds of author assistance) in US format e.g., as.Date("01/25/18", format = "%m/%d/%y")
```

------------------------------------------------------------------------

Methods summary:
================

Authors tested whether physical experience enhances science learning. Students completed a pretest assessing their understanding the physical concept of angular momentum and then were divided into an 'observation' group and an experimental group in which they experienced the physical consequences of conservation of angular momentum. Authors predicted that physical exposure to the concept of angular momentum would boost learning and thus cause higher scores at posttest.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Target outcomes:
================

Pretest performance did not differ as a function of group, as revealed by a one-way analysis of variance (ANOVA), F(1, 42) = 0.01, p &gt; .250 (see Fig. 2, left panel). However, an ANOVA controlling for pretest accuracy revealed that group did have a significant effect on posttest performance, F(1, 41) = 5.21, p = .028, ηp 2 = .113. Students in the action group showed a significantly nonzero (~10%) gain in accuracy from pretest to posttest, t(21) = 3.07, p = .006. Those in the observation group did not, t(21) = –0.01, p &gt; .250 (see Fig. 2, left panel).

------------------------------------------------------------------------

\[PILOT/COPILOT DO NOT CHANGE THE CODE IN THE CHUNK BELOW\]

Step 1: Load packages and prepare report object
===============================================

\[PILOT/COPILOT Some useful packages are being loaded below. You can add any additional ones you might need too.\]

``` r
# load packages
library(tidyverse) # for data munging
library(knitr) # for kable table formating
library(haven) # import and export 'SPSS', 'Stata' and 'SAS' Files
library(readxl) # import excel files
library(afex)
library(ez)
#library(CARPSreports) # custom report functions
```

\[PILOT/COPILOT DO NOT MAKE CHANGES TO THE CODE CHUNK BELOW\]

``` r
# Prepare report object. This will be updated automatically by the reproCheck function each time values are compared
reportObject <- data.frame(dummyRow = TRUE, reportedValue = NA, obtainedValue = NA, valueType = NA, percentageError = NA, comparisonOutcome = NA, eyeballCheck = NA)
```

Step 2: Load data
=================

``` r
load("~/Desktop/CARPS_EXT_24_4_2015/data/study 1 physics_dataverse.RData")

study_one_df <- x
```

Step 3: Tidy data
=================

Data are already tidy.

Step 4: Run analysis
====================

Pre-processing
--------------

``` r
#Renaming variables to have more descriptive titles.

study_one_renamed_df <- 
  study_one_df %>% 
  rename(
    pretest_magnitude = mag1,
    posttest_magnitude = mag2,
    pretest_direction = dir1,
    posttest_direction = dir2,
    pretest_rt = RT1,
    posttest_rt = RT2
  ) %>% 
  mutate(
    difference_check = posttest_magnitude - pretest_magnitude,
    difference_accuracy = difference_check == mag_improvement
  )

pre_post_summary <- 
  study_one_renamed_df %>% 
  group_by(group) %>% 
  summarize(
    pretest_mean = mean(pretest_magnitude),
    postest_mean = mean(posttest_magnitude)
  )
```

Inferential statistics
----------------------

``` r
##One-way ANOVA showing no difference at pretest.
a1 <- aov_ez(
  id = "subject",
  data = study_one_renamed_df,
  dv = "pretest_magnitude",
  between = "group"
)

a1
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: pretest_magnitude
    ##   Effect    df  MSE    F   ges p.value
    ## 1  group 1, 42 0.04 0.01 .0004     .90
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

``` r
#reproCheck on F-statistic
reportObject <- 
  reproCheck(
  reportedValue = "0.01", 
  obtainedValue = a1$anova_table$`F`,
  valueType = "F",
  eyeballCheck = TRUE
)
```

    ## [1] "MATCH for F. Eyeball comparison only."

``` r
#reproCheck on df
reportObject <- 
  reproCheck(
  reportedValue = "42",
  obtainedValue = a1$anova_table$`den Df`,
  valueType = "df"
)
```

    ## [1] "MATCH for df. The reported value (42) and the obtained value (42) differed by 0%. Note that the obtained value was rounded to 0 decimal places to match the reported value."

``` r
#reproCheck on p-value
reportObject <- 
  reproCheck(
  reportedValue = ".250",
  obtainedValue = a1$anova_table$`Pr(>F)`,
  valueType = 'p',
  eyeballCheck = TRUE
)
```

    ## [1] "MATCH for p. Eyeball comparison only."

What does the "ANOVA controlling for pretest accuracy" mean?

Is it a one-way ANOVA on the difference scores?

``` r
aov_ez(id = "subject",
       dv = "mag_improvement",
       between = "group", 
       data = study_one_renamed_df)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: mag_improvement
    ##   Effect    df  MSE      F ges p.value
    ## 1  group 1, 42 0.02 4.33 * .09     .04
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

No - the DF are incorrect (42 vs. 41) and the p-value is not correct.

Next hypothesis. Could it be an ANCOVA? (Just specifying a covariate in the analysis)?

Here is the ANOVA on post-test magnitude alone.

``` r
aov_ez(id = "subject",
       dv = "posttest_magnitude",
       between = "group", 
       data = study_one_renamed_df)
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: posttest_magnitude
    ##   Effect    df  MSE    F ges p.value
    ## 1  group 1, 42 0.03 2.73 .06     .11
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

Here is the ANCOVA.

``` r
ezANOVA(study_one_renamed_df, 
        dv = posttest_magnitude,
        wid = subject,
        within_covariates = pretest_magnitude, 
        between = group)
```

    ## $ANOVA
    ##   Effect DFn DFd        F         p p<.05        ges
    ## 1  group   1  42 2.727787 0.1060743       0.06098642
    ## 
    ## $`Levene's Test for Homogeneity of Variance`
    ##   DFn DFd          SSn       SSd           F         p p<.05
    ## 1   1  42 0.0001113636 0.5300773 0.008823756 0.9256076

Another possibility is that they do the appropriate RM anova and look at the group \* time interation term.

``` r
s1_long <- study_one_renamed_df %>%
  gather(phase, value, pretest_magnitude, posttest_magnitude)

ezANOVA(s1_long, 
        dv = value,
        wid = subject,
        within = phase,
        between = group)$ANOVA
```

    ##        Effect DFn DFd         F          p p<.05        ges
    ## 2       group   1  42 0.6446282 0.42656011       0.01257963
    ## 3       phase   1  42 4.2523995 0.04541686     * 0.01691570
    ## 4 group:phase   1  42 4.3318819 0.04353861     * 0.01722642

But we don't see that test matching in terms of df or p value, either.

``` r
##Paired t-test for 'action' (experimental group)
action_group <- 
  study_one_renamed_df %>% 
  filter(group == "action")

t1_action <- 
  t.test(
    action_group$posttest_magnitude,
    action_group$pretest_magnitude,
    paired = TRUE
  )

t1_action
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  action_group$posttest_magnitude and action_group$pretest_magnitude
    ## t = 3.0668, df = 21, p-value = 0.005854
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.03160329 0.16476035
    ## sample estimates:
    ## mean of the differences 
    ##              0.09818182

``` r
#reproCheck on t-statistic
reportObject <- 
  reproCheck(
    reportedValue = "3.07",
    obtainedValue = t1_action$statistic,
    valueType = "t"
  )
```

    ## [1] "MATCH for t. The reported value (3.07) and the obtained value (3.07) differed by 0%. Note that the obtained value was rounded to 2 decimal places to match the reported value."

``` r
#reproCheck on degrees of freedom
reportObject <- 
  reproCheck(
    reportedValue = "21",
    obtainedValue = t1_action$parameter,
    valueType = "df"
  )
```

    ## [1] "MATCH for df. The reported value (21) and the obtained value (21) differed by 0%. Note that the obtained value was rounded to 0 decimal places to match the reported value."

``` r
#reproCheck on p-value
reportObject <- 
  reproCheck(
    reportedValue = ".006",
    obtainedValue = t1_action$p.value,
    valueType = "p"
  )
```

    ## [1] "MATCH for p. The reported value (0.006) and the obtained value (0.006) differed by 0%. Note that the obtained value was rounded to 3 decimal places to match the reported value."

``` r
##Paired t-test for observation (control) group
observation_group <- 
  study_one_renamed_df %>% 
  filter(group == "observation")

t1_observation <- 
  t.test(
    observation_group$posttest_magnitude,
    observation_group$pretest_magnitude,
    paired = TRUE
  )

t1_observation
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  observation_group$posttest_magnitude and observation_group$pretest_magnitude
    ## t = -0.013008, df = 21, p-value = 0.9897
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.07312168  0.07221259
    ## sample estimates:
    ## mean of the differences 
    ##           -0.0004545455

``` r
reportObject <- 
  reproCheck(
    reportedValue = "-0.01",
    obtainedValue = t1_observation$statistic,
    valueType = "t"
  )
```

    ## [1] "MATCH for t. The reported value (-0.01) and the obtained value (-0.01) differed by 0%. Note that the obtained value was rounded to 2 decimal places to match the reported value."

``` r
#reproCheck on degrees of freedom
reportObject <- 
  reproCheck(
    reportedValue = "21",
    obtainedValue = t1_observation$parameter,
    valueType = "df"
  )
```

    ## [1] "MATCH for df. The reported value (21) and the obtained value (21) differed by 0%. Note that the obtained value was rounded to 0 decimal places to match the reported value."

``` r
#reproCheck on p-value
reportObject <- 
  reproCheck(
    reportedValue = ".250",
    obtainedValue = t1_observation$p.value,
    valueType = "p",
    eyeballCheck = TRUE
  )
```

    ## [1] "MATCH for p. Eyeball comparison only."

Step 5: Conclusion
==================

This reproducibility check was mostly a success. All test statistics, p-values and degrees of freedom were reproducible, with the exception of the ANOVA showing that participants in the action group performed better at posttest compared to those in the observation group. A repeated-measures ANOVA was able to recover the same qualitative result, but I was unable to recover the numbers reported in the original paper.

\[PILOT/COPILOT ENTER RELEVANT INFORMATION BELOW\]

``` r
Author_Assistance = FALSE # was author assistance provided? (if so, enter TRUE)

Insufficient_Information_Errors <- 0 # how many discrete insufficient information issues did you encounter?

# Assess the causal locus (discrete reproducibility issues) of any reproducibility errors. Note that there doesn't necessarily have to be a one-to-one correspondance between discrete reproducibility issues and reproducibility errors. For example, it could be that the original article neglects to mention that a Greenhouse-Geisser correct was applied to ANOVA outcomes. This might result in multiple reproducibility errors, but there is a single causal locus (discrete reproducibility issue).

locus_typo <- 0 # how many discrete issues did you encounter that related to typographical errors?
locus_specification <- 1 # how many discrete issues did you encounter that related to incomplete, incorrect, or unclear specification of the original analyses?
locus_analysis <- 0 # how many discrete issues did you encounter that related to errors in the authors' original analyses?
locus_data <- 0 # how many discrete issues did you encounter that related to errors in the data files shared by the authors?
locus_unidentified <- 0 # how many discrete issues were there for which you could not identify the cause

# How many of the above issues were resolved through author assistance?
locus_typo_resolved <- NA # how many discrete issues did you encounter that related to typographical errors?
locus_specification_resolved <- NA # how many discrete issues did you encounter that related to incomplete, incorrect, or unclear specification of the original analyses?
locus_analysis_resolved <- NA # how many discrete issues did you encounter that related to errors in the authors' original analyses?
locus_data_resolved <- NA # how many discrete issues did you encounter that related to errors in the data files shared by the authors?
locus_unidentified_resolved <- NA # how many discrete issues were there for which you could not identify the cause

Affects_Conclusion <- FALSE # Do any reproducibility issues encounter appear to affect the conclusions made in the original article? TRUE, FALSE, or NA. This is a subjective judgement, but you should taking into account multiple factors, such as the presence/absence of decision errors, the number of target outcomes that could not be reproduced, the type of outcomes that could or could not be reproduced, the difference in magnitude of effect sizes, and the predictions of the specific hypothesis under scrutiny.
```

\[PILOT/COPILOT DOD NOT EDIT THE CODE CHUNK BELOW\]

``` r
reportObject <- reportObject %>%
  filter(dummyRow == FALSE) %>% # remove the dummy row
  select(-dummyRow) %>% # remove dummy row designation
  mutate(articleID = articleID) %>% # add variables to report 
  select(articleID, everything()) # make articleID first column

# decide on final outcome
if(any(reportObject$comparisonOutcome %in% c("MAJOR_ERROR", "DECISION_ERROR")) | Insufficient_Information_Errors > 0){
  finalOutcome <- "Failure without author assistance"
  if(Author_Assistance == T){
    finalOutcome <- "Failure despite author assistance"
  }
}else{
  finalOutcome <- "Success without author assistance"
  if(Author_Assistance == T){
    finalOutcome <- "Success with author assistance"
  }
}

# collate report extra details
reportExtras <- data.frame(articleID, pilotNames, copilotNames, pilotTTC, copilotTTC, pilotStartDate, copilotStartDate, completionDate, Author_Assistance, finalOutcome, Insufficient_Information_Errors, locus_typo, locus_specification, locus_analysis, locus_data, locus_unidentified, locus_typo_resolved, locus_specification_resolved, locus_analysis_resolved, locus_data_resolved, locus_unidentified_resolved)

# save report objects
if(reportType == "pilot"){
  write_csv(reportObject, "pilotReportDetailed.csv")
  write_csv(reportExtras, "pilotReportExtras.csv")
}

if(reportType == "final"){
  write_csv(reportObject, "finalReportDetailed.csv")
  write_csv(reportExtras, "finalReportExtras.csv")
}
```

Session information
===================

\[This function will output information about the package versions used in this report:\]

``` r
devtools::session_info()
```

    ##  setting  value                       
    ##  version  R version 3.5.0 (2018-04-23)
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  tz       America/Los_Angeles         
    ##  date     2018-10-19                  
    ## 
    ##  package    * version  date       source         
    ##  abind        1.4-5    2016-07-21 CRAN (R 3.5.0) 
    ##  afex       * 0.22-1   2018-09-24 CRAN (R 3.5.0) 
    ##  assertthat   0.2.0    2017-04-11 cran (@0.2.0)  
    ##  backports    1.1.2    2017-12-13 CRAN (R 3.5.0) 
    ##  base       * 3.5.0    2018-04-24 local          
    ##  bindr        0.1.1    2018-03-13 cran (@0.1.1)  
    ##  bindrcpp   * 0.2.2    2018-03-29 cran (@0.2.2)  
    ##  broom        0.4.4    2018-03-29 CRAN (R 3.5.0) 
    ##  car          3.0-2    2018-08-23 CRAN (R 3.5.0) 
    ##  carData      3.0-2    2018-09-30 CRAN (R 3.5.0) 
    ##  cellranger   1.1.0    2016-07-27 CRAN (R 3.5.0) 
    ##  cli          1.0.0    2017-11-05 cran (@1.0.0)  
    ##  colorspace   1.3-2    2016-12-14 CRAN (R 3.5.0) 
    ##  compiler     3.5.0    2018-04-24 local          
    ##  crayon       1.3.4    2017-09-16 cran (@1.3.4)  
    ##  curl         3.2      2018-03-28 CRAN (R 3.5.0) 
    ##  data.table   1.11.8   2018-09-30 CRAN (R 3.5.0) 
    ##  datasets   * 3.5.0    2018-04-24 local          
    ##  devtools     1.13.5   2018-02-18 CRAN (R 3.5.0) 
    ##  digest       0.6.15   2018-01-28 CRAN (R 3.5.0) 
    ##  dplyr      * 0.7.5    2018-05-19 cran (@0.7.5)  
    ##  evaluate     0.10.1   2017-06-24 CRAN (R 3.5.0) 
    ##  ez         * 4.4-0    2016-11-02 CRAN (R 3.5.0) 
    ##  forcats    * 0.3.0    2018-02-19 CRAN (R 3.5.0) 
    ##  foreign      0.8-70   2017-11-28 CRAN (R 3.5.0) 
    ##  ggplot2    * 2.2.1    2016-12-30 CRAN (R 3.5.0) 
    ##  glue         1.2.0    2017-10-29 cran (@1.2.0)  
    ##  graphics   * 3.5.0    2018-04-24 local          
    ##  grDevices  * 3.5.0    2018-04-24 local          
    ##  grid         3.5.0    2018-04-24 local          
    ##  gtable       0.2.0    2016-02-26 CRAN (R 3.5.0) 
    ##  haven      * 1.1.1    2018-01-18 CRAN (R 3.5.0) 
    ##  hms          0.4.2    2018-03-10 CRAN (R 3.5.0) 
    ##  htmltools    0.3.6    2017-04-28 CRAN (R 3.5.0) 
    ##  httr         1.3.1    2017-08-20 CRAN (R 3.5.0) 
    ##  jsonlite     1.5      2017-06-01 CRAN (R 3.5.0) 
    ##  knitr      * 1.20     2018-02-20 CRAN (R 3.5.0) 
    ##  lattice      0.20-35  2017-03-25 CRAN (R 3.5.0) 
    ##  lazyeval     0.2.1    2017-10-29 CRAN (R 3.5.0) 
    ##  lme4       * 1.1-18-1 2018-08-17 CRAN (R 3.5.0) 
    ##  lmerTest     3.0-1    2018-04-23 CRAN (R 3.5.0) 
    ##  lubridate    1.7.4    2018-04-11 CRAN (R 3.5.0) 
    ##  magrittr     1.5      2014-11-22 cran (@1.5)    
    ##  MASS         7.3-49   2018-02-23 CRAN (R 3.5.0) 
    ##  Matrix     * 1.2-14   2018-04-13 CRAN (R 3.5.0) 
    ##  memoise      1.1.0    2017-04-21 CRAN (R 3.5.0) 
    ##  methods    * 3.5.0    2018-04-24 local          
    ##  mgcv         1.8-23   2018-01-21 CRAN (R 3.5.0) 
    ##  minqa        1.2.4    2014-10-09 CRAN (R 3.5.0) 
    ##  mnormt       1.5-5    2016-10-15 CRAN (R 3.5.0) 
    ##  modelr       0.1.2    2018-05-11 CRAN (R 3.5.0) 
    ##  munsell      0.5.0    2018-06-12 CRAN (R 3.5.0) 
    ##  nlme         3.1-137  2018-04-07 CRAN (R 3.5.0) 
    ##  nloptr       1.2.1    2018-10-03 CRAN (R 3.5.0) 
    ##  numDeriv     2016.8-1 2016-08-27 CRAN (R 3.5.0) 
    ##  openxlsx     4.1.0    2018-05-26 CRAN (R 3.5.0) 
    ##  parallel     3.5.0    2018-04-24 local          
    ##  pillar       1.2.3    2018-05-25 cran (@1.2.3)  
    ##  pkgconfig    2.0.1    2017-03-21 cran (@2.0.1)  
    ##  plyr         1.8.4    2016-06-08 CRAN (R 3.5.0) 
    ##  psych        1.8.4    2018-05-06 CRAN (R 3.5.0) 
    ##  purrr      * 0.2.5    2018-05-29 cran (@0.2.5)  
    ##  R6           2.2.2    2017-06-17 CRAN (R 3.5.0) 
    ##  Rcpp         0.12.17  2018-05-18 cran (@0.12.17)
    ##  readr      * 1.1.1    2017-05-16 CRAN (R 3.5.0) 
    ##  readxl     * 1.1.0    2018-04-20 CRAN (R 3.5.0) 
    ##  reshape2     1.4.3    2017-12-11 CRAN (R 3.5.0) 
    ##  rio          0.5.10   2018-03-29 CRAN (R 3.5.0) 
    ##  rlang        0.2.1    2018-05-30 cran (@0.2.1)  
    ##  rmarkdown    1.10     2018-06-11 CRAN (R 3.5.0) 
    ##  rprojroot    1.3-2    2018-01-03 CRAN (R 3.5.0) 
    ##  rstudioapi   0.7      2017-09-07 CRAN (R 3.5.0) 
    ##  rvest        0.3.2    2016-06-17 CRAN (R 3.5.0) 
    ##  scales       0.5.0    2017-08-24 CRAN (R 3.5.0) 
    ##  splines      3.5.0    2018-04-24 local          
    ##  stats      * 3.5.0    2018-04-24 local          
    ##  stringi      1.2.2    2018-05-02 cran (@1.2.2)  
    ##  stringr    * 1.3.1    2018-05-10 cran (@1.3.1)  
    ##  tibble     * 1.4.2    2018-01-22 cran (@1.4.2)  
    ##  tidyr      * 0.8.1    2018-05-18 cran (@0.8.1)  
    ##  tidyselect   0.2.4    2018-02-26 cran (@0.2.4)  
    ##  tidyverse  * 1.2.1    2017-11-14 CRAN (R 3.5.0) 
    ##  tools        3.5.0    2018-04-24 local          
    ##  utils      * 3.5.0    2018-04-24 local          
    ##  withr        2.1.2    2018-03-15 CRAN (R 3.5.0) 
    ##  xml2         1.2.0    2018-01-24 CRAN (R 3.5.0) 
    ##  yaml         2.1.19   2018-05-01 CRAN (R 3.5.0) 
    ##  zip          1.0.0    2017-04-25 CRAN (R 3.5.0)
