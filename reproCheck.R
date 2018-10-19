#' reproCheck function
#'
#' This is a helper function to aid comparison of values reported in articles to values obtained in reproducibility checks.
#' (1) calculates the percentage error (PE) between a reported value and an obtained value.
#' (2) identifies the error type (decision error, major numerical, minor numerical, no error)
#' (3) user must specificy the value type from the defaults list (or use 'other' if not listed)
#' Errors types are defined as follows:
#' >> 'minor numerical': i.e., >= 2% PE < 10%
#' >> 'major numerical' (i.e., PE >= 10%)
#' If p values are being compared, also returns an additional error type:
#' >> 'decision error' (i.e., reported p and obtained p fall on different sides of the .05 threshold)
#' The default value types are as follows: "p" (p-value), "mean" (mean), "sd" (standard deviation), "se" (standard error), "df" (degrees of freedom), "F" (F test statistic), "t" (t test statistic), "bf" (bayes factor), "ci" (confidence interval), "median" (median), "d" (Cohen's d), "irr" (inter-rater reliability), "r" (Pearson correlation coefficient), "z" (Wilcoxon z), "coeff" (regression coefficients), "n" (count or proportion), "x2" (chi squared), "phi" (phi), "pes" (partial eta squared), "other")
#' Note that sometimes exact values are not reported in the article (e.g., p < .05 ot t <1). In these cases you should visually inspect (eyeball) whether the obtained value falls on the correct side of the specified boundary (e.g., p <.05). If eyeballing is necessary, change the "eyeballCheck" parameter to TRUE if the values appear to match or FALSE if the values do not apepar to match. The eyeballCheck parameter defaults to NA because usually no eyeball check is required.
#' @param reportedValue Enter the value reported in the article
#' @param obtainedValue Enter the corresponding value obtained in your reproducibility check
#' @param valueType The type of value being compared
#' @param eyeballCheck whether a manual 'eyeball' comparison was required, and the outcome of the comparison TRUE/FALSE. Defaults to NA.
#' @param round whether the obtained value should be rounded to same number of decimal places as the reported value TRUE/FALSE. Defaults to TRUE.
#' @return Returns a short text report noting the error type and the PE. Output can also be assigned to a reportObject to keep a running tally of comparisons
#' @export
#' @examples
#' reproCheck(reportedValue = '3.45', obtainedValue = condition_mean, valueType = 'mean', eyeballCheck = NA)
#' reproCheck(reportedValue = '.054', obtainedValue = a_p_value, valueType = 'p', eyeballCheck = NA)
#' reproCheck(reportedValue = '15.63', obtainedValue = this_sd, valueType = 'sd', eyeballCheck = NA)
#' reproCheck(reportedValue = '<.05', obtainedValue = a_significant_p_value, valueType = 'p', eyeballCheck = TRUE)
#' reproCheck(reportedValue = '<.05', obtainedValue = a_nonsignificant_p_value, valueType = 'p', eyeballCheck = FALSE)

reproCheck <- function(reportedValue,
                       obtainedValue,
                       valueType = c("p", "mean", "sd", "se", "df", "F", "t", "bf", "ci", "median", "d", "irr", "r", "z", "coeff", "n", "x2", "phi", "pes", "other"),
                       eyeballCheck = NA,
                       round = TRUE,
                       updatedReportObject = reportObject) {
  
  # check that obtained value is length one
  if(length(reportedValue) != 1){
    stop('WHOOPS! - THE REPORTED VALUE NEEDS TO HAVE LENGTH ONE')
  }
  
  # check that value type was specified
  if(missing(valueType)){
    stop('WHOOPS! - YOU NEED TO ENTER THE VALUE TYPE')
  }
  
  # check that value type was an accepted default
  if(!valueType %in% c("p", "mean", "sd", "se", "df", "F", "t", "bf", "ci", "median", "d", "irr", "r", "z", "coeff", "n", "x2", "phi", "pes", "other")){
    stop('WHOOPS! - YOU NEED TO ENTER THE VALUE TYPE FROM THE SPECIFIED LIST (you can also specify "other")')
  }
  
  # check that reported value was entered as a string
  if(!is.character(reportedValue)){
    stop('WHOOPS! - YOU NEED TO ENTER THE REPORTED VALUE AS A CHARACTER STRING, NOT A NUMBER')
  }
  
  # check that eyeball check was an accepted default
  if(!eyeballCheck %in% c(NA, TRUE, FALSE)){
    stop('WHOOPS! - VALUE FOR EYEBALL CHECK NEEDS TO BE NA, TRUE, OR FALSE. NOTE THESE ARE NOT STRINGS. ENTER NA IF EYEBALL CHECK NOT REQUIRED. ENTER TRUE IF EYEBALL WAS REQUIRED AND VALUES MATCH. ENTER FALSE IF EYEBALL REQUIRED AND VALUES DO NOT MATCH.')
  }
  
  # identify if its a p value
  if(valueType == 'p'){
    isP <- TRUE
  }else{
    isP <- FALSE
  }
  
  if(!is.na(eyeballCheck)){ # reported value was eyeballed
    pe <- NA # set percent error as not applicable
    if(eyeballCheck == TRUE) { # eyeball check suggests match
      comparisonOutcome <- "MATCH"
      reportText <- paste0("MATCH for ", valueType, ". Eyeball comparison only.")
    }else if(eyeballCheck == FALSE) { # eyeball check suggests mismatch
      if(isP == TRUE){ # its a p-value
        comparisonOutcome <- "EYEBALL_ERROR"
        reportText <- paste0("EYEBALL CHECK ERROR for ", valueType, ". Eyeball comparison only.")
      }else if(isP == FALSE){ # its not a p-value
        comparisonOutcome <- "MAJOR_ERROR"
        reportText <- paste0("MAJOR ERROR for ", valueType, ". Eyeball comparison only.")
      }
    }
  }else{ # its a regular reported value (i.e., doesn't need to be eyeballed) - let's check it out in more detail
    
    if(round == TRUE){
      # make sure reported value and obtained value have the same number of decimal places
      # this function will return the number of decimal places
      decimalPlaces <- function(x) {
        nchar(stringr::str_split_fixed(x, "\\.", n = 2))[,2]
      }
      
      dp <- decimalPlaces(reportedValue) # get number of decimal places for reported value
      obtainedValue <- round(as.numeric(obtainedValue), dp) # round obtained value to the same number of decimal places
    }
    
    obtainedValue <- as.numeric(obtainedValue) # ensure obtained value is numeric
    reportedValue <- as.numeric(reportedValue) # ensure reported value is numeric
    
    pe <- ((abs(obtainedValue - reportedValue))/abs(reportedValue))*100 # calculate percentage error
    
    # identify comparison outcome based on amount of percentage error
    if(pe >= 10){
      comparisonOutcome <- "MAJOR_ERROR"
    }else if(pe > 0 & pe < 10){
      comparisonOutcome <- "MINOR_ERROR"
    }else{
      comparisonOutcome <- "MATCH"
    }
    
    if(isP){ # if we are comparing p values
      if((reportedValue >= .05 && obtainedValue <.05) || (reportedValue < .05 && obtainedValue >= .05)){ # if p on wrong side of boundary
        comparisonOutcome <- "DECISION_ERROR"
      }else{ # p value is not on wrong side of boundary
        if(pe >= 10){
          comparisonOutcome <- "MAJOR_ERROR"
        }else if(pe > 0 & pe < 10){
          comparisonOutcome <- "MINOR_ERROR"
        }else{
          comparisonOutcome <- "MATCH"
        }
      }
    }
    if(round == TRUE){
      reportText <- paste0(comparisonOutcome, " for ", valueType, ". The reported value (", reportedValue,") and the obtained value (", obtainedValue,") differed by ", round(pe, 2), "%. Note that the obtained value was rounded to ", dp," decimal places to match the reported value.")
    }else{
      reportText <- paste0(comparisonOutcome, " for ", valueType, ". The reported value (", reportedValue,") and the obtained value (", obtainedValue,") differed by ", round(pe, 2), "%. Note that the obtained value was not rounded.")
    }
  }
  
  # update the reportObject
  newRow <- data.frame(dummyRow = FALSE, reportedValue = reportedValue, obtainedValue = obtainedValue, valueType = valueType, percentageError = pe, comparisonOutcome = comparisonOutcome, eyeballCheck = eyeballCheck)
  updatedReportObject <- rbind(updatedReportObject, newRow)
  
  # print outcome
  print(reportText)
  
  return(updatedReportObject)
}

