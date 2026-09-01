#' @title Wrangle and Summarize 'Select All' Survey Question Data
#' 
#' @description Calculate number of unique respondents for 'select all that apply' type survey questions. Also calculates percent of respondents that checked that box (regardless of other boxes that a particular respondent also checked).
#' 
#' @param df (data.frame) Table of survey data containing the response of interest
#' @param q (character) Name of column (in `df`) containing question data of interest
#' 
#' importFrom magrittr %>%
#' 
prep_select_all <- function(df = NULL, q = NULL, summarize = TRUE){

  # Error checks for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided as a dataframe-like object")

  # Error checks for 'q'
  if(is.null(q) || length(q) != 1 || is.character(q) != TRUE || q %in% names(df) != TRUE)
    stop("'q' must match a single column name in 'df'")
  
  # Error checks for 'summarize'
  if(!is.logical(summarize)){
    warning("'summarize' must be a logical. Coercing to TRUE")
    summarize <- TRUE }
    
  # Remove NAs in relevant question
  df_v02 <- df[!is.na(df[[q]]),]
  
  # Pare down columns
  df_v03 <- dplyr::select(.data = df_v02, ResponseId, dplyr::all_of(q))

  # Handle difference between commas in actual response text versus collapsing char
  df_v04 <- df_v03 %>% 
    dplyr::rename_with(.fn = ~ gsub(pattern = q, replacement = "question", x = .)) %>% 
    dplyr::mutate(question = gsub(", ", "___", question)) %>% 
    dplyr::mutate(question = gsub(",", ";", question)) %>% 
    dplyr::mutate(question = gsub("___", ", ", question))

  # Count number of boxes checked per question
  delim_ct <- stringr::str_count(string = df_v04$question, pattern = ";")
  max_delim <- max(delim_ct, na.rm = TRUE)

  # Get one row per 'checked box' in original question
  df_v05 <- df_v04 %>% 
    tidyr::separate_wider_delim(cols = -ResponseId, delim = ";",
      names = c(paste0("box", 1:(max_delim + 1))), too_few = "align_start") %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("box")) %>% 
    dplyr::select(-name) %>% 
    dplyr::filter(!is.na(value))

  # Do generally-needed tidying of those responses
  df_v06 <- df_v05 %>% 
    dplyr::mutate(value = gsub(pattern = "’", replacement = "'", x = value))
  
  # Summarize response data
  if(summarize == TRUE){
  df_v07 <- df_v06 %>% 
    dplyr::mutate(total_respondents = length(unique(ResponseId))) %>% 
    dplyr::group_by(value, total_respondents) %>% 
    dplyr::summarize(unique_respondents = length(unique(ResponseId)),
      .groups = "drop") %>% 
    dplyr::mutate(percent = round((unique_respondents / total_respondents) * 100, digits = 1)) %>% 
    dplyr::arrange(dplyr::desc(percent)) 
  
  } else { df_v07 <- df_v06 }
  
  # Return it
  return(df_v07) }

# End ----
