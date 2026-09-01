#' @title Wrangle and Summarize 'Select All' Survey Question Data
#' 
#' @description Calculate number of unique respondents for 'select all that apply' type survey questions. Also calculates percent of respondents that checked that box (regardless of other boxes that a particular respondent also checked).
#' 
#' @param df (data.frame) Table of survey data containing the response of interest
#' @param q (character) Name of column (in `df`) containing question data of interest
#' @param grp (character) Name of columns in `df`, by which to group the `q` values before calculating percent responses
#' 
#' importFrom magrittr %>%
#' 
prep_select_all <- function(df = NULL, q = NULL, grp = NULL, summarize = TRUE){
  
  # Error checks for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided as a dataframe-like object")

  # Error checks for 'q'
  if(is.null(q) || length(q) != 1 || is.character(q) != TRUE || q %in% names(df) != TRUE)
    stop("'q' must match a single column name in 'df'")
  
  # Error checks for 'grp'
  if(is.null(grp) != TRUE){
    if(is.character(grp) != TRUE || all(grp %in% names(df)) != TRUE)
      stop("'All entries in 'grp' must be an exact match for columns in 'df'")
  }

  # Error checks for 'summarize'
  if(!is.logical(summarize)){
    warning("'summarize' must be a logical. Coercing to TRUE")
    summarize <- TRUE }
    
  # Remove NAs in relevant question
  df_v02 <- df[!is.na(df[[q]]),]

  # If desired, also remove NAs from grouping column(s)
  if(is.null(grp) != TRUE){
    for(g in seq_along(grp)){
      df_v02 <- df_v02[!is.na(df_v02[[grp[g]]]),]
    }
  }
  
  # Pare down columns
  need_cols <- intersect(x = names(df_v02), y = c("ResponseId", q, grp))
  df_v03 <- dplyr::select(.data = df_v02, dplyr::all_of(need_cols))

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
    tidyr::separate_wider_delim(cols = question, delim = ";",
      names = c(paste0("box", 1:(max_delim + 1))), too_few = "align_start") %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("box")) %>% 
    dplyr::select(-name) %>% 
    dplyr::filter(!is.na(value))

  # Do generally-needed tidying of those responses
  df_v06 <- df_v05 %>% 
    dplyr::mutate(value = gsub(pattern = "’", replacement = "'", x = value))
  
  # Count total respondents
  df_v07 <- dplyr::mutate(.data = df_v06, total_respondents = length(unique(ResponseId)))

  # Assign correct grouping structure
  if(is.null(grp) != TRUE){
    df_v08 <- df_v07 %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, "value", "total_respondents"))))
  } else {
    df_v08 <- df_v07 %>% 
      dplyr::group_by(value, total_respondents)
  }

  # Summarize response data
  if(summarize == TRUE){
    df_v09 <- df_v08 %>% 
      dplyr::summarize(unique_respondents = length(unique(ResponseId)),
        .groups = "drop") %>% 
      dplyr::mutate(percent = round((unique_respondents / total_respondents) * 100, digits = 1)) %>% 
      dplyr::arrange(dplyr::desc(percent))   
  } else { df_v09 <- dplyr::ungroup(df_v08) }
  
  # Return it
  return(df_v09) }

# End ----
