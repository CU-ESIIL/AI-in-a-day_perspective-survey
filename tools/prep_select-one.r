#' @title Wrangle and Summarize 'Select One' Survey Question Data
#' 
#' @description Calculate number and percent of responses for 'select one' type survey questions. Makes the answer categories into a factor if they are not already (uses the order of the corresponding "__value" column if one can be found in the data).
#' 
#' @param df (data.frame) Table of survey data containing the response of interest
#' @param q (character) Name of column (in `df`) containing question data of interest
#' @param grp (character) Name of columns in `df`, by which to group the `q` values before calculating percent responses
#' 
#' importFrom magrittr %>%
#' 
prep_select_one <- function(df = NULL, q = NULL, grp = NULL){

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
  
  # Remove NAs in relevant question
  df_v02 <- df[!is.na(df[[q]]),]

  # Also remove NAs from grouping column(s)
  if(is.null(grp) != TRUE){
    for(g in seq_along(grp)){
      df_v02 <- df_v02[!is.na(df_v02[[grp[g]]]),]
    }
  }
  
  # Pare down columns
  need_cols <- intersect(x = names(df_v02), y = c("ResponseId", q, grp))
  df_v03 <- dplyr::select(.data = df_v02, dplyr::all_of(need_cols))
  
  # Handle response order
  if(paste0(q, "__value") %in% names(df)){
    df_v04 <- dplyr::arrange(.data = df_v03, dplyr::across(dplyr::starts_with(paste0(q, "__value"))))
  } else { df_v04 <- dplyr::arrange(.data = df_v03, dplyr::across(dplyr::starts_with(q))) }
  
  # Make response into a factor
  if(is.factor(df[[q]])){
    df_v05 <- df_v04
  } else {
    df_v05 <- df_v04
    df_v05[[q]] <- factor(x = df_v05[[q]], levels = unique(df_v05[[q]]))
  }

  # Make grouping variables a factor (if any are provided)
  if(is.null(grp) != TRUE){
    for(k in seq_along(grp)){
      df_v05 <- df_v05 %>% 
        dplyr::arrange(dplyr::across(dplyr::starts_with(paste0(grp[g], "__value"))))
      df_v05[[grp[g]]] <- factor(x = df_v05[[grp[g]]], levels = unique(df_v05[[grp[g]]]))
    }
  }

  # Count total respondents
  df_v06 <- dplyr::mutate(.data = df_v05, total_respondents = length(unique(ResponseId)))
  
  # Assign correct grouping structure
  if(is.null(grp) != TRUE){
    df_v07 <- df_v06 %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(c(grp, "total_respondents")))) %>% 
      dplyr::mutate(grp_respondents = length(unique(ResponseId))) %>% 
      dplyr::ungroup()
  } else {
    df_v07 <- dplyr::mutate(df_v06, grp_respondents = total_respondents)
  }

  # Summarize response data
  df_v08 <- df_v07 %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(
      c(grp, q, "total_respondents", "grp_respondents")))) %>% 
    dplyr::summarize(unique_respondents = length(unique(ResponseId)),
      .groups = "drop") %>% 
    dplyr::mutate(percent = round((unique_respondents / total_respondents) * 100, digits = 1),
      relative_percent = round((unique_respondents / grp_respondents) * 100, digits = 1)) %>% 
    dplyr::arrange(dplyr::desc(percent))
  
  # Return it
  return(df_v08) }

# End ----
