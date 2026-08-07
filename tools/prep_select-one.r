#' @title Wrangle and Summarize 'Select One' Survey Question Data
#' 
#' @description Calculate number and percent of responses for 'select one' type survey questions. Makes the answer categories into a factor if they are not already (uses the order of the corresponding "__value" column if one can be found in the data).
#' 
#' @param df (data.frame) Table of survey data containing the response of interest
#' @param q (character) Name of column (in `df`) containing question data of interest
#' 
#' importFrom magrittr %>%
#' 
prep_select_one <- function(df = NULL, q = NULL){

  # Error checks for 'df'
  if(is.null(df) || "data.frame" %in% class(df) != TRUE)
    stop("'df' must be provided as a dataframe-like object")

  # Error checks for 'q'
  if(is.null(q) || length(q) != 1 || is.character(q) != TRUE || q %in% names(df) != TRUE)
    stop("'q' must match a single column name in 'df'")
  
  # Remove NAs in relevant question
  df_v02 <- df[!is.na(df[[q]]),]

  # Identify value column name
  val_name <- paste0(q, "__value")

  # Handle response order
  if(paste0(q, "__value") %in% names(df)){
    df_v03 <- dplyr::arrange(.data = df_v02, dplyr::across(dplyr::starts_with(paste0(q, "__value"))))
  } else { df_v03 <- dplyr::arrange(.data = df_v02, dplyr::across(dplyr::starts_with(q))) }

  # Make response into a factor
  if(is.factor(df[[q]])){
    df_v04 <- df_v03
  } else {
    df_v04 <- df_v03
    df_v04[[q]] <- factor(x = df_v04[[q]], levels = unique(df_v04[[q]]))
  }

  # Count responses per category
  df_v05 <- df_v04 %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(q))) %>% 
    dplyr::summarize(response_count = dplyr::n(),
      .groups = "drop") %>% 
    dplyr::mutate(total_responses = sum(response_count, na.rm = TRUE),
      percent = round((response_count / total_responses) * 100, digits = 1))

  # Return it
  return(df_v05) }

# End ----
