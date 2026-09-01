#' @title Wrangle, Summarize, and Graph Frequency Survey Question Data
#' 
#' @description Calculate number of responses for frequency questions and make a simple stacked barplot. Makes the answer categories into a factor if they are not already (uses the order of the corresponding "__value" column if one is found in the data).
#' 
#' @param df (data.frame) Table of survey data containing the response of interest
#' @param q (character) Name of column (in `df`) containing question data of interest
#' 
#' importFrom magrittr %>%
#' 
graph_select_one <- function(df = NULL, q = NULL, grp = NULL){

  # Prepare the data
  ready_df <- prep_select_one(df = df, q = q, grp = grp)

  # Create a graph
  simp_graph <- ggplot2::ggplot(data = ready_df, 
      aes(x = as.character(q), y = percent, fill = .data[[q]], 
      color = "x")) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_color_manual(values = "#000") +
    ggplot2::guides(color = "none")

  # Return it
  return(simp_graph) }

# End ----
