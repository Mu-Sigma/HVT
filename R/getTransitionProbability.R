#' @name getTransitionProbability
#' @title Creating Transition Probability Matrix
#' @description This is the main function to create transition probability matrix
#' The transition probability matrix quantifies the likelihood of transitioning from one state to another. 
#' States: The table includes the current states and the possible next states.
#' Probabilities: For each current state, it lists the probability of transitioning to each of the next possible states. 
#' @param df Data frame. The input data frame should contain two columns, 
#' cell ID from scoreHVT function and time stamp of that dataset.
#' @param cellid_column Character. Name of the column containing cell IDs.
#' @param time_column Character. Name of the column containing time stamps.
#' @param type Character. A character value indicating the type of transition probability table to create.
#' Accepted entries are "with_self_state" and "without_self_state".
#' @return Stores a data frames with transition probabilities.
#' @author PonAnuReka Seenivasan <ponanureka.s@@mu-sigma.com>, Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Timeseries_Analysis
#' @importFrom magrittr %>%
#' @examples
#' dataset <- data.frame(t = as.numeric(time(EuStockMarkets)),
#'                       DAX = EuStockMarkets[, "DAX"],
#'                       SMI = EuStockMarkets[, "SMI"],
#'                       CAC = EuStockMarkets[, "CAC"],
#'                       FTSE = EuStockMarkets[, "FTSE"])
#' hvt.results<- trainHVT(dataset[-1],n_cells = 60, depth = 1, quant.err = 0.1,
#'                        distance_metric = "L1_Norm", error_metric = "max",
#'                        normalize = TRUE,quant_method = "kmeans")
#' scoring <- scoreHVT(dataset, hvt.results)
#' cell_id <- scoring$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$t
#' dataset <- data.frame(cell_id, time_stamp)
#' table <- getTransitionProbability(dataset, cellid_column = "cell_id",time_column = "time_stamp")
#' @export getTransitionProbability


getTransitionProbability <- function(df, cellid_column, time_column, type = "with_self_state") {
  
  ##for cran warnings
  Current_State <- Transition_Probability <- Cumulative_Probability <- Next_State <- Relative_Frequency <- NULL
  
  # Rename columns for consistency
  colnames(df)[colnames(df) == time_column] <- "Timestamp"
  colnames(df)[colnames(df) == cellid_column] <- "Cell.ID"
  
  
   if (type == "with_self_state" || type == "without_self_state") {
  
  # Get a sorted list of unique Cell.ID values
  cell_id_list <- unique(df$Cell.ID) %>% sort()
  
  prob_results <- lapply(cell_id_list, function(state) {
    # Find rows with current state
    row_numbers <- which(df$Cell.ID == state)
    
    # Check if the next state exists (not at the end of dataset)
    valid_rows <- row_numbers[row_numbers + 1 <= nrow(df)]
    
    # If no valid transitions exist, return NULL or empty dataframe
    if(length(valid_rows) == 0) {
      return(NULL)
    }
    
    # Get next states only for valid rows
    tplus1_states <- df[valid_rows + 1, "Cell.ID"]
    
    prob_table <- table(tplus1_states)
    total_count <- sum(prob_table)
    probabilities <- as.vector(prob_table) / total_count
    
    result_df <- data.frame(
      Current_State = state,
      Next_State = as.numeric(names(prob_table)),
      Relative_Frequency = as.vector(prob_table),
      Transition_Probability = round(probabilities, 4)
    )
    
    result_df <- result_df %>%
      group_by(Current_State) %>%
      mutate(Cumulative_Probability = round(cumsum(Transition_Probability), 4),
             Cumulative_Probability = if_else(row_number() == n(), 1, Cumulative_Probability)) %>% 
      as.data.frame()
    
    return(result_df)
  })
  
  # Remove NULL results and combine remaining dataframes
  prob_results <- do.call(rbind, prob_results[!sapply(prob_results, is.null)])
  

    trans_prob_matx <- prob_results %>%
      group_by(Current_State) %>%
      filter(!(Current_State == Next_State & n() > 1)) %>%
      mutate(
        Transition_Probability = round((Relative_Frequency / sum(Relative_Frequency)), 4),
        Cumulative_Probability = round(cumsum(Transition_Probability),4),
        Cumulative_Probability = if_else(row_number() == n(), 1, Cumulative_Probability)
      ) %>%
      ungroup()
    
    trans_prob_matx$Next_State <- as.numeric(trans_prob_matx$Next_State)

  } else {
    stop("Invalid input for `type` argument. Only `with_self_state` , `without_self_state`, or `mean_var_table` is allowed.")
  }
    
  
  if (type == "with_self_state") {
    return(prob_results)
  } else if (type == "without_self_state") {
    return(trans_prob_matx)}
 
}
