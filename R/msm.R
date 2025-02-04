#' @name msm
#' @title Performing Monte Carlo Simulations of Markov Chain
#' @description This is the main function to perform Monte Carlo simulations of Markov Chain on
#' the dynamic forecasting of HVT States of a time series dataset. It includes both ex-post and ex-ante analysis 
#' offering valuable insights into future trends while resolving state transition 
#' challenges through clustering and nearest-neighbor methods to enhance simulation accuracy. 
#' @param state_time_data DataFrame. A dataframe containing state transitions over time(cell id and timestamp)
#' @param forecast_type Character. A character to indicate the type of forecasting.  
#' Accepted values are "ex-post" or "ex-ante".
#' @param initial_state Numeric. An integer indicatiog the state at t0. 
#' @param n_ahead_ante Numeric. A vector of n ahead points to be predicted further in ex-ante analyzes. 
#' @param transition_probability_matrix DataFrame. A dataframe of transition probabilities/ output of 
#' `getTransitionProbability` function
#' @param num_simulations Integer. A number indicating the total number of simulations to run.
#' Default is 100.
#' @param trainHVT_results List.`trainHVT` function output 
#' @param scoreHVT_results List. `scoreHVT` function output
#' @param actual_data Dataframe. A dataFrame for ex-post prediction period with teh actual raw data values
#' @param raw_dataset DataFrame. A dataframe of input raw dataset from the mean and standard deviation will 
#' be calculated to scale up the predicted values
#' @param k Integer. A number of optimal clusters when handling problematic states. 
#' Default is 5.
#' @param handle_problematic_states Logical. To indicate whether to handle problematic states or not.
#' Default is FALSE.
#' @param n_nearest_neighbor Integer. A number of nearest neighbors to consider when handling problematic states.
#' Default is 1.
#' @param show_simulation Logical. To indicate whether to show the simulation lines in plots or not. Default is TRUE.
#' @param mae_metric Character. A character to indicate which metric to calculate Mean Absolute Error. 
#' Accepted entries are "mean", "median", or "mode". Default is "median".
#' @return A list object that contains the forecasting plots and MAE values.
#' \item{[[1]]}{Simulation plots and MAE values for state and centroids plot} 
#' \item{[[2]]}{Summary Table, Dendogram plot and Clustered Heatmap when handle_problematic_states is TRUE} 
#' @author Vishwavani <vishwavani@@mu-sigma.com>
#' @keywords Timeseries_Analysis
#' @include msm_plots.R
#' @examples 
#' dataset <- data.frame(t = as.numeric(time(EuStockMarkets)),
#' DAX = EuStockMarkets[, "DAX"],
#' SMI = EuStockMarkets[, "SMI"],
#' CAC = EuStockMarkets[, "CAC"],
#' FTSE = EuStockMarkets[, "FTSE"])
#' hvt.results<- trainHVT(dataset[,-1],n_cells = 60, depth = 1, quant.err = 0.1,
#'                       distance_metric = "L1_Norm", error_metric = "max",
#'                       normalize = TRUE,quant_method = "kmeans")
#' scoring <- scoreHVT(dataset, hvt.results)
#' cell_id <- scoring$scoredPredictedData$Cell.ID
#' time_stamp <- dataset$t
#' temporal_data <- data.frame(cell_id, time_stamp)
#' table <- getTransitionProbability(temporal_data, 
#' cellid_column = "cell_id",time_column = "time_stamp")
#' colnames(temporal_data) <- c("Cell.ID","t")
#' ex_post_forecasting <- dataset[1800:1860,]
#' ex_post <- msm(state_time_data = temporal_data,
#'               forecast_type = "ex-post",
#'               transition_probability_matrix = table,
#'               initial_state = 2,
#'               num_simulations = 100,
#'               scoreHVT_results = scoring,
#'               trainHVT_results = hvt.results,
#'               actual_data = ex_post_forecasting,
#'               raw_dataset = dataset,
#'              mae_metric = "median",
#'              show_simulation = FALSE)
#' @export msm


msm <- function(state_time_data, 
                forecast_type = "ex-post",
                initial_state, 
                n_ahead_ante, 
                transition_probability_matrix, 
                num_simulations = 100, 
                trainHVT_results, 
                scoreHVT_results, 
                actual_data = NULL,   
                raw_dataset,
                k = 5,
                handle_problematic_states = FALSE,
                n_nearest_neighbor = 1,show_simulation=TRUE,
                mae_metric = "median") {
  
  suppressWarnings(suppressMessages(requireNamespace('NbClust')))
  
  ##FOR CRAN WARNINGS
  pdf <- Current_State <- Next_State <-is_self_only <-transitions <- next_states <- nearest_neighbor <- time <- NULL
  
  
  # Input validation checks
  if(forecast_type == "ex-post" && is.null(actual_data)) {
    stop("actual_data is required for ex-post predictions")
  }
  
  if (!mae_metric %in% c("median", "mean", "mode")) {
    stop("Only 'mean', 'median', or 'mode' are accepted as mae_metric")
  }
  
  if(!"t" %in% colnames(raw_dataset)) {
    stop("Please rename the time column in your dataset to 't' in all places")
  }
  
  if(!"Cell.ID" %in% colnames(state_time_data)) {
    stop("The input data should have a column named 'Cell.ID' for cell ids")
  }
  if(!"t" %in% colnames(state_time_data)) {
    stop("The input data should have a column named 't' for timestamps")
  }
  if(is.null(transition_probability_matrix) || nrow(transition_probability_matrix) == 0) {
    stop("transition_probability_matrix cannot be empty")
  }
  
  # Data variable assignation
  centroid_2d_points <- scoreHVT_results$cellID_coordinates
  centroid_data <- trainHVT_results[[3]][["centroid_data"]]
  
  # Set prediction horizon
  if(forecast_type == "ex-post") {
    test_data <- tail(state_time_data, nrow(actual_data))
    n_ahead <- nrow(actual_data)
  } else if(forecast_type == "ex-ante") {
    n_ahead <- length(n_ahead_ante)
  } else {
    stop("Invalid prediction type. Please choose either 'ex-post' or 'ex-ante'")
  }
  
  if(handle_problematic_states) {
    
    pdf(NULL)
    
    # Identify problematic states
    max_state <- max(state_time_data$Cell.ID, na.rm = TRUE)
    cell_seq <- seq(1, max_state)
    missing_states <- cell_seq[!(cell_seq %in% transition_probability_matrix$Current_State)]
    
    # Find self-transition states
    self_transition_states <- transition_probability_matrix %>%
      group_by(Current_State) %>%
      summarize(
        is_self_only = n() == 1 && all(Current_State == Next_State),
        .groups = 'drop'
      ) %>%
      filter(is_self_only) %>%
      pull(Current_State)
    
    # Find cyclic states
    find_cyclic_states <- function(trans_table) {
      states_with_two <- trans_table %>%
        group_by(Current_State) %>%
        summarize(
          transitions = n_distinct(Next_State),
          next_states = list(unique(Next_State)),
          .groups = 'drop'
        ) %>%
        filter(transitions == 2)
      
      cyclic_states <- states_with_two %>%
        rowwise() %>%
        filter({
          curr_next_states <- unlist(next_states)
          next_states_trans <- trans_table %>%
            filter(Current_State %in% curr_next_states) %>%
            group_by(Current_State) %>%
            summarize(n_trans = n_distinct(Next_State), 
                      trans_to = list(unique(Next_State)),
                      .groups = 'drop')
          
          all(next_states_trans$n_trans == 2) &&
            all(unlist(next_states_trans$trans_to) %in% c(Current_State, curr_next_states)) &&
            all(sapply(next_states_trans$trans_to, function(x) Current_State %in% x))
        }) %>%
        pull(Current_State)
      
      return(cyclic_states)
    }
    
    cyclic_states <- find_cyclic_states(transition_probability_matrix)
    
    # Combine all problematic states
    problematic_states <- unique(c(missing_states, self_transition_states, cyclic_states))
    
    # Handle clustering if there are problematic states
    if(length(problematic_states) > 0) {
      # Perform clustering
      cluster_data <- data.frame(
        Cell.ID = centroid_2d_points$Cell.ID,
        x_coordinate = centroid_2d_points$x,
        y_coordinate = centroid_2d_points$y
      )
      
      temp <- utils::capture.output({
        suppressWarnings({
          suppressMessages({
            clust.results <- clustHVT(
              data <- centroid_2d_points %>% select(-Cell.ID),
              trainHVT_results = trainHVT_results,
              scoreHVT_results = scoreHVT_results,
              clusters_k = k
            )
          })
        })
      })
      
      grDevices:: dev.off()
      
      clusters <- stats::cutree(clust.results$hc, k = k)
      
      cluster_data <- data.frame(
        Cell.ID = centroid_2d_points$Cell.ID,
        cluster = clusters,
        names.column = scoreHVT_results$centroidData$names.column
      )
      
     
      
      # Check for singleton clusters
      cluster_counts <- table(cluster_data$cluster)
      if(any(cluster_counts == 1)) {
        stop(paste0("Singleton clusters present with k = ", k, ". Please try a different value of k."))
      }
      
      # Create stp_list for mapping problematic states to their neighbors
      neighbor_mapping <- cluster_data %>%
        group_by(cluster) %>%
        group_split() %>%
        purrr::map_dfr(function(cluster_group) {
          prob_states_in_cluster <- intersect(cluster_group$Cell.ID, problematic_states)
          
          if(length(prob_states_in_cluster) == 0) return(NULL)
          
          coords <- centroid_2d_points[centroid_2d_points$Cell.ID %in% cluster_group$Cell.ID, -1]
          dist_matrix <- as.matrix(dist(coords, method = "euclidean"))
          rownames(dist_matrix) <- cluster_group$Cell.ID
          colnames(dist_matrix) <- cluster_group$Cell.ID
          
          purrr::map_dfr(prob_states_in_cluster, function(prob_state) {
            distances <- dist_matrix[as.character(prob_state), ]
            valid_neighbors <- setdiff(names(sort(distances)), 
                                       as.character(c(prob_state, problematic_states)))
            
            if(length(valid_neighbors) > 0) {
              # Take up to n_nearest_neighbor neighbors
              num_neighbors <- min(length(valid_neighbors), n_nearest_neighbor)
              nearest_n <- as.numeric(valid_neighbors[1:num_neighbors])
              # Create single row with list of nearest neighbors
              tibble(
                problematic_state = prob_state,
                nearest_neighbor = list(nearest_n)
              )
            } else {
              NULL
            }
          })
        })
      
      # Convert neighbor mapping to stp_list format
      stp_list <- neighbor_mapping %>%
        mutate(nearest_neighbor = sapply(nearest_neighbor, 
                                         function(x) paste(x, collapse=",")))
    }
    
    

    
    # Core simulation functions
    find_next_state <- function(random_shock, transition) {
      next_state <- transition$Next_State[which.max(random_shock <= transition$cumulative_prob)]
      return(as.numeric(next_state))
    }
    
    find_nearest_neighbor <- function(problematic_state, cluster_data, centroid_2d_points, problematic_states) {
      current_cluster <- cluster_data$cluster[cluster_data$Cell.ID == problematic_state]
      coords <- centroid_2d_points[centroid_2d_points$Cell.ID %in% 
                                     cluster_data$Cell.ID[cluster_data$cluster == current_cluster], ]
      
      current_coords <- coords[coords$Cell.ID == problematic_state, c("x", "y")]
      distances <- sqrt((coords$x - current_coords$x)^2 + (coords$y - current_coords$y)^2)
      
      valid_neighbors <- coords$Cell.ID[!coords$Cell.ID %in% c(problematic_state, problematic_states)]
      
      if(length(valid_neighbors) > 0) {
        neighbor_distances <- distances[coords$Cell.ID %in% valid_neighbors]
        num_neighbors <- min(length(valid_neighbors), n_nearest_neighbor)
        nearest_n <- valid_neighbors[order(neighbor_distances)][1:num_neighbors]
        
        neighbor_probs <- 1/neighbor_distances[order(neighbor_distances)][1:num_neighbors]
        neighbor_probs <- neighbor_probs/sum(neighbor_probs)
        
        random_shock <- round(stats::runif(min=0, max=1, n=1), 4)
        cumulative_probs <- cumsum(neighbor_probs)
        return(nearest_n[which.max(random_shock <= cumulative_probs)])
      }
      return(problematic_state)
    }
    
    
    
    ################
    all_nearest_neighbors <- neighbor_mapping %>%
      pull(nearest_neighbor) %>%  
      unlist() %>%              
      unique()                
    
    # Generate cluster heatmap with both problematic states and their neighbors
    cluster_heatmap <- clusterPlot(
      dataset = data.frame(
        Cell.ID = cluster_data$Cell.ID,
        Cluster = as.factor(cluster_data$cluster),
        names.column = cluster_data$names.column
      ),
      hvt.results = trainHVT_results,
      domains.column = "Cluster",
      highlight_cells = c(problematic_states, all_nearest_neighbors)
    )
    ###################
    
    
    simulate_step <- function(i, current_state) {
      if (i == 1) {
        return(initial_state)
      }
      
      # Handle problematic states
      if(current_state %in% problematic_states) {
        return(find_nearest_neighbor(current_state, cluster_data, centroid_2d_points, problematic_states))
      }
      
      # Normal transition (same as first algorithm)
      random_shock <- round(stats::runif(min=0, max=1, n=1), 4)
      transition <- subset(transition_probability_matrix, Current_State == current_state)
      
      if(nrow(transition) == 0) {
        return(current_state)
      }
      
      transition <- transition[order(-transition$Transition_Probability), ]
      transition$cumulative_prob <- cumsum(transition$Transition_Probability)
      next_state <- find_next_state(random_shock, transition)
      
      # If next state is problematic, find its neighbor
      if(next_state %in% problematic_states) {
        return(find_nearest_neighbor(next_state, cluster_data, centroid_2d_points, problematic_states))
      }
      
      return(next_state)
    }
    
    simulate_sequence <- function() {
      current_state <- initial_state
      simulated_values <- sapply(1:n_ahead, function(i) {
        current_state <<- simulate_step(i, current_state)
        return(current_state)
      })
      return(simulated_values)
    }
    
    # Run simulations
    simulation_results <- replicate(num_simulations, simulate_sequence())
    simulation_results <- as.data.frame(simulation_results)
    colnames(simulation_results) <- paste0("Sim_", seq_len(num_simulations))
  }else {
    # Basic simulation without problematic states handling
    find_next_state <- function(random_shock, transition) {
      next_state <- transition$Next_State[which.max(random_shock <= transition$cumulative_prob)]
      return(as.numeric(next_state))
    }
    
    simulate_step <- function(i, current_state) {
      random_shock <- round(stats::runif(min=0, max=1, n=1),4)
      
      if (i == 1) {
        return(initial_state)
      }
      
      transition <- subset(transition_probability_matrix, Current_State == current_state)
      if (nrow(transition) == 0) {
        return(current_state)
      }
      
      transition <- transition[order(-transition$Transition_Probability), ]
      transition$cumulative_prob <- cumsum(transition$Transition_Probability)
      next_state <- find_next_state(random_shock, transition)
      
      return(next_state)
    }
    
    simulate_sequence <- function() {
      current_state <- initial_state
      simulated_values <- sapply(1:n_ahead, function(i) {
        current_state <<- simulate_step(i, current_state)
        return(current_state)
      })
      return(simulated_values)
    }
    
    simulation_results <- replicate(num_simulations, simulate_sequence())
    simulation_results <- as.data.frame(simulation_results)
    colnames(simulation_results) <- paste0("Sim_", seq_len(num_simulations))
  }
  
  # Add time column and calculate statistics
  time_values <- if(forecast_type == "ex-post") {
    actual_data$t
  } else {
    n_ahead_ante[1:nrow(simulation_results)]
  }
  
  simulation_results$time <- time_values
  
  simulation_results <- simulation_results %>%
    dplyr::select(time, everything()) %>%
    rowwise() %>%
    mutate(
     mean = round(mean(c_across(starts_with("Sim_")))),
      median = round(stats::median(c_across(starts_with("Sim_")))),
      mode = {
        tbl <- table(c_across(starts_with("Sim_")))
        as.numeric(names(tbl)[which.max(tbl)])
      }
    ) %>%
    ungroup()

  # Generate plots
  plots <- msm_plots(
    simulation_results = simulation_results,
    centroid_data = centroid_data,
    centroid_2d_points = centroid_2d_points, 
    actual_data = if(forecast_type == "ex-post") actual_data else NULL,
    time_column = 't',
    state_time_data = state_time_data,
    forecast_type = forecast_type, 
    n_ahead_ante = n_ahead_ante,
    type = "MSM",
    raw_dataset = raw_dataset,
    show_simulation = show_simulation,
    mae_metric = mae_metric)
  
  # Return results
  if(handle_problematic_states) {
     output_list <-  list(plots = plots, dendogram = clust.results$dendogram, problematic_states_list = stp_list,
                          cluster_heatmap = cluster_heatmap)
     class(output_list) <- "hvt.object"
     return(output_list)
  } else {
    output_list<-list(plots = plots)
  }
}