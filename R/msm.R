#' @name msm
#' @title Performing Monte Carlo Simulations of Markov Chain
#' @description This is the main function to perform Monte Carlo simulations of Markov Chain on
#' the dynamic forecasting of HVT States of a time series dataset. It includes both ex-post and ex-ante analysis 
#' offering valuable insights into future trends while resolving state transition 
#' challenges through clustering and nearest-neighbor methods to enhance simulation accuracy. 
#' @param state_time_data Dataframe. A dataframe containing state transitions over time(cell id and timestamp)
#' @param forecast_type Character. A character to indicate the type of forecasting.  
#' Accepted values are "ex-post" or "ex-ante".
#' @param initial_state Numeric. An integer indicatiog the state at t0. 
#' @param n_ahead_ante Numeric. A vector of n ahead points to be predicted further in ex-ante analyzes. 
#' @param transition_probability_matrix Dataframe. A dataframe of transition probabilities/ output of 
#' `getTransitionProbability` function
#' @param num_simulations Integer. A number indicating the total number of simulations to run.
#' Default is 100.
#' @param trainHVT_results List.`trainHVT` function output 
#' @param scoreHVT_results List. `scoreHVT` function output
#' @param actual_data Dataframe. A dataframe for ex-post prediction period with the actual raw data values
#' @param raw_dataset DataFrame. A dataframe of input raw dataset from the mean and standard deviation will 
#' be calculated to scale up the predicted values
#' @param k Integer. A number of optimal clusters when handling problematic states. 
#' Default is 5.
#' @param handle_problematic_states Logical. To indicate whether to handle problematic states or not.
#' Default is FALSE.
#' @param n_nearest_neighbor Integer. A number of nearest neighbors to consider when handling problematic states.
#' Default is 1.
#' @param show_simulation Logical. To indicate whether to show the simulation lines in plots or not. Default is TRUE.
#' @param time_column Character. The name of the time column in the input dataframe.
#' @param mae_metric Character. A character to indicate which metric to calculate Mean Absolute Error.
#' Accepted entries are "mean", "median", or "mode". Default is "median".
#' @param time_column Character. The name of the column containing time data. Used for aligning and plotting the results.
#' @param precomputed_problematic_states Vector. An internal parameter used in HVTMSMoptimization call 
#' to pass the problematic states detected
#' @param plot_mode Character. A character to indicate what should be included in output list 
#' "mae-only" values (used in HVTMSMoptimization) and "all" (used in direct call, include plots and mae)
#' @return A list object that contains the forecasting plots and MAE values.
#' \item{[[1]]}{Simulation plots and MAE values for state and centroids plot} 
#' \item{[[2]]}{Summary Table, Dendogram plot and Clustered Heatmap when handle_problematic_states is TRUE} 
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Nithya <nithya.sn@@mu-sigma.com>
#' @keywords Timeseries_Analysis
#' @importFrom magrittr %>%
#' @importFrom stats sd cutree
#' @include HVTMSM_support.R
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
#'               num_simulations = 10,
#'               scoreHVT_results = scoring,
#'               trainHVT_results = hvt.results,
#'               actual_data = ex_post_forecasting,
#'               raw_dataset = dataset,
#'               mae_metric = "median",
#'               handle_problematic_states = FALSE,
#'              show_simulation = FALSE,
#'              time_column = 't')
#' @export msm

msm <- function(state_time_data,
                forecast_type = "ex-post",
                initial_state,
                n_ahead_ante = 10,
                transition_probability_matrix,
                num_simulations = 100,
                trainHVT_results,
                scoreHVT_results,
                actual_data = NULL,
                raw_dataset,
                k = NULL,
                handle_problematic_states = TRUE,
                n_nearest_neighbor = NULL,
                show_simulation = TRUE,
                mae_metric = "median",
                time_column,
                precomputed_problematic_states = NULL,
                plot_mode = c("all","mae-only")) {

  # Global variables for CRAN warnings
  time <- simulation <- median <- sd <- studentized_residuals <- value <- Cell.ID <- cluster <- nearest_neighbor <- NULL

  # Detect if called directly (not from optimization)
  called_directly <- !isTRUE(getOption("hvt.msm.optimization", FALSE))
  
  plot_mode <- match.arg(plot_mode)
  suppressWarnings(suppressMessages(requireNamespace('NbClust')))
  
  # ============================================================================
  # CONSOLIDATED VALIDATION
  # ============================================================================
  
  validate_inputs <- function() {
    errors <- c()
    warnings <- c()
    
    # Basic validation
    if(!"Cell.ID" %in% colnames(state_time_data)) 
      errors <- c(errors, "ERROR: Missing 'Cell.ID' column in state_time_data")
    if(!time_column %in% colnames(state_time_data)) 
      errors <- c(errors, paste0("ERROR: Missing '", time_column, "' column in state_time_data"))
    if (is.null(transition_probability_matrix) || nrow(transition_probability_matrix) == 0)
      errors <- c(errors, "ERROR: transition_probability_matrix cannot be empty")
    if (!mae_metric %in% c("median", "mean", "mode"))
      errors <- c(errors, "ERROR: mae_metric must be 'mean', 'median', or 'mode'")

#browser()

    # Ex-post validation
    can_plot_states_actual <- FALSE
    if (forecast_type == "ex-post") {
      if (is.null(actual_data)) {
        warnings <- c(warnings, "No actual_data provided for ex-post - centroids will be plotted without actual comparison")
      } else {
        actual_times <- actual_data[[time_column]]
        available_times <- state_time_data[[time_column]]
        if (length(intersect(actual_times, available_times)) > 0) {
          can_plot_states_actual <- TRUE
        } else {
          stop("ERROR: No temporal state data available for actual data period - states comparison will be skipped")
        }
      }
    }
    
    # Transition matrix validation
    scored_cells_temp <- unique(scoreHVT_results$cellID_coordinates$Cell.ID)
    if(!is.null(transition_probability_matrix)) {
      transition_cells_temp <- unique(c(transition_probability_matrix$Current_State,
                                        transition_probability_matrix$Next_State))
      missing_cells <- setdiff(scored_cells_temp, transition_cells_temp)
      if(length(missing_cells) > 0) {
        warnings <- c(warnings, paste("Some scored cells missing from transition matrix:",
                                      length(missing_cells), "cells affected"))
      }
    }
    
    # Handle problematic states validation
    if (handle_problematic_states) {
      if (k > length(scored_cells_temp)) {
        errors <- c(errors, paste0("Invalid k vs cells (k=", k, ", cells=", length(scored_cells_temp), ")"))
      }
      max_possible_nn <- length(scored_cells_temp) - k
      if (n_nearest_neighbor > max_possible_nn) {
        errors <- c(errors, paste0("Insufficient NN - requested nn=", n_nearest_neighbor,
                                  ", available nn=", max_possible_nn))
      }
    }
    
    if(length(warnings) > 0 && called_directly) {
      message("Validation Warnings:")
      for(w in warnings) message("  - ", w)
    }
    if(length(errors) > 0) stop(paste(paste(errors, collapse = "\n")))
    
    return(list(warnings = warnings, can_plot_states_actual = can_plot_states_actual))
  }
  
  # ============================================================================
  # SIMPLIFIED CLUSTERING
  # ============================================================================
  
  perform_clustering <- function(clustering_data, k, centroid_2d_points) {
    tryCatch({
      # Try clustHVT first
      if (exists("clustHVT", envir = .GlobalEnv, inherits = FALSE)) {
        clustFun <- get("clustHVT", envir = .GlobalEnv, inherits = FALSE)
      } else {
        clustFun <- clustHVT
      }
      
      clust.results <- clustFun(
        data = clustering_data,
        trainHVT_results = trainHVT_results,
        scoreHVT_results = scoreHVT_results,
        clustering_method = "ward.D2",
        indices = NULL,
        clusters_k = as.integer(k),
        type = "default",
        domains.column = NULL
      )
      
      clusters <- cutree(clust.results$hc, k = k)
      cluster_data <- data.frame(
        Cell.ID = centroid_2d_points$Cell.ID,
        cluster = clusters,
        stringsAsFactors = FALSE
      )
      

      return(list(cluster_data = cluster_data, clust.results = clust.results))
      
    }, error = function(e) {
      # Fallback: direct hierarchical clustering
      hc_fb <- stats::hclust(stats::dist(as.matrix(clustering_data)), method = "ward.D2")
      n_items <- nrow(clustering_data)
      if (k < 1 || k > n_items) {
        stop(sprintf("ERROR: Clustering error: k=%d out of range [1..%d]", k, n_items))
      }
      clusters_fb <- stats::cutree(hc_fb, k = k)
      
      cluster_data <- data.frame(
        Cell.ID = centroid_2d_points$Cell.ID,
        cluster = clusters_fb,
        stringsAsFactors = FALSE
      )
      
      clust.results <- list(hc = hc_fb, clusters = clusters_fb)
      return(list(cluster_data = cluster_data, clust.results = clust.results))
    })
  }
  # ============================================================================
  # MAIN EXECUTION
  # ============================================================================
  
  # Run validation
  validation_result <- validate_inputs()
  can_plot_states_actual <- validation_result$can_plot_states_actual
  
  # Prepare data
  centroid_2d_points <- scoreHVT_results$cellID_coordinates
  centroid_data <- trainHVT_results[[3]]$summary
  
  common_cols <- intersect(colnames(raw_dataset), colnames(centroid_data))
  clustering_data <- (scoreHVT_results$cellID_coordinates) %>% dplyr::select(-Cell.ID)
  centroid_data <- centroid_data[, common_cols]
  
  scored_cells <- unique(scoreHVT_results$cellID_coordinates$Cell.ID)
  transition_cells <- unique(c(transition_probability_matrix$Current_State,
                               transition_probability_matrix$Next_State))
  # temporal_cells <- unique(state_time_data$Cell.ID)  # Commented out as not used
  
  has_full_transition_coverage <- all(scored_cells %in% transition_cells)
  
  # Determine n_ahead
  if (forecast_type == "ex-post") {
    if(!is.null(actual_data)) {
      n_ahead <- nrow(actual_data)
    } else {
      n_ahead <- min(nrow(state_time_data), 10)
      if (called_directly) message("No actual_data provided, using temporal data length: ", n_ahead)
    }
  } else if (forecast_type == "ex-ante") {
    n_ahead <- length(n_ahead_ante)
  } else stop("forecast_type must be either 'ex-post' or 'ex-ante'")
  
  # Initialize variables
  stp_list <- NULL; cluster_heatmap <- NULL; clust.results <- NULL; cluster_data <- NULL
  
  # Detect problematic states
  problematic_detection <- detect_problematic_states(transition_probability_matrix, scored_cells)
  # missing_states <- problematic_detection$missing_states  # Commented out as not used
  # self_transition_states <- problematic_detection$self_transition_states  # Commented out as not used
  # cyclic_states <- problematic_detection$cyclic_states  # Commented out as not used
  
  # Handle precomputed problematic states
  detected_problematic <- problematic_detection$all_problematic
  if (!is.null(precomputed_problematic_states)) {
    problematic_states <- intersect(unique(union(precomputed_problematic_states, detected_problematic)), scored_cells)
  } else {
    problematic_states <- intersect(detected_problematic, scored_cells)
  }
  
  # if (called_directly) {
  #   if (length(problematic_states) >= 1) {
  #     message("Problematic states found: ", paste(problematic_states, collapse = ", "))
  #   } else {
  #     message("No problematic states found")
  #   }
  # }
  
  # Handle problematic states with clustering
  if (handle_problematic_states && length(problematic_states) > 0) {
    tryCatch({
      #if (called_directly) message("Starting problematic states handling...")
      
      # Perform clustering
      clustering_result <- perform_clustering(clustering_data, k, centroid_2d_points)
      cluster_data <- clustering_result$cluster_data
      clust.results <- clustering_result$clust.results
      
      # Post-clustering validation: Check neighbor availability for problematic states
      if (!is.null(cluster_data) && length(problematic_states) > 0) {
        per_state_available <- sapply(problematic_states, function(ps) {
          cl_id <- cluster_data$cluster[cluster_data$Cell.ID == ps]
          members <- cluster_data$Cell.ID[cluster_data$cluster == cl_id]
          valid_neighbors <- setdiff(members, c(ps, problematic_states))
          length(valid_neighbors)
        })
        min_available <- if (length(per_state_available)) min(per_state_available) else 0
        
        # Category 1: Isolated problematic states (zero neighbors)
        if (min_available == 0) {
          stop(paste0("Isolated problematic states - At least one problematic state has no valid neighbors in its cluster",
                      "\nSuggestions:\n",
                      "1. Try a different k value (current: ", k, ")\n",
                      "2. Check if your data has sufficient non-problematic states"))
        }
        
        # Category 2: Limited availability of neighbors
        if (n_nearest_neighbor > min_available) {
          stop(paste0("Limited Availability of neighbors (Req = ", n_nearest_neighbor, 
                      ", Pre = ", min_available, ")",
                      "\nSuggestions:\n",
                      "1. Reduce n_nearest_neighbor to ", min_available, " or less (current: ", n_nearest_neighbor, ")\n",
                      "2. Try a different k value (current: ", k, ") to create larger clusters"))
        }
      }
      
      # Add names.column if available
      if (length(scoreHVT_results$centroidData$names.column) == nrow(cluster_data)) {
        cluster_data$names.column <- scoreHVT_results$centroidData$names.column
      }
      
      # Check for singleton clusters
      cluster_counts <- table(cluster_data$cluster)
      if(any(cluster_counts == 1)) {
        warning(paste0("Singleton clusters present with k = ", k,
                       ". Consider trying a different value of k."))
      }
      
      # Create neighbor mapping
      neighbor_mapping <- cluster_data %>%
        dplyr::group_by(cluster) %>%
        dplyr::group_split() %>%
        purrr::map_dfr(function(cluster_group) {
          prob_states_in_cluster <- intersect(cluster_group$Cell.ID, problematic_states)
          if (length(prob_states_in_cluster) == 0) return(NULL)
          coords <- centroid_2d_points[centroid_2d_points$Cell.ID %in% cluster_group$Cell.ID, c("Cell.ID", "x", "y")]
          rownames(coords) <- coords$Cell.ID
          coords_num <- coords[, c("x", "y")]
          dist_matrix <- as.matrix(dist(coords_num, method = "euclidean"))
          rownames(dist_matrix) <- coords$Cell.ID
          colnames(dist_matrix) <- coords$Cell.ID
          purrr::map_dfr(prob_states_in_cluster, function(prob_state) {
            distances <- dist_matrix[as.character(prob_state), ]
            valid_neighbors <- setdiff(names(sort(distances)), as.character(c(prob_state, problematic_states)))
            transition_problem <- dplyr::case_when(
              prob_state %in% problematic_detection$missing_states ~ "Absence Transitions",
              prob_state %in% problematic_detection$self_transition_states ~ "Self-State Only Transitions",
              prob_state %in% problematic_detection$cyclic_states ~ "Cyclic Transitions",
              TRUE ~ NA_character_
            )
            if (length(valid_neighbors) > 0) {
              num_neighbors <- min(length(valid_neighbors), n_nearest_neighbor)
              nearest_n <- as.numeric(valid_neighbors[1:num_neighbors])
              tibble::tibble(
                problematic_state = prob_state,
                `State Transition Problem` = transition_problem,
                nearest_neighbor = list(nearest_n)
              )
            } else NULL
          })
        })
      
      if(nrow(neighbor_mapping) > 0) {
        stp_list <- neighbor_mapping %>% dplyr::mutate(nearest_neighbor = sapply(nearest_neighbor, function(x) paste(x, collapse = ",")))
        all_nearest_neighbors <- neighbor_mapping %>% dplyr::pull(nearest_neighbor) %>% unlist() %>% unique()
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
      }
      
    }, error = function(e) {
      warning(paste("Enhanced clustering failed:", e$message))
      # cluster_data <- NULL; stp_list <- NULL; cluster_heatmap <- NULL; clust.results <- NULL  # Commented out as not used
    })
  }
  
  # Adjust initial state
  initial_state <- adjust_initial_state(initial_state,
                                        transition_cells,
                                        problematic_states,
                                        scored_cells,
                                        centroid_2d_points,
                                        cluster_data,
                                        n_nearest_neighbor)
  
  # Final validation for problematic states handling
  if (handle_problematic_states && length(problematic_states) > 0) {
    if (is.null(cluster_data)) stop("ERROR: Clustering failed but problematic states exist. Cannot proceed with MSM.")
    # Neighbor availability already validated post-clustering
  }
  
  # Run simulations
  simulation_results <- sapply(seq_len(num_simulations), function(sim_index) {
    simulate_sequence_enhanced(sim_index = sim_index,
                               n_ahead = n_ahead,
                               initial_state = initial_state,
                               transition_probability_matrix = transition_probability_matrix,
                               centroid_2d_points = centroid_2d_points,
                               cluster_data = cluster_data,
                               problematic_states = problematic_states,
                               n_nearest_neighbor = n_nearest_neighbor,
                               scored_cells = scored_cells)
  })
  simulation_results <- as.data.frame(simulation_results)
  colnames(simulation_results) <- paste0("Sim_", seq_len(num_simulations))
  
  # Add time column
  time_values <- if(forecast_type == "ex-post") {
    if(!is.null(actual_data)) actual_data[[time_column]] else head(state_time_data[[time_column]], n_ahead)
  } else {
    n_ahead_ante[1:nrow(simulation_results)]
  }
  simulation_results$time <- time_values
  
  # Calculate summary statistics
  simulation_results <- simulation_results %>%
    dplyr::select(time, dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      mean   = round(mean(dplyr::c_across(dplyr::starts_with("Sim_")))),
      median = round(stats::median(dplyr::c_across(dplyr::starts_with("Sim_")))),
      mode   = {
        tbl <- table(dplyr::c_across(dplyr::starts_with("Sim_")))
        as.numeric(names(tbl)[which.max(tbl)])
      }
    ) %>%
    dplyr::ungroup()
  
  # Fast MAE-only return
  if (plot_mode == "mae-only") {
    if (forecast_type != "ex-post" || is.null(actual_data))
      stop("mae-only mode currently expects ex-post with actual_data.")
    
    test_data_states <- state_time_data[state_time_data[[time_column]] %in% simulation_results$time, ]
    summary_data <- simulation_results[, c("time", "mean", "median", "mode")]
    
    # Calculate MAEs
    raw_dataset_wo_time <- raw_dataset %>% dplyr::select(-(time_column))
    mean_raw <- raw_dataset_wo_time %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE)))
    sd_raw   <- raw_dataset_wo_time %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ stats::sd(., na.rm = TRUE)))
    
    name_columns <- colnames(centroid_data)
    centroid_map_df <- cbind(centroid_data, Cell.ID = centroid_2d_points$Cell.ID)
    
    actual_raw_dfs <- lapply(name_columns, function(col_name) {
      df <- actual_data[, c(time_column, col_name)]
      names(df) <- c("time", paste0("actual_", col_name))
      df
    })
    names(actual_raw_dfs) <- name_columns
    
    compute_mae_for_metric <- function(metric_name) {
      pred_states_metric <- summary_data[[metric_name]]
      states_mae_metric <- mean(abs(test_data_states$Cell.ID[seq_along(pred_states_metric)] - pred_states_metric), na.rm = TRUE)
      centroid_mae_list_metric <- lapply(name_columns, function(var) {
        map_vec <- setNames(centroid_map_df[[var]], centroid_map_df$Cell.ID)
        pred_numeric <- as.numeric(map_vec[as.character(pred_states_metric)])
        if (isTRUE(trainHVT_results[["model_info"]][["input_parameters"]][["normalize"]])) {
          pred_numeric <- pred_numeric * sd_raw[[1, var]] + mean_raw[[1, var]]
        }
        act_vec <- actual_raw_dfs[[var]][[paste0("actual_", var)]][seq_along(pred_numeric)]
        this_mae <- mean(abs(act_vec - pred_numeric), na.rm = TRUE)
        list(mae = as.numeric(this_mae))
      })
      list(centroid_mae_list = centroid_mae_list_metric, states_mae = as.numeric(states_mae_metric))
    }

    mae_by_metric <- list(
      mean   = compute_mae_for_metric("mean"),
      median = compute_mae_for_metric("median"),
      mode   = compute_mae_for_metric("mode")
    )

    out <- list(mae_by_metric = mae_by_metric)
    class(out) <- "hvt.object"
    return(out)
  }
  
  # Generate plots
  if (called_directly || plot_mode != "mae-only") {
    plots <- mcmc_plots(
      simulation_results = simulation_results,
      centroid_data = centroid_data,
      centroid_2d_points = centroid_2d_points,
      actual_data = actual_data,
      state_time_data = state_time_data,
      forecast_type = forecast_type,
      n_ahead_ante = n_ahead_ante,
      type = "MSM",
      raw_dataset = raw_dataset,
      show_simulation = show_simulation,
      mae_metric = mae_metric,
      time_column = time_column,
      trainHVT_results = trainHVT_results
    )
  } else {
    # plots <- list(centroid_mae_list, list(mae = as.numeric(states_mae)))  # Commented out as variables not defined
    plots <- list()
  }
  
  # Return results
  if(handle_problematic_states) {
    output_list <- list(
      plots = plots,
      dendogram = clust.results$dendogram,
      problematic_states_list = stp_list,
      cluster_heatmap = cluster_heatmap,
      problematic_states = problematic_states,
      data_coverage = list(
        scored_cells = scored_cells,
        transition_cells = transition_cells,
        can_plot_states_actual = can_plot_states_actual,
        has_full_transition_coverage = has_full_transition_coverage
      )
    )
    class(output_list) <- "hvt.object"
    return(output_list)
  } else {
    output_list <- list(
      plots = plots,
      simulation_results = simulation_results,
      data_coverage = list(
        scored_cells = scored_cells,
        transition_cells = transition_cells,
        can_plot_states_actual = can_plot_states_actual,
        has_full_transition_coverage = has_full_transition_coverage
      )
    )
    class(output_list) <- "hvt.object"
    return(output_list)
  }
}
