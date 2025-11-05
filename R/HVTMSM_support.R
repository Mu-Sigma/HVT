#' @importFrom magrittr %>%
#' @importFrom stats runif setNames
#' @keywords internal

# Calculate dataset statistics for scaling
calculate_dataset_stats <- function(numeric_dataset) {
  list(
    mean = numeric_dataset %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(mean(.), 4))),
    sd   = numeric_dataset %>% dplyr::summarise(dplyr::across(dplyr::everything(), ~ round(stats::sd(.),   4)))
  )
}

# Pre-compute HVT models sequentially to ensure consistency
precompute_hvt_models <- function(entire_dataset, ncell_range, hvt_params,
                                  verbose = FALSE, time_column) {
  
  # Pre-prepare dataset once
  numeric_dataset <- entire_dataset %>% dplyr::select(-dplyr::all_of(time_column))
  
  # Prepare HVT training dataset
  hvt_train_dataset <- numeric_dataset
  
  # Train models using lapply
  hvt_models <- lapply(ncell_range, function(nclust) {
    tryCatch({
      set.seed(279)
      hvt_args <- c(list(dataset = hvt_train_dataset, n_cells = nclust), hvt_params)  
      hvt_results <- do.call(trainHVT, hvt_args)
      if (is.null(hvt_results) || length(hvt_results) == 0) {
        stop("trainHVT returned NULL or empty results")
      }
      return(hvt_results)
    }, error = function(e) NULL)
  })
  
  # Name the list elements
  names(hvt_models) <- as.character(ncell_range)
  
  return(hvt_models)
}

# Process a single nclust value using pre-computed HVT model
process_single_nclust_with_precomputed_model <- function(nclust, hvt_model, numeric_dataset, entire_dataset, expost_forecasting,
                                  time_column, k_range, nn_range, num_simulations,
                                  time_values, n_ahead, mae_metric, 
                                  raw_dataset_stats, parallel, verbose) {
  
  tryCatch({
    if (verbose) cat("Processing nclust =", nclust, "\n")
    
    # Check if HVT model exists
    if (is.null(hvt_model)) {
      stop("Pre-computed HVT model for nclust = ", nclust, " is NULL")
    }
    
    # Score complete dataset using pre-computed model
    complete_dataset <- dplyr::bind_rows(entire_dataset, expost_forecasting)
    scoring <- score_hvt_model(complete_dataset, hvt_model)
    
    # Prepare temporal data
    temporal_data_all <- prepare_temporal_data(scoring, complete_dataset, time_column)
    
    # Create training-only temporal data for transition probabilities
    max_train_t <- max(entire_dataset[[time_column]], na.rm = TRUE)
    temporal_data_train <- temporal_data_all %>%
      dplyr::filter(.data[[time_column]] <= max_train_t)
    
    # Get transition probabilities using ONLY training data
    prob_trans_matx <- get_transition_probabilities(temporal_data_train, time_column)
    
    # Get initial state
    initial_state <- get_initial_state(temporal_data_train, time_column)
    
    # Prepare centroid data
    centroid_2d_points <- scoring$cellID_coordinates
    centroid_data_for_mae <- prepare_centroid_data_for_mae(
       hvt_model, numeric_dataset, scoring,
      complete_dataset, raw_dataset_stats, mae_metric, centroid_2d_points
    )
    
    # Detect problematic states
    scored_cells <- unique(scoring$scoredPredictedData$Cell.ID)
    problematic_detection <- detect_problematic_states(prob_trans_matx, scored_cells)
    problematic_states <- problematic_detection$all_problematic
    problematic_states <- intersect(problematic_states, scored_cells)
    
    # Filter to training cells only
    training_cells <- unique(temporal_data_train$Cell.ID)
    problematic_states <- problematic_states[problematic_states %in% training_cells]
    
    if (verbose && length(problematic_states) > 0) {
      cat("  Found", length(problematic_states), "problematic states\n")
    }
    
    # Initialize attempts storage
    all_attempts_by_metric <- initialize_attempts_storage(mae_metric)
    
    # Check if there are problematic states
    if (length(problematic_states) == 0) {
      # No problematic states - run baseline simulation only
      baseline_mae <- run_baseline_simulation(
        transition_matrix = prob_trans_matx,
        temporal_data_train = temporal_data_train,
        scoring = scoring,
        hvt_results = hvt_model,
        initial_state = initial_state,
        centroid_data_for_mae = centroid_data_for_mae,
        mae_metrics = mae_metric,
        raw_dataset_stats = raw_dataset_stats,
        time_column = time_column,
        entire_dataset = entire_dataset,
        expost_forecasting = expost_forecasting,
        num_simulations = num_simulations,
        time_values = time_values,
        n_ahead = n_ahead
      )
      
      # Store baseline results
      for (metric in mae_metric) {
        status_msg <- if (is.na(baseline_mae[[metric]]) || is.null(baseline_mae[[metric]])) {
          "ERROR: Data structure error in MSM baseline"
        } else {
          "Successful Iteration without Problematic state"
        }
        
        all_attempts_by_metric[[metric]][[1]] <- data.frame(
          `Number of Cells` = nclust, 
          k = "-", 
          `Number of nearest neighbors` = "-",
          mae = baseline_mae[[metric]], 
          status = status_msg, 
          stringsAsFactors = FALSE, 
          check.names = FALSE
        )
      }
      
      comprehensive_results <- list(
        simulation_results = list(),
        all_attempts = all_attempts_by_metric
      )
      
    } else {
      # Problematic states exist - run full k-nn combinations
      clustering_data <- centroid_2d_points %>% dplyr::select(-.data$Cell.ID)
      
      comprehensive_results <- process_all_k_nn_combinations(
        k_local_range = k_range,
        nn_range = nn_range,  
        problematic_states = problematic_states,
        centroid_2d_points = centroid_2d_points,
        clustering_data = clustering_data,
        hvt_results = hvt_model,
        scoring = scoring,
        nclust = nclust,
        mae_metric = mae_metric,
        all_attempts_by_metric = all_attempts_by_metric,
        prob_trans_matx = prob_trans_matx,
        temporal_data_complete = temporal_data_all,
        initial_state = initial_state,
        centroid_data_for_mae = centroid_data_for_mae,
        raw_dataset_stats = raw_dataset_stats,
        time_column = time_column,
        entire_dataset = entire_dataset,
        expost_forecasting = expost_forecasting,
        num_simulations = num_simulations,
        time_values = time_values,
        n_ahead = n_ahead,
        parallel = FALSE,
        verbose = verbose
      )
    }
    
    # Process and format results
    processed_results <- process_simulation_results(
      simulation_results = comprehensive_results$simulation_results,
      mae_metric = mae_metric,
      nclust = nclust,
      all_attempts_by_metric = comprehensive_results$all_attempts
    )
    
    return(list(
      successful_results = processed_results$successful_results,
      best_result = processed_results$best_result,
      all_attempts = processed_results$all_attempts,
      success = TRUE,
      nclust = nclust
    ))
  }, error = function(e) {
    if (verbose) cat("  ERROR with nclust", nclust, ":", e$message, "\n")
    
    all_config_failure <- list()
    for (metric in mae_metric) {
      all_config_failure[[metric]] <- data.frame(
        `Number of Cells` = nclust, k = "-", `Number of nearest neighbors` = "-",
        mae = NA, status = paste0("ERROR: High-level error: ", e$message),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    
    return(list(
      successful_results = NULL,
      best_result = NULL,
      all_attempts = all_config_failure,
      success = FALSE,
      error = e$message,
      nclust = nclust
    ))
  })
}

# Collect and format final optimization results

collect_final_results <- function(optimization_results, mae_metric, verbose) {
  
  # Efficient result categorization
  successful_results <- optimization_results[purrr::map_lgl(optimization_results, ~ .x$success %||% FALSE)]
  failed_results <- optimization_results[purrr::map_lgl(optimization_results, ~ !(.x$success %||% FALSE))]
  
  final_output <- list()
  
  if (length(optimization_results) > 0) {
    # Process each metric efficiently
    for (metric in mae_metric) {
      metric_key <- paste0(metric, "_mae")
      
      # Collect successful results with consistent data types (5 columns only)
      all_successful_metric <- purrr::map_dfr(optimization_results, function(x) {
        
        # Get k,nn simulation results
        sim_results <- if (!is.null(x$successful_results) && !is.null(x$successful_results[[metric]])) {
          result <- x$successful_results[[metric]]
          result$k <- as.character(result$k)
          result$`Number of nearest neighbors` <- as.character(result$`Number of nearest neighbors`)
          req <- c("Number of Cells","k","Number of nearest neighbors","mae","status")
          result <- result[, intersect(names(result), req), drop = FALSE]
          for (nm in setdiff(req, names(result))) {
            if (nrow(result) > 0) {
              result[[nm]] <- NA
            } else {
              result[[nm]] <- if (nm %in% c("Number of Cells")) integer(0) else if (nm %in% c("k", "Number of nearest neighbors", "status")) character(0) else numeric(0)
            }
          }
          result[req]
        } else {
          data.frame(
            `Number of Cells` = integer(0),
            k = character(0),
            `Number of nearest neighbors` = character(0),
            mae = numeric(0),
            status = character(0),
            check.names = FALSE
          )
        }
        
        # Get baseline results from all_attempts
        baseline_results <- if (!is.null(x$all_attempts) && !is.null(x$all_attempts[[metric]])) {
          tryCatch({
            if (is.list(x$all_attempts[[metric]]) && !is.data.frame(x$all_attempts[[metric]])) {
              attempts_list <- lapply(x$all_attempts[[metric]], function(df) {
                if (is.data.frame(df) && nrow(df) > 0) {
                  df$k <- as.character(df$k)
                  df$`Number of nearest neighbors` <- as.character(df$`Number of nearest neighbors`)
                }
                df
              })
              attempts_df <- do.call(rbind, attempts_list)
            } else {
              attempts_df <- x$all_attempts[[metric]]
            }
            
            if (is.data.frame(attempts_df) && "status" %in% colnames(attempts_df)) {
              success_rows <- attempts_df[grepl("^Successful Iteration", attempts_df$status), ]
              if (nrow(success_rows) > 0) {
                success_rows$k <- as.character(success_rows$k)
                success_rows$`Number of nearest neighbors` <- as.character(success_rows$`Number of nearest neighbors`)
              }
              req <- c("Number of Cells","k","Number of nearest neighbors","mae","status")
              success_rows <- success_rows[, intersect(names(success_rows), req), drop = FALSE]
              for (nm in setdiff(req, names(success_rows))) success_rows[[nm]] <- NA
              success_rows[req]
            } else {
              data.frame(
                `Number of Cells` = integer(0),
                k = character(0),
                `Number of nearest neighbors` = character(0),
                mae = numeric(0),
                status = character(0),
                check.names = FALSE
              )
            }
          }, error = function(e) {
            data.frame(
              `Number of Cells` = integer(0),
              k = character(0),
              `Number of nearest neighbors` = character(0),
              mae = numeric(0),
              status = character(0),
              check.names = FALSE
            )
          })
        } else {
          data.frame(
            `Number of Cells` = integer(0),
            k = character(0),
            `Number of nearest neighbors` = character(0),
            mae = numeric(0),
            status = character(0),
            check.names = FALSE
          )
        }
        
        # Ensure both sim_results and baseline_results have consistent column types
        if (nrow(sim_results) > 0) {
          sim_results$k <- as.character(sim_results$k)
          sim_results$`Number of nearest neighbors` <- as.character(sim_results$`Number of nearest neighbors`)
        }
        
        if (nrow(baseline_results) > 0) {
          baseline_results$k <- as.character(baseline_results$k) 
          baseline_results$`Number of nearest neighbors` <- as.character(baseline_results$`Number of nearest neighbors`)
        }
        
        # Safe combine with consistent structures and drop duplicates
        combined <- dplyr::bind_rows(sim_results, baseline_results)
        combined <- dplyr::distinct(
          combined,
          .data$`Number of Cells`, .data$k, .data$`Number of nearest neighbors`, .data$mae, .data$status,
          .keep_all = FALSE
        )
        combined
      })
      
      # Sort all_successful_metric by Number of Cells, k, and nn
      if (nrow(all_successful_metric) > 0) {
        # Convert to numeric for proper sorting, handling special cases
        all_successful_metric$k_sort <- suppressWarnings({
          ifelse(is.na(all_successful_metric$k) | all_successful_metric$k == "-", 999, as.numeric(all_successful_metric$k))
        })
        all_successful_metric$nn_sort <- suppressWarnings({
          ifelse(is.na(all_successful_metric$`Number of nearest neighbors`) | 
                 all_successful_metric$`Number of nearest neighbors` == "-", 999, 
                 as.numeric(all_successful_metric$`Number of nearest neighbors`))
        })
        # Replace any NAs from conversion with 999
        all_successful_metric$k_sort[is.na(all_successful_metric$k_sort)] <- 999
        all_successful_metric$nn_sort[is.na(all_successful_metric$nn_sort)] <- 999
        
        all_successful_metric <- all_successful_metric %>%
          dplyr::arrange(.data$`Number of Cells`, .data$k_sort, .data$nn_sort) %>%
          dplyr::select(-.data$k_sort, -.data$nn_sort)
      }
      
      # Collect best results per nclust
      best_results_metric <- purrr::map_dfr(successful_results, function(x) {
        if (!is.null(x$best_result) && !is.null(x$best_result[[metric]])) {
          result <- x$best_result[[metric]]
          result$k <- as.character(result$k)
          result$`Number of nearest neighbors` <- as.character(result$`Number of nearest neighbors`)
          result
        } else {
          data.frame()
        }
      })
      
      # Enhanced all attempts collection with error handling
      all_attempts_metric <- tryCatch({
        purrr::map_dfr(optimization_results, function(x) {
          if (!is.null(x$all_attempts) && !is.null(x$all_attempts[[metric]])) {
            if (length(x$all_attempts[[metric]]) > 0) {
              tryCatch({
                if (is.list(x$all_attempts[[metric]]) && !is.data.frame(x$all_attempts[[metric]])) {
                  attempts_df <- do.call(rbind, x$all_attempts[[metric]])
                } else {
                  attempts_df <- x$all_attempts[[metric]]
                }
                
                if (nrow(attempts_df) > 0) {
                  attempts_df$k <- as.character(attempts_df$k)
                  attempts_df$`Number of nearest neighbors` <- as.character(attempts_df$`Number of nearest neighbors`)
                }
                
                attempts_df
              }, error = function(e) {
                data.frame()
              })
            } else {
              data.frame()
            }
          } else {
            data.frame()
          }
        })
      }, error = function(e) {
        data.frame()
      })
      
      # Sort all results tables by Number of Cells, k, and nn
      if (nrow(all_attempts_metric) > 0) {
        # Convert to numeric for proper sorting, handling special cases
        all_attempts_metric$k_sort <- suppressWarnings({
          ifelse(is.na(all_attempts_metric$k) | all_attempts_metric$k == "-", 999, as.numeric(all_attempts_metric$k))
        })
        all_attempts_metric$nn_sort <- suppressWarnings({
          ifelse(is.na(all_attempts_metric$`Number of nearest neighbors`) | 
                 all_attempts_metric$`Number of nearest neighbors` == "-", 999, 
                 as.numeric(all_attempts_metric$`Number of nearest neighbors`))
        })
        # Replace any NAs from conversion with 999
        all_attempts_metric$k_sort[is.na(all_attempts_metric$k_sort)] <- 999
        all_attempts_metric$nn_sort[is.na(all_attempts_metric$nn_sort)] <- 999
        
        all_attempts_metric <- all_attempts_metric %>%
          dplyr::arrange(.data$`Number of Cells`, .data$k_sort, .data$nn_sort) %>%
          dplyr::select(-.data$k_sort, -.data$nn_sort)
      }
      
      if (nrow(best_results_metric) > 0) {
        # Convert to numeric for proper sorting, handling special cases
        best_results_metric$k_sort <- suppressWarnings({
          ifelse(is.na(best_results_metric$k) | best_results_metric$k == "-", 999, as.numeric(best_results_metric$k))
        })
        best_results_metric$nn_sort <- suppressWarnings({
          ifelse(is.na(best_results_metric$`Number of nearest neighbors`) | 
                 best_results_metric$`Number of nearest neighbors` == "-", 999, 
                 as.numeric(best_results_metric$`Number of nearest neighbors`))
        })
        # Replace any NAs from conversion with 999
        best_results_metric$k_sort[is.na(best_results_metric$k_sort)] <- 999
        best_results_metric$nn_sort[is.na(best_results_metric$nn_sort)] <- 999
        
        best_results_metric <- best_results_metric %>%
          dplyr::arrange(.data$`Number of Cells`, .data$k_sort, .data$nn_sort) %>%
          dplyr::select(-.data$k_sort, -.data$nn_sort)
        req <- c("Number of Cells","k","Number of nearest neighbors","mae","status")
        best_results_metric <- best_results_metric[, intersect(names(best_results_metric), req), drop = FALSE]
        for (nm in setdiff(req, names(best_results_metric))) best_results_metric[[nm]] <- NA
        best_results_metric <- best_results_metric[req]
      }
      
      # Find overall best
      overall_best_metric <- if (nrow(all_successful_metric) > 0) {
        tryCatch({
        best_row <- all_successful_metric %>% 
          dplyr::arrange(.data$mae) %>% 
          dplyr::slice(1)
          best_row
        }, error = function(e) {
          data.frame()
        })
      } else {
        data.frame()
      }
      
      # enforce 5-column shape everywhere
      req <- c("Number of Cells","k","Number of nearest neighbors","mae","status")
      shape <- function(df){
        if (is.null(df) || !nrow(df)) return(data.frame(`Number of Cells`=integer(0),k=character(0),`Number of nearest neighbors`=character(0),mae=numeric(0),status=character(0),check.names=FALSE))
        df <- df[, intersect(names(df), req), drop = FALSE]
        for (nm in setdiff(req, names(df))) df[[nm]] <- NA
        df[req]
      }
      final_output[[metric_key]] <- list(
        successful_results = shape(all_successful_metric),
        nclust_best_results = shape(best_results_metric),
        overall_best = shape(overall_best_metric),
        all_results = shape(all_attempts_metric)
      )
    }
  }
  
  # Print summary if verbose
  if (verbose) {
    cat("Optimization Summary:\n")
    cat("  Total configurations tested:", length(optimization_results), "\n")
    cat("  Successfully processed:", length(successful_results), "\n")
    cat("  Failed:", length(failed_results), "\n")
    
    for (metric in mae_metric) {
      metric_key <- paste0(metric, "_mae")
      all_results <- final_output[[metric_key]]$all_results
      if (nrow(all_results) > 0) {
        successful_count <- sum(grepl("^Successful Iteration", all_results$status), na.rm = TRUE)
        total_count <- nrow(all_results)
        success_rate <- round(100 * successful_count / total_count, 1)
        cat("  ", metric, "success rate:", success_rate, "% (", successful_count, "/", total_count, ")\n")
        
        if (nrow(final_output[[metric_key]]$overall_best) > 0) {
          best <- final_output[[metric_key]]$overall_best
          cat("  Best", metric, "MAE:", round(best$mae[1], 4), 
              "with", best$`Number of Cells`[1], "cells, k =", best$k[1], ", nn =", best$`Number of nearest neighbors`[1], "\n")
        }
      }
    }
  }
  
  return(final_output)
}



# ============================================================================
# HVT PROCESSING FUNCTIONS
score_hvt_model <- function(complete_dataset, hvt_results) {
  scoring <- scoreHVT(
    complete_dataset, hvt_results,
    analysis.plots = FALSE,
    names.column = complete_dataset[, 1]
  )
  
  if (is.null(scoring) || is.null(scoring$scoredPredictedData)) {
    stop("scoreHVT returned NULL or missing scoredPredictedData")
  }
  return(scoring)
}

# Prepare temporal data for MSM
prepare_temporal_data <- function(scoring, complete_dataset, time_column) {
  scored_data <- scoring$scoredPredictedData
  scored_data[[time_column]] <- as.POSIXct(complete_dataset[[time_column]])
  
  # Reorder columns to put time first
  time_col_pos <- which(names(scored_data) == time_column)
  if (length(time_col_pos) == 1) {
    other_cols <- names(scored_data)[-time_col_pos]
    scored_data <- scored_data[, c(time_column, other_cols), drop = FALSE]
  }
  
  temporal_data_all <- scored_data %>% dplyr::select(dplyr::all_of(c(time_column, "Cell.ID")))
  
  if (any(is.na(temporal_data_all[[time_column]]))) {
    stop("Failed to parse time column")
  }
  return(temporal_data_all)
}

# Get transition probabilities from training data ONLY
get_transition_probabilities <- function(temporal_data_train, time_column) {
  
  prob_trans_matx <- getTransitionProbability(
    df = temporal_data_train,  # ONLY training data
    cellid_column = "Cell.ID",
    time_column = time_column,
    type = "with_self_state"
  )
  
  if (is.null(prob_trans_matx) || nrow(prob_trans_matx) == 0) {
    stop("getTransitionProbability (train window) returned NULL or empty matrix")
  }
  
  return(prob_trans_matx)
}

# Get initial state for simulation
get_initial_state <- function(temporal_data_train, time_column) {
  last_t_train <- max(temporal_data_train[[time_column]], na.rm = TRUE)
  initial_state <- temporal_data_train$Cell.ID[temporal_data_train[[time_column]] == last_t_train][1]
  return(initial_state)
}

# Prepare centroid data for MAE calculation
# prepare_centroid_data_for_mae <- function(dependent_variables, hvt_results, numeric_dataset,
#                                           scoring, complete_dataset, raw_dataset_stats,
#                                           mae_metric, centroid_2d_points) {
#   if (is.null(dependent_variables)) {
#     # Normal flow: align to training variables only
#     centroid_data_for_mae <- hvt_results[[3]]$summary %>%
#       dplyr::select(dplyr::any_of(colnames(numeric_dataset)))
#   } else {
#     # DV flow: build normalized per-state centroids for DVs
#     assign_df <- scoring$scoredPredictedData %>% dplyr::select("Cell.ID")
#     dv_df <- cbind(assign_df, complete_dataset[, dependent_variables, drop = FALSE])
#     
#     mu_vec <- as.numeric(raw_dataset_stats$mean[1, dependent_variables, drop = TRUE])
#     sd_vec <- as.numeric(raw_dataset_stats$sd[1, dependent_variables, drop = TRUE])
#     sd_vec[is.na(sd_vec) | sd_vec == 0] <- 1
#     
#     dv_norm <- as.data.frame(Map(function(col, m, s) (col - m)/s,
#                                  dv_df[dependent_variables], mu_vec, sd_vec))
#     dv_norm$Cell.ID <- dv_df[["Cell.ID"]]
#     
#     agg_fun <- switch(mae_metric[1],
#                       mean = function(x) mean(x, na.rm = TRUE),
#                       median = function(x) median(x, na.rm = TRUE),
#                       mode = function(x) {
#                         tb <- table(x);
#                         as.numeric(names(tb)[which.max(tb)])
#                       })
#     
#     dv_state_map_scaled <- dv_norm %>%
#       dplyr::group_by(.data$Cell.ID) %>%
#       dplyr::summarise(dplyr::across(dplyr::all_of(dependent_variables), ~ agg_fun(.x)), .groups = "drop")
#     
#     all_cells <- data.frame(Cell.ID = centroid_2d_points[["Cell.ID"]])
#     dv_state_map_scaled <- dplyr::left_join(all_cells, dv_state_map_scaled, by = "Cell.ID")
#     
#     for (nm in dependent_variables) {
#       dv_state_map_scaled[[nm]][is.na(dv_state_map_scaled[[nm]])] <- 0
#     }
#     
#     centroid_data_for_mae <- dv_state_map_scaled %>% dplyr::select(-"Cell.ID")
#   }
#   
#   return(centroid_data_for_mae)
# }

# Prepare centroid data for MAE calculation
prepare_centroid_data_for_mae <- function(hvt_results, numeric_dataset,
                                          scoring, complete_dataset, raw_dataset_stats,
                                          mae_metric, centroid_2d_points) {
  # Normal flow: align to training variables only
  centroid_data_for_mae <- hvt_results[[3]]$summary %>%
    dplyr::select(dplyr::any_of(colnames(numeric_dataset)))
  
  return(centroid_data_for_mae)
}
# Initialize attempts storage for each metric
initialize_attempts_storage <- function(mae_metric) {
  all_attempts_by_metric <- list()
  for (metric in mae_metric) {
    all_attempts_by_metric[[metric]] <- list()
  }
  return(all_attempts_by_metric)
}

# ============================================================================
# SIMULATION FUNCTIONS
# ============================================================================

# Run MSM simulation directly for optimization - MSM handles all validation
run_single_simulation <- function(k_val, n_nn, cluster_data, transition_matrix,
                                  temporal_data_train, scoring, hvt_results, initial_state,
                                  problematic_states, centroid_data_for_mae, mae_metrics,
                                  raw_dataset_stats, time_column, entire_dataset,
                                  expost_forecasting, num_simulations, time_values, n_ahead) {
  
  # Create temporal data properly for MSM
  scored_data <- scoring$scoredPredictedData
  complete_dataset <- rbind(entire_dataset, expost_forecasting)
  scored_data[[time_column]] <- as.POSIXct(complete_dataset[[time_column]])
  temporal_data_all <- scored_data %>% dplyr::select(dplyr::all_of(c(time_column, "Cell.ID")))
  
  tryCatch({
    # Set optimization flag for hvt_msm_opt
    options(hvt.msm.optimization = TRUE)
  
    # Let MSM handle ALL validation and clustering - no pre-validation
    msm_result <- msm(
      state_time_data = temporal_data_all,
      forecast_type = "ex-post",
      transition_probability_matrix = transition_matrix,
      initial_state = initial_state,
      num_simulations = num_simulations,
      scoreHVT_results = scoring,
      trainHVT_results = hvt_results,
      actual_data = expost_forecasting,
      raw_dataset = complete_dataset,
      k = k_val,
      handle_problematic_states = length(problematic_states) > 0,
      n_nearest_neighbor = n_nn,
      mae_metric = mae_metrics[1],
      show_simulation = FALSE,
      time_column = time_column,
      precomputed_problematic_states = problematic_states,
      plot_mode = "mae-only"
    )
    
    # Extract MAE from MSM results
    mae_results <- extract_mae_from_msm_results(msm_result, mae_metrics)
    
    # Cleanup heavy objects
    rm(msm_result, temporal_data_all, scored_data)
    gc(verbose = FALSE)
    
    # Return success with models
    if (!is.null(mae_results) && !is.na(mae_results[[mae_metrics[1]]])) {
      return(list(
        success = TRUE,
        k = k_val,
        nn = n_nn,
        mae_results = mae_results
        ))
    } else {
      return(list(success = FALSE, k = k_val, nn = n_nn, error = "MAE extraction failed"))
    }
    
  }, error = function(e) {
    # MSM failed (could be validation, clustering, or simulation failure)
    gc(verbose = FALSE)
    return(list(success = FALSE, k = k_val, nn = n_nn, error = as.character(e$message)))
  })
}

# Extract MAE values from MSM results
extract_mae_from_msm_results <- function(msm_result, mae_metrics) {
  mae_results <- list()
  
  # New compact structure from mae-only path
  if (!is.null(msm_result$mae_by_metric)) {
    for (metric in mae_metrics) {
      mm <- msm_result$mae_by_metric[[metric]]
      if (!is.null(mm)) {
        centroid_maes <- sapply(mm$centroid_mae_list, function(x) x[["mae"]])
        mae_results[[metric]] <- mean(centroid_maes, na.rm = TRUE)
      } else {
        mae_results[[metric]] <- NA
      }
    }
    return(mae_results)
  }
  
  # Backward-compatible path (plots-based)
  for (metric in mae_metrics) {
    if (!is.null(msm_result$plots) && length(msm_result$plots) >= 2) {
      centroid_maes <- sapply(msm_result$plots[[1]], function(x) x[["mae"]])
      mae_results[[metric]] <- mean(centroid_maes, na.rm = TRUE)
    } else {
      mae_results[[metric]] <- NA
    }
  }
  mae_results
}

# Run baseline simulation using MSM without problematic state handling
run_baseline_simulation <- function(transition_matrix, temporal_data_train, scoring, hvt_results,
                                    initial_state, centroid_data_for_mae, mae_metrics, raw_dataset_stats,
                                    time_column, entire_dataset, expost_forecasting, num_simulations,
                                    time_values, n_ahead) {
  
  # Create temporal data for MSM
  scored_data <- scoring$scoredPredictedData
  complete_dataset <- rbind(entire_dataset, expost_forecasting)
  scored_data[[time_column]] <- as.POSIXct(complete_dataset[[time_column]])
  temporal_data_all <- scored_data %>% 
    dplyr::select(dplyr::all_of(c(time_column, "Cell.ID")))
  
  tryCatch({
    # Set optimization flag for hvt_msm_opt
    options(hvt.msm.optimization = TRUE)
    
      # Baseline MSM call: no problematic state handling
      msm_result <- msm(
      state_time_data = temporal_data_all,
      forecast_type = "ex-post",
      transition_probability_matrix = transition_matrix,
      initial_state = initial_state,
      num_simulations = num_simulations,
      scoreHVT_results = scoring,
      trainHVT_results = hvt_results,
      actual_data = expost_forecasting,
      raw_dataset = complete_dataset,
      handle_problematic_states = FALSE,  # Baseline = no problematic state handling
      mae_metric = mae_metrics[1],
      show_simulation = FALSE,
      time_column = time_column,
      plot_mode = "mae-only"
    )
    
    # Extract MAE from MSM results
    mae_results <- extract_mae_from_msm_results(msm_result, mae_metrics)
    
    # Cleanup
    rm(msm_result, temporal_data_all, scored_data)
    gc(verbose = FALSE)
    
    return(mae_results)
    
  }, error = function(e) {
    # Return NA for all metrics if baseline fails
    mae_results <- list()
    for (metric in mae_metrics) {
      mae_results[[metric]] <- NA
    }
    return(mae_results)
  })
}

# Process all k-nn combinations with pre-filtering checkpoint for invalid combinations
process_all_k_nn_combinations <- function(k_local_range, nn_range, problematic_states,
                                          centroid_2d_points, clustering_data, hvt_results,
                                          scoring, nclust, mae_metric, all_attempts_by_metric,
                                          prob_trans_matx, temporal_data_complete, initial_state, 
                                          centroid_data_for_mae, raw_dataset_stats, time_column,
                                          entire_dataset, expost_forecasting, num_simulations,
                                          time_values, n_ahead, parallel,
                                          verbose = FALSE) {
  
  # Generate ALL possible k-nn combinations
  all_combinations <- expand.grid(k = k_local_range, nn = nn_range)
  
  if (verbose) {
    cat("  Total k-nn combinations:", nrow(all_combinations), "\n")
  }
  
  # CHECKPOINT: Pre-filter mathematically invalid combinations
  valid_combinations <- list()
  invalid_combinations <- list()
  
  for (i in seq_len(nrow(all_combinations))) {
    k_val <- all_combinations$k[i]
    nn_val <- all_combinations$nn[i]
    
    # Check 1: k must be less than or equal to number of cells
    if (k_val > nclust) {
      invalid_combinations[[length(invalid_combinations) + 1]] <- list(
        k = k_val,
        nn = nn_val,
        status = paste0("Mathematical bound reached - Invalid k (", k_val, ") for given n_cells (", nclust, ")")
      )
      next
    }
    
    # Check 2: nn must be <= (cells - k) [mathematical upper bound]
    max_possible_nn <- nclust - k_val
    if (nn_val > max_possible_nn) {
      invalid_combinations[[length(invalid_combinations) + 1]] <- list(
        k = k_val,
        nn = nn_val,
        status = paste0("Mathematical bound reached - Invalid nn (", nn_val, ") for given n_cells (", nclust, ") and k (", k_val, ")")
      )
      next
    }
    
    # Passed both checks - valid combination
    valid_combinations[[length(valid_combinations) + 1]] <- list(
      k = k_val,
      nn = nn_val,
      status = "ready_for_msm",
      valid = TRUE
    )
  }
  
  # Store invalid combinations in all_attempts_by_metric
  if (length(invalid_combinations) > 0) {
    for (combo in invalid_combinations) {
      for (metric in mae_metric) {
        idx <- length(all_attempts_by_metric[[metric]]) + 1
        all_attempts_by_metric[[metric]][[idx]] <- data.frame(
          `Number of Cells` = nclust,
          k = as.character(combo$k),
          `Number of nearest neighbors` = as.character(combo$nn),
          mae = NA,
          status = combo$status,  # Already contains "ERROR: " prefix
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }
  
  if (verbose) {
    cat("  Valid combinations:", length(valid_combinations), "\n")
    cat("  Pre-filtered (invalid):", length(invalid_combinations), "\n")
  }
  
  # Run MSM simulations for VALID combinations only (invalid ones already filtered)
  simulation_results <- list()
  
  if (length(valid_combinations) > 0) {
    # Simulation function for valid combinations
    simulation_function <- function(combo) {
      # Call run_single_simulation which calls MSM
      result <- run_single_simulation(
        k_val = combo$k, n_nn = combo$nn, cluster_data = NULL,
        transition_matrix = prob_trans_matx, temporal_data_train = NULL,
        scoring = scoring, hvt_results = hvt_results, initial_state = initial_state,
        problematic_states = problematic_states, centroid_data_for_mae = NULL,
        mae_metrics = mae_metric, raw_dataset_stats = NULL,
        time_column = time_column, entire_dataset = entire_dataset,
        expost_forecasting = expost_forecasting, num_simulations = num_simulations,
        time_values = NULL, n_ahead = NULL
      )
      return(result)
    }
    
    # Execute ALL combinations
    if (parallel) {
      if (!requireNamespace("furrr", quietly = TRUE) || !requireNamespace("future", quietly = TRUE)) {
        stop("Packages 'furrr' and 'future' are required for parallel processing. ",
             "Please install them with: install.packages(c('furrr', 'future'))")
      }
      simulation_results <- furrr::future_map(valid_combinations, simulation_function,
                                       .options = furrr::furrr_options(seed = TRUE, packages = c("dplyr", "data.table")))
    } else {
      simulation_results <- purrr::map(valid_combinations, simulation_function)
    }
    
    if (verbose) {
      successful_count <- sum(sapply(simulation_results, function(x) x$success %||% FALSE))
      cat("  Completed:", length(simulation_results), "simulations,", successful_count, "successful\n")
    }
  }
  
  return(list(simulation_results = simulation_results, all_attempts = all_attempts_by_metric))
}

# Process simulation results - lightweight version
process_simulation_results <- function(simulation_results, mae_metric, nclust, all_attempts_by_metric) {
  
  if (length(simulation_results) == 0) {
    formatted_results <- list()
    best_results <- list()
    for (metric in mae_metric) {
      formatted_results[[metric]] <- data.frame()
      best_results[[metric]] <- data.frame()
    }
    return(list(
      successful_results = formatted_results,
      best_result = best_results,
      all_attempts = all_attempts_by_metric
    ))
  }
  
  formatted_results <- list()
  best_results <- list()
  successful_results_list <- list()
  
  # Initialize lists for each metric
  for (metric in mae_metric) {
    successful_results_list[[metric]] <- list()
  }
  
  # Process each simulation result
  for (result in simulation_results) {
    if (result$success && !is.null(result$mae_results)) {
      # Process successful results
      for (metric in mae_metric) {
        # Add to all attempts
        idx <- length(all_attempts_by_metric[[metric]]) + 1
        all_attempts_by_metric[[metric]][[idx]] <- data.frame(
          `Number of Cells` = nclust, 
          k = as.character(result$k), 
          `Number of nearest neighbors` = as.character(result$nn), 
          mae = round(result$mae_results[[metric]], 4), 
          status = "Successful Iteration by Handling Problematic state", 
          stringsAsFactors = FALSE, 
          check.names = FALSE
        )
        
        # Add to successful results WITHOUT models
        success_idx <- length(successful_results_list[[metric]]) + 1
        successful_results_list[[metric]][[success_idx]] <- data.frame(
          `Number of Cells` = nclust, 
          k = as.character(result$k), 
          `Number of nearest neighbors` = as.character(result$nn), 
          mae = round(result$mae_results[[metric]], 4),
          status = "Successful Iteration by Handling Problematic state",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    } else {
      # Process failed results
      for (metric in mae_metric) {
        idx <- length(all_attempts_by_metric[[metric]]) + 1
        status_msg <- if (!is.null(result$error)) {
          result$error  # Message comes as-is from MSM (already formatted)
        } else {
          "ERROR: MSM failed"
        }
        all_attempts_by_metric[[metric]][[idx]] <- data.frame(
          `Number of Cells` = nclust, 
          k = as.character(result$k), 
          `Number of nearest neighbors` = as.character(result$nn), 
          mae = NA, 
          status = status_msg, 
          stringsAsFactors = FALSE, 
          check.names = FALSE
        )
      }
    }
  }
  
  # Format successful results and find best combinations WITHOUT models
  for (metric in mae_metric) {
    if (length(successful_results_list[[metric]]) > 0) {
      # Extract configs and de-duplicate strictly on the 5 columns
      configs <- purrr::map_dfr(successful_results_list[[metric]], ~ .x)
      configs <- dplyr::distinct(configs, `Number of Cells`, k, `Number of nearest neighbors`, mae, status, .keep_all = FALSE)
      
      # Ensure numeric conversion for sorting
      configs$k_numeric <- as.numeric(configs$k)
      configs$nn_numeric <- as.numeric(configs$`Number of nearest neighbors`)
      configs$mae_numeric <- as.numeric(configs$mae)
      
      # Deduplicate and sort, keep only required 5 cols
      configs <- dplyr::distinct(configs, .data$`Number of Cells`, .data$k, .data$`Number of nearest neighbors`, .data$mae, .keep_all = TRUE)
      sort_order <- order(configs$`Number of Cells`, configs$k_numeric, configs$nn_numeric)
      formatted <- configs[sort_order, c("Number of Cells","k","Number of nearest neighbors","mae","status")]
      formatted_results[[metric]] <- formatted
      
      # Find best combination WITHOUT storing models
      best_idx <- which.min(configs$mae_numeric)
      best_combination <- configs[best_idx, ] %>%
        dplyr::select(-.data$k_numeric, -.data$nn_numeric, -.data$mae_numeric)
      best_results[[metric]] <- best_combination[, c("Number of Cells","k","Number of nearest neighbors","mae","status")]
      
    } else {
      formatted_results[[metric]] <- data.frame()
      best_results[[metric]] <- data.frame()
    }
  }
  
  # Sort all attempts
  for (metric in mae_metric) {
    if (length(all_attempts_by_metric[[metric]]) > 0) {
      combined_attempts <- do.call(rbind, all_attempts_by_metric[[metric]])
      
      # Sort by cells, k, nn
      combined_attempts$k_sort <- ifelse(combined_attempts$k == "-", 999, as.numeric(combined_attempts$k))
      combined_attempts$nn_sort <- ifelse(combined_attempts$`Number of nearest neighbors` == "-", 999, 
                                          as.numeric(combined_attempts$`Number of nearest neighbors`))
      
      combined_attempts <- combined_attempts %>%
        dplyr::arrange(.data$`Number of Cells`, .data$k_sort, .data$nn_sort) %>%
        dplyr::select(-.data$k_sort, -.data$nn_sort)
      
      # Convert back to list format
      all_attempts_by_metric[[metric]] <- split(combined_attempts, seq_len(nrow(combined_attempts)))
      all_attempts_by_metric[[metric]] <- lapply(all_attempts_by_metric[[metric]], as.data.frame)
    }
  }
  
  gc(verbose = FALSE)
  
  return(list(
    successful_results = formatted_results,
    best_result = best_results,
    all_attempts = all_attempts_by_metric
  ))
}

# ============================================================================
# PROBLEMATIC STATE DETECTION FUNCTIONS
# ============================================================================

# Detect all types of problematic states in a transition matrix
detect_problematic_states <- function(prob_trans_matx, scored_cells) {
  # Missing states: states in scored_cells but not in transition matrix
  transition_states <- unique(prob_trans_matx$Current_State)
  missing_states <- setdiff(scored_cells, transition_states)
  
  # Self-transition states (100% self-transition)
  self_transition_states <- prob_trans_matx %>%
    dplyr::group_by(.data$Current_State) %>%
    dplyr::summarize(
      is_self_only = dplyr::n() == 1 && all(.data$Current_State == .data$Next_State),
      .groups = 'drop'
    ) %>%
    dplyr::filter(.data$is_self_only == TRUE) %>%
    dplyr::pull(.data$Current_State)
  
  # Cyclic states detection
  cyclic_states <- find_cyclic_states(prob_trans_matx)
  
  # Combine all problematic states
  all_problematic <- unique(c(missing_states, self_transition_states, cyclic_states))
  
  return(list(
    missing_states = missing_states,
    self_transition_states = self_transition_states,
    cyclic_states = cyclic_states,
    all_problematic = all_problematic
  ))
}

# Detect cyclic states in a transition matrix
find_cyclic_states <- function(trans_table) {
  all_states <- unique(c(trans_table$Current_State, trans_table$Next_State))
  state_info <- trans_table %>%
    dplyr::group_by(.data$Current_State) %>%
    dplyr::summarize(next_states = list(unique(.data$Next_State)), .groups = 'drop')
  adjacency <- setNames(lapply(state_info$next_states, unlist), as.character(state_info$Current_State))
  two_way_cycles <- c(); one_way_traps <- c(); processed_pairs <- character(0)
  for (state_a in all_states) {
    if (!as.character(state_a) %in% names(adjacency)) next
    next_states_a <- adjacency[[as.character(state_a)]]
    other_states_a <- next_states_a[next_states_a != state_a]
    for (state_b in other_states_a) {
      pair_key <- paste(sort(c(state_a, state_b)), collapse = "-")
      if (pair_key %in% processed_pairs) next
      processed_pairs <- c(processed_pairs, pair_key)
      if (!as.character(state_b) %in% names(adjacency)) next
      next_states_b <- adjacency[[as.character(state_b)]]
      states_in_system <- c(state_a, state_b)
      reaches_outside_a <- setdiff(next_states_a, states_in_system)
      reaches_outside_b <- setdiff(next_states_b, states_in_system)
      if (length(reaches_outside_a) == 0 && length(reaches_outside_b) == 0) {
        a_can_reach_b <- state_b %in% next_states_a
        b_can_reach_a <- state_a %in% next_states_b
        if (a_can_reach_b && b_can_reach_a) two_way_cycles <- c(two_way_cycles, state_a, state_b)
        else if (a_can_reach_b && !b_can_reach_a) {
          if (length(next_states_b) == 1 && next_states_b[1] == state_b) one_way_traps <- c(one_way_traps, state_a, state_b)
        } else if (!a_can_reach_b && b_can_reach_a) {
          if (length(next_states_a) == 1 && next_states_a[1] == state_a) one_way_traps <- c(one_way_traps, state_a, state_b)
        }
      }
    }
  }
  unique(c(two_way_cycles, one_way_traps))
}

# ============================================================================
# MISSING MSM HELPER FUNCTIONS
# ============================================================================

# Validate MSM clustering for problematic states
validate_msm_clustering <- function(cluster_data, problematic_states, n_nearest_neighbor,
                                    centroid_2d_points, verbose = TRUE) {
  if (length(problematic_states) == 0) {
    return(list(is_valid = TRUE, message = "No problematic states found"))
  }
  validation_issues <- c()
  for (prob_state in problematic_states) {
    prob_cluster <- cluster_data$cluster[cluster_data$Cell.ID == prob_state]
    cluster_members <- cluster_data$Cell.ID[cluster_data$cluster == prob_cluster]
    valid_neighbors <- setdiff(cluster_members, c(prob_state, problematic_states))
    if (verbose) {
      cat("Problematic state", prob_state, "in cluster", prob_cluster,
          "- Valid neighbors:", length(valid_neighbors), "\n")
    }
    if (length(valid_neighbors) == 0) {
      validation_issues <- c(validation_issues,
                             paste0("State ", prob_state, " has no valid neighbors in cluster ", prob_cluster))
    } else if (length(valid_neighbors) < n_nearest_neighbor) {
      validation_issues <- c(validation_issues,
                             paste0("State ", prob_state, " has only ", length(valid_neighbors),
                                    " valid neighbors, but ", n_nearest_neighbor, " requested"))
    }
  }
  if (length(validation_issues) > 0) {
    return(list(
      is_valid = FALSE,
      message = paste(validation_issues, collapse = "; ")
    ))
  }
  list(is_valid = TRUE, message = "MSM clustering validation passed")
}

# Choose a nearest non-problematic neighbor for a problematic state
find_nearest_neighbor_enhanced <- function(problematic_state,
                                           centroid_2d_points,
                                           cluster_data,
                                           problematic_states,
                                           n_nearest_neighbor,
                                           scored_cells,
                                           initial_state) {
  if (is.null(cluster_data)) {
    valid_states <- setdiff(scored_cells, problematic_states)
    if (length(valid_states) > 0) return(sample(valid_states, 1))
    warning("No valid fallback states available, returning initial state")
    return(initial_state)
  }
  current_cluster <- cluster_data$cluster[cluster_data$Cell.ID == problematic_state]
  coords <- centroid_2d_points[centroid_2d_points$Cell.ID %in%
                                 cluster_data$Cell.ID[cluster_data$cluster == current_cluster], ]
  current_coords <- coords[coords$Cell.ID == problematic_state, c("x", "y")]
  distances <- sqrt((coords$x - current_coords$x)^2 + (coords$y - current_coords$y)^2)
  valid_neighbors <- coords$Cell.ID[!coords$Cell.ID %in% c(problematic_state, problematic_states)]
  if (length(valid_neighbors) == 0) {
    warning(paste0("No valid neighbors for problematic state ", problematic_state,
                   " in cluster ", current_cluster, ". Using global fallback."))
    all_coords <- centroid_2d_points
    all_distances <- sqrt((all_coords$x - current_coords$x)^2 +
                            (all_coords$y - current_coords$y)^2)
    global_valid <- setdiff(all_coords$Cell.ID, c(problematic_state, problematic_states))
    if (length(global_valid) > 0) {
      global_neighbor_distances <- all_distances[all_coords$Cell.ID %in% global_valid]
      closest_global <- global_valid[which.min(global_neighbor_distances)]
      return(closest_global)
    } else {
      warning("No valid neighbors globally available, returning initial state")
      return(initial_state)
    }
  }
  neighbor_distances <- distances[coords$Cell.ID %in% valid_neighbors]
  num_neighbors <- min(length(valid_neighbors), n_nearest_neighbor)
  nearest_n <- valid_neighbors[order(neighbor_distances)][1:num_neighbors]
  neighbor_probs <- 1/neighbor_distances[order(neighbor_distances)][1:num_neighbors]
  neighbor_probs <- neighbor_probs/sum(neighbor_probs)
  random_shock <- round(runif(min=0, max=1, n=1), 4)
  cumulative_probs <- cumsum(neighbor_probs)
  nearest_n[which.max(random_shock <= cumulative_probs)]
}

# Single simulation step for MSM
simulate_step_enhanced <- function(i, current_state, random_val,
                                   transition_probability_matrix,
                                   centroid_2d_points,
                                   cluster_data,
                                   problematic_states,
                                   n_nearest_neighbor,
                                   scored_cells,
                                   initial_state) {
  random_shock <- if (is.null(random_val)) round(runif(min=0, max=1, n=1), 4) else random_val
  if (i == 1) return(initial_state)
  
  # Handle problematic states only if handle_problematic_states = TRUE (indicated by cluster_data being non-NULL)
  # If cluster_data is NULL, just follow the transition matrix normally, even for problematic states
  if (!is.null(cluster_data) && current_state %in% problematic_states) {
    return(find_nearest_neighbor_enhanced(current_state, centroid_2d_points, cluster_data,
                                          problematic_states, n_nearest_neighbor, scored_cells, initial_state))
  }
  transition <- subset(transition_probability_matrix, Current_State == current_state)
  if (nrow(transition) == 0) {
    if (current_state %in% scored_cells) return(current_state)
    else return(find_nearest_neighbor_enhanced(current_state, centroid_2d_points, cluster_data,
                                               problematic_states, n_nearest_neighbor, scored_cells, initial_state))
  }
  transition <- transition[order(-transition$Transition_Probability), ]
  transition$cumulative_prob <- cumsum(transition$Transition_Probability)
  next_state <- transition$Next_State[which.max(random_shock <= transition$cumulative_prob)]
  next_state <- as.numeric(next_state)
  
  # Replace problematic states only if handle_problematic_states = TRUE (cluster_data is non-NULL)
  # If handle_problematic_states = FALSE, follow transition matrix even if it leads to problematic states
  if (!is.null(cluster_data) && next_state %in% problematic_states) {
    return(find_nearest_neighbor_enhanced(next_state, centroid_2d_points, cluster_data,
                                          problematic_states, n_nearest_neighbor, scored_cells, initial_state))
  }
  # Always replace invalid states (not in scored_cells)
  if (!next_state %in% scored_cells) {
    return(find_nearest_neighbor_enhanced(next_state, centroid_2d_points, cluster_data,
                                          problematic_states, n_nearest_neighbor, scored_cells, initial_state))
  }
  next_state
}

# Simulate a full MSM sequence
simulate_sequence_enhanced <- function(sim_index, n_ahead,
                                       initial_state, transition_probability_matrix,
                                       centroid_2d_points, cluster_data, problematic_states,
                                       n_nearest_neighbor, scored_cells) {
  current_state <- initial_state
  simulated_values <- sapply(1:n_ahead, function(i) {
    random_val <- NULL
    current_state <<- simulate_step_enhanced(i, current_state, random_val,
                                            transition_probability_matrix,
                                            centroid_2d_points, cluster_data,
                                            problematic_states, n_nearest_neighbor,
                                            scored_cells, initial_state)
    current_state
  })
  as.numeric(simulated_values)
}

# Adjust invalid initial state using neighbors
adjust_initial_state <- function(initial_state, transition_cells, problematic_states,
                                 scored_cells, centroid_2d_points, cluster_data,
                                 n_nearest_neighbor) {
  if (!(initial_state %in% transition_cells) || initial_state %in% problematic_states) {
    return(find_nearest_neighbor_enhanced(initial_state, centroid_2d_points, cluster_data,
                                          problematic_states, n_nearest_neighbor,
                                          scored_cells, initial_state))
  }
  initial_state
}
