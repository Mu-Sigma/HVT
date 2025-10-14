#' @name HVTMSMoptimization
#' @title HVT-MSM Optimization Function
#' @description This function runs multiple iterations/experiments over the dataset across 
#' different cell counts, and computes the MAE. If a given cell configuration 
#' not results in problematic states, the baseline simulation is used; otherwise, 
#' the simulation proceeds with problematic state handling. This process helps identify 
#' the best-performing model (lowest MAE). In essence, it performs 
#' trainHVT → scoreHVT → transition probability estimation on the training 
#' data, followed by msm simulation on the ex-post (test) dataset.  
#' Note: This is not applicable for ex-ante analysis.
#' @param entire_dataset Dataframe. Train dataset for model training
#' @param expost_forecasting Dataframe. Test dataset for ex-post forecasting  
#' @param time_column Character. Name of the time column
#' @param ncell_range Numeric vector. Range of cells to run experiments (default 3:5)
#' @param k_range Numeric vector. Range of clusters to run experiments (default: 2:9)
#' @param nn_range Numeric vector. Range of nearest neighbors to run experiments (default: 2:7)
#' @param num_simulations Integer. Number of simulations (default: 10)
#' @param mae_metric Character. MAE calculation method(s): "mean", "median", "mode", or
#'  "all" (default: "median")
#' @param hvt_params List. Set of parameters for Model Training (refer trainHVT)
#' @param parallel Character. Whether to use parallel processing (default: TRUE)
#' @param verbose Character. Whether to print progress information (default: TRUE)
#' @param parallel_strategy Character. Parallel processing strategy: "multisession", 
#' "multicore", etc. (default: "multisession")
#' @param max_workers Maximum number of parallel workers 
#' (default: NULL for auto-detect) 
#' @return List containing optimization results for each MAE metric:
#' \item{[[successful_results]] }{All successful parameter combinations} 
#' \item{[[nclust_best_results]] }{Best combination for each cell}
#' \item{[[overall_best]] }{Overall best parameter combination}
#' \item{[[all_results]] }{All attempted combinations with status}
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Nithya <nithya.sn@@mu-sigma.com>
#' @keywords Hyperparameter_Tuning
#' @include HVTMSM_support.R
#' @importFrom magrittr %>%
#' @export HVTMSMoptimization

HVTMSMoptimization <- function(entire_dataset,
                               expost_forecasting,
                               time_column,
                               ncell_range = 3:5,
                               k_range = 2:9,
                               nn_range = 2:7,
                               num_simulations = 10,
                               mae_metric,
                               hvt_params = list(
                                            depth = 1,
                                            quant.err = 0.2,
                                            normalize = TRUE,
                                            distance_metric = "L1_Norm",
                                            error_metric = "max",
                                            dim_reduction_method = "sammon"
                              ),
                               parallel = TRUE,
                               verbose = TRUE,
                               parallel_strategy = "multisession",
                               max_workers = NULL) {
  
  # Validate inputs
  if (!time_column %in% names(entire_dataset)) {
    stop("Time column '", time_column, "' not found in entire_dataset")
  }
  if (!time_column %in% names(expost_forecasting)) {
    stop("Time column '", time_column, "' not found in expost_forecasting")
  }
  
  if (hvt_params$depth >= 2) {
    stop("HVTMSMOptimization doesn't work for depth equal to or greater than 2")
  }
  
  # Normalize MAE metric
  if (length(mae_metric) == 1 && mae_metric == "all") {
    mae_metric <- c("mean", "median", "mode")
  }
  if (!all(mae_metric %in% c("median", "mean", "mode"))) {
    stop("mae_metric must be one or more of: 'mean', 'median', 'mode', or 'all'")
  }
  
  # Validate k_range and nn_range against ncell_range
  min_ncell <- min(ncell_range)
  max_k <- max(k_range)
  max_nn <- max(nn_range)
  
  # Check if k_range is compatible with ncell_range
  if (max_k >= min_ncell) {
    stop("Invalid parameter ranges: max(k_range) = ", max_k, 
         " must be less than min(ncell_range) = ", min_ncell, 
         ".\nk represents number of clusters and must always be less than the number of cells available.",
         "\nSuggestion: Set k_range with max value < ", min_ncell, 
         " or increase min(ncell_range) to > ", max_k)
  }
  
  # Check if nn_range is compatible with ncell_range and k_range
  min_k <- min(k_range)
  max_possible_nn <- min_ncell - min_k
  if (max_nn > max_possible_nn) {
    stop("Invalid parameter ranges: max(nn_range) = ", max_nn,
         " exceeds maximum possible neighbors = ", max_possible_nn,
         " (calculated as min(ncell_range) - min(k_range) = ", min_ncell, " - ", min_k, ").",
         "\nSuggestion: Set nn_range with max value <= ", max_possible_nn,
         " or increase min(ncell_range) or reduce min(k_range)")
  }
  
  # Setup parallel processing
  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE) || !requireNamespace("future", quietly = TRUE)) {
      stop("Packages 'furrr' and 'future' are required for parallel processing. ",
           "Please install them with: install.packages(c('furrr', 'future'))")
    }
    if (is.null(max_workers)) {
      max_workers <- min(parallel::detectCores() - 1, 16)
    }
    if (verbose) cat("Setting up parallel processing with", max_workers, "workers\n")
    future::plan(parallel_strategy, workers = max_workers)
    on.exit(future::plan(future::sequential))
  }
  
  # Prepare data
  numeric_dataset <- entire_dataset %>% dplyr::select(-dplyr::all_of(time_column))
  n_ahead <- nrow(expost_forecasting)
  time_values <- expost_forecasting[[time_column]]
  raw_dataset_stats <- calculate_dataset_stats(numeric_dataset)
  
  if (verbose) cat("Starting optimization with", length(ncell_range), "cell configurations\n")
  
  # Pre-compute HVT models
  hvt_models <- precompute_hvt_models(
    entire_dataset = entire_dataset,
    ncell_range = ncell_range,
    hvt_params = hvt_params,
    #dependent_variables = dependent_variables,
    verbose = verbose,
    time_column = time_column
  )
  
  # Main optimization process
  if (verbose) cat("Running optimization...\n")
  
  if (parallel) {
    optimization_results <- furrr::future_map(ncell_range, function(nclust) {
      process_single_nclust_with_precomputed_model(
        nclust = nclust,
        hvt_model = hvt_models[[as.character(nclust)]],
        numeric_dataset = numeric_dataset,
        entire_dataset = entire_dataset,
        expost_forecasting = expost_forecasting,
        time_column = time_column,
        k_range = k_range,
        nn_range = nn_range,
        num_simulations = num_simulations,
        time_values = time_values,
        n_ahead = n_ahead,
        mae_metric = mae_metric,
        #dependent_variables = dependent_variables,
        raw_dataset_stats = raw_dataset_stats,
        parallel = FALSE,
        verbose = verbose
      )
    }, .options = furrr::furrr_options(seed = TRUE))
  } else {
    optimization_results <- purrr::map(ncell_range, function(nclust) {
      process_single_nclust_with_precomputed_model(
        nclust = nclust,
        hvt_model = hvt_models[[as.character(nclust)]],
        numeric_dataset = numeric_dataset,
        entire_dataset = entire_dataset,
        expost_forecasting = expost_forecasting,
        time_column = time_column,
        k_range = k_range,
        nn_range = nn_range,
        num_simulations = num_simulations,
        time_values = time_values,
        n_ahead = n_ahead,
        mae_metric = mae_metric,
        #dependent_variables = dependent_variables,
        raw_dataset_stats = raw_dataset_stats,
        parallel = parallel,
        verbose = verbose
      )
    })
  }
  
  # Collect and format final results
  final_output <- collect_final_results(optimization_results, mae_metric, verbose)
  
  if (verbose) cat("Optimization complete\n")
  
  return(final_output)
}