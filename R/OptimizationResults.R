#' @name OptimizationResults
#' @title Table display of all combinations performed in `HVTMSMoptimization`
#' @description This function helps to tabulate the `HVTMSMoptimization` all iterations
#' performed (successful and failed combinations)
#' @param msm_results Dataframe. The `all_results` object of `HVTMSMoptimization` 
#' output based on a specific mae_metric
#' @param show_top_global Numeric. To tabulate top n lowest MAE results
#' @return Table of all iterations will be displayed
#' @author Vishwavani <vishwavani@@mu-sigma.com>, Nithya <nithya.sn@@mu-sigma.com>
#' @keywords Hyperparameter_Tuning
#' @include HVTMSMoptimization.R
#' @importFrom magrittr %>%
#' @export OptimizationResults


OptimizationResults <- function(msm_results,show_top_global = NULL) {
  
  color_cell_min = "#d4f6d4"
  color_global_min = "#ffebee"
  
  # Check for DT package (in Suggests)
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for this function. Install it with: install.packages('DT')",
         call. = FALSE)
  }
  
  flag <- NULL
  # Input validation and data extraction
  if (is.data.frame(msm_results)) {
    df <- msm_results
  } else if (!is.null(msm_results[["all_results"]])) {
    df <- msm_results[["all_results"]]
  } else {
    return(DT::datatable(data.frame(Message = "No results to display"), rownames = FALSE))
  }
  
  # Data type conversions
  df <- df %>%
    mutate(
      `Number of Cells` = suppressWarnings(as.integer(`Number of Cells`)),
      k = suppressWarnings(as.integer(k)),
      `Number of nearest neighbors` = suppressWarnings(as.integer(`Number of nearest neighbors`)),
      mae = suppressWarnings(as.numeric(mae))
    )
  
  # Optional nclust filter
  # if (!is.null(nclust_filter)) {
  #   df <- dplyr::filter(df, `Number of Cells` %in% nclust_filter)
  # }
  # if (nrow(df) == 0) {
  #   return(DT::datatable(data.frame(Message = "No results to display"), rownames = FALSE))
  # }
  

  
  # Calculate best results per cell count and globally
  df <- df %>%
    group_by(`Number of Cells`) %>%
    mutate(is_cell_min = mae == min(mae, na.rm = TRUE)) %>%
    ungroup()
  
  global_min <- suppressWarnings(min(df$mae, na.rm = TRUE))
  if (!is.finite(global_min)) global_min <- NA_real_
  
  # Apply top-N global filter if specified
  if (!is.null(show_top_global)) {
    df <- df %>%
      arrange(mae) %>%
      slice(seq_len(min(show_top_global, n())))
  }
  
  # Create highlighting flags
  if (!is.null(show_top_global)) {
    # Rank top-N globally and color-code top 3 as Gold/Silver/Bronze
    df <- df %>%
      mutate(
        .rank_global = dplyr::row_number(),
        flag = dplyr::case_when(
          .rank_global == 1 ~ "rank_1",
          .rank_global == 2 ~ "rank_2",
          .rank_global == 3 ~ "rank_3",
          TRUE              ~ "top_n"
        )
      )
  } else {
    df <- df %>%
      mutate(
        flag = dplyr::case_when(
          !is.na(global_min) & mae == global_min ~ "overall_min",
          is_cell_min                           ~ "cell_min",
          TRUE                                  ~ "none"
        )
      )
  }
  
  # Sort results
  if (!is.null(show_top_global)) {
    df <- df %>% arrange(mae, `Number of Cells`, k, `Number of nearest neighbors`)
  } else {
    df <- df %>% arrange(`Number of Cells`, k, `Number of nearest neighbors`, mae)
  }
  
  # Select final columns for display - ONLY status, not status_category and display_status
  final_df <- df %>%
    select(`Number of Cells`, k, `Number of nearest neighbors`, mae, status, flag)
  # Round MAE values for display consistency
  final_df$mae <- round(final_df$mae, 4)
  
  # Column indices for DT options (0-based)
  col_flag      <- which(names(final_df) == "flag") - 1
  col_mae       <- which(names(final_df) == "mae") - 1
  col_cells     <- which(names(final_df) == "Number of Cells") - 1
  col_k         <- which(names(final_df) == "k") - 1
  col_neighbors <- which(names(final_df) == "Number of nearest neighbors") - 1
  
  # Dynamic length menu calculation
  calculate_dynamic_length_menu <- function(total_entries, base_step = 100) {
    max_option <- ceiling(total_entries / base_step) * base_step
    max_option <- max(max_option, 100)
    options <- seq(base_step, by = base_step, length.out = max_option / base_step)
    options <- c(25, options)
    return(options)
  }
  
  dynamic_length_menu <- calculate_dynamic_length_menu(nrow(final_df))
  
  # Create user-friendly headers
  display_names <- names(final_df)
  display_names[display_names == "Number of nearest neighbors"] <- "NN (Nearest Neighbors)"
 # display_names[display_names == "status"] <- "Optimization Status"
  
  # Create and configure DT table
  dt_table <- DT::datatable(
    final_df,
    colnames = display_names,
    options = list(
      pageLength = if (!is.null(show_top_global)) min(show_top_global, 25) else 25,
      scrollX = TRUE,
      lengthMenu = dynamic_length_menu,
      columnDefs = list(list(visible = FALSE, targets = col_flag)),
      order = if (!is.null(show_top_global)) {
        list(list(col_mae, "asc"), list(col_cells, "asc"), list(col_k, "asc"), list(col_neighbors, "asc"))
      } else {
        list(list(col_cells, "asc"), list(col_k, "asc"), list(col_neighbors, "asc"), list(col_mae, "asc"))
      }
    ),
    class = "nowrap display",
    rownames = FALSE
  ) %>%
    DT::formatRound(columns = "mae", digits = 4) %>%
    {
      # Choose styling depending on whether we are showing Top-N globally
      if (!is.null(show_top_global)) {
        # Gold / Silver / Bronze for ranks 1/2/3 respectively.
        gold   <- "#d4f6d4"  # 1st
        silver <- "#C0C0C0"  # 2nd
        bronze <- "#E5E4E2"  # 3rd
        DT::formatStyle(
          ., "flag", target = "row",
          backgroundColor = DT::styleEqual(
            c("rank_1", "rank_2", "rank_3"),
            c(gold, silver, bronze)
          ),
          fontWeight = DT::styleEqual(
            c("rank_1", "rank_2", "rank_3"),
            c("bold", "bold", "bold")
          ),
          color = DT::styleEqual(
            c("rank_1", "rank_2", "rank_3"),
            c("#2e7d32", "#434343","#656565")
          )
        )
      } else {
        DT::formatStyle(
          ., "flag", target = "row",
          backgroundColor = DT::styleEqual(
            c("cell_min", "overall_min", "top_n"),
            c(color_cell_min, color_cell_min, color_cell_min)
          ),
          fontWeight = DT::styleEqual(
            c("cell_min", "overall_min", "top_n"),
            c("bold", "bold", "bold")
          ),
          color = DT::styleEqual(
            c("cell_min", "overall_min", "top_n"),
            c("#2e7d32", "#2e7d32", "#2e7d32")
          )
        )
      }
    }
  
  # Additional emphasis on global minimum MAE
  if (!is.na(global_min)) {
    dt_table <- dt_table %>% 
      DT::formatStyle("mae", fontWeight = DT::styleEqual(global_min, "bold"))
  }
  
  return(dt_table)
}