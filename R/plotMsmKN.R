#' @keywords internal

plotMsmKN <- function(optimization_results) {
  
  # Initialize NSE symbols to avoid R CMD check NOTES
  nclust <- mae <- k <- nn <- is_overall_best <- is_overall_worst <- hover_text <- NULL
  
  
  # Quick guard
  if (is.null(optimization_results)) {
    cat("No results provided.\n")
    return(list(plot = NULL, table = NULL))
  }
  
  # ---------- 1) Standardize a helper to pick columns safely ----------
  pick_col <- function(df, choices) {
    nm <- choices[choices %in% names(df)]
    if (length(nm) == 0) return(NULL)
    nm[1]
  }
  
  # ---------- 2) Prefer ALL RESULTS, fallback to nclust_best_results ----------
  src <- NULL
  if (!is.null(optimization_results$all_results) &&
      nrow(optimization_results$all_results) > 0) {
    src <- optimization_results$all_results
  } else if (!is.null(optimization_results$nclust_best_results) &&
             nrow(optimization_results$nclust_best_results) > 0) {
    src <- optimization_results$nclust_best_results
  } else {
    cat("No MSM optimization results to plot.\n")
    return(list(plot = NULL, table = NULL))
  }
  
  # Identify columns (support old/new schemas)
  nclust_col <- pick_col(src, c("Number_of_Cells","Number of Cells"))
  nn_col     <- pick_col(src, c("Number_of_nearest_neighbors","Number of nearest neighbors"))
  mae_col    <- pick_col(src, c("MAE","mae"))
  k_col      <- pick_col(src, c("k"))
  #metric_col <- pick_col(src, c("metric"))  # optional
  
  if (is.null(nclust_col) || is.null(nn_col) || is.null(mae_col) || is.null(k_col)) {
    stop("Required columns not found in results: need cells/k/nn/MAE.")
  }
  
  # ---------- 3) Build plot_data from ALL rows (ensures one point per tested cell) ----------
  # Coerce to numeric, drop problematic rows with NA in key fields
  src_clean <- src %>%
    dplyr::mutate(
      nclust = suppressWarnings(as.numeric(.data[[nclust_col]])),
      k      = suppressWarnings(as.numeric(.data[[k_col]])),
      nn     = suppressWarnings(as.numeric(.data[[nn_col]])),
      mae    = suppressWarnings(as.numeric(.data[[mae_col]]))
    ) %>%
    tidyr::drop_na(nclust, mae)  # keep only rows we can plot
  
  if (nrow(src_clean) == 0) {
    cat("No plottable rows (all NA after coercion).\n")
    return(list(plot = NULL, table = NULL))
  }
  
  # Choose one row per cell: the true best (min MAE) for that cell
  # If multiple rows tie, slice_min keeps one (with_ties = FALSE).
  plot_data <- src_clean %>%
    dplyr::group_by(nclust) %>%
    dplyr::slice_min(mae, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(nclust)
  
  # ---------- 4) Determine highest and lowest MAE ----------
  min_idx <- which.min(plot_data$mae)
  max_idx <- which.max(plot_data$mae)
  best_nclust <- plot_data$nclust[min_idx]
  worst_nclust <- plot_data$nclust[max_idx]
  
  plot_data <- plot_data %>%
    dplyr::mutate(
      is_overall_best = (nclust == best_nclust),
      is_overall_worst = (nclust == worst_nclust),
      hover_text = paste0(
        "Cells: ", nclust, "<br>",
        "k: ", k, "<br>",
        "nn: ", nn, "<br>",
        "MAE: ", sprintf("%.4f", mae)
      )
    )
  
  # ---------- 5) Build table ----------
  results_table <- plot_data %>%
    dplyr::transmute(
      `Number of Cells`   = nclust,
      k                   = k,
      `Nearest Neighbors` = nn,
      MAE                 = round(mae, 4),
      `Best Result`       = ifelse(is_overall_best, "\u2605", "")
    )
  
  # ---------- 6) Plot ----------
  n_points <- nrow(plot_data)
  point_size <- dplyr::case_when(
    n_points <= 20 ~ 3,
    n_points <= 50 ~ 2,
    n_points <= 100 ~ 1.5,
    TRUE ~ 1
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = nclust, y = mae)) +
    ggplot2::geom_line(color = "blue", alpha = 0.7, linewidth = 0.8) +
    ggplot2::geom_point(
      data = dplyr::filter(plot_data, !is_overall_best & !is_overall_worst),
      ggplot2::aes(text = hover_text),
      color = "blue",
      size = point_size,
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(plot_data, is_overall_best),
      ggplot2::aes(text = hover_text),
      color = "#2e7d32",
      size = point_size * 1.5,
      alpha = 1
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(plot_data, is_overall_worst),
      ggplot2::aes(text = hover_text),
      color = "red",
      size = point_size * 1.5,
      alpha = 1
    ) +
    ggplot2::scale_x_continuous(
      name = "Number of Cells",
      breaks = if (n_points <= 20) {
        plot_data$nclust
      } else if (n_points <= 50) {
        pretty(plot_data$nclust, n = 10)
      } else {
        pretty(plot_data$nclust, n = 8)
      },
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      name = "Mean Absolute Error (MAE)",
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05)),
      labels = function(x) sprintf("%.3f", x)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 20)),
      axis.title = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(
        angle = if (n_points > 30) 45 else 0,
        hjust = if (n_points > 30) 1 else 0.5,
        margin = ggplot2::margin(t = 8)
      ),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey90", linewidth = 0.5),
      panel.grid.major.y = ggplot2::element_line(colour = "grey90", linewidth = 0.5),
      plot.margin = ggplot2::margin(30, 30, 30, 30)
    ) +
    ggplot2::ggtitle("MSM Optimization Results: MAE vs Number of Cells")
  
  interactive_plot <- plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 80, r = 50, t = 80, b = 100),
      hovermode = "closest",
      showlegend = FALSE,
      xaxis = list(title = list(standoff = 20), tickangle = if (n_points > 30) -45 else 0),
      yaxis = list(title = list(standoff = 30))
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c(
        "pan2d", "select2d", "lasso2d", "autoScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      ),
      displaylogo = FALSE,
      responsive = TRUE
    )
  
  best_row <- plot_data[which.min(plot_data$mae), ]
  
  list(
    plot = interactive_plot,
    table = results_table,
    best_nclust = best_row$nclust,
    best_k = best_row$k,
    best_nn = best_row$nn,
    best_mae = round(best_row$mae, 4)
  )
}
