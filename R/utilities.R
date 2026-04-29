#' @keywords internal


extract_cell_coordinates <- function(hvt.results) {
  hvt_res1 <- hvt.results[[2]][[1]]$`1`
  hvt_res2 <- hvt.results[[3]]$summary$Cell.ID
  b <- stats::na.omit(as.vector((1:length(hvt_res1))[hvt_res2]))
  coordinates <- do.call(rbind.data.frame, lapply(1:length(hvt_res1), function(x) hvt_res1[[x]]$pt))
  colnames(coordinates) <- c("x", "y")
  coordinates$Cell.ID <- b
  coordinates
}

calculate_polygon_centroid <- function(x_coords, y_coords) {
  n <- length(x_coords)
  if (n < 3) {
    # If not enough points, return mean
    return(list(x = mean(x_coords), y = mean(y_coords)))
  }
  # Close the polygon if not already closed
  if (x_coords[1] != x_coords[n] || y_coords[1] != y_coords[n]) {
    x_coords <- c(x_coords, x_coords[1])
    y_coords <- c(y_coords, y_coords[1])
    n <- n + 1
  }
  # Calculate signed area using shoelace formula
  signed_area <- 0
  cx <- 0
  cy <- 0
  for (i in 1:(n - 1)) {
    cross_product <- x_coords[i] * y_coords[i + 1] - x_coords[i + 1] * y_coords[i]
    signed_area <- signed_area + cross_product
    cx <- cx + (x_coords[i] + x_coords[i + 1]) * cross_product
    cy <- cy + (y_coords[i] + y_coords[i + 1]) * cross_product
  }
  signed_area <- signed_area / 2
  if (abs(signed_area) < 1e-10) {
    # Degenerate polygon, return mean
    return(list(x = mean(x_coords[1:(n-1)]), y = mean(y_coords[1:(n-1)])))
  }
  centroid_x <- cx / (6 * signed_area)
  centroid_y <- cy / (6 * signed_area)
  return(list(x = centroid_x, y = centroid_y))
}


get_cell_label_alignment <- function(position, renderer = c("ggplot", "plotly")) {
  renderer <- match.arg(renderer)
  
  if (renderer == "ggplot") {
    alignments <- list(
      center = list(hjust = 0.5,  vjust =  0.5),
      right  = list(hjust = -0.5, vjust =  0.5),
      left   = list(hjust = 1.5,  vjust =  0.5),
      bottom = list(hjust = 0.5,  vjust =  1.5),
      top    = list(hjust = 0.5,  vjust = -0.5)
    )
  } else {
    alignments <- list(
      center = list(xshift = 0,  yshift =  0, xanchor = "center", yanchor = "middle"),
      right  = list(xshift = 5,  yshift =  0, xanchor = "left",   yanchor = "middle"),
      left   = list(xshift = -5, yshift =  0, xanchor = "right",  yanchor = "middle"),
      bottom = list(xshift = 0,  yshift = -7, xanchor = "center", yanchor = "top"),
      top    = list(xshift = 0,  yshift =  7, xanchor = "center", yanchor = "bottom")
    )
  }
  rlang::`%||%`(alignments[[position]], alignments$bottom)
}

build_boundary_coords <- function(plotList) {
  lapply(plotList, function(x) {
    data.frame(
      "Segment.Level" = x[["Segment.Level"]],
      "Segment.Parent" = x[["Segment.Parent"]],
      "Segment.Child" = x[["Segment.Child"]],
      "x" = x$pt["x"],
      "y" = x$pt["y"],
      "bp.x" = I(x$x),
      "bp.y" = I(x$y)
    )
  }) %>% bind_rows(.)
}


subtract_predicted_actual <- function(data, actual_prefix = "act_", predicted_prefix = "pred_") {
  actual_cols <- grep(paste0("^", actual_prefix), names(data), value = TRUE)
  df_new <- data.frame(matrix(ncol = 1, nrow = nrow(data)))
  temp0  <- data.frame(matrix(nrow = nrow(data)))
  for (col in actual_cols) {
    predicted_col <- gsub(actual_prefix, predicted_prefix, col)
    if (predicted_col %in% names(data)) {
      temp0[[predicted_col]] <- abs(data[[col]] - data[[predicted_col]])
    }
  }
  temp0 <- temp0 %>% purrr::discard(~ all(is.na(.) | . == ""))
  df_new[, 1] <- rowMeans(temp0)
  return(df_new)
}