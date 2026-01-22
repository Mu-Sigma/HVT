#' @keywords internal

clusterPlot <- function(dataset, hvt.results, domains.column, highlight_cells = NULL,
                        cell_id = TRUE,
                        cell_id_position = "center",
                        centroids = TRUE,
                        cell_id_size = 4,
                        centroid.size = 1,
                        centroid.color = "black") {
  suppressWarnings({
    
    # ============================================================================
    # CONSTANTS
    # ============================================================================
    LINE_SIZE_NORMAL <- 0.5
    LINE_SIZE_HIGHLIGHT <- 1.5
    WRAP_LINE_LENGTH <- 100
    MAX_TEXT_CHARS <- 500
    CENTROID_BASE_SIZE <- 1.5
    CELL_ID_LABEL_YSHIFT <- -10
    CELL_ID_LABEL_SIZE <- 10
    
    # Define color palette
    DOMAIN_COLORS <- c(
      "#0000FF", "#00FFFF", "#FFD700", "#00FF00", "#FF00FF",
      "#FF0000", "#F012BE", "#85144b", "#3D9970", "#39CCCC",
      "#01FF70", "#DDDDDD", "#AAAAAA", "#FF6F61", "#6B5B95",
      "#88B04B", "#F7CAC9", "#92A8D1", "#955251", "#B565A7"
    )
    
    # ============================================================================
    # HELPER FUNCTIONS
    # ============================================================================
    
    # Text wrapping function
    wrap_and_limit_text <- function(text, line_length = WRAP_LINE_LENGTH, 
                                     max_chars = MAX_TEXT_CHARS) {
      if (nchar(text) > max_chars) {
        text <- substr(text, 1, max_chars - 3)
        text <- paste0(text, "...")
      }
      wrapped <- sapply(seq(1, nchar(text), line_length), function(i) {
        substr(text, i, min(i + line_length - 1, nchar(text)))
      })
      paste(wrapped, collapse = "<br>")
    }
    
    # Add hover text to dataframe
    add_hover_text <- function(dataframe) {
      if (nrow(dataframe) == 0) {
        dataframe$hoverText <- NULL
        return(dataframe)
      }
      
      dataframe$hoverText <- apply(dataframe, 1, function(row) {
        cell_id <- row["Cell.ID"]
        full_names <- row["names.column"]
        formatted_names <- wrap_and_limit_text(full_names)
        paste("Cell.ID:", cell_id, "\nObservations:", formatted_names)
      })
      
      return(dataframe)
    }
    
    # Add polygon layer to plot
    add_polygon_layer <- function(plot, data, fill_colors, border_color, 
                                   line_size, hovertext_col) {
      if (nrow(data) == 0) {
        return(plot)
      }
      
      plot + ggplot2::geom_polygon(
        data = data,
        mapping = ggplot2::aes(
          x = x,
          y = y,
          group = interaction(depth, cluster, child),
          fill = fill_colors,
          text = hovertext_col
        ),
        colour = border_color,
        size = line_size,
        tooltip = "text",
        show.legend = TRUE
      )
    }
    
    # Extract color palette with sufficient colors
    get_color_palette <- function(n_needed, base_palette) {
      if (n_needed <= length(base_palette)) {
        return(base_palette[1:n_needed])
      }
      
      # Use colorRampPalette for smooth color generation
      color_func <- grDevices::colorRampPalette(base_palette)
      return(color_func(n_needed))
    }
    
    # Clean legend labels
    clean_legend_label <- function(label) {
      gsub("^\\(1,|\\)$", "", label)
    }
    
    # Safely modify plotly trace properties
    modify_plotly_trace <- function(trace) {
      # Clean legend name
      if (!is.null(trace$name)) {
        trace$name <- clean_legend_label(trace$name)
      }
      
      # Modify marker properties
      if (!is.null(trace$marker)) {
        trace$marker$line$width <- 0
        if (!is.null(trace$showlegend) && trace$showlegend) {
          trace$marker$line$color <- "rgba(0,0,0,0)"
        }
      }
      
      return(trace)
    }
    
    # Extract cluster information from nested list
    extract_cluster_info <- function(hvt_list, depth, cluster_no, child_no, hvt_res2) {
      current_cluster <- hvt_list[[2]][[depth]][[cluster_no]][[child_no]]
      
      list(
        x = as.numeric(current_cluster[["x"]]),
        y = as.numeric(current_cluster[["y"]]),
        x_cor = as.numeric(current_cluster[["pt"]][["x"]]),
        y_cor = as.numeric(current_cluster[["pt"]][["y"]]),
        cell_id = hvt_res2[child_no],
        depth = depth,
        cluster = cluster_no,
        child = child_no
      )
    }
    
    # ============================================================================
    # DATA EXTRACTION AND PROCESSING
    # ============================================================================
    
    # Extract HVT results
    hvt_list <- hvt.results
    hvt_res1 <- hvt_list[[2]][[1]]$`1`
    hvt_res2 <- hvt_list[[3]]$summary$Cell.ID
    a <- seq_along(hvt_res1)
    b <- a[hvt_res2]
    b <- as.vector(b)
    hvt_res2 <- stats::na.omit(b)
    
    # Extract coordinates
    coordinates_value1 <- lapply(seq_along(hvt_res1), function(x) {
      hvt_res1[[x]]$pt
    })
    cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
    colnames(cellID_coordinates) <- c("x", "y")
    cellID_coordinates$Cell.ID <- hvt_res2
    cellID_coordinates <- cellID_coordinates %>% arrange(Cell.ID)

#browser()


    maxDepth <- 1
    
    # ============================================================================
    # CALCULATE BOUNDARIES (for potential future use)
    # ============================================================================
    
    # Note: boundaries calculation kept for potential future boundary checks
    # Currently not used in plotting as ggplot handles scaling automatically
    
    # ============================================================================
    # EXTRACT CLUSTER INFORMATION
    # ============================================================================
    
    # Pre-calculate total number of iterations for vector pre-allocation
    total_cells <- sum(sapply(seq_len(maxDepth), function(depth) {
      sum(sapply(seq_along(hvt_list[[2]][[depth]]), function(cluster_no) {
        length(hvt_list[[2]][[depth]][[cluster_no]])
      }))
    }))
    
    # Initialize lists for collecting data
    cluster_info_list <- vector("list", total_cells)
    idx <- 1
    
    # Extract all cluster information
    for (depth in seq_len(maxDepth)) {
      for (cluster_no in seq_along(hvt_list[[2]][[depth]])) {
        for (child_no in seq_along(hvt_list[[2]][[depth]][[cluster_no]])) {
          cluster_info_list[[idx]] <- extract_cluster_info(
            hvt_list, depth, cluster_no, child_no, hvt_res2
          )
          idx <- idx + 1
        }
      }
    }
    
    # Convert collected data to vectors
    cell_info <- list(
      depthVal = sapply(cluster_info_list, `[[`, "depth"),
      clusterVal = sapply(cluster_info_list, `[[`, "cluster"),
      childVal = sapply(cluster_info_list, `[[`, "child"),
      cell_ids = sapply(cluster_info_list, `[[`, "cell_id"),
      x_cor = sapply(cluster_info_list, `[[`, "x_cor"),
      y_cor = sapply(cluster_info_list, `[[`, "y_cor")
    )
    
    # Create position data
    position_info <- lapply(cluster_info_list, function(info) {
      n_points <- length(info$x)
      data.frame(
        depth = rep(info$depth, n_points),
        cluster = rep(info$cluster, n_points),
        child = rep(info$child, n_points),
        x = info$x,
        y = info$y,
        stringsAsFactors = FALSE
      )
    })
    positionsDataframe <- do.call(rbind, position_info)
    
    # ============================================================================
    # CREATE DATAFRAMES
    # ============================================================================
    
    valuesDataframe <- data.frame(
      depth = cell_info$depthVal,
      cluster = cell_info$clusterVal,
      child = cell_info$childVal,
      cellid = cell_info$cell_ids,
      stringsAsFactors = FALSE
    )
    
    centroidDataframe <- data.frame(
      x = cell_info$x_cor,
      y = cell_info$y_cor,
      lev = cell_info$depthVal,
      stringsAsFactors = FALSE
    )
    
    # Merge dataframes
    datapoly <- merge(valuesDataframe, positionsDataframe, 
                      by = c("depth", "cluster", "child"))
    colnames(datapoly) <- c("depth", "cluster", "child", "Cell.ID", "x", "y")
    datapoly <- merge(datapoly, dataset, by = c("Cell.ID"))
    
    # Process centroid dataframes
    centroidDataframe <- centroidDataframe %>% 
      cbind(cellID_coordinates$Cell.ID)
    names(centroidDataframe) <- c("x", "y", "lev", "Cell.ID")
    
    dataset_new <- dataset %>% dplyr::select('Cell.ID', 'names.column')
    centroidDataframe <- merge(centroidDataframe, dataset_new, by = 'Cell.ID')
    
    centroidDataframe_2 <- merge(cellID_coordinates, centroidDataframe[, c("x", "y", "lev")], 
                                 by = c("x", "y"))
    centroidDataframe_2 <- centroidDataframe_2 %>% 
      cbind(centroidDataframe$names.column)
    colnames(centroidDataframe_2) <- c("x", "y", "Cell.ID", "lev", "names.column")
    
    # Helper function to calculate geometric centroid of a polygon
    calculate_polygon_centroid <- function(x_coords, y_coords) {
      n <- length(x_coords)
      if (n < 3) {
        return(list(x = mean(x_coords), y = mean(y_coords)))
      }
      if (x_coords[1] != x_coords[n] || y_coords[1] != y_coords[n]) {
        x_coords <- c(x_coords, x_coords[1])
        y_coords <- c(y_coords, y_coords[1])
        n <- n + 1
      }
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
        return(list(x = mean(x_coords[1:(n-1)]), y = mean(y_coords[1:(n-1)])))
      }
      centroid_x <- cx / (6 * signed_area)
      centroid_y <- cy / (6 * signed_area)
      return(list(x = centroid_x, y = centroid_y))
    }
    
    # Calculate polygon centroids for cell ID positioning (when cell_id_position == "center")
    polygon_centroids_df <- data.frame(Cell.ID = numeric(0), x = numeric(0), y = numeric(0))
    if (cell_id == TRUE && cell_id_position == "center") {
      polygon_centroids_list <- list()
      for (depth_val in 1:maxDepth) {
        depth_data <- datapoly[datapoly$depth == depth_val, ]
        if (nrow(depth_data) > 0) {
          polygon_centroids <- depth_data %>%
            dplyr::group_by(depth, cluster, child) %>%
            dplyr::summarise(
              x = calculate_polygon_centroid(x, y)$x,
              y = calculate_polygon_centroid(x, y)$y,
              .groups = 'drop'
            )
          polygon_centroids_list[[depth_val]] <- polygon_centroids
        }
      }
      if (length(polygon_centroids_list) > 0) {
        polygon_centroids_df_base <- do.call(rbind, polygon_centroids_list)
        # Match with Cell.IDs for level 1
        if (maxDepth >= 1 && nrow(polygon_centroids_df_base) > 0) {
          level1_poly_centroids <- polygon_centroids_df_base[polygon_centroids_df_base$depth == 1, ]
          cellID_mapping <- hvt_list[[3]][["summary"]] %>%
            dplyr::filter(Segment.Level == 1) %>%
            dplyr::select(Segment.Parent, Segment.Child, Cell.ID) %>%
            dplyr::mutate(
              Segment.Parent = as.character(Segment.Parent),
              Segment.Child = as.numeric(Segment.Child)
            )
          polygon_centroids_df <- level1_poly_centroids %>%
            dplyr::mutate(
              cluster = as.character(cluster),
              child = as.numeric(child)
            ) %>%
            dplyr::left_join(cellID_mapping, 
                             by = c("cluster" = "Segment.Parent", "child" = "Segment.Child")) %>%
            dplyr::filter(!is.na(Cell.ID)) %>%
            dplyr::select(Cell.ID, x, y)
        }
      }
    }
        
    # ============================================================================
    # ADD HOVER TEXT
    # ============================================================================
    
    datapoly <- add_hover_text(datapoly)
    centroidDataframe <- add_hover_text(centroidDataframe)
    
    # ============================================================================
    # PREPARE COLORS
    # ============================================================================
    
    column_color <- as.factor(datapoly[, domains.column])
    n_colors_needed <- length(unique(column_color))
    domain_colors <- get_color_palette(n_colors_needed, DOMAIN_COLORS)
    
    # ============================================================================
    # CREATE PLOT
    # ============================================================================
    
    domains_plot <- ggplot2::ggplot()
    
    # Plot polygons for each depth level
    for (i in maxDepth:1) {
      # Pre-compute highlighted status
      is_highlighted <- datapoly[datapoly$depth == i, "Cell.ID"] %in% highlight_cells
      
      # Split data
      depth_data <- datapoly[datapoly$depth == i, ]
      data_normal <- depth_data[!is_highlighted, ]
      data_highlight <- depth_data[is_highlighted, ]
      
      # Get corresponding color subsets
      colors_normal <- column_color[datapoly$depth == i][!is_highlighted]
      colors_highlight <- column_color[datapoly$depth == i][is_highlighted]
      
      # Add polygon layers
      domains_plot <- add_polygon_layer(
        domains_plot, data_normal, colors_normal,
        "black", LINE_SIZE_NORMAL, data_normal$hoverText
      )
      
      domains_plot <- add_polygon_layer(
        domains_plot, data_highlight, colors_highlight,
        "white", LINE_SIZE_HIGHLIGHT, data_highlight$hoverText
      )
    }
    
    # Add centroids for each depth (conditionally based on centroids parameter)
    if (centroids == TRUE) {
      for (depth in seq_len(maxDepth)) {
        depth_size <- centroid.size[depth]
        depth_color <- centroid.color[depth]
        domains_plot <- domains_plot + ggplot2::geom_point(
          data = centroidDataframe[centroidDataframe["lev"] == depth, ],
          ggplot2::aes(x = x, y = y, text = hoverText),
          size = depth_size,
          pch = 21,
          fill = depth_color,
          color = depth_color,
          tooltip = "text"
        )
      }
    }
    
    # ============================================================================
    # STYLE PLOT
    # ============================================================================
    
    domains_plot <- domains_plot +
      ggplot2::scale_fill_manual(
        name = "Domains",
        values = domain_colors,
        breaks = levels(column_color)
      ) +
      ggplot2::theme_bw() + 
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 20,
          hjust = 0.5,
          margin = ggplot2::margin(0, 0, 20, 0)
        ),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0))
    
    # ============================================================================
    # CONVERT TO PLOTLY AND FINALIZE
    # ============================================================================
    
    domainsPlot <- plotly::ggplotly(domains_plot, tooltip = "text")
    
    # Ensure centroids are filled circles in plotly
    if (centroids == TRUE) {
      n_traces <- length(domainsPlot$x$data)
      for (i in seq_len(n_traces)) {
        trace <- domainsPlot$x$data[[i]]
        if (!is.null(trace$mode) && grepl("markers", trace$mode)) {
          if (is.null(domainsPlot$x$data[[i]]$marker)) {
            domainsPlot$x$data[[i]]$marker <- list()
          }
          # Get the depth from the trace (if available) or use first depth color
          depth_idx <- min(i, length(centroid.color))
          domainsPlot$x$data[[i]]$marker$fillcolor <- centroid.color[depth_idx]
          if (is.null(domainsPlot$x$data[[i]]$marker$line)) {
            domainsPlot$x$data[[i]]$marker$line <- list()
          }
          domainsPlot$x$data[[i]]$marker$line$color <- centroid.color[depth_idx]
          domainsPlot$x$data[[i]]$marker$line$width <- 0.5
        }
      }
    }
    
    # Add cell ID annotations (conditionally based on cell_id parameter)
    if (cell_id == TRUE) {
      # Prepare annotation data based on cell_id_position
      if (cell_id_position == "center" && nrow(polygon_centroids_df) > 0) {
        annotation_data <- polygon_centroids_df
      } else {
        annotation_data <- subset(centroidDataframe_2, lev == 1)
      }
      
      # Calculate position offsets based on cell_id_position
      if (cell_id_position == "center") {
        xshift <- 0
        yshift <- 0
        xanchor <- "center"
        yanchor <- "middle"
      } else {
        alignments <- list(
          right = list(xshift = 5, yshift = 0, xanchor = "left", yanchor = "middle"),
          left = list(xshift = -5, yshift = 0, xanchor = "right", yanchor = "middle"),
          bottom = list(xshift = 0, yshift = -10, xanchor = "center", yanchor = "top"),
          top = list(xshift = 0, yshift = 10, xanchor = "center", yanchor = "bottom")
        )
        alignment <- rlang::`%||%`(alignments[[cell_id_position]], alignments$bottom)
        xshift <- alignment$xshift
        yshift <- alignment$yshift
        xanchor <- alignment$xanchor
        yanchor <- alignment$yanchor
      }
      
      # Convert cell_id_size from ggplot2 size to plotly font size
      plotly_font_size <- max(8, cell_id_size * 2.845)
      
      if (nrow(annotation_data) > 0) {
        domainsPlot <- domainsPlot %>%
          plotly::add_annotations(
            x = annotation_data$x,
            y = annotation_data$y,
            text = as.character(annotation_data$Cell.ID),
            showarrow = FALSE,
            font = list(size = plotly_font_size, color = "black"),
            xshift = xshift,
            yshift = yshift,
            xanchor = xanchor,
            yanchor = yanchor,
            xref = "x",
            yref = "y"
          )
      }
    }
    
    # Modify all traces in a single loop
    domainsPlot$x$data <- lapply(domainsPlot$x$data, modify_plotly_trace)
    
    # Add final layout modifications
    domainsPlot <- domainsPlot %>%
      plotly::layout(
        hoverlabel = list(bgcolor = "rgba(255,255,0,0.2)"),
        legend = list(
          title = list(text = "Domains"),
          itemsizing = "constant",
          itemdoubleclick = FALSE,
          itemclick = "toggleothers",
          traceorder = "reversed"
        )
      ) %>%
      plotly::config(displayModeBar = TRUE)
    
    return(domainsPlot)
  })
}
