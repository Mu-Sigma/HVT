clusterPlot <- function(dataset, hvt.results, domains.column, highlight_cells = NULL) {
  suppressWarnings({
    hoverText = NULL
    lev = NULL
    
    # Define color palette
    domain.color <- c(
      "#FFD700" ,"#FF0000", "#00FFFF", "#0000FF", "#FF00FF",
      "#00FF00", "#F012BE", "#85144b", "#3D9970", "#39CCCC",
      "#01FF70", "#DDDDDD", "#AAAAAA", "#FF6F61", "#6B5B95",
      "#88B04B", "#F7CAC9", "#92A8D1", "#955251", "#B565A7"
    )
    
    # Extract HVT results
    hvt_list <- hvt.results
    hvt_res1 <- hvt_list[[2]][[1]]$`1`
    hvt_res2 <- hvt_list[[3]]$summary$Cell.ID
    a <- 1:length(hvt_res1)
    b <- a[hvt_res2]
    b <- as.vector(b)
    hvt_res2 <- stats::na.omit(b)
    
    # Extract coordinates
    coordinates_value1 <- lapply(1:length(hvt_res1), function(x) {
      centroids1 <- hvt_res1[[x]]
      coordinates1 <- centroids1$pt
    })
    cellID_coordinates <- do.call(rbind.data.frame, coordinates_value1)
    colnames(cellID_coordinates) <- c("x", "y")
    cellID_coordinates$Cell.ID <- hvt_res2
    
    maxDepth <- 1
    
    # Initialize boundaries
    min_x <- 1e9
    min_y <- 1e9
    max_x <- -1e9
    max_y <- -1e9
    
    # Initialize vectors
    depthVal <- c()
    clusterVal <- c()
    childVal <- c()
    x_pos <- c()
    y_pos <- c()
    x_cor <- c()
    y_cor <- c()
    depthPos <- c()
    clusterPos <- c()
    childPos <- c()
    cell_ids <- c()
    x_coords <- c()
    y_coords <- c()
    levelCluster <- c()
    
    # Calculate boundaries
    for (clusterNo in 1:length(hvt_list[[2]][[1]][[1]])) {
      bp_x <- hvt_list[[2]][[1]][[1]][[clusterNo]][["x"]]
      bp_y <- hvt_list[[2]][[1]][[1]][[clusterNo]][["y"]]
      
      min_x <- min(min_x, min(bp_x))
      max_x <- max(max_x, max(bp_x))
      min_y <- min(min_y, min(bp_y))
      max_y <- max(max_y, max(bp_y))
    }
    
    # Extract cluster information
    for (depth in 1:maxDepth) {
      for (clusterNo in 1:length(hvt_list[[2]][[depth]])) {
        for (childNo in 1:length(hvt_list[[2]][[depth]][[clusterNo]])) {
          current_cluster <- hvt_list[[2]][[depth]][[clusterNo]][[childNo]]
          
          x <- as.numeric(current_cluster[["x"]])
          y <- as.numeric(current_cluster[["y"]])
          x_cor <- c(x_cor, as.numeric(current_cluster[["pt"]][["x"]]))
          y_cor <- c(y_cor, as.numeric(current_cluster[["pt"]][["y"]]))
          cell_ids[[length(cell_ids) + 1]] <- hvt_res2[childNo]
          x_coords[[length(x_coords) + 1]] <- x
          y_coords[[length(y_coords) + 1]] <- y
          depthVal <- c(depthVal, depth)
          clusterVal <- c(clusterVal, clusterNo)
          childVal <- c(childVal, childNo)
          depthPos <- c(depthPos, rep(depth, length(x)))
          clusterPos <- c(clusterPos, rep(clusterNo, length(x)))
          childPos <- c(childPos, rep(childNo, length(x)))
          x_pos <- c(x_pos, x)
          y_pos <- c(y_pos, y)
          levelCluster <- c(levelCluster, depth)
        }
      }
    }
    
    # Create dataframes
    valuesDataframe <- data.frame(depth = depthVal, cluster = clusterVal, 
                                  child = childVal, cellid = unlist(cell_ids))
    positionsDataframe <- data.frame(depth = depthPos, cluster = clusterPos, 
                                     child = childPos, x = x_pos, y = y_pos)
    centroidDataframe <- data.frame(x = x_cor, y = y_cor, lev = levelCluster)
    
    # Merge dataframes
    datapoly <- merge(valuesDataframe, positionsDataframe, 
                      by = c("depth", "cluster", "child"))
    centroidDataframe_2 <- merge(cellID_coordinates, centroidDataframe, 
                                 by = c("x", "y"))
    
    # Rename and merge with dataset
    colnames(datapoly) <- c("depth", "cluster", "child", "Cell.ID", "x", "y")
    datapoly <- merge(datapoly, dataset, by = c("Cell.ID"))
    
    # Ensure column_color is properly factored
    column_color <- as.factor(datapoly[, domains.column])
    n_colors_needed <- length(unique(column_color))
    
    # Extend domain.color if needed
    while(length(domain.color) < n_colors_needed) {
      domain.color <- c(domain.color, 
                        sample(domain.color, n_colors_needed - length(domain.color)))
    }
    
    # Process centroid data
    centroidDataframe <- centroidDataframe %>% 
      cbind(cellID_coordinates$Cell.ID)
    names(centroidDataframe) <- c("x", "y", "lev", "Cell.ID")
    dataset_new <- dataset %>% dplyr::select('Cell.ID', 'names.column')
    centroidDataframe <- merge(centroidDataframe, dataset_new, by = 'Cell.ID')
    centroidDataframe_2 <- centroidDataframe_2 %>% 
      cbind(centroidDataframe$names.column)
    colnames(centroidDataframe_2) <- c("x", "y", "Cell.ID", "lev", "names.column")
    
    # Text wrapping function
    wrap_and_limit_text <- function(text, line_length = 100, max_chars = 500) {
      if (nchar(text) > max_chars) {
        text <- substr(text, 1, max_chars - 3)
        text <- paste0(text, "...")
      }
      wrapped <- sapply(seq(1, nchar(text), line_length), function(i) {
        substr(text, i, min(i + line_length - 1, nchar(text)))
      })
      paste(wrapped, collapse = "<br>")
    }
    
    # Add hover text to datapoly
    if (nrow(datapoly) != 0) {
      datapoly$hoverText <- apply(datapoly, 1, function(row) {
        cell_id <- row["Cell.ID"]
        full_names <- row["names.column"]
        formatted_names <- wrap_and_limit_text(full_names)
        paste("Cell.ID:", cell_id, "\nObservations:", formatted_names)
      })
    } else {
      datapoly$hoverText <- NULL
    }
    
    # Create plot
    domains_plot <- ggplot2::ggplot()
    
    # Plot polygons for each depth level
    for (i in maxDepth:1) {
      is_highlighted <- datapoly[which(datapoly$depth == i), "Cell.ID"] %in% 
        highlight_cells
      data_normal <- datapoly[which(datapoly$depth == i & !is_highlighted), ]
      data_highlight <- datapoly[which(datapoly$depth == i & is_highlighted), ]
      
      # Plot non-highlighted cells
      if(nrow(data_normal) > 0) {
        domains_plot <- domains_plot + ggplot2::geom_polygon(
          data = data_normal,
          mapping = ggplot2::aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = column_color[which(datapoly$depth == i & !is_highlighted)],
            text = hoverText
          ),
          colour = "black",
          size = 0.5,
          tooltip = "text",
          show.legend = TRUE
        )
      }
      
      # Plot highlighted cells
      if(nrow(data_highlight) > 0) {
        domains_plot <- domains_plot + ggplot2::geom_polygon(
          data = data_highlight,
          mapping = ggplot2::aes(
            x = x,
            y = y,
            group = interaction(depth, cluster, child),
            fill = column_color[which(datapoly$depth == i & is_highlighted)],
            text = hoverText
          ),
          colour = "white",
          size = 1.5,
          tooltip = "text",
          show.legend = TRUE
        )
      }
    }
    
    # Add hover text to centroidDataframe
    if (nrow(centroidDataframe) != 0) {
      centroidDataframe$hoverText <- apply(centroidDataframe, 1, function(row) {
        cell_id <- row["Cell.ID"]
        full_names <- row["names.column"]
        formatted_names <- wrap_and_limit_text(full_names)
        paste("Cell.ID:", cell_id, "\nObservations:", formatted_names)
      })
    } else {
      centroidDataframe$hoverText <- NULL
    }
    
    # Add centroids
    for (depth in 1:maxDepth) {
      domains_plot <- domains_plot + ggplot2::geom_point(
        data = centroidDataframe[centroidDataframe["lev"] == depth, ],
        ggplot2::aes(
          x = x,
          y = y,
          text = hoverText
        ),
        size = (1.5 / (2^(depth - 1))),
        pch = 21,
        fill = "black",
        color = "black",
        tooltip = "text"
      )
    }
    
    # Get subset data for annotations
    subset_data <- subset(centroidDataframe_2, lev == 1)
    
    # Add plot styling
    domains_plot <- domains_plot +
      ggplot2::scale_fill_manual(
        name = "Domains",
        values = domain.color[1:n_colors_needed],
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
    
    # Convert to plotly
    domainsPlot <- plotly::ggplotly(domains_plot, tooltip = "text")
    
    # Add cell ID annotations
    domainsPlot <- domainsPlot %>%
      plotly::add_annotations(
        x = subset_data$x,
        y = subset_data$y,
        text = subset_data$Cell.ID,
        showarrow = FALSE,
        font = list(size = 10),
        yshift = -10
      )
    
    # Clean up legend labels
    clean_label <- function(label) {
      gsub("^\\(1,|\\)$", "", label)
    }
    
    # Fix legend issues
    for (i in seq_along(domainsPlot$x$data)) {
      if (!is.null(domainsPlot$x$data[[i]]$name)) {
        domainsPlot$x$data[[i]]$name <- clean_label(domainsPlot$x$data[[i]]$name)
      }
      if (!is.null(domainsPlot$x$data[[i]]$marker)) {
        domainsPlot$x$data[[i]]$marker$line$width <- 0
      }
    }
    
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
      plotly::config(displayModeBar = T)
    
    # Final marker adjustments
    for (i in seq_along(domainsPlot$x$data)) {
      if (!is.null(domainsPlot$x$data[[i]]$marker) && 
          !is.null(domainsPlot$x$data[[i]]$showlegend) &&
          domainsPlot$x$data[[i]]$showlegend) {
        domainsPlot$x$data[[i]]$marker$line$width <- 0
        domainsPlot$x$data[[i]]$marker$line$color <- "rgba(0,0,0,0)"
      }
    }
    
    return(domainsPlot)
  })
}