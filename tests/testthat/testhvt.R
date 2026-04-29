context("HVT package")

# ─────────────────────────────────────────────────────────────
# Shared setup
# ─────────────────────────────────────────────────────────────

USArrests_scaled <- scale(datasets::USArrests, center = TRUE, scale = TRUE)

eustock <- data.frame(
  t    = as.numeric(time(EuStockMarkets)),
  DAX  = EuStockMarkets[, "DAX"],
  SMI  = EuStockMarkets[, "SMI"],
  CAC  = EuStockMarkets[, "CAC"],
  FTSE = EuStockMarkets[, "FTSE"]
)

# ─────────────────────────────────────────────────────────────
# trainHVT
# ─────────────────────────────────────────────────────────────

test_that("trainHVT: L1_Norm + mean", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "mean",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_equal(length(hvt.results), 7)
  expect_equal(hvt.results[[3]]$summary[, "Quant.Error"],
               c(0.3609466, 0.2791825, 0.2990230, 0.3566383, 0.2383200,
                 0.3392648, 0.2637289, 0.0000000, 0.3795258, 0.2487440), tolerance = 1e-5)
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]], 0.1)
})

test_that("trainHVT: L2_Norm + mean", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L2_Norm", error_metric = "mean",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_equal(length(hvt.results), 7)
  expect_equal(hvt.results[[3]]$summary[, "Quant.Error"],
               c(0.2027530, 0.1638378, 0.1638530, 0.2100104, 0.1350629,
                 0.1868272, 0.1477676, 0.0000000, 0.2131553, 0.1453146), tolerance = 1e-5)
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]], 0.7)
})

test_that("trainHVT: L1_Norm + max", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_equal(length(hvt.results), 7)
  expect_equal(hvt.results[[3]]$summary[, "Quant.Error"],
               c(0.5282025, 0.3198631, 0.4280770, 0.5259606, 0.2936061,
                 0.3972785, 0.2977948, 0.0000000, 0.4775840, 0.2994434), tolerance = 1e-5)
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]], 0.1)
})

test_that("trainHVT: L2_Norm + max", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L2_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_equal(length(hvt.results), 7)
  expect_equal(hvt.results[[3]]$summary[, "Quant.Error"],
               c(0.3098097, 0.1954690, 0.2424398, 0.3029314, 0.1719962,
                 0.2006439, 0.1599492, 0.0000000, 0.2916686, 0.1742819), tolerance = 1e-5)
  expect_equal(hvt.results[[3]]$compression_summary[["percentOfCellsBelowQuantizationErrorThreshold"]], 0.5)
})

test_that("trainHVT: output structure is correct", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_s3_class(hvt.results, "hvt.object")
  expect_true(all(c("summary", "compression_summary") %in% names(hvt.results[[3]])))
  expect_true(all(c("Segment.Level", "Segment.Parent", "Segment.Child", "n", "Quant.Error", "Cell.ID")
                  %in% colnames(hvt.results[[3]]$summary)))
  expect_equal(hvt.results[["model_info"]][["type"]], "hvt_model")
})

test_that("trainHVT: depth = 2 produces multi-level summary", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 3, depth = 2, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  expect_s3_class(hvt.results, "hvt.object")
  expect_true(max(hvt.results[[3]]$summary$Segment.Level, na.rm = TRUE) >= 1)
})

test_that("trainHVT: normalize = FALSE runs without error", {
  skip_on_cran()
  set.seed(420)
  expect_no_error(
    HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                  distance_metric = "L1_Norm", error_metric = "max",
                  normalize = FALSE, quant_method = "kmeans", dim_reduction_method = "sammon")
  )
})


# ─────────────────────────────────────────────────────────────
# scoreHVT
# ─────────────────────────────────────────────────────────────

test_that("scoreHVT: output structure is correct", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored <- HVT::scoreHVT(USArrests_scaled, hvt.results)
  expect_s3_class(scored, "hvt.object")
  expect_true(all(c("scoredPredictedData", "cellID_coordinates", "predictInput") %in% names(scored)))
})

test_that("scoreHVT: Cell.IDs are valid integers within trained cell range", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored <- HVT::scoreHVT(USArrests_scaled, hvt.results)
  cell_ids <- scored$scoredPredictedData$Cell.ID
  expect_true(all(!is.na(cell_ids)))
  expect_true(all(cell_ids >= 1 & cell_ids <= 10))
})

test_that("scoreHVT: scored rows match input rows", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 10, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored <- HVT::scoreHVT(USArrests_scaled, hvt.results)
  expect_equal(nrow(scored$scoredPredictedData), nrow(USArrests_scaled))
})

test_that("scoreHVT: rejects input with NA values", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  bad_data <- USArrests_scaled
  bad_data[1, 1] <- NA
  expect_error(HVT::scoreHVT(bad_data, hvt.results))
})

test_that("scoreHVT: rejects dataset missing training columns", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  bad_data <- USArrests_scaled[, 1:2]
  expect_error(HVT::scoreHVT(bad_data, hvt.results))
})

# ─────────────────────────────────────────────────────────────
# plotHVT
# ─────────────────────────────────────────────────────────────

test_that("plotHVT: returns a ggplot object for depth 1", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  p <- HVT::plotHVT(hvt.results, child.level = 1, hmap.cols = NULL, plot.type = "2Dhvt")
  expect_s3_class(p, "ggplot")
})

test_that("plotHVT: heatmap mode returns a ggplot object", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(USArrests_scaled, n_cells = 5, depth = 1, quant.err = 0.2,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  p <- HVT::plotHVT(hvt.results, child.level = 1, hmap.cols = "Murder", plot.type = "2Dhvt")
  expect_s3_class(p, "ggplot")
})

# ─────────────────────────────────────────────────────────────
# getTransitionProbability
# ─────────────────────────────────────────────────────────────

test_that("getTransitionProbability: output has required columns", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(eustock[, -1], n_cells = 10, depth = 1, quant.err = 0.1,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored     <- HVT::scoreHVT(eustock, hvt.results)
  state_data <- data.frame(cell_id = scored$scoredPredictedData$Cell.ID, t = eustock$t)
  tpm        <- HVT::getTransitionProbability(state_data, cellid_column = "cell_id", time_column = "t")
  expect_true(all(c("Current_State", "Next_State", "Transition_Probability") %in% colnames(tpm)))
})

test_that("getTransitionProbability: probabilities sum to 1 per state", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(eustock[, -1], n_cells = 10, depth = 1, quant.err = 0.1,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored     <- HVT::scoreHVT(eustock, hvt.results)
  state_data <- data.frame(cell_id = scored$scoredPredictedData$Cell.ID, t = eustock$t)
  tpm        <- HVT::getTransitionProbability(state_data, cellid_column = "cell_id", time_column = "t")
  row_sums   <- tapply(tpm$Transition_Probability, tpm$Current_State, sum)
  expect_true(all(abs(row_sums - 1) < 0.01))
})

# ─────────────────────────────────────────────────────────────
# msm
# ─────────────────────────────────────────────────────────────

test_that("msm: ex-post forecast returns expected output structure", {
  skip_on_cran()
  set.seed(420)
  hvt.results <- HVT::trainHVT(eustock[, -1], n_cells = 10, depth = 1, quant.err = 0.1,
                               distance_metric = "L1_Norm", error_metric = "max",
                               quant_method = "kmeans", dim_reduction_method = "sammon")
  scored     <- HVT::scoreHVT(eustock, hvt.results)
  state_data <- data.frame(Cell.ID = scored$scoredPredictedData$Cell.ID, t = eustock$t)
  tpm        <- HVT::getTransitionProbability(
    data.frame(cell_id = state_data$Cell.ID, t = state_data$t),
    cellid_column = "cell_id", time_column = "t"
  )
  train_states <- head(state_data, 1500)
  test_states  <- tail(state_data, nrow(state_data) - 1500)
  result <- HVT::msm(
    state_time_data             = train_states,
    forecast_type               = "ex-post",
    initial_state               = train_states$Cell.ID[nrow(train_states)],
    transition_probability_matrix = tpm,
    num_simulations             = 50,
    trainHVT_results            = hvt.results,
    scoreHVT_results            = scored,
    actual_data                 = NULL,
    raw_dataset                 = eustock,
    time_column                 = "t",
    handle_problematic_states   = FALSE
  )
  expect_true(is.list(result))
  expect_true("plots" %in% names(result))
})