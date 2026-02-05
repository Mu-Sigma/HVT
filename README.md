# HVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty

##### Created Date: 2018-11-15
##### Modified Date: 2026-02-05

<div id="TOC">

*   [<span class="toc-section-number">1.</span> Abstract](#abstract)
*   [<span class="toc-section-number">2.</span> Vignettes](#vignettes)
*   [<span class="toc-section-number">3.</span> Package Version History](#version-history)
    *   [<span class="toc-section-number">3.11</span> HVT (v26.1.2)](#hvt-(v26.1.2))
    *   [<span class="toc-section-number">3.10</span> HVT (v26.1.1) | What’s New](#hvt-(v26.1.1)-whats-new)
    *   [<span class="toc-section-number">3.9</span> HVT (v25.2.8)](#hvt-(v25.2.8))
    *   [<span class="toc-section-number">3.8</span> HVT (v25.2.7)](#hvt-(v25.2.7))
    *   [<span class="toc-section-number">3.7</span> HVT (v25.2.6)](#hvt-(v25.2.6))
    *   [<span class="toc-section-number">3.6</span> HVT (v25.2.5)](#hvt-(v25.2.5))
    *   [<span class="toc-section-number">3.5</span> HVT (v25.2.4)](#hvt-(v25.2.4))
    *   [<span class="toc-section-number">3.4</span> HVT (v24.9.1)](#hvt-(v24.9.1))
    *   [<span class="toc-section-number">3.3</span> HVT (v24.5.2)](#hvt-(v24.5.2))
    *   [<span class="toc-section-number">3.2</span> HVT (v23.11.02)](#hvt-(v23.11.02))
    *   [<span class="toc-section-number">3.1</span> HVT (v22.12.06)](#hvt-(v22.12.06))
*   [<span class="toc-section-number">4.</span> Installation of HVT (v26.1.2)](#installation-of-hvt-(v26.1.2))


</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1.</span> Abstract

The HVT package offers a suite of R functions designed to construct <a href="https://link.springer.com/chapter/10.1007/1-84628-118-0_7" target="_blank">topology preserving maps</a> for in-depth analysis of multivariate data. It is particularly well-suited for datasets with numerous records. The package organizes the typical workflow into several key stages:

1.  **Data Compression**: Long datasets are compressed using Hierarchical Vector Quantization (HVQ) to achieve the desired level of data reduction.

2.  **Data Projection**:  Compressed cells are projected into one and two dimensions using dimensionality reduction algorithms, producing <a href="https://en.wikipedia.org/wiki/Embedding" target="_blank">embeddings</a> that preserve the original topology. This allows for intuitive visualization of complex data structures.

3.  **Tessellation**: Voronoi tessellation partitions the projected space into distinct cells, supporting hierarchical visualizations. Heatmaps and interactive plots facilitate exploration and insights into the underlying data patterns.

4.  **Scoring**: Test dataset is evaluated against previously generated maps, enabling their placement within the existing structure. Sequential application across multiple maps is supported if required.

5. **Temporal Analysis and Visualization**: Functions in this stage examine time-series data to identify patterns, estimate transition probabilities, and visualize data flow over time.

6. **Dynamic Forecasting**:  Monte Carlo simulations of Markov chain provides forecasting capabilities for both ex-post and ex-ante scenarios with meticulously handling problematic states when found.

The HVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. Below is an image depicting a captivating tessellation of a torus, see
<a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/HVT_vignette.html" target="_blank">**vignette**</a> for more details.

<p align="center">
  <img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/pngs/torus2.png" width="642px" height="440px" />
</p>
<p align="center"><em>Figure 1: The Voronoi tessellation for layer 1 and number of cells 500 with the heat map overlaid for variable 'z'.</em></p>



</div>

<div id="vignettes" class="section level1" number="2">

# <span class="header-section-number">2.</span> Vignettes

 

Following are the links to the vignettes for the HVT package:

| Version | Vignette Title | Description |
|----|----------|----------------|
| v18.05.17 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/HVT_vignette.html" target="_blank" rel="noopener noreferrer">HVT Vignette</a> | Contains the workflow of the functions used for vector quantization and construction of Hierarchical Voronoi Tessellations for data analysis. |
| v18.05.17 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/HVT_model_diagnostics_vignette.html" target="_blank" rel="noopener noreferrer">HVT Model Diagnostics Vignette</a> | Contains demonstrations of functions used to perform model diagnostics and validation for the trained HVT model. |
| v23.05.16 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/Scoring_Cells_with_Layers_using_scoreLayeredHVT.html" target="_blank" rel="noopener noreferrer">HVT Scoring Cells with Layers using scoreLayeredHVT</a> | Contains explanations of the functions used for scoring cells with layers based on a sequence of maps using scoreLayeredHVT. |
| v23.10.26 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/HVT_Temporal_Analysis.html" target="_blank" rel="noopener noreferrer">Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT</a> | Contains implementations of the functions used for analyzing time series data and creating its state transition flow maps. |
| v24.05.16 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/LLM_Embeddings_in_HVT.html" target="_blank" rel="noopener noreferrer">Visualizing LLM Embeddings using HVT</a> | Contains implementation and analysis of hierarchical clustering using functions to evaluate and visualize token embeddings generated by OpenAI in 2D Space. |
| v24.08.14 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/Implementation_of_tsne_umap_in_trainHVT.html" target="_blank" rel="noopener noreferrer">Implementation of t-SNE and UMAP in trainHVT function</a> | Contains enhancements to the `trainHVT` function with advanced dimensionality reduction techniques such as t-SNE and UMAP, and includes a table of evaluation metrics to improve interpretability. |
| v25.03.01 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/Dynamic_Forecasting_macroeconomic_data.html" target="_blank" rel="noopener noreferrer">Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT</a> | Contains enhancements to the HVT package for dynamic forecasting using Monte Carlo Simulations of Markov Chain (MSM) on macroeconomic time series dataset. |
| v25.08.25 | <a href="https://raw.githack.com/Mu-Sigma/HVT/master/vignettes/Experimentation_of_hyperparameters_in_msm.html" target="_blank" rel="noopener noreferrer">Hyperparameter Experimentation for Champion Model Selection in MSM Dynamic Forecasting</a> | Contains enhancements to enable strategic selection of the champion model based on the lowest Mean Absolute Error by hyperparameter tuning in msm - dynamic forecasting. |

<div id="version-history" class="section level1" number="3">

# <span class="header-section-number">3.</span> Package Version History 

<div id="hvt-(v26.1.2)" class="section level2" number="3.11">

## 3.11 HVT (v26.1.2)

5th February, 2026

*In this version of the HVT package, a minor issue related to the global initialization of a variable has been fixed.*
</div>

<div id="hvt-(v26.1.1)-whats-new" class="section level2" number="3.1">

## 3.10 HVT (v26.1.1) - What's New

22nd January, 2026

In this version of the HVT package, the following enhancement and feature has been made:

**Enhancement**

1. **LLM Embeddings Vignette**: This vignette showcases the HVT workflow applied to numerical embedding data generated using an OpenAI model, featuring a more robust preprocessing pipeline and a revamped implementation aligned with recent AI developments.

**Feature**

1. **2D HVT Plots**: In this version, the aesthetics of compressed 2D HVT plots have been enhanced by adding new arguments to the `plotHVT` and `scoreHVT` functions. These updates allow users to toggle the display of cell centroids, adjust their sizes, and position cell IDs at the center, enabling clearer visualization of dense heatmaps with more legible centroids and cell labels.

</div>

<div id="hvt-(v25.2.8)" class="section level2" number="3.2">

## 3.9 HVT (v25.2.8)

16th December, 2025

In this version of the HVT package, the following new feature has been introduced:

1. **Ex-ante raw series forecasting**: A new feature enabling ex-ante raw series forecasting directly from transformed year-over-year (YoY) forecasts using a 12-month lookback approach. This allows the percentage changes from ex-ante transformed forecasts to be translated back into the raw series for each variable. This feature is introduced and explained in 'Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT' vignette. 

</div>

<div id="hvt-(v25.2.7)" class="section level2" number="3.3">

## 3.8 HVT (v25.2.7) 

22nd October, 2025

In this version of the HVT package, the following new feature and vignette have been introduced:

**Feature**

1. **Experimentation of hyperparameters in `msm`**: This update introduces a new function called `HVTMSMoptimization` that runs grid search experiments across different hyperparameters (number of cells, clusters(k), nearest neighbors(nn)) by training and scoring HVT models, running MSM simulations for each combination and identify the champion model (lowest MAE across all results).

2. **Tabulation and Visualization**: Accessory functions to `HVTMSMoptimization` such as `OptimizationResults` and `plotMsmKN` has been added which helps to tabulate all the iterations and visualize the output via plotly object.

**Vignette** 

1. **Hyperparameter Experimentation for Champion Model Selection in MSM Dynamic Forecasting**: This vignette provides a comprehensive demonstration of using `HVTMSMoptimization`, covering the complete workflow from initial dataset handling, selection for train & test, executing hyperparameter tuning and identifying the champion model, implementing the champion model, and comparing MAE results.




</div>

<div id="hvt-(v25.2.6)" class="section level2" number="3.4">

## 3.7 HVT (v25.2.6)

14th October, 2025

*The issue with time-series animation plots from previous release has now been resolved with the latest gganimate update.*

</div>


<div id="hvt-(v25.2.5)" class="section level2" number="3.5">

## 3.6 HVT (v25.2.5)

04th July, 2025

*Dropping the time-series animation plots from the package since the latest version of gganimate doesn’t support them — a patched release will follow once the issue is resolved.*

</div>

<div id="hvt-(v25.2.4)" class="section level2" number="3.6">

## 3.5 HVT (v25.2.4) 

04th June, 2025

In this version of the HVT package, the following new features and vignette have been introduced:

**Features**

1. **Dynamic Forecasting of a Time Series Dataset**: This update introduces a new function called `msm` Monte Carlo Simulations of Markov Chain for dynamic forecasting of states in time series dataset. It supports both ex-post and ex-ante forecasting, offering valuable insights into future trends while resolving state transition challenges through clustering and nearest-neighbor methods to enhance simulation accuracy.

2. **Z score Plots**: This update introduces a new function called `plotZscore` that generates Z-score plots corresponding to the HVT cells for the given data, offering a visual representation of data distribution and highlighting potential outliers.

**Vignette** 

1. **Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT**: This vignette illustrates the practical use of the new msm function on a macroeconomic dataset with 10 variables. It covers all steps, including data preparation, model training, scoring, and forecasting, while addressing challenges related to state transitions and evaluating performance using Mean Absolute Error (MAE).




</div>


<div id="hvt-(v24.9.1)" class="section level2" number="3.7">

## 3.4 HVT (v24.9.1) 

4th September, 2024

In this version of the HVT package, the following new features and vignettes have been introduced:

**Features**

1. **Implementation of t-SNE and UMAP in `trainHVT`**: This update incorporates dimensionality reduction methods like t-SNE and UMAP in the `trainHVT` function, complementing the existing Sammon's projection. It also enables the visualization of these techniques across all hierarchical levels within the HVT framework.

2. **Implementation of dimensionality reduction evaluation metrics**: This update introduces highly effective dimensionality reduction evaluation metrics as part of the output list of the `trainHVT` function. These metrics are organized into two levels: Level 1 (L1) and Level 2 (L2). The L1 metrics address key areas of dimensionality reduction which are mentioned below, by ensuring comprehensive evaluation and performance.

- Structure Preservation Metrics 
- Distance Preservation Metrics
- Human Centered Metrics
- Interpretive Quality Metrics
- Computational Efficiency Metrics


3. **Introduction of `clustHVT` function**: In this update, we introduced a new function called `clustHVT` specifically designed for Hierarchical clustering analysis. The function performs clustering of cells exclusively when the hierarchy level is set to 1, determining the optimal number of clusters by evaluating various indices. Based on user input, it conducts hierarchical clustering using AGNES with the default ward.D2 method. The output includes a dendrogram and an interactive 2D clustered HVT map that reveals cell context upon hovering. This function is not applicable when the hierarchy level is greater than 1.



**Vignettes** 

1. **Implementation of t-SNE and UMAP in `trainHVT` function**: This vignette showcases the integration of t-SNE and UMAP in the `trainHVT` function, offering a comprehensive guide on how to apply and visualize these dimensionality reduction techniques. It also covers the dimensionality reduction evaluation metrics and provides insights into their interpretation.

2. **Visualizing LLM Embeddings using HVT (Hierarchical Voronoi Tessellation)**: This vignette will outline the process of analyzing OpenAI-generated token embeddings using the HVT package, covering data compression, visualization, and hierarchical clustering, as well as comparing domain name assignments for clusters. It examines HVT's effectiveness in preserving contextual relationships between embeddings. Additionally, it provides a brief overview of the newly added `clustHVT` function and its parameters.



</div>


<div id="hvt-(v24.5.2)" class="section level2" number="3.8">

## 3.3 HVT (v24.5.2) 

2nd May, 2024

In this version of HVT package, the following new features have been introduced:

1. **Updated Nomenclature:** To make the function names more consistent and understandable/intuitive, we have renamed the functions throughout the package. Given below are the few instances.

* `HVT` to `trainHVT`
* `predictHVT` to `scoreHVT`
* `predictLayerHVT` to `scoreLayeredHVT`

2. **Restructured Functions:** The functions have been rearranged and grouped into new sections which are highlighted on the index page of package’s PDF documentation. Given below are the few instances.

* `trainHVT` function now resides within the `Training_or_Compression` section.
* `plotHVT` function now resides within the `Tessellation_and_Heatmap` section.
* `scoreHVT` function now resides within the `Scoring` section.

3. **Enhancements:** The pre-existed functions, `hvtHmap` and `exploded_hmap`, have been combined and incorporated into the `plotHVT` function. Additionally, `plotHVT` now includes the ability to perform 1D plotting.

4. **Temporal Analysis** 
- The new update focuses on the integration of time series capabilities into the HVT package by extending its foundational operations to time series data which is emphasized in this vignette.
- The new functionalities are introduced to analyze underlying patterns and trends within the data, providing insights into its evolution over time and also offering the capability to analyze the movement of the data by calculating its transitioning probability and creates elegant plots and GIFs.

Below are the new functions and its brief descriptions:

- `plotStateTransition`: Provides the time series flowmap plot.
- `getTransitionProbability`: Provides a list of transition probabilities.
- `reconcileTransitionProbability`: Provides plots and tables for comparing transition probabilities calculated manually and from markovchain function.
- `plotAnimatedFlowmap`: Creates flowmaps and animations for both self state and without self state scenarios.

</div>



<div id="hvt-(v23.11.02)" class="section level2" number="3.9">

## 3.2 HVT (v23.11.02) 

17th November, 2023

This version of HVT package offers functionality to score cells with layers based on a sequence of maps created using `scoreLayeredHVT`. Given below are the steps to created the successive set of maps.

1. **Map A** - The output of `trainHVT` function which is trained on parent data.

2. **Map B** - The output of `trainHVT` function which is trained on the 'data with novelty' created from `removeNovelty` function.

3. **Map C** - The output of `trainHVT` function which is trained on the 'data without novelty' created from `removeNovelty` function.

The `scoreLayeredHVT` function uses these three maps to score the test datapoints.

Let us try to understand the steps with the help of the diagram below

<p align="center">
  <img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/pngs/scoreLayeredHVT_function.png" width="672px" height="480px" />
</p>
<p align="center"><em>Figure 2: Data Segregation for scoring based on a sequence of maps using scoreLayeredHVT()</em></p>



</div>

<div id="hvt-(v22.12.06)" class="section level2" number="3.10">

## 3.1 HVT (v22.12.06) 

06th December, 2022

This version of HVT package offers features for both training an HVT model and eliminating outlier cells from the trained model.

1. **Training or Compression:** The initial step entails training the parent data using the `trainHVT` function, specifying the desired compression percentage and quantization error.

2. **Remove novelty cells:** Following the training process, outlier cells can be identified manually from the 2D hvt plot. These outlier cells can then be inputted into the `removeNovelty` function, which subsequently produces two datasets in its output: one containing 'data with novelty' and the other containing 'data without novelty'.


</div>


<div id="installation-of-hvt-(v26.1.2)" class="section level2" number="4">

# <span class="header-section-number">4.</span> Installation of HVT (v26.1.2)

<div class="sourceCode" id="cb1">

**CRAN Installation**

`install.packages("HVT")`

**Git Hub Installation**

`library(devtools)`

#increase the package download timelimit, if faced with error:  options(timeout = 1200) 

`devtools::install_github(repo = "Mu-Sigma/HVT")`

</div>

</div>


