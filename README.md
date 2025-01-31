# HVT: Collection of functions used to build hierarchical topology preserving maps

#### Zubin Dowlaty

##### Created Date: 2018-11-15
##### Modified Date: 2025-01-28


<div id="TOC">

*   [<span class="toc-section-number">1.</span> Abstract](#abstract)
*   [<span class="toc-section-number">2.</span> Vignettes](#vignettes)
    *   [<span class="toc-section-number">2.1</span> HVT Vignette](#hvt-vignette)
    *   [<span class="toc-section-number">2.2</span> HVT Model Diagnostics Vignette](#hvt-model-diagnostics-vignette)
    *   [<span class="toc-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT ](#hvt-scoring-cells-with-layers-using-scoreLayeredHVT)
    *   [<span class="toc-section-number">2.4</span> Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT](#temporal-analysis-and-visualization-leveraging-time-series-capabilities-in-hvt)
    *   [<span class="toc-section-number">2.5</span> Visualizing LLM Embeddings using HVT (Hierarchical Voronoi Tessellation)](#visualizing-llm-embeddings-using-hvt-hierarchical-voronoi-tessellation)
    *   [<span class="toc-section-number">2.6</span> Implementation of t-SNE and UMAP in trainHVT function](#implementation-of-t-sne-and-umap-to-trainhvt-function)
    *   [<span class="toc-section-number">2.7</span> Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT](#dynamic-forecasting-of-macroeconomic-time-series-dataset-using-hvt)
*   [<span class="toc-section-number">3.</span> Version History](#version-history)
    *   [<span class="toc-section-number">3.5</span> HVT (v25.1.1) | What’s New?](#hvt-(v25.1.1)-whats-new)
    *   [<span class="toc-section-number">3.4</span> HVT (v24.9.1)](#hvt-(v24.9.1))
    *   [<span class="toc-section-number">3.3</span> HVT (v24.5.2)](#hvt-(v24.5.2))
    *   [<span class="toc-section-number">3.2</span> HVT (v23.11.02)](#hvt-(v23.11.02))
    *   [<span class="toc-section-number">3.1</span> HVT (v22.12.06)](#hvt-(v22.12.06))
*   [<span class="toc-section-number">4.</span> Installation of HVT (v25.1.1)](#installation-of-hvt-(v25.1.1))


</div>

<div id="abstract" class="section level1" number="1">

# <span class="header-section-number">1.</span> Abstract

The HVT package is a collection of R functions to facilitate building <a href="https://link.springer.com/chapter/10.1007/1-84628-118-0_7" target="_blank">topology preserving maps</a> for rich multivariate data analysis, see `Figure 1` as an example of a 2D torus map generated from the package. Tending towards a big data preponderance, a large number of rows. A collection of R functions for this typical workflow is organized below:

1.  **Data Compression**: Vector quantization (VQ), HVQ (hierarchical vector quantization) using means or medians. This step compresses the rows (long data frame) using a compression objective.

2.  **Data Projection**: Dimension projection of the compressed cells to 1D,2D or Interactive surface plot with the Sammons Non-linear Algorithm. This step creates topology preserving map (also called <a href="https://en.wikipedia.org/wiki/Embedding" target="_blank">embeddings</a>) coordinates into the desired output dimension. 

3.  **Tessellation**: Create cells required for object visualization using the Voronoi Tessellation method, package includes heatmap plots for hierarchical Voronoi tessellations (HVT). This step enables data insights, visualization, and interaction with the topology preserving map useful for semi-supervised tasks.

4.  **Scoring**: Scoring new data sets and recording their assignment using the map objects from the above steps, in a sequence of maps if required.

5. **Temporal Analysis and Visualization**: A Collection of new functions that leverages the capacity of the HVT package by analyzing time series data for its underlying patterns, calculation of transitioning probabilities and the visualizations for the flow of data over time.

6. **Dynamic Forecasting**: Simulate future states of dynamic systems using Monte Carlo simulations of Markov Chain (MSM), enabling predictions for time-series data.

The HVT package allows creation of visually stunning tessellations, showcasing the power of topology preserving maps. Below is an image depicting a captivating tessellation of a torus, see
<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_vignette.html" target="_blank">**vignette:**</a> for more details.

<div style="text-align: center;">
  <img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/pngs/torus2.png" width="642px" height="440px" />
  <p class="caption">Figure 1: The Voronoi tessellation for layer 1 and number of cells 500 with the heat map overlaid for variable z.</p>
</div>



</div>

<div id="vignettes" class="section level1" number="2">

# <span class="header-section-number">2.</span> Vignettes

Following are the links to the vignettes for the HVT package:

<div id="hvt-vignette" class="section level2" number="2.1">

## <span class="header-section-number">2.1</span> HVT Vignette

<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_vignette.html" target="_blank">**HVT Vignette (Created at: 17-May-2018):**</a> Contains descriptions of the functions used for vector quantization and construction of hierarchical voronoi tessellations for data analysis.


</div>

<div id="hvt-model-diagnostics-vignette" class="section level2" number="2.2">

## <span class="header-section-number">2.2</span> HVT Model Diagnostics Vignette

<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_model_diagnostics_vignette.html" target="_blank">**HVT Model Diagnostics Vignette (Created at: 17-May-2018):**</a> Contains descriptions of functions used to perform model diagnostics and validation for HVT model.

</div>

<div id="hvt-scoring-cells-with-layers-using-scoreLayeredHVT" class="section level2" number="2.3">

## <span class="header-section-number">2.3</span> HVT Scoring Cells with Layers using scoreLayeredHVT

<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/Scoring_Cells_with_Layers_using_scoreLayeredHVT.html" target="_blank">**HVT Scoring Cells with Layers using scoreLayeredHVT (Created at: 16-May-2023):**</a> Contains descriptions of the functions used for scoring cells with layers based on a sequence of maps using scoreLayeredHVT.

</div>

<div id="temporal-analysis-and-visualization-leveraging-time-series-capabilities-in-hvt" class="section level2" number="2.4">

## <span class="header-section-number">2.4</span> Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT

<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/HVT_Temporal_Analysis.html" target="_blank">**Temporal Analysis and Visualization: Leveraging Time Series Capabilities in HVT (Created at: 26-Oct-2023):**</a> Contains descriptions of the functions used for analyzing time series data and its flow maps.

</div>


<div id="visualizing-llm-embeddings-using-hvt-hierarchical-voronoi-tessellation" class="section level2" number="2.5">

## <span class="header-section-number">2.5</span> Visualizing LLM Embeddings using HVT (Hierarchical Voronoi Tessellation)

<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/LLM_Embeddings_in_HVT.html" target="_blank">**Visualizing LLM Embeddings using HVT  (Created at: 16-May-2024):**</a> Contains the implementation and analysis of hierarchical clustering using the `clustHVT` function to evaluate and visualize token embeddings generated by OpenAI.
</div>

<div id="#implementation-of-t-sne-and-umap-to-trainhvt-function" class="section level2" number="2.6">


## <span class="header-section-number">2.6</span> Implementation of t-SNE and UMAP to trainHVT function


<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/Implementation_of_tsne_umap_in_trainHVT.html" target="_blank">**Implementation of t-SNE and UMAP in trainHVT function (Created at: 14-Aug-2024):**</a> Contains enhancements to the `trainHVT` function with advanced dimensionality reduction techniques such as t-SNE and UMAP, and includes a table of evaluation metrics to improve analysis, visualization, and interpretability.

</div>


<div id="#dynamic-forecasting-of-macroeconomic-time-series-dataset-using-hvt" class="section level2" number="2.7">


## <span class="header-section-number">2.7</span> Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT


<a href="https://nbviewer.org/github/Mu-Sigma/HVT/blob/master/vignettes/Dynamic_Forecasting_macroeconomic_data.html" target="_blank">**Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT (Created at: 12-Nov-2024):**</a> Contains enhancements to the `trainHVT` function with advanced dimensionality reduction techniques such as t-SNE and UMAP, and includes a table of evaluation metrics to improve analysis, visualization, and interpretability.

</div>


<div id="version-history" class="section level1" number="3">

# <span class="header-section-number">3.</span> Version History 

<div id="hvt-(v25.1.1)-whats-new" class="section level2" number="3.1">

## 3.5 HVT (v25.1.1) 

27th January, 2025

In this version of the HVT package, the following new features and vignette have been introduced:

**Features**

1. **Dynamic Forecasting of a Time Series Dataset**: This update introduces a new function called `msm` Monte Carlo Simulations of Markov Chain for dynamic forecasting of states in time series dataset. It supports both ex-post and ex-ante forecasting, offering valuable insights into future trends while resolving state transition challenges through clustering and nearest-neighbor methods to enhance simulation accuracy.

2. **Z score Plots**: This update introduces a new function called `plotZscore` that generates Z-score plots corresponding to the HVT cells for the given data, offering a visual representation of data distribution and highlighting potential outliers.

**Vignette** 

1. **Dynamic Forecasting of Macroeconomic Time Series Dataset using HVT**: This vignette illustrates the practical use of the new msm function on a macroeconomic dataset with 10 variables. It covers all steps, including data preparation, model training, scoring, and forecasting, while addressing challenges related to state transitions and evaluating performance using Mean Absolute Error (MAE).




</div>


<div id="hvt-(v24.9.1)" class="section level2" number="3.2">

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


<div id="hvt-(v24.5.2)" class="section level2" number="3.3">

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



<div id="hvt-(v23.11.02)" class="section level2" number="3.4">

## 3.2 HVT (v23.11.02) 

17th November, 2023

This version of HVT package offers functionality to score cells with layers based on a sequence of maps created using `scoreLayeredHVT`. Given below are the steps to created the successive set of maps.

1. **Map A** - The output of `trainHVT` function which is trained on parent data.

2. **Map B** - The output of `trainHVT` function which is trained on the 'data with novelty' created from `removeNovelty` function.

3. **Map C** - The output of `trainHVT` function which is trained on the 'data without novelty' created from `removeNovelty` function.

The `scoreLayeredHVT` function uses these three maps to score the test datapoints.

Let us try to understand the steps with the help of the diagram below

<img src="https://raw.githubusercontent.com/Mu-Sigma/HVT/master/vignettes/pngs/scoreLayeredHVT_function.png" width="672px" height="480px" />
<p class="caption">
Figure 2: Data Segregation for scoring based on a sequence of maps using scoreLayeredHVT()</p>



</div>

<div id="hvt-(v22.12.06)" class="section level2" number="3.5">

## 3.1 HVT (v22.12.06) 

06th December, 2022

This version of HVT package offers features for both training an HVT model and eliminating outlier cells from the trained model.

1. **Training or Compression:** The initial step entails training the parent data using the `trainHVT` function, specifying the desired compression percentage and quantization error.

2. **Remove novelty cells:** Following the training process, outlier cells can be identified manually from the 2D hvt plot. These outlier cells can then be inputted into the `removeNovelty` function, which subsequently produces two datasets in its output: one containing 'data with novelty' and the other containing 'data without novelty'.


</div>


<div id="installation-of-hvt-(v25.1.1)" class="section level2" number="4">

# <span class="header-section-number">4.</span> Installation of HVT (v25.1.1)

<div class="sourceCode" id="cb1">

**CRAN Installation**

`install.packages("HVT")`

**Git Hub Installation**

`library(devtools)`

##Extend the timeout duration for initial installation

`options(timeout = 1200)`

`devtools::install_github(repo = "Mu-Sigma/HVT")`

</div>

</div>


