Gathering knowledge of supply curves in electricity markets is critical to both energy producers and regulators. Indeed, power producers strategically plan their generation of electricity considering various scenarios to maximize profit, leveraging the characteristics of these curves. For their part, regulators need to forecast the supply curves to monitor the market's performance and identify market distortions. However, the prevailing approaches in the technical literature for analyzing, clustering, and predicting these curves are based on structural assumptions that electricity supply curves do not satisfy in practice, namely, boundedness and smoothness. Furthermore, any attempt to satisfactorily cluster the supply curves observed in a market must take into account the market's specific features.

Against this background, this article introduces a hierarchical clustering method based on a novel weighted-distance that is specially tailored to non-bounded and non-smooth supply curves and embeds information on the price distribution of offers, thus overcoming the drawbacks of conventional clustering techniques. Once the clusters have been obtained, a supervised classification procedure is used to characterize them as a function of relevant market variables. Additionally, the proposed distance is used in a learning procedure by which explanatory information is exploited to forecast the supply curves in a day-ahead electricity market. This procedure combines the idea of nearest neighbors with a machine-learning method. The prediction performance of our proposal is extensively evaluated and compared against two nearest-neighbor benchmarks and existing competing methods. To this end, supply curves from the markets of Spain, Pennsylvania-New Jersey-Maryland (PJM), and West Australia are considered.

This joint work has been published [1]. The procedure applied to the Spanish market is archived and represented in the repository but without losing generalization to other markets.

[1] Li, Z., Alonso, A. M., Elías, A., & Morales, J. M. (2024). Clustering and forecasting of day-ahead electricity supply curves using a market-based distance. International Journal of Electrical Power & Energy Systems, 158, 109977. https://doi.org/https://doi.org/10.1016/j.ijepes.2024.109977

**UPDATE**
* A C++ version of the distance calculation function is incorporated
* A package including the above function is built. Therefore, each core involved in the parallel computation could call the function as we did in the pure R context.

We reduce the time by magnitude, from ~160 seconds to 60 seconds, in the case of obtaining 10,000 distances. See the performance comparison and package-built procedure in *cpp code/distance_calcualtion_Rcpp_example.R*

