# MCMC_wingposterior_ProcrustesDensity

This R script performs Bayesian Procrustes analysis of 2D landmark data. The analysis focuses on comparing two data sets, which could represent different species or samples, and visualizes the posterior distributions of a Procrustes variance parameter. Our novel, simple R package \textbf{BPviGM1} ("Bayesian Procrustes Variance-based inferences in Geometric Morphometrics 1") includes R codes for clear computation of the proposed models and methodologies (see appendix with the paper). This project includes scripts that read data, perform Generalized Procrustes Analysis (GPA), execute MCMC posterior sampling for various sample sizes, and generate density plots of the posterior distributions for the Procrustes variance parameter, allowing for comparison across different sample sizes. Make sure to customize the placeholder paths and data according to your needs.
Ensure you have the following R packages installed:
    • “Shapes”
    • “Morpho”
    • “BPviGM1”
    • “Scales”
    • “readxl”

License
This project is licensed under the MIT License - see the License file for details
