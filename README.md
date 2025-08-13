## Simulation and analyses of multifractional processes 
<p>
<img width="200" height="220" alt="logo" src="https://github.com/user-attachments/assets/5055ed96-bbed-4907-b609-5411c4f84f92" align="right"align="right" />
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
 
Rmfrac provides a collection of tools for simulating, analysing and visualising multifractional processes and time series. The package includes built-in estimation techniques for the Hurst function, Local Fractal Dimension and several other geometric statistics. It provides highly customisable plotting functions for simulated realisations, user-provided time series and their statistics. 
</p>

**Features**
- Simulation of Brownian motion, fractional Brownian motion, fractional Gaussian noise, Brownian bridge and fractional Brownian bridge
- Simulation of Gaussian Haar-based multifractional process (GHBMP)
- Estimation of Hurst function and Local Fractal Dimension
- Customisable plotting functions for GHBMP and user provided time series with estimates of Hurst function and Local Fractal Dimension
- Estimation and visualisation of geometric statistics using realisations of stochastic processes and time series. Clustering based on the Hurst function, sojourn measure, excursion area, etc.
- An interactive Shiny application that provides options to explore and visualise the core functionalities of the package through simulations and user-provided time series.

 
## Installation

To install the development version of [Rmfrac](https://github.com/Nemini-S/Rmfrac) package version from GitHub:
```{r}
# Install devtools if not available
# install.packages("devtools")
devtools::install_github("Nemini-S/Rmfrac")
```

## Getting started 
```{r}
library(Rmfrac)
```
To simulate a Gaussian Haar-based multifractional process for a constant Hurst function
```{r}
t <- seq(0,1,by=(1/2)^10)
H1 <- function(t) {return(0.5 +0*t)}
X1 <- GHBMP(t,H1,J=12)
```
Oscillating Hurst function
```{r}
H2 <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
X2 <- GHBMP(t,H2,J=12)
```
Piecewise Hurst function
```{r}
H3 <- function(x) {
ifelse(x >= 0 & x <= 0.8, 0.375 * x + 0.2,
      ifelse(x > 0.8 & x <= 1,-1.5 * x + 1.7, NA))
}
X3 <- GHBMP(t,H3,J=12)
```
To estimate the Hurst function and Local Fractal Dimension with visualizations
```{r}
Hurst_estimates <- Hurst(X2,N=100)
LFD_estimates <- LFD(X2,N=100)

plot(X2,Raw_EST_H=TRUE,Smooth_Est_H=TRUE,LFD_Est=TRUE,LFD_Smooth_Est=TRUE)
```
<img width="1918" height="975" alt="image" src="https://github.com/user-attachments/assets/c7df246f-368f-41b2-b108-1b0f1dc002fa" />


## Citation
Ayache A, Olenko A, Samarakoon N (2025). “On Construction, Properties and Simulation
of Haar-Based Multifractional Processes.” doi:10.48550/arXiv.2503.07286. Submitted,
URL https://arxiv.org/abs/2503.07286.
