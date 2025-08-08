## Simulation and analyses of multifractional processes 
<p>
<img width="200" height="220" alt="logo" src="https://github.com/user-attachments/assets/5055ed96-bbed-4907-b609-5411c4f84f92" align="right"align="right" />
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
H <- function(t) {return(0.4 +0*t)}
GHBMP(t,H)
```
Linear Hurst function
```{r}
H <- function(t) {return(0.4 +0*t)}
GHBMP(t,H)
```

Oscillating Hurst function
```{r}
H <- function(t) {return(0.5-0.4*sin(6*3.14*t))}
GHBMP(t,H)
```
Piecewise Hurst function
```{r}
H <- function(x) {
ifelse(x >= 0 & x <= 0.8, 0.375 * x + 0.2,
      ifelse(x > 0.8 & x <= 1,-1.5 * x + 1.7, NA))
}
GHBMP(t,H)
```
## Citation
Ayache A, Olenko A, Samarakoon N (2025). “On Construction, Properties and Simulation
of Haar-Based Multifractional Processes.” doi:10.48550/arXiv.2503.07286. Submitted,
URL https://arxiv.org/abs/2503.07286.
