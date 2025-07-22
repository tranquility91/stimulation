# stimulation

This repository contains a collection of R scripts for statistical simulation and estimation, covering topics like Monte Carlo integration, inverse transform sampling, variance reduction techniques, and simulation-based inference. Each script corresponds to a specific chapter exercise or homework.

## Contents

### CH2.R
This script implements multiple **Monte Carlo integration** tasks:

- 2-1: Estimates ∫₀¹ exp(x) dx using uniform sampling.
- 2-2: Estimates ∫₋²² exp(x + x²) dx via importance sampling.
- 2-3: Approximates ∫₋∞^∞ exp(-x²) dx, leveraging normal random variables and Gaussian density.
- 2-4: Contains a custom sampling method (appears to be for estimating sample size, partial).

### CH3.R
Simulates random variables using the **inverse transform method**:

- Simulates from distributions defined by F(x) = xⁿ, visualizes samples vs. theoretical PDF.
- Implements Gamma(5, 2) sampling using the product of uniforms method and compares histogram with theory.

### CH4.R
Explores **online computation of sample mean and variance** and **confidence interval convergence**:

- Simulates 300,000 normal samples, computes rolling mean and variance (Welford's algorithm).
- Plots mean, variance, and confidence intervals over sample size.

### CH5.R
Demonstrates **variance reduction via antithetic variates**:

- Compares naive Monte Carlo estimates of E[exp(U)] with antithetic pairs exp(U) and exp(1 − U).
- Calculates variance reduction percentage.
- Repeats until confidence interval length is less than 0.001 using stopping rule.

### HW2.R
Covers two distinct simulation problems:

- Q3: Uses inverse transform sampling for a custom discrete distribution.
- Q4: Simulates matching cards to positions and estimates expected number of correct placements (a variant of the **matching problem**).

### HW3.R
Focuses on truncated exponential distribution simulation:

- Q6: Uses inverse transform method to sample from Exp(1) truncated at 0.05.
- Compares theoretical vs. simulated mean.
- Additional sections (e.g., Q10) present other distribution or sampling methods (content truncated).

### HW4.R
Simulates required sample size and estimates **Euler’s constant (e)**:

- Q7: Computes required `n` for achieving a specified margin of error.
- Q8: Repeatedly draws `U(0,1)` values until their sum exceeds 1, and uses average count to estimate **e**.

---

## How to Run

To run any script:

```r
source("CH2.R")  # or any other filename
