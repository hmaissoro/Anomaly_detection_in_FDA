## Anomaly Detection in Curve Collections using Function Data Analysis
In the context of functional data analysis, we analyze collections of trajectories from stochastic processes. Here, the observation unit is a curve or a vector of curves. This differs from traditional time series analysis, where the observations come from a single trajectory or a fixed-dimension vector of trajectories. Functional data analysis takes advantage of observing multiple trajectories simultaneously, eliminating the need for assumptions like stationarity or unit root processes, which are typical in time series analysis of single trajectories.

A typical example of functional data is the collection of load curves from wind farms across France. Wind farms produce electricity over time, which is represented by load curves. Similarly, functional data arises when analyzing time series of electricity consumption by French companies, growth trajectories in medical data, or water flow measurements from hydrometric stations for flood monitoring in France.

This repository contains code for detecting "anomalous" curves in a collection using the concept of "depth," which ranks curves from the center outward.

### Study and implementation of depth functions for functional data
- Tukey depth and simplicial depth functions
- Application to wind farm load curves
- Cluster-based anomaly detection to reduce the influence of exogenous factors like wind speed.

### Study and implementation of anomaly detection on the residuals of a functional linear model:
- The aim is to isolate the influence of wind on wind power generation for better anomaly detection.
- Functional linear model:
    - **Dependent variable:** Load curves
    - **Independent variable:** Wind speed curves at 100 meters.
- Residual extraction and anomaly detection applied to the entire curve collection and homogeneous clusters.
