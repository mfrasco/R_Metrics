# Metrics

![Build Status](https://travis-ci.org/mfrasco/Metrics.png)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Metrics)](https://cran.r-project.org/package=Metrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/Metrics)](https://cran.rstudio.com/web/packages/Metrics/)

## How to Install this Package

This package is distributed from CRAN. From the R prompt, run `install.packages("Metrics")`.

## Metrics Repo

This repository contains code for the **Metrics** package in R. **Metrics** was created by Ben Hamner and came from [this github repo](https://github.com/benhamner/Metrics/). Hamner's repo contains packages for common machine learning metrics in several programming languages, not just R. On 2017-04-21, CRAN orphaned the R package. To revive the status of the R package, I cloned the original and created this repo. I have added new metrics, improved documentation, and fixed bugs. This repository will be the home of active development on the **Metrics** R package moving forward.

## Community Feedback

If you notice anything wrong with the **Metrics** package or have any ideas on how to improve it, please create an issue in this github repository that describes your issue. I also welcome improvements to this package via a pull request. This is a simple R package, which makes it perfect for first time open source contributors. [Here is a guide](https://opensource.guide/how-to-contribute/) that walks you through how to make an open source contribution.

## What Metrics are Included in this Package?

All functions in the **Metrics** package take at least two arguments: `actual` and `predicted`. In the table below, I abbreviate `actual` as x and `predicted` as y for the sake of mathematical brevity.

| Metric Type | Metric Name | Function Name | Formula |
| ---- | ------------------------ | ---- | ------------------------------- |
| regression | Squared Error | se | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%28x_i-y_i%29%5E2) |
| regression | Mean Squared Error | mse | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20y_i%29%20%5E%202) |
| regression | Root Mean Squared Error | rmse | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Csqrt%7B%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20y_i%29%5E%202%7D) |
| regression | Absolute Error | ae | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Clvert%20x_i%20-%20y_i%20%5Crvert) |
| regression | Mean Absolute Error | mae | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%5Clvert%20x_i%20-%20y_i%20%5Crvert) |
| regression | Absolute Percent Error | ape | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B%5Clvert%20x_i%20-%20y_i%20%5Crvert%7D%7Bx_i%7D) |
| regression | Mean Absolute Percent Error | mape | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%5Cfrac%7B%5Clvert%20x_i%20-%20y_i%20%5Crvert%7D%7Bx_i%7D) |
| regression | Symmetric Mean Absolute Percent Error | smape | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B2%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%5Cfrac%7B%5Clvert%20x_i%20-%20y_i%20%5Crvert%7D%7B%5Clvert%20x_i%20%5Crvert%20&plus;%20%5Clvert%20y_i%20%5Crvert%7D) |
| regression | Squared Log Error | sle | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cbig%28%5Cln%281%20&plus;%20x_i%29%20-%20%5Cln%281%20&plus;%20y_i%29%5Cbig%29%20%5E%202) |
| regression | Mean Squared Log Error | msle | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%5Cbig%28%5Cln%281%20&plus;%20x_i%29%20-%20%5Cln%281%20&plus;%20y_i%29%5Cbig%29%20%5E%202) |
| regression | Root Mean Squared Log Error | rmsle | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Csqrt%7B%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20%5Cbig%28%5Cln%281%20&plus;%20x_i%29%20-%20%5Cln%281%20&plus;%20y_i%29%5Cbig%29%20%5E%202%7D) |
| regression | Relative Squared Error | rse | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20y_i%29%20%5E%202%5CBig%5D%20/%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20%5Cbar%7Bx%7D%29%20%5E%202%5CBig%5D) |
| regression | Root Relative Squared Error | rrse | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Csqrt%7B%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20y_i%29%20%5E%202%5CBig%5D%20/%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%28x_i%20-%20%5Cbar%7Bx%7D%29%20%5E%202%5CBig%5D%7D) |
| regression | Relative Absolute Error | rae | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%5Clvert%20x_i%20-%20y_i%20%5Crvert%5CBig%5D%20/%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%5Clvert%20x_i%20-%20%5Cbar%7Bx%7D%20%5Crvert%5CBig%5D) |
| time series |  Mean Absolute Scaled Error | mase | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5CBig%5B%5Csum_%7Bi%3D1%7D%5En%20%5Clvert%20x_i%20-%20y_i%20%5Crvert%5CBig%5D%20/%20%5CBig%5B%5Cfrac%7Bn%7D%7Bn%20-%20m%7D%20%5Csum_%7Bi%3D1&plus;m%7D%5En%20%5Clvert%20x_i%20-%20x_%7Bi-m%7D%20%5Crvert%20%5CBig%5D)|
| classification | Classification Error | ce | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20I%28x_i%20%5Cneq%20y_i%29) |
| classification | Accuracy | accuracy | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20I%28x_i%20%3D%20y_i%29) |
| classification | F1 Score | f1 | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B2%20*%20%5Ctext%7Bprecision%7D%20*%20%5Ctext%7Brecall%7D%7D%7B%5Ctext%7Bprecision%7D%20&plus;%20%5Ctext%7Brecall%7D%7D) |
| binary classification | Area Under ROC Curve | auc | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cint_0%5E1%20%5B1%20-%20G_1%28G%5E%7B-1%7D_0%281%20-%20v%29%29%5D%20dv). `help(auc)` for details. |
| binary classification | Log Loss | ll | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20x_i%20*%20%5Cln%28y_i%29%20&plus;%20%281%20-%20x_i%29%20*%20%5Cln%281%20-%20y_i%29) |
| binary classification | Mean Log Loss | logloss | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7Bn%7D%20%5Csum_%7Bi%3D1%7D%5En%20x_i%20*%20%5Cln%28y_i%29%20&plus;%20%281%20-%20x_i%29%20*%20%5Cln%281%20-%20y_i%29) |
| binary classification | Precision | precision | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7B%5Csum%20I%28y_i%20%3D%201%29%7D%5Csum_%7Bi%3D1%7D%20%5E%7Bn%7D%20I%28y_i%20%3D%201%29x_i) |
| binary classification | Recall | recall | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%5Cfrac%7B1%7D%7B%5Csum%20I%28x_i%20%3D%201%29%7D%5Csum_%7Bi%3D1%7D%20%5E%7Bn%7D%20I%28x_i%20%3D%201%29y_i) |
| binary classification | F-beta Score | fbeta_score | ![equation](https://latex.codecogs.com/gif.latex?%5Cdpi%7B150%7D%20%281%20&plus;%20%5Cbeta%5E2%29%20%5Cfrac%7B%5Ctext%7Bprecision%7D%20*%20%5Ctext%7Brecall%7D%7D%7B%20%28%5Cbeta%5E2%20*%20%5Ctext%7Bprecision%7D%29%20&plus;%20%5Ctext%7Brecall%7D%7D) |
| binary classification | Sensitivity | sensitivity | ![equation](http://latex.codecogs.com/gif.latex?\frac{TP}{TP%20+%20FN}) |
| binary classification | Specificity | specificity | ![equation](http://latex.codecogs.com/gif.latex?\frac{TN}{TN%20+%20FP}) |
| binary classification | False Negative Rate | fnr | ![equation](http://latex.codecogs.com/gif.latex?\frac{FN}{TP%20+%20FN}%20=%20(1%20-%20{sensitivity})) |
| binary classification | False Positive Rate | fpr | ![equation](http://latex.codecogs.com/gif.latex?\frac{FP}{TN%20+%20FP}%20=%20(1%20-%20{specificity})) |