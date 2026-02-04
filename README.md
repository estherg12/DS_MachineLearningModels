# Data Science: Machine Learning with R (HarvardX)
This repository contains a collection of R scripts developed while completing the HarvardX: Data Science Building Machine Learning Models course. The scripts follow chapters 27 through 33 of the course material, transitioning from theoretical foundations of probability to advanced ensemble modeling and matrix factorization.
The goal of this project is to implement Machine Learning algorithms from scratch using the ```caret```, ```dslabs```, and ```tidyverse``` ecosystems in R. The repository is organized to show the evolution of a data scientist, moving from simple decision rules to complex multi-model ensembles.

## Project Roadmap & File Directory
The files are categorized by their learning phase and purpose. It is recommended to explore them in the following order:
1. Foundations and Probability
   * 2.1BasicMLAlgorithims.R: introduction to simple decision rules and manual cutoffs (Iris and Heights datasets)
   * 2.2ConditionalProbabilities.R: theoretical grounding in Bayes' Theorem, prevalence, and conditional expectations

2. Parametric and Non-parametric Regression
   * regresionLineal.R and 3.1RegresionLineal.R: predicting continuous outcomes using Linear Regression and evaluating with RMSE
   * 3.2Smoothing.R: non-linear trend detection using ```LOESS``` (Local Regression) on mortality and ```MNIST``` data
   * regresionLogistica.R: categorical classification and decision boundary for binary outcomes

3. Model Tuning and Validation
   * 4ValidacionCruzada&Bootstrap.R: implementation of kNN using Bootstrap to estimate model stability
   * knnValidacionCruzada.R: advanced kNN tuning using the ```caret``` package to optimize the hyperparameter ```k```
   * 5.1TreesRandomForests.R: building decision trees and random forests to handle non-linear complex data

4. High - Dimensional data and Optimazation
   * 5.2ValidacionCruzada.R: data leakage, p-hacking and importance of a valid feature selection
   * 6.3FactorizacionMatrices.R: using Singular Value Decomposition (SVD) and PCA to reduce dimensionality in large datasets
   * 6.3Regularizacion.R: implementing penalties to account for "noisy" small sample sizes in ranking systems
  
5. Applied case studies
   * 6.mnist.R: digit recognition using the classic ```MNISR``` dataset
   * BreastCancerProject.R: real world application of PCA and Ensemble modeling for biopsy diagnosis
   * Titanic.R: classic kaggle challenge of predicting survival using socio-economic and demographic features
   * 6.2SistemasRecomendacion.R: building a movie recommendation engine using ```MovieLens``` data
  
## User Manual
### Prerequisites
To run these files, you need R (v4.0+) and RStudio. You must install the following packages:
```install.packages(c("tidyverse", "caret", "dslabs", "matrixStats", 
                   "rpart", "randomForest", "lubridate", "broom", "HistData"))
```

### Execution Steps
1. Clone the Repository
```git clone https://github.com/your-username/your-repo-name.git```
2. Set working directory: open RStudio and set your directory to the folder where the files are located
3. Run Scripts: open any file and run the code block-by-block. Some files, like 6.mnist.R or BreastCancerProject.R perform Cross-Validation which may take 1-5 minutes to execute (depending on the HW)
4. Check outputs: the scripts are designed to output diagnostic plots and metrics in the Console

## Key Insights gained
* Thi Bias-Variance tradeoff: learning how to tune k in kNN or cp in trees to prevent overfitting
* Ensemble power: demonstrating that combining multiple models (random forest + kNN + GLM) usually outperforms any single model
* Preprocessing matters: scaling, centering and handling NAs
