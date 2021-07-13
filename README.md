
# jemodel

Provides some helper functions for modelling (mainly the glmnet and ranger packages).

This package is in very early development.

+ ROC functions. `roc_plot()` and `roc_cut()` to easily get ROC plots and optimal cuts.
+ Ranger functions. Several functions beginning `rang_` to work with the ranger package: mtry tuning, ROC output, out-of-bag errors by number of trees.
+ `glmnet_to_table()`: put coefficients of a glmnet model into a more convenient table.


## Installation

You can install the package from github with:

```
# install.packages("devtools")
devtools::install_github("jedwards24/jemodel")
```


