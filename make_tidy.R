#######################################
# Assignment 2 parsnip Models
# STA314H1, Fall 2020
# Author: Ziyue Yang
# Student Number: 1004804759
# Contact: ziyue.yang@mail.utoronto.ca
##########################s#############

# Loading libraries and functions
library(tidymodels)
library(tidyverse)
source("functions.R")

# Registering new model PIRLS

PIRLS <- function(mode = "classification") {
  # "PIRLS" for Penalized Iterative Reweighted Least Squares
  new_model_spec("PIRLS",
                 args = NULL, 
                 mode = mode,
                 eng_args = NULL,
                 method = NULL, 
                 engine = NULL)
}
  
set_new_model("PIRLS")
set_model_mode(model = "PIRLS", mode = "classification")
set_model_engine("PIRLS",
  mode = "classification",
  eng = "fit_logistic_lasso"
)

set_dependency("PIRLS", eng = "fit_logistic_lasso", pkg = "base")

set_encoding(
  model = "PIRLS",
  eng = "fit_logistic_lasso",
  mode = "classification", 
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE
    # allow_sparse_x = FALSE
  )
)

show_model_info("PIRLS")

# Configuring PIRLS 

set_fit(
  model = "PIRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  value = list(
    interface = "matrix",
    protect = c("x", "y"),
    func = c(fun = "fit_logistic_lasso"),
    defaults = list()
      )
)

set_pred(
  model = "PIRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict_logistic_lasso"),
    args = list(
      fit = expr(object$fit),
      new_x = expr(as.matrix(new_data[, names(object$fit$data)]))
    )
  )
)

show_model_info("PIRLS")

