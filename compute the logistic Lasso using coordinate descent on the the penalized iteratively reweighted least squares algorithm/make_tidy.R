library(tidyverse)
library(tidymodels)
#install.packages("parsnip")
library(parsnip)
# setwd("C:/Users/WLJY8/Desktop/Courses/YEAR 4/STA314/A2/qs")
# source("functions qs.R")

IRLS <- function(mode = "classification", penalty) {
  args <- list(penalty = rlang::enquo(penalty))
  new_model_spec("IRLS",
                 args = args,
                 mode = mode,
                 eng_args = NULL,
                 method = NULL,
                 engine = NULL)
}

set_new_model("IRLS")

set_model_mode(model = "IRLS", mode = "classification")
set_model_engine("IRLS",
                 mode = "classification",
                 eng = "fit_logistic_lasso"
)

set_dependency("IRLS", eng = "fit_logistic_lasso", pkg = "base")

set_model_arg(
  model = "IRLS",
  eng = "fit_logistic_lasso",
  parsnip = "penalty", ## what parsnip will call it
  original = "lambda", ## what we call it!
  func = list(pkg = "dials", fun = "penalty"), ## Use dials::penalty() to set
  has_submodel = FALSE # If you don't know, don't worry.
)

set_encoding(
  model = "IRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)

#show_model_info("IRLS")


set_fit(
  model = "IRLS",
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
  model = "IRLS",
  eng = "fit_logistic_lasso",
  mode = "classification",
  type = "class",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict_logistic_lasso"),
    args = list(
      fit = expr(object$fit),
      new_x = expr(as.matrix(new_data[, names(object$fit$beta)]))
    )
  )
)

update.IRLS <- function(object, ...) {
  new_model_spec("IRLS", args = object$args, eng_args = NULL,
                 mode = "classification", method = NULL, engine = object$engine)
}
