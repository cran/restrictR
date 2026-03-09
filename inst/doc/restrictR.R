## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  dev = "svglite",
  fig.ext = "svg",
  error = TRUE
)

## ----setup--------------------------------------------------------------------
library(restrictR)

## -----------------------------------------------------------------------------
require_newdata <- restrict("newdata") |>
  require_df() |>
  require_has_cols(c("x1", "x2")) |>
  require_col_numeric("x1", no_na = TRUE, finite = TRUE) |>
  require_col_numeric("x2", no_na = TRUE, finite = TRUE) |>
  require_nrow_min(1L)

## -----------------------------------------------------------------------------
good <- data.frame(x1 = c(1, 2, 3), x2 = c(4, 5, 6))
require_newdata(good)

## -----------------------------------------------------------------------------
require_newdata(42)

## -----------------------------------------------------------------------------
require_newdata(data.frame(x1 = c(1, NA), x2 = c(3, 4)))

## -----------------------------------------------------------------------------
require_newdata(data.frame(x1 = c(1, 2), x2 = c("a", "b")))

## -----------------------------------------------------------------------------
require_pred <- restrict("pred") |>
  require_numeric(no_na = TRUE, finite = TRUE) |>
  require_length_matches(~ nrow(newdata))

## -----------------------------------------------------------------------------
newdata <- data.frame(x1 = 1:5, x2 = 6:10)
require_pred(c(0.1, 0.2, 0.3, 0.4, 0.5), newdata = newdata)

## -----------------------------------------------------------------------------
require_pred(c(0.1, 0.2, 0.3), newdata = newdata)

## -----------------------------------------------------------------------------
require_pred(c(0.1, 0.2, 0.3))

## -----------------------------------------------------------------------------
require_pred(1:5, .ctx = list(newdata = newdata))

## -----------------------------------------------------------------------------
require_method <- restrict("method") |>
  require_character(no_na = TRUE) |>
  require_length(1L) |>
  require_one_of(c("euclidean", "manhattan", "cosine"))

## -----------------------------------------------------------------------------
require_method("euclidean")

## -----------------------------------------------------------------------------
require_method("chebyshev")

## -----------------------------------------------------------------------------
require_weights <- restrict("weights") |>
  require_numeric(no_na = TRUE) |>
  require_between(lower = 0, upper = 1) |>
  require_custom(
    label = "must sum to 1",
    fn = function(value, name, ctx) {
      if (abs(sum(value) - 1) > 1e-8) {
        stop(sprintf("%s: must sum to 1, sums to %g", name, sum(value)),
             call. = FALSE)
      }
    }
  )

## -----------------------------------------------------------------------------
require_weights(c(0.5, 0.3, 0.2))

## -----------------------------------------------------------------------------
require_weights(c(0.5, 0.5, 0.5))

## -----------------------------------------------------------------------------
require_probs <- restrict("probs") |>
  require_numeric(no_na = TRUE) |>
  require_custom(
    label = "length must match number of classes",
    deps = "n_classes",
    fn = function(value, name, ctx) {
      if (length(value) != ctx$n_classes) {
        stop(sprintf("%s: expected %d probabilities, got %d",
                     name, ctx$n_classes, length(value)), call. = FALSE)
      }
    }
  )

require_probs(c(0.3, 0.7), n_classes = 2L)

## -----------------------------------------------------------------------------
require_newdata

## -----------------------------------------------------------------------------
as_contract_text(require_newdata)

## -----------------------------------------------------------------------------
cat(as_contract_block(require_newdata))

## -----------------------------------------------------------------------------
base <- restrict("x") |> require_numeric()
v1 <- base |> require_length(1L)
v2 <- base |> require_between(lower = 0)

# base is unchanged
length(environment(base)$steps)
length(environment(v1)$steps)
length(environment(v2)$steps)

## -----------------------------------------------------------------------------
sessionInfo()

