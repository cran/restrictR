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
require_feature <- restrict("feature") |>
  require_numeric(no_na = TRUE, finite = TRUE)

require_newdata <- restrict("newdata") |>
  require_df() |>
  require_has_cols(c("x1", "x2")) |>
  require_col("x1", require_feature) |>
  require_col("x2", require_feature) |>
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
require_event <- restrict("event") |>
  require_class("Date")

require_event(as.Date("2026-01-01"))

## -----------------------------------------------------------------------------
require_event("2026-01-01")

## -----------------------------------------------------------------------------
require_survey <- restrict("survey") |>
  require_df() |>
  require_has_cols(c("age", "income", "status")) |>
  require_col("age", restrict("age") |>
                require_numeric(no_na = TRUE) |>
                require_between(lower = 0, upper = 150)) |>
  require_col("income", restrict("income") |>
                require_numeric(no_na = TRUE, finite = TRUE)) |>
  require_col("status", restrict("status") |>
                require_one_of(c("active", "inactive", "pending")))

## -----------------------------------------------------------------------------
good_survey <- data.frame(
  age = c(25, 40, 33),
  income = c(35000, 60000, 45000),
  status = c("active", "inactive", "active")
)
require_survey(good_survey)

## -----------------------------------------------------------------------------
bad_survey <- data.frame(
  age = c(25, -5, 200),
  income = c(35000, 60000, 45000),
  status = c("active", "inactive", "active")
)
require_survey(bad_survey)

## -----------------------------------------------------------------------------
require_age <- restrict("age") |>
  require_integer(no_na = TRUE) |>
  require_between(0, 120)

require_people <- restrict("people") |>
  require_df() |>
  require_col("age", require_age)

validation_errors(require_people, data.frame(age = c(30L, 150L)))

## -----------------------------------------------------------------------------
require_layers <- restrict("layers") |>
  require_class("list") |>
  require_each(restrict("layer") |> require_numeric())

validation_errors(require_layers, list(1, "a", 3))

## -----------------------------------------------------------------------------
require_weights <- restrict("weights") |>
  require_numeric(no_na = TRUE) |>
  require_positive() |>
  allow_null()
require_weights(NULL)

require_num_or_df <- restrict("x") |>
  require_any(
    restrict("x") |> require_numeric(),
    restrict("x") |> require_df() |> require_has_cols("value")
  )
validation_errors(require_num_or_df, "a")

## -----------------------------------------------------------------------------
require_period <- restrict("start") |>
  require_between(as.Date("2020-01-01"), as.Date("2020-12-31"))
validation_errors(require_period, as.Date("2021-03-01"))

## -----------------------------------------------------------------------------
require_code <- restrict("code") |>
  require_character() |>
  require_nonempty() |>
  require_pattern("^[A-Z]{3}-[0-9]{2}$")
validation_errors(require_code, c("ABC-12", "abc-12", " "))

## -----------------------------------------------------------------------------
require_input <- restrict("path") |> require_file_exists(extension = "csv")
validation_errors(require_input, "data/missing.csv")

## -----------------------------------------------------------------------------
train <- data.frame(g = factor(c("a", "b")), x = 1:2)
require_newdata <- restrict("newdata") |>
  require_names(c("g", "x"), mode = "superset") |>
  require_col("g", restrict("g") |>
                 require_levels(levels(train$g), mode = "subset"))
validation_errors(require_newdata,
                  data.frame(g = factor("c"), x = 3L))

## -----------------------------------------------------------------------------
messy_survey <- data.frame(
  age = c(25, -5, 200),
  income = c(35000, NA, 45000),
  status = c("active", "banned", "active")
)
require_survey(messy_survey, .on_fail = "all")

## -----------------------------------------------------------------------------
is_valid(require_survey, good_survey)
validation_errors(require_survey, messy_survey)

## ----eval = FALSE-------------------------------------------------------------
# test_that("newdata contract", {
#   expect_valid(require_newdata, data.frame(g = factor("a"), x = 1L))
#   expect_invalid(require_newdata, data.frame(x = 1L), regexp = "missing required")
# })

## -----------------------------------------------------------------------------
steps(require_code)[, c("step", "label")]

## -----------------------------------------------------------------------------
require_weights <- restrict("weights") |>
  require_numeric(no_na = TRUE) |>
  require_between(lower = 0, upper = 1) |>
  require_custom(
    label = "must sum to 1",
    fn = function(value, name, ctx) {
      if (abs(sum(value) - 1) > 1e-8) {
        fail(name, "must sum to 1",
             found = sprintf("sum = %g", sum(value)))
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
        fail(name, sprintf("expected %d probabilities", ctx$n_classes),
             found = sprintf("length %d", length(value)))
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

