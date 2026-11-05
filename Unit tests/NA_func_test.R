library(testthat)

NA_func <- function(x) {
  x %>%
    group_by(PCC) %>%
    summarise(
      across(
        everything(),
        ~ sum(is.na(.)),
        .names = "{.col}"
      ),
      .groups = "drop"
    )
}

test_that("returns one row per PCC", {
  
  df <- tibble(
    PCC = c("A", "A", "B"),
    value = c(1, NA, 2)
  )
  
  result <- NA_func(df)
  
  expect_equal(
    nrow(result),
    2
  )
  
})

test_that("counts NAs correctly", {
  
  df <- tibble(
    PCC = c("A", "A", "B"),
    value = c(1, NA, NA)
  )
  
  result <- NA_func(df)
  
  expect_equal(
    result$value[result$PCC == "A"],
    1
  )
  
  expect_equal(
    result$value[result$PCC == "B"],
    1
  )
  
})

test_that("returns zero when no NAs exist", {
  
  df <- tibble(
    PCC = c("A", "A"),
    value = c(1, 2)
  )
  
  result <- NA_func(df)
  
  expect_equal(
    result$value,
    0
  )
  
})

test_that("counts NAs for all columns", {
  
  df <- tibble(
    PCC = c("A", "A"),
    x = c(NA, 1),
    y = c(NA, NA)
  )
  
  result <- NA_func(df)
  
  expect_equal(result$x, 1)
  expect_equal(result$y, 2)
})

test_that("PCC values preserved", {
  
  df <- tibble(
    PCC = c("A", "A", "B"),
    value = c(1, NA, 2)
  )
  
  result <- NA_func(df)
  
  expect_setequal(
    result$PCC,
    c("A", "B")
  )
  
})

test_that("handles columns that are entirely NA", {
  
  df <- tibble(
    PCC = c("A", "A"),
    value = c(NA, NA)
  )
  
  result <- NA_func(df)
  
  expect_equal(
    result$value,
    2
  )
  
})
