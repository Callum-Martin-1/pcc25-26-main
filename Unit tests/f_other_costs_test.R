library(testthat)
library(tidyr)

f_other_costs <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  other_costs <- read_xlsx(
    x,
    sheet = 3,
    range = "A10:D50"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    pivot_longer(
      cols = 4,
      names_to = "Category",
      values_to = "Number"
    ) %>%
    mutate(
      Number = as.character(Number),
      Number_QA = case_when(
        Number == "" | is.na(Number) ~ "No_number_given",
        grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) &
          !grepl("e\\+", Number) &
          !grepl("e-", Number) ~ "Text_given",
        TRUE ~ Number
      )
    )
  
  as.data.frame(other_costs)
  
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_other_costs extracts the PCC name", {
  
  result <- f_other_costs(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("f_other_costs returns a data.frame", {
  
  result <- f_other_costs(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_other_costs(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("pivot gives correct row count", {
  
  result <- f_other_costs(x)
  
  expect_equal(nrow(result), 6)
  
})