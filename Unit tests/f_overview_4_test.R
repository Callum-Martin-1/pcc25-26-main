library(testthat)
library(tidyr)

f_overview_4 <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  overview_4 <- read_xlsx(x, range = "1. Overview!H23:I33") %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    pivot_longer(
      cols = 2,
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
      ),
      number_num = suppressWarnings(as.numeric(Number))
    ) %>%
    mutate(
      number_num = tidyr::replace_na(number_num, 0)
    )
  
  as.data.frame(overview_4)
  
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_overview_4 extracts the PCC name", {
  
  result <- f_overview_4(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_overview_4(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_overview_4 returns a data.frame", {
  
  result <- f_overview_4(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_overview_4(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("pivot gives correct row count", {
  
  result <- f_overview_4(x)
  
  expect_equal(nrow(result), 10)
  
})