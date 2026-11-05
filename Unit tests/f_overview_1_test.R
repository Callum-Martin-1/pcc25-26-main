library(testthat)

f_overview_1 <- function(x) {
  
  PCC <- read_xlsx(x, range = "1. Overview!D6")
  PCC_name <- colnames(PCC)
  
  if (length(PCC_name) == 0) {
    stop(paste("No PCC name found in", x))
  }
  
  overview_1 <- read_xlsx(x, range = "1. Overview!B10:D17") %>%
    mutate(PCC = PCC_name) %>%
    pivot_longer(
      cols = 2:3,
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
      number_num = suppressWarnings(as.numeric(Number_QA))
    ) %>%
    mutate(
      number_num = ifelse(is.na(number_num), 0, number_num)
    )
  
  return(as.data.frame(overview_1))
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_overview_1 extracts the PCC name", {
  
  result <- f_overview_1(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_overview_1(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_overview_1 returns a data.frame", {
  
  result <- f_overview_1(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_overview_1(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("pivot doubles row count", {
  
  result <- f_overview_1(x)
  
  expect_equal(nrow(result), 14)
  
})