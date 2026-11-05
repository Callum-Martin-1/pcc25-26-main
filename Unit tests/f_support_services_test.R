library(testthat)
library(tidyr)

f_support_services <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  support_services <- read_xlsx(
    x,
    range = "2b. Finance - Support Services!A15:L218"
  ) %>%
    filter(!is.na(.[[1]])) %>%
    mutate(
      PCC = PCC_name,
      ID = row_number()
    ) %>%
    pivot_longer(
      cols = 9:10,
      names_to = "Variable",
      values_to = "Response"
    ) %>%
    mutate(
      Response = as.character(Response),
      num_check = case_when(
        grepl("Spend|Spent", Variable, ignore.case = TRUE) ~ "Number",
        TRUE ~ "Text"
      ),
      response_QA = case_when(
        Response == "" | is.na(Response) ~ "No_response_given",
        num_check == "Number" &
          grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Response) &
          !grepl("e\\+", Response) &
          !grepl("e-", Response) ~ "Text_given",
        TRUE ~ Response
      )
    )
  
  support_services <- as.data.frame(support_services)
  
  support_services$`Primary Crime Category*\r\n\r\n(hover for more info)` <-
    support_services$`Primary Crime Category*\r\n\r\n(hover for more info)` %>%
    str_trim() %>%
    str_to_title()
  
  support_services$number_num <-
    suppressWarnings(as.numeric(support_services$Response))
  
  support_services$number_num[
    is.na(support_services$number_num)
  ] <- 0
  
  support_services
  
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_support_services extracts the PCC name", {
  
  result <- f_support_services(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_support_services(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_support_services returns a data.frame", {
  
  result <- f_support_services(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_support_services(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("pivot gives correct row count", {
  
  result <- f_support_services(x)
  
  expect_equal(nrow(result), 56)
  
})

test_that("Response is character", {
  
  result <- f_support_services(x)
  
  expect_type(result$Response, "character")
  
})

test_that("spend variables classified as Number", {
  
  result <- f_support_services(x)
  
  spend_rows <- grepl("Spend|Spent", result$Variable, ignore.case = TRUE)
  
  expect_true(all(result$num_check[spend_rows] == "Number"))
  
})