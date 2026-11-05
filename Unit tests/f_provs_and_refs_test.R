library(testthat)
library(tidyr)


f_provs_and_refs <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  provs_and_refs <- read_xlsx(
    x,
    range = "3.Service Providers & Referrals!A8:V204"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    pivot_longer(
      cols = 7:22,
      names_to = "Variable",
      values_to = "Response",
      values_transform = list(Response = as.character)
    ) %>%
    mutate(
      Response = as.character(Response),
      num_check = case_when(
        grepl("Number|How many|Eligible", Variable, ignore.case = TRUE) ~ "Number",
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
    ) %>%
    mutate(
      `Crime Category` = `Crime Category` %>%
        str_trim() %>%
        str_to_title()
    )
  
  provs_and_refs <- as.data.frame(provs_and_refs)
  
  provs_and_refs$number_num <-
    suppressWarnings(as.numeric(provs_and_refs$Response))
  
  provs_and_refs$number_num[
    is.na(provs_and_refs$number_num)
  ] <- 0
  
  provs_and_refs
  
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_provs_and_refs extracts the PCC name", {
  
  result <- f_provs_and_refs(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_provs_and_refs(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_provs_and_refs returns a data.frame", {
  
  result <- f_provs_and_refs(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_provs_and_refs(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("rows with missing provider names are removed", {
  
  result <- f_provs_and_refs(x)
  
  expect_false(any(is.na(result[[1]])))
  
})

test_that("Response is character", {
  
  result <- f_provs_and_refs(x)
  
  expect_type(result$Response, "character")
  
})

test_that("number fields correctly identified", {
  
  result <- f_provs_and_refs(x)
  
  rows <- grepl("Number|How many|Eligible", result$Variable, ignore.case = TRUE)
  
  expect_true(all(result$num_check[rows] == "Number"))
  
})