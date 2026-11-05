library(testthat)
library(tidyr)


f_provs_and_refs_WIDE <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  provs_and_refs <- read_xlsx(
    x,
    range = "3.Service Providers & Referrals!A8:T204"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    mutate(
      `Crime Category` = `Crime Category` %>%
        str_trim() %>%
        str_to_title()
    ) %>%
    mutate(
      across(c(7:13, 15:17, 19:20), as.numeric)
    )
  
  as.data.frame(provs_and_refs)
  
}


x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_provs_and_refs_WIDE extracts the PCC name", {
  
  result <- f_provs_and_refs_WIDE(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("f_provs_and_refs_WIDE returns a data.frame", {
  
  result <- f_provs_and_refs_WIDE(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_provs_and_refs_WIDE(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("rows with missing provider names are removed", {
  
  result <- f_provs_and_refs_WIDE(x)
  
  expect_false(any(is.na(result[[1]])))
  
})


