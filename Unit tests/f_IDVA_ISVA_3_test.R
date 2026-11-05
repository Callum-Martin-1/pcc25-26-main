library(testthat)
library(tidyr)

f_IDVA_ISVA_3 <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B46:C47"
  ) %>%
    mutate(PCC = PCC_name)
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_IDVA_ISVA_3 extracts the PCC name", {
  
  result <- f_IDVA_ISVA_3(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("f_IDVA_ISVA_3 returns a data.frame", {
  
  result <- f_IDVA_ISVA_3(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_IDVA_ISVA_3(x)
  
  expect_true("PCC" %in% names(result))
  
})

