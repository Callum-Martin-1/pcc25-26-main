library(testthat)
library(tidyr)

f_IDVA_ISVA_2 <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B38:E44"
  ) %>%
    mutate(PCC = PCC_name) %>%
    pivot_longer(
      cols = 3:4,
      names_to = "Post",
      values_to = "Number",
      values_transform = list(Number = as.character)
    ) %>%
    mutate(
      Number_QA = case_when(
        Number == "" | is.na(Number) ~ "No_number_given",
        
        grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) &
          !grepl("e\\+", Number) &
          !grepl("e\\-", Number) ~ "Text_given",
        
        TRUE ~ Number
      ),
      number_num = suppressWarnings(as.numeric(Number)),
      number_num = tidyr::replace_na(number_num, 0)
    )
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_IDVA_ISVA_2 extracts the PCC name", {
  
  result <- f_IDVA_ISVA_2(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("f_IDVA_ISVA_2 returns a data.frame", {
  
  result <- f_IDVA_ISVA_2(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_IDVA_ISVA_2(x)
  
  expect_true("PCC" %in% names(result))
  
})

