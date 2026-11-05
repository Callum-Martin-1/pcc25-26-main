library(testthat)
library(tidyr)



f_demogs <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  demogs <- read_xlsx(
    x,
    range = "4.Service Providers&Demographic!A8:AE204"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    rename(
      "Gender not stated" = `Not stated...5`,
      "Gender total" = `Total...6`,
      "Age not stated" = `Not stated...13`,
      "Age total" = `Total...14`,
      "Sexual orientation not stated" = `Not stated...19`,
      "Sexual orientation total" = `Total...20`,
      "Ethnicity not stated" = `Not stated...26`,
      "Ethnicity total" = `Total...27`,
      "Disability not stated" = `Not stated...30`,
      "Disability total" = `Total...31`
    ) %>%
    mutate(across(2:31, ~ str_replace_all(as.character(.), "^-$", "0"))) %>%
    pivot_longer(
      cols = 2:31,
      names_to = "Variable",
      values_to = "Number"
    ) %>%
    mutate(
      Number_QA = case_when(
        Number == "" | is.na(Number) ~ "No_number_given",
        grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) &
          !grepl("e\\+", Number) &
          !grepl("e\\-", Number) ~ "Text_given",
        TRUE ~ as.character(Number)
      )
    ) %>%
    clean_names() %>%
    mutate(
      crime_category_drop_down = str_to_title(
        str_trim(crime_category_drop_down)
      ),
      number_num = suppressWarnings(as.numeric(number)),
      number_num = tidyr::replace_na(number_num, 0)
    )
  
  demogs
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_demogs extracts the PCC name", {
  
  result <- f_demogs(x)
  
  expect_equal(unique(result$pcc), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_demogs(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_demogs returns a data.frame", {
  
  result <- f_demogs(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_demogs(x)
  
  expect_true("pcc" %in% names(result))
  
})

test_that("rows with missing provider names are removed", {
  
  result <- f_demogs(x)
  
  expect_false(any(is.na(result[[1]])))
  
})

