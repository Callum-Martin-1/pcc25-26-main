library(testthat)
library(tidyr)

f_IDVA_ISVA_NEW_1 <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  IDVA_ISVA_a <- read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B8:E14"
  ) %>%
    mutate(
      PCC = PCC_name,
      Funds = "Core"
    ) %>%
    filter(!is.na(.[[2]]))
  
  IDVA_ISVA_b <- read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B14:E19"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[2]])) %>%
    rename(
      "Advocates - in your PCC area" = 1,
      Measure = 2,
      ISVAs = 3,
      IDVAs = 4
    ) %>%
    mutate(Funds = "DA/SV ringfenced")
  
  IDVA_ISVA_c <- read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B19:E24"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[2]])) %>%
    rename(
      "Advocates - in your PCC area" = 1,
      Measure = 2,
      ISVAs = 3,
      IDVAs = 4
    ) %>%
    mutate(Funds = "SV test funding")
  
  bind_rows(
    IDVA_ISVA_a,
    IDVA_ISVA_b,
    IDVA_ISVA_c
  ) %>%
    pivot_longer(
      cols = c(ISVAs, IDVAs),
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

test_that("f_IDVA_ISVA_NEW_1 extracts the PCC name", {
  
  result <- f_IDVA_ISVA_NEW_1(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("f_IDVA_ISVA_NEW_1 returns a data.frame", {
  
  result <- f_IDVA_ISVA_NEW_1(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_IDVA_ISVA_NEW_1(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("Funds column exists", {
  
  result <- f_IDVA_ISVA_NEW_1(x)
  
  expect_true("Funds" %in% names(result))
  
})

test_that("fund types assigned correctly", {
  
  result <- f_IDVA_ISVA_NEW_1(x)
  
  expect_setequal(unique(result$Funds), 
                  c(
                    "Core",
                    "DA/SV ringfenced",
                    "SV test funding"
                  ))
  
})