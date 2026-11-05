library(testthat)


f_overview_3 <- function(x) {
  PCC <- read_xlsx(x, range = "1. Overview!D6")
  PCC_name <- colnames(PCC)
  
  
  overview_3 <- read_xlsx(x, range = "1. Overview!E23:F35") %>%
    mutate(PCC = PCC_name) %>%  # Add PCC name to each row
    filter(!is.na(.[[1]])) %>%  # Filter out rows where the first column is NA
    
    # Reshape the second column to long format
    pivot_longer(cols = 2, names_to = "Category", values_to = "Number") %>%
    
    # Add QA for the 'Number' column
    mutate(Number_QA = case_when(
      Number == "" | is.na(Number) ~ "No_number_given",
      grepl("[A-Za-z]|^[[:punct:]]|[[:punct:]]$", Number) == TRUE &
        grepl("e+", Number) == FALSE & grepl("e-", Number) == FALSE ~ "Text_given",
      TRUE ~ as.character(Number)
    ))
  
  # Convert the 'Number' column to numeric, and replace NA values with 0
  overview_3$number_num <- as.numeric(overview_3$Number)
  overview_3$number_num[is.na(overview_3$number_num)] <- 0
  
  # Return the cleaned and transformed data
  return(as.data.frame(overview_3))
}

x = './Return forms/Avon  Somerset PCC EYR 25.26 - Finalized.xlsx'

test_that("f_overview_3 extracts the PCC name", {
  
  result <- f_overview_3(x)
  
  expect_equal(unique(result$PCC), "Avon & Somerset")
  
})

test_that("Numeric column is numeric", {
  
  result <- f_overview_3(x)
  
  expect_true(is.numeric(result$number_num))
  
})

test_that("f_overview_3 returns a data.frame", {
  
  result <- f_overview_3(x)
  
  expect_s3_class(result, "data.frame")
  
})

test_that("PCC column is created", {
  
  result <- f_overview_3(x)
  
  expect_true("PCC" %in% names(result))
  
})

test_that("pivot gives correct row count", {
  
  result <- f_overview_3(x)
  
  expect_equal(nrow(result), 12)
  
})