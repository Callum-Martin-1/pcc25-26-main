# Set up -------------------
# Author: Cal Martin
# Date code last updated: 20/10/2026
# Date data was last updated i.e. downloaded from policy team's Sharepoint (AWS Bucket): 02/09/2026

# Load in required libraries
install.packages("renv")
library(renv)
library(Rs3tools)
library(tidyverse)
library(readxl)
library(validate)
library(misty)
library(lubridate)
library(stringr)
library(reticulate)
library(dplyr)
library(janitor)

PCC_list <- NULL
# Creating PCC list to run through the for loops
PCC_list <- list.files(path = "./Return forms", pattern = '*.xlsx', full.names = TRUE)


# Functions --------------------------------------------

# Read overview of spend by funding source
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



f_overview_2 <- function(x) {
  
  PCC <- read_xlsx(x, range = "1. Overview!D6")
  PCC_name <- colnames(PCC)
  
  overview_2 <- read_xlsx(x, range = "1. Overview!B23:C42") %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.data[["Victims’ Services Core Grant Breakdown - Crime Category"]])) %>%
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
      number_num = replace_na(number_num, 0)
    )
  
  as.data.frame(overview_2)
  
}



# Read breakdown of core grant spend by support type
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



# Read breakdown of core grant fund by provider type
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



# Read breakdown of core grant fund other costs
f_other_costs <- function(x) {
  
  PCC_name <- colnames(
    read_xlsx(x, range = "1. Overview!D6")
  )
  
  other_costs <- read_xlsx(
    x,
    sheet = 3,
    range = "A10:D50"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[1]])) %>%
    pivot_longer(
      cols = 4,
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
      )
    )
  
  as.data.frame(other_costs)
  
}



# Read breakdown of support services
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


# Read breakdown of referral numbers by support services
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



# Read breakdown of crime category by demographic
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



# Read IDVA ISVA stats
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



# Read IDVA ISVA stats
f_IDVA_ISVA_1 <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B8:E36"
  ) %>%
    mutate(PCC = PCC_name) %>%
    filter(!is.na(.[[2]])) %>%
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



# Read IDVA ISVA by and for specific stats
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



# Read qualitative IDVA ISVA question responses
f_IDVA_ISVA_3 <- function(x) {
  
  PCC_name <- read_xlsx(x, range = "1. Overview!D6") %>%
    colnames()
  
  read_xlsx(
    x,
    range = "5. IDVA ISVA Statistics!B46:C47"
  ) %>%
    mutate(PCC = PCC_name)
}


# ---- Partnership working extractor ----------------------------------
f_partnership_working <- function(x, sheet = "6. Additional Qs") {
  
  stopifnot(file.exists(x))
  
  PCC_name <- readxl::read_xlsx(
    x,
    range = "1. Overview!D6"
  ) %>%
    colnames()
  
  raw <- suppressMessages(
    readxl::read_excel(
      x,
      sheet = sheet,
      col_names = FALSE
    )
  )
  
  chr <- raw %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      dplyr::across(
        everything(),
        ~ {
          x <- as.character(.x)
          x[is.na(x)] <- ""
          stringr::str_squish(trimws(x))
        }
      )
    )
  
  n_cols <- ncol(chr)
  n_rows <- nrow(chr)
  
  `%||%` <- function(a, b) {
    if (is.null(a) || is.na(a) || !nzchar(a)) b else a
  }
  
  top <- chr[seq_len(min(n_rows, 25)), , drop = FALSE]
  
  find_col <- function(pattern) {
    cols <- which(
      vapply(
        top,
        function(col) {
          any(
            stringr::str_detect(
              col,
              stringr::regex(pattern, ignore_case = TRUE)
            )
          )
        },
        logical(1)
      )
    )
    
    if (length(cols)) cols[1] else NA_integer_
  }
  
  find_row <- function(pattern) {
    rows <- which(
      apply(
        chr,
        1,
        function(row) {
          any(
            stringr::str_detect(
              row,
              stringr::regex(pattern, ignore_case = TRUE)
            )
          )
        }
      )
    )
    
    if (length(rows)) rows[1] else NA_integer_
  }
  
  block_defs <- c(
    "1. Local Authorities" =
      "^\\s*1\\.?\\s*Local\\s*Authorities",
    "2. Integrated Care System(s)" =
      "^\\s*2\\.?\\s*Integrated\\s*Care\\s*System",
    "3. Local Sexual Assault Referral Centres" =
      "^\\s*3\\.?\\s*Local\\s*Sexual\\s*Assault\\s*Referral\\s*Centres?",
    "4. Other" =
      "^\\s*4\\.?\\s*Other"
  )
  
  block_cols <- purrr::map_int(
    block_defs,
    find_col
  )
  
  block_cols <- block_cols[!is.na(block_cols)]
  
  if (!length(block_cols)) {
    stop(
      "Couldn't find partner block headers on '",
      sheet,
      "'.",
      call. = FALSE
    )
  }
  
  row_improved <- find_row(
    "Has this partnership\\s+improved\\s+your ability to support victims"
  )
  
  row_reasons <- find_row(
    "^\\s*Please\\s+give\\s+reasons\\s+for\\s+how\\s+this\\s+partnership\\s+has\\s+and/or\\s+hast\\s+not\\s+improved\\s+your\\s+ability\\s+to\\s+support\\s+victims."
  )
  
  row_if_no <- find_row(
    "^\\s*If\\s*no,\\s*please\\s*give\\s*a\\s*reason"
  )
  
  if (is.na(row_improved) || is.na(row_reasons)) {
    stop(
      "Couldn't find key anchors ('improved?' or 'Reasons for good partnership').",
      call. = FALSE
    )
  }
  
  first_block_col <- min(block_cols)
  reason_label_col <- max(1, first_block_col - 1)
  
  candidate_rows <- seq(
    row_reasons + 1,
    min(n_rows, row_reasons + 12)
  )
  
  labels_raw <- chr[candidate_rows, reason_label_col]
  
  nz <- which(nzchar(labels_raw))
  
  if (length(nz)) {
    last_non_empty <- max(nz)
    
    reason_rows <- candidate_rows[min(nz):last_non_empty]
    labels <- labels_raw[min(nz):last_non_empty]
    
  } else {
    
    reason_rows <- integer(0)
    labels <- character(0)
  }
  
  extract_block <- function(j, block_name) {
    
    j <- as.integer(j)
    
    if (is.na(j)) {
      stop(
        "Block column is not numeric for '",
        block_name,
        "'."
      )
    }
    
    j1 <- min(n_cols, j + 1)
    j2 <- min(n_cols, j + 2)
    
    improved_val <- chr[
      (row_improved):(min(n_rows, row_improved + 6)),
      j
    ]
    
    improved_val <- improved_val[nzchar(improved_val)]
    
    improved_val <- if (length(improved_val)) {
      improved_val[1]
    } else {
      NA_character_
    }
    
    improved <- dplyr::case_when(
      stringr::str_detect(
        improved_val %||% "",
        stringr::regex("^yes$", TRUE)
      ) ~ TRUE,
      
      stringr::str_detect(
        improved_val %||% "",
        stringr::regex("^no$", TRUE)
      ) ~ FALSE,
      
      TRUE ~ NA
    )
    
    reasons_selected <- NA_character_
    other_reason_text <- NA_character_
    
    if (length(reason_rows)) {
      
      marks <- chr[reason_rows, j1]
      
      picked <- labels[
        stringr::str_detect(
          marks,
          stringr::regex("\\bX\\b", TRUE)
        )
      ]
      
      reasons_selected <- if (length(picked)) {
        paste(picked, collapse = "; ")
      } else {
        NA_character_
      }
      
      other_idx <- which(
        stringr::str_detect(
          labels,
          stringr::regex("^Other", TRUE)
        )
      )[1]
      
      if (!is.na(other_idx)) {
        
        txt <- chr[reason_rows[other_idx], j2]
        
        if (nzchar(txt) &&
            !stringr::str_to_upper(txt) %in% "X") {
          
          other_reason_text <- txt
        }
      }
    }
    
    if_no_reason_text <- NA_character_
    
    if (!is.na(row_if_no)) {
      
      vals <- chr[
        (row_if_no + 1):(min(n_rows, row_if_no + 3)),
        j
      ]
      
      vals <- vals[nzchar(vals)]
      
      if (length(vals)) {
        if_no_reason_text <- vals[1]
      }
    }
    
    tibble::tibble(
      PCC = PCC_name,
      partner_type = block_name,
      improved = improved,
      reasons_selected = reasons_selected,
      other_reason_text = other_reason_text,
      if_no_reason_text = if_no_reason_text
    )
  }
  
  purrr::imap_dfr(
    block_cols,
    extract_block
  )
}       



# Check for NAs function
NA_func <- function(x) {
  x %>%
    group_by(PCC) %>%
    summarise(
      across(
        everything(),
        ~ sum(is.na(.)),
        .names = "{.col}"
      ),
      .groups = "drop"
    )
}


# For loops --------------------------------------------------

# Making sure all the dataframes are empty before we run the for loop
overview_1 <- NULL
overview_2 <- NULL
overview_3 <- NULL
overview_4 <- NULL
other_costs <- NULL
support_services <- NULL
provs_and_refs <- NULL
provs_and_refs_WIDE <- NULL
demogs <- NULL
IDVA_ISVA_1 <- NULL
IDVA_ISVA_2 <- NULL
IDVA_ISVA_3 <- NULL
IDVA_ISVA_4 <- NULL
IDVA_ISVA_g <- NULL
add_info <- NULL
i <- NULL

# Running the above functions over all PCCs
# This for loop will bind all PCC data into their discrete tables
for (i in PCC_list) {
  
  a <- f_overview_1(i)
  overview_1 <- rbind(overview_1, a)
  
  a <- f_overview_2(i)
  overview_2 <- rbind(overview_2, a)
  
  a <- f_overview_3(i)
  overview_3 <- rbind(overview_3, a)
  
  a <- f_overview_4(i)
  overview_4 <- rbind(overview_4, a)
  
  a <- f_other_costs(i)
  other_costs <- rbind(other_costs, a)
  
  a <- f_support_services(i)
  support_services <- rbind(support_services, a)
  
  a <- f_provs_and_refs(i)
  provs_and_refs <- rbind(provs_and_refs, a)
  
  a <- f_provs_and_refs_WIDE(i)
  provs_and_refs_WIDE <- rbind(provs_and_refs_WIDE, a)
  
  a <- f_demogs(i)
  demogs <- rbind(demogs, a)
  
  a <- f_IDVA_ISVA_NEW_1(i)
  IDVA_ISVA_g <- rbind(IDVA_ISVA_g, a)
  
  a <- f_IDVA_ISVA_1(i)
  IDVA_ISVA_1 <- rbind(IDVA_ISVA_1, a)
  
  a <- f_IDVA_ISVA_2(i)
  IDVA_ISVA_2 <- rbind(IDVA_ISVA_2, a)
  
  a <- f_IDVA_ISVA_3(i)
  IDVA_ISVA_3 <- rbind(IDVA_ISVA_3, a)
  
  a <- f_partnership_working(i)
  add_info <- rbind(add_info, a)
  
}