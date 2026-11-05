# Set up -------------------
# Author: Anna Hollywood & Emma Chatwin + Cal Martin
# Date code last updated: 02/09/2026
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
source("DownloadPCC1.R")

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
    select(everything()) %>%
    group_by(PCC) %>%
    summarise_all(funs(sum(is.na(.))))
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


# QA ---------------------------------

# overview_1 QA ---------------------
# Check for NAs/missing data
NA_overview_1 <- overview_1 %>%
  filter(Number_QA %in% c("No_number_given",
                          "Text_given"))

# Check all funds have a record for every PCC
overview_1_QA <- overview_1 %>%
  select(-Number, -Number_QA) %>%
  pivot_wider(
    names_from = Category,
    values_from = number_num
  ) %>%
  count(Fund, PCC, name = "count") %>%
  pivot_wider(
    names_from = Fund,
    values_from = count
  ) %>%
  filter(if_any(-PCC, is.na))

# Check autosums are correct 
overview_1_QA2 <- overview_1 %>%
  select(-Number, -Number_QA) %>%
  pivot_wider(
    names_from = Category,
    values_from = number_num
  ) %>%
  rename_with(~ make.names(.x, unique = TRUE)) %>%
  mutate(
    underspend = Allocation.... -
      Year.to.date.spend.from.MoJ.grant....1.Apr.25...31.March.26
  ) %>%
  filter(underspend != 0)


# overview_2 QA ------------------------
# Check for NAs/missing data
NA_overview_2 <- overview_2 %>%
  filter(Number_QA %in% c("No_number_given",
                          "Text_given"))

# Check all crime categories have a record for every PCC
overview_2_QA <- overview_2 %>%
  select(-Number_QA, -Number) %>%
  pivot_wider(
    names_from = Category,
    values_from = number_num
  ) %>%
  count(
    `Victims’ Services Core Grant Breakdown - Crime Category`,
    PCC,
    name = "count"
  ) %>%
  pivot_wider(
    names_from = `Victims’ Services Core Grant Breakdown - Crime Category`,
    values_from = count
  ) %>%
  filter(if_any(everything(), is.na))



# overview_3 QA -------------------------
# Check for NAs/missing data
NA_overview_3 <- overview_3 %>%
  filter(Number_QA %in% c("No_number_given",
                          "Text_given"))

# Check all support types have a record for every PCC 
overview_3_QA <- overview_3 %>%
  select(-Number_QA, -Number) %>%
  pivot_wider(
    names_from = Category,
    values_from = number_num
  ) %>%
  group_by(`Type of  support/intervention/ service primarily provided`, PCC) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `Type of  support/intervention/ service primarily provided`,
    values_from = count
  ) %>%
  filter(if_any(everything(), is.na))


# overview_4 QA -------------------------
# Check for NAs/missing data
NA_overview_4 <- overview_4 %>%
  filter(Number_QA %in% c("No_number_given",
                          "Text_given"))

# Check all provider types have a record for every PCC - THEY DO
overview_4_QA <- overview_4 %>%
  select(-Number_QA, -Number) %>%
  pivot_wider(
    names_from = Category,
    values_from = number_num
  ) %>%
  count(PCC, `Types of Provider`) %>%
  pivot_wider(
    names_from = `Types of Provider`,
    values_from = n
  ) %>%
  filter(if_any(everything(), is.na))



# other_costs QA ----------------------------------
# NEED TO CARRY OUT THIS CLEANING STEP AS CAN'T DO IT IN FUNCTION DUE TO SOME PCCS HAVING NO DATA HERE
other_costs <- other_costs %>%
  rename_with(
    ~ .x %>%
      gsub("[()]", "", ., perl = TRUE) %>%
      gsub("hover for more info", "", ., fixed = TRUE) %>%
      trimws()
  )


other_costs$number_num <- sapply(other_costs[6],as.numeric)
other_costs["number_num"][is.na(other_costs["number_num"])] <- 0

# Check for NAs/missing data - NAs exist, new cleaner dataframe made below (other_costs_NEW) - amendments added to QA log
NA_other_costs <- other_costs %>%
  filter(Number_QA %in% c("No_number_given",
                          "Text_given"))
# Checking for when both to date spend and forecast spend are 0/NA
 NA_2_other_costs <- other_costs %>%
   select(-Number_QA, -Number) %>%
   spread(Category, number_num) %>%
   #select(1:5)
   mutate(errors = case_when(
#     # ERROR if spend category is missing OR if (spend amount to date is missing OR 0) AND (forecast spend is missing OR 0)
     is.na("1") |
        (is.na("5" == 0)) ~ "Error",    
       #(is.na("6" == 0)) ~ "Error",
     #(is.na("Number" == 0)) ~ "Error",
     TRUE ~ "No error"
   )) %>%
   filter(errors == "Error")

# Amendments (block out unless needed)
 other_costs_NEW <- other_costs %>%
 select(-Number_QA, -Number) %>% 
 spread(Category, number_num) %>%
 #select(1:7)

#Some PCCs have left spend/forecast spend blank but put '3 days filtering bids at £100 per day' in further info
#Have updated the spend to date to £300 in this case
mutate(errors = case_when(
 is.na("1") | #Spend Category
   #((is.na(`Year to date spend from MoJ grant (£) 1 Apr 23 - 31 Mar 24`))) ~ "Error",#
  (is.na("5")) ~ "Error",
TRUE ~ "No error"
)) %>%
#Remove records where spend category is missing OR if (spend amount to date is missing OR 0) AND (forecast spend is missing OR 0) AND where further info is not the '3 days filtering bids...'
filter(errors != "Error") %>%
select(-(errors))



# Support services QA ---------------------------
# Check for NAs/missing data - NAs exist so having a more detailed look into these in the next dataframe
 NA_support_services <- support_services %>%
   select(-c(response_QA, Response)) %>%
   pivot_wider(
     names_from = Variable,
     values_from = number_num
   ) %>%
   filter(
     if_any(
       -c(
         PCC,
         `Secondary Crime Category\r\n\r\n(hover for more info)`,
         `What type of support/intervention/service is secondarily provided?`,
         `If no, why not?`
       ),
       ~ . > 0
     )
   )

NA_support_services <- support_services %>%
  filter(response_QA %in% c("No_number_given",
                            "Text_given"))


# Providers and referrals QA -----------------------------
NA_provs_and_refs <- provs_and_refs %>%
  filter(response_QA %in% c("No_number_given",
                            "Text_given"))

# Need to flag to policy that there are some services where the total number of supported is less than the number of new supported
VAL_provs_and_refs <- provs_and_refs_WIDE %>%
  mutate(new_vs_total = ifelse(`Number of new victims supported in the reporting period*` > `Total number of supported victims (existing and new)*`, "error", "okay")) %>%
  filter(new_vs_total == "error")



# Demographics QA ---------------------------------
NA_demogs <- demogs %>%
  filter(number_qa %in% c("No_number_given",
                          "Text_given"))

# Data validation checks
VAL_demogs <- demogs %>%
  filter(grepl("total", variable)==TRUE) %>%
  select(-number, -number_qa) %>%
  group_by(`crime_category_drop_down`, pcc, variable) %>%
  summarise(Number_total = sum(number_num, na.rm = T)) %>%
  spread(variable, Number_total) %>%
  ungroup() %>%
  mutate(check = case_when(
    .[[3]] == .[[4]] & .[[3]] == .[[5]] & .[[3]] == .[[6]] & .[[3]] == .[[7]] ~ "equal",
    TRUE ~ "different"
  )) %>%
  filter(check == "different")
write.csv(VAL_demogs, "csv outputs PCC/Differences in demographic totals - TOTALS.csv", row.names = F)

# Checking how the number of referrals/supported victims compares to the demographic numbers
VAL_refs_demogs <- provs_and_refs %>%
  filter(Variable %in% c("Number of new victim referrals to support services in the reporting period*",
                         "Number of new victims supported in the reporting period*",
                         "Total number of supported victims (existing and new)*")) %>%
  group_by(PCC, `Crime Category`, Variable) %>%
  summarise(Total = sum(number_num, na.rm = T)) %>%
  left_join(demogs %>%
              filter(grepl("total", variable)==TRUE) %>%
              select(-number, -number_qa) %>%
              group_by(`crime_category_drop_down`, pcc, variable) %>%
              summarise(Number_total = sum(number_num, na.rm = T)) %>%
              spread(variable, Number_total) %>%
              ungroup(), by = c("PCC" = "pcc",
                                "Crime Category" = "crime_category_drop_down"))

VAL_refs_demogs_tots <- VAL_refs_demogs %>%
  # Using total number of supported as proxy for comparison
  filter(Variable == "Total number of supported victims (existing and new)*") %>%
  group_by(PCC) %>%
  summarise(tot_refs = sum(Total, na.rm = T),
            # Using total number by age as proxy for demogs comparison
            tot_demog_refs = sum(`Age total`, na.rm = T),
            diff = tot_refs - tot_demog_refs)



# IDVA ISVA 1 QA------------------------------
# 1 NA - will be treating it as a 0
 NA_IDVA_ISVA_1 <- IDVA_ISVA_1 %>%
   filter(`Advocates - in your PCC area` %in% c('Total number of posts funded from Core PCC grant funding',
                                                'Total number of posts funded with ISVA/IDVA Baseline Funding',
                                                'Total number of posts funded with ISVA/IDVA Additional Funding',
                                                'Total number of posts funded with DA/SV ringfenced funding'),
          Number_QA %in% c("No_number_given",
                           "Text_given"))
 caseloads <- IDVA_ISVA_1 %>%
   filter("Advocates - in your PCC area" == "Average caseload per ISVA/IDVA")


#AMENDED UP TO THIS POINT ---------------------------
 
 
# Check all funds have a record for every PCC
 IDVA_ISVA_1_QA <- IDVA_ISVA_1 %>%
   group_by(`Advocates - in your PCC area`, PCC) %>%
   summarise(count = n()) %>%
   spread(`Advocates - in your PCC area`, count) %>%
   filter_all(any_vars(is.na(.)))
 
# Checking that those with posts have given an average caseload (and vice versa) - creating table to join to table below
 IDVA_ISVA_1_QA2 <- IDVA_ISVA_1 %>%
   filter(`Advocates - in your PCC area` %in% c('Total number of posts funded from Core PCC grant funding',
                                                'Total number of posts funded with ISVA/IDVA Baseline Funding',
                                                'Total number of posts funded with ISVA/IDVA Additional Funding',
                                                'Total number of posts funded with DA/SV ringfenced funding')) %>%
   group_by(PCC, Post) %>%
   summarise(Total = sum(number_num, na.rm = T))
# Checking that those with posts have given an average caseload (and vice versa)
 IDVA_ISVA_1_QA3 <- IDVA_ISVA_1 %>%
   filter(`Advocates - in your PCC area` == "Average caseload per ISVA/IDVA") %>%
   group_by(PCC, Post) %>%
   summarise(Total = sum(number_num, na.rm = T)) %>%
   full_join(IDVA_ISVA_1_QA2, by = c("PCC" = "PCC",
                                     "Post" = "Post")) %>%
   mutate(check = case_when(
     (Total.x == 0 | is.na(Total.x)) & (Total.y == 0 | is.na(Total.y)) ~ "Fine",
     Total.x > 0 & (Total.y == 0 | is.na(Total.y)) ~ "Error",
     Total.y > 0 & (Total.x == 0 | is.na(Total.x)) ~ "Error",
     Total.y > 0 & Total.x > 0 ~ "Fine",
     TRUE ~ "Other"
   )) %>%
   filter(check == "Error")
 
 
 
# IDVA ISVA 2 QA----------------------------------------
# All NAs treated as 0s
 NA_IDVA_ISVA_2 <- IDVA_ISVA_2 %>%
   filter(Number_QA %in% c("No_number_given",
                           "Text_given"))

 
# IDVA ISVA 3 QA--------------------------------
#
 NA_IDVA_ISVA_3 <- IDVA_ISVA_3 %>%
   filter(`...1` != "Any additional comments, information you feel is relevant or would like to make us aware of") %>%
   NA_func() %>% filter_at(vars(-c(PCC, `...1`)), any_vars(.>0))
# Check all questions have a record for every PCC
 IDVA_ISVA_3_QA <- IDVA_ISVA_3 %>%
   group_by(`...1`, PCC) %>%
   summarise(count = n()) %>%
   spread(`...1`, count) %>%
  filter_all(any_vars(is.na(.)))



# Clean column names to avoid special characters or spaces issues
colnames(other_costs) <- make.names(colnames(other_costs), unique = TRUE)

# Create other_costs_T with cleaned column names
other_costs_T <- other_costs %>%
  group_by(PCC, `From.which.fund`) %>%
  summarise(`Year.to.date.spend.from.MoJ.grant...1.Apr.25...31.March.26` = sum(number_num, na.rm = TRUE)) %>%
 mutate(
    `If.co.commissioned.with.MOJ..total.spent.from.other.sources.of.funding` = 0, 
    `From.which.fund` = "Unknown - other costs") %>%
  rename(Year.to.date.spend.from.MoJ.grant......1.Apr.25...31.Mar.26.....hover.to.see.example.in.note. = `Year.to.date.spend.from.MoJ.grant...1.Apr.25...31.March.26`,
         From.which.fund......drop.down. = `From.which.fund`,
         If.co.commissioned.with.MOJ..total.spent.from.other.sources.of.funding.....hover.to.see.example.in.note. = `If.co.commissioned.with.MOJ..total.spent.from.other.sources.of.funding`)%>% 
  
  select(PCC, `From.which.fund......drop.down.`, `If.co.commissioned.with.MOJ..total.spent.from.other.sources.of.funding.....hover.to.see.example.in.note.`, `Year.to.date.spend.from.MoJ.grant......1.Apr.25...31.Mar.26.....hover.to.see.example.in.note.`) %>%
  ungroup()


colnames(overview_1) <- make.names(colnames(overview_1), unique = TRUE)
# Clean up the column names first before starting the pipeline
colnames(support_services) <- make.names(colnames(support_services), unique = TRUE)


# -----------------------------------------------------------------------------------
# Main transformation pipeline
DB_total_spend <- support_services %>%
  filter(grepl("Year to date", Variable) | grepl("total spent", Variable)) %>%
  group_by(PCC, Variable, From.which.fund......drop.down.) %>%
  summarise(spend = sum(number_num, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variable, values_from = spend) %>%
  ungroup()

# Ensure column names in the resulting DB_total_spend are unique
colnames(DB_total_spend) <- make.names(colnames(DB_total_spend), unique = TRUE)

# Continue with binding and joining operations
DB_total_spend <- DB_total_spend %>%
  bind_rows(other_costs_T) %>%
  full_join(
    overview_1 %>%
      filter(Category == "Allocation (£)") %>%
      group_by(PCC, Fund) %>%
      summarise(Total_grant_allocation = sum(number_num)),
    by = c("PCC" = "PCC", "From.which.fund......drop.down." = "Fund")
  ) %>%
  filter(From.which.fund......drop.down. != "Total") %>%
  rename(
    Total_spend_from_MoJ_funding = `Year.to.date.spend.from.MoJ.grant......1.Apr.25...31.Mar.26.....hover.to.see.example.in.note.`,
    Total_additional_cocomissioned_spend = `If.co.commissioned.with.MOJ..total.spent.from.other.sources.of.funding.....hover.to.see.example.in.note.`,
    PCC_area = PCC,
    Funding_stream = From.which.fund......drop.down.
  ) %>%
  mutate(
    Diff_allocation_and_spend = Total_grant_allocation - Total_spend_from_MoJ_funding,
    Total_spend_outside_MoJ_funding = 0,  # Empty column for dashboard table
    Year = "2025/26"
  ) %>%
  select(
    Year, PCC_area, Total_grant_allocation, Total_spend_from_MoJ_funding,
    Diff_allocation_and_spend, Total_additional_cocomissioned_spend,
    Total_spend_outside_MoJ_funding, Funding_stream
  )

# Writing the result to CSV
write.csv(DB_total_spend, "csv outputs PCC/2526 DB total spend.csv", row.names = FALSE)



# Spend source output -------------------------------------------------
# Break down of spend source by year to date spend
DB_spend_source_MOJ <- support_services %>%
  #group_by(Primary.Crime.Category.....hover.for.more.info. , PCC) %>%
  
  
  filter(grepl("Year to date", Variable)) %>%
  group_by(PCC, Primary.Crime.Category......hover.for.more.info., Variable) %>%
  summarise(spend_tot = sum(number_num, na.rm = TRUE)) %>%
  pivot_wider(names_from = Primary.Crime.Category......hover.for.more.info., values_from = spend_tot) 
  #mutate(Funding = "MoJ") %>%
  #rename("PCC NOT STATED" = `<NA>`)
  
  


# Break down of spend source by co-commissioned spend
DB_spend_source_cocomm <- support_services %>%
  filter(grepl("total spent", Variable)) %>%
  group_by(PCC, Primary.Crime.Category......hover.for.more.info., Variable) %>% 
  
  

  summarise(co_comm_tot = sum(number_num, na.rm = TRUE)) %>%
  pivot_wider(names_from = Primary.Crime.Category......hover.for.more.info., values_from = co_comm_tot)
  

# Binding the two spend source dataframes together
DB_spend_source <- rbind(DB_spend_source_MOJ, DB_spend_source_cocomm) %>%
  # Putting in blank columns for crime categories which exist in table I will be binding to in dashboard
  mutate(`Initial Referral And Support` = 0,
         `Restorative Justice` = 0,
         `Building Capacity And Capability` = 0,
         `Covid 19` = 0,
         Year = "2025/26") %>%
  rename("CSA" = `Csa/Cse`,
         "Other Services" = `Other Services For Victims Of Crime`)
         #"CCE" = Cce) %>%
  # Reordering columns so they're in same order as columns in table I am binding to in dashboard
DB_spend_source[is.na(DB_spend_source)] <- 0
write.csv(DB_spend_source, "csv outputs PCC/2526 spend source.csv", row.names = F)

# Including the other costs in the cost category breakdown
other_costs_temp <- other_costs %>%
  group_by(PCC) %>%
  summarise(Value = sum(number_num, na.rm = TRUE)) %>%
  mutate(Year = "2025/26",
         Funding_source = "MoJ",
         Crime_type = "Unknown",
         Funding_stream = "Unknown - other costs") %>%
  rename("PCC_area" = PCC) %>%
  select(Year, PCC_area, Funding_source, Crime_type, Value, Funding_stream)

 #To join to previous dashboard inputs - total spend by crime category and funding source

overview_3 <- overview_3 %>%
  filter(!grepl("TOTAL", `Type of  support/intervention/ service primarily provided`))

write.csv(overview_3, "csv outputs PCC/2526 DB support services.csv", row.names = FALSE)

overview_4 <- overview_4 %>%
  filter(!grepl("TOTAL", `Types of Provider`))

write.csv(overview_4, "csv outputs PCC/2526 DB provider type.csv", row.names = FALSE)

DB_spend_source <- support_services %>%
  filter(str_detect(Variable, "Year to date|total spent")) %>%
  group_by(
    PCC,
    Primary.Crime.Category......hover.for.more.info.,
    Variable,
    From.which.fund......drop.down.
  ) %>%
  summarise(spend = sum(number_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = Primary.Crime.Category......hover.for.more.info.,
    values_from = spend,
    values_fill = 0
  ) %>%
  mutate(
    Funding_source = case_when(
      str_detect(Variable, "Year to date") ~ "MoJ",
      str_detect(Variable, "total spent")  ~ "Co-commissioned",
      TRUE                                 ~ "Other"
    ),
    Year = "2025/26"
  ) %>%
  select(-Variable) %>%
  rename(
    PCC_area        = PCC,
    CSA             = `Csa/Cse`,
    `Other Services`= `Other Services For Victims Of Crime`,
    Funding_stream  = From.which.fund......drop.down.
  ) %>%
  pivot_longer(
    cols = -c(Year, PCC_area, Funding_source, Funding_stream),
    names_to  = "Crime_type",
    values_to = "Value"
  ) %>%
  select(Year, PCC_area, Funding_source, Crime_type, Value, Funding_stream)





write.csv(DB_spend_source, "csv outputs PCC/2526 DB spend source.csv", row.names = FALSE)  

# New groupings of data that aren't currently set up in dashboard  -------------------------

# Clean ISVA and IDVA data -----------------
# POSTS - ALL
ISVA_IDVA_posts_all <- IDVA_ISVA_g %>%
  # Filtering out totals so these aren't double counted when pulled through into PowerBI
  filter(`Advocates - in your PCC area` %in% c("Of which are mainstream/not exclusively by and for posts",
                                               "Of which are by and for posts",
                                               "Of which are specialist/tailored posts",
                                               "No. of CYP posts")) %>%
  select(-Measure)

write.csv(ISVA_IDVA_posts_all, "csv outputs PCC/2526 ISVA IDVA posts by fund and type.csv", row.names = F)
write.csv(IDVA_ISVA_1, "csv outputs PCC/2526 ISVA IDVA posts by fund and typeALL.csv", row.names = F)

# AD-HOC - ALL
IDVA_ISVA_adhoc <- IDVA_ISVA_1 %>%
  filter(`Advocates - in your PCC area` %in% c("How many posts have been recruited since the last report?",
                                               "a.How many victims have been supported by an advocate whilst also engaging with the CJS?",
                                               "b. Please provide the number of victims where the information relating to engagement with CJS is unknown",
                                               "a. How many victims/witnesses have withdrawn from the CJS despite having support from an ISVA/IDVA?",
                                               "b. Please provide the number of victims where this information is unknown (those being supported by ISVA/IDVA)")) %>%
  mutate(group = case_when(
    `Advocates - in your PCC area` == "How many posts have been recruited since the last report?" ~ "Posts recruited since last report",
    `Advocates - in your PCC area` == "a.How many victims have been supported by an advocate whilst also engaging with the CJS?" ~ "Supported whilst engaging with CJS",
    `Advocates - in your PCC area` == "b. Please provide the number of victims where the information relating to engagement with CJS is unknown" ~ "Engagement with CJS UNKNOWN",
    `Advocates - in your PCC area` == "a. How many victims/witnesses have withdrawn from the CJS despite having support from an ISVA/IDVA?" ~ "Withdrawn from CJS despite support",
    `Advocates - in your PCC area` == "b. Please provide the number of victims where this information is unknown (those being supported by ISVA/IDVA)" ~ "Withdrawn from CJS despite support UNKNOWN",
    TRUE ~ "Error"
  )) %>%
  select(-`Advocates - in your PCC area`)

#write.csv(IDVA_ISVA_adhoc, "csv outputs PCC/Adhoc ISVA IDVA data collected.csv", row.names = F)

# LENGTH AND CASELOADS - IDVA
time_caseload <- IDVA_ISVA_1 %>%
  filter(`Advocates - in your PCC area` %in% c("How long on average did the victim stay with the service provider?",
                                               "How long on average did the victim stay engaged with the CJS?",
                                               "Average caseload per ISVA/IDVA"))

write.csv(time_caseload, "csv outputs PCC/2526 ISVA IDVA average stay and caseloads.csv", row.names = F)

# QUALITATIVE QUESTIONS - ALL
IDVA_ISVA_Qs <- IDVA_ISVA_3 %>%
  spread(`...1`, `Free form answers below`)

# Support services ----------------
write.csv(support_services, "csv outputs PCC/All support service data.csv", row.names=F) #print(colnames(DB_total_spend))

# Other costs ---------------
write.csv(other_costs, "csv outputs PCC/All other costs CLEANED.csv", row.names = F)


# Demographics output ------------------------------------------------------------------

# for demographics data
 DB_demogs <- demogs %>%
#   # Cleaning/shortening some crime category and variable names
   mutate(Crime_type = case_when(
     `crime_category_drop_down` == "Csa/Cse" ~ "CSA",
     `crime_category_drop_down` == "Cce" ~ "Child Criminal Exploitation",
     `crime_category_drop_down` == "Other Services For Victims Of Crime" ~ "Other Services",
     TRUE ~ `crime_category_drop_down`
   ),
   Variable_2 = case_when(
     variable == "12 years and under" ~ "12 and under",
     variable == "13 to 17 years" ~ "13-17",
     variable == "18 to 24 years" ~ "18-24",
     variable == "25 to 34 years" ~ "25-34",
     variable == "35 to 44 years" ~ "35-44",
     variable == "45 to 54 years" ~ "45-54",
     variable == "55 to 64 years" ~ "55-64",
     variable == "65 to 74 years" ~ "65-74",
     variable == "75 years or over" ~ "75 and over",
     variable == "Black / African / Caribbean / Black British" ~ "Black",
     variable == "Woman" ~ "Female",
     variable == "Man" ~ "Male",
     grepl("not stated", variable) ~ "Not stated",
     grepl("total", variable) ~ "Total",
     TRUE ~ variable
   )) %>%
   filter(Variable_2 != "Total") %>%
   rename("Type" = variable,
          "Category" = Variable_2,
          "Value" = number_num) %>%
   select(pcc, Type, Category, Crime_type, Value)

# Demographics data as is structured in the 2021/22 data
demogs_ALL_t <- demogs %>%
  mutate(crime_category = case_when(
    # Some discrepancies in the way people were reporting under certain crime types, so have grouped them here (mainly formatting/spelling errors)
    `crime_category_drop_down` %in% c("Cyber crime", "Cyber Crime") ~ "Cyber Crime",
    `crime_category_drop_down` %in% c("Road Crime", "Road Crime ") ~ "Road Crime",
    `crime_category_drop_down` %in% c("Sexual Violence", "Sexul Violence") ~ "Sexual Violence",
    `crime_category_drop_down` %in% c("Stalking & Harrassment Total", "Stalking and harassment") ~ "Stalking And Harassment",
    `crime_category_drop_down` %in% c("Other Services For Victims Of Crime", "Other Victims Of Crime") ~ "Other Services",
    `crime_category_drop_down` %in% c("Theft", "Theft Total") ~ "Theft",
    `crime_category_drop_down` == "Csa/Cse" ~ "CSA/CSE",
    `crime_category_drop_down` == "Cce" ~ "CCE",
    TRUE ~ `crime_category_drop_down`
  )) %>%
  select(-`crime_category_drop_down`) %>%
  # Filtering out auto-summed totals to avoid duplication at a later stage
  filter(grepl("total", variable) == FALSE) %>%
  mutate(Category = case_when(
    variable %in% c('Man','Woman','Non-Binary','Gender not stated') ~ "Gender",
    grepl("\\d", variable) == TRUE | variable == "Age not stated" ~ "Age",
    variable %in% c('Heterosexual / Straight', 'Gay / Lesbian',
                    'Bisexual', 'Other', 'Sexual orientation not stated') ~ "Sexual Orientation",
    variable %in% c('White', 'Mixed / Multiple Ethnic Groups',
                    'Asian / Asian British', 'Black / African / Caribbean / Black British',
                    'Other Ethnic Group', 'Ethnicity not stated') ~ "Ethnicity",
    grepl("disability", variable, ignore.case = TRUE) ~ "Disability",
    TRUE ~ "Other"
  )) %>%
  # Putting in blank columns for crime categories which exist in table I will be binding to in dashboard
  mutate(Year = "EY 2025/26")




demogs_max_t <- demogs_ALL_t %>%
  group_by(crime_category, Category, pcc, Year) %>%
  summarise(Total = sum(number_num)) %>%
  spread(Category, Total)
demogs_max_t$max <- pmax(demogs_max_t$Age, demogs_max_t$Disability, demogs_max_t$Gender, demogs_max_t$Ethnicity, demogs_max_t$`Sexual Orientation`)
demogs_max <- demogs_max_t %>%
  mutate("Unknown_age" = max - Age,
         "Unknown_gender" = max - Gender,
         "Unknown_disability" = max - Disability,
         "Unknown_ethnicity" = max - Ethnicity,
         "Unknown_sexor" = max - `Sexual Orientation`) %>%
  select(-c(4:9)) %>%
  rename("Age" = Unknown_age,
         "Gender" = Unknown_gender,
         "Disability" = Unknown_disability,
         "Ethnicity" = Unknown_ethnicity,
         "Sexual Orientation" = Unknown_sexor) %>%
  gather("Category", "number_num", 4:8) %>%
  mutate(variable = "Unknown",
         Number = number_num,
         Number_QA = number_num) %>%
  select(pcc, variable, Number, Number_QA, number_num, crime_category, Category, Year)

demogs_max <- as.data.frame(demogs_max)

demogs_ALL <- bind_rows(as_tibble(demogs_ALL_t), as_tibble(demogs_max))

write.csv(demogs_ALL, "csv outputs PCC/2526 demographics.csv", row.names = F) 


# Full support services output -----------------------------------------------

# Full support services dataframe to add into dashboard
support_services_clean <- support_services %>%
  # Cleaning/shortening some crime category names
  mutate(Prim_crim_cat_clean = case_when(
    `Primary.Crime.Category......hover.for.more.info.` == "Csa/Cse" ~ "CSA",
    `Primary.Crime.Category......hover.for.more.info.` == "Cce" ~ "CCE",
    TRUE ~ `Primary.Crime.Category......hover.for.more.info.`))

#write.csv(support_services_clean, "csv outputs PCC/2526 support services.csv", row.names = FALSE)


# Full providers and referrals output ------------------------------------------

# Full providers and referrals dataframe to add into dashboard
provs_and_refs_clean <- provs_and_refs %>%
  # Creating short/clean versions of variable names for dashboard
  mutate(Referral_metric = case_when(
    grepl("Number of new victim referrals", Variable) == TRUE ~ "New referrals",
    grepl("Number of new victims supported", Variable) == TRUE ~ "New supported",
    Variable == "Total number of supported victims (existing and new)" ~ "Total supported",
    grepl("Number of victims who have exited", Variable) == TRUE ~ "Exited support",
    grepl("referred to other services", Variable) == TRUE ~ "Referred other services",
    grepl("police prior to engaging", Variable) == TRUE ~ "Reported police prior to engagement",
    grepl("police throughout/after", Variable) == TRUE ~ "Reported police during/after engagement",
    grepl("able to cope and build resilience", Variable) == TRUE ~ "Better able to cope and build resilience",
    grepl("measure 'cope and build", Variable) == TRUE ~ "Outcome measurement tool",
    grepl("referred for mental health", Variable) == TRUE ~ "Referred MH support",
    grepl("average number of people on the waiting", Variable) == TRUE ~ "Avg no waitlist",
    grepl("average waiting period", Variable) == TRUE ~ "Avg waiting period",
    grepl("type of waiting list", Variable) == TRUE ~ "Waitlist type",
    grepl("Eligible: No Contact", Variable) == TRUE ~ "Eligible no contact",
    grepl("Eligible: Disengaged", Variable) == TRUE ~ "Eligible disengaged",
    grepl("Additional supporting comments", Variable) == TRUE ~ "Add supp comments",
    TRUE ~ Variable
  ),
  # Cleaning/shortening some crime category names
  Crim_cat_clean = case_when(
    `Crime Category` == "Csa/Cse" ~ "CSA",
    `Crime Category` == "Cce" ~ "CCE",
    `Crime Category` == "Other Services For Victims Of Crime" ~ "Other Services",
    TRUE ~ `Crime Category`
    )
  ) 

write.csv(provs_and_refs_clean, "csv outputs PCC/2526 provs and refs.csv", row.names = FALSE)  


wipe_excel_files <- function(folder_path) {
  # List all .xlsx and .xls files in the folder
  excel_files <- list.files(path = folder_path, pattern = "\\.xlsx$|\\.xls$", full.names = TRUE)
  
  # Check if there are any Excel files in the folder
  if (length(excel_files) == 0) {
    message("No Excel files found in the folder.")
  } else {
    # Delete the files
    file.remove(excel_files)
    message(paste(length(excel_files), "Excel files have been deleted."))
  }
}



folder <- "~/pcc24-25-main/Return forms"
wipe_excel_files(folder)


