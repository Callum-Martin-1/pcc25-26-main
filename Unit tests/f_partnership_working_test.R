library(testthat)
library(tidyr)
library(purrr)
library(readxl)

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
