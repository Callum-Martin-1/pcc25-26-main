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



folder <- "~/pcc25-26-main/Return forms"
wipe_excel_files(folder)