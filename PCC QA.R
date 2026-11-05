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





