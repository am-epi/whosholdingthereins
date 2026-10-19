####################
## Load libraries ##
####################

library(ggplot2)   
library(dplyr)     
library(readxl)    
library(tidyr)
library(writexl)
library(stringr)
library(openxlsx)
library(likert)
library(psych)

##################
## Load in data ##
##################

# By worksheet name
datawhole <- read_excel("whosholdingthereins - datasets.xlsx", sheet = "Opinions on testing")

colnames(datawhole)

###################################
## clean df of open text answers ##
###################################

data <- datawhole %>% 
        select(EIDSID, new_arrival_one_off_test, paired_samples_quarantine, clearance_green_amber, testing_clinically_suspected, provides_useful_information)

colnames(data)

###############################
## general desc answer count ##
###############################

## NEW ARRIVIAL TESTING ##

newarrival_count <- data %>%
  #filter(!is.na(xxxxx)) %>%
  group_by(new_arrival_one_off_test) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))%>%
  select(new_arrival_one_off_test, n, percent, CI)

## PAIRED QUARANTINE ## 

pairedquarantine_count <- data %>%
  #filter(!is.na(xxxxx)) %>%
  group_by(paired_samples_quarantine) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)
  ) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))%>%
  select(paired_samples_quarantine, n, percent, CI)

## GREEN/AMBER CLEARANCE ##

greenamber_count <- data %>%
  #filter(!is.na(xxxxx)) %>%
  group_by(clearance_green_amber) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))%>%
  select(clearance_green_amber, n, percent, CI)

## CLINICAL DIAGNOSTICS ##

clinicaldx_count <- data %>%
  #filter(!is.na(xxxxx)) %>%
  group_by(testing_clinically_suspected) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))%>%
  select(testing_clinically_suspected, n, percent, CI)

## USEFULNESS ##

usefulness_count <- data %>%
  #filter(!is.na(xxxxx)) %>%
  group_by(provides_useful_information) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(totalreported = sum(n),
         percent = round(n / totalreported * 100, 1)) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(n, totalreported)$conf.int * 100),
         ci_lower = round(ci[[1]], 1),
         ci_upper = round(ci[[2]], 1)) %>%
  select(-ci) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(percent = paste0(percent, "%"),
         CI = paste0(ci_lower, " - ", ci_upper))%>%
  select(provides_useful_information, n, percent, CI)

#############################################
## SAVE AS EXCEL DOC WITH TABS FOR EACH DF ##
#############################################

# Create a new workbook
wb <- createWorkbook()

# Add sheets to the workbook
addWorksheet(wb, "newarrival_count")
addWorksheet(wb, "pairedquarantine_count")
addWorksheet(wb, "greenamber_count")
addWorksheet(wb, "clinicaldx_count")
addWorksheet(wb, "usefulness_count")

# Write the data frames to the corresponding sheets
writeData(wb, sheet = "newarrival_count", newarrival_count)
writeData(wb, sheet = "pairedquarantine_count", pairedquarantine_count)
writeData(wb, sheet = "greenamber_count", greenamber_count)
writeData(wb, sheet = "clinicaldx_count", clinicaldx_count)
writeData(wb, sheet = "usefulness_count", usefulness_count)
# Save the workbook to a file

saveWorkbook(wb, "serology testing opinions counts.xlsx", overwrite = TRUE)

###-------------------------------------------####
# Since the input data is in a tibble with all responses as character vectors, you need to:
# 1) Convert relevant columns to ordered factors based on their appropriate Likert scale,
# 2) Separate the questions by scale (because likert::likert() requires that),
# 3) Then use likert() and plot() to visualize.
###-------------------------------------------####

##############################
## Define the Likert scales ##
##############################

#Plots from likert(), assume that responses move from low → high

usefulness_levels <- c("Not at all useful", "Slightly useful", "Moderately useful", "Very useful", "Extremely useful")

agreement_levels <- c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", 
                      "Somewhat agree", "Strongly agree")

# define the factor levels in low-to-high order, so that R assigns:

#Internally: 1 < 2 < 3 < 4 < 5
#Meaningfully: "Not at all useful" < ... < "Extremely useful"
#This ensures ordered factors work correctly with both likert() and any ordinal stats you might run later.

#############################################
## Convert to ordered factors for analysis ##
#############################################

# usefulness questions
usefulness_questions <- data %>%
  mutate(across(c(new_arrival_one_off_test, paired_samples_quarantine, clearance_green_amber, testing_clinically_suspected),
                ~ factor(., levels = usefulness_levels, ordered = TRUE))) %>% 
  select(EIDSID,new_arrival_one_off_test, paired_samples_quarantine, clearance_green_amber, testing_clinically_suspected)

#agreement question
agreement_question <- data %>%
  mutate(provides_useful_information = factor(provides_useful_information,
                                              levels = agreement_levels,
                                              ordered = TRUE)) %>% 
  select(EIDSID, provides_useful_information)

##Answers still look like text but 'in the back end' they are numerical.
# If you want to check do the following code for each variable:
# as.numeric(usefulness_questions$paired_samples_quarantine)

#######################################
## MAKE SINGLE DFs FOR EACH ANALYSIS ##
#######################################

# plot colors
my_colors <- c("#19345B", "#8C99AD", "#DCDCDC", "#96ACCF", "#2D5A9F")

## ADD SCORE TO LIKERT ANSWERS ## ---------------------------- usefulness_count
usefulness_count <- usefulness_count %>%
  mutate(
    # 1. Remove % symbols if they exist, then convert to numeric
    percent = as.numeric(str_remove(percent, "%")),
    # 2. Re-apply the score logic to ensure everything is fresh
    score = case_when(
      provides_useful_information == "Strongly agree"             ~ 5,
      provides_useful_information == "Somewhat agree"             ~ 4,
      provides_useful_information == "Neither agree nor disagree" ~ 3,
      provides_useful_information == "Somewhat disagree"          ~ 2,
      provides_useful_information == "Strongly disagree"          ~ 1),
    score = factor(score, levels = c("1", "2", "3", "4", "5")))


## MAKE PLOT ##

# Plot using scale_fill_manual
ggplot(usefulness_count, aes(x = "Response", y = percent, fill = score)) +
  geom_col(color = "white", width = 0.7, 
           position = position_stack(reverse = TRUE)) + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            size = 4, fontface = "bold", color = "white") + # Changed text to white for dark backgrounds
  coord_flip() +
  # Use your custom hex codes here
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL,
    fill = "Score")

## ADD SCORE TO LIKERT ANSWERS ## ---------------------------- newarrival_count
newarrival_count <- newarrival_count %>%
  mutate(
    # 1. Remove % symbols if they exist, then convert to numeric
    percent = as.numeric(str_remove(percent, "%")),
    # 2. Re-apply the score logic to ensure everything is fresh
    score = case_when(
      new_arrival_one_off_test == "Extremely useful"             ~ 5,
      new_arrival_one_off_test == "Very useful"             ~ 4,
      new_arrival_one_off_test == "Moderately useful" ~ 3,
      new_arrival_one_off_test == "Slightly useful"          ~ 2,
      new_arrival_one_off_test == "Not at all useful"          ~ 1),
    score = factor(score, levels = c("1", "2", "3", "4", "5")))


## MAKE PLOT ##

# Plot using scale_fill_manual
ggplot(newarrival_count, aes(x = "Response", y = percent, fill = score)) +
  geom_col(color = "white", width = 0.7, 
           position = position_stack(reverse = TRUE)) + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            size = 4, fontface = "bold", color = "white") + # Changed text to white for dark backgrounds
  coord_flip() +
  # Use your custom hex codes here
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL,
    fill = "Score")

## ADD SCORE TO LIKERT ANSWERS ## ---------------------------- pairedquarantine_count
pairedquarantine_count <- pairedquarantine_count %>%
  mutate(
    # 1. Remove % symbols if they exist, then convert to numeric
    percent = as.numeric(str_remove(percent, "%")),
    # 2. Re-apply the score logic to ensure everything is fresh
    score = case_when(
      paired_samples_quarantine == "Extremely useful"             ~ 5,
      paired_samples_quarantine == "Very useful"             ~ 4,
      paired_samples_quarantine == "Moderately useful" ~ 3,
      paired_samples_quarantine == "Slightly useful"          ~ 2,
      paired_samples_quarantine == "Not at all useful"          ~ 1),
    score = factor(score, levels = c("1", "2", "3", "4", "5")))


## MAKE PLOT ##

#  Plot using scale_fill_manual
ggplot(pairedquarantine_count, aes(x = "Response", y = percent, fill = score)) +
  geom_col(color = "white", width = 0.7, 
           position = position_stack(reverse = TRUE)) + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            size = 4, fontface = "bold", color = "white") + # Changed text to white for dark backgrounds
  coord_flip() +
  # Use your custom hex codes here
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL,
    fill = "Score")

## ADD SCORE TO LIKERT ANSWERS ## ---------------------------- greenamber_count
greenamber_count <- greenamber_count %>%
  mutate(
    # 1. Remove % symbols if they exist, then convert to numeric
    percent = as.numeric(str_remove(percent, "%")),
    # 2. Re-apply the score logic to ensure everything is fresh
    score = case_when(
      clearance_green_amber == "Extremely useful"             ~ 5,
      clearance_green_amber == "Very useful"             ~ 4,
      clearance_green_amber == "Moderately useful" ~ 3,
      clearance_green_amber == "Slightly useful"          ~ 2,
      clearance_green_amber == "Not at all useful"          ~ 1),
    score = factor(score, levels = c("1", "2", "3", "4", "5")))


## MAKE PLOT ##

# Plot using scale_fill_manual
ggplot(greenamber_count, aes(x = "Response", y = percent, fill = score)) +
  geom_col(color = "white", width = 0.7, 
           position = position_stack(reverse = TRUE)) + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            size = 4, fontface = "bold", color = "white") + # Changed text to white for dark backgrounds
  coord_flip() +
  # Use your custom hex codes here
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL,
    fill = "Score")

## ADD SCORE TO LIKERT ANSWERS ## ---------------------------- clinicaldx_count
clinicaldx_count <- clinicaldx_count %>%
  mutate(
    # 1. Remove % symbols if they exist, then convert to numeric
    percent = as.numeric(str_remove(percent, "%")),
    # 2. Re-apply the score logic to ensure everything is fresh
    score = case_when(
      testing_clinically_suspected      == "Extremely useful"             ~ 5,
      testing_clinically_suspected      == "Very useful"             ~ 4,
      testing_clinically_suspected      == "Moderately useful" ~ 3,
      testing_clinically_suspected      == "Slightly useful"          ~ 2,
      testing_clinically_suspected      == "Not at all useful"          ~ 1),
    score = factor(score, levels = c("1", "2", "3", "4", "5")))


## MAKE PLOT ##


# Plot using scale_fill_manual
ggplot(clinicaldx_count, aes(x = "Response", y = percent, fill = score)) +
  geom_col(color = "white", width = 0.7, 
           position = position_stack(reverse = TRUE)) + 
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5, reverse = TRUE), 
            size = 4, fontface = "bold", color = "white") + # Changed text to white for dark backgrounds
  coord_flip() +
  # Use your custom hex codes here
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(
    title = NULL,
    x = NULL, 
    y = NULL,
    fill = "Score")


##############################################################
## Run descriptive stats for Likert scenarios and agreement ##
##############################################################

##SCENARIOS

# Select only the 4 Likert item columns
LKTusefulness_items <- usefulness_questions[, 2:5]

# Summary statistics
LKTusefulness_pysch <- describe(LKTusefulness_items)

LKTusefulness_pysch

##AGREEMENT

### agreement_question
agreement_questionLKTdf <- agreement_question %>%
  select(provides_useful_information) %>%
  as.data.frame()

LKTagreement_pysch <- describe(agreement_questionLKTdf$provides_useful_information)

LKTagreement_pysch

####################
## Save workspace ##
####################

save.image(file = "likert analysis.RData")
