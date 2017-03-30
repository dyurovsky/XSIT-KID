library(readr)
library(dplyr)
library(purrr)
library(tidyr)

#######################################################################
############################## CONSTANTS ##############################
#######################################################################
NUM_TRIALS <- 8
NUM_MEASURES <- 16

#######################################################################
############################ LOADING FILES ############################
#######################################################################
# Read demographic data
demos <- read_tsv("raw_data/demo/demos_xsit.txt") %>%
  separate(Subject, c("prefix", "subj"), "_") %>%
  mutate(subj = as.numeric(subj)) %>%
  select(subj, age, gender, language, include)

# Read hand-coded data from first few kids
early_data <- read_tsv("raw_data/data_MIN/early_data.txt") %>%
  mutate(overwrite = FALSE,
         trialType = factor(trialType, levels = c("Same", "Switch")))


# Read value for overwriting based on kids switching sides
test_overwrite <- read_tsv("raw_data/data_MIN/overwrite.txt") %>%
  mutate(overwrite = TRUE)


#These are all of the kids for whome we have ipad data
all_results <- list.files(path = "raw_data/data_MIN/",
                          pattern = 'results_*', 
                          all.files = FALSE, full.names = TRUE)


# Set up subject numbers for automatically-coded children
subj <- max(early_data$subj) + 1

# Read in data in html format and make into a data table
read_results <- function(file) {
 
  # read raw data and break up by html lists
  data <- readLines(file, warn=FALSE) %>%
    strsplit('<li>') %>%
    unlist
  
  # Drop a bunch of html tags
  split_data <- data[NUM_TRIALS + 1:(NUM_TRIALS*NUM_MEASURES)] %>%
    gsub("\\\\\"", "", .) %>%
    gsub("</li>", "", .) %>%
    gsub("</ul>\\},\\{<ul>", "", .) %>%
    gsub("</ul>\\}]","", .) %>%
    gsub("^.*?: ","", .)
  
  # Reformat into a data table
  data_wide <- matrix(split_data, NUM_MEASURES, NUM_TRIALS, byrow=TRUE) %>%
    as_data_frame() %>%
    mutate(subj = subj)
  
  subj <<- subj + 1
  
  #rename the rows to their semantic labels for easier analysis
  names(data_wide) <-  c("itemNum", "trialType", "samePos", "chosen", "chosen_idx",
                     "kept", "kept_idx", "rt", "subj")
  
  data_wide
}

# Fix subject numbers for children when data was not collected
fix_subj <- function(subj) {
  if(subj >= 47)
    subj <- subj + 1
  if(subj >= 71)
    subj <- subj + 1
  if(subj >= 82)
    subj <- subj + 1
  if(subj >= 85)
    subj <- subj + 1
  
  subj
}

# Concatenate all of the data
ipad_data <- map(all_results, read_results) %>%
  bind_rows() %>%
  rowwise() %>%
  mutate(subj = fix_subj(subj)) %>%
  ungroup()

# Find the number of example items each child got right
example_data <- ipad_data %>%
  group_by(subj) %>%
  slice(c(1:4, 9:12)) %>%
  summarise(example_correct = sum(chosen == c("shoe", "shoe" , "cup", "cup", 
                                              "flower", "flower", 
                                              "truck", "truck"))) 


# Make the test data array for each participant
test_data <- ipad_data %>%
  mutate(trialType = factor(trialType, labels = c("Same", "Switch"))) %>%
  group_by(subj) %>%
  slice(c(6, 8, 14, 16)) %>%
  mutate(correct = chosen == kept,
         itemNum = as.numeric(itemNum),
         samePos = as.numeric(samePos)) %>%
  select(-chosen, -kept, -chosen_idx, -kept_idx) %>%
  left_join(example_data) %>% # join in #correct on example trials
  mutate(overwrite = FALSE) 


test_overwrite_df <- test_overwrite %>%
  left_join(select(test_data, -correct, -overwrite))

test_data_overwritten <- test_data %>%
  anti_join(test_overwrite, by = c("itemNum", "subj")) %>%
  bind_rows(test_overwrite_df) %>% # join in manual corrections for tapping problems
  ungroup() %>%
  left_join(example_data) %>%
  select(subj, itemNum, trialType, samePos, correct, rt, example_correct, 
         overwrite) %>%
  arrange(subj, itemNum)


all_data <- test_data %>%
  left_join(demos) %>%
  mutate(setting = if_else(subj > 59, "CDM", "Bing"))

write_csv(all_data, "data/xsit_min.csv")
