# title: extract data
# author: Ian Hussey (ian.hussey@ugent.be)


# dependencies ------------------------------------------------------------


library(plyr)
library(tidyverse)


# data acquisition and cleaning - experiment 0 (discarded data)  ---------------



setwd("/Users/Ian/git/finn-reanalysis/experiment 2")

# exp 2

# conditions data ---------------------------------------------------------


full_inst_condition_summary <- read.csv("data/raw/full instructions condition/combined summary.csv")
minimal_inst_condition_summary <- read.csv("data/raw/minimal instructions condition/combined summary.csv")
combined_summary <- rbind(full_inst_condition_summary, minimal_inst_condition_summary)

conditions_temp <- combined_summary %>%
  dplyr::filter(column_1 == "Participant Name or Code: " | 
                  column_1 == "The consistent responding rule was: " |
                  column_1 == "The Consistent-First Sequence was Selected" |
                  column_1 == "The Inconsistent-First Sequence was Selected") %>%
  tibble::rownames_to_column() %>%
  dplyr::select(rowname, column_1, column_2) %>%
  dplyr::mutate(n_participant = floor(as.numeric(rowname)/4 - .25)+1) %>%  # hacky way to get a grouping var
  tidyr::spread(column_1, column_2) %>%
  dplyr::rename(participant = `Participant Name or Code: `,
                rule = `The consistent responding rule was: `,
                block_order_1 = `The Consistent-First Sequence was Selected`,
                block_order_2 = `The Inconsistent-First Sequence was Selected`) %>%
  dplyr::select(-rowname) %>%
  dplyr::arrange(n_participant)

conditions_temp_2 <- conditions_temp %>%
  dplyr::distinct(participant, .keep_all = TRUE) %>%
  dplyr::select(participant, n_participant) %>%
  dplyr::filter(!is.na(participant)) %>%
  dplyr::mutate(participant = as.factor(participant))
    
conditions_temp_3 <- conditions_temp %>%
  dplyr::select(n_participant, rule) %>%
  dplyr::filter(!is.na(rule)) %>%
  dplyr::mutate(rule = ifelse(rule == "Colours are colours and shapes are shapes", "full", 
                              ifelse(rule == "Respond correctly to the stimuli", "minimal", NA)))

conditions_temp_4 <- conditions_temp %>%
  dplyr::select(n_participant, block_order_1) %>%
  dplyr::filter(!is.na(block_order_1)) %>%
  dplyr::mutate(block_order_1 = "con")

conditions_temp_5 <- conditions_temp %>%
  dplyr::select(n_participant, block_order_2) %>%
  dplyr::filter(!is.na(block_order_2)) %>%
  dplyr::mutate(block_order_2 = "incon")


conditions_data <- 
  plyr::join_all(list(as.data.frame(conditions_temp_2),  # join_all throws a requires input be data.frame error, despite is.data.frame returning TRUE for all members of list. Workaround is to coerce all to DF here. 
                      as.data.frame(conditions_temp_3),
                      as.data.frame(conditions_temp_4),
                      as.data.frame(conditions_temp_5)),
                 by = "n_participant",
                 type = "full") %>%
  dplyr::mutate(block_order = paste0(block_order_1, block_order_2),
                block_order = ifelse(block_order == "conNA", "con", 
                                     ifelse(block_order == "NAincon", "incon", NA)),
                participant = as.factor(participant)) %>%
  select(-block_order_1, -block_order_2, -n_participant)

conditions_data %>% write.csv("data/processed/conditions data.csv", row.names = FALSE)


# d score data ------------------------------------------------------------


# generated via the existing excel data processor used on the two concated Raw files (because I'm lazy).
# paste data into the first sheet in the empty columns, and then copy the last few purple columns to a new sheet using paste values.
# then do an edit>select all>blanks and delete>empty rows to delete the excess rows, 
# then do a search and replace for "Participant " in the participant column to change this to a numeric.
# finally, save as a csv called "D scored data.csv"


# combine d scores and conditions -----------------------------------------


d_scores_data <- 
  read.csv("data/processed/D scored data.csv") %>%
  select(-block_order) %>%
  dplyr::rename(participant = participant_code) %>%
  dplyr::mutate(participant = as.factor(participant))


all_data <- dplyr::inner_join(conditions_data, d_scores_data, by = "participant")


all_data %>% write.csv("processed data.csv", row.names = FALSE)

