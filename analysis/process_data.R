library(here)
library(tidyverse)
library(jsonlite)
library(testthat)

processed_data_directory <- here("..","data","processed_data")
file_name <- "moral_character"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#extract json data
end_questions <- exp_data %>% 
  filter(trial_type == "survey-text") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,Q0:Q3)

demo <- exp_data %>% 
  filter(trial_type == "survey-html-form") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,pid:gender) %>%
  rename(participant_id=pid)

man_check_cols <- c("manipulation_check_nate","manipulation_check_justin")
likert_responses_man_check <- exp_data %>%
  filter(trial_type == "survey-likert"&trial_index==4) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(manipulation_check_question_order=question_order) %>%
  select(random_id,manipulation_check_question_order,all_of(man_check_cols))

justin_cols <- c("justin.far",
                   "justin.standards",
                   "justin.certain",
                   "justin.reservations",
                   "justin.conflicted",
                   "justin.deep",
                   "justin.principles",
                   "justin.calm",
                   "justin.upset")
likert_responses_justin <- exp_data %>%
  filter(trial_type == "survey-likert"&((trial_index==5&first_question_page=="justin")|(trial_index==6&first_question_page=="nate"))) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(justin_question_order=question_order) %>%
  select(random_id,
  justin_question_order,all_of(justin_cols))

nate_cols <- c("nate.far",
               "nate.standards",
               "nate.certain",
               "nate.reservations",
               "nate.conflicted",
               "nate.deep",
               "nate.principles",
               "nate.calm",
               "nate.upset")
likert_responses_nate <- exp_data %>%
  filter(trial_type == "survey-likert"&((trial_index==5&first_question_page=="nate")|(trial_index==6&first_question_page=="justin"))) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  rename(nate_question_order=question_order) %>%
  select(random_id,
         nate_question_order,
         all_of(nate_cols))

#join into exp_data
exp_data_wide <- exp_data %>%
  distinct(random_id,condition,first_question_page) %>%
  left_join(end_questions) %>%
  left_join(demo) %>%
  left_join(likert_responses_man_check) %>%
  left_join(likert_responses_justin) %>%
  left_join(likert_responses_nate)

#double check that participant ids are unique
counts_by_random_id <- exp_data_wide %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#convert data to long
processed_data <- exp_data_wide %>%
  pivot_longer(
    cols=c(all_of(man_check_cols),all_of(justin_cols),all_of(nate_cols)),
    names_to="likert_question",
    values_to="likert_response"
  ) %>%
  relocate(
    participant_id,.after="random_id"
  ) %>%
  mutate(likert_response = likert_response+1)

#filter participant ids
filter_ids <- c(
  "1","12","1234","a1","as","nm","p4","p6"
)

processed_data <- processed_data %>%
  mutate(participant_id = trimws(tolower(participant_id))) %>%
  #fix some ids
  mutate(
    participant_id = case_when(
      participant_id == "herson" ~ "heron",
      participant_id == "p73" ~ "giraffe",
      participant_id == "a1789315" ~ "rabbit",
      participant_id == "a17287899" ~ "trex",
      participant_id == "a16995566" ~ "squirrel",
      participant_id == "a16486413" ~ "beaver",
      participant_id == "a18151131" ~ "rhino",
      TRUE ~ participant_id
    )
  ) %>%
  filter(!(participant_id %in% filter_ids))

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
