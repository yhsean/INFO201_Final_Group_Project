#INFO_201_Final_Group_Project

library(dplyr)
library(stringr)

obesity_df <- read.csv("National_Obesity_By_State.csv")
walkability_df <- read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")

#PART 1: Joining two datasets (Sean)
#combine two datasets by creating a state_abb column
location_walkability <- walkability_df$CBSA_Name
states_abb_walkability <- str_extract(location_walkability, paste(state.abb, collapse = "|"))
walkability_df$state_abb <- states_abb_walkability

#Store the two letter state abbreviation for each state in the obesity dataset 
states_abb_obesity <- state.abb[match(obesity_df$NAME, state.name)]
obesity_df$state_abb <- states_abb_obesity

#Join the two data sets within the state abbreviation
combine_df <- merge(obesity_df, walkability_df, by = "state_abb", all.x = TRUE)



#Part 2: Data Cleaning & Augmentation (Jisoo)
#create a new dataset with only the column used to determine the walkability index
combine_df_a <- select(combine_df, "state" = state_abb, "location" = CBSA_Name, "obesity" = Obesity, "intersection_density" = D3B_Ranked, "transit_stops" = D4A_Ranked, "employment_mix" = D2B_Ranked, "employment_plus_household" = D2A_Ranked)

#create a new dataset with only states while taking the average value of each states
combine_df_avg <- combine_df_a %>%
  group_by(state) %>%
  summarize(
    obesity = mean(obesity),
    intersection_density = mean(intersection_density),
    transit_stops = mean(transit_stops),
    employment_mix = mean(employment_mix),
    employment_and_household = mean(employment_plus_household)
  ) 

#creating a new numerical variable: determine the walkability index by the formula given by EPA. 
combine_df_avg <- combine_df_avg %>%
  mutate(walkability_index = (intersection_density/3) + (transit_stops/3) + (employment_mix/6) + (employment_and_household/6))

#creating a new categorical variable: determine the walkability index category by the categorical range given by EPA. 
combine_df_avg <- combine_df_avg %>% 
  mutate(walkability_category = case_when(
    walkability_index >= 1 & walkability_index <= 5.75 ~ "least walkable",
    walkability_index >= 5.76 & walkability_index <= 10.5 ~ "below average walkable",
    walkability_index >= 10.51 & walkability_index <= 15.25 ~ "above average walkable",
    walkability_index >= 15.26 & walkability_index <= 20 ~ "most walkable",
  ))

#removing uncessary row for the data analysis i.e. U.S. Territories and misc. locations
combine_df_avg <- combine_df_avg %>%
  slice(-51)

#summarizing data
#first, we will summarize combine_df_avg to calculate the minimum, maximum, mean, and median for walkability index and obesity. 
walkability_summary <- combine_df_avg %>%
  group_by() %>%
  summarise(wi_min = min(walkability_index),
            wi_max = max(walkability_index),
            wi_mean = mean(walkability_index),
            wi_median = median(walkability_index),
            ob_min = min(obesity),
            ob_max = max(obesity),
            ob_mean = mean(obesity),
            ob_median = median(obesity))

#then I found the average obesity for the states that were categorized 'below average walkable' and 'above average walkable' as there were only these two categories present in the dataframe
a <- combine_df_avg %>%
  filter(str_sub(walkability_category, ) == "below average walkable") %>%
  summarize(ob_ba_walkable = mean(obesity))
b <- combine_df_avg %>%
  filter(str_sub(walkability_category, ) == "above average walkable") %>%
  summarize(ob_aa_walkable = mean(obesity))

#Finally, we combined these dataframes to make a finalized summary dataframe 
combined_summary <- bind_cols(walkability_summary, a, b)

write.csv(combine_df, "INFO201_group_project_data_joining.csv", row.names = FALSE)
write.csv(combine_df_avg, "INFO201_group_project_data_cleaning.csv", row.names = FALSE)
