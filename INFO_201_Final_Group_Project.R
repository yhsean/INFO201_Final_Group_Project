#INFO_201_Final_Group_Project

library(dplyr)
library(stringr)

obesity_df <- read.csv("National_Obesity_By_State.csv")
walkability_df <- read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")

#CASE 1: SORT COLUMNS TO USE FIRST BEFORE JOIN
#Since we have only five columns to utilize among 50 columns, I think it would be way more efficient to size down the dataset first and start joining
#Also for group by state_abb and summarize steps to size down the walkability dataset, I have no clue with what functions to apply on other columns(e.g. Which one should we use, either mean() or sum() on the column GEOID10?)

#walkability_df <- select(walkability_df, "location" = CBSA_Name, "intersection_density" = D3B, "transit_stops" = D4A, "employment_mix" = D2B_E8MIXA, "employment_plus_household" = D2A_EPHHM)


#Extract the two letter state abbreviation from each location
#and store the state_abb data in the walkability dataset

#location_walkability <- walkability_df$location
#states_abb_walkability <- str_extract(location_walkability, paste(state.abb, collapse = "|"))
#walkability_df$state_abb <- states_abb_walkability


#Group by state_abb and summarize the data
###            I wonder at this point if we should use mean() or sum() for summarization

#walkability_df <- group_by(walkability_df, state_abb)
#walkability_df <- summarize(walkability_df, intersection_density = mean(intersection_density), transit_stops = mean(transit_stops), employment_mix = mean(employment_mix), employment_plus_household = mean(employment_plus_household) )




#CASE 2: COMBINE TWO DATASETS FIRST AND THEN ARRANGE COLUMNS

#location_walkability <- walkability_df$CBSA_Name
#states_abb_walkability <- str_extract(location_walkability, paste(state.abb, collapse = "|"))
#walkability_df$state_abb <- states_abb_walkability


#Store the two letter state abbreviation for each state in the obesity dataset 
states_abb_obesity <- state.abb[match(obesity_df$NAME, state.name)]
obesity_df$state_abb <- states_abb_obesity


#Make sure to check if each dataset has data for total 50 states

#states_in_walkability_df <- unique(walkability_df$state_abb)
#states_in_obesity_df <- unique(obesity_df$state_abb)


#Join the two data sets within the state abbreviation

combine_df <- merge(obesity_df, walkability_df, by = "state_abb", all.x = TRUE)


