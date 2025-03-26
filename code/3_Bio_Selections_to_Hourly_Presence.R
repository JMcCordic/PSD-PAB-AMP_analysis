-#' ---------------------------------------------------------------
#'
#'
#'  Create hourly presence sheets from Raven selection tables
#'  
#'
#' ---------------------------------------------------------------

#### Load libraries ####

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(tcltk)

#############################################################
#### User-defined variables - UPDATE FOR EACH DEPLOYMENT ####

##### Deployment info ###
site_id <- "DNE"
dep_id <- "D1"

# Start and end dates of deployment
start_dep_date <- as_date("2020-09-09")
end_dep_date <- as_date("2020-11-04")
#############################################################

#### source helper file w/functions
source("scripts/AMP_pkgs_funs.R")

#### Compile Biology Selections from Raven ####
all_bio_selns <- Compile_Raven_bio_selns(site_id = site_id, 
                                         dep_id = dep_id)

# Save compiled selection table in outputs folder
write.table(all_bio_selns, 
            paste0("outputs/", site_id,"_",dep_id,"_all_biology_selections.txt"),
            row.names = FALSE)

#### Load in dolphin detection hourly presence ####
dol_det <- read_csv(tk_choose.files()) |>
  # rename date and hour columns for easier merging later
  rename(Begin_Hour = Hour,
         Begin_Date = Day) |>
  # convert Begin_Date to datetime object
  mutate(Begin_Date = as_datetime(Begin_Date, format = "%m/%d/%Y"),
         # add bio category + call type columns
         Bio_Category = "dol",
         Call_type = "w",
         category_calltype = paste0(Bio_Category, "_", Call_type))

#### Create Hourly Presence Table ####

# get hours from date-times with functions from lubridate pkg
# fix typos etc from original selection tables --> look into better way to do this?

all_bio_selns_hr <- all_bio_selns |>
  mutate(Begin_Clock = as_datetime(Begin_Clock, format = "%H:%M:%OS"),
         End_Clock = as_datetime(End_Clock, format = "%H:%M:%OS"),
         Begin_Date = as_datetime(Begin_Date, format = "%Y/%m/%d"),
         Begin_Hour = hour(Begin_Clock),
         End_Hour = hour(End_Clock),
         # make some replacements to account for common inconsistencies
         # get rid of spaces
         Bio_Category = gsub(pattern = " ", replacement = "", x = Bio_Category),
         Call_type = gsub(pattern = " ", replacement = "", x = Call_type),
         # capitalization
         # Bio_Category = gsub(pattern = "Fish", replacement = "fish", x = Bio_Category),
         # Bio_Category = gsub(pattern = "UK", replacement = "uk", x = Bio_Category),
         # Call_type = gsub(pattern = "UK", replacement = "uk", x = Call_type),
         # fix codes
         # Call_type = case_when(Bio_Category == "pygmyblue" | Bio_Category == "pygmybluewhale" ~ "pbw",
         #                       TRUE ~ Call_type),
         # Bio_Category = gsub(pattern = "SpermWhale", replacement = "pm", x = Bio_Category),
         # Bio_Category = gsub(pattern = "pn", replacement = "pm", x = Bio_Category),
         # Bio_Category = gsub(pattern = "ifish", replacement = "fish", x = Bio_Category),
         Bio_Category = gsub(pattern = "uku", replacement = "uk", x = Bio_Category),
         Call_type = gsub(pattern = "pulse.*", replacement = "p", x = Call_type),
         Call_type = gsub(pattern = "\\[", replacement = "p", x = Call_type)
         )




#### Count instances of each category per date-hour ####

hr_bio_tally <- all_bio_selns_hr |>
  filter(is.na(Bio_Category)==FALSE) |>
  filter(Bio_Category!="")|> 
  mutate(
    "category_calltype" = paste0(Bio_Category,"_",Call_type)) |>
  # some quality control and filtering
  # group by Bio_Category, date, and hour to summarize
  group_by(category_calltype, Begin_Date, Begin_Hour) |>
  # count instances in each grouping
  count() |>
  # pivot wider to get behavs as columns and fill missing values with 0
  pivot_wider(names_from = category_calltype, 
              values_from = n,
              values_fill = 0)

# pivot dolphin table to get tally
hr_dol_tally <- dol_det |>
  filter(Presence == 1) |>
  group_by(category_calltype, Begin_Date, Begin_Hour) |>
  # count instances in each grouping
  count() |>
  # pivot wider to get behavs as columns and fill missing values with 0
  pivot_wider(names_from = category_calltype, 
              values_from = n,
              values_fill = 0)


all_bio_hr <- left_join(hr_bio_tally, hr_dol_tally, by = c("Begin_Date", "Begin_Hour"))


# create a new df with all hours for the whole deployment
date_range_dep <- seq.Date(from = start_dep_date, to = end_dep_date, by = "day") |>
  crossing(seq(0,23,1)) 

# rename columns
names(date_range_dep) <- c("Begin_Date","Begin_Hour")

# join 2 data frames together to add behavior tally
hourly_bio_pres <- date_range_dep |>
  left_join(all_bio_hr, by = c("Begin_Date","Begin_Hour")) |>
  replace_na(list(bal_pbw = 0, 
                  dol_echolocation = 0,
                  dol_uk = 0, 
                  dol_w = 0,
                  fish_c = 0,
                  fish_p = 0,
                  fish_spikeylowfrequencyp = 0,
                  fish_uk = 0,
                  pm_clicks = 0,
                  pm_uk = 0, 
                  uk_uk = 0))

# bring in Notes from all_selns_hr
seln_bio_notes <- aggregate(Notes ~ Begin_Date + Begin_Hour, data = all_bio_selns_hr, paste0, collapse = "; ") |>
  # get rid of blank entries for notes
  filter(Notes != "" & Notes != "; ")

hourly_bio_pres_notes <- hourly_bio_pres |>
  left_join(seln_bio_notes, by = c("Begin_Date","Begin_Hour"))


#### Save Hourly Presence table into /outputs folder

write.csv(hourly_bio_pres_notes, 
          paste0("outputs/",site_id,"_",dep_id,"_Biology_Hourly_Presence.csv"))
