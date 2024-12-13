#' ---------------------------------------------------------
#' 
#' 
#' Create hourly presence sheets from Raven selection tables
#' 
#' 
#' ---------------------------------------------------------


#### Load libraries ####

# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(svDialogs)
library(tcltk)

#############################################################
#### User-defined variables - UPDATE FOR EACH DEPLOYMENT ####

##### Deployment info ###
site_id <- dlg_input(message = "Site ID, e.g. 'TRE'")$res
dep_id <- dlg_input(message = "Deployment ID, YYYYMM")$res

# Start and end dates of deployment
start_dep_date <- as_date(dlg_input(message = "Start date: YYYY-MM-DD")$res)
end_dep_date <- as_date(dlg_input(message = "End date: YYYY-MM-DD")$res)
#############################################################

#### source helper file w/functions
source("scripts/AMP_pkgs_funs.R")

#### Compile Vessel Selections from Raven ####
all_selns <- Compile_Raven_selns(site_id = site_id, 
                                 dep_id = dep_id)

# Save compiled selection table in outputs folder
write.table(all_selns, 
            paste0("outputs/", site_id,"_",dep_id,"_all_vessel_selections.txt"),
            row.names = FALSE)


#### Create Hourly Presence Table ####

# get hours from date-times with functions from lubridate pkg

all_selns_hr <- all_selns |>
  mutate(Begin_Clock = as_datetime(Begin_Clock, format = "%H:%M:%OS"),
         End_Clock = as_datetime(End_Clock, format = "%H:%M:%OS"),
         Begin_Date = as_datetime(Begin_Date, format = "%Y/%m/%d"),
         Begin_Hour = hour(Begin_Clock),
         End_Hour = hour(End_Clock),
         # re-code anything with a maneuver as "Maneuver" (e.g., "Maneuver+CPA" = "Maneuver")
         Behavior = gsub(pattern = ".*Maneuver.*", replacement = "Maneuver", x = Behavior),
         # re-code all transit as "Transit" (e.g., "TransitA" = "Transit")
         # important that this happens AFTER the Maneuver line above so that "TransitAManeuver" doesn't get re-coded
         Behavior = gsub(pattern = ".*Transit.*", replacement = "Transit", x = Behavior),
         Behavior = gsub(pattern = "CPAManeuver", replacement = "Maneuver", x = Behavior),
         Behavior = gsub(pattern = "CPA", replacement = "Transit", x = Behavior),
         Behavior = gsub(pattern = " ", replacement = "", x = Behavior),
         Behavior = na_if(Behavior, ""),
         Behavior = replace_na(Behavior, "Not_Assigned"))
         
# Count instances of each behavior per date-hour
hr_tally <- all_selns_hr |>
  mutate(Behavior = as.factor(Behavior)) |>
  group_by(Behavior, Begin_Date, Begin_Hour) |>
  count() |>
  # pivot wider to get behavs as columns and fill missing values with 0
  pivot_wider(names_from = Behavior, 
              values_from = n,
              values_fill = 0)

# create a new df with all hours for the whole deployment
date_range_dep <- seq.Date(from = start_dep_date, to = end_dep_date, by = "day") |>
  crossing(seq(0,23,1)) 

# rename columns
names(date_range_dep) <- c("Begin_Date","Begin_Hour")

# join 2 data frames together to add behavior tally
hourly_pres <- date_range_dep |>
  left_join(hr_tally, by = c("Begin_Date","Begin_Hour")) |>
  replace_na(list(Transit = 0, Maneuver = 0))

# bring in Notes from all_selns_hr
seln_notes <- aggregate(Notes ~ Begin_Date + Begin_Hour, data = all_selns_hr, paste0, collapse = "; ") |>
  # get rid of blank entries for notes
  filter(Notes != "" & Notes != "; ")

hourly_pres_notes <- hourly_pres |>
  left_join(seln_notes, by = c("Begin_Date","Begin_Hour")) |>
  mutate(SiteID = site_id)

#### Save Hourly Presence table into /outputs folder

write.csv(hourly_pres_notes, 
          paste0("outputs/",site_id,"_",dep_id,"_Vessel_Hourly_Presence.csv"))
