#' ------------------------------------------------------
#' 
#' 
#' 
#' Create hourly presence and figures for vessels inside the park
#' 
#' 
#' J. McCordic, last updated 2024-11-25
#' 
#' ------------------------------------------------------



# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(svDialogs)
library(openxlsx)
library(tcltk2)
#############################################################

#### source helper file w/functions
source("scripts/AMP_pkgs_funs.R")



# Assign user-defined inputs ----------------------------------------------

# Since server folders are in a standard structure, use parent folder to get list of all deployments
dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)

# select the deployment(s) to be plotted
dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res

# apply getDeploymentInfo() from AMP_pkgs_funs.R to each deployment
#   prompts user for site name, start/end date, and time zones
dep_info <- dep_list |>
  map(~getDeploymentInfo()) |>
  set_names(dep_list)


# 

# for each deployment in deplist
# load csvs
ins_ves_og <- dep_info |>
  map(~read_csv(choose.files(caption = "Select HighProb_Small .csv")))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y)) |>
  bind_rows()



#### Duration ####
# Summarize total hours of presence per day
dur_bydate <- ins_ves_og |>
  group_by(Site_ID, Begin_Date_loc, Behavior) |>
  summarize(DeltaHours = sum(DeltaHours, na.rm = TRUE))

# summarize by date only to get "Total" value, then append to dur_bydate
dur_total_bydate <- ins_ves_og |>
  group_by(Site_ID, Begin_Date_loc) |>
  summarize(DeltaHours = sum(DeltaHours, na.rm = TRUE)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(dur_bydate))|>
  mutate(Behavior = factor(Behavior, levels=c("Maneuver","Transit","Total")))
#assign factors for behavior order
# Dur$Behavior = factor(Dur$Behavior, levels = c("T", "M"))

# summarize vessel event duration by behavior
dur_summ_behav <- ins_ves_og |>
  group_by(Site_ID, Behavior) |>
  summarize(mean_dur_h = mean(DeltaHours, na.rm = TRUE),
            med_dur_h = median(DeltaHours, na.rm = TRUE),
            min_dur_h = min(DeltaHours, na.rm = TRUE),
            max_dur_h = max(DeltaHours, na.rm = TRUE))

write.csv(dur_summ_behav,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Inside_Small_Duration_Summary_AWST.csv"))


















