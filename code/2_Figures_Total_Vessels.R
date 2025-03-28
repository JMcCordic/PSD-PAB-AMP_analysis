#'-------------------------------------------
#'
#'
#'
#' Create diel and weekly summary figures for vessel presence
#'    Requires: Hourly presence (.csv)
#'              Compiled selection table (.txt)
#'              Time zone of original files
#'              Time zone used for plotting
#'
#' Last updated J. McCordic
#' 2024-11-19
#' 
#' 
#'-------------------------------------------


# Load libraries ----------------------------------------------------------



# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(svDialogs)
library(openxlsx)
# library(tcltk2)
#############################################################

#### source helper file w/functions
source("code/AMP_pkgs_funs.R")



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



# Load data ---------------------------------------------------------------

# For each deployment in the dep_info list, load in hourly presence table & compiled selection table

hp_og <- dep_info |>
  map(~read_csv(choose.files(caption = "Choose Hourly Presence sheet .csv")))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))

selns_og <- dep_info |>
  map(~read_delim(choose.files(caption = "Choose complied seln table .txt")))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y)) 




# Mutate data ------------------------------------------------------------


#### Add some columns, update time zone ####

# Hourly presence
hp_data <- hp_og |>
  map(~rename(.,
              "TR" = "Transit",
              "M" = "Maneuver",
              "Dep" = "Dep_ID")) |>
  map2(.y = dep_info, 
       ~mutate(.x,
               Site_ID = {.y}$site_id, 
               Dep_ID = {.y}$dep_id,
               Total_Vessels = rowSums(across(c(TR, M))),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Hour, ":00"),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "ymd H:M", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()


# reshape selection tables to plot duration


selns_data <- selns_og |>
  map(~rename(., "Dep" = "Dep_ID")) |>
  map2(.y = dep_info, 
       ~mutate(.x, 
               Begin_Date = ymd(Begin_Date),
               Site_ID = {.y}$site_id, 
               Dep = {.y}$dep_id,
               # re-code anything with a maneuver as "Maneuver" (e.g., "Maneuver+CPA" = "Maneuver")
               Behavior = gsub(pattern = ".*Maneuver.*", replacement = "Maneuver", x = Behavior),
               # re-code all transit as "Transit" (e.g., "TransitA" = "Transit")
               # important that this happens AFTER the Maneuver line above so that "TransitAManeuver" doesn't get re-coded
               Behavior = gsub(pattern = ".*Transit.*", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = "CPAManeuver", replacement = "Maneuver", x = Behavior),
               Behavior = gsub(pattern = "CPA", replacement = "Transit", x = Behavior),
               Behavior = gsub(pattern = " ", replacement = "", x = Behavior),
               DeltaHours = Delta_Time_s/3600,
               # Total_Vessels = rowSums(across(c(TR,M))),
               # pull hour as time object along with date
               Hr_time = paste(Begin_Date, Begin_Clock),
               # get components of date-time object and assign time zone
               Hr_time = parse_date_time(Hr_time, "y/m/d H:M:S", tz = {.y}$tz_files),
               # change to local time zone
               Hr_local = with_tz(Hr_time, tzone = {.y}$tz_local),
               # pull out new hour and date in local time
               Begin_Hour_loc = as.numeric(hour(Hr_local)),
               Begin_Date_loc = date(Hr_local),
               # add weekday column
               Weekday = weekdays(Begin_Date_loc))) |>
  bind_rows()

write.csv(selns_data, paste0("outputs/",paste0(dep_list, collapse = "_"),"_compiled_selections_data_localtz.csv" ))

# Reshape data for plotting ----------------------------------------------------

# Ignore NAs for days with no vessel presence

#### Diel presence ####
hp_diel <- hp_data |>
  group_by(Site_ID, 
           Dep, Begin_Hour_loc) |>
  summarise(Transit = sum(TR, na.rm = TRUE),
            Maneuver = sum(M, na.rm = TRUE),
            Total = sum(Total_Vessels, na.rm = TRUE)) |>
  pivot_longer(cols = c("Transit","Maneuver","Total"),
               names_to = "Behav",
               values_to = "N_ves")

write.csv(hp_diel,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Diel_Summary_localtz.csv"))


#### Weekday presence ####
hp_weekday <- hp_data |>
  group_by(Site_ID, Dep, Weekday) |>
  summarise(Transit = sum(TR, na.rm = T),
            Maneuver = sum(M, na.rm = T),
            Total = sum(Total_Vessels, na.rm = T))|>
  pivot_longer(cols = c("Transit","Maneuver","Total"),
               names_to = "Behav",
               values_to = "N_ves") |>
  mutate(Weekday = factor(Weekday, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")))
levels(hp_weekday$Weekday) <- c("S","M","T","W","R","F","Sa")

write.csv(hp_weekday,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Weekday_Summary_localtz.csv"))


#### Date ####
hp_bydate <- hp_data |>
  group_by(Site_ID, Dep, Begin_Date_loc) |>
  summarise(Transit = sum(TR, na.rm = T),
            Maneuver = sum(M, na.rm = T),
            Total = sum(Total_Vessels, na.rm = T))|>
  pivot_longer(cols = c("Transit","Maneuver","Total"),
               names_to = "Behav",
               values_to = "N_ves")

write.csv(hp_bydate,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Daily_Count_Summary_localtz.csv"))


#### Duration ####
# Summarize total hours of presence per day
dur_bydate <- selns_data |>
  group_by(Site_ID, Begin_Date_loc, Behavior) |>
  summarize(DeltaHours = sum(DeltaHours, na.rm = TRUE))

# summarize by date only to get "Total" value, then append to dur_bydate
dur_total_bydate <- selns_data |>
  group_by(Site_ID, Begin_Date_loc) |>
  summarize(DeltaHours = sum(DeltaHours, na.rm = TRUE)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(dur_bydate))|>
  mutate(Behavior = factor(Behavior, levels=c("Maneuver","Transit","Total")))
#assign factors for behavior order
# Dur$Behavior = factor(Dur$Behavior, levels = c("T", "M"))

# summarize vessel event duration by behavior
dur_summ_behav <- selns_data |>
  group_by(Site_ID, Behavior) |>
  summarize(mean_dur_h = mean(DeltaHours, na.rm = TRUE),
            med_dur_h = median(DeltaHours, na.rm = TRUE),
            min_dur_h = min(DeltaHours, na.rm = TRUE),
            max_dur_h = max(DeltaHours, na.rm = TRUE))

dur_summ_total <- selns_data |>
  group_by(Site_ID) |> 
  summarize(mean_dur_h = mean(DeltaHours, na.rm = TRUE),
            med_dur_h = median(DeltaHours, na.rm = TRUE),
            min_dur_h = min(DeltaHours, na.rm = TRUE),
            max_dur_h = max(DeltaHours, na.rm = TRUE)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(dur_summ_behav)) |>
  mutate(mean_dur_m = mean_dur_h*60,
         med_dur_m = med_dur_h*60,
         min_dur_m = min_dur_h*60,
         max_dur_m = max_dur_h*60)

  

write.csv(dur_summ_total,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Event_Duration_Summary_localtz.csv"))

# summarize vessel duration by day (e.g., mean/min/max vessel hours in a 24h period by behavior)
dur_summ_daily <- dur_total_bydate |>
  group_by(Site_ID, Behavior) |>
  summarize(mean_dur_h = mean(DeltaHours, na.rm = TRUE),
            med_dur_h = median(DeltaHours, na.rm = TRUE),
            min_dur_h = min(DeltaHours, na.rm = TRUE),
            max_dur_h = max(DeltaHours, na.rm = TRUE))

write.csv(dur_summ_daily,
          paste0("outputs/", paste0(dep_list, collapse = "_"),"_Daily_Duration_Behav_Summary_localtz.csv"))


# dur_summ_behav <- summaryBy(DeltaHours~Behavior+SiteID, data=Dur, FUN=c(sum,min,max,median,length))
# dur_summ_tot <- summaryBy(DeltaHours~Begin_Date+SiteID, data=df_all_selns, FUN=c(sum,min,max,median,length))





#### Plots ####

#### Diel ####



ggplot(data=hp_diel, aes(x=Begin_Hour_loc, y = N_ves)) +
  geom_col(aes(fill=Behav), 
           colour="black", width=1)+ 
  scale_fill_manual(limits=c("Total", "Transit","Maneuver"),
                    values= c("Total" = "grey75", "Transit" = "#895c89", "Maneuver" = "#ffa156"))+
  facet_grid(rows=vars(forcats::fct_relevel(Behav, "Transit","Maneuver","Total")), cols = vars(Site_ID))+
  ylab("Number of Discrete Events")+
  xlab("Hour of Day (AWST, UTC+8)")+
  # ylim(c(0,100))+
  ggtitle("Vessel Activity by Time of Day")+
  scale_x_continuous(breaks = seq(0,23, by=6))+
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")
  #guides(fill=guide_legend(title="Behavioral \nCategory"))

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"),"_Diel_AWST.png"), width=8, height=8,
       units="in", dpi=300)




#### Weekday ####


ggplot(data=hp_weekday, aes(x=Weekday, y = N_ves)) +
  geom_col(aes(fill=Behav), 
           colour="black", width=1)+ 
  scale_fill_manual(limits=c("Transit","Maneuver","Total"),
                    values= c("Transit" = "#895c89", "Maneuver" = "#ffa156", "Total" = "grey75"))+
  facet_grid(rows=vars(forcats::fct_relevel(Behav, "Transit","Maneuver","Total")), cols = vars(Site_ID))+
  ylab("Number of Discrete Events")+
  xlab("Day of Week")+
  # ylim(c(0,100))+
  ggtitle("Vessel Activity by Day of Week")+
  # scale_x_continuous(breaks = seq(0,23, by=6))+
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")  
  #guides(fill=guide_legend(title="Behavioral \nCategory"))

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"),"_Weekday_AWST.png"), width=8, height=8,
       units="in", dpi=300)



#### By Date ####


ggplot(data=hp_bydate, aes(x=Begin_Date_loc, y = N_ves)) +
  geom_col(aes(fill=Behav), 
           colour="black", width=1)+ 
  scale_fill_manual(limits=c("Transit","Maneuver","Total"),
                    values= c("Transit" = "#895c89", "Maneuver" = "#ffa156", "Total" = "grey75"))+
  facet_grid(rows=vars(forcats::fct_relevel(Behav, "Transit","Maneuver","Total")), cols = vars(Site_ID))+
  ylab("Number of Discrete Events")+
  xlab("Date")+
  # ylim(c(0,100))+
  ggtitle("Vessel Activity by Date")+
  scale_x_date(
    name="Date", 
    date_breaks="13 days",
    date_labels="%d-%b",
    expand=expansion(0,0))+  
  theme(text=element_text(size=18), 
        strip.background = element_rect(color = "white", fill = "black"), 
        strip.text.x = element_text(colour = "white", face = "bold"), 
        strip.text.y = element_text(colour = "white", face = "bold"), 
        plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none") 
  #guides(fill=guide_legend(title="Behavioral \nCategory"))

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"),"_Daily_AWST.png"), width=8, height=8,
       units="in", dpi=300)



### PLOT - Duration ###
ggplot(data=dur_total_bydate, 
       aes(x=Begin_Date_loc, y=DeltaHours, fill=Behavior))+
  facet_grid(rows=vars(Behavior), cols = vars(Site_ID), drop = T)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Vessel Duration (hr)")+
  xlab("Date")+
  ggtitle(expression('Daily Vessel Duration')) +
  scale_x_date(
    name="Date", 
    date_breaks="13 days",
    date_labels="%d-%b",
    expand=expansion(0,0))+
  scale_fill_manual(limits=c("Transit","Maneuver","Total"),
                    values= c("Transit" = "#895c89", "Maneuver" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=18),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        strip.text.y = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle=90, vjust=0.5),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"),"_Duration_AWST.png"), width=8, height=8,
       units="in", dpi=300)
