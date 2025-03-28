#' ############################################
#'
#'
#' Reshaping datasets and making figures
#' for vessels estimated to occur within
#' AMP NPZ boundaries
#' 
#' Originally written L. Kline 2019
#' 
#' 
#' Last updated J. McCordic Dec 2024
#'
#'
#'
#'#############################################




# NOAA approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(svDialogs) # dialog boxes
library(tcltk2) # server file and folder navigation

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

# For each deployment in the dep_info list, load in High-probability csv files

# Only need small size class CSV since med size class will be a subset of this
ins_small_og <- dep_info |>
  map(~read_csv(choose.files(caption = "Choose HighProb_Small .csv")))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y))

ins_small_data <- ins_small_og |>
  bind_rows()

ins_smallmed_data <- ins_small_data |>
  filter(pins_ovrll >= 0.75)




# Small vessels only ------------------------------------------------------

#### Daily count + duration ####

# summarize daily count + duration by behavior
ins_daily_dur_behav <- ins_small_data |>
  group_by(Site_ID, Dep_ID, Begin_Date_loc, Behavior) |>
  summarize(count_ins_sm = n(),
            tot_daily_hrs = sum(DeltaHours),
            med_daily_hrs = median(DeltaHours),
            min_daily_hrs = min(DeltaHours),
            max_daily_hrs = max(DeltaHours))

# get totals and add to above
ins_daily_totals <- ins_small_data |>
  group_by(Site_ID, Dep_ID, Begin_Date_loc) |>
  summarize(count_ins_sm = n(),
            tot_daily_hrs = sum(DeltaHours),
            med_daily_hrs = median(DeltaHours),
            min_daily_hrs = min(DeltaHours),
            max_daily_hrs = max(DeltaHours)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(ins_daily_dur_behav)) |>
  mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
         Behavior = recode(Behavior, Transit = "T",
                           Maneuver = "M",
                           Total = "Total"))

#### Plot daily duration ####

ggplot(data=ins_daily_totals, 
       aes(x=Begin_Date_loc, y=tot_daily_hrs, fill=Behavior))+
  facet_grid(rows=vars(Behavior), 
             cols = vars(Site_ID), drop = F)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  # geom_rect(data = date_gaps, 
  #           inherit.aes = FALSE,
  #           aes(xmin = gap_start, 
  #               xmax = gap_end,
  #               ymin = 0, 
  #               ymax = 8)) +
  ylab("Vessel Duration (hr)")+
  xlab("Date")+
  ggtitle(expression('Daily Vessel Duration, P'['Small in']>= 0.75)) +
  scale_x_date(name="Date",
               limits = as.Date(c("2022-11-21","2023-01-18"),format = "%Y-%m-%d"),
               date_breaks="13 days", date_labels="%d-%b")+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  
  theme(text=element_text(size=18),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        strip.text.y = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle=90, vjust=0.5),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))
 
ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_Small_Duration.png"), width=9, height=8,
        units="in", dpi=300)


#### Plot daily counts ####
 
ggplot(data=ins_daily_totals, 
        aes(x=Begin_Date_loc, 
            y=count_ins_sm, 
            fill=Behavior))+
   facet_grid(rows=vars(Behavior), 
              cols=vars(Site_ID), drop=F)+
   geom_col(width=1, colour="black")+
   geom_hline(yintercept=0)+
   ylab("Vessels")+
   xlab("Date")+
   ggtitle(expression('Daily Vessel Count, P'['Small in']>= 0.75))+
   # scale_y_continuous(breaks = seq(0,8,2), labels = c("",2,4,6,8), limits = c(0,8))+
   #scale_y_continuous(breaks=c(0, 8))+
   #scale_x_continuous(breaks=c(0,6,12,18,23))+
   scale_x_date(name="Date",
                limits = as.Date(c("2022-11-21","2023-01-18"),format = "%Y-%m-%d"),
                date_breaks="13 days", date_labels="%d-%b")+
   scale_fill_manual(limits=c("T","M","Total"),
                     values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
   theme(text=element_text(size=16),
         strip.background = element_rect(color = "white", fill = "black"),
         strip.text.y = element_text(color = "white", face = "bold"),
         axis.text.x = element_text(angle=90, vjust=0.5),
         plot.title = element_text(hjust = 0.5)) 
 
 ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_Small_DailyCounts.png"), width=9, height=8,
        units="in", dpi=300)
 
 
 
#### Weekday ####

 
 # summarize daily count + duration by behavior
 ins_weekday_dur_behav <- ins_small_data |>
   group_by(Site_ID, Dep_ID, Weekday, Behavior) |>
   summarize(count_weekday_ins_sm = n(),
             tot_weekday_hrs = sum(DeltaHours),
             med_weekday_hrs = median(DeltaHours),
             min_weekday_hrs = min(DeltaHours),
             max_weekday_hrs = max(DeltaHours))
 
 # get totals and add to above
 ins_weekday_totals <- ins_small_data |>
   group_by(Site_ID, Dep_ID, Weekday) |>
   summarize(count_weekday_ins_sm = n(),
             tot_weekday_hrs = sum(DeltaHours),
             med_weekday_hrs = median(DeltaHours),
             min_weekday_hrs = min(DeltaHours),
             max_weekday_hrs = max(DeltaHours)) |>
   mutate(Behavior = "Total") |>
   ungroup() |>
   rbind(ungroup(ins_weekday_dur_behav)) |>
   mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
          Behavior = recode(Behavior, Transit = "T",
                            Maneuver = "M",
                            Total = "Total"),
          Weekday = factor(Weekday, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
          Weekday = recode(Weekday,
                           Sunday = "Su",
                           Monday = "M",
                           Tuesday = "T",
                           Wednesday = "W",
                           Thursday = "R",
                           Friday = "F",
                           Saturday = "Sa"))

 

#### Plot weekday counts ####
 
ggplot(data=ins_weekday_totals, 
       aes(x=Weekday, 
           y=count_weekday_ins_sm, 
           fill=Behavior))+
  facet_grid(rows=vars(Behavior),
             cols=vars(Site_ID), drop=F)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Number of Discrete Vessels")+
  xlab("Day of Week")+
  ggtitle(expression('Weekday Vessel Count, P'['Small in']>='0.75'))+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=18),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))


ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_Small_weekday.png"), width=9, height=8,
        units="in", dpi=300)


##### Diel ####
# summarize hourly count by behavior
ins_diel_behav <- ins_small_data |>
  group_by(Site_ID, Dep_ID, Begin_Hour_loc, Behavior) |>
  summarize(count_ins_sm = n())

# get totals and add to above
ins_diel_totals <- ins_small_data |>
  group_by(Site_ID, Dep_ID, Begin_Hour_loc) |>
  summarize(count_ins_sm = n()) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(ins_diel_behav)) |>
  mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
         Behavior = recode(Behavior, Transit = "T",
                           Maneuver = "M",
                           Total = "Total"))


#### Plot diel ####
ggplot(data=ins_diel_totals, 
       aes(x=Begin_Hour_loc, 
           y=count_ins_sm, fill=Behavior))+
  facet_grid(rows=vars(Behavior), cols=vars(Site_ID), drop=F)+
  # geom_col(data=night_hours, aes(x=night, y=night_val, alpha=0.6), width=1, fill="grey58")+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Number of Discrete Vessels")+
  xlab("Hour (AWST, UTC+8)")+
  ggtitle(expression('Hourly Vessel Count, P'['Small in']>= 0.75))+
  # scale_y_continuous(breaks=c(0, 1))+
  scale_x_continuous(breaks=c(0,6,12,18,23))+
  # scale_y_continuous(breaks=c(0,1,2,3), labels = c("","1","2","3"))+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=16),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.y = element_text(color = "white", face = "bold"),
        strip.text.x = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))


ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_Small_diel.png"), width=9, height=8,
       units="in", dpi=300)



# Small or Med vessels ------------------------------------------------------

#### Daily count + duration ####

# summarize daily count + duration by behavior
ins_daily_dur_behav_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Begin_Date_loc, Behavior) |>
  summarize(count_ins_sm = n(),
            tot_daily_hrs = sum(DeltaHours),
            med_daily_hrs = median(DeltaHours),
            min_daily_hrs = min(DeltaHours),
            max_daily_hrs = max(DeltaHours))

# get totals and add to above
ins_daily_totals_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Begin_Date_loc) |>
  summarize(count_ins_sm = n(),
            tot_daily_hrs = sum(DeltaHours),
            med_daily_hrs = median(DeltaHours),
            min_daily_hrs = min(DeltaHours),
            max_daily_hrs = max(DeltaHours)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(ins_daily_dur_behav_md)) |>
  mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
         Behavior = recode(Behavior, Transit = "T",
                           Maneuver = "M",
                           Total = "Total"))

#### Plot daily duration ####

ggplot(data=ins_daily_totals_md, 
       aes(x=Begin_Date_loc, y=tot_daily_hrs, fill=Behavior))+
  facet_grid(rows=vars(Behavior), 
             cols = vars(Site_ID), drop = F)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  # geom_rect(data = date_gaps, 
  #           inherit.aes = FALSE,
  #           aes(xmin = gap_start, 
  #               xmax = gap_end,
  #               ymin = 0, 
  #               ymax = 8)) +
  ylab("Vessel Duration (hr)")+
  xlab("Date")+
  ggtitle(expression('Daily Vessel Duration, P'['in']>= 0.75)) +
  scale_x_date(name="Date",
               limits = as.Date(c("2022-11-21","2023-01-18"),format = "%Y-%m-%d"),
               date_breaks="13 days", date_labels="%d-%b")+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  
  theme(text=element_text(size=18),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        strip.text.y = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle=90, vjust=0.5),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_SmallMed_Duration.png"), width=9, height=8,
       units="in", dpi=300)


#### Plot daily counts ####

ggplot(data=ins_daily_totals_md, 
       aes(x=Begin_Date_loc, 
           y=count_ins_sm, 
           fill=Behavior))+
  facet_grid(rows=vars(Behavior), 
             cols=vars(Site_ID), drop=F)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Vessels")+
  xlab("Date")+
  ggtitle(expression('Daily Vessel Count, P'['in']>= 0.75))+
  # scale_y_continuous(breaks = seq(0,8,2), labels = c("",2,4,6,8), limits = c(0,8))+
  #scale_y_continuous(breaks=c(0, 8))+
  #scale_x_continuous(breaks=c(0,6,12,18,23))+
  scale_x_date(name="Date",
               limits = as.Date(c("2022-11-21","2023-01-18"),format = "%Y-%m-%d"),
               date_breaks="13 days", date_labels="%d-%b")+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=16),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.y = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle=90, vjust=0.5),
        plot.title = element_text(hjust = 0.5)) 

ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_SmallMed_DailyCounts.png"), width=9, height=8,
       units="in", dpi=300)



#### Weekday ####


# summarize daily count + duration by behavior
ins_weekday_dur_behav_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Weekday, Behavior) |>
  summarize(count_weekday_ins_sm = n(),
            tot_weekday_hrs = sum(DeltaHours),
            med_weekday_hrs = median(DeltaHours),
            min_weekday_hrs = min(DeltaHours),
            max_weekday_hrs = max(DeltaHours))

# get totals and add to above
ins_weekday_totals_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Weekday) |>
  summarize(count_weekday_ins_sm = n(),
            tot_weekday_hrs = sum(DeltaHours),
            med_weekday_hrs = median(DeltaHours),
            min_weekday_hrs = min(DeltaHours),
            max_weekday_hrs = max(DeltaHours)) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(ins_weekday_dur_behav_md)) |>
  mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
         Behavior = recode(Behavior, Transit = "T",
                           Maneuver = "M",
                           Total = "Total"),
         Weekday = factor(Weekday, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
         Weekday = recode(Weekday,
                          Sunday = "Su",
                          Monday = "M",
                          Tuesday = "T",
                          Wednesday = "W",
                          Thursday = "R",
                          Friday = "F",
                          Saturday = "Sa"))



#### Plot weekday counts ####

ggplot(data=ins_weekday_totals_md, 
       aes(x=Weekday, 
           y=count_weekday_ins_sm, 
           fill=Behavior))+
  facet_grid(rows=vars(Behavior),
             cols=vars(Site_ID), drop=F)+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Number of Discrete Vessels")+
  xlab("Day of Week")+
  ggtitle(expression('Weekday Vessel Count, P'['in']>='0.75'))+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=18),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text.y = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))


ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_SmallMed_weekday.png"), width=9, height=8,
       units="in", dpi=300)


##### Diel ####
# summarize hourly count by behavior
ins_diel_behav_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Begin_Hour_loc, Behavior) |>
  summarize(count_ins_sm = n())

# get totals and add to above
ins_diel_totals_md <- ins_smallmed_data |>
  group_by(Site_ID, Dep_ID, Begin_Hour_loc) |>
  summarize(count_ins_sm = n()) |>
  mutate(Behavior = "Total") |>
  ungroup() |>
  rbind(ungroup(ins_diel_behav_md)) |>
  mutate(Behavior = factor(Behavior, levels = c("Transit","Maneuver","Total")),
         Behavior = recode(Behavior, Transit = "T",
                           Maneuver = "M",
                           Total = "Total"))


#### Plot diel ####
ggplot(data=ins_diel_totals_md, 
       aes(x=Begin_Hour_loc, 
           y=count_ins_sm, fill=Behavior))+
  facet_grid(rows=vars(Behavior), cols=vars(Site_ID), drop=F)+
  # geom_col(data=night_hours, aes(x=night, y=night_val, alpha=0.6), width=1, fill="grey58")+
  geom_col(width=1, colour="black")+
  geom_hline(yintercept=0)+
  ylab("Number of Discrete Vessels")+
  xlab("Hour (AWST, UTC+8)")+
  ggtitle(expression('Hourly Vessel Count, P'['in']>= 0.75))+
  # scale_y_continuous(breaks=c(0, 1))+
  scale_x_continuous(breaks=c(0,6,12,18,23))+
  # scale_y_continuous(breaks=c(0,1,2,3), labels = c("","1","2","3"))+
  scale_fill_manual(limits=c("T","M","Total"),
                    values= c("T" = "#895c89", "M" = "#ffa156", "Total" = "grey75"))+
  theme(text=element_text(size=16),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.y = element_text(color = "white", face = "bold"),
        strip.text.x = element_text(color = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(title="Behavioral \nCategory"))


ggsave(paste0("outputs/", paste0(dep_list, collapse = "_"), "_Inside_SmallMed_diel.png"), width=9, height=8,
       units="in", dpi=300)



# Save summary data

# small vessels
write.csv(ins_daily_totals, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_Small_Duration_Sum.csv"))
write.csv(ins_weekday_totals, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_Small_Weekday_Sum.csv"))
write.csv(ins_diel_totals, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_Small_Diel_Sum.csv"))

# small-medium vessels
write.csv(ins_daily_totals_md, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_SmallMed_Duration_Sum.csv"))
write.csv(ins_weekday_totals_md, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_SmallMed_Weekday_Sum.csv"))
write.csv(ins_diel_totals_md, paste0("outputs/", paste0(dep_list, collapse = "_"),"_Ins_Small_DielMed_Sum.csv"))
