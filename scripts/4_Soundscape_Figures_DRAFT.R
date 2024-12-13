#' -------------------------------------------------------------------
#' 
#' 
#' Create plots of biological presence in AMP
#' Based on scripts used to create figures in McCordic et al. 2021
#' 
#' 
#' -------------------------------------------------------------------


#### Load in packages ####
# NOAA-approved tidyverse
tidyverse_short<-c("broom","cli","crayon","dbplyr","dplyr","dtplyr","forcats","ggplot2","googledrive","googlesheets4","hms","httr","jsonlite","lubridate","magrittr","modelr","pillar","purrr","readr","readxl","reprex","rlang","rstudioapi","rvest","stringr","tibble","tidyr","xml2") 
lapply(tidyverse_short, require, character.only = TRUE) 
library(svDialogs)
library(lunar)


#############################################################
#### User-defined variables - UPDATE FOR EACH DEPLOYMENT ####

##### Deployment info ###
site_id <- "MRE"
dep_id <- "D1"

# Start and end dates of study period
start_dep_date <- as_date("2020-02-08")
end_dep_date <- as_date("2020-05-03")


# Time zone of sound files
tz_files <- dlg_list(title = "Original file TZ",choices = grep("GMT", OlsonNames(), value=TRUE))$res
# Local time zone
tz_local <- dlg_list(title = "TZ for Figures (+/- are backwards for GMT)",choices = grep("GMT", OlsonNames(), value=TRUE))$res

#############################################################

#### LOAD DATA ####
#### Load hourly presence CSVs ####

# pull from inputs folder -- might be a better workflow for this???

# Biology hourly pres sheet
hp_og_bio <- read_csv(choose.files(caption = "Choose Biology Hourly Presence sheet"))

# Vessels hourly pres sheet
hp_og_ves <- read_csv(choose.files(caption = "Choose Vessel Hourly Presence sheet")) |>
  mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))


# Join both hourly presence tables

hp_og_all <- hp_og_bio |>
  left_join(hp_og_ves, by = c("Begin_Date", "Begin_Hour")) |>
  # add columns to summarize presence and add more time info
  mutate(
    # sum across vessel behavior columns to get total number
    Total_Vessels = rowSums(across(c(Transit, Maneuver))),
    
    # create 0/1 column for presence of sources
    ves_pres = ifelse(Total_Vessels == 0, 0, 1),
    fish_c_pres = ifelse(fish_c > 0, 1, 0), 
    fish_p_pres = ifelse(fish_p > 0, 1,0),
    bal_pres = ifelse(bal_uk > 0, 1,0),
    uk_uk = ifelse(uk_uk > 0,1,0),
    
    # pull hour as time object along with date
    Hr_time = paste0(Begin_Date," ", Begin_Hour, ":00"),
    # get components of date-time object and assign time zone
    Hr_UTC = parse_date_time(Hr_time, "ymd H:M", tz = tz_files),
    # change to local time zone
    Hr_local = with_tz(Hr_UTC, tz = tz_local),
    # pull out new hour and date in local time
    Begin_Hour_loc = as.numeric(hour(Hr_local)),
    Begin_Date_loc = date(Hr_local),
    
    # add weekdays, weeks, months, and day-months (without year) to subset later
    Weekday = weekdays(Begin_Date_loc),
    Week = week(Begin_Date_loc),
    Month = month(Begin_Date_loc),
    Day_month = day(Begin_Date_loc)
  )


#### Subset data frame to 3-day subsample to reflect analyzed dates ####
 hp_subset_all <- hp_og_all #|>
  # want second half of original date and first half of next day to 
  # account for 8h time zone shift
  # This is a super awkward way to do this so will need to figure out a better 
  # log-term solution
  # filter(Begin_Date_loc %in% seq(ymd("2022-01-19"), to = ymd("2022-04-11"),
  #                          by = "3 day") & Begin_Hour_loc %in% seq(12,23,1) | 
  #          Begin_Date_loc %in% seq(ymd("2022-01-20"), to = ymd("2022-04-11"),
  #                                  by = "3 day") & Begin_Hour_loc %in% seq(0,11,1))



#### Summarize full df to get diel df's for plotting ####

## *** There is probably a better way to do this than what's below--could maybe pivot whole
##      table first and then group by source?  Something to think about for script we'll use in long term

#### Use percent hours per deployment hours to allow comparison across deployments in the future
# --> accounts for variability in deployment length

# Percent values represent proportion of times each source is present in a given hour
# divided by the number of times each hour occurs in the dataset

# Vessels
Ves_diel_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Hour_loc, ves_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  group_by(Begin_Hour_loc, ves_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  filter(ves_pres > 0) |>
  mutate(Source = "Vessel")


# Fish choruses
Fish_ch_diel_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Hour_loc, fish_c_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  # complete(Begin_Hour_loc, ves_yn,
  #          fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(Begin_Hour_loc, fish_c_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  filter(fish_c_pres > 0) |>
  mutate(Source = "Fish chorus")



# Fish pulses
Fish_p_diel_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Hour_loc, fish_p_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  # complete(Begin_Hour_loc, ves_yn,
  #          fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(Begin_Hour_loc, fish_p_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  filter(fish_p_pres > 0) |>
  mutate(Source = "Fish pulse")


# Baleen whales
Bal_diel_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Hour_loc, bal_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  group_by(Begin_Hour_loc, bal_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_hour") |>
  filter(bal_pres > 0) |>
  mutate(Source = "Pygmy blue whale")



#### Merge all diel tables together for plotting ####

diel_perc_all <- Fish_ch_diel_perc |>
  merge(Fish_p_diel_perc, all = TRUE) |>
  merge(Ves_diel_perc, all = TRUE) |>
  merge(Bal_diel_perc, all = TRUE) |>
  mutate(Source = factor(Source, levels = c("Fish pulse",
                                            "Fish chorus",
                                            "Pygmy blue whale",
                                            "Vessel")))

#### Plot diel patterns for all sources ####
ggplot(data=diel_perc_all, # only plot 'yes' proportions
       aes(x=Begin_Hour_loc, y = Perc_per_hour, fill = Perc_per_hour,))+
  geom_bar(stat="identity") +
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\nTotal deployment Hours") +
  ylim(0,1)+
  xlab("Hour")+
  scale_x_continuous(breaks = seq(0,24,3),
                     minor_breaks = seq(0,24,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  coord_polar() +
  facet_wrap(facets = vars(Source),
             nrow = 1)

#### Save diel combined plot ####
ggsave("outputs/SWS_D1_diel_combined_1row.jpg", device = "jpeg",
        width=12, height=4, units="in", dpi=600)


#### Diel plots for individual sources ####

# Diel Polar Plot - Vessels
ggplot(data=Ves_diel_perc |> filter(ves_pres==1), # only plot 'yes' proportions
       aes(x=Begin_Hour_loc, y = Perc_per_hour, fill = Perc_per_hour,))+
  geom_bar(stat="identity")+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\nTotal deployment Hours") +
  ylim(0,1)+
  xlab("Hour")+
  scale_x_continuous(breaks = seq(0,24,3),
                     minor_breaks = seq(0,24,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  coord_polar()

#### Save individual vessel plot
ggsave("outputs/SWS_D1_diel_vessel.jpg", device = "jpeg",
       width=10, height=10, units="in", dpi=600)

# Diel Polar Plot - Fish choruses
ggplot(data=Fish_ch_diel_perc |> filter(fish_c_pres > 0), # only plot presence
       aes(x=Begin_Hour_loc, y = Perc_per_hour, fill = Perc_per_hour))+
  geom_bar(stat="identity")+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\nTotal deployment Hours") +
  ylim(0,1)+
  xlab("Hour")+
  scale_x_continuous(breaks = seq(0,24,3),
                     minor_breaks = seq(0,24,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  coord_polar()

#### Save individual fish chorus plot
ggsave("outputs/SWS_D1_diel_fishchorus.jpg", device = "jpeg",
       width=10, height=10, units="in", dpi=600)


# Diel Polar Plot - Fish pulses
ggplot(data=Fish_p_diel_perc |> filter(fish_p_pres > 0), # only plot presence
       aes(x=Begin_Hour_loc, y = Perc_per_hour, fill = Perc_per_hour))+
  geom_bar(stat="identity")+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\nTotal deployment Hours") +
  ylim(0,1)+
  xlab("Hour")+
  scale_x_continuous(breaks = seq(0,24,3),
                     minor_breaks = seq(0,24,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  coord_polar()

#### Save individual fish pulse plot
ggsave("outputs/SWS_D1_diel_fishpulse.jpg", device = "jpeg",
       width=10, height=10, units="in", dpi=600)



# Diel Polar Plot - Baleen (pygmy blues)
ggplot(data=Bal_diel_perc |> filter(bal_pres > 0), # only plot presence
       aes(x=Begin_Hour_loc, y = Perc_per_hour, fill = Perc_per_hour))+
  geom_bar(stat="identity")+
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\nTotal deployment Hours") +
  # ylim(0,1)+
  xlab("Hour")+
  scale_x_continuous(breaks=seq(0,23,3), 
                     minor_breaks = seq(0,23,1))+
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  coord_polar()

#### Save individual pygmy blue whale plot
ggsave("outputs/SWS_D1_diel_pygmyblue.jpg", device = "jpeg",
       width=10, height=10, units="in", dpi=600)

#### Manipulate data for plots by date ####

# summarize by date to look at percent hours per 24h period
# of 24h recording time analyzed, how many hours was each source present? 


# Vessels
Ves_daily_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Date_loc, ves_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  group_by(Begin_Date_loc, ves_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_day") |>
  filter(ves_pres > 0) |>
  mutate(Source = "Vessel")

# Fish choruses
Fish_ch_daily_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Date_loc, fish_c_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  group_by(Begin_Date_loc, fish_c_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_day") |>
  filter(fish_c_pres > 0) |>
  mutate(Source = "Fish chorus")



# Fish pulses
Fish_p_daily_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Date_loc, fish_p_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  # complete(Begin_Hour_loc, ves_yn,
  #          fill = list(ves_yn = "Y", freq = 0)) |>
  group_by(Begin_Date_loc, fish_p_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_day") |>
  filter(fish_p_pres > 0) |>
  mutate(Source = "Fish pulse")


# Baleen whales
Bal_daily_perc <- hp_subset_all |>
  # group by Begin Hour to collapse into hours
  group_by(Begin_Date_loc, bal_pres) |>
  # get total number of hours for Y and N groups per each hour per deployment
  summarize(n_hours = n())|>
  mutate(freq = n_hours/sum(n_hours)) |>
  ungroup() |>
  group_by(Begin_Date_loc, bal_pres) |>
  # reshape for easier plotting
  pivot_longer(cols = c("freq"),
               values_to = "Perc_per_day") |>
  filter(bal_pres > 0) |>
  mutate(Source = "Pygmy blue whale")



#### Merge all daily tables together for plotting ####

daily_perc_all <- Fish_ch_daily_perc |>
  merge(Fish_p_daily_perc, all = TRUE) |>
  merge(Ves_daily_perc, all = TRUE) |>
  merge(Bal_daily_perc, all = TRUE) |>
  mutate(Source = factor(Source, levels = c("Fish pulse",
                                            "Fish chorus",
                                            "Pygmy blue whale",
                                            "Vessel")),
         Lunar_ill = lunar.illumination(Begin_Date_loc, shift = 8))

# Reshape data to look at correlations

# from daily percent table
daily_perc_num <- daily_perc_all |>
  # select only the columns we need to keep daily percentages
  select(Begin_Date_loc, Perc_per_day, Source) |>
  # pivot wide to get Source as columns
  pivot_wider(names_from = Source, 
              values_from = Perc_per_day,
              values_fill = 0) |>
  mutate(Lunar_ill = lunar.illumination(Begin_Date_loc, shift = 8))

# plot preliminary scatter to look at potential correlations
pairs(daily_perc_num)

# try simple scatterplot
ggplot(data = daily_perc_num, 
       aes(x = `Fish pulse`,
           y = Lunar_ill,
           color = Begin_Date_loc)) +
  geom_point()


#### Plot daily patterns for all sources ####
ggplot(data=daily_perc_all, # only plot 'yes' proportions
       aes(x=Begin_Date_loc, y = Perc_per_day, fill = Perc_per_day,))+
  geom_bar(stat="identity") +
 # geom_line(aes(y = Lunar_ill,
  #              color = "red")) +
  scale_fill_viridis_c(begin = 0, end = 1, limits = c(0,1), breaks = c(0,0.25,0.5,0.75,1), option = "D")+
  ylab("N hours present /\n Total deployment Hours") +
  ylim(0,1)+
  xlab("Date")+
  scale_x_date(minor_breaks = "3 days") +
  theme(text=element_text(size=20),
        strip.background = element_rect(color = "white", fill = "black"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        strip.text.y = element_text(colour = "white", face = "bold"),
        strip.text.y.left = element_text(angle = 0),
        plot.title = element_text(hjust = 0.5))+
  #R turns the proportion values into bins in the legend when I change the title, so I'm removing it instead
  theme(legend.title=element_blank()) +
  # coord_polar() +
  facet_wrap(facets = vars(Source),
             nrow = 1)

#### Save daily combined plot ####
ggsave("outputs/SWS_D1_daily_combined_1row.jpg", device = "jpeg",
       width=12, height=4, units="in", dpi=600)
