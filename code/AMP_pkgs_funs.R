#' ---------------------------------------------
#' 
#' 
#' Helper functions for AMP analysis
#' 
#' 
#' ---------------------------------------------

library(tcltk2)

Compile_Raven_selns <- function(site_id = character(), 
                                dep_id = character()){
  dir_seln <- tk_choose.dir(caption = "Select folder with selection tables (.txt)")
  seln_tables <- dir_seln |>
    # list all files with .txt extension
    list.files(pattern = ".txt") |>
    map_chr(~paste0(dir_seln,"\\",  .)) |>
    # use map to iterate read.delim() function over each file in the directory
    map(~read.delim(.)) |>
    # rename columns with funky character formatting
    map(~rename(.,"Begin_Time" = "Begin.Time..s.",
                "End_Time" = "End.Time..s.",
                "Low_Freq" = "Low.Freq..Hz.",
                "High_Freq" = "High.Freq..Hz.", 
                "Begin_Date" = "Begin.Date", 
                "Begin_Clock" = "Begin.Clock.Time",
                "End_Clock" = "End.Clock.Time", 
                "Delta_Time_s" = "Delta.Time..s.")) |>
    map(~mutate(., Begin_file_date = first(Begin_Date)))
  
  
  # Now we have selection tables as a list, but we want them all together in one dataframe
  all_selns <- do.call("rbind", seln_tables) |>
    # fix any T's that got converted to logical, then convert all to TRANSIT
    mutate(Behavior = gsub(pattern = "TRUE", replacement = "T", x = Behavior),
           Behavior = gsub(pattern = "T", replacement = "Transit", fixed=TRUE, x = Behavior),
           Behavior = gsub(pattern = "M", replacement = "Maneuver", fixed=TRUE, x = Behavior))
  
 
  return = all_selns
  
}

# Test
# Compile_Raven_selns(site_id = "SWS", dep_id = "test")



Compile_Raven_bio_selns <- function(site_id = character(), 
                                dep_id = character()){
  dir_seln <- tk_choose.dir(caption = "Select folder with selection tables (.txt)")
  seln_tables <- dir_seln |>
    # list all files with .txt extension
    list.files(pattern = ".txt") |>
    map_chr(~paste0(dir_seln,"\\",  .)) |>
    # use map to iterate read.delim() function over each file in the directory
    map(~read.delim(.)) |>
    # rename columns with funky character formatting
    map(~rename(.,"Begin_Time" = "Begin.Time..s.",
                "End_Time" = "End.Time..s.",
                "Low_Freq" = "Low.Freq..Hz.",
                "High_Freq" = "High.Freq..Hz.", 
                "Begin_Date" = "Begin.Date", 
                "Begin_Clock" = "Begin.Clock.Time",
                "End_Clock" = "End.Clock.Time", 
                "Delta_Time_s" = "Delta.Time..s.",
                "Call_type" = "Call.Type"))
  
  
  # Now we have selection tables as a list, but we want them all together in one dataframe
  all_selns <- do.call("rbind", seln_tables) 
  
  
  return = all_selns
  
}



Compile_ves_detections <- function(site_id = character()){
  dir_detect <- choose.dir(caption = paste0("Select folder with validated detections ", site_id))
  det_tables <- dir_detect |>
    # list all files with .csv extension
    list.files(pattern = ".csv") |>
    map_chr(~paste0(dir_detect,"\\",  .)) |>
    # use map to iterate read_csv() function over each file in the directory
    map(~read_csv(.) |>
          select(Detection, ISOStartTime, ISOEndTime, StartTime, EndTime,
                 start, end, Labels, `new labels`) |>
          # mutate to add deployment ID from filename 
          # use .* operator to subset after "_MA-RI_" and before "_5s_48Hz" in filename
          mutate(start_date_ISO = as_date(ISOStartTime),
                 Dep_ID = sub(".*_MA-RI_","", .x),
                 Dep_ID = sub("_5s_48Hz.*","",Dep_ID))) 
  
  
  # Now we have selection tables as a list, but we want them all together in one dataframe
  all_dets <- do.call("rbind", det_tables) |>
    # add new column for site
    mutate(SITE = site_id)
  
  
  return = all_dets
  
}



getDeploymentInfo <- function(){
  
  ##### Deployment info ###
  site_id <- dlg_input(message = "Site ID, e.g. 'TRE'")$res
  dep_id <- dlg_input(message = "Deployment ID, YYYYMM")$res
  
  # Start and end dates of deployment
  start_dep_date <- as_date(dlg_input(message = "Start date: YYYY-MM-DD")$res)
  end_dep_date <- as_date(dlg_input(message = "End date: YYYY-MM-DD")$res)
  
  # Time zone of sound files
  tz_files <- dlg_list(title = "Original file TZ",choices = c(grep("Etc/", OlsonNames(), value=TRUE)))$res
  # Local time zone
  tz_local <- dlg_list(title = "TZ for Figures",choices = c(grep("Etc/", OlsonNames(), value=TRUE)))$res
  
  return = list(site_id = site_id, 
                dep_id = dep_id, 
                start_dep_date = start_dep_date, 
                end_dep_date = end_dep_date, 
                tz_files = tz_files, 
                tz_local = tz_local)
  
}




inside_tables_to_hp <- function(ins_table, 
                                site_id = character()){
  # ves_og <- read_csv(choose.files(caption = "Choose Ship Detection Notes for given dep")) |>
  ves_og <- ins_table |>
    # change Behavior to factor and explicitly set all behaviors as levels
    mutate(Behavior = factor(Behavior, levels = c("CPA","CPAM","TA","TAM","TB","MANEUVER","TRANSIT")))
  
  
  
  #### Create Hourly Presence Table ####
  
  # Count instances of each behavior per date-hour
  hr_tally <- ves_og |>
    mutate(Begin_Date = as_date(Begin_Date, format = "%m/%d/%Y"))|>
    group_by(Begin_Date, Behavior, Begin_hour, SiteID) |>
    count() |>
    pivot_wider(names_from = Behavior,
                values_from = n,
                names_expand = TRUE,
                names_prefix = "ins_",
                values_fill = 0) |> # expand to include a column for all possible levels
    mutate(trans_inside = rowSums(across(c(ins_TB, ins_CPA, ins_TA, ins_TRANSIT))),
           man_inside = rowSums(across(c(ins_TAM, ins_CPAM, ins_MANEUVER))),
           total_inside = rowSums(across(c(trans_inside, man_inside))))
  
  # create a new df with all hours for the whole dataset
  date_range_dep <- seq.Date(from = min(hr_tally$Begin_Date), 
                             to = max(hr_tally$Begin_Date), by = "day") |>
    crossing(seq(0,23,1))
  
  # rename columns
  names(date_range_dep) <- c("Begin_Date","Begin_hour")
  
  # join 2 data frames together to add behavior tally
  hourly_pres <- date_range_dep |>
    left_join(hr_tally, by = c("Begin_Date","Begin_hour")) |>
    replace_na(list(Ves_counts_inside = 0,
                    SiteID = site_id))
  
  return = hourly_pres
}