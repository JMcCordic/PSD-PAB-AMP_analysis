# load packages
library(tidyverse)
library(openxlsx)
library(svDialogs)
library(tcltk2)

source("scripts/AMP_pkgs_funs.R")



# R version of Plausible SLs Matlab script

# Last updated J. McCordic 2024-11-20




# Collect user-defined inputs ------------------------------------------------------

# --> LATER TO-DO: restructure this to accommodate multiple deployments per park?

#### Deployment info ####
dep_id <- dlgInput(message = "Site name, deployment")$res
depth_m <- as.numeric(dlgInput(message = "Hydrophone depth in meters")$res)

#### Load data ####
# load in data table with reviewed peak freq/ peak levs for unknown vessels
data_og <- read_csv(tk_choose.files(caption=paste0("Select the file with unknown vessels peak RLs: ", dep_id)))
data <- subset(data_og, data_og$used==1)

# Load reviewed PF/distance table from calibration vessel
calib_data_og <- read_csv(tk_choose.files(caption=paste0("Select the file with calibration track RLs: ", dep_id)))
calib_data <- subset(calib_data_og, calib_data_og$used==1) |>
  mutate(Distance_km = Distance/1000,
         slant_range_m = sqrt(Distance^2 + depth_m^2))

# Transmission loss model -------------------------------------------------

# Use nonlinear least squares regression to model transmission loss function
tl_model <- nls(PeakLev ~ SL - loss_geo*log10(slant_range_m) - loss_abs*slant_range_m, 
                algorithm = "port",
                upper = c(200, 20, 0.1),
                lower = c(0, 0, 0),
                data = calib_data,
                start = list(SL = 0,
                             loss_geo = 0,
                             loss_abs = 0))

summary(tl_model)

# TL equation params - change each time ***make dialog box?***
SL <- round(coef(tl_model)[1], digits = 2)
loss_geo <- round(coef(tl_model)[2], digits = 2)
loss_abs <- round(coef(tl_model)[3], digits = 3)


# plot Peak Level as function of distance

# generate plot label text
tl_label <- paste0("RL = ", SL, " - ", 
                   loss_geo, "log10(r)", " - ", loss_abs,"r")


ggplot(data = calib_data, 
       aes(x = Distance, 
           y = PeakLev, color = PeakFreq)) +
  geom_point() + 
  scale_color_viridis_c()+
  stat_smooth(method = "nls", 
              formula = y ~ a - b*log10(x) + c*x,
              se = FALSE,
              method.args = list(start = c(a = 150,
                                              b = 15,
                                              c = 0),
                                 algorithm = "port",
                                 upper = c(200, 20, 0.001),
                                 lower = c(0, 0, 0)
                                 )
              ) +
  annotate("text", x = 300, y = 110, label = tl_label)+
  labs(x = "Distance (m)", y = expression(paste("Peak Level, dB re 1", mu, "Pa")))+
  theme_bw(base_size = 14)

ggsave(ggsave(paste0("outputs/", dep_id,"TL_model.png"), width=8, height=8,
              units="in", dpi=300))




# average distance between ST and park boundary
park_dist <- as.numeric(dlgInput(message = paste0("Avg meters to park boundary (whole number): ", dep_id))$res)

# ambent metrics (NL50) to get detection distance
# loading this takes a little while
tol_data <- read.csv(tk_choose.files(caption=paste0("Select TOL output: ",dep_id)), header = TRUE)




# Calculate detection range -----------------------------------------------

# get quantiles for TOL with median peak frequency

#***figure out a way to get median PF internally... maybe load all behavs at once and then run through them?***

median(data$PeakFreq)

# old tol output from matlab
# qt_tol <- quantile(tol_data$X100, c(0.25, 0.50, 0.75))

# tol output from HMD data via PAMScapes
qt_tol <- quantile(tol_data$TOL_250, c(0.25, 0.50, 0.75), na.rm = TRUE)


#use 50th percentile as NL
nl50 <- qt_tol[2]

# get range of x values to plot/test
xranges <- seq(1,100000,10)

# plug in values from TL model, set signal excess to 5dB
det_ranges <- loss_geo*(log10(xranges)) + loss_abs*xranges + 5

# plot to check
plot(xranges[1:1000],det_ranges[1:1000])

sml_r <- xranges[min(which((det_ranges>round(150 - nl50, digits = 1))==TRUE))]
med_r <- xranges[min(which((det_ranges>round(170 - nl50, digits = 1))==TRUE))]
lrg_r <- xranges[min(which((det_ranges>round(180 - nl50, digits = 1))==TRUE))]


# Calculate plausible SLs -------------------------------------------------


# use detection range for med vessels as max range
# set up sequence of distances from 1 to max range for medium vessel
prange <- seq(1,med_r,10)

TL <- loss_geo*(log10(prange)) + loss_abs*prange

data_sl <- NULL
# for each vessel selection
for (i in 1:nrow(data)){
  # get RL
  peakRL <- data$PeakLev[i]
  #   # add RL to vector of TL per distance to get SL at each distance
  SL_plaus <- TL + peakRL
  #   #remove implausible SLs (below 125 or over 180dB)
  SL_plaus[which(SL_plaus<125|SL_plaus>180)]<-NA
  data_sl <- rbind(data_sl, SL_plaus)
}

data_sl <- as.data.frame(data_sl)
rownames(data_sl) <- data$Filename


# Range plots -------------------------------------------------------------


# make tricolor plausible SL plots per vessel
data_plot <- as.data.frame(t(data_sl))
data_plot$distance <- prange

#make list to save plots
plot_list <- list()

#create plots
for (j in 1:nrow(data)){
  sl_plot <- ggplot(data_plot, aes(x=distance, y = data_plot[,j]))+
    geom_rect(xmin=0, xmax=max(data_plot$distance), ymin=125, ymax = 150, fill="skyblue")+
    geom_rect(xmin=0, xmax=max(data_plot$distance), ymin=150, ymax = 170, fill="cadetblue2")+
    geom_rect(xmin=0, xmax=max(data_plot$distance), ymin=170, ymax = 180, fill="deepskyblue4")+
    geom_point()+
    geom_vline(aes(xintercept=park_dist))+
    ylim(125,180)+
    ylab("SL")+
    xlab("Distance (m)")+
    ggtitle(paste0("Plausible SLs: ",str_sub(data$Filename[j], 11,-5),", ",as.character(data$Behav[j])))
  
  plot_list[[j]] <- sl_plot
  
}

# save plots

fol_name <- tk_choose.dir("Select folder to save range figures")
for(j in 1:nrow(data)){
  file_name <- paste0(fol_name,"/","SL_vs_dist_", str_sub(data$Filename[j], 6,-5),"_",as.character(data$Behav[j]),".png")
  png(file_name, width=5, height=5,units="in", res = 300)
  print(plot_list[[j]])
  dev.off()
}


# Inside park probs -------------------------------------------------------


# set up new numeric columns

data$ins_sm <- 0
data$out_sm <- 0
data$ins_med <- 0
data$out_med <- 0
data$ins_lg <- 0
data$out_lg <- 0

# number of inside points = how many points between 0 and max listening range
# are inside the park boundaries
ins_points <- length(which(prange < park_dist))

# how many are outside
out_points <- length(prange) - ins_points

# create new df that's only plausible SLs inside park
iSL <- data_sl[,1:ins_points]
# create new df that's only plausible SLs outside park

oSL <- data_sl[,(ins_points+1):ncol(data_sl)]



# for each vessel
for (i in 1:nrow(data)){
  # get total number of points used per size class
  # get number of points inside park for small size class from iSL
  
  sm_indx <- which(iSL[i,] <= 150)
  # if no SLs corresponding to that size, set inside points to 0
  if(length(sm_indx)==0){
    data$ins_sm[i] <- 0
  } else {
    # otherwise, set number of points to length of SLs inside
    data$ins_sm[i] <- length(sm_indx)
  }
  
  med_indx <- which(iSL[i,] > 150 & iSL[i,] <= 170)
  # if no SLs corresponding to that size, set inside points to 0
  if(length(med_indx)==0){
    data$ins_med[i] <- 0
  } else {
    # otherwise, set number of points to length of SLs inside
    data$ins_med[i] <- length(med_indx)
  }
  
  
  lg_indx <- which(iSL[i,] > 170)
  if(length(lg_indx)==0){
    data$ins_lg[i] <- 0
  } else {
    # otherwise, set number of points to length of SLs inside
    data$ins_lg[i] <- length(lg_indx)
  }
  
  # IF there are outside points at all, repeat for oSL (outside points)
  if(class(oSL) != "numeric") {
    
    sm_odx <- which(oSL[i,] <= 150)
    # if no SLs corresponding to that size, set outside points to 0
    if(length(sm_odx)==0){
      data$out_sm[i] <- 0
    } else {
      # otherwise, set number of points to length of SLs outside
      data$out_sm[i] <- length(sm_odx)
    }
    
    med_odx <- which(oSL[i,] > 150 & oSL[i,] <= 170)
    # if no SLs corresponding to that size, set outside points to 0
    if(length(med_odx)==0){
      data$out_med[i] <- 0
    } else {
      # otherwise, set number of points to length of SLs outside
      data$out_med[i] <- length(med_odx)
    }
    
    
    lg_odx <- which(oSL[i,] > 170)
    if(length(lg_odx)==0){
      data$out_lg[i] <- 0
    } else {
      # otherwise, set number of points to length of SLs inside
      data$out_lg[i] <- length(lg_odx)
    }
    
  } else {
    # if there are no prange points outside park boundaries
    data$out_lg <- 0
    data$out_med <- 0
    data$out_sm <- 0
  }
  
}

data$pins_sm <- data$ins_sm/(data$ins_sm+data$out_sm)
data$pins_med <- data$ins_med/(data$ins_med+data$out_med)
data$pins_lg <- data$ins_lg/(data$ins_lg+data$out_lg)
data$pins_ovrll <- (data$ins_sm + data$ins_med) / (data$ins_sm+data$out_sm+data$ins_med+data$out_med)

write.csv(data, file = paste0(tk_choose.dir(caption="Select folder to save output table"), "/",dep_id, "_inside-outside_park.csv"))



#### Generate high probability tables ####

# Assign deployment info
# Since server folders are in a standard structure, use parent folder to get list of all deployments
dep_names <- list.dirs(tk_choose.dir(caption = "Select parent dir for all deployment folders"), recursive = FALSE, full.names = FALSE)

# select the deployment(s) to be plotted
dep_list <- dlg_list(title = "Select deployments to plot", choices = dep_names, multiple = TRUE)$res

# apply getDeploymentInfo() from AMP_pkgs_funs.R to each deployment
#   prompts user for site name, start/end date, and time zones
dep_info <- dep_list |>
  map(~getDeploymentInfo()) |>
  set_names(dep_list)


# load compiled selections
selns_og <- dep_info |>
  map(~read_delim(choose.files(caption = "Choose complied seln table .txt")))|>
  # use imap() to get info based on index of each iteration
  # in this case, we want the name of the list element, designated as ".y"
  imap(~mutate(., Dep_ID = .y)) 


selns_data <- selns_og |>
  map(~rename(., "Dep" = "Dep_ID")) |>
  map2(.y = dep_info, 
       ~mutate(.x, 
               Begin_Date = ymd(Begin_Date),
               Begin_file_date = ymd(Begin_file_date),
               Site_ID = {.y}$site_id, 
               Dep = {.y}$dep_id,
               Behavior = gsub(pattern = "CPAManeuver", replacement = "Maneuver", x = Behavior),
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




highprob_ins_small <- data |>
  filter(pins_sm >= 0.75) |>
  mutate(begin_file_date = ymd(str_sub(Date, 1,6)),
         seln_num = as.numeric(gsub(pattern = "S", replacement = "", x = Selection)))|>
  left_join(selns_data, 
            by = c("seln_num" = "Selection", 
                   "begin_file_date" = "Begin_file_date"))

highprob_ins_smmed <- highprob_ins_small |>
  filter(pins_ovrll >= 0.75)
  
write_csv(highprob_ins_small, paste0("outputs/", dep_id, "HighProb_Ins_Small.csv"))
write_csv(highprob_ins_smmed, paste0("outputs/", dep_id, "HighProb_Ins_SmallMed.csv"))

  




