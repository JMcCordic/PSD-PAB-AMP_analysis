# load packages
library(tidyverse)
library(openxlsx)
library(svDialogs)
library(tcltk2)
library(broom)

# source("code/AMP_pkgs_funs.R")

# Collect user-defined inputs ------------------------------------------------------

#### Deployment info ####
dep_id <- dlgInput(message = "Site name, deployment")$res
depth_m <- as.numeric(dlgInput(message = "Hydrophone depth in meters")$res)

#### Load data ####

# # load in data table with reviewed peak freq/ peak levs for unknown vessels
# data_og <- read_csv(tk_choose.files(caption=paste0("Select the file with unknown vessels peak RLs: ", dep_id)))
# data <- subset(data_og, data_og$used==1)

# Load reviewed PF/distance table from calibration vessel
calib_data_og <- read_csv(tk_choose.files(caption=paste0("Select the file with calibration track RLs: ", dep_id)))
calib_data <- subset(calib_data_og, calib_data_og$used==1) |>
  # filter(PeakFreq > 178 & PeakFreq < 282)|>
  mutate(Distance_km = Distance/1000,
         slant_range_m = sqrt(Distance^2 + depth_m^2))

# Transmission loss model -------------------------------------------------

# Use nonlinear least squares regression to model transmission loss function
tl_model <- nls(PeakLev ~ SL - loss_geo*log10(slant_range_m) - loss_abs*slant_range_m, 
                algorithm = "port",
                upper = c(200, 30, 0.1),
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
           y = PeakLev)) +
  geom_point(aes(color = PeakFreq)) + 
  scale_color_viridis_c()+
  stat_smooth(method = "nls", 
              formula = y ~ a - b*log10(x) - c*x,
              se = FALSE,
              method.args = list(start = c(a = 150,
                                              b = 15,
                                              c = 0),
                                 algorithm = "port",
                                 upper = c(200, 20, 0.1),
                                 lower = c(0, 0, 0)
                                 ),
              fullrange = TRUE
              ) +
  annotate("text", x = 300, y = 110, label = tl_label)+
  labs(x = "Distance (m)", y = expression(paste("Peak Level, dB re 1", mu, "Pa")))+
  theme_bw(base_size = 14)

ggsave(ggsave(paste0("output/", dep_id,"TL_model.png"), width=8, height=8,
              units="in", dpi=300))


# Use broom package to tidy up model summary into dataframe
tl_tidy <- tidy(tl_model) |>
  mutate(Dep_ID = dep_id,
         depth_m = depth_m)

# save model coefficients into output
write_csv(tl_tidy, paste0("output/",dep_id,"TL_model_coeff.csv"))


