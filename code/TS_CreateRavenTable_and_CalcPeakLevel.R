# Load helper functions and pop-up dialogue library
source('code/AMP_pkgs_funs.R')
library(svDialogs)

# Part 1 ArcGIS output -> Fix timestamps for Raven -> Write Raven table ####
# Input requires ArcGIS Excel file and Wav file for those selections
arcFile <- choose.files(caption='Select ArcGIS file (.xls)')
wavFile <- choose.files(caption='Select recording file (.wav)')

# Default Raven .txt file naming matches the input ArcGIS table, store in output folder
ravenOut <- gsub('xlsx?$', 'txt', arcFile)
ravenOut <-  file.path('output', basename(ravenOut))

# Timezones for both recording & GPS (usually UTC) to make sure times match
wavTimezone <- dlg_list(title='Recording File Timezone', choices=c(paste0('UTC+', 12:1), 'UTC', paste0('UTC-', 1:12)))$res
gpsTimezone <- dlg_list(title='GPS Data Timezone', choices=c(paste0('UTC+', 12:1), 'UTC', paste0('UTC-', 1:12)))$res

# This writes the raven table, "freq" and "duration" can be adjusted here if desired
# from the defaults of 250/5
ravFromArc <- arcToRaven(arc=arcFile, 
                         wav=wavFile, 
                         wavTz=wavTimezone, 
                         gpsTz=gpsTimezone,
                         freq=250, 
                         duration=5, 
                         file=ravenOut)

# Part 2 Exported Raven clips -> Calculate Peak Level -> write CSV ####
# Input requires folder of clips, calibration value, and allowed frequency range for peak finding
clipDirectory <- choose.dir(caption='Select folder with Raven soundclips')
calibration <- as.numeric(dlg_input(message='Enter total calibration (large negative number)')$res)
freqMin <- as.numeric(dlg_input(message='Enter lower frequency bound (Hz)')$res)
freqMax <- as.numeric(dlg_input(message='Enter higher frequency bound (Hz)')$res)
peakLevOut <- dlg_input(message='Enter name of output file (.csv)')$res
peakLevOut <- file.path('output', peakLevOut)

# This takes a bit, so will show a progress bar. Output is written to the CSV file
peakLevData <- calculatePeakLev(dir=clipDirectory,
                                cal=calibration,
                                freqMin=freqMin,
                                freqMax=freqMax,
                                file=peakLevOut)
