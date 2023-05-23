"-----------Get bootstrapped sample------------"
# Function to resample data at transect-level (i.e., `Sample.Label`) 
# Adapted from `bootdht_resample_data` from package `Distance`
# Specify effort level (`transect_effort`)
# sample from unique `Sample.Label` with or without replacement
resample_data <- function(bootdat, transect_effort = 400, replacement = T){
  
  sample_label = "Sample.Label"
  obs_label = "object"
  
  # sample from unique transects (`Sample.Label`) with or without replacement
  transects <- unique(bootdat[["Sample.Label"]])
  transect_samps <- sample(transects, size = transect_effort, replace = replacement)
  
  # remap sample labels
  remap <- make.unique(transect_samps)
  
  # now populate each list element with the corresponding observations
  # or empty transects
  resampled_dat <- list()
  
  for(ii in 1:length(transect_samps)){
      resampled_dat[[ii]] <- bootdat[bootdat[[sample_label]] == transect_samps[ii], ]
      resampled_dat[[ii]][[sample_label]] <- remap[ii]
  }
  
  # concatenate list elements to data.frame
  rr <- do.call(rbind.data.frame, resampled_dat)
  
  # reset the object IDs to be unique (where there are observations) -- WHY???
  rr[[obs_label]][is.na(rr[["distance"]])] <- NA
  rr[[obs_label]][!is.na(rr[["distance"]])] <- 1:length(rr[[obs_label]][!is.na(rr[["distance"]])])
  
  return(rr)
}

"-----------Get bootstrapped sample estimate------------"
# Function that resamples data (with `resample_data`) 
# and applies a distance analysis to obtain density point and precision estimates.
# Here, I only analyse the resampled data with a half-normal key function, 
# which was the best fit model for the original/full data.
get_boot_est <- function(bootdat, transect_effort = 400, replacement = T, 
                         right_w = 125, bin_int = 25,
                         conversion = convert_units("meter", "meter", "square kilometer")){
  
  # boostrap sample
  sampdat <- resample_data(bootdat, transect_effort = transect_effort, replacement = replacement)
  
  # distance analysis
  hn_samp <- ds(sampdat, 
                truncation = list(left=0, right=right_w), 
                convert_units = conversion, cutpoints = seq(0,right_w, bin_int), 
                nadj = 0, key = "hn", adjustment = "cos")
  
  # get density point estimate and cv
  dens_samp <- hn_samp$dht$individuals$D
  
  output <- data.frame(Effort = transect_effort,
                       Estimate = dens_samp[dens_samp$Label == "Total", "Estimate"],
                       cv = dens_samp[dens_samp$Label == "Total", "cv"])
  
  return(output)
}
