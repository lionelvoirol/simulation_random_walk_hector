#---------------------------- Vanilla simulation of random walk and estimation with Hector


# ----------------- Clean ws
rm(list=ls())


# --------------------------- import libraries
library(progress)


# ---------------------------------- Define functions 

write_ts_mom = function(y, t, filename, format = "mom") {
  
  if (substr(filename, nchar(filename)-3, nchar(filename)) != ".mom") {
    stop("filename must end with '.mom'")
  }
  
  headers = c(
    sprintf("# sampling period %.6f", 1)
  )
  con = file(filename, "w")
  writeLines(headers, con)
  
  write.table(
    cbind(
      t,
      y
    ),
    file = con, row.names = FALSE, col.names = FALSE, append = TRUE
  )
  
  close(con)
  
  
}


infuse_estimate_trend_tmp = function(data_file,
                                     working_folder,
                                     output_file,
                                     n_seasonal=0,
                                     mdl= "ar1+rw+wn"){
  if(mdl=="ar1+wn"){
    estimate_trend_tmpl = "DataFile              data_file_value\nDataDirectory         data_directory_value\nOutputFile            output_file_value\ninterpolate           no\nPhysicalUnit          mm\nScaleFactor           1.0\nNoiseModels           White ARMA\nAR_p                         1\nMA_q                         0\nseasonalsignal        seasonal_signal_value\nhalfseasonalsignal    half_seasonal_signal_value\nestimateoffsets       no\nestimatepostseismic   no\nestimateslowslipevent no\nRandomiseFirstGuess   no\nTimeNoiseStart        1\nDegreePolynomial      1\nLikelihoodMethod      AmmarGrag\nJSON                  yes\n"
  }else if(mdl=="ar1+rw+wn"){
    estimate_trend_tmpl = "DataFile              data_file_value\nDataDirectory         data_directory_value\nOutputFile            output_file_value\ninterpolate           no\nPhysicalUnit          mm\nScaleFactor           1.0\nNoiseModels           White ARMA RandomWalkGGM\nAR_p                         1\nMA_q                         0\nGGM_1mphi                         6.9e-06\nseasonalsignal        seasonal_signal_value\nhalfseasonalsignal    half_seasonal_signal_value\nestimateoffsets       no\nestimatepostseismic   no\nestimateslowslipevent no\nRandomiseFirstGuess   no\nTimeNoiseStart        1\nDegreePolynomial      1\nLikelihoodMethod      AmmarGrag\nJSON                  yes\n"
  }else if(mdl=="rw"){
    estimate_trend_tmpl = "DataFile              data_file_value\nDataDirectory         data_directory_value\nOutputFile            output_file_value\ninterpolate           no\nPhysicalUnit          mm\nScaleFactor           1.0\nNoiseModels           RandomWalkGGM\nGGM_1mphi                         6.9e-06\nseasonalsignal        seasonal_signal_value\nhalfseasonalsignal    half_seasonal_signal_value\nestimateoffsets       no\nestimatepostseismic   no\nestimateslowslipevent no\nRandomiseFirstGuess   no\nTimeNoiseStart        1\nDegreePolynomial      1\nLikelihoodMethod      AmmarGrag\nJSON                  yes\n"
    
  }
  
  
  
  
  # replace data_file_value by data_file
  tpl_1 = stringi::stri_replace( str = estimate_trend_tmpl, regex =  "data_file_value" , replacement = data_file)
  
  tpl_2 = stringi::stri_replace( str = tpl_1,regex =  "data_directory_value" , replacement = working_folder)
  
  tpl_3 = stringi::stri_replace( str = tpl_2,regex =  "output_file_value" , replacement = output_file)
  
  tpl_4 = stringi::stri_replace( str = tpl_3,regex =  "seasonal_signal_value" , replacement = if (n_seasonal > 0) "yes" else "no")
  
  tpl_5 = stringi::stri_replace( str = tpl_4,regex =  "half_seasonal_signal_value" , replacement = if (n_seasonal > 1) "yes" else "no")
  
  tpl_6 = stringi::stri_replace( str = tpl_5,regex =  "degree_polynomial_value" , replacement = 1)
  
  tpl_6
}

# ---------------------- Simulation

B = 500
n = 500
mat_param_rw = matrix(NA, ncol=1, nrow=B)
pb <- progress_bar$new(total = B)
gamma2_rw = 2

for(b in seq(B)){
  
  # set seed
  seed_number = 12345 + b
  
  # generate ts
  set.seed(seed_number)
  y = cumsum(rnorm(n=n, mean = 0, sd = sqrt(gamma2_rw)))

  # define a temporary working folder
  working_folder = paste(tempdir(), stringi::stri_rand_strings(n=1,length = 16), sep = "/")
  
  # create temp folder
  dir.create(working_folder, showWarnings = F, recursive = T)
  
  # define t
  t = 1:n
  
  # write file
  write_ts_mom(y = y, t=t, filename = paste(working_folder, "ts.mom", sep = "/"))
  
  # write ctl file
  cfg = infuse_estimate_trend_tmp(  data_file = "ts.mom",
                                    working_folder = working_folder,
                                    output_file = paste(working_folder, "output.mom", sep="/"),
                                    mdl = "rw",
                                    n_seasonal = 0)
  
  cfg_file = paste(working_folder, "estimate.ctl", sep="/")
  write(cfg, file = cfg_file)
  
  # launch estimation via command line
  cmd = sprintf("cd %s; %s '%s'", working_folder, "estimatetrend", cfg_file)
  
  # run hector
  timing = system.time({out = system(cmd, intern = TRUE)})
  
  # parse json
  json = rjson::fromJSON(file = paste(working_folder, "estimatetrend.json", sep="/"))
  
  # save estimated parameters in matrix
  mat_param_rw[b, 1] =  json$driving_noise^2 * json$NoiseModel$RandomWalkGGM$fraction
  
  pb$tick()
}

boxplot(mat_param_rw[,1])
abline(h=gamma2_rw)
