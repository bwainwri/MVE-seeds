# KM Hall
# 20231017
# 
# Function that is used to import MVE data into R. This file will be sources by the
# scripts that process MVE Blue, Black, and Creosote.



# path to where the raw data files reside - CHANGE THIS TO THE PATH ON YOUR COMPUTER
path_to_data_folder <- "/Users/brookewainwright/Documents/Mean-Variance-seeds/mve_data_processing/raw_data/"



# function to read .dat files into R after downloading from GDrive
read_mve_in <- function(file_name) {
  
  # there are 4 header rows in the files. The 2nd row contains the variable names
  header <- names(read_csv(paste0(path_to_data_folder, file_name),
                           skip = 1,
                           n_max = 0))
  
  # read file into R using correct header name
  mve <- read_csv(paste0(path_to_data_folder, file_name),
                  skip = 4,
                  col_names = header)
  
  return(mve)
}






# you can use this function by itself to load the raw data in its wide format while
# handling the multiple header rows
# here is one example loading the more recent MVE Blue data - UNCOMMENT NEXT LINE TO RUN:
mve_blue <- read_mve_in("MVE_Blue.dat")



