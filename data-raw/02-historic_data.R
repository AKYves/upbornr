#creating the historic data using the previous dowloaded pdfs
historic_long_data <- convert_files("./data-raw/pdf_data")

historic_wide_data <- convert_files("./data-raw/pdf_data", keepwide = TRUE)

#use_data
use_data(historic_long_data, compress = "bzip2", overwrite = T)
use_data(historic_wide_data, compress = "bzip2", overwrite = T)
