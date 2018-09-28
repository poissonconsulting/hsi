
hsi_data <- data.frame(Habitat = as.double(1:10), 
                       Index = c(1,1,1,0.5,0.5,0.4,0.3,0.2,0.1,0.0))

hsi_data <- tibble::as_tibble(hsi_data)

devtools::use_data(hsi_data)