hsi_data <- data.frame(
  Habitat = as.double(1:10),
  Index = c(0, 1, 1, 0.5, 0.5, 0.4, 0.3, 0.2, 0.1, 0)
)

hsi_data <- tibble::as_tibble(hsi_data)

trans_data <- data.frame(
  Distance = c(1, 2, 2.5, 3, 4, 6),
  Habitat = c(10, 10.1, 10.15, 10.15, 10, 9.85)
)

hsi_data <- tibble::as_tibble(hsi_data)
trans_data <- tibble::as_tibble(trans_data)

devtools::use_data(hsi_data, overwrite = TRUE)
devtools::use_data(trans_data, overwrite = TRUE)
