# default is to use tidyverse functions
select <- dplyr::select 
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete

# colours 
global_kicols <- c(
  grDevices::rgb(143, 170, 220, maxColorValue = 255), # blue
  grDevices::rgb(244, 177, 131, maxColorValue = 255), # orange
  grDevices::rgb(165, 165, 165, maxColorValue = 255) # grey
)

# used for calculation of ci 
global_z05 <- qnorm(1 - 0.025)