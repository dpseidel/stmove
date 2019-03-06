# els.rda creation
library(tidyverse)
library(adehabitatLT)

# read in pre-cleaned elephant data
elephants <- read_csv("data-raw/elephants.csv")
usethis::use_data(elephants, internal = T, overwrite = T)

traj <- elephants %>% dl()

# regularize
els <- c(
  adehabitatLT::setNA(traj[1], date.ref = traj[[1]]$date[1], dt = 20, units = "min"),
  adehabitatLT::setNA(traj[2], date.ref = traj[[2]]$date[1], dt = 15, units = "min")
) %>% ld() %>% dplyr::select(x, y, date, id) %>% mutate(id = as.character(id)) %>% split(.$id)

AG195 <- stmove::kalman(els[[1]])
AG268 <- stmove::kalman(els[[2]])

write_csv(AG195, "data-raw/AG195.csv")
write_csv(AG268, "data-raw/AG268.csv")

# export
usethis::use_data(AG195, overwrite = T)
usethis::use_data(AG268, overwrite = T)
