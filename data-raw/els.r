# els.rda creation
library(tidyverse)
library(adehabitatLT)

# read in pre-cleaned elephant data
clean_els <- read_csv("../Elephants/Data/clean_allels.csv", col_types = "ddTdddddddccc")

# filter out 2 elephants, of different fix rates, for all of 2010
traj <- filter(clean_els, id %in% c("AG195", "AG268"), lubridate::year(date) == 2010) %>% adehabitatLT::dl(.)

AG268_raw <- ld(traj[2])
write_csv(AG268_raw, "data-raw/AG268_raw.csv")

# regularize
els <- c(
  adehabitatLT::setNA(traj[1], date.ref = traj[[1]]$date[1], dt = 20, units = "min"),
  adehabitatLT::setNA(traj[2], date.ref = traj[[2]]$date[1], dt = 15, units = "min")
) %>% ld() %>% dplyr::select(x, y, date, id) %>% mutate(id = as.character(id)) %>% split(.$id)

AG195 <- stmove::kalman(els[[1]])
AG268 <- stmove::kalman(els[[2]])

write_csv(AG195, "data-raw/AG195.csv")
write_csv(AG268, "data-raw/AG268.csv")

elephants <- rbind(AG195, AG268)
write_csv(elephants, "data-raw/elephants.csv")

# export
usethis::use_data(AG195, overwrite = T)
usethis::use_data(AG268, overwrite = T)
usethis::use_data(AG268_raw, overwrite = T)
usethis::use_data(elephants, overwrite = T)
