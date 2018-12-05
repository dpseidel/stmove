# ### The basic constructions:
# 1.  Construct 25%, 50%  and 95% home range isopleths by season using two methods:
#   - 1 general space-time constructor using `t-locoh` hull sets
# - 1 autocorrelated utilization distribution analysis modeled after `ctmm::akde`


# ctmm will require telemetry objects.

# this is a scratch function for moving an sf object to move object to telemetry object
# should be generalized

# els.tm <- map(els, function(i) {
#   AG <- sf_els %>%
#     filter(id == i) %>%
#     st_set_crs(32733) %>%
#     mutate(
#       location.x = st_coordinates(.)[, 1],
#       location.y = st_coordinates(.)[, 2],
#       timestamp = ymd_hms(date)
#     ) %>%
#     dplyr::select(
#       id, timestamp,
#       location.x, location.y
#     )
#   mv <- move(
#     x = AG$location.x,
#     y = AG$location.y,
#     time = AG$timestamp,
#     data = as.data.frame(AG),
#     animal = AG$id,
#     proj = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs")
#   ) %>%
#     as.telemetry(., projection = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs"))
#
#   mv@info$identity <- i
#
#   return(mv)
# })
