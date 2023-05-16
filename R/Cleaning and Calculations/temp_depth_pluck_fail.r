dat <- data.frame(
  depth = c(NA, 4.24, 4.55, 1.82, NA, 3.03, NA, 1.21, 2.12, NA, 2.43, NA),
  temp = c(8.81, NA, NA, NA, 16.2, NA, 17.0, NA, NA, 14.3,NA, 14.6)
)



dats <- data.frame(
  sensor = c("depth", "temp", "depth", "temp", "depth", "temp", "depth", "temp",
             "depth", "depth", "depth", "depth", "temp"),
  value = c(3, 12.6, 2.8, 15.2, 1.8, 15.8, 1.5, 11.6, 4.1, 7.8, 9.2,
            9.6, 7.6)
)

test <- data.frame(
  sensor = c("depth", "temp", "depth", "temp", "temp", "temp", "depth", "temp",
             "depth", "depth", "depth", "depth", "temp"),
  value = c(3, 12.6, 2.8, 15.2, 17.2, 15.8, 1.5, 11.6, 4.1, 7.8, 9.2,
            9.6, 7.6)
)








dat


dats 
test


inds = which(dat$sensor == "temp" & dat$value > 15)
# We use lapply() to get all rows for all indices, result is a list
rows <- lapply(inds, function(x) (x-2):(x+2))
# With unlist() you get all relevant rows
to_hilight <- dat[unlist(rows),] %>%
  merge(., dat)

# dats <- dat|>
#   tidyr::pivot_longer(cols = depth:temp, names_to = "sensor", values_to = "value")



f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}





library(sf)
df <- read.csv('thedata')

library(microbenchmark)
microbenchmark(a = {
  k <- st_as_sfc(structure(df$the_geom, class = 'WKB'), EWKB = T)
  k <- st_set_geometry(df, k)
}, b = {
  k <- st_as_sf(k,
                coords = c('longitude', 'latitude'),
                crs = 4326)
}
)
















# 
# dat |>
#   dplyr::filter(
#     # temp > 15 | !is.na(depth) & is.na(temp))
#     case_when(
#     temp > 15  ~ TRUE,
#     is.na(lead(temp)) ~ TRUE
#     )
# )
# dat[temp > 15 | shift(temp > 15, n = 1L, type = "lag") | shift(temp > 15, 
#                                                               n = 2L, 
#                                                               type = "lead")]
# 
# df[Mark == 1 | shift(Mark==1, n=5L, type = "lag") | shift(search==1, n=5L, type = "lead")]
# gropp_
# #3 rows before the condition matches
# n <- 0:3
# 
# lapply(dat, function(x) {
#   inds <- which(dat$temp > 15)
#   if(length(inds)) x[sort(unique(sapply(inds, `-`, n))), ]
# })    
# 
# inds = which(dat$temp > 15)
# library(data.table)
# 
# dat <- data.table::setDT(dat)
# 
# # dat_temps <- dat_temp[temp > 15 & 
#                         shift(temp > 15, 
#                               # n = 1L, 
#                               type = "lag") | 
#                         shift(temp > 15, 
#                               # n = 1L, 
#                               type = "lead")]
# 
# 
# # 
# # 
# dat_temps %>% 
#   filter(temp > 15 )
# 
# dat_temps <- dat_temp %>% 
#   filter(case_when(
#     temp > 15 ~ TRUE,  
#       lag(depth, n = 1L) & lead(depth, n = 1L) ~ TRUE
#   )
#   )
# 
# 
# 
# dat_temps <- dat_temp %>% 
#   group_by(floy_tag) %>% 
#   slice(rep(which(temp > 15), each = 1) + 0:3)
# 
# dat_temps <- dat_temp[which(dat_temp$temp > 15)[3] + c(-1:1), ]
# dat_temps <- dat_temp %>% 
#   mutate(grp = case_when(
#     temp > 15 ~ 1, 
#     temp > 15 ~ 0
#   ))