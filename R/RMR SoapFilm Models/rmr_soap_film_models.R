
# load packages ----
{
  library(broom.mixed)
  library(dplyr)
  library(fitdistrplus)
  library(ggplot2)
  library(ggtext)
  library(ggh4x)
  library(gamm4)
  library(gamlss)
  library(gratia)
  library(here)
  library(itsadug)
  library(lubridate)
  library(lemon)
  library(janitor)
  library(lme4)
  library(mgcv)
  library(multcomp)
  library(openxlsx)
  library(patchwork)
  library(purrr)
  library(readr)
  library(sf)
  library(soapcheckr)
  library(tibble)
  library(tidymv)
  library(tidyr)
  library(visreg)
  source(here("R", 
              "Cleaning and Calculations", 
              "julian_date_reorder.r"))
}
# ---- papineau and metabolism data -----

ful <- read_rds(here("Saved Data", 
                     "BioE_lt_w_lat_lon.rds"))

plake <- st_read(dsn = here("..",
                            "Spatial Ecology", 
                            "Shapefiles"), 
                 layer = "plake_edit_wo_link")
# create utm plake 
plake_utm <- plake %>% 
  st_transform(., crs = 32618) 

##### SUBSET OUT ONLY RMR/M_Swim DATA and determine daily temp FOR 2017 - 2021-------

glimpse(ful)

ful_rmr <- ful %>%
  filter(sensor_unit %in% "m/s²" &
           year %in% c(2019, 2020)) %>%
  filter(!m_swim == is.nan(m_swim)) %>%
  group_by(floy_tag, weight, fish_basin, 
           # time_bins, 
           date,
           week, month, season, year,
           sensor_unit, labels) %>%
  summarise(mean_rmr = mean(m_swim), 
            mean_lon = mean(long_mean), 
            mean_lat = mean(lat_mean)) %>%
  ungroup() %>%
  mutate(
    floy_tag = factor(floy_tag),
    fish_basin = factor(
      stringr::str_replace(as.character(fish_basin), " Basin", ""),
      levels = c("East", "West", "North")),
    doy = yday(date),
    month = factor(month,
                   levels = c("May", "June", "July",
                              "August", "September", "October",
                              "Novemeber", "December", "January",
                              "February", "March", "April")),
    doy_id = days(date, end = "05-22"),
    season = forcats::fct_relevel(season, "Spring", "Summer",
                                  "Fall", "Winter")
  ) %>%
  arrange(date) %>%
  dplyr::select(floy_tag:sensor_unit, doy, doy_id, 
                mean_rmr, mean_lon, mean_lat)
glimpse(ful_rmr)



# ---- create boundary list we need to supply soap-film -----
plake_utm_mp <- plake_utm %>%
  st_cast("POLYGON") %>%
  dplyr::select(AREA_GEO, geometry) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  filter(id == 3) %>% 
  st_cast("MULTIPOINT") %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  dplyr::select(id, geometry)

plake_utm_mp

plake_bnd <- plake_utm_mp %>% 
  split(.$id) %>% 
  map(~ .x %>% 
        dplyr::select(id, geometry) %>% 
        st_cast("POINT") %>% 
        mutate(
          x = st_coordinates(.)[,"X"], 
          y = st_coordinates(.)[,"Y"]
        ) %>% 
        st_drop_geometry() %>% 
        dplyr::select(-id)
  ) 

plake_bnd
nr <- 1:69
plake_bnd_ls <- lapply(nr, function(n) as.list.data.frame(plake_bnd[[n]]))

plake_bnd_ls

# plake_bnd_ls <- lapply(nr,
#                 function(n)
#                   plake_bnd_ls[[n]] <- c(plake_bnd_ls[[n]],
#                                   list(f = rep(0, length(plake_bnd_ls[[n]]$x)))))
# ---- check boundary ----

soap_check(bnd = plake_bnd_ls)

# ---- create knots ----- 

plake_grid <- plake_utm %>%
  st_make_grid(cellsize = 200, square = TRUE, what = "centers") %>% 
  st_as_sf() 

plake_intesects <- st_intersection(plake_utm, plake_grid)

plake_intesects_points <-  plake_intesects %>% 
  # st_transform(., crs = 4326) %>% 
  mutate(
    lon = st_coordinates(.)[,"X"], 
    lat = st_coordinates(.)[,"Y"]
  ) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  dplyr::select(lon, lat) %>% 
  rename(mean_lon = lon, 
         mean_lat = lat)

# ---- check knots ----
soap_check(bnd = plake_bnd_ls, knots = plake_intesects_points, 
           x_name = "mean_lon", 
           y_name = "mean_lat")

crunch_ind <- autocruncher(plake_bnd_ls, knots = plake_intesects_points, 
                           xname = "mean_lon", yname = "mean_lat")

plake_intesects_points <- plake_intesects_points[-crunch_ind, ]

soap_check(bnd = plake_bnd_ls, knots = plake_intesects_points, 
           x_name = "mean_lon", 
           y_name = "mean_lat")

# ggplot() +
#   geom_sf(data = plake, fill = NA, colour = "black") +
#   # geom_sf(data = buf, fill = NA)
#   geom_sf(data = plake_intesects) + 
#   theme_bw(base_size = 15) + 
#   theme(
#     panel.grid = element_blank(),
#     axis.text = element_blank(), 
#     axis.ticks = element_blank()
#   ) -> p 
# 
# p
# ---- look at rmr distitibuion and what would fit best -----

descdist(ful_rmr$mean_rmr) 

norm <- fitdist(ful_rmr$mean_rmr, distr = "norm", method = "mle")
plot(norm)
gammas <- fitdist(ful_rmr$mean_rmr, distr = "gamma", method = "mme")
plot(gammas)

ggplot(data = ful_rmr, aes(x = mean_rmr)) + 
  geom_histogram()

# seems like gamma distribution 
# ---- add start_event to dataframe ---- 
ful_rmr <-  ful_rmr %>% 
  group_by(floy_tag, year) %>% 
  arrange(floy_tag, year, doy_id) %>% 
  mutate(start_event = if_else(doy_id == min(doy_id), true = TRUE, 
                               false = FALSE)) %>% 
  ungroup() %>% 
  arrange(date, start_event)

glimpse(ful_rmr)

# ---- transform into sf object ----- 
ful_rmr_sf <- st_as_sf(ful_rmr, coords = c("mean_lon", "mean_lat"), 
                       crs = 4326) %>% 
  st_transform(., crs = st_crs(plake_utm))


# ---- remove any points that fall outside the boundary ---- 
ful_rmr_sf_edit <- st_intersection(ful_rmr_sf, plake_utm) %>% 
  dplyr::select(floy_tag:start_event, geometry)
glimpse(ful_rmr_sf_edit)

# ---- remove geometry ----- 
ful_rmr_edit <- ful_rmr_sf_edit %>%
  # st_transform(., crs = 4326) %>% 
  mutate(
    x = st_coordinates(.)[,"X"], 
    y = st_coordinates(.)[,"Y"]
  ) %>% 
  st_drop_geometry()
ful_rmr_edit  


rmr_sfs <- ful_rmr_edit %>% 
  dplyr::select(x, y)
soap_check(bnd = plake_bnd_ls, data = rmr_sfs)

# ----- Model ----- 
glimpse(plake_intesects_points)
glimpse(ful_rmr_edit)
glimpse(plake_bnd_ls)
# plake_bnd_ls_1 <- lapply(1:length(plake_bnd_ls), 
#                        function(x) names(plake_bnd_ls[x]) <- c("mean_lon", 
#                                                                  "mean_lat")
# )

plake_intesects_points <- plake_intesects_points %>% 
  rename(
    x = mean_lon, 
    y = mean_lat
  )



m <- bam(mean_rmr ~ fish_basin + s(x, y, by = fish_basin,
                                   k = 100) +
           s(floy_tag, bs = "re"), 
         method = "fREML", 
         family = Gamma(link = "log"),
         data = ful_rmr_edit, 
         # knots = plake_intesects_points
)
m
summary(m)

draw(m)
rm(ful)
gc()
# plake_bnd_tb <- plake_bnd %>% 
#   bind_rows(.id = "id")

# plake_sample <- plake_utm %>%
#   st_make_grid(cellsize = 10, square = TRUE, what = "centers") %>% 
#   st_as_sf() 
# 
# # plake_sample <- plake_utm %>% 
# #   st_sample(
# #    size = 75000 
# #   ) %>% 
# #   st_as_sf()
# 
# 
# st_geometry(plake_sample) <- "geometry"
# 
# plake_sample <- st_intersection(plake_sample, plake_utm) %>% 
#   dplyr::select(geometry)
# # plot(plake_sample)
# 
# 
# 
# plake_smp <- crossing(
#   plake_sample %>% 
#   mutate(
#     x = st_coordinates(.)[,"X"], 
#     y = st_coordinates(.)[,"Y"], 
#   ) %>% 
#   st_drop_geometry(),
#   fish_basin = unique(ful_rmr_edit$fish_basin)
# ) %>% 
#   dplyr::select(fish_basin, x, y)


# write_rds(plake_smp, here::here("Results", 
#                                 "trps_lake_grid.rds"))

plake_smp <- read_rds(here::here("results", 
                                 "trps_lake_grid.rds")) %>% 
  mutate(
    floy_tag = "a"
  )
glimpse(plake_smp)
# pred <- augment(m, newdata = plake_smp) %>% 
#   mutate(
#     lower = exp(1) ^ (.fitted - 1.96 * .se.fit),
#     upper = exp(1) ^ (.fitted + 1.96 * .se.fit), 
#     .fitted = exp(1) ^ .fitted
#   )
dat_2 <- ful_rmr_edit %>% 
  mutate(
    floy_tag = "a"
  )

pred <- augment(m, newdata = plake_smp, type.predict = "response", 
                exclude = "s(floy_tag)")
  # mutate(
  #   lower = exp(1) ^ (.fitted - 1.96 * .se.fit),
  #   upper = exp(1) ^ (.fitted + 1.96 * .se.fit), 
  #   .fitted = exp(1) ^ .fitted
  # )

# write_rds(plake_smp, here::here("Results", 
#                                 "trps_lake_grid.rds"))

# pred_sf <- pred %>% 
#   st_as_sf(coords = c("x", "y"),
#            crs = st_crs(plake_utm))
# 
# pred_sf


# plake_bnd_tb[,2]
# grid.x <- with(m$var.summary,
#                seq(min(c(x, plake_bnd_tb[, 2])), max(c(x, plake_bnd_tb[, 2])), by = 2.5))
# grid.y <- with(m$var.summary,
#                seq(min(c(y, plake_bnd_tb[,3])), max(c(y, plake_bnd_tb[,3])), by = 2.5))
# pdata_1 <- with(m$var.summary, expand.grid(x = grid.x, y = grid.y))
# 
# head(pdata_1)
# names(pdata) <- c("x","y")
# ps <- autocruncher(bnd = plake_bnd_ls, knots = pdata_1)

##predictions
# pdata <- transform(pdata, Depth = predict(m, pdata, type = "response"))
# beep::beepr()
# glimpse(pdata)
# tmp <- pdata                         # temporary version...
# take <- with(tmp, Depth > 0)        # getting rid of > 0 depth points
# tmp$Depth[take] <- NA
# 
# pdata
# pdata <- pdata %>% 
#   filter(Depth > 0) %>% 
#   rename(
#     pred_rmr = Depth
#   ) %>% 
#   as_tibble()
# 
# glimpse(pdata)
# pdata  <- pdata %>% 
#   filter(pred_rmr < 200)
# write_rds(pdata, here::here("Results",
#                             "pred_data.rds"))
# 
# pdata_sf <- pdata %>% 
#   st_as_sf(coords = c("x", "y"), 
#            crs = st_crs(plake_utm))
# 
# plake_utm <- plake_utm %>% 
#   dplyr::select(geometry)
# 
# pdata_sf <- st_intersection(pdata_sf, plake_utm)
# 
# glimpse(pdata_sf)
# ggplot() +
#   # geom_sf(data = plake_utm) + 
#   geom_raster(data = pdata, aes(x = x, y = y, fill = pred_rmr)) +
#   # geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
#   coord_fixed() +
#   scale_fill_viridis_c(na.value = NA)


ggplot() +
  geom_sf(data = pred_sf, aes(colour = .fitted)) +
  geom_sf(data = plake_utm, fill = NA, colour = "black") +
  facet_wrap(~ fish_basin) + 
  # geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
  # coord_fixed() +
  scale_colour_viridis_c(na.value = NA) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank()
  )

glimpse(pred)

ggplot() +
  geom_raster(data = pred, aes(x = x, y = y, fill = .fitted)) +
  geom_sf(data = plake_utm, fill = NA, colour = "black") +
  facet_wrap(~ fish_basin) + 
  # geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
  # coord_fixed() +
  scale_fill_viridis_c(na.value = NA, option = "B", 
                       name = "Active Metabolism <br>(mg O<sub>2</sub> 
                       kg <sup>-1</sup> h<sup>-1</sup>)") + 
  scale_colour_viridis_c(na.value = NA, option = "B", 
                       name = "Active Metabolism <br>(mg O<sub>2</sub> 
                       kg <sup>-1</sup> h<sup>-1</sup>)") + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_blank(),
    legend.position = c(0.93, 0.88),
    legend.title = element_markdown(),
    # legend.key = elem
    # axis.text = element_blank(),
    # axis.title = element_blank(),
    # axis.ticks = element_blank(),
    strip.background = element_blank()
  ) + 
  guides(fill = guide_colourbar(
    
    barheight = unit(1.5, "in"),
    ticks.linewidth = 0.5,
    frame.linewidth = 0.3,
    ticks.colour = 'black', 
    frame.colour = 'black')) + 
  labs(x = "Longitude", 
       y = "Latitude") -> p

p

# ggsave(filename = here::here("Plots", 
#                              "TRPS and SF GAMs", 
#                              "TRPS_basin.png"), plot = p,
#        width = 5.5 * 3, height = 11)
# m <- read_rds(here::here("model objects", 
#                          "soap_gam_rmr.rds"))

# m
# pdata2 <- transform(plake_intesects_points[, 1:2], 
#                     mean_rmr = predict(m, newdata = plake_intesects_points))
# glimpse(pdata2)



# ful_rmr_edit <- as.data.frame(ful_rmr_edit)
gc()


# rm(pdata_1)
plake_bnd_ls <- lapply(nr,
                       function(n)
                         plake_bnd_ls[[n]] <- c(plake_bnd_ls[[n]],
                                                list(f = rep(0, 
                                                             length(plake_bnd_ls[[n]]$x)))))
# head(ful_rmr_edit)
east_basin <- ful_rmr_edit %>%
  filter(fish_basin %in% "East")

descdist(east_basin$mean_rmr)
hist(east_basin$mean_rmr)
m1 <- gam(mean_rmr ~ s(x, y, bs = "so",
                       # by = fish_basin,
                       xt = list(bnd = plake_bnd_ls,
                                 nmax = 1500)), 
          method = "REML", 
          control = list(nthreads = 6, 
                         ncv.threads = 4), 
          family = Gamma(link = "log"),
          data = ful_rmr_edit, 
          knots = plake_intesects_points
)


summary(m1)
AIC(m1)
beepr::beep()
# summary(m2)
# draw(m1)

# lims <- apply(crds, 2, range)
# ylim <- lims[,2]
# xlim <- lims[,1]
# 
# plot(m1, asp = 1, se = FALSE, scheme = 2, main = "")

plake_sample <- plake_utm %>% 
  st_make_grid(cellsize = 5, square = TRUE, what = "centers") %>%
  st_as_sf()

# plake_sample_2 <- plake_utm %>% 
#   st_make_grid(cellsize = 20, square = TRUE, what = "centers") %>%
#   st_as_sf()

# # plake_sample <- plake_utm %>% 
# #   st_sample(
# #    size = 75000 
# #   ) %>% 
# #   st_as_sf()
# 
# 
st_geometry(plake_sample) <- "geometry"
st_geometry(plake_sample_2) <- "geometry"

glimpse(plake_sample)

plake_sample <- st_intersection(plake_sample, plake_utm) %>%
  dplyr::select(geometry)
plake_sample_2 <- st_intersection(plake_sample_2, plake_utm) %>%
  dplyr::select(geometry)
# # plot(plake_sample)
# 
# 
# 
plake_smp <- 
  # crossing(
  plake_sample %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"],
  ) %>%
  st_drop_geometry()
plake_smp_2 <- 
  # crossing(
  plake_sample_2 %>%
  mutate(
    x = st_coordinates(.)[,"X"],
    y = st_coordinates(.)[,"Y"],
  ) %>%
  st_drop_geometry()

glimpse(plake_smp)
glimpse(plake_smp_2)
#   fish_basin = unique(ful_rmr_edit$fish_basin)
# ) %>%
#   dplyr::select(fish_basin, x, y)


# write_rds(plake_smp, here::here("Results", 
#                                 "trps_lake_grid.rds"))

# plake_smp <- read_rds(here::here("results", 
#                                  "trps_lake_grid.rds"))
glimpse(plake_smp)
ggplot() +
  geom_sf(data = plake_sample)
# plake_smp_single <- plake_smp %>% 
#   filter(fish_basin %in% "East") %>% 
#   dplyr::select(-fish_basin)

gc()
glimpse(plake_smp_single)
pred_soap <- augment(m1, newdata = plake_smp) 
beepr::beep()
pred_soap <- pred_soap %>%
  mutate(
  lower = exp(1) ^ (.fitted - 1.96 * .se.fit),
  upper = exp(1) ^ (.fitted + 1.96 * .se.fit),
  .fitted_tran = exp(1) ^ .fitted
)
pred_soap_1 <- pred_soap %>% 
  filter(.fitted_tran > 0 & .fitted_tran < 200)

summary(pred_soap_1$.fitted_tran)
write_rds(pred_soap_1, file = here::here("Saved Data", 
                                         "10_m_predict_sf_gam_gamma.rds"))
ggplot() +
  geom_raster(data = pred_soap_1, aes(x = x, y = y, fill = .fitted_tran)) +
  geom_sf(data = plake_utm, fill = NA, colour = "black") +
  # facet_wrap(~ fish_basin) + 
  # geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
  # coord_fixed() +
  scale_fill_viridis_c(na.value = NA, option = "B", 
                       name = "Active Metabolism <br>(mg O<sub>2</sub> 
                       kg <sup>-1</sup> h<sup>-1</sup>)") + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_blank(),
    legend.position = c(0.93, 0.88),
    legend.title = element_markdown(),
    # legend.key = elem
    # axis.text = element_blank(),
    # axis.title = element_blank(),
    # axis.ticks = element_blank(),
    strip.background = element_blank()
  ) + 
  guides(fill = guide_colourbar(
    
    barheight = unit(1.5, "in"),
    ticks.linewidth = 0.5,
    frame.linewidth = 0.3,
    ticks.colour = 'black', 
    frame.colour = 'black')) + 
  labs(x = "Longitude", 
       y = "Latitude") -> p1

p1

ggsave(filename = here::here("Plots",
                             "TRPS and SF GAMs",
                             "sf_no_basin_10_gamma.png"), plot = p1,
       width = 5.5, height = 11)
# write_rds(plake_smp, here::here("Results", 
#                                 "trps_lake_grid.rds"))

# pred_soap_sf <- pred_soap %>% 
#   st_as_sf(coords = c("x", "y"),
#            crs = st_crs(plake_utm))


# head(pdata[, 1:2])
# test <- plake_intesects_points[1, ]
# rmr_raster_smooth <- crossing(
#   # tibble(date = unique(df$daily)),
#   tibble(mean_lon = unique(plake_intesects_points$mean_lon)),
#   # tibble(year = unique(df$year)), 
#   tibble(mean_lat = unique(plake_intesects_points$mean_lat)),
#   # depths can now be any value
#   # tibble(depth = seq(1, 20, length.out = 100))
#   
# ) %>%
#   mutate(
#     # day_in_year = jdate,
#     # day_in_year = as.numeric(date - floor_date(date, 
#     #                                            unit = "years"), 
#     #                          unit = "days"),
#     rmr = predict(
#       m, 
#       newdata = tibble(
#         # date = date,
#         # jdate = jdate,
#         mean_lon = mean_lon,  
#         # year = year,
#         # receiver_name = receiver_name,
#         mean_lat = mean_lat, 
#       )
#     )
#   )
# 
# 
# plake_grid_pred <- plake_utm %>%
#   st_make_grid(cellsize = 50, square = TRUE) %>% 
#   st_as_sf() %>% 
#   # st_transform(., crs = 4326) %>% 
#   st_cast("POINT")
# 
# plake_intesects_pred <- st_intersection(plake_utm, plake_grid_pred)
# 
# # plake_intesects_buffer <- plake_intesects[d[,1]>units::set_units(50,m)]
# plake_intesects_points_pred <-  plake_intesects_pred %>% 
#   # st_transform(., crs = 4326) %>% 
#   mutate(
#     lon = st_coordinates(.)[,"X"], 
#     lat = st_coordinates(.)[,"Y"]
#   ) %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(lon, lat) %>% 
#   rename(mean_lon = lon, 
#          mean_lat = lat) %>% 
#   as_tibble()
# 
# plake_intesects_points_pred

# plake_intesects_points %>% 
#   as_tibble()
# 
# pred_ras <- predict(m, ty = "response")
# 
# pred_rs <- augment(m, newdata = plake_intesects_points, type.predict = "link")



# row.names(plake_intesects_points) <- NULL
# ggplot() +
#   geom_sf(data = plake, fill = NA, colour = "black") +
#   # geom_sf(data = buf, fill = NA)
#   geom_sf(data = pred_rs) + 
#   theme_bw(base_size = 15) + 
#   theme(
#     panel.grid = element_blank(),
#     axis.text = element_blank(), 
#     axis.ticks = element_blank()
#   ) -> p2 

# p

# rmr_raster_smooth <- plake_intesects_points_pred %>%
#   mutate(
#     # day_in_year = jdate,
#     # day_in_year = as.numeric(date - floor_date(date, 
#     #                                            unit = "years"), 
#     #                          unit = "days"),
#     rmr = predict(
#       m, 
#       newdata = tibble(
#         # date = date,
#         # jdate = jdate,
#         mean_lon = .$mean_lon,  
#         # year = year,
#         # receiver_name = receiver_name,
#         mean_lat = .$mean_lat, 
#       )
#     )
#   )
# 
# beepr::beep()


# rmr_raster_smooth
# pred_rs_2 <- pred_rs %>%
#   filter(.fitted > 0 & .fitted < 2e+2)
# 
# 
# 
# pdata2_sf <- st_as_sf(pred_rs_2, coords = c("mean_lon", "mean_lat"), 
#                       crs = st_crs(plake_utm)) %>% 
#   st_cast("MULTIPOINT") %>% 
#   st_cast("POLYGON")
# 
# ggplot() + 
#   geom_sf(data = plake_utm, fill = NA, colour = "black") + 
#   geom_tile(data = pred_rs_2, aes(x = mean_lon, y = mean_lat, 
#                                   fill = .fitted)) + 
#   scale_fill_viridis_c(option = "B")
# ggplot() + 
#   geom_sf(data = plake_utm, fill = NA, colour = "black") + 
#   geom_sf(data = pdata2_sf, aes(fill = .fitted), shape = 21) + 
#   scale_fill_viridis_c(option = "B")
# 
# summary(pdata2$mean_rmr)
