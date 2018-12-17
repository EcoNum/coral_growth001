##### analysis on the coral growth ####
SciViews::R

growth_monitoring_plateau <- read_delim("data/raw/growth_monitoring_plateau.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE,
                                        skip = 11)

visdat::vis_dat(growth_monitoring_plateau)
dplyr::glimpse(growth_monitoring_plateau)

growth_monitoring_plateau %>.%
  dplyr::mutate(., sample_date = lubridate::dmy_hm(.$sample_date),
                   id = as.factor(id),
                   skel_weight = skeleton_weight(.$weight,
                                                 .$salinity,
                                                 .$temperature)) -> growth
visdat::vis_dat(growth)
dplyr::glimpse(growth)

growth %>.%
  stats::na.omit(.) %>.%
  ggplot(., aes(x = sample_date, y = skel_weight, col = id)) +
    geom_line() +
    geom_point() +
    theme_bw()

growth %>.%
  na.omit(.) %>.%
  group_by(., sample_date) %>.%
  summarise(., n = n(),
               skelweight_mean = mean(skel_weight),
               skelweight_sd = sd(skel_weight)) %>.%
  ggplot(., aes(x = sample_date, y = skelweight_mean))+
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = skelweight_mean - skelweight_sd,
                      ymax = skelweight_mean + skelweight_sd)) +
    theme_bw()


#### Import data ####

# obj <- "data/coral_growth/growth_monitoring_051118.csv"

dir("data/coral_growth/", full.names = TRUE) -> file_list

file_list[!stringr::str_detect(file_list, "bis")] -> file_list

convert_coral <- function(file_list) {

  # Create monitoring_growth
  monitoring_growth <- data.frame()

  # Import data from file list
  for (obj in file_list) {

    # Import metadata and extract information
    metadata_read <- readr::read_lines(obj, n_max = 5,
                                     locale = readr::locale(encoding = "LATIN1"))
    metadata_list <- stringr::str_extract_all(metadata_read,
                                       "(\\w+:?/?\\.?\\w*:?/?\\.?\\w*)")

    # Rename list elements
    sapply(metadata_list, `[[`, 1) -> names(metadata_list)

    # metadata
    dplyr::tibble(projet = metadata_list$sample[[2]],
                  date = lubridate::dmy_hm(paste(metadata_list$sample_date[[2]],
                                                 metadata_list$sample_date[[3]],
                                                 sep = " ")),
                  author = metadata_list$author[[2]],
                  comment = metadata_list$comment[[2]]) -> metadata

    # date test
    if (lubridate::date(metadata$date) !=
        lubridate::dmy(str_extract_all(obj, "\\d+"))) {
      stop(paste( "\n ERROR !!! \n",
                  "date file     : ",
                  lubridate::dmy(str_extract_all(obj, "\\d+")), "\n",
                  "date metadata : ", lubridate::date(metadata$date)))
    }

    # Import data
    growth_monitoring <- read_delim(obj,";", escape_double = FALSE,
                                    trim_ws = TRUE,skip = 11)

    # Merge
    cbind(growth_monitoring, metadata) -> growth_monitoring
    rbind(monitoring_growth, growth_monitoring) -> monitoring_growth

  }

  # id != NA, id as factor,arrange by date and id
  monitoring_growth %>.%
    filter(., id != "NA") %>.%
    mutate(., id = as.factor(.$id)) %>.%
    arrange(., date, id) -> monitoring_growth

  # Add attributes
  attr(monitoring_growth$localisation, "label") <- "Mesocosm"
  attr(monitoring_growth$species, "label") <- "Species"
  attr(monitoring_growth$id, "label") <- "ID"
  attr(monitoring_growth$date, "label") <- "Date"

  attr(monitoring_growth$weight, "units") <- "g"
  attr(monitoring_growth$weight, "label") <- "Buoyant weight"

  attr(monitoring_growth$salinity, "units") <- "PSU"
  attr(monitoring_growth$salinity, "label") <- "Salinity"

  attr(monitoring_growth$temperature, "units") <- "°C"
  attr(monitoring_growth$temperature, "label") <- "Temperature"

  # Print data visualisation and information
  cat("Use visdat::vis_dat to print the preliminary visualisation of data", sep = "\n")
  print(visdat::vis_dat(monitoring_growth))
  cat("\n" ,"Data.frame structure :", "\n", sep = "")
  glimpse(monitoring_growth)
  cat("\n" ,"Print buoyant weight", "\n", sep = "")
  monitoring_growth %>.%
    ggplot(., aes(x = date, y = weight, col = id)) +
    geom_line() +
    geom_point() +
    theme_bw() -> weight_plot
  print(weight_plot)

  # Return data.frame
  return(monitoring_growth)
}

# Use convert_coral function
convert_coral(file_list) -> growth

file_list[!stringr::str_detect(file_list, "221018")] -> file_list2

convert_coral(file_list2) -> growth

# Fonction coral_growth
coral_growth <- function(obj) {

  obj %>.%
    as.data.frame(.) %>.%
    dplyr::mutate(., row_num = seq(1, to = nrow(.))) %>.%
    dplyr::select(., row_num, localisation, species, id, weight, salinity,
                  temperature, sample_date) %>.%
    dplyr::mutate(., skel_weight = skeleton_weight(.$weight,
                                                   .$salinity,
                                                   .$temperature)) %>.%
    dplyr::group_by(., localisation, species, id) %>.%
    dplyr::mutate(., day = as.numeric(round(lubridate::make_difftime(sample_date - sample_date[1],
                                                                     units = "day"),
                                            digits = 0)),
                  day_lag = dplyr::lag(day),
                  n_day = day - day_lag,
                  growth = skel_weight/skel_weight[1],
                  growth_ln = ln(growth)) %>.%
    as.data.frame(.) -> obj

  # Identification des numeros de lignes pour la régression
  obj %>.%
    group_by(., localisation, species, id) %>.%
    summarize(., row = list(row_num)) -> model_infos

  mod_list <- list()
  for (i in 1:nrow(model_infos)) {
    obj %>.%
      .[model_infos$row[[i]],] %>.%
      # lm(data = ., formula = growth_ln ~ 0 + day) -> mod_list[[i]]
      lm(data = ., formula = growth_ln ~ day) -> mod_list[[i]]
    names(mod_list)[i] <- paste(model_infos$localisation[i],
                                model_infos$species[i],
                                model_infos$id[i],
                                sep = "/")
  }

  ggplot(obj, aes(x = day, y = growth_ln, col = as.factor(id))) +
    #geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 0.5) +
    theme_bw() -> reg_plot
  print(reg_plot)

  stargazer::stargazer(mod_list, type = "text", column.labels = names(mod_list))

  attr(obj, "regression") <- mod_list
  return(obj)

}

coral_growth(growth) -> test

###################

googlesheets_as_csv <- "https://docs.google.com/spreadsheets/d/{id}/export?format=csv"
coral_id <- "1iMH4YXh80SxG0Rg6miMVABglIB7fnH42cYenml9WsRM"
coral_url <- glue::glue(googlesheets_as_csv, id = coral_id)

coral <- read.csv(coral_url, dec = ',')

coral2 <- coral[1:271,1:7]
visdat::vis_dat(coral2)
coral2$sample_date <- lubridate::dmy_hm(coral2$sample_date)
visdat::vis_dat(coral2)

coral_growth(coral2) -> test
test[test$id == 11,]

coral_growth2(coral2)

###################

coral_growth2 <- function(obj) {

  arrange(obj, sample_date)[1, "sample_date"] -> start_date
  obj$id <- as.factor(obj$id)

  obj %>.%
    as.data.frame(.) %>.%
    dplyr::mutate(., row_num = seq(1, to = nrow(.))) %>.%
    dplyr::select(., row_num, localisation, species, id, weight, salinity,
                  temperature, sample_date) %>.%
    dplyr::mutate(., skel_weight = skeleton_weight(.$weight,
                                                   .$salinity,
                                                   .$temperature),
                     day_graphe = as.numeric(round(lubridate::make_difftime(sample_date - start_date,
                                                                         units = "day"),
                                                digits = 0))) %>.%
    dplyr::group_by(., localisation, species, id) %>.%
    dplyr::mutate(., day = as.numeric(round(lubridate::make_difftime(sample_date - sample_date[1],
                                                                     units = "day"),
                                            digits = 0)),
                  day_lag = dplyr::lag(day),
                  n_day = day - day_lag,
                  growth = skel_weight/skel_weight[1],
                  growth_ln = ln(growth)) %>.%
    as.data.frame(.) -> obj


  # Identification des numeros de lignes pour la régression
  obj %>.%
    group_by(., localisation, species, id) %>.%
    summarize(., row = list(row_num)) -> model_infos

  mod_list <- list()
  for (i in 1:nrow(model_infos)) {
    obj %>.%
      .[model_infos$row[[i]],] %>.%
      # lm(data = ., formula = growth_ln ~ 0 + day) -> mod_list[[i]]
      lm(data = ., formula = growth_ln ~ day) -> mod_list[[i]]
    names(mod_list)[i] <- paste(model_infos$localisation[i],
                                model_infos$species[i],
                                model_infos$id[i],
                                sep = "/")
  }

  ggplot(obj, aes(x = day_graphe, y = growth_ln, col = id)) +
    #geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 0.5) +
    theme_bw() -> reg_plot
  print(reg_plot)

  stargazer::stargazer(mod_list, type = "text", column.labels = names(mod_list))

  attr(obj, "regression") <- mod_list
  return(obj)

}




###################

# Add skel_weight with skeleton_weight
growth %>.%
  dplyr::mutate(., skel_weight = skeleton_weight(.$weight,
                                                 .$salinity,
                                                 .$temperature)) -> growth

growth %>.%
  group_by(., localisation, species, id) %>.%
  mutate(., number_day = as.numeric(round(lubridate::make_difftime(date - date[1],
                                                                   units = "day"),
                                          digits = 0)),
            growth_rate = (skel_weight - skel_weight[1])/skel_weight[1],
            growth_rate_byday = growth_rate/number_day*100,
            date_lag = dplyr::lag(date),
            number_day_lag = as.numeric(lubridate::make_difftime(lubridate::date(date) -
                                                                 lubridate::date(date_lag))),
            skel_weight_lag = dplyr::lag(skel_weight),
            growth_rate_lag = (skel_weight - skel_weight_lag)/skel_weight_lag,
            growth_rate_lag_day = growth_rate_lag/number_day_lag) -> growth

ggplot(growth, aes(number_day, growth_rate, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()

growth %>.%
  filter(., number_day != 0) %>.%
  ggplot(., aes(number_day, growth_rate_byday, col = id)) +
    geom_line() +
    geom_point() +
    theme_bw()

growth %>.%
  filter(., number_day != 0) %>.%
  ggplot(., aes(number_day, growth_rate_lag_day, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(growth, aes(x = date, y = weight, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()

growth %>.%
  select(., localisation, species, id, date, skel_weight) %>.%
  group_by(., localisation, species, id) %>.%
  mutate(., day = as.numeric(round(lubridate::make_difftime(date - date[1],
                                                            units = "day"),
                                   digits = 0)),
            skel_weight_lag = dplyr::lag(skel_weight),
            day_lag = dplyr::lag(day),
            n_day = day - day_lag,
            ## avant divisait par day skel_weight_lag :: growth_rate = skel_weight - skel_weight_lag/n_day
            growth_rate = (skel_weight - skel_weight_lag)/n_day,
            # growth_rate_day1 = (skel_weight - skel_weight[1])/day,
            growth_rate_dayone = growth_rate/skel_weight[1],
            growth_dayone = skel_weight/skel_weight[1],
            growth_dayone_ln = ln(skel_weight/skel_weight[1]),
            growth_dayone_lag = lag(growth_dayone),
            growth_dayone_rate1 = (growth_dayone - growth_dayone[1])/day,
            growth_dayone_rate2 = (growth_dayone - growth_dayone_lag) / n_day
         ) -> growth2


ggplot(growth2, aes(x = day, y = growth_dayone, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(growth2, aes(x = day, y = growth_dayone_ln, col = id)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  theme_bw()

ggplot(growth2, aes(x = day, y = growth_dayone_rate1, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(growth2, aes(x = day, y = growth_dayone_rate2, col = id)) +
  geom_line() +
  geom_point() +
  theme_bw()


# lm(data = growth2, growth_rate_dayone ~ ln(day)) %>.%
#   plot(.)
#
# ggplot(growth2, aes(x = day, y = growth_rate_dayone)) +
#   # geom_line() +
#   geom_point(aes(col = id)) +
#   theme_bw() +
#   geom_smooth(method = "lm", formula = y ~ ln(x) )
#


for (i in unique(growth2$id)) {
  growth2 %>.%
    filter(., id == i) %>.%
    lm(data = ., formula = growth_dayone_ln ~ day) -> lmod
  print(i)
  print(lmod$coefficients)
  print(summary(lmod)$r.squared)
}

growth2 %>.%
  group_by(., localisation, species, id) %>.%
  summarize(., n = n()) -> model_test2

mod_list <- list()
for (i in seq_along(unique(growth2$id))) {
  growth2 %>.%
    .[.$id == i,] %>.%
    # lm(data = ., formula = growth_dayone_ln ~ 0 + day) -> mod_list[[i]]
    lm(data = ., formula = growth_dayone_ln ~ day) -> mod_list[[i]]
    names(mod_list)[i] <- paste("id", i)
}


# Identification des numeros de lignes pour la régression
growth2 %>.%
  as.data.frame(.) %>.%
  mutate(., row_num = seq(1, to = nrow(.))) %>.%
  select(., localisation, species, id, date, skel_weight, day, growth_dayone_ln, row_num) %>.%
  group_by(., localisation, species, id) %>.%
  summarize(., row = list(row_num)) -> model_infos
mod_list <- list()
for (i in 1:nrow(model_infos)) {
  growth2 %>.%
    .[model_infos$row[[i]],] %>.%
    # lm(data = ., formula = growth_dayone_ln ~ 0 + day) -> mod_list[[i]]
    lm(data = ., formula = growth_dayone_ln ~ day) -> mod_list[[i]]
  names(mod_list)[i] <- paste(model_infos$localisation[i],
                              model_infos$species[i],
                              model_infos$id[i])
}


stargazer::stargazer(mod_list, type = "text", column.labels = names(mod_list))

