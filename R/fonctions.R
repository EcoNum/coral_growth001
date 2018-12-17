
SciViews::R
require(flow)
require(dplyr)
require(lubridate)

skeleton_weight <- function(buoyant_weight, S, T, P = 0, rho_aragonite = 2930){
  x <- seacarb::rho(S = S, T = T, P = P)
  y <- buoyant_weight / (1 - (x / rho_aragonite))
  attributes(y) <- NULL
  y
}


coral_import <- function(coral_url) {

  read.csv(coral_url, dec = ',') %>.%
    dplyr::mutate(., row_num = seq(1, to = nrow(.))) %>.%
    dplyr::mutate(., skel_weight = skeleton_weight(.$weight,
                                                   .$salinity,
                                                   .$temperature),
                     date = lubridate::dmy_hm(sample_date),
                     datefilter = lubridate::date(date),
                     id = as.factor(id)) %>.%
    dplyr::select(., row_num, localisation, species, id, sample_date,
                     skel_weight, weight, salinity, temperature,
                     date, datefilter) %>.%
    dplyr::arrange(., date, id) %>.%
    na.omit(.) -> obj
  return(obj)
}


coral_growth <- function(obj) {

  obj[1, "date"] -> start_date

  obj %>.%
    dplyr::mutate(., by_day = as.numeric(round(lubridate::make_difftime(date - start_date,
                                                                        units = "day"),
                                               digits = 0))) %>.%
    dplyr::group_by(., localisation, species, id) %>.%
    dplyr::mutate(., day = as.numeric(round(lubridate::make_difftime(date - date[1],
                                                                     units = "day"),
                                            digits = 0)),
                     day_lag = dplyr::lag(day),
                     n_day = day - day_lag,
                     growth = skel_weight/skel_weight[1],
                     growth_ln = ln(growth)) %>.%
    as.data.frame(.) -> obj
}

coral_plot <- function(obj, x, y) {

  chart(obj, as.formula(paste(y, "~", x, "%col=% id"))) +
  # ggplot(obj, aes(x = day, y = growth, col = id)) +
    geom_point() +
    theme_bw() -> graphe

  return(graphe)
}

coral_plot_ln <- function(obj) {
  ggplot(obj, aes(x = day, y = growth_ln, col = id)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, size = 0.5) +
    theme_bw() -> graphe

  return(graphe)
}

#
#   # Identification des numeros de lignes pour la régression
#   obj %>.%
#     group_by(., localisation, species, id) %>.%
#     summarize(., row = list(row_num)) -> model_infos
#
#   mod_list <- list()
#   for (i in 1:nrow(model_infos)) {
#     obj %>.%
#       .[model_infos$row[[i]],] %>.%
#       # lm(data = ., formula = growth_ln ~ 0 + day) -> mod_list[[i]]
#       lm(data = ., formula = growth_ln ~ day) -> mod_list[[i]]
#     names(mod_list)[i] <- paste(model_infos$localisation[i],
#                                 model_infos$species[i],
#                                 model_infos$id[i],
#                                 sep = "/")
#   }
#
#   ggplot(obj, aes(x = day, y = growth_ln, col = as.factor(id))) +
#     #geom_line() +
#     geom_point() +
#     geom_smooth(method = "lm", se = FALSE, size = 0.5) +
#     theme_bw() -> reg_plot
#   print(reg_plot)
#
#   stargazer::stargazer(mod_list, type = "text", column.labels = names(mod_list))
#
#   attr(obj, "regression") <- mod_list
#   return(obj)
#
# }
