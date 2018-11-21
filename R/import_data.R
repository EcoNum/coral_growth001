# analysis on the coral growth
SciViews::R

growth_monitoring_plateau <- read_delim("data/raw/growth_monitoring_plateau.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE,
                                        skip = 11)
