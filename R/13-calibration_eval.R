##
## 13. Epidemic Model Parameter Calibration, Local evaluation
##
#

# Setup ------------------------------------------------------------------------
suppressMessages({
  library("EpiModel")
  library("dplyr")
})

d <- readRDS("data/output/calib/assessments_100k.rds")
d <- readRDS("data/output/calib/assessments.rds")

glimpse(d)

d %>%
  filter(scenario_name == "empty_scenario") %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  pivot_wider(names_from = quant, values_from = value)


