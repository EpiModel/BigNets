##
## 13. Epidemic Model Parameter Calibration, Local evaluation
##
#

# Setup ------------------------------------------------------------------------
suppressMessages({
  library(EpiModel)
  library(dplyr)
  library(tidyr)
})

d <- readRDS("data/output/calib/assessments.rds")

glimpse(d)

d %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  filter(quant == "q2") %>%
  pivot_wider(names_from = scenario_name, values_from = value)


d %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  pivot_wider(names_from = quant, values_from = value)

d10k <- d %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  filter(quant == "q2") %>%
  pivot_wider(names_from = scenario_name, values_from = value)

d100k <- readRDS("data/output/calib/assessments100k.rds") %>%
  pivot_longer(-scenario_name) %>%
  separate(name, into = c('name', 'quant'), sep = "__") %>%
  filter(quant == "q2") %>%
  pivot_wider(names_from = scenario_name, values_from = value)

left_join(d10k, d100k, by = c("name", "quant"))

