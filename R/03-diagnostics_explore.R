##
## 02. Network Model Diagnostics: Interactive Analysis
##

# Setup ------------------------------------------------------------------------
rm(list = ls())
suppressMessages({
  library(EpiModelHIV)
})

netstats <- readRDS("data/input/netstats.rds")
dx <- readRDS("data/input/netdx.rds")

# Interactive Dx Analysis ------------------------------------------------------

# Main
print(dx$dx_main, digits = 2)
plot(dx$dx_main)

netstats$main

print(dx$dx_main_static, digits = 2)
plot(dx$dx_main_static)

# Casual
print(dx$dx_casl, digits = 2)
plot(dx$dx_casl)

netstats$casl

print(dx$dx_casl_static, digits = 2)
plot(dx$dx_casl_static)

# Inst
print(dx$dx_inst, digits = 2)
plot(dx$dx_inst)

