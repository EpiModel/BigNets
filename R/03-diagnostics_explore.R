##
## 02. Network Model Diagnostics: Interactive Analysis
##

# scp -r sph:/projects/epimodel/BigNets/data/input input/data/

# Setup ------------------------------------------------------------------------
suppressMessages({
  library("EpiModelHIV")
})



# Interactive Dx Analysis ------------------------------------------------------

# Main
dx <- readRDS(paste0("data/input/netdx-main-", NETSIZE, ".rds"))
print(dx$dx_main, digits = 2)
plot(dx$dx_main)

print(dx$dx_main_static, digits = 2)
plot(dx$dx_main_static)

# Casual
dx <- readRDS(paste0("data/input/netdx-casl-", NETSIZE, ".rds"))
print(dx$dx_casl, digits = 2)
plot(dx$dx_casl)

print(dx$dx_casl_static, digits = 2)
plot(dx$dx_casl_static)

# Inst
dx <- readRDS(paste0("data/input/netdx-inst-", NETSIZE, ".rds"))
print(dx$dx_inst, digits = 2)
plot(dx$dx_inst)
