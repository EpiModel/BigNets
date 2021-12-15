#!/bin/bash

# Send
scp R/*.R sph:/projects/epimodel/sjenness/BigNets/R

# Receive

scp sph:/projects/epimodel/sjenness/BigNets/data/input/*.rds data/input/
scp sph:/projects/epimodel/sjenness/BigNets/data/output/*.rda data/output/
