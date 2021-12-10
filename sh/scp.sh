#!/bin/bash

# Send
scp R/*.R sph:/projects/epimodel/sjennes/BigNets/R

# Receive

scp sph:/projects/epimodel/sjennes/BigNets/data/input/*.rds data/input
