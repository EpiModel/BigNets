# BigNets

Testing EpiModel for big networks

## Setup

This project works with [`renv`](https://rstudio.github.io/renv/index.html).
Before trying to run the scripts, you must initialize it with `renv::init()` and
choose "Restore the project from the lockfile.". This will install the packages
dependencies in their correct versions.

*Note:* some packages are from private GitHub repositories. You need to have
access to them and have you [private Access Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token) setup correctly.

## Selecting the size of the networks

The goal of this project is to experiment with different network size. For
consistency across the scripts, the networks size to use is defined in
"R/utils-netsize.R". This script create the `NETSIZE` variable and define the
corresponding `netsize_string` which is a formatted string to be
used for saving ".rds" files with consistent names (it prevents the conversion
of big numbers in scientific format in strings).

Setting `NETSIZE` with this script will avoid discrepancies in network sizes
between the different scripts of the project.
