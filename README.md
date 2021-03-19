# NEONiso

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/188347333.svg)](https://zenodo.org/badge/latestdoi/188347333)
[![R-CMD-check](https://github.com/SPATIAL-Lab/NEONiso/workflows/R-CMD-check/badge.svg)](https://github.com/SPATIAL-Lab/NEONiso/actions)
[![codecov](https://codecov.io/gh/SPATIAL-Lab/NEONiso/branch/main/graph/badge.svg?token=ZHDFEU5NZW)](https://codecov.io/gh/SPATIAL-Lab/NEONiso)
[![CRAN status](https://www.r-pkg.org/badges/version/NEONiso)](https://CRAN.R-project.org/package=NEONiso)
<!-- badges: end -->

Author: Rich Fiorella \
Last Updated: March 19, 2021.

This repository contains an R package to calibrate NEON atmospheric isotope data. A stable version of the package can be installed from CRAN, and a development version of this package can be installed here using devtools (see below).

Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Citation information:
A manuscript describing the carbon isotope calibration techniques used in this package has just been accepted at JGR-Biogeosciences (doi: [10.1029/2020JG005862](https://doi.org/10.1029/2020JG005862)). Users of this package should also cite the Zenodo DOI above.

Please also check to ensure that you are compliant with NEON's data citation policy for any
products derived from this package: https://www.neonscience.org/data/about-data/data-policies

## Installing the development version:
1) You will need the rhdf5 package, which is not on CRAN. rhdf5 is available from bioconductor using:
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("rhdf5")
```
2) Install devtools, which is available on CRAN.
3) Install NEONiso from GitHub. Development version can be installed using:
```R
devtools::install_github("SPATIAL-Lab/NEONiso")
```
Alternatively, you can install a specific version of the package (e.g., v0.1)
by specifying the version tag:
```R
devtools::install_github("SPATIAL-Lab/NEONiso@v0.1")
```

## Usage:

Two methods are available to calibrate NEON Carbon isotope data and they take slightly different approaches: a) the 'Bowling_2003' method calibrates 12CO2 and 13CO2 mole fractions independently, while b) the 'linreg' method calibrates d13C and CO2 directly without converting to isotopologue mole fractions. The method is specified as an argument to calibrate_carbon_bymonth(). Both methods yield very similar results, but the error and precision estimates are slightly better from the calibrate_carbon_Bowling2003() function (Fiorella et al., 2021; JGR-Biogeosciences)

This function is meant to be applied to a list or vector of uncalibrated data files, and produce output hdf5 files that have (currently) only the CO2 and d13C variables instead of the entire data bundle. Development was targeted and tested on monthly basic files, but the functions should also work on the extended data files.

neonUtilities:::stackEddy *should* work on these output files - please file an issue if it does not.

## Future plans
There will be two major changes and a minor change coming to this package in the next few months:
1) We are starting to work on calibration routines for the NEON water isotope products - more info soon. This will likely not make it into the CRAN package until at least two minor releases from now (0.6.0).
2) We are also working on versions of the carbon routines that work on the entire data series in a single pass (e.g., instead of monthly files - a single output file is created for the whole time series). This allows for more flexibility for which calibration data may be included in the calibration data file, and this functionality may be part of the next minor release (0.5.0).

## DATA ALERT:

Several months of data on the NEON data portal have an issue where the Picarro time clock has diverged from the valve manifold time. A fix has been developed, but has not been propagated to the NEON data portal. In the interim, corrected files *for carbon isotopes only* are available here: https://www.dropbox.com/sh/i9a61g2crv26ess/AADFCT80TPeMz2ayfNsCotg8a?dl=0

