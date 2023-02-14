# clanTopas
R package for reading output from Topas (TopasMC.org) Monte-Carlo simulations of ionizing radiation transport and dosimetry. 

Main functions for reading scorer files produced by Topas.  
```
read.topas.simple (no binning)
read.topas.xyz (for a binned scorer)
read.topas.spectrum
read.topas.phasespace
read.topas.demo (examples)
```
Details of how to install the clanTopas package is given at the end of this readme file.

Additional Topas tools are available here: https://github.com/claus-e-andersen/TopasTools

## Main functionality
- The functions can extract field names automatecially from the Topas output.
- The functions facilitate the use of meta data to combine results from different runs or scorers.
- The functions simplify the naming of fields to a more concise form.

## How to read a phasespace file?

```
# Assuming the phasespace file and header are called:
#   Four-pi-detector-10011-Phasespace1.phsp
#   Four-pi-detector-10011-Phasespace1.header
# placed in some topas folder called ... Four-pi-detector (see pn.full),
# then you can read the data into a data frame as follows:

pn.full <- "~//topas//examples//clan//Four-pi-detector//"
fn.main <- "Four-pi-detector-10011-"

df <-read.topas.phasespace(pn.full, 
                           fn.main, 
                           fn.scorer="Phasespace1",
                           what="Phasespace",
                           what2="Phasespace.z.minus")
                           
# The meta data what and what2 are just for your own use, and can be ignored if you do not want them.                           
```                          

## How to read two binned scorer files and then combine the results?

```
# Assuming the files are called:
#   sandbox-10012-DoseScorer1.csv
#   sandbox-10012-DoseScorer2.csv
# placed in some topas folder called ... clan (see pn.full),
# then you can read the data into a data frame as follows:

pn.full <- "~//topas//examples//clan//"
fn.main <- "sandbox-10012-"

df1 <- read.topas.xyz(pn.full,
                      fn.main,
                      fn.scorer="DoseScorer1",
                      what="Dose",
                      what2="Dose.at.isocenter")


df2 <- read.topas.xyz(pn.full,
                      fn.main,
                      fn.scorer="DoseScorer2",
                      what="Dose",
                      what2="Dose.at.10cm")

df <- rbind(df1,df2)

# The meta data what and what2 are just for your own use, and can be ignored if you do not want them.                           
```

## How to read a spectrum file?

```
# Assuming the file is called:
#   linac-spectra-test-10001-Fluence-spectrum.csv
# placed in some topas folder called ... linac-spectra-Ali-and-Rogers (see pn.full),
# then you can read the data into a data frame as follows:

pn.full <- "~//topas//examples//clan//linac-spectra-Ali-and-Rogers//"
fn.main <- "linac-spectra-test-10001-"

df <- read.topas.spectrum(pn.full,
                          fn.main,
                          fn.scorer="Fluence-spectrum",
                          what="Fluence",
                          what2="Fluence.electrons.primaries")

# The meta data what and what2 are just for your own use, and can be ignored if you do not want them.                           
```

## Installation in R or Rstudio

The library can be loaded into R using the install_github command which is in the devtools package. So you first need to ascertain that you have this package and you need to load it with the library command:

```
install.packages("devtools")
library(devtools)
install_github("claus-e-andersen/clanTopas")
library(clanTopas)
```
You will also need the clanTools package:

```
install_github("claus-e-andersen/clanTools")
library(clanTools)
```

plus the stringr and dplyr packages.

```
library(stringr)
library(dplyr)
```
if you do already have these packages you will first need to install them:
```
install.packages("stringr")
install.packages("dplyr")
```
