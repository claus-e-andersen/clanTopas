# clanTopas
R package for reading output from Topas (TopasMC.org) Monte-Carlo simulations of ionizing radiation. 

Main functions for reading scorer files produced by Topas
```
read.topas.simple (no binning)
read.topas.xyz (for a binned scorer)
read.topas.spectrum
read.topas.phasespace
```
Details of how to install the clanTopa package is given at the end of this readme file.

## How to read a phasespace file?

```
pn.full <- "~//topas//examples//clan//Four-pi-detector//"
fn.main <- "Four-pi-detector-10011-"
df <-read.topas.phasespace(pn.full, 
                           fn.main, 
                           fn.scorer="Phasespace1",
                           what="Phasespace",
                           what2="Phasespace, z.minus")
```                          

## How to read two binned scorer files and then combine the results?

```
pn.full <- "~//topas//examples//clan//"
fn.main <- "sandbox-10012-"

df1 <-read.topas.xyz(pn.full,
                     fn.main,
                     fn.scorer="DoseScorer1",
                     what="Dose",
                     what2="Dose-to-medium")


df2 <-read.topas.xyz(pn.full,
                     fn.main,
                     fn.scorer="FluenceScorer1",
                     what="Fluence",
                     what2="Fluence, prim. electrons")
df <- rbind(df1,df2)
```

## How to read a spectrum file?

```
pn.full <- "~//topas//examples//clan//linac-spectra-Ali-and-Rogers//"
fn.main <- "linac-spectra-test-10001-"
df <-read.topas.spectrum(pn.full,
                          fn.main,
                          fn.scorer="Fluence-spectrum",
                          what="Dose",
                          what2="Dose")

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
```

plus the stringr and dplyr packages.
