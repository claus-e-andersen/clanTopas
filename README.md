# clanTopas
R package for reading output from Topas (TopasMC.org) Monte-Carlo simulations of ionizing radiation. 

Main functions for reading scorer files produced by Topas
```
read.topas.simple (no binning)
read.topas.xyz (for a binned scorer)
read.topas.spectrum
read.topas.phasespace
```
Installation in R or Rstudio

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
