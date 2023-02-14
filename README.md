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

# Alternatively, you can do the same thing without splitting the file name into fn.main
# and fn.scorer. This can be done as follows:

df <-read.topas.phasespace(pn.full, 
                           "", 
                           fn.scorer="Four-pi-detector-10011-Phasespace1",
                           what="Phasespace",
                           what2="Phasespace.z.minus")

# The meta data what and what2 are just for your own use, and can be ignored if you do not want them.                           
```                          

Example of output from the first 3 lines of a data frame read with the read.topas.phasespace function.
Notice how fn.main, fn.scorer, what, and what2 are included in the data frame.
```
  Pos.X.cm Pos.Y.cm  Pos.Z.cm Dir.Cosine.X Dir.Cosine.Y Energy.MeV Weight Particle.Type
1  15.0753  8.03777 -10.39860     0.753764     0.401888   0.510999      1            22
2 -15.0753 -8.03777  10.39860    -0.753764    -0.401888   0.510999      1            22
3  18.8698  6.06366  -2.67614     0.943491     0.303183   0.510999      1            22
  Flag.Third.Dir.Cosine.Neg Flag.First.Scored Time.of.Flight.ns Run.ID Event.ID Track.ID
1                         1                 1          0.670464      0       40        3
2                         0                 0          0.670464      0       40        2
3                         1                 1          0.670464      0       41        3
  Parent.ID Charge Creator.Process.Name Init.KE.MeV Vertex.Pos.X.cm Vertex.Pos.Y.cm
1         1      0              annihil    0.510999     -5.6355e-08     4.82672e-08
2         1      0              annihil    0.510999     -5.6355e-08     4.82672e-08
3         1      0              annihil    0.510999     -4.9940e-08    -2.86780e-08
  Vertex.Pos.Z.cm Init.Dir.Cosine.X Init.Dir.Cosine.Y Init.Dir.Cosine.Z Seed.Part.1
1     3.07908e-08          0.753764          0.401888         -0.519928  1878463799
2     3.07908e-08         -0.753764         -0.401888          0.519928  1878463799
3     3.07908e-08          0.943491          0.303183         -0.133807  1878463799
  Seed.Part.2 Seed.Part.3 Seed.Part.4                 fn.main   fn.scorer what what2
1           1    61314202     8717185 Four-pi-detector-10000- Phasespace1 Dose  Dose
2           1    61314202     8717185 Four-pi-detector-10000- Phasespace1 Dose  Dose
3           1    46770376    20239334 Four-pi-detector-10000- Phasespace1 Dose  Dose
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

plus the stringr and dplyr packages:

```
library(stringr)
library(dplyr)
```
if you do already have these packages you will first need to install them:
```
install.packages("stringr")
install.packages("dplyr")
```
