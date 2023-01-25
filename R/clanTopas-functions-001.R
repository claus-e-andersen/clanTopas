require(clanTools)
require(dplyr)
require(stringr)

# January 25, 2023

#install.packages("devtools")
#library(devtools)
#install_github("claus-e-andersen/clanTools")
#install_github("claus-e-andersen/clanOptim")
#install_github("claus-e-andersen/clanCRC32")
#install_github("claus-e-andersen/clanMEview")
#install_github("claus-e-andersen/clanLattice")
#install_github("claus-e-andersen/clanEGSnrc")
#install_github("claus-e-andersen/clanElectrons")

# clanTopas functions
#   read.topas.phasespace
#   read.topas.xyz
#   read.topas.spectrum

scorer.names.simplify <- function(scorer.names="void"){
scorer.names <- stringr::str_replace(scorer.names,"Standard_Deviation","SD")
scorer.names <- stringr::str_replace(scorer.names,"Histories_with_Scorer_Active","HIST.active")
return(scorer.names)
}

#' read.topas.phasespace
#' @description
#' Reading Topas phasespace file (header+phsp)
#'@details
#' This function can extract the main information from the header file.
#'
#' @param pn.full = Path to folder where scorer can be found
#' @param fn.main = First part of scorer filename.
#' @param fn.scorer = Last part of scorer filename (typically the name of the scorer).
#' @param what = Meta data like "Fluence of electrons".
#' @param what2 = Meta data (supplement to what).
#' @param get.col.names.from.header = If TRUE, extract names of fields from file header.
#' @param number.of.columns = If get.col.names.from.header == FALSE, you will need to define the number of fields in the file.
#' @param include.meta.data =If TRUE, include the meta data (what, what2 + filename info) in returned dataframe.
#' @param verbose = TRUE or FALSE,
#' @return a dataframe with scorer data included meta data, if requested.
#'
#' @export
#######################################################################################################
# read.phasespace
#######################################################################################################
read.topas.phasespace <- function(pn.full="~//",fn.main="",fn.scorer="DoseScorer1",what="Dose",what2="Dose",
                            get.col.names.from.header=TRUE, number.of.columns=28,
                            include.meta.data=TRUE,
                            verbose=FALSE){
# Read Topas phasespace file
# Created: January 17, 2023
# Revised: January 24, 2023
# Name :   Claus E. Andersen

# Note that column names are simplified.
# If column names are not read from the header file, you have to provide
# the correct number of columns (e.g. number.of.columns = 28).

# Input:
#   pn.full   = Path to wher files are located
#   fn.main   = Main part of the file name, It canbe a prefix or "".
#   fn.scorer = Name of scorer
#   what      = Meta data
#   what2     = More meta data
#   get.col.names.from.header = TRUE/FALSE
#   number.of.columns = Number of data columns to use if column names are not extracted from header
#   include.meta.data = Include fn.main, fn.scorer, what, and what2 in the data frame.
#   verbose = TRUE/FALSE

# Sample call:
#   pn.full <- "~//topas//examples//clan//Four-pi-detector//"
#   fn.main <- "Four-pi-detector-10011-"
#   df <-read.topas.phasespace(
#                        pn.full=pn.full,
#                        fn.main=fn.main,
#                        fn.scorer="Phasespace1",
#                        what="Phasespace",
#                        what2="Phasespace, z.minus",
#                        get.col.names.from.header=TRUE,
#                        number.of.columns=28,
#                        include.meta.data =TRUE,
#                        verbose=!TRUE)
print("Welcome to read.topas.phasespace")
fn <- paste(pn.full,fn.main,fn.scorer,".phsp",sep="")
fn.header <- paste(pn.full,fn.main,fn.scorer,".header",sep="")
print(paste("Phasespace file name = ",fn))
print(paste("Header file name = ",fn.header))

  ##################################################################
  # Reader header files (column names)
  ##################################################################

  if(!get.col.names.from.header){
    print("Will not try to read header file.")
    print(paste("You have specified that file consists of :",number.of.columns," data items (columns).",sep=" "))
    col.names <- paste("col",1:number.of.columns,sep="")
  }

  if(get.col.names.from.header){
  header.txt <- readLines(fn.header)
    if(verbose){
      print(header.txt)
    }
    line.no.vec <- grep("[ ]*[0-9]+:",header.txt)

    col.names <- paste("col",1:99,sep="")
    for(i in 1:length(line.no.vec)){
    line.no <- line.no.vec[i]
    tt <- strsplit(header.txt[line.no],":")[[1]][2]
    # Simplify names
    tt <- clanTools::trim.whitespace(tt)
    tt <- stringr::str_replace(tt,"Flag to tell if Third Direction Cosine is Negative \\(1 means true\\)","Flag.Third.Dir.Cosine.Neg")
    tt <- stringr::str_replace(tt,"Flag to tell if this is the First Scored Particle from this History \\(1 means true\\)","Flag.First.Scored")
    tt <- clanTools::substitute.char(tt," ",".")
    tt <- clanTools::substitute.char(tt,"\\[","")
    tt <- clanTools::substitute.char(tt,"]","")
    tt <- stringr::str_replace(tt,".\\(in.PDG.Format\\)","")
    tt <- stringr::str_replace(tt,"Position","Pos")
    tt <- stringr::str_replace(tt,"Direction","Dir")
    tt <- stringr::str_replace(tt,"Kinetic.Energy","KE")
    tt <- stringr::str_replace(tt,"Initial","Init")
    tt <- stringr::str_replace(tt,".to.tell","")
    tt <- stringr::str_replace(tt,"Charge.e\\+","Charge")
    col.names[i] <- tt
  }
  col.names <- col.names[1:i]
  col.names

  if(verbose){
    print("-------------------------------")
    print("Identified column names from header:")
    print(paste(1:length(col.names),"=",col.names,collapse="   "))
    print("-------------------------------")
  }#verbose
  } # get.col.names.from.header

  ##################################################################
  # Reader phasespace files
  ##################################################################
  df0 <- read.csv(fn, header=!TRUE, skip=0, sep="")
  names(df0) <- col.names

  print(names(df0))
  if(include.meta.data){
    df0 %>%
    mutate(fn.main = fn.main) %>%
    mutate(fn.scorer = fn.scorer) %>%
    mutate(what = what) %>%
    mutate(what2 = what2) %>%
    data.frame(.) -> df0
  }

  if(verbose){
    print(head(df0))
  }

  print("ByeBye from read.phasespace")
  return(df0)
} # read.phasespace
#######################################################################################################
# END read.topas.phasespace
#######################################################################################################


#' read.topas.xyz
#' @description
#' Reading Topas scorer file (e.g. dose-to-medium or fluence vs. xyz)
#'@details
#' This function can use the main information from the header file.
#'
#' @param pn.full = Path to folder where scorer can be found
#' @param fn.main = First part of scorer filename.
#' @param fn.scorer = Last part of scorer filename (typically the name of the scorer).
#' @param what = Meta data like "Fluence of electrons".
#' @param what2 = Meta data (supplement to what).
#' @param get.col.names.from.header = If TRUE, extract names of fields from file header.
#' @param extra.col.names.pre = Names of fields in the scorer file occuring BEFORE the main data (e.g. c("x","y","z") for binned data.
#' @param extra.col.names.post = Names of fields in the scorer file occuring AFTER the main data.
#' @param scorer.names.default = If get.col.names.from.header == FALSE, you will need to provide field names (e.g. c("Count_in_Bin","Mean","Sum"),)
#' @param include.meta.data =If TRUE, include the meta data (what, what2 + filename info) in returned dataframe.
#' @param verbose = TRUE or FALSE,
#' @return a dataframe with scorer data included meta data, if requested.
#' @export
#######################################################################################################
# read.topas.xyz
#######################################################################################################
read.topas.xyz <- function(pn.full="~//",fn.main="",fn.scorer="DoseScorer1",what="Dose",what2="Dose",
                           get.col.names.from.header=TRUE,
                           extra.col.names.pre=c("x","y","z"),
                           extra.col.names.post=NULL,
                           scorer.names.default =c("Count_in_Bin","Mean","Sum"),
                           include.meta.data=TRUE, verbose=FALSE){
  # Read Topas simple scorer file (xyz, no time resolution)
  # Created: January 17, 2023
  # Revised: January 24, 2023
  # Name :   Claus E. Andersen

  # If column names are not read from the header file, then they will
  # be called scorer.names.default.

  # The item "label" is extracted from the header.

  # Input:
  #   pn.full   = Path to wher files are located
  #   fn.main   = Main part of the file name, It canbe a prefix or "".
  #   fn.scorer = Name of scorer
  #   what      = Meta data
  #   what2     = More meta data
  #   get.col.names.from.header = TRUE/FALSE
  #   include.meta.data = Include fn.main, fn.scorer, what, what2 and label in the data frame.
  #   verbose = TRUE/FALSE

  # Sample call:
  # n.full <- "~//topas//examples//clan//"
  # fn.main <- "sandbox-10012-"
  # df1 <-read.topas.xyz(pn.full=pn.full,
  #                      fn.main=fn.main,
  #                      fn.scorer="DoseScorer1",
  #                      what="Dose",
  #                      what2="Dose",
  #                      get.col.names.from.header=TRUE,
  #                      include.meta.data=TRUE,
  #                      verbose=FALSE)


  print("Welcome to read.topas.xyz")
  fn <- paste(pn.full,fn.main,fn.scorer,".csv",sep="")
  print(paste("File name = ",fn))

  txt <- readLines(fn)
  N.skip <- sum(substring(txt,1,1) =="#")

  print("Last line:")
  print(txt[N.skip])
  label <- "Void"

  if(is.null(extra.col.names.pre)){extra.col.names.pre <- character(0L)}
  if(is.null(extra.col.names.post)){extra.col.names.post <- character(0L)}
  scorer.names <- c(extra.col.names.pre,scorer.names.default,extra.col.names.post)

  if(get.col.names.from.header){
    header.txt <- txt[1:N.skip]
    header.last.line <- txt[N.skip]

    label <- strsplit(header.last.line,":")[[1]][1]
    label <- stringr::str_replace(label,"#","")
    label <- clanTools::trim.whitespace(label)

    xx <- strsplit(header.last.line,":")[[1]][2]
#    xx <- " Count_in_Bin   Sum   Mean   Standard_Deviation   "
    xx <- clanTools::trim.whitespace(xx)
    xx <- stringr::str_split(xx, " ")[[1]]
    ok <- !xx==""
    xx <- xx[ok]
    xx
    scorer.names <- c(extra.col.names.pre,xx,extra.col.names.post)
    scorer.names <- scorer.names.simplify(scorer.names)

    if(verbose){
      print("-------------------------------")
      print("Identified column names from header + requested pre/post columns:")
      print(paste(1:length(scorer.names),"=",scorer.names,collapse="   "))
      print("-------------------------------")
    }#verbose

  } # get.col.names.from.header


    if(verbose){
    print(txt)
    }

  xx <- read.csv(fn,header=!TRUE,skip=N.skip,sep=",")
  names(xx) <- scorer.names
  df0 <- xx
  print(names(df0))

  if(include.meta.data){
    df0 %>%
      mutate(fn.main = fn.main) %>%
      mutate(fn.scorer = fn.scorer) %>%
      mutate(what = what) %>%
      mutate(what2 = what2) %>%
      mutate(label = label) %>%
      data.frame(.) -> df0
  }

  print("ByeBye from read.topas.xyz")
  return(df0)
}

#######################################################################################################
# END read.topas.xyz
#######################################################################################################



#' read.topas.simple
#' @description
#' Reading simple Topas scorer file (i.e. no xyz binning etc.)
#'@details
#' This function can use the main information from the header file.
#'
#' @param pn.full = Path to folder where scorer can be found
#' @param fn.main = First part of scorer filename.
#' @param fn.scorer = Last part of scorer filename (typically the name of the scorer).
#' @param what = Meta data like "Fluence of electrons".
#' @param what2 = Meta data (supplement to what).
#' @param get.col.names.from.header = If TRUE, extract names of fields from file header.
#' @param scorer.names.default = If get.col.names.from.header == FALSE, you will need to provide field names (e.g. c("Count_in_Bin","Mean","Sum"),)
#' @param include.meta.data =If TRUE, include the meta data (what, what2 + filename info) in returned dataframe.
#' @param verbose = TRUE or FALSE,
#' @return a dataframe with scorer data included meta data, if requested.
#' @export
#######################################################################################################
# read.topas.simple
#######################################################################################################
read.topas.simple <- function(...){
  print("ByeBye from read.topas.simple")
  # No extra.col.names.pre or .post. Otherwise, read.topas.simple = read.topas.xyz.
  df <- read.topas.xyz(...,extra.col.names.pre=NULL,extra.col.names.post=NULL)
  print("ByeBye from read.topas.simple")
  return(df)
}
#######################################################################################################
# END read.topas.simple
#######################################################################################################


#' read.topas.spectrum
#' @description
#' Reading Topas spectrum file (e.g. fluence spectra)
#'@details
#' This function can use the main information from the header file.
#'
#' @param pn.full = Path to folder where scorer can be found
#' @param fn.main = First part of scorer filename.
#' @param fn.scorer = Last part of scorer filename (typically the name of the scorer).
#' @param what = Meta data like "Fluence of electrons".
#' @param what2 = Meta data (supplement to what).
#' @param get.col.names.from.header = If TRUE, extract names of fields from file header.
#' @param scorer.names.default = If get.col.names.from.header == FALSE, you will need to provide field names (e.g. c("Count_in_Bin","Mean","Sum"),)
#' @param include.meta.data =If TRUE, include the meta data (what, what2 + filename info) in returned dataframe.
#' @param verbose = TRUE or FALSE,
#' @return a dataframe with scorer data included meta data, if requested.
#' @export
#######################################################################################################
# read.topas.spectrum
#######################################################################################################
read.topas.spectrum <- function(pn.full="~//",fn.main="",fn.scorer="DoseScorer1",what="Dose",what2="Dose",
                           get.col.names.from.header=TRUE,
                           scorer.names.default=c("Count_in_Bin","Mean","Sum"),
                           include.meta.data=TRUE, verbose=FALSE){
  # Read Topas simple scorer file (spectral data, no time resolution)
  # Created: January 17, 2023
  # Revised: January 24, 2023
  # Name :   Claus E. Andersen

  # If column names are not read from the header file, then they will
  # be called as given by scorer.names.default.

  # The item "label" is extracted from the header.

  # Input:
  #   pn.full   = Path to wher files are located
  #   fn.main   = Main part of the file name, It canbe a prefix or "".
  #   fn.scorer = Name of scorer
  #   what      = Meta data
  #   what2     = More meta data
  #   get.col.names.from.header = TRUE/FALSE
  #   include.meta.data = Include fn.main, fn.scorer, what, what2 and label in the data frame.
  #   verbose = TRUE/FALSE

  # Sample call:
  # n.full <- "~//topas//examples//clan//"
  # fn.main <- "sandbox-10012-"
  # df1 <-read.topas.xyz(pn.full=pn.full,
  #                      fn.main=fn.main,
  #                      fn.scorer="DoseScorer1",
  #                      what="Dose",
  #                      what2="Dose",
  #                      get.col.names.from.header=TRUE,
  #                      include.meta.data=TRUE,
  #                      verbose=FALSE)


  print("Welcome to read.topas.spectrum")
  fn <- paste(pn.full,fn.main,fn.scorer,".csv",sep="")
  print(paste("File name = ",fn))

  txt <- readLines(fn)
  N.skip <- sum(substring(txt,1,1) =="#")

  print("Third last line:")
  print(txt[N.skip-2])
  print("Second last line:")
  print(txt[N.skip-1])
  print("Last line:")
  print(txt[N.skip])

    scorer.names <- scorer.names.default
    label <- "Void"
    N.bins <- 2000
    E.min <- 0
    E.max <- 20

  if(get.col.names.from.header){
    header.txt <- txt[1:N.skip]
    header.label.line <- txt[N.skip-2]
    header.bins.line <- txt[N.skip-1]

    label <- strsplit(header.label.line,":")[[1]][1]
    label <- stringr::str_replace(label,"#","")
    label <- clanTools::trim.whitespace(label)

    # Format of header.bins.line: Binned by pre-step energy in 2000 bins of 0.01 MeV from 0 MeV to 20 MeV"
    N.bins <- clanTools::extract.given.number(header.bins.line,1)
    E.min  <- clanTools::extract.given.number(header.bins.line,3)
    E.max  <- clanTools::extract.given.number(header.bins.line,4)


    xx <- strsplit(header.label.line,":")[[1]][2]
    xx <- clanTools::trim.whitespace(xx)
    xx <- stringr::str_split(xx, " ")[[1]]
    ok <- !xx==""
    xx <- xx[ok]
    xx
    scorer.names <- c(xx)
    if(verbose){
      print("-------------------------------")
      print("Identified column names from header:")
      print(paste(1:length(scorer.names),"=",scorer.names,collapse="   "))
      print("-------------------------------")
    }#verbose
  } # get.col.names.from.header


  if(verbose){
    print(txt)
  }


    print(scorer.names)
    scorer.names <- scorer.names.simplify(scorer.names)

  xx <- read.csv(fn,header=!TRUE,skip=N.skip,sep=",")
  df0 <- xx
  N.col <- length(scorer.names)
  #N.bins <- 2000
  #E.min <- 0
  #E.max <- 20
  deltaE <- E.max/N.bins
  EE.min <- seq(0,E.max-deltaE,length=N.bins)
  EE.max <- seq(deltaE,E.max,length=N.bins)
  EE.min <- c(0,0,EE.min,0)
  EE.max <- c(0,0,EE.max,0)
  yy <- unlist(xx)
  names(yy) <- NULL
  yy
  ii <- rep(1:N.col,N.bins+3)

  # df0 <-  data.frame(E.min=EE.min,E.max=EE.max,counts=yy[ii==2],
  #                    sum=yy[ii==2], mean=yy[ii==3], sd=yy[ii==4], N=yy[ii==5])
  # df0 <-  data.frame(E.min=EE.min,E.max=EE.max, mean=yy[ii==1])

   txt <- paste(scorer.names,"=yy[ii==",1:length(scorer.names),"]",sep="",collapse=", ")
   txt <- paste("data.frame(E.min=EE.min, E.max=EE.max,",txt,")",sep="")
   df0 <- eval(parse(text=txt))

  ###############
  if(get.col.names.from.header){names(xx) <- scorer.names}
#  df0 <- xx
#  print(names(df0))

  if(include.meta.data){
    df0 %>%
      mutate(fn.main = fn.main) %>%
      mutate(fn.scorer = fn.scorer) %>%
      mutate(what = what) %>%
      mutate(what2 = what2) %>%
      mutate(label = label) %>%
      data.frame(.) -> df0
  }

  print("ByeBye from read.topas.spectrum")
  return(df0)
}

#######################################################################################################
# END read.topas.spectrum
#######################################################################################################


#' read.topas.demo
#' @description
#' Demonstration of the read.topas functions
#'
#'@details
#' This function contains demonstration of the functions in clanTopas.
#' See function body.
#'
#'
#' @return Text message
#'
#' @export

read.topas.demo <- function(){

######################################################################
pn.full <- "~//topas//examples//clan//Four-pi-detector//"
fn.main <- "Four-pi-detector-10011-"
df <-read.topas.phasespace(pn.full=pn.full, fn.main=fn.main, fn.scorer="Phasespace1",
                           what="Phasespace",what2="Phasespace, z.minus",
                           get.col.names.from.header=TRUE, number.of.columns=28,
                           include.meta.data =TRUE,
                           verbose=!TRUE)

######################################################################



pn.full <- "~//topas//examples//clan//"
fn.main <- "sandbox-10012-"
df1 <-read.topas.xyz(pn.full=pn.full,
                     fn.main=fn.main,
                     fn.scorer="DoseScorer1",
                     what="Dose",
                     what2="Dose",
                     get.col.names.from.header=TRUE,
                     include.meta.data=TRUE,
                     verbose=FALSE)


df2 <-read.topas.xyz(pn.full=pn.full,
                     fn.main=fn.main,
                     fn.scorer="FluenceScorer1",
                     what="Fluence",
                     what2="Fluence, prim. electrons",
                     get.col.names.from.header=TRUE,
                     include.meta.data=TRUE,
                     verbose=FALSE)
head(df1)
head(df2)

#############################################

pn.full <- "~//topas//examples//clan//linac-spectra-Ali-and-Rogers//"
fn.main <- "linac-spectra-test-10001-"
df1 <-read.topas.spectrum(pn.full=pn.full,
                          fn.main=fn.main,
                          fn.scorer="Fluence-spectrum",
                          what="Dose",
                          what2="Dose",
                          get.col.names.from.header=TRUE,
                          scorer.names.default=c("Count_in_Bin","Mean","Sum"),
                          include.meta.data=TRUE,
                          verbose=FALSE)

head(df1)
########################

#linac-spectra-test-10010-06MVDoseScorer1.csv
pn.full <- "~//topas//examples//clan//linac-spectra-Ali-and-Rogers//"
fn.main <- "linac-spectra-test-10010-"

df1 <-read.topas.xyz(pn.full=pn.full,
                     fn.main=fn.main,
                     fn.scorer="06MVDoseScorer1",
                     what="Dose",
                     what2="Dose",
                     get.col.names.from.header=TRUE,
                     extra.col.names.pre=c("x","y","z"),
                     extra.col.names.post=NULL,
                     include.meta.data=TRUE,
                     verbose=FALSE)

return("This function contains demonstration of the functions in clanTopas. See function body! ByeBye from read.topas.demo.")
} # clanTopasDemo
