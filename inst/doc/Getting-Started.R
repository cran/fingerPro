## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE
)

## ----echo=FALSE, out.width="100%", fig.align="center"-------------------------
knitr::include_graphics("LOGO2026_FingerPro-EESA.png")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("fingerPro")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("FingerPro_2.1.tar.gz", repos = NULL, type = "source")

## -----------------------------------------------------------------------------
library(fingerPro)

## ----eval=FALSE---------------------------------------------------------------
# setwd("C:/your/project/folder")

## ----eval=FALSE---------------------------------------------------------------
# data <- read_database("my_input_database.csv")

## ----include=FALSE------------------------------------------------------------
library(fingerPro)

data_geo_raw <- read.csv(
  system.file("extdata", "example_geochemical_3s_raw.csv", package = "fingerPro")
)

data_geo_mean <- read.csv(
  system.file("extdata", "example_geochemical_3s_mean.csv", package = "fingerPro")
)

data_iso_raw <- read.csv(
  system.file("extdata", "example_isotopic_3s_raw.csv", package = "fingerPro")
)

data_iso_mean <- read.csv(
  system.file("extdata", "example_isotopic_3s_mean.csv", package = "fingerPro")
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(data_geo_raw),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: example_geochemical_3s_raw.csv</span>",
  escape = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(data_geo_mean),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: example_geochemical_3s_mean.csv</span>",
  escape = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(data_iso_raw),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: example_isotopic_3s_raw.csv</span>",
  escape = FALSE
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(data_iso_mean),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: example_isotopic_3s_mean.csv</span>",
  escape = FALSE
)

