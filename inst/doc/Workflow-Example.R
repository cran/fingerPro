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

## -----------------------------------------------------------------------------
library(fingerPro)

## -----------------------------------------------------------------------------
data <- read_database(
  system.file("extdata", "example_geochemical_3s_raw.csv", package = "fingerPro")
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(data),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: example_geochemical_3s_raw.csv</span>",
  escape = FALSE
)

## -----------------------------------------------------------------------------
box_plot(data)

## ----eval=FALSE---------------------------------------------------------------
# help("unmix")

## -----------------------------------------------------------------------------
box_plot(data, page = 2)

## -----------------------------------------------------------------------------
box_plot(data, page = 3)

## -----------------------------------------------------------------------------
correlation_plot(data)

## -----------------------------------------------------------------------------
LDA_plot(data)

## -----------------------------------------------------------------------------
PCA_plot(data)

## ----results='hide'-----------------------------------------------------------
ternary_diagram(data)

## ----results='hide'-----------------------------------------------------------
ternary_diagram(data, page = 2)

## ----results='hide'-----------------------------------------------------------
ternary_diagram(data, page = 3)

## ----results='hide'-----------------------------------------------------------
range_test(data)

## ----results='hide'-----------------------------------------------------------
tracers_seeds <- CTS_explore(data, iter = 1000)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(tracers_seeds),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: Minimal tracer combinations </span>",
  escape = FALSE
)

## -----------------------------------------------------------------------------
selected_data <- CTS_select(data, tracers_seeds, seed_id = 1, error_threshold = 0.05)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(selected_data),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: dataset after CTS_select</span>",
  escape = FALSE
)

## ----results='hide'-----------------------------------------------------------
output_unmix <- unmix(selected_data)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(output_unmix),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: unmixing results</span>",
  escape = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# help("unmix")

## -----------------------------------------------------------------------------
plot_results(output_unmix, violin = FALSE, )

## -----------------------------------------------------------------------------
plot_results(output_unmix, violin = TRUE,)

## -----------------------------------------------------------------------------
apportionments <- c(0.435, 0.285, 0.280)
normalized_error <- validate_results(selected_data, apportionments)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  head(normalized_error),
  caption = "<span style='color:#22513f; display:block; text-align:left;'>Preview: normalized error values from validate_results</span>",
  escape = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# ###################################
# ###### 0. Install and set wd
# ###################################
# install.packages("fingerPro") # one time
# setwd("C:/your/file/directory") # your own working directory (wd)
# 
# 
# ###################################
# ###### 1. Load and verify the data
# ###################################
# library(fingerPro)
# data <- read_database(system.file("extdata", "example_geochemical_3s_raw.csv", package = "fingerPro")) # Input example dataset
# 
# 
# ###################################
# ###### 2. Exploratory analysis
# ###################################
# 
# 
# ###### Box plots
# 
# box_plot(data)
# 
# box_plot(data, page = 1) # Visualise a specific page (e.g. page 1)
# box_plot(data, page = 2) # Visualise a specific page (e.g. page 2)
# box_plot(data, page = 3) # Visualise a specific page (e.g. page 3)
# box_plot(data, n_row = 3, n_col = 6,) # Visualise all tracers
# 
# # Save results as a PNG image
# png("output_boxplot_all.png", width = 30, height = 15, units = "cm", res = 300) # to save .png results
# box_plot(data, n_row = 3, n_col = 6,) # Visualise all tracers
# dev.off()
# 
# # Check 'help' for more information
# help("box_plot")
# 
# 
# ###### Correlation analysis
# 
# correlation_plot(data)
# 
# correlation_plot(data, columns = c(1:8)) # correlation plot of  n tracers (e.g. 1 to 8)
# 
# # Save results as a PNG image
# png("output_correlationplot_tracers1-8.png", width = 25, height = 15, units = "cm", res = 300) # to save .png results
# correlation_plot(data, columns = c(1:8)) # correlation plot of  n tracers (e.g. 1 to 8)
# dev.off()
# 
# # Check 'help' for more information
# help("correlation_plot")
# 
# 
# ###### Linear Discriminant Analysis (LDA)
# 
# LDA_plot(data)
# 
# # Save results as a PNG image
# png("output_LDA.png", width = 15, height = 12, units = "cm", res = 300) # to save .png results
# LDA_plot(data)
# dev.off()
# 
# 
# ###### Principal Component Analysis (PCA)
# 
# PCA_plot(data)
# 
# # Save results as a PNG image
# png("output_PCA.png", width = 15, height = 12, units = "cm", res = 300) # to save .png results
# PCA_plot(data)
# dev.off()
# 
# 
# ###### Individual tracer analysis and ternary diagrams
# 
# output_ternary <- ternary_diagram(data)
# 
# ternary_diagram(data, page = 1) # Visualise a specific page (e.g. page 1)
# ternary_diagram(data, page = 2) # Visualise a specific page (e.g. page 2)
# ternary_diagram(data, page = 3) # Visualise a specific page (e.g. page 3)
# ternary_diagram(data, rows = 4, cols = 5)  # Visualise all tracers
# 
# # e.g. Save ternary_diagram results as a PNG image
# png("output_ternary_all.png", width = 18, height = 12, units = "cm", res = 300) # to save .png results
# output_ternary_all <- ternary_diagram(data, rows = 4, cols = 5)  # Visualise all tracers
# dev.off()
# 
# # Check 'help' for more information
# help("ternary_diagram")
# 
# 
# ###### Range test
# data_rangetest <- range_test(data)
# write.csv(data_rangetest, "output_rangetest.csv")
# 
# 
# ###################################
# ###### 3. Tracer selection
# ###################################
# 
# 
# ###### CTS_explore
# 
# tracers_seeds <- CTS_explore(data, iter = 1000)
# write.csv(tracers_seeds, "output_CTS_explore_tracers_seeds.csv")
# 
# # Check 'help' for more information
# help("CTS_explore")
# 
# 
# ###### CTS_select
# 
# selected_data <- CTS_select(data, tracers_seeds, seed_id = 1, error_threshold = 0.05) # (e.g. Seed 1 selected with an error of 5% (0.05))
# write.csv(selected_data, "output_CTS_select_selected_data.csv")
# 
# # Check 'help' for more information
# help("CTS_select")
# 
# ###################################
# ###### 4. Unmix
# ###################################
# 
# output_unmix <- unmix(selected_data)
# write.csv(output_unmix, "output_unmix.csv")
# 
# # Check 'help' for more information
# help("unmix")
# 
# plot_results(output_unmix, violin = FALSE) # Density plot
# plot_results(output_unmix, violin = TRUE) # Violing plot
# 
# # save density plot
# png("output_unmix_densityplot.png", width = 18, height = 12, units = "cm", res = 300) # to save .png results
# plot_results(output_unmix, violin = FALSE) # Density plot
# dev.off()
# 
# # save violin plot
# png("output_unmix_violinplot.png", width = 18, height = 12, units = "cm", res = 300) # to save .png results
# plot_results(output_unmix, violin = TRUE) # Violing plot
# dev.off()
# 
# 
# ###################################
# ###### 5. Validate results
# ###################################
# 
# apportionments <- c(0.435, 0.285, 0.280)
# normalized_error <- validate_results(selected_data, apportionments = c(0.435, 0.285, 0.280), error_threshold = 0.05)
# write.csv(normalized_error, "output_validate_results_normalized_error.csv")
# 
# # Check 'help' for more information
# help("validate_results")

