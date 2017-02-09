## simulate data
library(car)
library(RColorBrewer)
library(psych)
library(nFactors)
library(lavaan)
library(semPlot)
library(semTools)
library(semPLS)
library(DiagrammeR)
library(magrittr)
library(dplyr)

pies_sim_data <- read.csv("data/rintro-chapter10pies.csv", stringsAsFactors = TRUE)

some(pies_sim_data)
describe(pies_sim_data)

scatterplotMatrix(pies_sim_data[, c(1,2,4,5,8,9)], diagonal = "histogram", col = brewer.pal(3, "Paired"), ellipse = TRUE)

nScree(pies_sim_data)
plotnScree(nScree(pies_sim_data))
factanal(pies_sim_data, factors = 3)

#### confirmatory factor analysis
pies_model <- " General =~ i1 + i2 + i3
                Feature =~ i4 + i5 + i6  + i7
                Image   =~ i8 + i9 + i10 + i11
                PIES =~ General + Feature + Image "

pies_fit <- cfa(pies_model, data = pies_sim_data)
summary(pies_fit, fit.measures = TRUE)

semPaths(pies_fit, what = "est", fade = FALSE, residuals = FALSE, edge.label.cex = 0.75)

## addess model fit
# current PIES 3 + 1
# vs one single latent factor - PIES 1
# vs uncorrelated 3 factor model - PIES 3

pies_model_nh1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11"
pies_fit_nh1 <- cfa(pies_model_nh1, data = pies_sim_data)

pies_model_nh3 <- " General =~ i1 + i2 + i3
                    Feature =~ i4 + i5 + i6  + i7
                    Image   =~ i8 + i9 + i10 + i11
                    General ~~ 0.1 * Feature
                    General ~~ 0.1 * Image
                    Feature ~~ 0.1 * Image "
pies_fit_nh3 <- cfa(pies_model_nh3, data = pies_sim_data)

compareFit(pies_fit_nh1, pies_fit_nh3, pies_fit)

#### structural equation models
sat_sim_data <- read.csv("data/rintro-chapter10sat.csv", stringsAsFactors = TRUE)

sat_model <- " Quality =~ CSat + Value + q1 + q2 + q3 + 0*Cost
               Cost =~ Value + Repeat + c1 + c2 + c3
               Value =~ CSat + v1 + v2 + v3
               CSat =~ Repeat + cs1 + cs2 + cs3
               Repeat =~ r1 + r2 + r3 "

sat_fit <- sem(sat_model, data = sat_sim_data, std.lv = TRUE)
summary(sat_fit, fit.measures = TRUE)

semPaths(sat_fit, what = "est", fade = FALSE, residuals = FALSE, layout = "tree", structural = TRUE, nCharNodes = 7, edge.label.cex = 1)

sat_alt_model <- " Quality =~ CSat + q1 + q2 + q3 + 0*Cost
                   Cost =~ Value + c1 + c2 + c3
                   Value =~ CSat + v1 + v2 + v3
                   CSat =~ Repeat + cs1 + cs2 + cs3
                   Repeat =~ r1 + r2 + r3 "
sat_alt_fit <- sem(sat_alt_model, data = sat_sim_data, std.lv = TRUE)

compareFit(sat_fit, sat_alt_fit, nested = TRUE)

#### partial least squares
## CB-SEM requires
# strict assumptions about data distributions (continuous data, normally distributed residuals)
# number of indicators per factor (generally three or more)
# reliability of indicators
# sample size (some authorities recommend several hundred, although it is possible that samples of N = 100 or even N = 50 may be adequate when measures are very reliable

## partial least squares (PLS-SEM)
# data do not comply with the assumptions of CB-SEM
# or come from a modest sample size with potentially less reliable indicators
# but hard to addess model fit or comparative strength

set.seed(90704)
sat_sim_data_2 <- sat_sim_data[sample(nrow(sat_sim_data), 50),]
describe(sat_sim_data_2)

sat_fit_2 <- sem(sat_model, data = sat_sim_data_2, std.lv = TRUE)
summary(sat_fit_2, fit.measures = TRUE)

sat_pls_mm <- matrix(c(
  "Quality", "q1",
  "Quality", "q2",
  "Quality", "q3",
  "Cost", "c1",
  "Cost", "c2",
  "Cost", "c3",
  "Value", "v1",
  "Value", "v2",
  "Value", "v3",
  "CSat", "cs1",
  "CSat", "cs2",
  "CSat", "cs3",
  "Repeat", "r1",
  "Repeat", "r2",
  "Repeat", "r3"), ncol = 2, byrow = TRUE
)

sat_pls_sm <- matrix(c(
  "Quality", "CSat",
  "Quality", "Value",
  "Cost", "Value",
  "Cost", "Repeat",
  "Value", "CSat",
  "CSat", "Repeat"), ncol = 2, byrow = TRUE
)

sat_pls_mod <- plsm(data = sat_sim_data_2, strucmod = sat_pls_sm, measuremod = sat_pls_mm)
sat_pls_fit <- sempls(model = sat_pls_mod, data = sat_sim_data_2)

plsLoadings(sat_pls_fit)
pathCoeff(sat_pls_fit)

diagram <- capture.output(pathDiagram(sat_pls_fit, full = FALSE, digits = 2, edge.labels = "values", output.type = "graphics"))
grViz(diagram)
#pathDiagram(sat_pls_fit, file = "sat_pls_struct", full = FALSE, digits = 2, edge.labels = "values", output.type = "graphics", graphics.fmt = "pdf")

#########
######### check PLS-PM
#########

## assessing PLS-SEM model
#1. examine model's coefficients
#2. examine overall r squared and determine whether sufficient variance is explained to be useful
#3. bootstrap model to examine coefficient stability

#2. subjective assessment
rSquared(sat_pls_fit)

#3.
set.seed(04460)
sat_pls_boot <- bootsempls(sat_pls_fit, nboot = 500, start = "ones")

### PLS revisit with PLS-PM package

















