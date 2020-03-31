library(stargazer)
library(ggplot2)
library(lmtest)
library(sandwich)
library(stringi)
require(stringr)
require(gridExtra)
require(multiwayvcov)
library(interflex)
library(tidyverse)

rm(list=ls())

#set directory
setwd("~/Dropbox/Gender_2/replication_file")

#create functions to use
source('./code/functions.R')


#read in main analysis data sets
constituency_data_main <- read.csv("./data/pub_opinion_results.csv")  
individual_data_main <- read.csv("./data/pub_opinion_results_individual.csv")  
estimation_sample_data <- read.csv("./data/estimation_sample_dapils_w_gender.csv")
budget_data <- read.csv("./data/budget_results.csv")
candidates_2019_data <- read.csv("./data/candidate_2019_data.csv")
balance_elections_data <- read.csv("./data/election_characteristics_balance.csv")
health_laws_2009 <- read.csv("./data/laws_dprd_2009.csv")
p2tp2a_data <- read.csv("./data/center_data.csv")


############################
##ANALYSIS TABLES###########
############################

#make table 1 -- MAIN EFFECT, females
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_1.R')

#make table 2 -- 2007 PLACEBO
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_3.R')

#make table XX -- MAIN EFFECT, males
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_2.R')

#make table XX -- INDIVIDUAL LEVEL FEMALE OUTCOMES
individual_data <- individual_data_main[abs(individual_data_main$win_margin) < 0.01,]
source('./code/tables/table_c1i.R')

#make table XX -- 2007 PLACEBO SAME CONSTITUENCY SUBSET
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01 & !is.na(constituency_data_main$v744a_f2012),]
source('./code/tables/table_b5i.R')

#make table XX -- BUDGET MECHANISM ANALYSIS
source('./code/tables/table_c5i.R')

#make table XX -- lagged DV for female respondents
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_c4i.R')

#make table XX -- lagged DV for male respondents
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_c4ii.R')

#make table XX -- supplementary puskemas level analysis
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_c5ii.R')

#make tableXX -- supplementary HTE by p2tp2a center
constituency_data_main <- left_join(constituency_data_main, p2tp2a_data[,c("kab_code", "center", "established_year")], by = "kab_code")
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/tables/table_c5iii.R')


############################
##DESCRIPTIVE TABLES########
############################

#make tale XX -- DESCRIPTIVE STATS OUTCOMES
constituency_data <- constituency_data_main
source('./code/tables/table_a3.R')


############################
##FIGURES ##################
############################

#balance plot
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source('./code/figures/figure_b2i.R')

#balance plot -- elections
balance_elections_data_trim <- balance_elections_data[abs(balance_elections_data$win_margin) < 0.01,]
source('./code/figures/figure_b2ii.R')

#smoothness plot
constituency_data <- constituency_data_main
source('./code/figures/figure_b3i.R')

#first stage plot
estimation_sample <- estimation_sample_data
source('./code/figures/figure_b1i.R')

#graphical representation
constituency_data <- constituency_data_main
source('./code/figures/figure_c2i.R')

#sensitivity analysis
constituency_data <- constituency_data_main
source('./code/figures/figure_c3i.R')

#graphical representation of irrelevant outcome
constituency_data <- constituency_data_main
source('./code/figures/figure_b5i.R')

#analysis of 2019 candidate platforms
source('./code/figures/figure_c5i.R')

#make graph of health laws to 2009
source("./code/figures/figure_b4i.R")

#make figureXX -- supplementary HTE by p2tp2a center
constituency_data <- constituency_data_main[abs(constituency_data_main$win_margin) < 0.01,]
source("./code/figures/figure_c5ii.R")


