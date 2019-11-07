
library(shiny)





library(tidyverse)
library(haven)
library(here)
library(kableExtra)
library(readxl)

# not sure if this makes a difference
knitr::opts_knit$set(root.dir = here())



nsims <- 1e2

source("all_analysis.R")




shinyUI( fluidPage(
  sidebarPanel(
               fluidRow(id = "tPanel",style = "max-width: 400px; max-height: 300px; position:relative;",
                 numericInput("param1", label = h3("N Sims = "), value = 1e2),
               checkboxInput("rescale", label = "Click if want to rescale x-axis", value = FALSE),
               actionButton("run", label = "Run Simulation"),
               withMathJax(),
               useShinyjs(), 
               selectInput("policy_est", "Policy Estimate:",
                           choices=policy_estimates_text, selected = "Fiscal effects, 2016(W@W) B & C, yes ext")
               ),
               fluidRow(id = "tPanel1",style = "overflow-y:scroll; max-width: 400px; max-height: 800px; position:relative;",
               tabsetPanel(
                  tabPanel("Data", 
                           uiOutput("data_in") , 
                           sliderInput("param32", label = "AAACounts adjustment = ",
                                       min = counts_par_so / 2, max = 2 * counts_par_so, value = counts_par_so),
                           sliderInput("param32_1", label = "SD = ",
                                       min = 0.0000001 * counts_par_sd_so, max = 10 * counts_par_sd_so, value = counts_par_sd_so)
                           ),
                  tabPanel("Research",
                           uiOutput("research_in"),
                           ),
                  tabPanel("GW",
                           uiOutput("gw_in")
                           )
                  )
               )
               ),
  mainPanel(
    plotOutput("plot1")
  )
)
)
