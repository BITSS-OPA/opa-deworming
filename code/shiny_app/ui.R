
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
               selectInput("policy_est", "Policy Estimate:",
                           choices=policy_estimates_text, selected = "Fiscal effects, 2016(W@W) B & C, yes ext")
               ),
               fluidRow(id = "tPanel1",style = "overflow-y:scroll; max-width: 400px; max-height: 800px; position:relative;",
               tabsetPanel(
                  tabPanel("Data", 
                           uiOutput("data_in") 
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
