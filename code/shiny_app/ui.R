
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
               withMathJax(),
               selectInput("policy_est", "Policy Estimate:",
                           choices=policy_estimates_text, selected = "Total effects, 2019(KLPS4) B & EA C, no ext")
               ),
               fluidRow(id = "tPanel",style = "overflow-y:scroll; max-width: 400px; max-height: 800px; position:relative;",
               tabsetPanel(
                  tabPanel("Data", 
                           uiOutput("data_in")
                           ),
                  tabPanel("Research",
                           uiOutput("research_in")
                           ),
                  tabPanel("GW",
               numericInput("param29_1", label = h3("Lambda 1_1_new = "), value = lambda1_new_so[1]),
               numericInput("param29_1_1", label = h3("sd = "), value = lambda1_new_sd_so[1]),
               numericInput("param29_2", label = h3("Lambda 1_2_new = "), value = lambda1_new_so[2]),
               numericInput("param29_2_1", label = h3("sd = "), value = lambda1_new_sd_so[2]),
               numericInput("param29_3", label = h3("Lambda 1_3_new = "), value = lambda1_new_so[3]),
               numericInput("param29_3_1", label = h3("sd = "), value = lambda1_new_sd_so[3]),
               br("Guesswork"),
               numericInput("param21_1", label = h3("Coef Xp = "), value = coef_exp_so[1]),
               numericInput("param21_2", label = h3("Coef Xp^2 = "), value = coef_exp_so[2]),
               sliderInput("param22", label = "Teacher salary = ",
                           min = teach_sal_so / 2, max = 2 * teach_sal_so, value = teach_sal_so),
               sliderInput("param22_1", label = "SD = ",
                           min = 0.00000001* teach_sal_so, max = 1 * teach_sal_so, value = 0.1 * teach_sal_so),
               sliderInput("param23", label = "Teacher benefits = ",
                           min = teach_ben_so / 2, max = 2 * teach_ben_so, value = teach_ben_so),
               sliderInput("param23_1", label = "SD = ",
                           min = 0.0000001* teach_ben_so, max = 1 * teach_ben_so, value = 0.1 * teach_ben_so),
               sliderInput("param24", label = "Student per teach = ",
                           min = n_students_so / 2, max = 2 * n_students_so, value = n_students_so),
               sliderInput("param24_1", label = "SD = ",
                           min = 0.0000001* n_students_so, max = 1 * n_students_so, value = 0.1 * n_students_so),
               sliderInput("param30", label = "Prevalence in original study = ",
                           min = 0, max = 1, value = alpha_0_so),
               sliderInput("param30_1", label = "SD = ",
                           min = 0.0000001* alpha_r_so, max = 1 * alpha_0_so, value = 0.1 * alpha_0_so) ,
               sliderInput("param31", label = "Prevalence in new region = ",
                           min = 0 / 2, max = 1, value = alpha_r_so),
               sliderInput("param31_1", label = "SD = ",
                           min = 0.0000001* alpha_r_so, max = 1 * alpha_r_so, value = 0.1 * alpha_r_so) ,
               sliderInput("param32", label = "Counts adjustment = ",
                           min = counts_par_so / 2, max = 2 * counts_par_so, value = counts_par_so),
               sliderInput("param32_1", label = "SD = ",
                           min = 0.0000001 * counts_par_sd_so, max = 10 * counts_par_sd_so, value = counts_par_sd_so) ,
               sliderInput("param33", label = "Additional costs due to staff time = ",
                           min = staff_time_so / 2, max = 2 * staff_time_so, value = staff_time_so),
               sliderInput("param33_1", label = "SD = ",
                           min = 0.0000001* staff_time_so, max = 1 * staff_time_so, value = 0.1 * staff_time_so) ,
               sliderInput("param34", label = "Costs adjustments = ",
                           min = costs_par_so / 2, max = 20000 * costs_par_so, value = costs_par_so),
               sliderInput("param34_1", label = "SD = ",
                           min = 0.0000001* costs_par_sd_so, max = 10 * costs_par_sd_so, value = costs_par_sd_so)
                )
               )
               )
  ),
  mainPanel(
    plotOutput("plot1")
  )
)
)
