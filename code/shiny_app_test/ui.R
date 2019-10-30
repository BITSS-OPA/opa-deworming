
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
  sidebarPanel(id = "tPanel",style = "overflow-y:scroll; max-width: 400px; max-height: 800px; position:relative;",
               numericInput("param1", label = h3("N Sims = "), value = 1e2),
               checkboxInput("rescale", label = "Click if want to rescale x-axis", value = FALSE),
               br("Data"),
               withMathJax(),
               selectInput("policy_est", "Policy Estimate:",
                           choices=policy_estimates_text, selected = "Total effects, 2019(KLPS4) B & EA C, no ext"),
               hr(),
               sliderInput("param2", label = "Gov Bonds (\\( i \\))"  ,
                           min = 0.001, max = 0.2, value = gov_bonds_so),
               sliderInput("param2_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * gov_bonds_so, value = 0.1 * gov_bonds_so),
               sliderInput("param3", label = "Inflation (\\( \\pi \\) ) = ",
                           min = 0.001, max = 0.2, value = inflation_so),
               sliderInput("param3_1", label = "SD = ",
                           min = 0.0000001, max = 0.4 * inflation_so, value = 0.1 * inflation_so),
               sliderInput("param4", label = "Agri Wages (\\( w_{ag} \\))",
                           min = wage_ag_so / 2, max = 2 * wage_ag_so, value = wage_ag_so),
               sliderInput("param4_1", label = "SD = ",
                           min = 0.0000001* wage_ag_so, max = 1 * wage_ag_so, value = 0.1 * wage_ag_so),
               sliderInput("param5", label = "Work-non ag-Wages  (\\( w_{ww} \\))",
                           min = wage_ww_so / 2, max = 2 * wage_ww_so, value = wage_ww_so),
               sliderInput("param5_1", label = "SD = ",
                           min = 0.0000001* wage_ww_so, max = 1 * wage_ww_so, value = 0.1 * wage_ww_so),
               sliderInput("param6", label = "Profits se = ",
                           min = profits_se_so / 2, max = 2 * profits_se_so, value = profits_se_so),
               sliderInput("param6_1", label = "SD = ",
                           min = 0.000001* profits_se_so, max = 1 * profits_se_so, value = 0.1 * profits_se_so),
               sliderInput("param7", label = "Hours se (>0) = ",
                           min = hours_se_cond_so / 2, max = 2 * hours_se_cond_so, value = hours_se_cond_so),
               sliderInput("param7_1", label = "SD = ",
                           min = 0.000001* hours_se_cond_so, max = 1 * hours_se_cond_so, value = 0.1 * hours_se_cond_so),
               sliderInput("param8", label = "H_ag = ",
                           min = hours_ag_so / 2, max = 2 * hours_ag_so, value = hours_ag_so),
               sliderInput("param8_1", label = "SD = ",
                           min = 0.000001* hours_ag_so, max = 1 * hours_ag_so, value = 0.1 * hours_ag_so),
               sliderInput("param9", label = "H_ww = ",
                           min = hours_ww_so / 2, max = 2 * hours_ww_so, value = hours_ww_so),
               sliderInput("param9_1", label = "SD = ",
                           min = 0.000001* hours_ww_so, max = 1 * hours_ww_so, value = 0.1 * hours_ww_so),
               sliderInput("param10", label = "H_se = ",
                           min = hours_se_so / 2, max = 2 * hours_se_so, value = hours_se_so),
               sliderInput("param10_1", label = "SD = ",
                           min = 0.000001* hours_se_so, max = 1 * hours_se_so, value = 0.1 * hours_se_so),
               sliderInput("param11", label = "Exchange rate = ",
                           min = ex_rate_so / 2, max = 2 * ex_rate_so, value = ex_rate_so),
               sliderInput("param11_1", label = "SD = ",
                           min = 0.000001* ex_rate_so, max = 1 * ex_rate_so, value = 0.1 * ex_rate_so),
               sliderInput("param12", label = "growth = ",
                           min = growth_rate_so / 2, max = 2 * growth_rate_so, value = growth_rate_so),
               sliderInput("param12_1", label = "SD = ",
                           min = 0.000001* growth_rate_so, max = 1 * growth_rate_so, value = 0.1 * growth_rate_so),
               sliderInput("param13", label = "Coverage (R) = ",
                           min = coverage_so / 2, max = 2 * coverage_so, value = coverage_so),
               sliderInput("param13_1", label = "SD = ",
                           min = 0.000001* coverage_so, max = 1 * coverage_so, value = 0.1 * coverage_so),
               sliderInput("param15", label = "Tax rate = ",
                           min = tax_so / 2, max = 2 * tax_so, value = tax_so, step = 0.001),
               sliderInput("param15_1", label = "SD = ",
                           min = 0.00001* tax_so, max = 1 * tax_so, value = 0.1 * tax_so),
               sliderInput("param16", label = "Costs ot T (local $) = ",
                           min = unit_cost_local_so / 2, max = 2 * unit_cost_local_so, value = unit_cost_local_so),
               sliderInput("param16_1", label = "SD = ",
                           min = 0.000001* unit_cost_local_so, max = 1 * unit_cost_local_so, value = 0.1 * unit_cost_local_so),
               sliderInput("param17", label = "Years of T = ",
                           min = years_of_treat_so / 2, max = 2 * years_of_treat_so, value = years_of_treat_so),
               sliderInput("param17_1", label = "SD = ",
                           min = 0.000001* years_of_treat_so, max = 1 * years_of_treat_so, value = 0.1 * years_of_treat_so),

               br("Research"),
               numericInput("param18_1", label = h3("Lambda 1_m = "), value = lambda1_so[1]),
               numericInput("param18_1_1", label = h3("sd = "), value = 0.17),
               numericInput("param18_2", label = h3("Lambda 1_f = "), value = lambda1_so[2]),
               numericInput("param18_2_1", label = h3("sd = "), value = 0.17),
               sliderInput("param19", label = "Lambda 2 = ",
                           min = 0, max = 2 * lambda2_so, value = lambda2_so * 1),
               sliderInput("param19_1", label = "SD = ",
                           min = 0.0000001* lambda2_so, max = 1 * lambda2_so, value = 0.1 * lambda2_so),
               sliderInput("param20", label = "Take-up = ",
                           min = q_full_so / 2, max = 2 * q_full_so, value = q_full_so),
               sliderInput("param20_1", label = "SD = ",
                           min = 0.00000001* q_full_so, max = 1 * q_full_so, value = 0.1 * q_full_so),
               sliderInput("param28", label = "Take-up with no subsidy = ",
                           min = q_zero_so / 2, max = 2 * q_zero_so, value = q_zero_so),
               sliderInput("param28_1", label = "SD = ",
                           min = 0.00000001* q_zero_so, max = 1 * q_zero_so, value = 0.1 * q_zero_so),
               #                 checkboxInput("checkbox1", label = "Use additional education with externalities", value = TRUE),
               sliderInput("param26", label = "x * Delta E = ",
                           min = 0.0000001, max = 4, value = delta_ed_par_so),
               sliderInput("param26_1", label = "SD = ",
                           min = 0.0000001, max = 4, value = delta_ed_par_so * 0.1),
               sliderInput("param27", label = "x * Delta E (ext)  = ",
                           min = 0.0000001, max = 4, value = delta_ed_ext_par_so),
               sliderInput("param27_1", label = "SD = ",
                           min = 0.0000001, max = 4, value = delta_ed_ext_par_so * 0.1),
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
                           min = counts_par_so / 2, max = 2 * counts_par_so, value = 1),
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
  ),
  mainPanel(
    plotOutput("plot1")
  )
)
)
