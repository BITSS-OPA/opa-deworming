library(shiny)
library(tidyverse)
library(haven)
library(here)
library(kableExtra)
library(readxl)
library(shinyjs)
#library(plotly)
library(shinyBS)
library(shinythemes)
library(ggplot2)

# not sure if this makes a difference
knitr::opts_knit$set(root.dir = here())
source("all_analysis.R")

costs_temp_india   <-
  costs1_p2_f(
    country_total_var = costs_data$total,
    country_cost_var = costs_data$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("india")
  )
costs_temp_kenya   <-
  costs1_p2_f(
    country_total_var = costs_data$total,
    country_cost_var = costs_data$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("kenya")
  )
costs_temp_nigeria <-
  costs1_p2_f(
    country_total_var = costs_data$total,
    country_cost_var = costs_data$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("nigeria")
  )
costs_temp_vietnam <-
  costs1_p2_f(
    country_total_var = costs_data$total,
    country_cost_var = costs_data$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("vietnam")
  )


prevalence_india <- prevalence_r_so["india"]
prevalence_kenya <- prevalence_r_so["kenya"]
prevalence_nigeria <- prevalence_r_so["nigeria"]
prevalence_vietnam <- prevalence_r_so["vietnam"]


nsims <- 1e3

# Before each deployment: copy and paste 'data' and 'rawdata' folders into 'shiny_app\'
# here() creates conflits with shiny deployment. Use source("all_analysis.R") intead
# source(here("code", "shiny_app", "all_analysis.R"))

#fluidPage is something must have
shinyUI(
  fluidPage( theme = shinytheme("cerulean"),
    navbarPage("Open Policy Analysis for Deworming Interventions: Open Output Component",
               # Begin main policy estimate tab ---- 
               tabPanel(
                "Main Policy Estimate",
                sidebarPanel(
                  fluidRow(column(
                    12,
                    align = "center",
                    tags$a(
                      img(
                        src = "bitss_just_logo_transparent.png",
                        width = "20%",
                        height = "auto"
                      ),
                      href = "https://bitss.org"
                    ),
                    tags$a(
                      img(
                        src = "cega_transparent.png",
                        width = "70%",
                        height = "auto"
                      ),
                      href = "https://cega.berkeley.edu"
                    )
                  )),
                  fluidRow(
                    p(
                      "This visualization is one of three key components of an",
                      tags$a(href = "http://www.bitss.org/opa/projects/deworming/", "Open Policy Analysis (OPA)"),
                      "on the costs and benefits of
                       mass deworming interventions in various settings. This components are:",
                      tags$li(
                        tags$span(
                          "This app, which presents a single output that best represents the factual information required by policy makers to inform their position regarding a policy of mass deworming. Additional two other tabs allow reader to modify key assumptions and components and see how this output changes"
                        )
                      ),
                      tags$li(
                        tags$a(href = "https://rpubs.com/fhoces/547979", "A detailed report"),
                        "that describes how to obtain the policy estimate and describes each component of the analysis"
                      ),
                      tags$li(
                        tags$a(href = "https://github.com/BITSS-OPA/opa-deworming", "A repository"),
                        "that contains all the materials needed to reproduce the analysis with minimal effort (report and interactive app)."
                      ),
                    ),
                    p(
                      "The app is the result of a collaboration between the",
                      tags$a(
                        href = "https://www.bitss.org/",
                        "Berkeley Initiative
                                     for Transparency in the Social Sciences"
                      ),
                      "and",
                      tags$a(href = "https://www.evidenceaction.org/dewormtheworld-2/",
                             "Evidence Action.")
                    )
                  ),
                  fluidRow(
                    id = "tPanel_main",
                    style = "max-width: 400px; max-height: 300px; position:relative;",
                    br(),
                    h4(strong("Description of Results")),
                    p(
                      "We simulate finding the lifetime income effects on
                              treated children many times, then plot the values
                              to create this figure. The height of the curve represents
                              how often an outcome appeared, i.e. the highest point
                              means that particular value appeared the most frequently.
                              The blue line indicates that half of all values are
                              on either side of the line."
                    )
                  )
                ),
                 mainPanel(
                   fluidRow(id = "output_id1_main", style = "max-width: 800px; max-height: 700px; position:relative;",
                            plotOutput("plot1_main")
                   )
                 )
               ),
               # end of main policy estimate tab ---- 
               # Begin of key assumptions tab ----
               tabPanel(
                 "Key Assumptions", 
                 sidebarPanel(
                   
                   fluidRow(id = "tPanel1_ka", 
                            style = "overflow-y:scroll; max-width: 600px; max-height: 600px; position:relative;", 
                            numericInput(
                              "param35_ka",
                              label = h4("Yearly unit costs in new country (in $US)"),
                              value = round(costs2_ea_in, 2), 
                              min = 0
                            ),
                            helpText("For reference:", br(),
                                     paste("Unit costs in India is", round(costs_temp_india,2)), br(),
                                     paste("Unit costs in Kenya is", round(costs_temp_kenya,2)), br(),
                                     paste("Unit costs in Nigeria is", round(costs_temp_nigeria,2)), br(),
                                     paste("Unit costs in Vietnam is", round(costs_temp_vietnam,2))),
                            numericInput(
                              "param31_ka",
                              label = "Prevalence in new region (\\( \\eta_{new} \\)) = ",
                              min = 0 ,
                              max = 1,
                              value = round(prevalence_r_in, 2)
                            ),
                            helpText("For reference:", br(),
                                     paste("Prevalence in India is", round(prevalence_india,2)), br(),
                                     paste("Prevalence in Kenya is", round(prevalence_kenya,2)), br(),
                                     paste("Prevalence in Nigeria is", round(prevalence_nigeria,2)), br(),
                                     paste("Prevalence in Vietnam is", round(prevalence_vietnam,2))), 
                            numericInput(
                              "param17_new_ka",
                              label = h4("Length of treatment (years)"),
                              value = round(years_of_treat_0_so, 2), 
                              min = 0,
                              max = 10,
                              step = 0.1,
                            ),
                            helpText("For reference:", br(),
                                     paste("Prevalence in India is", round(prevalence_india,2)), br(),
                                     paste("Prevalence in Kenya is", round(prevalence_kenya,2)), br(),
                                     paste("Prevalence in Nigeria is", round(prevalence_nigeria,2)), br(),
                                     paste("Prevalence in Vietnam is", round(prevalence_vietnam,2)))
                   )
                 ),
                 mainPanel(
                   fluidRow(id = "output_id1_ka", style = "max-width: 800px; max-height: 700px; position:relative;",
                            plotOutput("plot1_ka")
                   )
                 )
               ),
               # end of key assumptions tab ----
               # Begin All assumptions tab ----
               tabPanel(
                 "All Assumptions",
                 sidebarPanel(
                   fluidRow(id = "tPanel",style = "max-width: 600px; max-height: 400px; position:relative;",
                            # Begin policy estimate description ----
                            selectInput("policy_est",
                                        h4("Policy Estimate:"),
                                        choices = policy_estimates_text,
                                        selected = "A3. All income of A2. Main Policy Estimate"),
                            withMathJax(), 
                            useShinyjs(),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. Tax revenue' ",
                              helpText(
                                "Approach 1.1. Welfare measured as additional tax revenue.", br(),
                                " - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10 year follow-up. No externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With externalities. Tax' ",
                              helpText(
                                "Approach 1.2. Welfare measured as additional tax revenue.", br(),
                                " - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10 year follow-up. Including externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. All income' ",
                              helpText(
                                "Approach 1.3. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. No externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With ext. All income' ",
                              helpText(
                                "Approach 1.4. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings. Including externalities.
                                   Data from 10 year follow-up. Including externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. Tax' ",
                              helpText(
                                "Approach 2.1. Welfare measured as additional tax revenue.", br(),
                                " - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. All income' ",
                              helpText(
                                "Approach 2.2. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities", br(),
                                " - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling"
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1' ",
                              helpText(
                                "Approach 3.1. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. No externalities. 
                                Adjusted for prevalence and length of treatment", br(),
                                " - Costs: current implementation costs in several settings."
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1, with ext.' ",
                              helpText(
                                "Approach 3.2. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. Including externalities. 
                                Adjusted for prevalence and length of treatment", br(),
                                " - Costs: current implementation costs in several settings."
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A2. Main Policy Estimate' ",
                              helpText(
                                "Approach 3.3. Welfare measured as additional earnings.", br(),
                                " - Benefits: predicted additional earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities. 
                                Adjusted for prevalence and length of treatment", br(),
                                " - Costs: current implementation costs in several settings."
                              )
                            ),
                            # end policy estimate description ----
                            checkboxInput("rescale", 
                                          label = "Click if want to rescale x-axis. Unclick to fix reference point", 
                                          value = FALSE), 
                            numericInput("param1",
                                         label = h4("Number of simulations"),
                                         value = 1e2)
                   ),
                   fluidRow(id = "tPanel1",
                            style = "overflow-y:scroll; 
                                     max-width: 600px; 
                                     max-height: 400px; 
                                     position:relative;",
                            tabsetPanel(
                              # Begin tabpanel research ----
                              tabPanel(
                                "Research",
                                br(),
                                a(id = "toggleResearchSDs", "Show/hide all SDs", href =
                                    "#"),
                                br(),
                                br(),
                                numericInput(
                                  "param18_1",
                                  label = ("\\( \\lambda_{1m} \\) "),
                                  value = lambda1_so[1]
                                ),
                                bsPopover(
                                  id = "param18_1",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Male)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD22",
                                  numericInput(
                                    "param18_1_1", 
                                    label = "SD = ", 
                                    value = lambda1_sd_so[1])
                                )),
                                numericInput(
                                  "param18_2",
                                  label = ("\\( \\lambda_{1f} \\) = "),
                                  value = lambda1_so[2]
                                ),
                                bsPopover(
                                  id = "param18_2",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Female)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD23",
                                  numericInput(
                                    "param18_2_1", 
                                    label = "SD = ", value = 
                                      lambda1_sd_so[2])
                                )),
                                numericInput(
                                  "param29_1",
                                  label = ("\\(\\alpha^{pooled} \\) = "),
                                  value = round(lambda1_new_so,2)
                                ),
                                bsPopover(
                                  id = "param29_1",
                                  title = "",
                                  content = "Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD29",
                                  numericInput("param29_1_1", label = "SD = ", value = lambda1_new_sd_so)
                                )),
                                sliderInput(
                                  "param19",
                                  label = "\\( \\lambda_{2} \\) = ",
                                  min = 0,
                                  max = 2 * lambda2_so,
                                  value = lambda2_so * 1
                                ),
                                bsPopover(
                                  id = "param19",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Externalities included)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD24",
                                  sliderInput(
                                    "param19_1",
                                    label = "SD = ",
                                    min = 0.1 * lambda2_sd_so,
                                    max = 5 * lambda2_sd_so,
                                    value = lambda2_sd_so,
                                    step = 1e-2
                                  )
                                )),
                                sliderInput(
                                  "param30",
                                  label = "Prevalence in original study (\\( \\eta \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = prevalence_0_so
                                ),
                                bsPopover(
                                  id = "param30",
                                  title = "",
                                  content = "Prevalence of parasitic worms in population (Miguel & Kremer 2004)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD32",
                                  sliderInput(
                                    "param30_1",
                                    label = "SD = ",
                                    min = 0.0000001 ,
                                    max = 1 ,
                                    value = 0.1
                                  )
                                )),
                                numericInput("param4", 
                                             label = "Agri Wages (\\( w_{ag} \\))", 
                                             value = wage_ag_so), 
                                bsPopover(
                                  id = "param4",
                                  title = "",
                                  content = "Average hourly wage of an agricultural worker (KSH)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD5",
                                  numericInput("param4_1", 
                                               label = "SD = ", 
                                               value = 0.1 * wage_ag_so)
                                )),
                                numericInput("param5", 
                                             label = "Work-non ag-Wages  (\\( w_{ww} \\))", 
                                             value = round(wage_ww_so, 2)), 
                                bsPopover(
                                  id = "param5",
                                  title = "",
                                  content = "Average hourly wage of a wage worker",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD6",
                                  numericInput("param5_1", 
                                               label = "SD = ", 
                                               value = round(0.1 * wage_ww_so, 2))
                                )),
                                numericInput("param6", 
                                             label = "Profits se = ", 
                                             value = profits_se_so),
                                bsPopover(id = "param6",
                                          title = "",
                                          content = "Average monthly self-employed profits (self-reported)"),
                                hidden(div(
                                  id = "SD7",
                                  numericInput("param6_1", 
                                               label = "SD = ", 
                                               value = 0.1 * profits_se_so)
                                )),
                                sliderInput(
                                  "param7",
                                  label = "Hours se (>10) = ",
                                  min = hours_se_cond_so / 2,
                                  max = 2 * hours_se_cond_so,
                                  value = hours_se_cond_so
                                ),
                                bsPopover(
                                  id = "param7",
                                  title = "",
                                  content = "Average weekly hours worked (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD8",
                                  sliderInput(
                                    "param7_1",
                                    label = "SD = ",
                                    min = 0.000001 * hours_se_cond_so,
                                    max = 1 * hours_se_cond_so,
                                    value = 0.1 * hours_se_cond_so
                                  )
                                )), 
                                sliderInput(
                                  "param8",
                                  label = "\\(\\ H_{ag} \\) = ",
                                  min = hours_ag_so / 2,
                                  max = 2 * hours_ag_so,
                                  value = hours_ag_so
                                ),
                                bsPopover(
                                  id = "param8",
                                  title = "",
                                  content = "Average weekly hours worked by agricultural workers (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD9",
                                  sliderInput(
                                    "param8_1",
                                    label = "SD = ",
                                    min = 0.000001 * hours_ag_so,
                                    max = 1 * hours_ag_so,
                                    value = 0.1 * hours_ag_so,
                                    round = -4,
                                    step = 0.001
                                  )
                                )),
                                sliderInput(
                                  "param9",
                                  label = "\\(\\ H_{ww} \\) = ",
                                  min = hours_ww_so / 2,
                                  max = 2 * hours_ww_so,
                                  value = hours_ww_so
                                ),
                                bsPopover(
                                  id = "param9",
                                  title = "",
                                  content = "Average weekly hours worked by wage earners (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD10",
                                  sliderInput(
                                    "param9_1",
                                    label = "SD = ",
                                    min = 0.000001 * hours_ww_so,
                                    max = 1 * hours_ww_so,
                                    value = 0.1 * hours_ww_so,
                                    step = 0.001
                                  )
                                )),
                                sliderInput(
                                  "param10",
                                  label = "\\(\\ H_{se} \\) = ",
                                  min = hours_se_so / 2,
                                  max = 2 * hours_se_so,
                                  value = hours_se_so
                                ),
                                bsPopover(
                                  id = "param10",
                                  title = "",
                                  content = "Average weekly hours worked by self-employed workers (control group - non-agricultural)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD11",
                                  sliderInput(
                                    "param10_1",
                                    label = "SD = ",
                                    min = 0.000001 * hours_se_so,
                                    max = 1 * hours_se_so,
                                    value = 0.1 * hours_se_so,
                                    step = 0.001
                                  )
                                )),
                                numericInput(
                                  "param21_1",
                                  label = ("Coefficients of \\(X_{p} \\) (\\( \\beta_{1} \\)) = "),
                                  value = coef_exp_so[1]
                                ),
                                bsPopover(
                                  id = "param21_1",
                                  title = "",
                                  content = "Teacher experience coefficient",
                                  placement = "top"
                                ),
                                numericInput(
                                  "param21_2",
                                  label = ("Coefficients of \\(X^{2}p \\) (\\( \\beta_{2} \\)) = "),
                                  value = coef_exp_so[2]
                                ),
                                bsPopover(
                                  id = "param21_2",
                                  title = "",
                                  content = "Teacher experience coefficient squared",
                                  placement = "top"
                                ),
                                sliderInput(
                                  "param13",
                                  label = "Coverage (\\( R \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = coverage_so,
                                  step = 0.01
                                ),
                                bsPopover(
                                  id = "param13",
                                  title = "",
                                  content = "Percent of treated primary schools students",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD14",
                                  sliderInput(
                                    "param13_1",
                                    label = "SD = ",
                                    min = 0.000001 * coverage_so,
                                    max = 1 * coverage_so,
                                    value = 0.1 * coverage_so,
                                    step = 0.000001
                                  )
                                )),
                                sliderInput(
                                  "param20",
                                  label = "Take-up (\\( Q(S_{2}) \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = q_full_so
                                ),
                                bsPopover(
                                  id = "param20",
                                  title = "",
                                  content = "Take up rate with full subsidy on deworming treatment costs",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD25",
                                  sliderInput(
                                    "param20_1",
                                    label = "SD = ",
                                    min = 0.00000001 * q_full_so,
                                    max = 1 * q_full_so,
                                    value = 0.1 * q_full_so,
                                    step = 1e-5
                                  )
                                )),
                                sliderInput(
                                  "param28",
                                  label = "Take-up with no subsidy (\\( Q(S_{1}) \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = q_zero_so
                                ),
                                bsPopover(
                                  id = "param28",
                                  title = "",
                                  content = "Take up rate without subsidy on deworming treatment costs (S1 = 0) ",
                                  placement = "top"
                                ),
                                sliderInput(
                                  "param26",
                                  label = "x * \\(\\Delta{E} \\) = ",
                                  min = 0.0000001,
                                  max = 4,
                                  value = delta_ed_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD27",
                                  sliderInput(
                                    "param26_1",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 4,
                                    value = delta_ed_par_so * 0.1
                                  )
                                )),
                                sliderInput(
                                  "param27",
                                  label = "x * \\(\\Delta{E} \\) (ext)  = ",
                                  min = 0.0000001,
                                  max = 4,
                                  value = delta_ed_ext_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD28",
                                  sliderInput(
                                    "param27_1",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 4,
                                    value = delta_ed_ext_par_so * 0.1
                                  )
                                )),
                                numericInput("param22", label = "Teacher salary = ", value = teach_sal_so),
                                bsPopover(
                                  id = "param22",
                                  title = "",
                                  content = "Average annual salary for Kenyan secondary school teacher",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD33",
                                  numericInput("param22_1", label = "SD = ", value = 0.1 * teach_sal_so)
                                )),
                                numericInput("param23", label = "Teacher benefits = ", value = teach_ben_so),
                                bsPopover(
                                  id = "param23",
                                  title = "",
                                  content = "Average annual benefits for Kenyan secondary school teacher (in KSH",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD34",
                                  numericInput("param23_1", label = "SD = ", value = 0.1 * teach_ben_so)
                                )),
                                numericInput("param24", label = "Students per teacher = ", value = n_students_so),
                                bsPopover(
                                  id = "param24",
                                  title = "",
                                  content = "Average number for students per teacher",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD35",
                                  numericInput(
                                    "param24_1",
                                    label = "SD = ",
                                    value = 0.1 * n_students_so
                                  )
                                )),
                                sliderInput(
                                  "param17",
                                  label = "Years of treatment in orginal study (\\(L_{0}\\))",
                                  min = 0,
                                  max = 10,
                                  step = 0.01,
                                  value = round(years_of_treat_t_so,2)
                                ),
                                bsPopover(
                                  id = "param17",
                                  title = "",
                                  content = "Average years of treatement in Kenya",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD18",
                                  sliderInput(
                                    "param17_1",
                                    label = "SD = ",
                                    min = 0.000001 * years_of_treat_0_so,
                                    max = 1 * years_of_treat_0_so,
                                    value = 0.1 * years_of_treat_0_so,
                                    step = 0.0001
                                  )
                                )),
                                numericInput(
                                  "param16", 
                                  label = "Costs of T (local $) = ", 
                                  value = round(unit_cost_local_so,2)),
                                bsPopover(
                                  id = "param16",
                                  title = "",
                                  content = "Costs of deworming per capita (KSH)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD16",
                                  numericInput(
                                    "param16_1",
                                    label = "SD = ",
                                    value = 0.1 * unit_cost_local_so
                                  )
                                )),
                                numericInput("param16_new", 
                                             label = "Costs of T (local $) = ", 
                                             value = round(unit_cost_2017usdppp_so, 2)),
                                bsPopover(
                                  id = "param16_new",
                                  title = "",
                                  content = "Costs of deworming per capita (USD)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD17",
                                  numericInput(
                                    "param16_1_new",
                                    label = "SD = ",
                                    value = 0.1 * unit_cost_2017usdppp_so
                                  )
                                )),
                                sliderInput(
                                  "param34",
                                  label = "Costs adjustments = ",
                                  min = costs_par_so / 2,
                                  max = 20000 * costs_par_so,
                                  value = costs_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD20",
                                  sliderInput(
                                    "param34_1",
                                    label = "SD = ",
                                    min = 0.0000001 * costs_par_sd_so,
                                    max = 10 * costs_par_sd_so,
                                    value = costs_par_sd_so
                                  )
                                )),
                                sliderInput(
                                  "param32",
                                  label = "Counts adjustment = ",
                                  min = counts_par_so / 2,
                                  max = 2 * counts_par_so,
                                  value = counts_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD21",
                                  sliderInput(
                                    "param32_1",
                                    label = "SD = ",
                                    min = 0.0000001 * counts_par_sd_so,
                                    max = 10 * counts_par_sd_so,
                                    value = counts_par_sd_so
                                  )
                                ))
                              ), 
                              # end tabpanel research ----
                              #
                              # Begin tabpanel data ----
                              tabPanel("Data",
                                       br(),
                                       a(id="toggleDataSDs", "Show/hide all SDs", href="#"),
                                       br(),
                                       br(),
                                       numericInput(
                                         "param35",
                                         label = "Yearly unit costs in new country (in $US)",
                                         value = round(costs2_ea_in, 2), 
                                         min = 0
                                       ),
                                       bsPopover(
                                         id = "param35",
                                         title = "",
                                         content = "Yearly unit costs in new country (in $US)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD39",
                                         sliderInput(
                                           "param35_1",
                                           label = "SD = ",
                                           min = 0.000001 * costs2_ea_in,
                                           max = 1 * costs2_ea_in,
                                           value = 0.1 * costs2_ea_in,
                                           step = 0.001
                                         )
                                       )),
                                       sliderInput(
                                         "param11",
                                         label = "Exchange rate (\\( ex \\)) = ",
                                         min = ex_rate_so / 2,
                                         max = 2 * ex_rate_so,
                                         value = ex_rate_so
                                       ),
                                       bsPopover(
                                         id = "param11",
                                         title = "",
                                         content = "Exchange rate in 1985? (KSH to International Dollar)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD12",
                                         sliderInput(
                                           "param11_1",
                                           label = "SD = ",
                                           min = 0.000001 * ex_rate_so,
                                           max = 1 * ex_rate_so,
                                           value = 0.1 * ex_rate_so,
                                           step = 0.001
                                         )
                                       )),
                                       sliderInput(
                                         "param12",
                                         label = "growth (\\( g \\)) = ",
                                         min = growth_rate_so / 2,
                                         max = 2 * growth_rate_so,
                                         value = growth_rate_so
                                       ),
                                       bsPopover(
                                         id = "param12",
                                         title = "",
                                         content = "Kenyan Per Capita GDP Growth Rate (2002-2011)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD13",
                                         sliderInput(
                                           "param12_1",
                                           label = "SD = ",
                                           min = 0.000001 * growth_rate_so,
                                           max = 1 * growth_rate_so,
                                           value = 0.1 * growth_rate_so,
                                           step = 0.00001
                                         )
                                       )),
                                       sliderInput(
                                         "param2",
                                         label = "Gov Bonds (\\( i \\))",
                                         min = 0.001,
                                         max = 0.2,
                                         value = round(gov_bonds_so,2)
                                       ), 
                                       bsPopover(
                                         id = "param2",
                                         title = "",
                                         content = "Interest rate on Kenyan government bonds",
                                         placement = "top"
                                       ), 
                                       hidden(div(
                                         id = "SD1",
                                         sliderInput(
                                           "param2_1",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * gov_bonds_so,
                                           value = 0.1 * gov_bonds_so
                                         )
                                       )), 
                                       sliderInput(
                                         "param2_new",
                                         label = "Gov Bonds (\\( i \\))",
                                         min = 0.001,
                                         max = 0.2,
                                         value = gov_bonds_new_so
                                       ), 
                                       bsPopover(
                                         id = "param2_new",
                                         title = "",
                                         content = "Interest rate on Kenyan government bonds",
                                         placement = "top"
                                       ), 
                                       hidden(div(
                                         id = "SD2",
                                         sliderInput(
                                           "param2_1_new",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * gov_bonds_new_so,
                                           value = 0.1 * gov_bonds_new_so
                                         )
                                       )), 
                                       sliderInput(
                                         "param3",
                                         label = "Inflation (\\( \\pi \\) ) = ",
                                         min = 0.001,
                                         max = 0.2,
                                         value = inflation_so
                                       ), 
                                       bsPopover(
                                         id = "param3",
                                         title = "",
                                         content = "Kenyan inflation rate",
                                         placement = "top"
                                       ), 
                                       hidden(div(
                                         id = "SD3",
                                         sliderInput(
                                           "param3_1",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * inflation_so,
                                           value = 0.1 * inflation_so
                                         )
                                       )), 
                                       sliderInput(
                                         "param3_new",
                                         label = "Inflation (\\( \\pi \\) ) = ",
                                         min = 0.001,
                                         max = 0.2,
                                         value = inflation_new_so
                                       ),
                                       bsPopover(
                                         id = "param3_new",
                                         title = "",
                                         content = "Kenyan inflation rate",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD4",
                                         sliderInput(
                                           "param3_1_new",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * inflation_new_so,
                                           value = 0.1 * inflation_new_so
                                         )
                                       )), 
                                       sliderInput(
                                         "param15",
                                         label = "Tax rate = ",
                                         min = tax_so / 2,
                                         max = 2 * tax_so,
                                         value = tax_so,
                                         step = 0.00001
                                       ),
                                       bsPopover(
                                         id = "param15",
                                         title = "",
                                         content = "Kenyan tax rate in 2013?",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD15",
                                         sliderInput(
                                           "param15_1",
                                           label = "SD = ",
                                           min = 0.00001 * tax_so,
                                           max = 1 * tax_so,
                                           value = 0.1 * tax_so,
                                           step = 0.000001
                                         )

                                       ))
                              ), 
                              # end tabpanel data ----                              
                              # Begin tabpanel GW ----
                              tabPanel(
                                "Guesswork",
                                br(),
                                a(id = "toggleGWSDs", "Show/hide all SDs", href =
                                    "#"),
                                br(),
                                br(),
                                sliderInput(
                                  "param31",
                                  label = "Prevalence in new region (\\( \\eta_{new} \\)) = ",
                                  min = 0 ,
                                  max = 1,
                                  value = round(prevalence_r_in, 2)
                                ),
                                bsPopover(
                                  id = "param31",
                                  title = "",
                                  content = "Prevalence of parasitic worms in new population",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD36",
                                  sliderInput(
                                    "param31_1",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 1 ,
                                    value = 0.1
                                  )
                                )),
                                sliderInput(
                                  "param17_new",
                                  label = "Years of treatment in new setting (\\(L_{new} \\))",
                                  min = 0,
                                  max = 10,
                                  step = 0.01,
                                  value = round(years_of_treat_t_so,2)
                                ),
                                bsPopover(
                                  id = "param17_new",
                                  title = "",
                                  content = "Input years of treatment",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD19",
                                  sliderInput(
                                    "param17_1_new",
                                    label = "SD = ",
                                    min = 0.000001 * years_of_treat_t_so,
                                    max = 1 * years_of_treat_t_so,
                                    value = 0.1 * years_of_treat_t_so,
                                    step = 0.0001
                                  )
                                )),
                                sliderInput(
                                  "param33",
                                  label = "Additional costs due to staff time (\\(\\delta_{g} \\))",
                                  min = 0,
                                  max = 2,
                                  step = 0.01,
                                  value = staff_time_so
                                ),
                                bsPopover(
                                  id = "param33",
                                  title = "",
                                  content = "Increased costs due to additional hours taught",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD37",
                                  sliderInput(
                                    "param33_1",
                                    label = "SD = ",
                                    min = 0.0000001 ,
                                    max = 5, 
                                    value = 0.1 * staff_time_so
                                  )
                                ))
                              )
                              # end tabpanel GW ----
                            )
                   )
                 ),
                 mainPanel(
                   fluidRow(id = "output_id1", style = "max-width: 800px; max-height: 700px; position:relative;",
                            plotOutput("plot1")
                   ),
                   fluidRow(id = "output_id2", style = "max-width: 800px; max-height: 300px; position:absolute;top: 500px;",
                            checkboxInput("show_eq", label = "Show equations", value = FALSE),
                            uiOutput('eqns', container = div)
                   )
                 )
               )
    )
  )
)
