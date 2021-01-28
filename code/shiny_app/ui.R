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
    country_total_var = costs_data_in$total,
    country_cost_var = costs_data_in$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("india")
  )
costs_temp_kenya   <-
  costs1_p2_f(
    country_total_var = costs_data_in$total,
    country_cost_var = costs_data_in$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("kenya")
  )
costs_temp_nigeria <-
  costs1_p2_f(
    country_total_var = costs_data_in$total,
    country_cost_var = costs_data_in$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("nigeria")
  )
costs_temp_vietnam <-
  costs1_p2_f(
    country_total_var = costs_data_in$total,
    country_cost_var = costs_data_in$costs_by_country,
    staff_time_var = staff_time_so,
    select_var = list("vietnam")
  )
costs_temp_original <- 0.42


prevalence_india <- prevalence_r_so["india"]
prevalence_kenya <- prevalence_r_so["kenya"]
prevalence_nigeria <- prevalence_r_so["nigeria"]
prevalence_vietnam <- prevalence_r_so["vietnam"]
prevalence_original <- 0.92

length_temp_india <- 1
length_temp_kenya <- 1
length_temp_nigeria <- 1
length_temp_vietnam <- 1
length_temp_original <- 1

nsims <- 1e4

# Before each deployment: copy and paste 'data' and 'rawdata' folders into 'shiny_app\'
# here() creates conflits with shiny deployment. Use source("all_analysis.R") intead
# source(here("code", "shiny_app", "all_analysis.R"))

#fluidPage is something must have
shinyUI(
  fluidPage(

    tags$head(
      tags$style(HTML(
        "
        .main-container{
        margin-right = 0px !important;
        max-width: 98% !important;
        }
        "
      ))
    ),
    theme = shinytheme("cerulean"),
    navbarPage("Open Policy Analysis for Deworming Interventions: Open Output Component",
               # Begin main policy estimate tab ----
               tabPanel(
                "Main Policy Estimate",
                sidebarPanel(
                  style = "width: 100%; max-height: 700px; overflow-y: scroll;",
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
                    style = "width: 100%; height: 100%; max-width: 400px;",
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
                        tags$a(href = "https://bitss-opa.github.io/opa-deworming/", "A detailed report"),
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
                     
                      
                    ),
                    p(
                      "See a full contributors list",
                      tags$a(href = "https://github.com/BITSS-OPA/opa-deworming/blob/master/readme.md", "here."), 
                      br(),
                      "See the dynamic document of this shiny app",
                      tags$a(href = "https://bitss-opa.github.io/opa-deworming/", "here."),
                      br(),
                      "See more OPA projects done by BITSS",
                      tags$a(href = "https://www.bitss.org/opa/projects/", "here.")
                    )
                  ),
                  fluidRow(
                    id = "tPanel_main",
                    style = "max-width: 400px; max-height: 300px; position:relative;",
                    br(),
                    h4(strong("Description of Results")),
                    p(
                      "We simulate finding the lifetime income effects (net of interventions costs)
                      on treated children many times, then plot the values
                              to create this figure. The height of the curve represents
                              how often an outcome appeared, i.e. the highest point
                              means that particular value appeared the most frequently.
                              The blue line indicates that half of all values are
                              on either side of the line."
                    )
                  )
                ),
                 mainPanel(
                   fluidRow(id = "output_id1_main", style = "width: 100%; height: 100%; position: relative",
                            plotOutput("plot1_main")
                   )
                 )
               ),
               # end of main policy estimate tab ----
               # Begin of key assumptions tab ----
               tabPanel(
                 "Key Assumptions",
                 sidebarPanel(
                   div( id = "KA",
                   fluidRow(id = "tPanel1_ka",
                            style = "overflow-y: scroll; width: 100%; height: 100%; position:relative;",
                            numericInput(
                              "param_ka_costs2_ea",
                              label = h4("Yearly unit costs in new country (in 2018 $US)"),
                              value = round(costs2_ea_in, 2),
                              min = 0, step = 0.1
                            ),
                            numericInput(
                              "param_ka_prevl_r",
                              label = h4("Prevalence in new region (\\( \\eta_{new} \\))"),
                              min = 0 ,
                              max = 1,
                              value = round(prevalence_r_in, 2)
                            ),
                            numericInput(
                              "param_ka_years_of_treat_t",
                              label = h4("Length of treatment (years)"),
                              value = round(years_of_treat_t_so, 2),
                              min = 0,
                              max = 6,
                              step = 0.1,
                            ),

                            helpText('For reference:'),
                            helpText(HTML(paste('
                                          <html>
                                          <head>
                                          <style>
                                          table, th, td {
                                            border: 1px solid black;
                                            border-collapse: collapse;
                                          }
                                          th, td {
                                            padding: 5px;
                                          }
                                          th {
                                            text-align: left;
                                          }
                                          </style>
                                          </head>
                                          <body>

                                          <table style="width:100%">
                                            <tr>
                                              <th>Country</th>
                                              <th>Unit Costs</th>
                                              <th>Prevalence</th>
<!--                                          <th>Length of Treatment</th> -->
                                            </tr>
                                            <tr>
                                              <th>India</th>
                                              <td><var>', round(costs_temp_india,2),'</var></td>
                                              <td><var>',round(prevalence_india,2),'</var></td>
<!--                                          <td><var>', round(length_temp_india,2),'</var></td> -->
                                            </tr>
                                            <tr>
                                              <th>Kenya</th>
                                              <td><var>',round(costs_temp_kenya,2),'</var></td>
                                              <td><var>',round(prevalence_kenya,2),'</var></td>
<!--                                          <td><var>',round(length_temp_kenya,2),'</var></td> -->

                                            </tr>
                                            <tr>
                                              <th>Nigeria</th>
                                              <td><var>',round(costs_temp_nigeria,2),'</var></td>
                                              <td><var>',round(prevalence_nigeria,2),'</var></td>
<!--                                          <td><var>',round(length_temp_nigeria,2),'</var></td> -->
                                            </tr>
                                            <tr>
                                              <th>Vietnam</th>
                                              <td><var>',round(costs_temp_vietnam,2),'</var></td>
                                              <td><var>',round(prevalence_vietnam,2),'</var></td>
<!--                                          <td><var>', round(length_temp_vietnam,2),'</var></td> -->
                                            </tr>
                                            <tr>
                                              <th>Original Study</th>
                                              <td><var>', round(costs_temp_original,2),'</var></td>
                                              <td><var>', round(prevalence_original,2),'</var></td>
<!--                                          <td><var>', round(length_temp_original,2),'</var></td> -->
                                            </tr>
                                          </table>

                                          </body>
                                          </html>
                                          '
                            ))),
                            actionButton("updateKA", "Update Plot", class = "btn-primary"),
                            actionButton("resetKA", "Reset Inputs"),
                            downloadButton("downloadPlotKA", "Save Plot")



                   )
                   ),

                 ),
                 mainPanel(
                   fluidRow(id = "output_id1_ka", style = "width: 100%; height: 100%; position: relative",
                            plotOutput("plot1_ka")
                   )
                 )
               ),
               # end of key assumptions tab ----
               # Begin All assumptions tab ----
               tabPanel(
                 "All Assumptions",
                 sidebarPanel(
                   div(id = "All",
                   fluidRow(id = "tPanel",
                            style = "width: 100%; max-height: 100%; position: relative;",
                            # Begin policy estimate description ----
                            selectInput("policy_est",
                                        h4("Choose Your Policy Estimate:"),
                                        choices = policy_estimates_text,
                                        selected = "A3. All income of A2. Main Policy Estimate"),
                            withMathJax(),
                            useShinyjs(),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. Tax revenue' ",
                              helpText(
                               HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)'>Approach 1.1.</a> Welfare measured as additional tax revenue.<br>
                                 - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10 year follow-up. No externalities. <br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With externalities. Tax' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)'>Approach 1.2.</a> Welfare measured as additional tax revenue. <br>
                                 - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10 year follow-up. Including externalities. <br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. All income' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)'>Approach 1.3.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. No externalities. <br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With ext. All income' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#21_Approach_1:_Baird_et_al_(2016)'>Approach 1.4.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings. Including externalities.
                                   Data from 10 year follow-up. Including externalities. <br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. Tax' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#22_Approach_2:_Hamory_et_al_(2020)'>Approach 2.1.</a> Welfare measured as additional tax revenue.<br>
                                 - Benefits: tax revenue over predicted effect on earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities.<br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. All income' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#22_Approach_2:_Hamory_et_al_(2020)'>Approach 2.2.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities.<br>
                                 - Costs: costs of treatment in Kenya in 1998 plus additional
                                   costs due to more schooling</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners'>Approach 3.1.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. No externalities.
                                Adjusted for prevalence and length of treatment. <br>
                                 - Costs: current implementation costs in several settings.</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1, with ext.' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners'>Approach 3.2.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings.
                                   Data from 10 year follow-up. Including externalities.
                                Adjusted for prevalence and length of treatment.<br>
                                 - Costs: current implementation costs in several settings.</p>")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A2. Main Policy Estimate' ",
                              helpText(
                                HTML("<p><a href = 'https://bitss-opa.github.io/opa-deworming/#23_Approach_3:_Combination_of_Previous_Approaches_and_Input_From_Key_Policy_Partners'>Approach 3.3.</a> Welfare measured as additional earnings.<br>
                                 - Benefits: predicted additional earnings.
                                   Data from 10, 15 and 20 year follow-up. No externalities.
                                Adjusted for prevalence and length of treatment.<br>
                                 - Costs: current implementation costs in several settings.</p>")
                              )
                            ),
                            # end policy estimate description ----
                            checkboxInput("rescale",
                                          label = "Click to rescale x-axis. Unclick to fix reference point",
                                          value = FALSE),
                            numericInput("param_num_of_sim",
                                         label = h4("Number of simulations"),
                                         value = 1e4),
                            bsPopover(
                              id = "param_num_of_sim",
                              title = "",
                              content = "For faster computations, change the input to 100.",
                              placement = "top"
                            )
                   ),
                   fluidRow(id = "tPanel1",
                            style = "overflow-y: scroll; width: 100%; max-height: 250px; position: relative",
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
                                  "param_lambda1_male",
                                  label = ("\\( \\lambda_{1m} \\) "),
                                  value = lambda1_so[1]
                                ),
                                bsPopover(
                                  id = "param_lambda1_male",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Male)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD22",
                                  numericInput(
                                    "param_lambda1_male_sd",
                                    label = "SD = ",
                                    value = lambda1_sd_so[1])
                                )),
                                numericInput(
                                  "param_lambda1_female",
                                  label = ("\\( \\lambda_{1f} \\) = "),
                                  value = lambda1_so[2]
                                ),
                                bsPopover(
                                  id = "param_lambda1_female",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Female)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD23",
                                  numericInput(
                                    "param_lambda1_female_sd",
                                    label = "SD = ", value =
                                      lambda1_sd_so[2])
                                )),
                                numericInput(
                                  "param_lambda1_new",
                                  label = ("\\(\\alpha^{pooled} \\) = "),
                                  value = round(lambda1_new_so,2)
                                ),
                                bsPopover(
                                  id = "param_lambda1_new",
                                  title = "",
                                  content = "Increase in yearly earnings (pooling 10, 15, 20 year follow-ups)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD29",
                                  numericInput("param_lambda1_new_sd", label = "SD = ", value = lambda1_new_sd_so)
                                )),
                                sliderInput(
                                  "param_lambda2",
                                  label = "\\( \\lambda_{2} \\) = ",
                                  min = 0,
                                  max = 2 * lambda2_so,
                                  value = lambda2_so * 1
                                ),
                                bsPopover(
                                  id = "param_lambda2",
                                  title = "",
                                  content = "Increase in number of hours worked due to treatment (Externalities included)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD24",
                                  sliderInput(
                                    "param_lambda2_sd",
                                    label = "SD = ",
                                    min = 0.1 * lambda2_sd_so,
                                    max = 5 * lambda2_sd_so,
                                    value = lambda2_sd_so,
                                    step = 1e-2
                                  )
                                )),
                                sliderInput(
                                  "param_prevl_0",
                                  label = "Prevalence in original study (\\( \\eta \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = prevalence_0_so
                                ),
                                bsPopover(
                                  id = "param_prevl_0",
                                  title = "",
                                  content = "Prevalence of parasitic worms in population (Miguel & Kremer 2004)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD32",
                                  sliderInput(
                                    "param_prevl_0_sd",
                                    label = "SD = ",
                                    min = 0.0000001 ,
                                    max = 1 ,
                                    value = 0.1
                                  )
                                )),
                                numericInput("param_wage_ag",
                                             label = "Agri Wages (\\( w_{ag} \\))",
                                             value = wage_ag_so),
                                bsPopover(
                                  id = "param_wage_ag",
                                  title = "",
                                  content = "Average hourly wage of an agricultural worker (KSH)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD5",
                                  numericInput("param_wage_ag_sd",
                                               label = "SD = ",
                                               value = 0.1 * wage_ag_so)
                                )),
                                numericInput("param_wage_non_ag",
                                             label = "Wages in Non-ag Jobs  (\\( w_{ww} \\))",
                                             value = round(wage_ww_so, 2)),
                                bsPopover(
                                  id = "param_wage_non_ag",
                                  title = "",
                                  content = "Average hourly wage of a wage worker",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD6",
                                  numericInput("param_wage_non_ag_sd",
                                               label = "SD = ",
                                               value = round(0.1 * wage_ww_so, 2))
                                )),
                                numericInput("param_profits_se",
                                             label = "Profits se = ",
                                             value = profits_se_so),
                                bsPopover(id = "param_profits_se",
                                          title = "",
                                          content = "Average monthly self-employed profits (self-reported)"),
                                hidden(div(
                                  id = "SD7",
                                  numericInput("param_profits_se_sd",
                                               label = "SD = ",
                                               value = 0.1 * profits_se_so)
                                )),
                                sliderInput(
                                  "param_hours_se_cond",
                                  label = "Hours se (>10) = ",
                                  min = hours_se_cond_so / 2,
                                  max = 2 * hours_se_cond_so,
                                  value = hours_se_cond_so
                                ),
                                bsPopover(
                                  id = "param_hours_se_cond",
                                  title = "",
                                  content = "Average weekly hours worked (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD8",
                                  sliderInput(
                                    "param_hours_se_cond_sd",
                                    label = "SD = ",
                                    min = 0.000001 * hours_se_cond_so,
                                    max = 1 * hours_se_cond_so,
                                    value = 0.1 * hours_se_cond_so
                                  )
                                )),
                                sliderInput(
                                  "param_hours_ag",
                                  label = "\\(\\ H_{ag} \\) = ",
                                  min = hours_ag_so / 2,
                                  max = 2 * hours_ag_so,
                                  value = hours_ag_so
                                ),
                                bsPopover(
                                  id = "param_hours_ag",
                                  title = "",
                                  content = "Average weekly hours worked by agricultural workers (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD9",
                                  sliderInput(
                                    "param_hours_ag_sd",
                                    label = "SD = ",
                                    min = 0.000001 * hours_ag_so,
                                    max = 1 * hours_ag_so,
                                    value = 0.1 * hours_ag_so,
                                    round = -4,
                                    step = 0.001
                                  )
                                )),
                                sliderInput(
                                  "param_hours_ww",
                                  label = "\\(\\ H_{ww} \\) = ",
                                  min = hours_ww_so / 2,
                                  max = 2 * hours_ww_so,
                                  value = hours_ww_so
                                ),
                                bsPopover(
                                  id = "param_hours_ww",
                                  title = "",
                                  content = "Average weekly hours worked by wage earners (control group)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD10",
                                  sliderInput(
                                    "param_hours_ww_sd",
                                    label = "SD = ",
                                    min = 0.000001 * hours_ww_so,
                                    max = 1 * hours_ww_so,
                                    value = 0.1 * hours_ww_so,
                                    step = 0.001
                                  )
                                )),
                                sliderInput(
                                  "param_hours_se",
                                  label = "\\(\\ H_{se} \\) = ",
                                  min = hours_se_so / 2,
                                  max = 2 * hours_se_so,
                                  value = hours_se_so
                                ),
                                bsPopover(
                                  id = "param_hours_se",
                                  title = "",
                                  content = "Average weekly hours worked by self-employed workers (control group - non-agricultural)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD11",
                                  sliderInput(
                                    "param_hours_se_sd",
                                    label = "SD = ",
                                    min = 0.000001 * hours_se_so,
                                    max = 1 * hours_se_so,
                                    value = 0.1 * hours_se_so,
                                    step = 0.001
                                  )
                                )),
                                numericInput(
                                  "param_coef_exp1",
                                  label = ("Coefficients of \\(X_{p} \\) (\\( \\beta_{1} \\)) = "),
                                  value = coef_exp_so[1]
                                ),
                                bsPopover(
                                  id = "param_coef_exp1",
                                  title = "",
                                  content = "Teacher experience coefficient",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD40",
                                  numericInput(
                                    "param_coef_exp1_sd",
                                    label = "SD = ",
                                    value = 0.1 * coef_exp_so[1]
                                  )
                                )),
                                numericInput(
                                  "param_coef_exp2",
                                  label = ("Coefficients of \\(X^{2}p \\) (\\( \\beta_{2} \\)) = "),
                                  value = coef_exp_so[2]
                                ),
                                bsPopover(
                                  id = "param_coef_exp2",
                                  title = "",
                                  content = "Teacher experience coefficient squared",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD41",
                                  numericInput(
                                    "param_coef_exp2_sd",
                                    label = "SD = ",
                                    value = 0.1 * coef_exp_so[2]
                                  )
                                )),
                                sliderInput(
                                  "param_coverage",
                                  label = "Coverage (\\( R \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = coverage_so,
                                  step = 0.01
                                ),
                                bsPopover(
                                  id = "param_coverage",
                                  title = "",
                                  content = "Percent of treated primary schools students",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD14",
                                  sliderInput(
                                    "param_coverage_sd",
                                    label = "SD = ",
                                    min = 0.000001 * coverage_so,
                                    max = 1 * coverage_so,
                                    value = 0.1 * coverage_so,
                                    step = 0.000001
                                  )
                                )),
                                sliderInput(
                                  "param_q_full",
                                  label = "Take-up (\\( Q(S_{2}) \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = q_full_so
                                ),
                                bsPopover(
                                  id = "param_q_full",
                                  title = "",
                                  content = "Take up rate with full subsidy on deworming treatment costs",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD25",
                                  sliderInput(
                                    "param_q_full_sd",
                                    label = "SD = ",
                                    min = 0.00000001 * q_full_so,
                                    max = 1 * q_full_so,
                                    value = 0.1 * q_full_so,
                                    step = 1e-5
                                  )
                                )),
                                sliderInput(
                                  "param_q_zero",
                                  label = "Take-up with no subsidy (\\( Q(S_{1}) \\)) = ",
                                  min = 0,
                                  max = 1,
                                  value = q_zero_so
                                ),
                                bsPopover(
                                  id = "param_q_zero",
                                  title = "",
                                  content = "Take up rate without subsidy on deworming treatment costs (S1 = 0) ",
                                  placement = "top"
                                ),
                                sliderInput(
                                  "param_delta_ed_par",
                                  label = "x * \\(\\Delta{E} \\) = ",
                                  min = 0.0000001,
                                  max = 4,
                                  value = delta_ed_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD27",
                                  sliderInput(
                                    "param_delta_ed_par_sd",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 4,
                                    value = delta_ed_par_so * 0.1
                                  )
                                )),
                                sliderInput(
                                  "param_delta_ed__ext_par",
                                  label = "x * \\(\\Delta{E} \\) (ext)  = ",
                                  min = 0.0000001,
                                  max = 4,
                                  value = delta_ed_ext_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD28",
                                  sliderInput(
                                    "param_delta_ed__ext_par_sd",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 4,
                                    value = delta_ed_ext_par_so * 0.1
                                  )
                                )),
                                numericInput("param_teach_sal", label = "Teacher salary = ", value = teach_sal_so),
                                bsPopover(
                                  id = "param_teach_sal",
                                  title = "",
                                  content = "Average annual salary for Kenyan secondary school teacher",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD33",
                                  numericInput("param_teach_sal_sd", label = "SD = ", value = 0.1 * teach_sal_so)
                                )),
                                numericInput("param_teach_ben", label = "Teacher benefits = ", value = teach_ben_so),
                                bsPopover(
                                  id = "param_teach_ben",
                                  title = "",
                                  content = "Average annual benefits for Kenyan secondary school teacher (in KSH",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD34",
                                  numericInput("param_teach_ben_sd", label = "SD = ", value = 0.1 * teach_ben_so)
                                )),
                                numericInput("param_n_students", label = "Students per teacher = ", value = n_students_so),
                                bsPopover(
                                  id = "param_n_students",
                                  title = "",
                                  content = "Average number for students per teacher",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD35",
                                  numericInput(
                                    "param_n_students_sd",
                                    label = "SD = ",
                                    value = 0.1 * n_students_so
                                  )
                                )),
                                sliderInput(
                                  "param_years_of_treat_0",
                                  label = "Years of treatment in orginal study (\\(L_{0}\\))",
                                  min = 0,
                                  max = 6,
                                  step = 0.01,
                                  value = round(years_of_treat_0_so,2)
                                ),
                                bsPopover(
                                  id = "param_years_of_treat_0",
                                  title = "",
                                  content = "Average years of treatement in Kenya",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD18",
                                  sliderInput(
                                    "param_years_of_treat_0_sd",
                                    label = "SD = ",
                                    min = 0.000001 * years_of_treat_0_so,
                                    max = 1 * years_of_treat_0_so,
                                    value = 0.1 * years_of_treat_0_so,
                                    step = 0.0001
                                  )
                                )),
                                numericInput(
                                  "param_unit_cost_local",
                                  label = "Costs of T (local $) = ",
                                  value = round(unit_cost_local_so,2)),
                                bsPopover(
                                  id = "param_unit_cost_local",
                                  title = "",
                                  content = "Costs of deworming per capita (KSH)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD16",
                                  numericInput(
                                    "param_unit_cost_local_sd",
                                    label = "SD = ",
                                    value = 0.1 * unit_cost_local_so
                                  )
                                )),
                                numericInput("param_unit_cost_2017usdppp",
                                             label = "Costs of T (USD) = ",
                                             value = round(unit_cost_2017usdppp_so, 2)),
                                bsPopover(
                                  id = "param_unit_cost_2017usdppp",
                                  title = "",
                                  content = "Costs of deworming per capita (USD)",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD17",
                                  numericInput(
                                    "param_unit_cost_2017usdppp_sd",
                                    label = "SD = ",
                                    value = 0.1 * unit_cost_2017usdppp_so
                                  )
                                )),
                                sliderInput(
                                  "param_costs_par",
                                  label = "Costs adjustments = ",
                                  min = costs_par_so / 2,
                                  max = 20000 * costs_par_so,
                                  value = costs_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD20",
                                  sliderInput(
                                    "param_costs_par_sd",
                                    label = "SD = ",
                                    min = 0.0000001 * costs_par_sd_so,
                                    max = 10 * costs_par_sd_so,
                                    value = costs_par_sd_so
                                  )
                                )),
                                sliderInput(
                                  "param_counts_par",
                                  label = "Counts adjustment = ",
                                  min = counts_par_so / 2,
                                  max = 2 * counts_par_so,
                                  value = counts_par_so
                                ),
                                #need more info for Popover
                                hidden(div(
                                  id = "SD21",
                                  sliderInput(
                                    "param_counts_par_sd",
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
                                         "param_costs2_ea",
                                         label = "Yearly unit costs in new country (in $US)",
                                         value = round(costs2_ea_in, 2),
                                         min = 0
                                       ),
                                       bsPopover(
                                         id = "param_costs2_ea",
                                         title = "",
                                         content = "Yearly unit costs in new country (in 2018 $US)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD39",
                                         sliderInput(
                                           "param_costs2_ea_sd",
                                           label = "SD = ",
                                           min = 0.000001 * costs2_ea_in,
                                           max = 1 * costs2_ea_in,
                                           value = 0.1 * costs2_ea_in,
                                           step = 0.001
                                         )
                                       )),
                                       sliderInput(
                                         "param_ex_rate",
                                         label = "Exchange rate (\\( ex \\)) = ",
                                         min = ex_rate_so / 2,
                                         max = 2 * ex_rate_so,
                                         value = ex_rate_so
                                       ),
                                       bsPopover(
                                         id = "param_ex_rate",
                                         title = "",
                                         content = "Exchange rate in 1985? (KSH to International Dollar)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD12",
                                         sliderInput(
                                           "param_ex_rate_sd",
                                           label = "SD = ",
                                           min = 0.000001 * ex_rate_so,
                                           max = 1 * ex_rate_so,
                                           value = 0.1 * ex_rate_so,
                                           step = 0.001
                                         )
                                       )),
                                       sliderInput(
                                         "param_growth_rate",
                                         label = "growth (\\( g \\)) = ",
                                         min = growth_rate_so / 2,
                                         max = 2 * growth_rate_so,
                                         value = growth_rate_so
                                       ),
                                       bsPopover(
                                         id = "param_growth_rate",
                                         title = "",
                                         content = "Kenyan Per Capita GDP Growth Rate (2002-2011)",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD13",
                                         sliderInput(
                                           "param_growth_rate_sd",
                                           label = "SD = ",
                                           min = 0.000001 * growth_rate_so,
                                           max = 1 * growth_rate_so,
                                           value = 0.1 * growth_rate_so,
                                           step = 0.00001
                                         )
                                       )),
                                       sliderInput(
                                         "param_gov_bonds16",
                                         label = "Gov Bonds (2016) (\\( i \\))",
                                         min = 0.001,
                                         max = 0.2,
                                         value = round(gov_bonds_so,2)
                                       ),
                                       bsPopover(
                                         id = "param_gov_bonds16",
                                         title = "",
                                         content = "Interest rate on Kenyan government bonds",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD1",
                                         sliderInput(
                                           "param_gov_bonds16_sd",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * gov_bonds_so,
                                           value = 0.1 * gov_bonds_so
                                         )
                                       )),
                                       sliderInput(
                                         "param_gov_bonds19",
                                         label = "Gov Bonds (2019) (\\( i \\))",
                                         min = 0.001,
                                         max = 0.2,
                                         value = gov_bonds_new_so
                                       ),
                                       bsPopover(
                                         id = "param_gov_bonds19",
                                         title = "",
                                         content = "Interest rate on Kenyan government bonds",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD2",
                                         sliderInput(
                                           "param_gov_bonds19_sd",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * gov_bonds_new_so,
                                           value = 0.1 * gov_bonds_new_so
                                         )
                                       )),
                                       sliderInput(
                                         "param_inflation16",
                                         label = "Inflation (2016) (\\( \\pi \\) ) = ",
                                         min = 0.001,
                                         max = 0.2,
                                         value = inflation_so
                                       ),
                                       bsPopover(
                                         id = "param_inflation16",
                                         title = "",
                                         content = "Kenyan inflation rate",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD3",
                                         sliderInput(
                                           "param_inflation16_sd",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * inflation_so,
                                           value = 0.1 * inflation_so
                                         )
                                       )),
                                       sliderInput(
                                         "param_inflation19",
                                         label = "Inflation (2019) (\\( \\pi \\) ) = ",
                                         min = 0.001,
                                         max = 0.2,
                                         value = inflation_new_so
                                       ),
                                       bsPopover(
                                         id = "param_inflation19",
                                         title = "",
                                         content = "Kenyan inflation rate",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD4",
                                         sliderInput(
                                           "param_inflation19_sd",
                                           label = "SD = ",
                                           min = 0.0000001,
                                           max = 0.4 * inflation_new_so,
                                           value = 0.1 * inflation_new_so
                                         )
                                       )),
                                       sliderInput(
                                         "param_tax",
                                         label = "Tax rate = ",
                                         min = tax_so / 2,
                                         max = 2 * tax_so,
                                         value = tax_so,
                                         step = 0.00001
                                       ),
                                       bsPopover(
                                         id = "param_tax",
                                         title = "",
                                         content = "Kenyan tax rate in 2013?",
                                         placement = "top"
                                       ),
                                       hidden(div(
                                         id = "SD15",
                                         sliderInput(
                                           "param_tax_sd",
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
                                  "param_prevl_r",
                                  label = "Prevalence in new region (\\( \\eta_{new} \\)) = ",
                                  min = 0 ,
                                  max = 1,
                                  value = round(prevalence_r_in, 2)
                                ),
                                bsPopover(
                                  id = "param_prevl_r",
                                  title = "",
                                  content = "Prevalence of parasitic worms in new population",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD36",
                                  sliderInput(
                                    "param_prevl_r_sd",
                                    label = "SD = ",
                                    min = 0.0000001,
                                    max = 1 ,
                                    value = 0.1
                                  )
                                )),
                                sliderInput(
                                  "param_years_of_treat_t",
                                  label = "Years of treatment in new setting (\\(L_{new} \\))",
                                  min = 0,
                                  max = 6,
                                  step = 0.01,
                                  value = round(years_of_treat_t_so,2)
                                ),
                                bsPopover(
                                  id = "param_years_of_treat_t",
                                  title = "",
                                  content = "Input years of treatment",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD19",
                                  sliderInput(
                                    "param_years_of_treat_t_sd",
                                    label = "SD = ",
                                    min = 0.000001 * years_of_treat_t_so,
                                    max = 1 * years_of_treat_t_so,
                                    value = 0.1 * years_of_treat_t_so,
                                    step = 0.0001
                                  )
                                )),
                                sliderInput(
                                  "param_staff_time",
                                  label = "Additional costs due to staff time (\\(\\delta_{g} \\))",
                                  min = 0,
                                  max = 2,
                                  step = 0.01,
                                  value = staff_time_so
                                ),
                                bsPopover(
                                  id = "param_staff_time",
                                  title = "",
                                  content = "Increased costs due to additional hours taught",
                                  placement = "top"
                                ),
                                hidden(div(
                                  id = "SD37",
                                  sliderInput(
                                    "param_staff_time_sd",
                                    label = "SD = ",
                                    min = 0.0000001 ,
                                    max = 5,
                                    value = 0.1 * staff_time_so
                                  )
                                ))
                              )
                              # end tabpanel GW ----
                            )
                   ),
                   fluidRow(id = "buttonUpdate",
                            style = "position:relative; padding-top:10px",
                            actionButton("updateAll", "Update Plot", class="btn-primary"),
                            actionButton("resetAll", "Reset Inputs")
                            
                            ),
                   fluidRow(id = "buttonDownload",
                            style = "position:relative; padding-top:10px;",
                            downloadButton("downloadParams", "Output Parameters"),
                            downloadButton("downloadPlotAll", "Save Plot")
                            )
                 )),
                 mainPanel(
                   fluidRow(id = "output_id1", style = "width: 100%; height: 100%; position:relative;",
                            plotOutput("plot1")
                   ),
                   fluidRow(id = "output_id2", style = "width: 100%; height: auto; position: absolute; top: 550px",
                            uiOutput('eqns', container = div)
                   )
                 )
               )
    )

  )


)
