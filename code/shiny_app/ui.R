
library(shiny)
library(tidyverse)
library(haven)
library(here)
library(kableExtra)
library(readxl)
library(shinyjs)
#library(plotly)
library(shinyBS)

# not sure if this makes a difference
knitr::opts_knit$set(root.dir = here())

costs_temp_india   <- 0.0478
costs_temp_kenya   <- 0.418
costs_temp_nigeria <- 0.661
costs_temp_vietnam <- 0.399


prevalence_india <- 0.57
prevalence_kenya <- 0.35
prevalence_nigeria <- 0.27
prevalence_vietnam <- 0.15


nsims <- 1e3

# Before each deployment: copy and paste 'data' and 'rawdata' folders into 'shiny_app\'
# here() creates conflits with shiny deployment. Use source("all_analysis.R") intead
# source(here("code", "shiny_app", "all_analysis.R"))
source("all_analysis.R")
#fluidPage is something must have
shinyUI(
  fluidPage( theme = shinytheme("cerulean"),
    navbarPage("Open Policy Analysis for Deworming Interventions: Open Output Component",
               tabPanel(
                 "Main Policy Estimate",
                 sidebarPanel(fluidRow(
                   column(12, align= "center",
                   a(img(src="bitss_just_logo_transparent.png", width="20%", height="auto"), href="https://bitss.org"),
                   a(img(src="cega_transparent.png", width="70%", height="auto"), href="https://cega.berkeley.edu"))),
                   fluidRow(id = "tPanel_main", style = "max-width: 400px; max-height: 300px; position:relative;",
                            br(),
                            h4(strong("Description of Results")),
                            p("We simulate finding the lifetime income effects on
                              treated children many times, then plot the values
                              to create this curve. The height of the curve represents
                              how often an outcome appeared, i.e. the highest point
                              means that particular value appeared the most frequently.
                              The blue line indicates that half of all values are
                              on either side of the line.")
                   ),
                   fluidRow(p("Under the other two tabs, you can adjust the model's
                              assumptions and rerun the simulation to explore the
                              impact on lifetime income effects."),
                            br(),
                            br(),
                            p("The app is the result of a collaboration between the",
                              tags$a(href="https://www.bitss.org/", "Berkeley Initiative
                                     for Transparency in the Social Sciences"),
                              "and",
                              tags$a(href="https://www.evidenceaction.org/dewormtheworld-2/",
                                     "Evidence Action.")),
                            p("This visualization is one of three key components of an",
                              tags$a(href="http://www.bitss.org/opa/projects/deworming/","Open Policy Analysis (OPA)"),
                            "on the costs and benefits of
                            mass deworming interventions in various settings. For example:",
                              tags$li(tags$span("item1")),tags$li(tags$span("item2"))),
                            p("The other two components correspond to",
                            tags$a(href="https://rpubs.com/fhoces/547979", "detailed documentation"),
                            " of all the analysis, and",
                            tags$a(href="https://github.com/BITSS-OPA/opa-deworming", "all the materials"),
                            "required to reproduce
                            the analysis with minimal effort. Together, these materials create a transparent and
                            reproducible analysis to facilitate collaboration and
                            discussion about deworming policy."),
                            )
                 ),
                 mainPanel(
                   fluidRow(id = "output_id1_main", style = "max-width: 800px; max-height: 700px; position:relative;",
                            plotOutput("plot1_main")
                   )
                 )
               ),
               tabPanel(
                 "Key Assumptions", #TO DO: repeat all code but with costs and prevalence as reactive only
                 sidebarPanel(
                   fluidRow(id = "tPanel_ka",style = "max-width: 400px; max-height: 400px; position:relative;",
                            withMathJax(),
                            useShinyjs(),
                            helpText("Choose the indicator to be your policy estimate"),
                            selectInput("policy_est_ka", "Policy Estimates",
                                        choices = policy_estimates_text,
                                        selected = "A3. All income of A2. Main Policy Estimate"),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A1. Tax revenue' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(), br(),
                                       "Benefits: Baird approach with tax included but not externalities", br(),br(),
                                       "Costs: Baird approach with no externalities"
                                       )
                            ),

                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A1. With externalities. Tax' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(), br(),
                                       "Benefits: Baird approach with both tax and externalities included", br(),br(),
                                       "Costs: Baird approach with externalities included")
                            ),

                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A1. All income' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax or externalities", br(),br(),
                                       "Costs: Baird approach with no externalities")
                            ),

                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A1. With ext. All income' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax but with externalities", br(),br(),
                                       "Costs: Baird approach with externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A2. Tax' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) with tax but not externalities", br(),br(),
                                       "Costs: Hamory approach (KLPS) with no externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A2. All income' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) without tax or externalities", br(),br(),
                                       "Costs: Hamory approach (KLPS) with no externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A3. All income of A1' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax or externalities but with prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A3. All income of A1, with ext.' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax but with externalities, prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est_ka == 'A3. All income of A2. Main Policy Estimate' ",
                              helpText("When we calculate NPV, we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) without tax or externalities but with prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            )


                   ),
                   fluidRow(id = "tPanel1_ka",style = "overflow-y:scroll; max-width: 600px; max-height: 600px; position:relative;",
                            numericInput("param35", label = h3("Unit costs in new country"), value = round(costs2_ea_in,2)),

                            # checkboxGroupInput("param36", "Choose countries:",
                            #                    choiceNames =
                            #                      list("India", "Kenya", "Nigeria", "Vietnam"),
                            #                    choiceValues =
                            #                      list("india", "kenya", "nigeria", "vietnam"),
                            #                    selected = list("india", "kenya", "nigeria", "vietnam")  ),
                            helpText("For reference:", br(),
                                     paste("Unit costs in India is", costs_temp_india), br(),
                                     paste("Unit costs in Kenya is", costs_temp_kenya), br(),
                                     paste("Unit costs in Nigeria is", costs_temp_nigeria), br(),
                                     paste("Unit costs in Vietnam is", costs_temp_vietnam)),
                            numericInput("param37", label = h3("Prevalence in the new region"), value = round(prevalence_r_in,2)),
                            helpText("For reference:", br(),
                                     paste("Prevalence in India is", prevalence_india), br(),
                                     paste("Prevalence in Kenya is", prevalence_kenya), br(),
                                     paste("Prevalence in Nigeria is", prevalence_nigeria), br(),
                                     paste("Prevalence in Vietnam is", prevalence_vietnam))
                   )
                 ),
                 mainPanel(
                   fluidRow(id = "output_id1_ka", style = "max-width: 800px; max-height: 700px; position:relative;",
                            plotOutput("plot1_ka")
                   )
                 )
               ),
               # Begin All assumptions tab ----
               tabPanel(
                 "All Assumptions",
                 sidebarPanel(
                   fluidRow(id = "tPanel",style = "max-width: 600px; max-height: 400px; position:relative;",

                            checkboxInput("rescale", label = "Click if want to rescale x-axis", value = TRUE),
                            numericInput("param1", label = h4("Number of simulations"), value = 1e3),
                            withMathJax(),
                            useShinyjs(),
                            selectInput("policy_est", "Policy Estimate:",
                                        choices = policy_estimates_text,
                                        selected = "A3. All income of A2. Main Policy Estimate"),
                            conditionalPanel(
                              condition = "input.policy_est == 'A1. Tax revenue' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(), br(),
                                       "Benefits: Baird approach with tax included but not externalities", br(), br(),
                                       "Costs: Baird approach with no externalities"
                              )
                            ),

                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With externalities. Tax' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(), br(),
                                       "Benefits: Baird approach with both tax and externalities included", br(),br(),
                                       "Costs: Baird approach with externalities included")
                            ),

                            conditionalPanel(
                              condition = "input.policy_est == 'A1. All income' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax or externalities", br(),br(),
                                       "Costs: Baird approach with no externalities")
                            ),

                            conditionalPanel(
                              condition = "input.policy_est == 'A1. With ext. All income' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax but with externalities", br(),br(),
                                       "Costs: Baird approach with externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. Tax' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) with tax but not externalities", br(),br(),
                                       "Costs: Hamory approach (KLPS) with no externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A2. All income' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) without tax or externalities", br(),br(),
                                       "Costs: Hamory approach (KLPS) with no externalities")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax or externalities but with prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A1, with ext.' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Baird approach without tax but with externalities, prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            ),
                            conditionalPanel(
                              condition = "input.policy_est == 'A3. All income of A2. Main Policy Estimate' ",
                              helpText("Our final policy estimate is NPV, and we make assumptions as below:", br(),br(),
                                       "Benefits: Hamory approach (KLPS) without tax or externalities but with prevalence and length of treatment considered", br(),br(),
                                       "Costs: Evidence Action (EA) Approach")
                            )

                   ),
                   fluidRow(id = "tPanel1",style = "overflow-y:scroll; max-width: 600px; max-height: 400px; position:relative;",
                            tabsetPanel(
                              # Begin tabpanel data ----
                              tabPanel("Data",
                                       br(),
                                       a(id="toggleDataSDs", "Show/hide all SDs", href="#"),
                                       br(),
                                       br(),
                                       sliderInput("param2", label = "Gov Bonds (\\( i \\))",
                                                   min = 0.001, max = 0.2, value = gov_bonds_so),
                                       bsPopover(id="param2", title="", content="Interest rate on Kenyan government bonds", placement="top"),
                                       hidden(div(id="SD1",
                                                  sliderInput("param2_1", label = "SD = ", min = 0.0000001, max = 0.4 * gov_bonds_so, value = 0.1 * gov_bonds_so))),
                                       sliderInput("param2_new", label = "Gov Bonds (\\( i \\))",
                                                   min = 0.001, max = 0.2, value = gov_bonds_new_so),
                                       bsPopover(id="param2_new", title="", content="Interest rate on Kenyan government bonds", placement="top"),
                                       hidden(div(id="SD2",
                                                  sliderInput("param2_1_new", label = "SD = ", min = 0.0000001, max = 0.4 * gov_bonds_new_so, value = 0.1 * gov_bonds_new_so))),
                                       sliderInput("param3", label = "Inflation (\\( \\pi \\) ) = ",
                                                   min = 0.001, max = 0.2, value = inflation_so),
                                       bsPopover(id="param3", title="", content="Kenyan inflation rate", placement= "top"),
                                       hidden(div(id="SD3",
                                                  sliderInput("param3_1", label = "SD = ", min = 0.0000001, max = 0.4 * inflation_so, value = 0.1 * inflation_so))),
                                       sliderInput("param3_new", label = "Inflation (\\( \\pi \\) ) = ",
                                                   min = 0.001, max = 0.2, value = inflation_new_so),
                                       bsPopover(id="param3_new",title="", content="Kenyan inflation rate", placement="top"),
                                       hidden(div(id="SD4",
                                                  sliderInput("param3_1_new", label = "SD = ", min = 0.0000001, max = 0.4 * inflation_new_so, value = 0.1 * inflation_new_so))),
                                       numericInput("param4", label = "Agri Wages (\\( w_{ag} \\))", value = wage_ag_so),
                                       bsPopover(id="param4",title="", content="Average hourly wage of an agricultural worker (KSH)", placement="top"),
                                       hidden(div(id="SD5",
                                                  numericInput("param4_1", label = "SD = ", value = 0.1 * wage_ag_so))),
                                       numericInput("param5", label = "Work-non ag-Wages  (\\( w_{ww} \\))", value = wage_ww_so),
                                       bsPopover(id="param5",title="", content="Average hourly wage of a wage worker", placement="top"),
                                       hidden(div(id="SD6",
                                                  numericInput("param5_1", label = "SD = ", value = 0.1 * wage_ww_so))),
                                       numericInput("param6", label = "Profits se = ", value = profits_se_so),
                                       bsPopover(id="param6",title="", content="Average monthly self-employed profits (self-reported)"),
                                       hidden(div(id="SD7",
                                                  numericInput("param6_1", label = "SD = ", value = 0.1 * profits_se_so))),
                                       sliderInput("param7", label = "Hours se (>10) = ",
                                                   min = hours_se_cond_so / 2, max = 2 * hours_se_cond_so, value = hours_se_cond_so),
                                       bsPopover(id="param7",title="", content="Average weekly hours worked (control group)", placement="top"),
                                       hidden(div(id="SD8",
                                                  sliderInput("param7_1", label = "SD = ", min = 0.000001* hours_se_cond_so, max = 1 * hours_se_cond_so, value = 0.1 * hours_se_cond_so))),
                                       sliderInput("param8", label = "\\(\\ H_{ag} \\) = ",
                                                   min = hours_ag_so / 2, max = 2 * hours_ag_so, value = hours_ag_so),
                                       bsPopover(id="param8",title="", content="Average weekly hours worked by agricultural workers (control group)", placement = "top"),
                                       hidden(div(id="SD9",
                                                  sliderInput("param8_1", label = "SD = ", min = 0.000001* hours_ag_so, max = 1 * hours_ag_so, value = 0.1 * hours_ag_so, round = -4, step = 0.001))),
                                       sliderInput("param9", label = "\\(\\ H_{ww} \\) = ",
                                                   min = hours_ww_so / 2, max = 2 * hours_ww_so, value = hours_ww_so),
                                       bsPopover(id="param9",title="", content="Average weekly hours worked by wage earners (control group)", placement="top"),
                                       hidden(div(id="SD10",
                                                  sliderInput("param9_1", label = "SD = ", min = 0.000001* hours_ww_so, max = 1 * hours_ww_so, value = 0.1 * hours_ww_so, step = 0.001))),
                                       sliderInput("param10", label = "\\(\\ H_{se} \\) = ",
                                                   min = hours_se_so / 2, max = 2 * hours_se_so, value = hours_se_so),
                                       bsPopover(id="param10",title="", content="Average weekly hours worked by self-employed workers (control group - non-agricultural)", placement="top"),
                                       hidden(div(id="SD11",
                                                  sliderInput("param10_1", label = "SD = ", min = 0.000001* hours_se_so, max = 1 * hours_se_so, value = 0.1 * hours_se_so, step = 0.001))),
                                       sliderInput("param11", label = "Exchange rate (\\( ex \\)) = ",
                                                   min = ex_rate_so / 2, max = 2 * ex_rate_so, value = ex_rate_so),
                                       bsPopover(id="param11",title="",content="Exchange rate in 1985? (KSH to International Dollar)", placement="top"),
                                       hidden(div(id="SD12",
                                                  sliderInput("param11_1", label = "SD = ", min = 0.000001* ex_rate_so, max = 1 * ex_rate_so, value = 0.1 * ex_rate_so, step = 0.001))),
                                       sliderInput("param12", label = "growth (\\( g \\)) = ",
                                                   min = growth_rate_so / 2, max = 2 * growth_rate_so, value = growth_rate_so),
                                       bsPopover(id="param12",title="", content="Kenyan Per Capita GDP Growth Rate (2002-2011)", placement="top"),
                                       hidden(div(id="SD13",
                                                  sliderInput("param12_1", label = "SD = ", min = 0.000001* growth_rate_so, max = 1 * growth_rate_so, value = 0.1 * growth_rate_so, step = 0.00001))),
                                       sliderInput("param13", label = "Coverage (\\( R \\)) = ",
                                                   min = 0, max = 1, value = coverage_so, step = 0.01),
                                       bsPopover(id="param13",title="", content="Percent of treated primary schools students", placement="top"),
                                       hidden(div(id="SD14",
                                                  sliderInput("param13_1", label = "SD = ", min = 0.000001* coverage_so, max = 1 * coverage_so, value = 0.1 * coverage_so, step = 0.000001))),
                                       sliderInput("param15", label = "Tax rate = ",
                                                   min = tax_so / 2, max = 2 * tax_so, value = tax_so, step = 0.00001),
                                       bsPopover(id="param15",title="", content="Kenyan tax rate in 2013?", placement="top"),
                                       hidden(div(id="SD15",
                                                  sliderInput("param15_1", label = "SD = ", min = 0.00001* tax_so, max = 1 * tax_so, value = 0.1 * tax_so, step = 0.000001))),
                                       numericInput("param16", label = "Costs of T (local $) = ", value = unit_cost_local_so),
                                       bsPopover(id="param16",title="", content="Costs of deworming per capita (KSH)", placement="top"),
                                       hidden(div(id="SD16",
                                                  numericInput("param16_1", label = "SD = ", value = 0.1 * unit_cost_local_so))),
                                       numericInput("param16_new", label = "Costs of T (local $) = ", value = unit_cost_2017usdppp_so),
                                       bsPopover(id="param16_new",title="", content="Costs of deworming per capita (USD)", placement="top"),
                                       hidden(div(id="SD17",
                                                  numericInput("param16_1_new", label = "SD = ", value = 0.1 * unit_cost_2017usdppp_so))),
                                       sliderInput("param17", label = "Years of treatment in orginal study",
                                                   min = years_of_treat_0_so / 2, max = 2 * years_of_treat_0_so, value = years_of_treat_0_so),
                                       bsPopover(id="param17",title="", content="Average years of treatement in Kenya", placement = "top"),
                                       hidden(div(id="SD18",
                                                  sliderInput("param17_1", label = "SD = ", min = 0.000001* years_of_treat_0_so, max = 1 * years_of_treat_0_so, value = 0.1 * years_of_treat_0_so, step = 0.0001))),
                                       sliderInput("param17_new", label = "Years of treatment in new setting",
                                                   min = years_of_treat_t_so / 2, max = 2 * years_of_treat_t_so, value = years_of_treat_t_so),
                                       bsPopover(id="param17_new",title="", content="Input years of treatment", placement="top"),
                                       hidden(div(id="SD19",
                                                  sliderInput("param17_1_new", label = "SD = ", min = 0.000001* years_of_treat_t_so, max = 1 * years_of_treat_t_so, value = 0.1 * years_of_treat_t_so, step = 0.0001))),
                                       sliderInput("param34", label = "Costs adjustments = ",
                                                   min = costs_par_so / 2, max = 20000 * costs_par_so, value = costs_par_so),
                                       #need more info for Popover
                                       hidden(div(id="SD20",
                                                  sliderInput("param34_1", label = "SD = ", min = 0.0000001* costs_par_sd_so, max = 10 * costs_par_sd_so, value = costs_par_sd_so))),
                                       sliderInput("param32", label = "Counts adjustment = ",
                                                   min = counts_par_so / 2, max = 2 * counts_par_so, value = counts_par_so),
                                       #need more info for Popover
                                       hidden(div(id="SD21",
                                                  sliderInput("param32_1", label = "SD = ", min = 0.0000001 * counts_par_sd_so, max = 10 * counts_par_sd_so, value = counts_par_sd_so)))
                              ),
                              # end tabpanel data ----
                              #
                              # Begin tabpanel research ----
                              tabPanel("Research",
                                       br(),
                                       a(id="toggleResearchSDs", "Show/hide all SDs", href="#"),
                                       br(),
                                       br(),
                                       numericInput("param18_1", label = ("\\( \\lambda_{1m} \\) "), value = lambda1_so[1]),
                                       bsPopover(id="param18_1", title="", content="Increase in number of hours worked due to treatment (Male)", placement="top"),
                                       hidden(div(id="SD22",
                                                  numericInput("param18_1_1", label = h3("SD = "), value = 0.17))),
                                       numericInput("param18_2", label = ("\\( \\lambda_{1m} \\) = "), value = lambda1_so[2]),
                                       bsPopover(id="param18_2", title="", content="Increase in number of hours worked due to treatment (Female)", placement="top"),
                                       hidden(div(id="SD23",
                                                  numericInput("param18_2_1", label = h3("SD = "), value = 0.17))),
                                       sliderInput("param19", label = "\\( \\lambda_{2} \\) = ",
                                                   min = 0, max = 2 * lambda2_so, value = lambda2_so * 1),
                                       bsPopover(id="param19", title="", content="Increase in number of hours worked due to treatment (Externalities included)", placement="top"),
                                       hidden(div(id="SD24",
                                                  sliderInput("param19_1", label = "SD = ", min = 0.0000001* lambda2_so, max = 1 * lambda2_so, value = 0.1 * lambda2_so, step = 1e-5))),
                                       sliderInput("param20", label = "Take-up (\\( Q_{full} \\)) = ",
                                                   min = 0, max = 1, value = q_full_so),
                                       bsPopover(id="param20", title="", content="Take up rate with full subsidy on deworming treatment costs", placement="top"),
                                       hidden(div(id="SD25",
                                                  sliderInput("param20_1", label = "SD = ", min = 0.00000001* q_full_so, max = 1 * q_full_so, value = 0.1 * q_full_so, step = 1e-5))),
                                       sliderInput("param28", label = "Take-up with no subsidy (\\( Q_{0} \\)) = ",
                                                   min = 0, max = 1, value = q_zero_so),
                                       bsPopover(id="param28", title="", content="Take up rate without subsidy on deworming treatment costs", placement="top"),
                                       hidden(div(id="SD26",
                                                  sliderInput("param28_1", label = "SD = ", min = 0.00000001* q_zero_so, max = 1 * q_zero_so, value = 0.1 * q_zero_so))),
                                       sliderInput("param26", label = "x * \\(\\Delta{E} \\) = ",
                                                   min = 0.0000001, max = 4, value = delta_ed_par_so),
                                       #need more info for Popover
                                       hidden(div(id="SD27",
                                                  sliderInput("param26_1", label = "SD = ", min = 0.0000001, max = 4, value = delta_ed_par_so * 0.1))),
                                       sliderInput("param27", label = "x * \\(\\Delta{E} \\) (ext)  = ",
                                                   min = 0.0000001, max = 4, value = delta_ed_ext_par_so),
                                       #need more info for Popover
                                       hidden(div(id="SD28",
                                                  sliderInput("param27_1", label = "SD = ", min = 0.0000001, max = 4, value = delta_ed_ext_par_so * 0.1))),
                                       numericInput("param29_1", label = ("\\(\\lambda_{KLPS1} \\) = "), value = lambda1_new_so[1]),
                                       #need more info for Popover
                                       hidden(div(id="SD29",
                                                  numericInput("param29_1_1", label = h3("SD = "), value = lambda1_new_sd_so[1]))),
                                       numericInput("param29_2", label = ("\\(\\lambda_{KLPS2} \\) = "), value = lambda1_new_so[2]),
                                       
                                       hidden(div(id="SD30",
                                                  numericInput("param29_2_1", label = h3("SD = "), value = lambda1_new_sd_so[2]))),
                                       numericInput("param29_3", label = ("\\(\\lambda_{KLPS3} \\) = "), value = lambda1_new_so[3]),
                                       #need more info for Popover
                                       hidden(div(id="SD31",
                                                  numericInput("param29_3_1", label = h3("SD = "), value = lambda1_new_sd_so[3]))),
                                       sliderInput("param30", label = "Prevalence in original study (\\( \\eta \\)) = ",
                                                   min = 0, max = 1, value = prevalence_0_so),
                                       bsPopover(id="param30", title="", content="Prevalence of parasitic worms in population (Miguel & Kremer 2004)", placement = "top"),
                                       hidden(div(id="SD32",
                                                  sliderInput("param30_1", label = "SD = ", min = 0.0000001 , max = 1 , value = 0.1)))
                              ),
                              # end tabpanel research ----
                              #
                              # Begin tabpanel GW ----
                              tabPanel("Guesswork",
                                       br(),
                                       a(id="toggleGWSDs", "Show/hide all SDs", href="#"),
                                       br(),
                                       br(),
                                       numericInput("param21_1", label = ("Coefficients of \\(X_{p} \\) (\\( \\beta_{1} \\)) = "), value = coef_exp_so[1]),
                                       bsPopover(id="param21_1", title="", content= "Teacher experience coefficient", placement="top"),
                                       numericInput("param21_2", label = ("Coefficients of \\(X^{2}p \\) (\\( \\beta_{2} \\)) = "), value = coef_exp_so[2]),
                                       bsPopover(id="param21_2", title="", content="Teacher experience coefficient squared", placement="top"),
                                       numericInput("param22", label = "Teacher salary = ", value = teach_sal_so),
                                       bsPopover(id="param22", title="", content="Average annual salary for Kenyan secondary school teacher", placement="top"),
                                       hidden(div(id="SD33",
                                                  numericInput("param22_1", label = "SD = ", value = 0.1 * teach_sal_so))),
                                       numericInput("param23", label = "Teacher benefits = ", value = teach_ben_so),
                                       bsPopover(id="param23", title="", content="Average annual benefits for Kenyan secondary school teacher (in KSH", placement="top"),
                                       hidden(div(id="SD34",
                                                  numericInput("param23_1", label = "SD = ", value = 0.1 * teach_ben_so))),
                                       numericInput("param24", label = "Students per teacher = ", value = n_students_so),
                                       bsPopover(id="param24", title="", content="Average number for students per teacher", placement="top"),
                                       hidden(div(id="SD35",
                                                  numericInput("param24_1", label = "SD = ", value = 0.1 * n_students_so))),
                                       sliderInput("param31", label = "Prevalence (\\( \\eta \\)) = ",
                                                   min = 0 , max = 1, value = prevalence_0_so),
                                       bsPopover(id="param31", title="", content="Prevalence of parasitic worms in population (Miguel & Kremer 2004)", placement = "top"),
                                       hidden(div(id="SD36",
                                                  sliderInput("param31_1", label = "SD = ", min = 0.0000001, max = 1 , value = 0.1))),
                                       sliderInput("param33", label = "Additional costs due to staff time = ",
                                                   min = staff_time_so / 2, max = 2 * staff_time_so, value = staff_time_so),
                                       bsPopover(id="param33", title="", content="Increased costs due to additional hours taught", placement = "top"),
                                       hidden(div(id="SD37",
                                                  sliderInput("param33_1", label = "SD = ", min = 0.0000001* staff_time_so, max = 1 * staff_time_so, value = 0.1 * staff_time_so)))
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
