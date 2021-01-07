## ----setup, include=FALSE, purl=TRUE------------------------------------------------------------------------------
html_format <- TRUE
# Loading required libraries
# before deploying in shinyapps.io, need to remove the following packages:
# "plotly", "bookdown", "rootSolve"
if (FALSE) {
  list.of.packages <- c("tidyverse", "here", "kableExtra", "readxl","plotly",
                        "bookdown", "rootSolve","shinyBS", "shinythemes",
                        "ggplot2")

  # sapply(list.of.packages, function(x) pacman::p_load(get(x)))

  new.packages <- list.of.packages[
    !( list.of.packages %in% installed.packages()[,"Package"] )
    ]
  if(length(new.packages)) install.packages(new.packages,
                                            repos = "https://cloud.r-project.org")

  lapply(list.of.packages, library, character.only = TRUE)
}

knitr::opts_knit$set(root.dir = here())

knitr::opts_chunk$set(echo = TRUE)

print_code <- html_format
show_summary_tables <- FALSE

equationIndex <- 0
sum_table_index <- 0

setwd(here())

colorize_f = function(x, color){
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}(%s)", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<font color='%s'>%s</font>", color, x)
  } else x
}
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(fig.width=12, fig.height=8)
#options(knitr.duplicate.label = "allow") # workaround for purl error
#knitr::purl("code/05_final_opa.Rmd", "code/shiny_app/all_analysis.R")




## ----sources, eval = TRUE, echo=print_code, message=FALSE, warning=FALSE------------------------------------------
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_sources <- function(){
###############################################################################
###############################################################################  

    #############
    ##### Setup
    #############  
    nsims_so <- 1e4
    rescale_so <- FALSE
    policy_estimate_so <- "a3_inc_a2_all_sim"
    run_sim_so <- FALSE
    main_run_so <- TRUE
    periods_so <- 50               #Total number of periods to forecast wages
    costs_temp_so <- 1
    main_pe_so <- 289.8


    #############
    ##### Data  
    #############
    # ATTENTION!
    # costs2_ea_in
    ex_rate_so <- 74               #Exchange Rate - Central Bank of Kenya 74 , 85
    ex_rate_2018_so        <- 101.30  # Exchange rate (KES per international $)
                                    # - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)
    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    gov_bonds_new_so <- 0.09
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    inflation_new_so <- 0.04
    tax_so <- 0.16575              #ADD INFO!

    # costs data
    df_costs_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "DtW Costs")
    # crosswalk data on region and country
    df_costs_cw_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "state_country")
    # data on number of treated children
    df_counts_so <- read_excel("rawdata/data/DtW Cost per Child Data.xlsx",
                           sheet = "DtW Treatment #s")
    # Prevalence data:
    # original study
    # Any infection on original study
    #prevalence_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" = 0.55,
    # "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    df_prevalence_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
    # prevalence_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    new_costs_so <- NULL
    country_sel_so <- list("india", "kenya", "nigeria", "vietnam")
    country_sel_pop_so <- c(
      "india" = 1.366417750 * 1e9,
      "kenya" = 5.257397 * 1e7,
      "nigeria" = 2.0096360 * 1e8,
      "vietnam" = 9.646211 * 1e7
    )
    #https://data.worldbank.org/indicator/SP.POP.TOTL
    # options: "a1_tax_sim","a1_x_tax_sim","a1_all_sim", "a1_x_all_sim", "a2_tax_sim",
    # "a2_all_sim", "a3_inc_a1_all_sim", "a3_inc_a1_all_x_sim", "a3_inc_a2_all_sim"




    #############
    ##### Research
    #############
    df_research_so <- read_csv("rawdata/research/research_params.csv")   
    lambda1_so <- c(3.49, 0)            #Hrs per week increase for men and women, table 3, row 1, cols 2 & 3
    lambda1_sd_so <- c(1.42, 1.36)      #table 3, row 2, cols 2 & 3
    lambda1_new_so <- c(79.51465)   # avg treatment effect from klps2-4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(76)  # ADD SOURCE
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, row 1 col 4
    lambda2_sd_so <- 7.8                # Table 3, row 2 col 4
    #This is are the parameters labeled eta in the doc
    prevalence_0_so <- 0.92 # 0.92 doi: https://doi.org/10.1111/j.1468-0262.2004.00481.x  location: table 2, row 6, column 1
    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B (Source data took the log, here the log is recovered)
    profits_se_so <- 1766          #Control group monthly self-employed profits -
                                   #Table 4, Panel C, Column 5, Row 1
                                   #FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 3, Panel B
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 3, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 3, Panel B
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)
                                                    #- see notes(0.1019575, -0.0010413), (0,0)
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct secondary schooling increase (from Joan)
                        0.0239963665555466, 0.027301406306074, 0.0234125454594173,
                       0.0279278879439199, 0.00647044449446303, 0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)
    delta_ed_par_so <- 1
    delta_ed_ext_par_so <- 1
    delta_ed_ext_so <- c(-0.0110126908021048,	0.0140448546741008,	-0.0034636291545585,  #Additional externality secondary schooling increase (from Joan)
                           0.0112940214439477,	0.0571608179771775,	-0.0560546793186931,
                           0.0558284756343451,	0.1546264843901160,	0.0055961489945619)
    delta_ed_ext_so <- cbind(delta_ed_ext_so, 1999:2007)
    include_ext_so <- TRUE
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    teach_sal_new_so <- (50000 * 12 / 49.773)
    teach_ben_new_so <- 0
                                  #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits -
                                  #news sources * 12 / ex_rate_2017_ppp_so
                                  # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                  # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    teach_sal_2017usdppp_so <- teach_sal_new_so * cpi_2017_so / cpi_2017_so # redundant, but for the sake of consistency

    n_students_so <- 45            #Average pupils per teacher	45
    #ATTENTION!
    years_of_treat_0_so <- 2.41      #Additional Years of Treatment - Table 1, Panel A
    unit_cost_local_so <- 43.66    #Deworm the World
    unit_cost_so <- 0.42           # Unit cost of deworming (in 2018 USD) - from Evidence Action
    #CALCULATIONS TO CONVERT ALL CURRENCY TO 2017 USD PPP
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018_so/ex_rate_2018_ppp_so
    unit_cost_2017usdppp_so <- unit_cost_ppp_so * cpi_2017_so / cpi_2018_so  # 0.8296927
    # Adjust for inflation: convert all costs to 2017 USD
    # Move this calculations into the body of the document (and outside of the sources chunk)
    costs_par_so <- 1
    costs_par_sd_so <- 0.1
    counts_par_so <- 1
    counts_par_sd_so <- 0.1






    #############
    ##### Guess work   
    #############
    # ATTENTION!
    # prevalence_r_in
    prevalence_r_so <- c("india" = 0.5665, "kenya" = 0.345, "nigeria" = 0.27, "vietnam" = 0.145)  #0.5665   0.5013121
    # based on https://docs.google.com/spreadsheets/d/1drKdU-kRjlRtwXq6nCqFC6gcoQ-eOaLfT9MWHSMZ0MA/edit?usp=sharing
    new_prevalence_r_so <- NULL
    years_of_treat_t_so <- 2.41      #Years of Treatment in new setting
    staff_time_so <- 0.3           #Added Deworming costs due to government staff time
    time_to_jm_so <- 10            #Time from initial period until individual join the labor force


    # Fix teach_sal_so       
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
###############################################################################
###############################################################################    
}
invisible( list2env(chunk_sources(),.GlobalEnv) )
#############
##### Notes:
#############
# on growth_rate_so: (http://data.worldbank.org/indicator/NY.GDP.PCAP.KD/), see calculation
# on "Kenya GDP per capita" tab. In W@W this equals 1.52%. ISSUE: This growth number should
# be updated to be 2002-2014, I think.
#
# on coef_exp_so: 1998/1999 Kenyan labor force survey; regression of earnings on age, age^2,
# female dummy, indicators for attained primary/secondary/beyond, and province dummies.
# Estimate used in W@W: (0.1019575, -0.0010413). ISSUE: For now assume no further life cycle
# adjustment beyond KLPS-3 (likely a conservative assumption).
#
# coverage_so: Overall Saturation (0.511) / 0.75 - not reported in table, average of T & C




## ----final-pe, echo=print_code------------------------------------------------------------------------------------
# - inputs: total per capita benefits, total per capita costs
# - outputs: Net Present Value (NPV)
chunk_final_pe <- function(){
###############################################################################
###############################################################################  

    NPV_pe_f <- function(benefits_var = 1, costs_var = 1){
        benefits_var - costs_var
    }

###############################################################################
###############################################################################  
    return(list("NPV_pe_f" = NPV_pe_f))
}
# Excecute the previos function and load the listed objects in to the current R
# session (global environment)
invisible( list2env(chunk_final_pe(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:


## ----benefits, echo=print_code------------------------------------------------------------------------------------
# - inputs: stream earnings, discounting rate, number of periods
# - outputs: function that computes the present value of benefits
chunk_benefits <- function(){
###############################################################################
###############################################################################  

  pv_benef_f <- function(
    earnings_var = earnings_in,
    interest_r_var = interest_in,
    periods_var = periods_so
  ) {
      index_t <- 0:periods_var
      res1 <- sum( ( 1 / (1 + interest_r_var) )^index_t * earnings_var )
      return(res1)   
    }

###############################################################################
###############################################################################  
    return(list("pv_benef_f" = pv_benef_f))
}
invisible( list2env(chunk_benefits(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:


## ----interest-rate, echo=print_code-------------------------------------------------------------------------------
# - inputs: nominal interest rate, inflation rate
# - outputs: real interest rate. exact and approximate formula
chunk_interest <- function(){
###############################################################################
###############################################################################  

    interest_f <- function(gov_bonds_var = gov_bonds_so ,
                           inflation_var = inflation_so) {  
        interest_exct_in <- (1 + gov_bonds_var) / (1 + inflation_var) - 1
        interest_in = gov_bonds_var - inflation_var
        return(list("interest_in" = interest_in,
                    "interest_exct_in" = interest_exct_in))
    }

###############################################################################
###############################################################################  
    return(list("interest_f" = interest_f))
}

invisible( list2env(chunk_interest(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:

interest_in <- as.numeric(
  interest_f(gov_bonds_var = gov_bonds_so,
             inflation_var = inflation_so)$interest_in
  )

interest_new_in <- as.numeric(
  interest_f(gov_bonds_var = gov_bonds_new_so,
             inflation_var = inflation_new_so)$interest_in  
  )





## ----earnings1, echo=print_code-----------------------------------------------------------------------------------
# - inputs: earnings wihtout treatment (wage_in), direct treatment eff
# (lambda1_so), indirect treatment eff (lambda2_so), saturation and coverage (coverage_so)
# - outputs: earnings (no name specified)
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings_app1_f <- function(wage_var = wage_t_in,
                          lambda1_var = lambda1_so,
                          lambda2_var = lambda2_so,
                          saturation_var = saturation,
                          coverage_var = coverage_so) {  
        res1 <- wage_var * ( lambda1_var + saturation_var *
                               lambda2_var / coverage_var )
        return(res1)
    }

###############################################################################
###############################################################################  
    return(list("earnings_app1_f" = earnings_app1_f))
}

invisible( list2env(chunk_earnings1(),.GlobalEnv) )


## ----wage_t, echo=print_code--------------------------------------------------------------------------------------
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_in) and function (wage_0_f),
# Wage trajectory: value (wage_t_in) and function (wage_t_f).
chunk_wages <- function(){
################################################################################
################################################################################  
    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision

  wage_0_f <- function(wage_ag_var,
                          wage_ww_var,
                          profits_se_var,
                          hours_se_cond_var,
                          hours_ag_var,
                          hours_ww_var,
                          hours_se_var,
                          ex_rate_var){
        experience_aux <- 0:periods_so - time_to_jm_so
        wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
        wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
        alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) /
          sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
        res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
        return(res1)
    }

  wage_t_f <- function(wage_0_var,
                          growth_rate_var,
                          coef_exp1_var,
                          coef_exp2_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        res1 <- 52 * wage_0_var * ( ( 1 + growth_rate_var )^experience_aux ) *
          ( 1 + coef_exp1_var * experience_aux + coef_exp2_var *
              (experience_aux^2) ) * ifelse(0:periods_so >= time_to_jm_so, 1, 0)
        return(res1)
    }



################################################################################
################################################################################
    return(list("wage_0_f" = wage_0_f,
                "wage_t_f" = wage_t_f))
}

invisible( list2env(chunk_wages(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
wage_0_in <- wage_0_f(wage_ag_var = wage_ag_so,  
                      wage_ww_var = wage_ww_so,
                      profits_se_var = profits_se_so,
                      hours_se_cond_var = hours_se_cond_so,  
                      hours_ag_var = hours_ag_so,
                      hours_ww_var = hours_ww_so,
                      hours_se_var = hours_se_so,
                      ex_rate_var = ex_rate_so)  

#close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
#but I suspect diff due to computational precision
wage_t_in <- wage_t_f(wage_0_var = wage_0_in,
                      growth_rate_var = growth_rate_so,
                      coef_exp1_var = coef_exp_so[1],
                      coef_exp2_var = coef_exp_so[2])



## ----lambdas, echo=print_code-------------------------------------------------------------------------------------
# - inputs: direct (lambda1_so), and indirect (lambda2_so) treatment effects by gender
# - outputs: simple average of direct and indirect treatment eff.
chunk_lambdas<- function(){
###############################################################################
###############################################################################    

    lambda1_in_f <- function(lambda1_var = lambda1_so) {
        rep(0.5 * lambda1_var[1] + 0.5 *lambda1_var[2], 2)
    }
    lambda2_in_f <- function(lambda2_var = lambda2_so){
        rep(lambda2_var, 2)
    }

##############################################################################
###############################################################################  
    return(list("lambda1_in_f" = lambda1_in_f,
                "lambda2_in_f" = lambda2_in_f ) )
}
invisible( list2env(chunk_lambdas(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_in <- lambda1_in_f()
lambda2_in <- lambda2_in_f()


## ----coverage-and-saturation, echo = print_code-------------------------------------------------------------------
# - inputs: coverage (coverage_so), take-up with full subsidy (q_full_so), and
# take-up with no subsidy (q_zero_so)
# - outputs: saturation (saturation_in)
chunk_coverage <- function(){
###############################################################################
###############################################################################  

  saturation_in_f <- function(coverage_var = coverage_so,
                              q_full_var = q_full_so,
                              q_zero_var = q_zero_so){
      saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
      return(list("saturation_in" = saturation_in))
    }

###############################################################################
###############################################################################  
    return(list("saturation_in_f" = saturation_in_f))   
}
invisible( list2env(chunk_coverage(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
saturation_in <- saturation_in_f()$saturation_in

# Computing values for inline text:

# pv_benef
# ├──── earnings_app1_f
# |      ├──── delta_ed_final_f
# |      ├──── saturation_in_f
# |      └──── wage_t_f()
# |      |      └──── wage_0_f()
# |      |            
# |      ├──── lambda1_in_f()
# |      └──── lambda2_in_f()
# └──── interest_f()

earnings_no_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_in[1],
  saturation_var = saturation_in,
  lambda2_var = 0,
  coverage_var = coverage_so
)

earnings_yes_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_in[1],
  saturation_var = saturation_in,
  lambda2_var = lambda2_in[1],
  coverage_var = coverage_so
)

pv_benef_no_ext_in <- pv_benef_f(
  earnings_var = earnings_no_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)

pv_benef_yes_ext_in <- pv_benef_f(
  earnings_var = earnings_yes_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)




## ----cost2, echo = print_code-------------------------------------------------------------------------------------
# - inputs: periods (periods_so), additional education (delta_ed_final_in),
#  discount rate (interest) (varies by approach), cost per student
#  (cost_per_student_in), cost per treatment (s2_in), take-up with treatment
#  (q2_in)
# - outputs: present value of all costs (pv_costs_f)
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

  pv_costs_f <- function(
    periods_var = periods_so,
    delta_ed_var = delta_ed_final_in,
    interest_r_var = NULL,
    cost_of_schooling_var = cost_per_student_in,
    s1_var = 0,
    q1_var = 0,
    s2_var = s2_in,
    q2_var = q2_in) {
        index_t <- 0:periods_var
        # Effects over 9 years of education (post treatment)
        delta_ed_s <- c(0, delta_ed_var, rep(0,41))
        (s2_var * q2_var  - s1_var * q1_var) +
          sum( ( 1 / (1 + interest_r_var) )^index_t *
                 delta_ed_s * cost_of_schooling_var)
    }

###############################################################################
###############################################################################  
    return(list("pv_costs_f" = pv_costs_f))    # Try to return only functions
}
invisible( list2env(chunk_cost2(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:  


## ----unit_costs2, echo = print_code-------------------------------------------------------------------------------
# - inputs: unit costs in local currency (unit_cost_local_so), exchange rate
#  (ex_rate_so), years of treatment (years_of_treat_0_so)
# - outputs: unit costs of treatment (s2_f)
chunk_unit_costs2 <- function(){
###############################################################################
###############################################################################  

    s2_f <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     years_of_treat_var = years_of_treat_0_so) {
      ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    }

###############################################################################
###############################################################################  
    return(list("s2_f" = s2_f) )
}
invisible( list2env(chunk_unit_costs2(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
s2_in <- s2_f()


## ----ed-costs, echo = print_code----------------------------------------------------------------------------------
# - inputs: teacher salary (teach_sal_so) and benefits (teach_ben_so), number
# of students (n_students_so), include externalities (include_ext_so), extra ed
# without ext (delta_ed_so), and extra ed due to ext (delta_ed_ext_so)
# - outputs: cost per student (cost_per_student_f), and total additional
# education (delta_ed_final_f)
chunk_edcosts <- function(){
###############################################################################
###############################################################################    

    cost_per_student_f <- function(teach_sal_var = teach_sal_so,
                                    teach_ben_var = teach_ben_so,
                                    n_students_var = n_students_so) {
        (teach_sal_var + teach_ben_var) / n_students_var
    }

    delta_ed_final_f <- function(include_ext_var = include_ext_so,
                                 delta_ed_var = delta_ed_so,
                                 delta_ed_ext_var = delta_ed_ext_so){
        if (include_ext_var == TRUE){
            delta_ed_final_in <-  delta_ed_ext_var[,1] + delta_ed_var[,1]
        }else{
            delta_ed_final_in <- delta_ed_var[,1]
        }
        return(delta_ed_final_in)
    }

###############################################################################
###############################################################################  
    return(list("cost_per_student_f" = cost_per_student_f,
                "delta_ed_final_f" = delta_ed_final_f))
}
invisible( list2env(chunk_edcosts(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
cost_per_student_in <- cost_per_student_f()
delta_ed_final_no_ext_in <- delta_ed_final_f(include_ext_var = FALSE)
delta_ed_final_yes_ext_in <- delta_ed_final_f(include_ext_var = TRUE)

# Computing values for inline text:

# pv_costs_f
#  ├──── delta_ed_final_f
#  ├──── interest_f
#  └──── cost_per_student_f
#  |      └──── x
#  ├──── s2_f
#  └──── lambda2_in_f

pv_cost_no_ext_in <- pv_costs_f(
      periods_var = periods_so,
      delta_ed_var = delta_ed_final_no_ext_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_so
    )

pv_cost_yes_ext_in <- pv_costs_f(
      periods_var = periods_so,
      delta_ed_var = delta_ed_final_yes_ext_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_so
    )






## ----delta-earnings, eval=TRUE, echo = print_code-----------------------------------------------------------------
# - inputs: index for time (t_var), pooled treatment effect (lambda1_new_so[1])
# - outputs: effect on lifetime earnings (earnings_app2_f)
chunk_new_earnings <- function(){
###############################################################################
###############################################################################  

    earnings_app2_f <- function(t_var = 1,
                            lambda1k1_var = lambda1_new_so[1]) {
        1*(10 <= t_var & t_var < 25) * lambda1k1_var
    }

###############################################################################
###############################################################################             
    return(list("earnings_app2_f" = earnings_app2_f))
}

invisible( list2env(chunk_new_earnings(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
earnings_no_ext_new_in <- earnings_app2_f(t_var = 0:50,
                                      lambda1k1_var = lambda1_new_so[1])




## ----unit_costs2_new, echo = print_code---------------------------------------------------------------------------
# - inputs: unit costs (unit_cost_local_so), exchange rate (ex_rate_so),
#  new interest rate (interest_new_in)
# - outputs: total unit costs (s2_new_f)
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

  s2_new_f <- function(
    unit_cost_local_var = unit_cost_local_so,
    ex_rate_var = ex_rate_so,
    interest_var = interest_new_in,
    year_of_treat_var = years_of_treat_t_so) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      periods_temp <- floor(year_of_treat_var)
      part_of_last_year_temp <- round(year_of_treat_var - periods_temp, 1)
      sum(
        ( unit_cost * (1 + interest_var)^(-(0:periods_temp)) ) *
            c(rep(1,periods_temp), part_of_last_year_temp)
        )
    }

###############################################################################
###############################################################################  
    return(list("s2_new_f" = s2_new_f) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
# New costs are all in dollars so, will compute them using ex rate of 1.
s2_new_in <- s2_new_f(
  interest_var = interest_new_in,
  unit_cost_local_var = unit_cost_2017usdppp_so,
  ex_rate_var = 1,
  year_of_treat_var = years_of_treat_t_so
)
q2_in <- q_full_so






## ----lambdas_eff, echo = print_code-------------------------------------------------------------------------------
# - inputs: previously estimated treatment effect (lambda1_in_f), prevalence
# rates in the original setting (prevalence_0_so), prevalence in the new setting
# (prevalence_r_so), countries included in the analysis (country_sel_so)
# and their population (country_sel_pop_so), or single input of new prevalence
# (new_prevalence_r_so)
# - outputs: effective treatment effect (lambda_eff_f)
chunk_lambdas_eff<- function(){
###############################################################################
###############################################################################    

    lambda_eff_f <- function(lambda1_var = lambda1_in_f(),
                           prevalence_0_var = prevalence_0_so,
                           prevalence_r_var = prevalence_r_so,
                           country_sel_var = country_sel_so,
                           country_sel_pop_var = country_sel_pop_so,
                           other_prevl_r_var = new_prevalence_r_so){
      temp_sel <- as.character(country_sel_var)  
      # if a positive number of countries is selected
      if (is.null(other_prevl_r_var)) {
        temp_weights <- country_sel_pop_var[temp_sel] /
          sum(country_sel_pop_var[temp_sel])
        prevalence_r_final <- sum( prevalence_r_var[temp_sel] * temp_weights )
      } else {
        prevalence_r_final <- other_prevl_r_var  
      }
      lambda1_eff_temp <- lambda1_var / prevalence_0_var
      lambda1_eff_in <- lambda1_eff_temp * prevalence_r_final
      return(  
        list("lambda1_eff_in" = lambda1_eff_in,
             "prevalence_r_final_in" = prevalence_r_final)
              )
    }  

###############################################################################
###############################################################################  
    return( list("lambda_eff_f" = lambda_eff_f) )
}
invisible( list2env(chunk_lambdas_eff(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_r_in <- lambda_eff_f()$lambda1_eff_in
prevalence_r_in <- lambda_eff_f()$prevalence_r_final_in


## ----lambdas_t, echo = print_code---------------------------------------------------------------------------------
# - inputs: treatment effect (lambda1_in_f), length of treatment in original
# study (years_of_treat_0_so), length of treatment in new setting (years_of_treat_t_so)
# - outputs: per year treatment effect (lambda1_t1) and total treatment effect
# (lambda1_t)

chunk_lambdas_t<- function(){
###############################################################################
###############################################################################    

    lambda_t_f <- function(lambda1_var = lambda1_in_f(),
                           years_of_treat_0_var = years_of_treat_0_so,
                           years_of_treat_t_var = years_of_treat_0_so){
          lambda1_t1 <- lambda1_var / years_of_treat_0_var
          if (years_of_treat_t_var<=6){
            lambda1_t <- years_of_treat_t_var * lambda1_t1
          } else if  (years_of_treat_t_var>6) {
            lambda1_t <- 6 * lambda1_t1
          }
          return(
            list(
              "lambda1_t1" = lambda1_t1,
              "lambda1_t" = lambda1_t)
            )
    }  

###############################################################################
###############################################################################  
    return( list("lambda_t_f" = lambda_t_f) )
}
invisible( list2env(chunk_lambdas_t(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:

# # earnings_app1_f
#  ├──── delta_ed_final_f
#  ├──── interest_f
#  └──── lambda_eff_f
#  |      └──── lambda_t_f
#  |            └──── lambda_in_f
#  ├──── saturation_f
#  └──── wage_t_f
#         └──── wage_0_f

lambda1_t_in <- lambda_eff_f(
  lambda1_var = lambda_t_f(
    lambda1_var = lambda1_in_f(),
    years_of_treat_0_var =  years_of_treat_0_so,
    years_of_treat_t_var =  years_of_treat_t_so
  )$lambda1_t,
  prevalence_0_var = prevalence_0_so,
  country_sel_var = list("india", "kenya", "nigeria", "vietnam"),
  other_prevl_r_var = NULL
)$lambda1_eff_in


app3_earnings_no_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_t_in[1],
  saturation_var = saturation_in,
  lambda2_var = 0,
  coverage_var = coverage_so
)

app3_earnings_yes_ext_in <- earnings_app1_f(
  wage_var = wage_t_in,
  lambda1_var = lambda1_t_in[1],
  saturation_var = saturation_in,
  lambda2_var = lambda2_in[1],
  coverage_var = coverage_so
)

app3_pv_benef_no_ext_in <- pv_benef_f(
  earnings_var = app3_earnings_no_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)

app3_pv_benef_yes_ext_in <- pv_benef_f(
  earnings_var = app3_earnings_yes_ext_in,
  interest_r_var = interest_in,
  periods_var = periods_so
)


lambda1_t_new_in <- lambda_eff_f(
  lambda1_var = lambda_t_f(
    lambda1_var = lambda1_new_so[1],
    years_of_treat_0_var =  years_of_treat_0_so,
    years_of_treat_t_var =  years_of_treat_t_so
  )$lambda1_t,
  prevalence_0_var = prevalence_0_so,
  country_sel_var = list("india", "kenya", "nigeria", "vietnam"),
  other_prevl_r_var = NULL
)$lambda1_eff_in


earnings_no_ext_new_in<- earnings_app2_f(t_var = 0:50,
                                      lambda1k1_var = lambda1_t_new_in)

app3_pv_benef_all_new_in <- pv_benef_f(earnings_var = earnings_no_ext_new_in,
                                interest_r_var = interest_new_in,
                                periods_var = periods_so)


## ----eq_3, echo=print_code, eval=TRUE-----------------------------------------------------------------------------
# - inputs: cost data by payer type at the contry/province level by year (df_costs_so)
#  crosswalk between country/state and region (df_costs_cw_so), treatment counts
#  by country/province and year (df_counts_so); staff time adjusment factor
#  (staff_time_so),
# - outputs: country level cost and population data (costs1_p1_f) and country
#  weights and per capita costs (costs1_p2_f)
#
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
  # clean and aggreagate data at country level
  costs1_p1_f <- function(
    df_costs_var = df_costs_so,
    df_costs_cw_var = df_costs_cw_so,
    df_counts_var = df_counts_so) {
    ## Counts
    # Data cleanning:
    # Add country variable
    # anoying message: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override

    df_counts_temp <- df_costs_cw_var %>%
      right_join(df_counts_var, by = "Country/State") %>%
      mutate(Country = tolower(Country))
    # keep only last year on record
    suppressMessages(            
        df_counts_last <- df_counts_temp %>%
          group_by(Country) %>%
          summarise("last_year" = max(Year)) %>%
          right_join(df_counts_temp, by = "Country") %>%
          filter(Year == last_year)          
    )
    # compute counts as the sum with-in country-year of treatments
    suppressMessages(            
        c_counts <- df_counts_last %>%
          group_by(Country, Year) %>%
          summarise("total" = sum(`# dewormed`))
    )

    ## Costs
    # Data cleaning:
    # Add country variable
    df_costs_temp <- df_costs_cw_var %>%
      right_join(df_costs_var, by = "Country/State") %>%
      select(-Country.y) %>% rename(Country = Country.x) %>%
      mutate(Country = tolower(Country))
    # values for last year with cost information
    suppressMessages(            
    df_costs_last <- df_costs_temp %>%
      group_by(Country) %>%
      summarise("last_year" = max(Year)) %>%
      right_join(df_costs_temp, by = "Country") %>%
      filter(Year == last_year)    
      )      
     # summing across payers and regions (last equation)
    suppressMessages(            
    costs_by_payer <- df_costs_last %>%
      filter(Payer != "Total") %>%
      group_by(Country, Payer) %>%
      summarise("costs_by_payer" =
                  sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE))
    )    
    #sum across payers
    suppressMessages(            
    country_cost <- costs_by_payer %>%
      group_by(Country) %>%
      summarise("costs_by_country" =
                  sum(costs_by_payer) )  
    # Compute the per capita cost for each country (c_i and w_i)
    )
    costs_data_in <- country_cost %>%
       left_join(c_counts, by = "Country")

    return( costs_data_in )
  }

  # Compute weights and per capta costs
  costs1_p2_f <- function(country_total_var = costs_data_in$total,
                         country_cost_var = costs_data_in$costs_by_country,
                         staff_time_var = staff_time_so,
                         country_name_var = costs_data_in$Country,
                         select_var = list("india", "kenya", "nigeria",
                                           "vietnam"),
                         other_costs_var = NULL) {
      # select countries
      country_total_var_temp <- country_total_var[country_name_var %in% select_var]
      country_cost_var_temp <- country_cost_var[country_name_var %in% select_var]
      # create country weight
      c_weights <- country_total_var_temp / sum(country_total_var_temp)
      # create country per capita costs, adjusted by staff time
      per_cap <- country_cost_var_temp * (1 + staff_time_var) /
        country_total_var_temp
      # replace contry costs with new one if there is a new country
      # (only count that new country)
      # (the weighthed sum of this scalar will just be the same number)
      if (!is.null(other_costs_var)) {
      # if (FALSE) {  
        per_cap <- other_costs_var * (1 + staff_time_var)
      }
      return( sum(c_weights * per_cap) )
    }
###############################################################################
###############################################################################  
    return( list("costs1_p1_f" = costs1_p1_f,
                 "costs1_p2_f" = costs1_p2_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
costs_data_in <- costs1_p1_f()
costs1_p2_in <- costs1_p2_f(select_var = list("india", "kenya", "nigeria",
                                              "vietnam"))




## ----mc-setup, eval=TRUE, echo = print_code-----------------------------------------------------------------------
# This function takes as inputs means and standard deviations of source
# parameters and simualte draws of each source. When the source is a scalar,
# it generates a draw from a noromal dist (mean, sd). When it is a "small"
# (less than 4 elements) vector, generates independent multivariate normals.


#begin by cleaning up the cost data once
costs_data_in <- costs1_p1_f(df_costs_var = df_costs_so,
                          df_costs_cw_var = df_costs_cw_so,
                          df_counts_var = df_counts_so)


# Data: source comes from a standard data source. Government statistic or other
# publicly available statistic
# Research: any sources that requieres some type of investigation to obtain
# Guesswork: no clear source available

sim_data1_f <- function(nsims_var2 = 1e2,                   # "Setup" vars
                      main_run_var2,
                      periods_var2,
                      costs_data_var2 = costs_data_in,
                      run_sim_var2,
                      countries_var2,

                      ex_rate_var2,                  # "Data" vars
                      ex_rate_sd_var2,
                      growth_rate_var2,
                      growth_rate_sd_var2,
                      gov_bonds_var2,
                      gov_bonds_sd_var2,
                      gov_bonds_new_var2,                                                              
                      gov_bonds_new_sd_var2,                                                          
                      inflation_var2,
                      inflation_sd_var2,
                      inflation_new_var2,                          
                      inflation_new_sd_var2,                      
                      tax_var2,
                      tax_sd_var2,

                      lambda1_var2,                  # "Research" vars
                      lambda1_sd_var2,
                      lambda1_new_var2,
                      lambda1_new_sd_var2,
                      lambda2_var2,
                      lambda2_sd_var2,
                      wage_ag_var2,                 
                      wage_ag_sd_var2,
                      wage_ww_var2,
                      wage_ww_sd_var2,
                      profits_se_var2,
                      profits_se_sd_var2,
                      hours_se_cond_var2,
                      hours_se_cond_sd_var2,
                      hours_ag_var2,
                      hours_ag_sd_var2,
                      hours_ww_var2,
                      hours_ww_sd_var2,
                      hours_se_var2,
                      hours_se_sd_var2,
                      coef_exp_var2,         # sd for coef_exp is hard coded
                      prevalence_0_var2,
                      prevalence_0_sd_var2,
                      prevalence_r_var2,
                      prevalence_r_sd_var2,
                      new_prevl_r_var2,       # substitudes the prev_r above??
                      new_prevl_r_sd_var2,
                      coverage_var2,
                      coverage_sd_var2,
                      q_full_var2,
                      q_full_sd_var2,
                      q_zero_var2,
                      q_zero_sd_var2,
                      delta_ed_var2,
                      delta_ed_sd_var2,
                      delta_ed_ext_var2,
                      delta_ed_ext_sd_var2,
                      teach_sal_var2,
                      teach_sal_sd_var2,
                      teach_ben_var2,
                      teach_ben_sd_var2,
                      teach_sal_new_var2,
                      teach_sal_new_sd_var2,
                      teach_ben_new_var2,
                      teach_ben_new_sd_var2,
                      n_students_var2,
                      n_students_sd_var2,
                      years_of_treat_0_var2,
                      years_of_treat_0_sd_var2,
                      years_of_treat_t_var2,
                      years_of_treat_t_sd_var2,
                      unit_cost_local_var2,
                      unit_cost_local_sd_var2,
                      unit_cost_local_new_var2,
                      unit_cost_local_new_sd_var2,
                      costs_par_var2,
                      costs_par_sd_var2,
                      counts_par_var2,
                      counts_par_sd_var2,
                      staff_time_var2,      # Guesswork
                      staff_time_sd_var2,
                      new_costs_var2,
                      new_costs_sd_var2
                      ) {
    start_time <- Sys.time()
    ################
    ###### Draws
    ################
    set.seed(142857)
    #Default dist: normal, default sd: 0.1* mean
    #
    # Sources are separated into: data, research and guess work
    ## Data
    gov_bonds_sim <-        rnorm(n = nsims_var2, mean = gov_bonds_var2,
                                  sd = gov_bonds_sd_var2)
    inflation_sim <-        rnorm(nsims_var2, inflation_var2,
                                  inflation_sd_var2)
    gov_bonds_new_sim <-    rnorm(n = nsims_var2, mean = gov_bonds_new_var2,
                                  sd = gov_bonds_new_sd_var2)
    inflation_new_sim <-    rnorm(nsims_var2, inflation_new_var2,
                                  inflation_new_sd_var2)                  
    growth_rate_sim <-      rnorm(nsims_var2, growth_rate_var2, growth_rate_sd_var2)
    ex_rate_sim <-          rnorm(nsims_var2, ex_rate_var2, ex_rate_sd_var2)
    tax_sim <-              rnorm(nsims_var2, tax_var2, tax_sd_var2)

    ## Research
    aux1 <-0.1 * c(lambda1_var2[1], 0.01)
    # Each list is a pair mean, sd.
    aux2 <-  lapply(1:2, function(x) c(lambda1_var2[x], c(1.42, 1.36)[x] ) )
    lambda1_sim <- sapply(aux2,
                          function(x)  rnorm(nsims_var2, mean = x[1], sd = x[2]) )
    lambda2_sim <-          rnorm(nsims_var2, lambda2_var2,  lambda2_sd_var2)
    # New lambda here
    lambda1_new_sim <- rnorm(nsims_var2, lambda1_new_var2,  lambda1_new_sd_var2)


    wage_ag_sim <-          rnorm(nsims_var2, wage_ag_var2, wage_ag_sd_var2)
    wage_ww_sim <-          rnorm(nsims_var2, wage_ww_var2, wage_ww_sd_var2)
    profits_se_sim <-       rnorm(nsims_var2, profits_se_var2, profits_se_sd_var2)
    hours_se_cond_sim <-    rnorm(nsims_var2, hours_se_cond_var2,
                                  hours_se_cond_sd_var2)
    hours_ag_sim <-         rnorm(nsims_var2, hours_ag_var2, hours_ag_sd_var2)
    hours_ww_sim <-         rnorm(nsims_var2, hours_ww_var2, hours_ww_sd_var2)
    hours_se_sim <-         rnorm(nsims_var2, hours_se_var2, hours_se_sd_var2)
    coverage_sim <-         rnorm(nsims_var2, coverage_var2, coverage_sd_var2)

    unit_cost_local_sim <-  rnorm(nsims_var2, unit_cost_local_var2,
                                  unit_cost_local_sd_var2)
    unit_cost_local_new_sim <-  rnorm(nsims_var2, unit_cost_local_new_var2,
                              unit_cost_local_new_sd_var2)

    years_of_treat_0_sim <-   rnorm(nsims_var2, years_of_treat_0_var2,
                                  years_of_treat_0_sd_var2)
    years_of_treat_t_sim <-   rnorm(nsims_var2, years_of_treat_t_var2,
                                  years_of_treat_t_sd_var2)

    q_full_sim <-           rnorm(nsims_var2, q_full_var2, q_full_sd_var2)
    q_zero_sim <-           rnorm(nsims_var2, q_zero_var2, q_zero_sd_var2)

    # Prevalence here TO DO: draw from a beta instead of "truncated" normal
    prevalence_0_sim <- rnorm(nsims_var2, prevalence_0_var2, prevalence_0_sd_var2)
    prevalence_0_sim <- ifelse(
      prevalence_0_sim > 1,
      yes = 1,
      no = ifelse(prevalence_0_sim < 0, yes = 0, no = prevalence_0_sim)
    )

    aux4 <- lapply(countries_var2, #will have trouble when selecting no countries
                   function(x) c(prevalence_r_so[x],
                                 prevalence_r_so[x]) )

    # first draw samples of prevalence for each country
    prevalence_r_sim <- sapply(aux4,
                                function(x)
                                  rnorm(
                                    nsims_var2,
                                    mean = x[1] * prevalence_r_var2,
                                    sd = x[2] * prevalence_r_sd_var2
                                  ))
    prevalence_r_sim <- ifelse(
      prevalence_r_sim > 1,
      yes = 1,
      no = ifelse(prevalence_r_sim < 0, yes = 0, no = prevalence_r_sim)
    )
    colnames(prevalence_r_sim) <- as.character(countries_var2)  

    # if there is a new entry of prevalence, draw from it. If there is not
    # then leave as null
    if (!is.null(new_prevl_r_var2)){
          new_prevl_r_sim <- rnorm(nsims_var2, new_prevl_r_var2, new_prevl_r_sd_var2)
          new_prevl_r_sim <- ifelse(
            new_prevl_r_sim > 1,
            yes = 1,
            no = ifelse(new_prevl_r_sim < 0, 0, new_prevl_r_sim)
          )
    } else if (is.null(new_prevl_r_var2)){
          new_prevl_r_sim <- NULL
    }
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims_var2, mean = x[1],
                                                    sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims_var2, teach_sal_var2, teach_sal_sd_var2)
    teach_ben_sim <-    rnorm(nsims_var2, teach_ben_var2, teach_ben_sd_var2)

    teach_sal_new_sim <-    rnorm(nsims_var2, teach_sal_new_var2,
                                  teach_sal_new_sd_var2)
    teach_ben_new_sim <-    rnorm(nsims_var2, teach_ben_new_var2,
                                  teach_ben_new_sd_var2)

    n_students_sim <-   rnorm(nsims_var2, n_students_var2, n_students_sd_var2)
    # TO DO: modify to have a scalar multlying the series, and have that
    # scalar being N(1,0.1)
    delta_ed_sim <- sapply(delta_ed_so[,1],
                           function(x) rnorm(
                             nsims_var2,
                             mean = x * delta_ed_var2,
                             sd = delta_ed_sd_var2 * sd(delta_ed_so[, 1])) )
    colnames(delta_ed_sim) <- 1999:2007
    # modify to have a scalar multlying the series, and have that scalar
    # being N(1,0.1)
    delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1],
                               function(x)  {
                                 rnorm(
                                   nsims_var2,
                                   mean = x * delta_ed_ext_var2,
                                   sd =  sd(delta_ed_ext_so[, 1]) *
                                     delta_ed_ext_sd_var2
                                 )
                                 }
                               )
    colnames(delta_ed_ext_sim) <- 1999:2007

    counts_in <- costs_data_var2$total
    costs_no_staff_in <- costs_data_var2$costs_by_country

    # drawing samples form counts
    costs1_counts_sim <- sapply(counts_in,
                                function(x)  rnorm(nsims_var2,
                                                   mean = x * counts_par_var2,  
                                                   sd = x * counts_par_sd_var2)
                                )
    # drawing samples from costs
    costs1_all_costs_sim <- sapply(costs_no_staff_in,
                                function(x)  rnorm(nsims_var2,
                                                   mean = x * costs_par_var2,  
                                                   sd = x * costs_par_sd_var2)
                                )

    #computing unit cost for each simulation draw
    costs1_df_sim <- NULL

    #building "nsims_var2" simulated data sets (corresponding to costs_data_in)
    for (aux1_i in 1:nsims_var2){
      costs1_df_sim[[aux1_i]] <- data.frame(
        "Country" = costs_data_var2$Country,
        "total" = costs1_counts_sim[aux1_i,],
        "costs_by_country" = costs1_all_costs_sim[aux1_i,]
        )
    }
    temp_cost_sim <- rnorm(nsims_var2,
                           mean = new_costs_var2,
                           sd = new_costs_sd_var2)
    ## Guess work
    # drawing samples from staff time
    staff_time_sim <- rnorm(nsims_var2, staff_time_var2, staff_time_sd_var2)      
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #periods until individual join the labor force

    ######    
    ######    

    ################
    ###### Runs    
    ################

    #Vectors to store the results of each simulation
    a1_tax_sim           <- rep(NA, nsims_var2) #a1_tax_pe
    a1_x_tax_sim         <- rep(NA, nsims_var2) #a1_x_tax_pe
    a1_all_sim           <- rep(NA, nsims_var2) #a1_all_pe
    a1_x_all_sim         <- rep(NA, nsims_var2) #a1_x_all_pe
    a2_tax_sim           <- rep(NA, nsims_var2) #a2_tax
    a2_all_sim           <- rep(NA, nsims_var2) #a2_all
    a3_inc_a1_all_sim    <- rep(NA, nsims_var2) #a3_inc_a1_all
    a3_inc_a1_all_x_sim  <- rep(NA, nsims_var2) #a3_inc_a1_all_x
    a3_inc_a2_all_sim    <- rep(NA, nsims_var2) #a3_inc_a2_all_mpe


    for (i in 1:nsims_var2) {
    # one_run_f, for the most part, does not include standard deviations   
      invisible( list2env(
        one_run_f(main_run_var1 = FALSE,              # HERE I NEED TO PLUG costs1_costs_sim
                run_sim_var1 = TRUE,
                wage_ag_var1 = wage_ag_sim[i],
                wage_ww_var1 = wage_ww_sim[i],
                profits_se_var1 = profits_se_sim[i],
                hours_se_cond_var1 = hours_se_cond_sim[i],
                hours_ag_var1 = hours_ag_sim[i],
                hours_ww_var1 = hours_ww_sim[i],
                hours_se_var1 = hours_se_sim[i],
                ex_rate_var1 = ex_rate_sim[i],
                growth_rate_var1 = growth_rate_sim[i],
                coef_exp_var1 = coef_exp_sim[i, 1], coef_exp2_var1 = coef_exp_sim[i,2],
                lambda1_var1 = lambda1_in_f(lambda1_var = lambda1_sim[i,]),
                prevalence_0_var1 = prevalence_0_sim[i],
                prevalence_r_var1 = prevalence_r_sim[i, ],
                new_prevl_r_var1 = new_prevl_r_sim[i],
                lambda2_var1 = lambda2_sim[i],
                coverage_var1 = coverage_sim[i],
                q_full_var1 = q_full_sim[i],
                q_zero_var1 = q_zero_sim[i],
                lambda1_new_var1 = lambda1_new_sim[i],
                gov_bonds_var1 = gov_bonds_sim[i],
                inflation_var1 = inflation_sim[i],
                gov_bonds_new_var1 = gov_bonds_new_sim[i],
                inflation_new_var1 = inflation_new_sim[i],
                delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                teach_sal_var1 = teach_sal_sim[i],
                teach_ben_var1 = teach_ben_sim[i],
                teach_sal_new_var1 = teach_sal_new_sim[i],
                teach_ben_new_var1 = teach_ben_new_sim[i],
                n_students_var1 = n_students_sim[i],
                unit_cost_local_var1 = unit_cost_local_sim[i],
                unit_cost_local_new_var1 = unit_cost_local_new_sim[i],
                years_of_treat_0_var1 = years_of_treat_0_sim[i],
                years_of_treat_t_var1 = years_of_treat_t_sim[i],
                tax_var1 = tax_sim[i],
                periods_var1 = periods_so,
                df_costs_var1 = costs1_df_sim[[i]],
                new_costs_var1 = temp_cost_sim[i],    
                staff_time_var1 = staff_time_sim[i],
                countries_var1 = countries_var2
                ),.GlobalEnv) ) # add costs here
      #Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
      a1_tax_sim[i] <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
      #Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
      a1_x_tax_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_x_in)
      # Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
      a1_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
      # Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
      a1_x_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_x_in)
      #KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
      a2_tax_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_new_in, costs_var = costs_a2_in)
      #KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
      a2_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new_in, costs_var = costs_a2_in)
      # EA1: no externality NPV using EAs costs
      a3_inc_a1_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_prevl_in, costs_var = costs2_ea_in)
      # EA2: yes externality NPV using EAs costs
      a3_inc_a1_all_x_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_prevl_in, costs_var = costs2_ea_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      a3_inc_a2_all_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_prevl_new_in, costs_var = costs2_ea_in)
    }

    total_time_sim <- Sys.time() - start_time

    ######    
    ######     
    return( list(
      "a1_tax_sim"          = a1_tax_sim,         
      "a1_x_tax_sim"        = a1_x_tax_sim,         
      "a1_all_sim"          = a1_all_sim,         
      "a1_x_all_sim"        = a1_x_all_sim,         
      "a2_tax_sim"          = a2_tax_sim,        
      "a2_all_sim"          = a2_all_sim,        
      "a3_inc_a1_all_sim"   = a3_inc_a1_all_sim,            
      "a3_inc_a1_all_x_sim" = a3_inc_a1_all_x_sim,            
      "a3_inc_a2_all_sim"   = a3_inc_a2_all_sim,            
      "total_time_sim"      = total_time_sim
    ) )
}

policy_estimates_varnames <- c(
  "a1_tax_sim",
  "a1_x_tax_sim",
  "a1_all_sim",
  "a1_x_all_sim",
  "a2_tax_sim",
  "a2_all_sim",
  "a3_inc_a1_all_sim",
  "a3_inc_a1_all_x_sim",
  "a3_inc_a2_all_sim"            
)

policy_estimates_text <- c(
  "A1. Tax revenue",
  "A1. With externalities. Tax",
  "A1. All income",
  "A1. With ext. All income",
  "A2. Tax",
  "A2. All income",
  "A3. All income of A1",
  "A3. All income of A1, with ext.",
  "A3. All income of A2. Main Policy Estimate"
  )


## ----all-steps, echo=print_code-----------------------------------------------------------------------------------
# TODO: Wrap this code chunk in chunk_xxxfunction

#chunk_runvalues <- function(){
# Function dependency is depicted as follows:
# f(g()) =
# f
# └──── g
#
#       ##     ###    ####    #####
# 1     2       3     4       5
#       ##     ###    ####    #####
# NPV_pe_f
# ├──── pv_benef_f
# │      ├──── earnings_app1_f
# │      |      ├──── wage_t_f
# │      |      |      └──── wage_0_f
# |      |      ├──── lambda_eff_f
# │      |      |      └────lambda1_t_f
# │      |      |            └────lambda1_in_f
# |      |      ├──── lambda1_in_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings_app2_f
# │      |      └────lambda_eff_f
# │      |           └────lambda1_t_f
# │      └──── interest_f
# └──── pv_costs_f (pv_costs_f)
#        ├──── delta_ed_final_f
#        ├──── interest_f
#        └──── s2_new_f
#        |      └──── costs1_p2_f
#        |             └──── costs1_p1_f
#        ├──── s2_f
#        └──── cost_per_student_f
#       ##     ###    ####    #####


# Approach 1
# NPV_pe_f --> a1_tax_pe
#     └────pv_benef_f --> pv_benef_tax_nx_in
#     |     ├────earnings_app1_f --> earnings_no_ext_in * tax_var1
#     |     |     ├────wage_t_f --> wage_t_in
#     |     |     |     └────wage_0_f --> wage_0_in
#     |     |     ├────lambda1_in_f --> lambda1_in
#     |     |     └────saturation_in_f --> saturation_in
#     |     └────interest_f --> interest_in
#     |
#     |
#     └────pv_costs_f --> costs2_in
#           ├────delta_ed_final_f --> delta_ed_final_in
#           ├────cost_per_student_f --> cost_per_student_in
#           ├────s2_f --> s2_in
#           └────interest_f --> interest_in


# unit test function
unit_test_f <- function(to_test_var, original_var, main_run_var = TRUE){
    if (main_run_var == TRUE) {
        if (length(to_test_var) > 1) {
            fails_test <- ( abs(sd(to_test_var) - original_var) > 0.0001 )
            text_val <- sd(to_test_var)
        } else {
            fails_test <- ( abs(to_test_var - original_var) > 0.0001 )
            text_val <- to_test_var
        }
        if (fails_test) {
            print(paste("Output has changed at",
                        deparse(substitute(to_test_var) ),
                        " to ", text_val) )
        }
      }
}

# TODO: update values of unit test within one_run_f
# one run of all the steps to get one policy estimate
one_run_f <-
  function(main_run_var1 = main_run_so,
           run_sim_var1 = run_sim_so,
           wage_ag_var1 = wage_ag_so,
           wage_ww_var1 = wage_ww_so,
           profits_se_var1 = profits_se_so,
           hours_se_cond_var1 = hours_se_cond_so,
           hours_ag_var1 = hours_ag_so,
           hours_ww_var1 = hours_ww_so,
           hours_se_var1 = hours_se_so,
           ex_rate_var1 = ex_rate_so,
           growth_rate_var1 = growth_rate_so,
           coef_exp_var1 = coef_exp_so[1],
           coef_exp2_var1 = coef_exp_so[2],
           lambda1_var1 = lambda1_in_f(lambda1_var = lambda1_so),
           prevalence_0_var1 = prevalence_0_so,
           prevalence_r_var1 = prevalence_r_so,
           new_prevl_r_var1 = new_prevalence_r_so,
           lambda2_var1 = lambda2_so,                                        
           coverage_var1 = coverage_so,                                        
           q_full_var1 = q_full_so,                                        
           q_zero_var1 = q_zero_so,                                        
           lambda1_new_var1 = lambda1_new_so,                                        
           gov_bonds_var1 = gov_bonds_so,                                        
           inflation_var1 = inflation_so,                                        
           gov_bonds_new_var1 = gov_bonds_new_so,                                                     
           inflation_new_var1 = inflation_new_so,                                       
           df_costs_var1 = costs_data_in,                                        
           staff_time_var1 = staff_time_so,                                        
           delta_ed_var1 = delta_ed_so,                                        
           delta_ed_ext_var1 = delta_ed_ext_so,                                        
           teach_sal_var1 = teach_sal_so,                                        
           teach_ben_var1 = teach_ben_so,                                        
           n_students_var1 = n_students_so,                                        
           teach_sal_new_var1 = teach_sal_new_so,                                            
           teach_ben_new_var1 = teach_ben_new_so,                              
           unit_cost_local_var1 = unit_cost_local_so,     
           unit_cost_local_new_var1 = unit_cost_2017usdppp_so,
           new_costs_var1 = new_costs_so,    
           countries_var1 = country_sel_so,
           years_of_treat_0_var1 = years_of_treat_0_so,
           years_of_treat_t_var1 = years_of_treat_t_so,
           tax_var1 = tax_so,                                        
           periods_var1 = periods_so) {                                        
    ####------------ Inputs for wage_t -----------------------------------------
    wage_0_in <- wage_0_f(
      wage_ag_var = wage_ag_var1,
      wage_ww_var = wage_ww_var1,
      profits_se_var = profits_se_var1,
      hours_se_cond_var = hours_se_cond_var1,
      hours_ag_var = hours_ag_var1,
      hours_ww_var = hours_ww_var1,
      hours_se_var = hours_se_var1,
      ex_rate_var = ex_rate_var1
    )
    unit_test_f(wage_0_in, 0.170124466664436, main_run_var = main_run_var1)
    ###---------- Inputs for earnings_app1_f ---------------------------------------
    wage_t_in <- wage_t_f(
      wage_0_var = wage_0_in,
      growth_rate_var = growth_rate_var1,
      coef_exp1_var = coef_exp_var1,
      coef_exp2_var = coef_exp2_var1
    )
    unit_test_f(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)

    lambda1_in <- lambda1_in_f(lambda1_var = lambda1_var1)
    unit_test_f(lambda1_in[1], 1.745, main_run_var = main_run_var1)

    lambda1_t_temp = lambda_t_f(
        lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
        years_of_treat_0_var = years_of_treat_0_var1,
        years_of_treat_t_var = years_of_treat_t_var1  
        )$lambda1_t

    lambda1_prevl_in <- lambda_eff_f(
      lambda1_var = lambda1_t_temp,
      prevalence_0_var = prevalence_0_var1,
      prevalence_r_var = prevalence_r_var1,
      other_prevl_r_var = new_prevl_r_var1,
      country_sel_var = countries_var1
      )$lambda1_eff_in
    unit_test_f(lambda1_prevl_in[1], 0.9508583060968, main_run_var = main_run_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)
    unit_test_f(lambda2_in[1], 10.2 , main_run_var = main_run_var1)

    saturation_in <- saturation_in_f(coverage_var = coverage_var1,
                                     q_full_var = q_full_var1,
                                     q_zero_var = q_zero_var1)$saturation_in
    unit_test_f(saturation_in, 0.511, main_run_var = main_run_var1)



    ###------------ Inputs for earnings_app2_f--------------------------------------
    lambda1_new_in <- lambda1_new_var1
    unit_test_f(lambda1_new_in, 79.51465,
              main_run_var = main_run_var1)
    lambda1_t_temp = lambda_t_f(
      lambda1_var = lambda1_new_var1,
      years_of_treat_0_var = years_of_treat_0_var1,
      years_of_treat_t_var = years_of_treat_t_var1  
    )$lambda1_t
    lambda1_prevl_new_in <- lambda_eff_f(lambda1_var = lambda1_t_temp,
                             prevalence_0_var = prevalence_0_var1,
                             prevalence_r_var = prevalence_r_var1,
                             other_prevl_r_var = new_prevl_r_var1,
                            country_sel_var = countries_var1
                            )$lambda1_eff_in
    unit_test_f(lambda1_prevl_new_in[1], 43.3278884864681, main_run_var = main_run_var1)

    ##------------ Inputs for pv_benef_f ---------------------------------------
    # earnings1
    earnings_no_ext_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_yes_ext_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )

    # earnings1 with prevalence
    earnings_no_ext_prevl_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prevl_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_yes_ext_prevl_in <- earnings_app1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prevl_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )

    # earnings2
    earnings_no_ext_new_in <- earnings_app2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1])
    # earnings2 with prevalence
    earnings_no_ext_prevl_new_in <- earnings_app2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_prevl_new_in[1])

    # interest rate NEED TO UPDATE TO EXACT RESULT
    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test_f(earnings_no_ext_in, 31.1421332040266,
              main_run_var = main_run_var1)
    unit_test_f(earnings_yes_ext_in, 167.667817450905,
              main_run_var = main_run_var1)
    unit_test_f(earnings_no_ext_prevl_in, 16.9694876943406,
              main_run_var = main_run_var1)
    unit_test_f(earnings_yes_ext_prevl_in, 153.495171941219,
              main_run_var = main_run_var1)    
    unit_test_f(interest_in, 0.0985, main_run_var = main_run_var1)

    ##-------------- Inputs for costs2_f----------------------------------------
    # Make explicit non-function inputs:
    delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE,
                                          delta_ed_var = delta_ed_var1,
                                          delta_ed_ext_var = delta_ed_ext_var1)
    unit_test_f(delta_ed_final_in, 0.01134819, main_run_var = main_run_var1)

    delta_ed_final_x_in <- delta_ed_final_f(
      include_ext_var = TRUE,
      delta_ed_var = delta_ed_var1,
      delta_ed_ext_var = delta_ed_ext_var1
    )
    unit_test_f(delta_ed_final_x_in,  0.05911765, main_run_var = main_run_var1)

    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test_f(interest_in, 0.0985, main_run_var = main_run_var1)

    interest_new_in <- interest_f(
      gov_bonds_var = gov_bonds_new_var1,
      inflation_var = inflation_new_var1)$interest_in

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)
    unit_test_f(cost_per_student_in,  116.8549, main_run_var = main_run_var1)



    cost_per_student_new_in <- cost_per_student_f(
      teach_sal_var = teach_sal_new_var1,
      teach_ben_var = teach_ben_new_var1,
      n_students_var = n_students_var1
    )

    s2_in <- s2_f(
      unit_cost_local_var = unit_cost_local_var1,
      ex_rate_var = ex_rate_var1,
      years_of_treat_var = years_of_treat_0_var1
    )
    unit_test_f(s2_in, 1.4219, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(
      earnings_var = earnings_no_ext_in * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_nx_in, 23.6070893378784,
              main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_in * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_yx_in, 127.0994867217, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(
      earnings_var = earnings_no_ext_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_nx_in, 142.42587835824, main_run_var = main_run_var1)
    #Baird all and no ext + prevalence
    pv_benef_all_nx_prevl_in <- pv_benef_f(
      earnings_var = earnings_no_ext_prevl_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_nx_prevl_in, 77.608498246463, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_yx_in, 766.814399527604,
              main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_prevl_in <- pv_benef_f(
      earnings_var = earnings_yes_ext_prevl_in,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_all_yx_prevl_in, 701.997019415827,
              main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new_in <- pv_benef_f(
      earnings_var = earnings_no_ext_new_in * tax_var1,
      interest_r_var = interest_new_in,
      periods_var = periods_var1
    )
    unit_test_f(pv_benef_tax_new_in, 88.1820199569814,
              main_run_var = main_run_var1)

    # KLPS4 all and no ext
    pv_benef_all_new_in <- pv_benef_f(earnings_var = earnings_no_ext_new_in,
                                   interest_r_var = interest_new_in,
                                   periods_var = periods_var1)
    unit_test_f(pv_benef_all_new_in, 532.018219951622, main_run_var = main_run_var1)
    # KLPS4 all and no ext + prevalence
    pv_benef_all_prevl_new_in <- pv_benef_f(earnings_var = earnings_no_ext_prevl_new_in,
                                   interest_r_var = interest_new_in,
                                   periods_var = periods_var1)
    unit_test_f(pv_benef_all_prevl_new_in, 289.899107986178, main_run_var = main_run_var1)
    #Costs asd
    # costs1: EA costs no externalities
    cost1_in <- costs1_p2_f(country_total_var = df_costs_var1$total,
                            country_cost_var = df_costs_var1$costs_by_country,
                            staff_time_var = staff_time_var1,
                            country_name_var = df_costs_var1$Country,
                            select_var = countries_var1,
                            other_costs_var = new_costs_var1)
    unit_test_f(cost1_in,  0.08480686,
              main_run_var = main_run_var1)
    # s2_ea_in <-- cost1_in (costs1_p2_f) <-- cost_data (costs1_p1_f())
    s2_ea_in <- s2_new_f(interest_var = interest_new_in,
                      unit_cost_local_var = cost1_in,
                      ex_rate_var = 1,
                      year_of_treat_var = years_of_treat_t_var1)
    unit_test_f(s2_ea_in,  0.19634422968991, main_run_var = main_run_var1)
    costs2_ea_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_new_in,
      cost_of_schooling_var = 0,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_ea_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_ea_in,  0.147258172267433, main_run_var = main_run_var1)
    # costs2: Baird no externalities
    costs2_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_in, 11.776188118988, main_run_var = main_run_var1)
earnings_no_ext_in
    # Baird yes externalities
    costs2_x_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_x_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs2_x_in,  25.1962130559894, main_run_var = main_run_var1)

    s2_new_in <- s2_new_f(interest_var = interest_new_in,
                          unit_cost_local_var = unit_cost_local_new_var1,
                          ex_rate_var = 1,
                          year_of_treat_var = years_of_treat_t_var1)
    # costs2: KLPS4
    costs_a2_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_new_in,
      cost_of_schooling_var = cost_per_student_new_in,
      s1_var = 0,
      q1_var = q_zero_var1,
      s2_var = s2_new_in,
      q2_var = q_full_var1
    )
    unit_test_f(costs_a2_in, 32.2977546110344, main_run_var = main_run_var1)
    return( list(
      "wage_0_in" = wage_0_in,
      "wage_t_in" = wage_t_in,
      "lambda1_in" = lambda1_in,
      "lambda1_prevl_in" = lambda1_prevl_in,
      "lambda2_in" = lambda2_in,
      "saturation_in" = saturation_in,
      "lambda1_new_in" = lambda1_new_in,
      "lambda1_prevl_new_in" = lambda1_prevl_new_in,
      "earnings_no_ext_in" = earnings_no_ext_in,
      "earnings_no_ext_prevl_in" = earnings_no_ext_prevl_in,
      "earnings_yes_ext_in" = earnings_yes_ext_in,
      "earnings_yes_ext_prevl_in" = earnings_yes_ext_prevl_in,
      "earnings_no_ext_new_in" = earnings_no_ext_new_in,
      "earnings_no_ext_prevl_new_in" = earnings_no_ext_prevl_new_in,
      "interest_in" = interest_in,
      "costs1_country_in" = costs_data_in,
      "delta_ed_final_in" = delta_ed_final_in,
      "delta_ed_final_x_in" = delta_ed_final_x_in,
      "cost_per_student_in" = cost_per_student_in,
      "s2_in" = s2_in,
      "pv_benef_tax_nx_in" = pv_benef_tax_nx_in,
      "pv_benef_tax_yx_in" = pv_benef_tax_yx_in,
      "pv_benef_all_nx_in" = pv_benef_all_nx_in,
      "pv_benef_all_nx_prevl_in" = pv_benef_all_nx_prevl_in,
      "pv_benef_all_yx_in" =  pv_benef_all_yx_in,
      "pv_benef_all_yx_prevl_in" = pv_benef_all_yx_prevl_in,
      "pv_benef_tax_new_in" = pv_benef_tax_new_in,
      "pv_benef_all_new_in" = pv_benef_all_new_in,
      "pv_benef_all_prevl_new_in" = pv_benef_all_prevl_new_in,
      "costs2_ea_in" = costs2_ea_in,
      "costs2_in" = costs2_in,
      "costs2_x_in" = costs2_x_in,
      "costs_a2_in" = costs_a2_in,
      "cost1_in" = cost1_in
    ) )
  }

invisible( list2env(one_run_f(),.GlobalEnv) )

#  return( sapply( ls(pattern= "_in\\b"), function(x) get(x)) )

#}




## ----generate-plot-function, purl = TRUE, echo = FALSE------------------------------------------------------------
# generate_plot_f: function to generate plots for both Dynamic Document and
# shiny app. It takes in the simulated data, policy estimate text, and rescale
# variable. These are intermediary variables to exclude the interactivity of
# shiny app from the plot generation process.  
chunk_generate_plot <- function() {
  generate_plot_f <- function(npv_all_sim,
                              policy_estimates_text_selected,
                              rescale, SD = FALSE){
    total_time_sim <- npv_all_sim$total_time_sim
    position <- which( policy_estimates_text == policy_estimates_text_selected)
    npv_sim <- npv_all_sim[[ policy_estimates_varnames[position] ]]    
    npv_for_text <- paste("Median NPV: ", round(median(npv_sim), 2))
    npv_for_text2 <- NULL
    if (SD){
    npv_for_text2 <- paste("SD NPV: ", round(sd(npv_sim), 2))
    }
    plot1 <- ggplot() +
      geom_density(
        aes(x = npv_sim,
            alpha = 1 / 2, ..scaled..),
        kernel = "gau",
        lwd = 1,
        fill = "#007ba7",
        color = "darkblue",
        alpha = 0.3
      ) +
      geom_vline(
        xintercept = c(0, median(npv_sim)),
        col = c("black", "darkblue"),
        lwd = c(1, 1),
        linetype = c("solid", "dashed")
      ) +
      coord_cartesian(xlim = c(-300,1000),  ylim =  c( 0, 1.2 ))  +  # fixing the x axis so shifts in the density can be seen
      #xlim(range(density(npv_sim)$x)) +
      guides(alpha = "none", colour = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0))) +
      annotate(
        "text",
        x = 1 * median(npv_sim),
        y = 0.2,
        label = npv_for_text,
        size = 6,
        color = "darkblue"
      ) +
      annotate(
        "text",
        x = 1 * median(npv_sim),
        y = 0.1,
        label = npv_for_text2,
        size = 6,
        color = "darkblue"
      ) +
      theme(
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", size = 1.5)
      )

    if (rescale == TRUE) {
      plot1 <-
        suppressMessages(plot1 + coord_cartesian(xlim = 1.2 * c(min(c(
          -1, npv_sim
        )), max(c(
          100, npv_sim
        )))))
    }
    return (list(plot1,position,total_time_sim))
}
################################################################################
################################################################################
return(list("generate_plot_f" = generate_plot_f))
}

invisible( list2env(chunk_generate_plot(),.GlobalEnv) )

