## ----setup, include=FALSE, purl=TRUE---------------------------------------------------------------------------------------------------------------------------------------------
# Loading required libraries
list.of.packages <- c("tidyverse", "here", "kableExtra", "readxl","plotly")

# sapply(list.of.packages, function(x) pacman::p_load(get(x)))

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos= "http://cran.cnr.berkeley.edu/")

lapply(list.of.packages, library, character.only = TRUE)

knitr::opts_knit$set(root.dir = here())
knitr::opts_chunk$set(echo = TRUE)

print_code <- TRUE

equationIndex <- 0
sum_table_index <- 0

# Emma: please read in detail about what "here()" does

setwd(here())

colorize = function(x, color){
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}(%s)", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<font color='%s'>%s</font>", color, x)
  } else x
}
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(warning = FALSE)




## ----sources, eval = TRUE, echo=print_code, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------
# - inputs: none
# - outputs: all sources coming from data, research and guesswork
chunk_params <- function(){
###############################################################################
###############################################################################  
    #############
    ##### Data  
    #############
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    gov_bonds_new_so <- 0.09
    inflation_new_so <- 0.04

    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B
    profits_se_so <- 1766          #Control group monthly self-employed profits -
                                   #Table 4, Panel C, Column 5, Row 1 
                                   #FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 4, Panel D
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 4, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 4, Panel A
    ex_rate_so <- 74               #Exchange Rate - Central Bank of Kenya 74 , 85

    ex_rate_2018        <- 101.30  # Exchange rate (KES per international $)
                                    # - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)

    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    tax_so <- 0.16575              #ADD INFO!
    unit_cost_local_so <- 43.66    #Deworm the World

    unit_cost_so <- 0.42           # Unit cost of deworming (in 2018 USD) - from Evidence Action
    #CALCULATIONS TO CONVERT ALL CURRENCY TO 2017 USD PPP
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so 
    # Adjust for inflation: convert all costs to 2017 USD
    # Move this calculations into the body of the document (and outside of the sources chunk)
    unit_cost_2017usdppp_so <- unit_cost_ppp_so * cpi_2017_so / cpi_2018_so  # 0.8296927

    years_of_treat_so <- 2.41      #Additional Years of Treatment - Table 1, Panel A
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

    #############
    ##### Research
    #############
    df_research_so <- read_csv("rawdata/research/research_params.csv")   
    lambda1_so <- c(3.49, 0)            #Hrs per week increase for men and women 
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, Panel B
    lambda1_new_so <- c(86.54642,   # avg treatment effect from klps2 (already adjusted for ppp and inflation) - w@w
                               82.99311,   # avg treatment effect from klps3 (already adjusted for ppp and inflation) - w@w
                               85.44088)   # avg treatment effect from klps4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(43, 83, 172)  # ADD SOURCE
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct secondary schooling increase (from Joan)
                        0.0239963665555466, 0.027301406306074, 0.0234125454594173,
                       0.0279278879439199, 0.00647044449446303, 0.00835739437790601)                                     
    delta_ed_so <- cbind(delta_ed_so, 1999:2007)
    delta_ed_ext_so <- c(-0.0110126908021048,	0.0140448546741008,	-0.0034636291545585,  #Additional externality secondary schooling increase (from Joan)
                           0.0112940214439477,	0.0571608179771775,	-0.0560546793186931,
                           0.0558284756343451,	0.1546264843901160,	0.0055961489945619)
    delta_ed_ext_so <- cbind(delta_ed_ext_so, 1999:2007)    
    delta_ed_par_so <- 1
    delta_ed_ext_par_so <- 1
    include_ext_so <- TRUE
    
    #This is are the parameters labeled eta in the doc
    prevalence_0_so <- 0.92 # 0.92 doi: https://doi.org/10.1111/j.1468-0262.2004.00481.x  location: table 2, row 6, column 1
    prevalence_r_so <- c("india" = 0.5665, "kenya" = 0.345, "nigeria" = 0.27, "vietnam" = 0.145)  #0.5665   0.5013121
    # based on https://docs.google.com/spreadsheets/d/1drKdU-kRjlRtwXq6nCqFC6gcoQ-eOaLfT9MWHSMZ0MA/edit?usp=sharing
    new_prevalence_r_so <- NULL
    #############
    ##### Guess work   
    #############
    periods_so <- 50               #Total number of periods to forecast wages
    time_to_jm_so <- 10            #Time from initial period until individual join the labor force
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)
                                                    #- see notes(0.1019575, -0.0010413), (0,0)
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    teach_sal_new_so <- (50000 * 12 / 49.77)
    teach_ben_new_so <- 0
                                  #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits -
                                  #news sources * 12 / ex_rate_2017_ppp_so
                                  # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                  # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    teach_sal_2017usdppp_so <- teach_sal_new_so * cpi_2017_so / cpi_2017_so # redundant, but for the sake of consistency

    n_students_so <- 45            #Average pupils per teacher	45
    staff_time_so <- 0.3           #Added Deworming costs due to goverment staff time
    run_sim_so <- FALSE
    main_run_so <- TRUE
    rescale_so <- TRUE
    costs_par_so <- 1
    costs_par_sd_so <- 0.1
    counts_par_so <- 1
    counts_par_sd_so <- 0.1
    nsims_so <- 1e3
    new_costs_so <- NULL
    country_sel_so <- list("india", "kenya", "nigeria", "vietnam")
    country_sel_pop_so <- c(
      "india" = 1.366417750 * 1e9,
      "kenya" = 5.257397 * 1e7,
      "nigeria" = 2.0096360 * 1e8,
      "vietnam" = 9.646211 * 1e7
    ) 
    #https://data.worldbank.org/indicator/SP.POP.TOTL
    # options: "baird1_sim","baird2_sim","baird3_sim", "baird4_sim", "klps4_1_sim",
    # "klps4_2_sim", "ea1_sim", "ea2_sim", "ea3_sim", "cea_no_ext_ea_sim", "rcea_no_ext_ea_sim"
    policy_estimate_so <- "ea3_sim"

    # Fix teach_sal_so       
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
###############################################################################
###############################################################################    
}
invisible( list2env(chunk_params(),.GlobalEnv) )

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




## ----eq_1, echo=print_code-------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: total per capita benefits, total per capita costs, fudging factor
# - outputs: Cost-effectiveness ratio & ratio to cash CEA, NPV
chunk_policy_est <- function(){
###############################################################################
###############################################################################  

    NPV_pe_f <- function(benefits_var = 1, costs_var = 1){
        benefits_var - costs_var
    }
    CEA_pe_f <- function(benefits_var = 1, fudging_var = 0, costs_var = 1) {
        ( benefits_var * ( 1 + fudging_var ) ) / costs_var
    }
    RCEA_pe_f <- function(CEA_var = 1, CEA_cash_var = 1){
        CEA_var / CEA_cash_var
    }

###############################################################################
###############################################################################  
    return(list("CEA_pe_f" = CEA_pe_f,
                "RCEA_pe_f" = RCEA_pe_f,
                "NPV_pe_f" = NPV_pe_f))
}
invisible( list2env(chunk_policy_est(),.GlobalEnv) )


## ----benefits, echo=print_code---------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: nothing
# - outputs: function that computes the country weights used in the final costs
chunk_benefits <- function(){
###############################################################################
###############################################################################  

    pv_benef_f <- function(earnings_var = earnings_in, interest_r_var = interest_in,
                    periods_var = periods_so) {
      index_t <- 0:periods_var
      res1 <- sum( ( 1 / (1 + interest_r_var) )^index_t * earnings_var )
      return(res1)   
    }

###############################################################################
###############################################################################  
    return(list("pv_benef_f" = pv_benef_f))
}
invisible( list2env(chunk_benefits(),.GlobalEnv) )


## ----interest-rate, echo=print_code----------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: gov_bonds_so, inflation_so 
# - outputs: interest_in, interest_exct_in 
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
interest_16 <- as.numeric( 
  interest_f(gov_bonds_var = gov_bonds_so,
             inflation_var = inflation_so)$interest_in 
  )
interest_19 <- as.numeric( 
  interest_f(gov_bonds_var = gov_bonds_new_so,
             inflation_var = inflation_new_so)$interest_in  
  )




## ----earnings1-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: wage_in, lambda1_so, lambda2_so, saturation, coverage_so
# - outputs: earnings (no name specified)
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings1_f <- function(wage_var = wage_in,
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
    return(list("earnings1_f" = earnings1_f))
}

invisible( list2env(chunk_earnings1(),.GlobalEnv) ) 


## ----wage_t----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_mo) and function (wage_0_mo_f), 
# Wage trajectory: value (wage_t_mo) and function (wage_t_mo_f).
chunk_wages <- function(){
################################################################################
################################################################################  
    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision

    wage_0_mo_f <- function(wage_ag_var, wage_ww_var, profits_se_var, 
                            hours_se_cond_var, hours_ag_var, hours_ww_var, 
                            hours_se_var, ex_rate_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
        wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
        alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) / 
          sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
        res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
        return(res1)
    }

    wage_t_mo_f <- function(wage_0_var,
                       growth_rate_var,
                       coef_exp1_var,
                       coef_exp2_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        res1 <- 52 * wage_0_var * ( ( 1 + growth_rate_var )^experience_aux ) *
          ( 1 + coef_exp1_var * experience_aux + coef_exp2_var * 
              (experience_aux^2) ) * ifelse(0:periods_so >= time_to_jm_so, 1, 0)
        return(res1)
    }

    wage_0_mo <- wage_0_mo_f(wage_ag_var = wage_ag_so,  
                         wage_ww_var = wage_ww_so,
                         profits_se_var = profits_se_so,
                         hours_se_cond_var = hours_se_cond_so,  
                         hours_ag_var = hours_ag_so,
                         hours_ww_var = hours_ww_so,
                         hours_se_var = hours_se_so,
                         ex_rate_var = ex_rate_so)  

    #close to value from spreadsheet (Calcs-Table 5!N21.. = 7.701634678),
    #but I suspect diff due to computational precision
    wage_t_mo <- wage_t_mo_f(wage_0_var = wage_0_mo,
                       growth_rate_var = growth_rate_so,
                       coef_exp1_var = coef_exp_so[1],
                       coef_exp2_var = coef_exp_so[2])

################################################################################
################################################################################
    return(list("wage_0_mo_f" = wage_0_mo_f, "wage_0_mo" = wage_0_mo,
                "wage_t_mo_f" = wage_t_mo_f, "wage_t_mo" = wage_t_mo))
}

invisible( list2env(chunk_wages(),.GlobalEnv) )


## ----lambdas---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: lambda1_so, lambda2_so
# - outputs: lambda1_in_f, lambda2_in_f functions
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


## ----coverage-and-saturation-----------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: coverage_so, q_full_so, q_zero_so
# - outputs: saturation_in
chunk_coverage <- function(){
###############################################################################
###############################################################################  

    saturation_in_f <- function(coverage_var = coverage_so, 
                                q_full_var = q_full_so, q_zero_var = q_zero_so){
      saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
      return(list("saturation_in" = saturation_in))
    }

###############################################################################
###############################################################################  
    return(list("saturation_in_f" = saturation_in_f))   
}
invisible( list2env(chunk_coverage(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:





## ----cost2-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: periods_so, delta_ed_final_in, interest (varies by approach), cost_per_student_in, s2_in, q2_in
# - outputs: pv_costs_f
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

    pv_costs_f <- function(periods_var = periods_so, 
                        delta_ed_var = delta_ed_final_in,
                        interest_r_var = NULL, 
                        cost_of_schooling_var = cost_per_student_in,
                        s1_var = 0, q1_var = 0, 
                        s2_var = s2_in, q2_var = q2_in) {
        index_t <- 0:periods_var
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


## ----unit_costs2-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: unit_cost_local_so, ex_rate_so, years_of_treat_so
# - outputs: s2_f
chunk_unit_costs2 <- function(){
###############################################################################
###############################################################################  

    s2_f <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so, 
                     years_of_treat_var = years_of_treat_so) {
      ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    }

###############################################################################
###############################################################################  
    return(list("s2_f" = s2_f) )
}
invisible( list2env(chunk_unit_costs2(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:




## ----ed-costs--------------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: teach_sal_so, teach_ben_so, n_students_so, include_ext_so, delta_ed_so, delta_ed_ext_so
# - outputs: cost_per_student_f, delta_ed_final_f
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
delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE)






## ----delta-earnings, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: t_var, lambda1_new_so[1], lambda1_new_so[2], lambda1_new_so[3]
# - outputs: earnings2_f
chunk_new_earnings <- function(){
###############################################################################
###############################################################################  

    earnings2_f <- function(t_var = 1,
                               lambda1k1_var = lambda1_new_so[1],
                               lambda1k2_var = lambda1_new_so[2],
                               lambda1k3_var = lambda1_new_so[3]) {
        1*(10 <= t_var & t_var < 15) * lambda1k1_var +
        1*(15 <= t_var & t_var < 20) * lambda1k2_var +
        1*(20 <= t_var) * lambda1k3_var
    }

###############################################################################
###############################################################################             
    return(list("earnings2_f" = earnings2_f))
}

invisible( list2env(chunk_new_earnings(),.GlobalEnv) )





## ----unit_costs2_new-------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: unit_cost_local_so, ex_rate_so, interest_19
# - outputs: s2_f_new
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

    s2_f_new <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     interest_var = interest_19) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      sum(( unit_cost * (1 + interest_var)^(-(0:2)) ) * c(1,1,0.4))
    }

###############################################################################
###############################################################################  
    return(list("s2_f_new" = s2_f_new) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
# New costs are all in dollars so, will compute them using ex rate of 1.
s2_in <- s2_f_new(
  interest_var = interest_19,
  unit_cost_local_var = unit_cost_2017usdppp_so,
  ex_rate_var = 1
)
s2_new <- s2_in
q2_in <- q_full_so


## ----ed-costs-new----------------------------------------------------------------------------------------------------------------------------------------------------------------

delta_ed_in <- delta_ed_so[,1]
cost_per_student_in_new <- cost_per_student_f(teach_sal_var = (50000*12/49.77),
                                          teach_ben_var = 0,
                                          n_students_var = 45)





## ----lambdas_eff-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: lambda1_in_f(), prevalence_0_so, prevalence_r_so
# - outputs: lambda_eff_f
chunk_lambdas_eff<- function(){
###############################################################################
###############################################################################    

    lambda_eff_f <- function(lambda1_var = lambda1_in_f(), 
                           prevalence_0_var = prevalence_0_so, 
                           prevalence_r_var = prevalence_r_so, 
                           country_sel_var = country_sel_so, 
                           country_sel_pop_var = country_sel_pop_so, 
                           other_prev_r = new_prevalence_r_so){
      temp_sel <- as.character(country_sel_var)  
      # if a positive number of countries is selected
      if (is.null(other_prev_r)) {
        temp_weights <- country_sel_pop_var[temp_sel] / 
          sum(country_sel_pop_var[temp_sel])
        prevalence_r_final <- sum( prevalence_r_var[temp_sel] * temp_weights )
      } else {
        prevalence_r_final <- other_prev_r  
      }
      # IF TO SELECT COUNTRY OR ENTER DATA
      # IF SELECT COUNTRY, PICK FROM:
      # country_sel_so 
      # country_sel_pop_so   
      # If no country selection then one number must be provided. 
      
      lambda1_eff_temp <- lambda1_var / prevalence_0_var
      lambda1_eff_in <- lambda1_eff_temp * prevalence_r_final
      return(  
        list("lambda1_eff_in" = lambda1_eff_in, 
             "prevalence_r_final_in" = prevalence_r_final)
              )
    }  

##############################################################################
###############################################################################  
    return( list("lambda_eff_f" = lambda_eff_f) )
}
invisible( list2env(chunk_lambdas_eff(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_r_in <- lambda_eff_f()$lambda1_eff_in
prevalence_r_in <- lambda_eff_f()$prevalence_r_final_in


## ----eq_3, echo=print_code, eval=TRUE--------------------------------------------------------------------------------------------------------------------------------------------
# - inputs: nothing
# - outputs: (1) function that computes the country weights used in the final 
# costs (2)  function that computes the weighted sum of country costs 
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
    # clean and aggreagate data at country level
    costs1_p1_f <- function(df_costs_var = df_costs_so,
                         df_costs_cw_var = df_costs_cw_so,
                         df_counts_var = df_counts_so) {
      #anoying message: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
      ## Counts
      # Data cleanning:
      # Add country

  suppressMessages(            
      df_counts_temp <- df_costs_cw_var %>%
        right_join(df_counts_var, by = "Country/State") %>%
        mutate(Country = tolower(Country))
  )
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
      # Add country
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
      #sum across payers and multiply by delta (second to last)
  suppressMessages(            
      country_cost <- costs_by_payer %>%
        group_by(Country) %>%
        summarise("costs_by_country" = 
                    sum(costs_by_payer) )  
      # Compute the per capita cost for each country (c_i and w_i)
  )
        costs_data <- country_cost %>%
         left_join(c_counts, by = "Country") 
      return( costs_data )
    }
    
    # Compute weights and per capta costs
    costs1_p2_f <- function(country_total_var = costs_data$total, 
                         country_cost_var = costs_data$costs_by_country,
                         staff_time_var = staff_time_so, 
                         country_name_var = costs_data$Country,
                         select_var = list("india", "kenya", "nigeria", "vietnam"), 
                         other_costs = NULL) {
      # select countries 
      country_total_var_temp <- country_total_var[country_name_var %in% select_var]
      country_cost_var_temp <- country_cost_var[country_name_var %in% select_var]
      # create country weight
      c_weights <- country_total_var_temp / sum(country_total_var_temp)
      # create country per capita costs, adjusted by staff time
      per_cap <- country_cost_var_temp * (1 + staff_time_var) / country_total_var_temp

      # replace contry costs with new one if there is a new country (only count that new country)
      #  (the weighthed sum of this scalar will just be the same number)
      if (!is.null(other_costs)) {
      #if (FALSE) {  
        per_cap <- other_costs * (1 + staff_time_var) 
      }

      sum(c_weights * per_cap) 
    }
###############################################################################
###############################################################################  
    return( list("costs1_p1_f" = costs1_p1_f, 
                 "costs1_p2_f" = costs1_p2_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
costs1_p1_in <- costs1_p1_f()
costs_data <- costs1_p1_in
costs1_p2_in <- costs1_p2_f()






## ----all-steps-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Function dependency is depicted as follows:
# f(g()) =
# f
# └──── g
#
#       ##     ###    ####    #####
# 1     2       3     4       5
#       ##     ###    ####    #####
# NPV_pe_f, CEA_pe_f, RCEA_pe_f
#  TO DO: review and update this function tree
# ├──── pv_benef_f
# │      ├──── earnings1_f
# │      |      ├──── wage_t_mo_f
# │      |      |      └──── wage_0_mo_f
# |      |      ├──── lambda1_in_f
# │      |      |      └────lambda_eff_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings2_f
# │      |      └────lambda_eff_f
# │      └──── interest_f
# └──── pv_costs_f (pv_costs_f)
#        ├──── delta_ed_final_f
#        ├──── interest_f
#        └──── s2_f_new
#        |      └──── costs1_p2_f
#        |             └──── costs1_p1_f
#        ├──── s2_f
#        └──── cost_per_student_f
#       ##     ###    ####    #####

#unit test function
unit_test <- function(to_test_var, original_var, main_run_var = TRUE){
    if (main_run_var == TRUE) {
        if (length(to_test_var) > 1) {
            fails_test <- ( abs(sd(to_test_var) - original_var) > 0.0001 )
            text_val <- sd(to_test_var)
        } else {
            fails_test <- ( abs(to_test_var - original_var) > 0.0001 )
            text_val <- to_test_var
        }
        if (fails_test) {
            print(paste("Output has change at", 
                        deparse(substitute(to_test_var) ), 
                        " to ", text_val) )
        }
      }
}

#TO DO: Put this somewhere that makes more sense
costs_data <- costs1_p1_f(df_costs_var = df_costs_so,
                          df_costs_cw_var = df_costs_cw_so,
                          df_counts_var = df_counts_so)

#TODO: update values of unit test within one_run
# one run of all the steps to get one policy estimate
one_run <-
  function(main_run_var1 = main_run_so,
           run_sim_var = run_sim_so,
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
           new_prev_r_var1 = new_prevalence_r_so,
           lambda2_var1 = lambda2_so,                                        
           coverage_var1 = coverage_so,                                        
           q_full_var1 = q_full_so,                                        
           q_zero_var1 = q_zero_so,                                        
           lambda1_new_var1 = lambda1_new_so,                                        
           gov_bonds_var1 = gov_bonds_so,                                        
           inflation_var1 = inflation_so,                                        
           gov_bonds_new_var1 = gov_bonds_new_so,                                                     
           inflation_new_var1 = inflation_new_so,                                       
           df_costs_var1 = costs_data,                                        
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
           years_of_treat_var1 = years_of_treat_so,                                        
           tax_var1 = tax_so,                                        
           periods_var1 = periods_so) {                                        
    ####------------ Inputs for wage_t -----------------------------------------
    wage_0_in <- wage_0_mo_f(
      wage_ag_var = wage_ag_var1,
      wage_ww_var = wage_ww_var1,
      profits_se_var = profits_se_var1,
      hours_se_cond_var = hours_se_cond_var1,
      hours_ag_var = hours_ag_var1,
      hours_ww_var = hours_ww_var1,
      hours_se_var = hours_se_var1,
      ex_rate_var = ex_rate_var1
    )
    unit_test(wage_0_in, 0.170124466664436, main_run_var = main_run_var1)
    ###---------- Inputs for earnings1_f ---------------------------------------
    wage_t_in <- wage_t_mo_f(
      wage_0_var = wage_0_in,
      growth_rate_var = growth_rate_var1,
      coef_exp1_var = coef_exp_var1,
      coef_exp2_var = coef_exp2_var1
    )
    unit_test(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)
    
    lambda1_in <- lambda1_in_f(lambda1_var = lambda1_var1)
    unit_test(lambda1_in[1], 1.745, main_run_var = main_run_var1)
#browser()
    lambda1_prev_in <- lambda_eff_f(
      lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
      prevalence_0_var = prevalence_0_var1,
      prevalence_r_var = prevalence_r_var1, 
      other_prev_r = new_prev_r_var1, 
      country_sel_var = countries_var1
      )$lambda1_eff_in
    unit_test(lambda1_prev_in[1], 0.9508583060968, main_run_var = main_run_var1)
    
    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)
    unit_test(lambda2_in[1], 10.2 , main_run_var = main_run_var1)
    
    saturation_in <- saturation_in_f(coverage_var = coverage_var1,
                                     q_full_var = q_full_var1,
                                     q_zero_var = q_zero_var1)$saturation_in 
    unit_test(saturation_in, 0.511, main_run_var = main_run_var1)

    
    

    ###------------ Inputs for earnings2_f--------------------------------------
    lambda1_new_in <- lambda1_new_var1
    unit_test(lambda1_new_in, 1.8184154558571, main_run_var = main_run_var1)
    
    lambda1_prev_new_in <- lambda_eff_f(lambda1_var = lambda1_new_var1,
                             prevalence_0_var = prevalence_0_var1,
                             prevalence_r_var = prevalence_r_var1, 
                             other_prev_r = new_prev_r_var1, 
                            country_sel_var = countries_var1
                            )$lambda1_eff_in
    unit_test(lambda1_prev_new_in, 0.990862716410, main_run_var = main_run_var1)

    ##------------ Inputs for pv_benef_f ---------------------------------------
    # earnings1
    earnings_in_no_ext <- earnings1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_in_yes_ext <- earnings1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )

    # earnings1 with prevalence
    earnings_in_no_ext_prev <- earnings1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prev_in[1],
      lambda2_var = 0,
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    earnings_in_yes_ext_prev <- earnings1_f(
      wage_var = wage_t_in,
      lambda1_var = lambda1_prev_in[1],
      lambda2_var = lambda2_in[1],
      saturation_var = saturation_in,
      coverage_var = coverage_var1
    )
    
    # earnings2
    earnings_in_no_ext_new <- earnings2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1],
                                          lambda1k2_var = lambda1_new_in[2],
                                          lambda1k3_var = lambda1_new_in[3])
    # earnings2 with prevalence 
    earnings_in_no_ext_prev_new <- earnings2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_prev_new_in[1],
                                          lambda1k2_var = lambda1_prev_new_in[2],
                                          lambda1k3_var = lambda1_prev_new_in[3])
    
    # interest rate NEED TO UPDATE TO EXACT RESULT
    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test(earnings_in_no_ext, 31.1421332040266, 
              main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext, 167.667817450905, 
              main_run_var = main_run_var1)
    unit_test(earnings_in_no_ext_prev, 16.9694876943406, 
              main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext_prev, 153.495171941219, 
              main_run_var = main_run_var1)    
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    ##-------------- Inputs for costs2_f----------------------------------------
    # Make explicit non-function inputs:
    delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE,
                                          delta_ed_var = delta_ed_var1,
                                          delta_ed_ext_var = delta_ed_ext_var1)
    unit_test(delta_ed_final_in, 0.01134819, main_run_var = main_run_var1)

    delta_ed_final_in_x <- delta_ed_final_f(
      include_ext_var = TRUE,
      delta_ed_var = delta_ed_var1,
      delta_ed_ext_var = delta_ed_ext_var1
    )
    unit_test(delta_ed_final_in_x,  0.05911765, main_run_var = main_run_var1)

    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in 
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    interest_in_new <- interest_f(
      gov_bonds_var = gov_bonds_new_var1,
      inflation_var = inflation_new_var1)$interest_in

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)
    unit_test(cost_per_student_in,  116.8549, main_run_var = main_run_var1)



    cost_per_student_in_new <- cost_per_student_f(
      teach_sal_var = teach_sal_new_var1,
      teach_ben_var = teach_ben_new_var1,
      n_students_var = n_students_var1
    )

    s2_in <- s2_f(
      unit_cost_local_var = unit_cost_local_var1,
      ex_rate_var = ex_rate_var1,
      years_of_treat_var = years_of_treat_var1
    )
    unit_test(s2_in, 1.4219, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f, CEA_pe_f and RCEA_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(
      earnings_var = earnings_in_no_ext * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_tax_nx_in, 23.6070893378784, 
              main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(
      earnings_var = earnings_in_yes_ext * tax_var1,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_tax_yx_in, 127.0994867217, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(
      earnings_var = earnings_in_no_ext,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_all_nx_in, 142.42587835824, main_run_var = main_run_var1)
    #Baird all and no ext + prevalence
    pv_benef_all_nx_prev_in <- pv_benef_f(
      earnings_var = earnings_in_no_ext_prev,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_all_nx_prev_in, 77.608498246463, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(
      earnings_var = earnings_in_yes_ext,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_all_yx_in, 766.814399527604,
              main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_prev_in <- pv_benef_f(
      earnings_var = earnings_in_yes_ext_prev,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_all_yx_prev_in, 701.997019415827,
              main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new <- pv_benef_f(
      earnings_var = earnings_in_no_ext_new * tax_var1,
      interest_r_var = interest_in_new,
      periods_var = periods_var1
    )
    unit_test(pv_benef_tax_new, 157.5017,
              main_run_var = main_run_var1)
    
    # KLPS4 all and no ext
    pv_benef_all_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    unit_test(pv_benef_all_new, 950.2367, main_run_var = main_run_var1)
    # KLPS4 all and no ext + prevalence
    pv_benef_all_prev_new <- pv_benef_f(earnings_var = earnings_in_no_ext_prev_new,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    unit_test(pv_benef_all_prev_new, 517.78821257177, main_run_var = main_run_var1)
    
    #Costs asd
    # costs1: EA costs no externalities
    cost1_in <- costs1_p2_f(country_total_var = df_costs_var1$total,
                            country_cost_var = df_costs_var1$costs_by_country,
                              staff_time_var = staff_time_var1, 
                              country_name_var = df_costs_var1$Country,
                              select_var = countries_var1, 
                              other_costs = new_costs_var1)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)
    # s2_ea_in <-- cost1_in (costs1_p2_f) <-- cost_data (costs1_p1_f())
    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, 
                      ex_rate_var = 1)
    unit_test(s2_ea_in,  0.19634422968991, main_run_var = main_run_var1)
    costs2_ea_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in_new,
      cost_of_schooling_var = 0,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_ea_in,
      q2_var = q_full_var1
    )
    unit_test(costs2_ea_in,  0.147258172267433, main_run_var = main_run_var1)
    # costs2: Baird no externalities
    costs2_in <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test(costs2_in, 11.776188118988, main_run_var = main_run_var1)

    # Baird yes externalities
    costs2_in_x <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in_x,
      interest_r_var = interest_in,
      cost_of_schooling_var = cost_per_student_in,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_in,
      q2_var = q_full_var1
    )
    unit_test(costs2_in_x,  25.1962130559894, main_run_var = main_run_var1)

    s2_new_in <- s2_f_new(interest_var = interest_in_new, 
                          unit_cost_local_var = unit_cost_local_new_var1, 
                          ex_rate_var = 1)
    # costs2: KLPS4
    costs_k <- pv_costs_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in_new,
      cost_of_schooling_var = cost_per_student_in_new,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_new_in,
      q2_var = q_full_var1
    )
    unit_test(costs_k, 32.2996145651321, main_run_var = main_run_var1)
    
    return( list( "wage_0_in" = wage_0_in, "wage_t_in" = wage_t_in, "lambda1_in" = lambda1_in,
                  "lambda1_prev_in" = lambda1_prev_in,
                  "lambda2_in" = lambda2_in, "saturation_in" = saturation_in,
                  "lambda1_new_in" = lambda1_new_in, 
                  "lambda1_prev_new_in" = lambda1_prev_new_in,
                  "earnings_in_no_ext" = earnings_in_no_ext,
                  "earnings_in_no_ext_prev" = earnings_in_no_ext_prev,
                  "earnings_in_yes_ext" = earnings_in_yes_ext,
                  "earnings_in_yes_ext_prev" = earnings_in_yes_ext_prev, 
                  "earnings_in_no_ext_new" = earnings_in_no_ext_new,
                  "earnings_in_no_ext_prev_new" = earnings_in_no_ext_prev_new, 
                  "interest_in" = interest_in, "costs1_country" = costs_data,
                  "delta_ed_final_in" = delta_ed_final_in, "delta_ed_final_in_x" = delta_ed_final_in_x,
                  "cost_per_student_in" = cost_per_student_in, "s2_in" = s2_in,  
                  "pv_benef_tax_nx_in"= pv_benef_tax_nx_in, "pv_benef_tax_yx_in" = pv_benef_tax_yx_in,
                  "pv_benef_all_nx_in" = pv_benef_all_nx_in,
                  "pv_benef_all_nx_prev_in" = pv_benef_all_nx_prev_in,
                  "pv_benef_all_yx_in" =  pv_benef_all_yx_in, 
                  "pv_benef_all_yx_prev_in" = pv_benef_all_yx_prev_in, 
                  "pv_benef_tax_new" = pv_benef_tax_new,
                  "pv_benef_all_new" = pv_benef_all_new, 
                  "pv_benef_all_prev_new" = pv_benef_all_prev_new, 
                  "costs2_ea_in" = costs2_ea_in,
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k, "cost1_in" = cost1_in 
                                ) )
  }

invisible( list2env(one_run(),.GlobalEnv) )




## ----mc-setup, eval=TRUE---------------------------------------------------------------------------------------------------------------------------------------------------------
# EXPLAIN
# This function takes as inputs means and standard deviations of source 
# parameters and simualte draws of each source. When the source is a scalar, 
# it generates a draw from a noromal dist (mean, sd). When it is a "small"
# (less than 4 elements) vector, generates independent multivariate normals, 
# when its a large vector...

costs_data <- costs1_p1_f(df_costs_var = df_costs_so,
                          df_costs_cw_var = df_costs_cw_so,
                          df_counts_var = df_counts_so)

sim.data1 <- function(nsims = 1e2,
                      main_run_var2,
                      run_sim_var2,
                      wage_ag_var2,
                      wage_ag_var2_sd,
                      wage_ww_var2,
                      wage_ww_var2_sd,
                      profits_se_var2,
                      profits_se_var2_sd,
                      hours_se_cond_var2,
                      hours_se_cond_var2_sd,
                      hours_ag_var2,
                      hours_ag_var2_sd,
                      hours_ww_var2,
                      hours_ww_var2_sd,
                      hours_se_var2,
                      hours_se_var2_sd,
                      ex_rate_var2,
                      ex_rate_var2_sd,
                      growth_rate_var2,
                      growth_rate_var2_sd,
                      coef_exp_var2,         # sd for coef_exp is hard coded
                      lambda1_var2,
                      lambda1_var2_sd,
                      prevalence_0_var2,
                      prevalence_0_var2_sd,
                      prevalence_r_var2,
                      prevalence_r_var2_sd,
                      new_prev_r_var2,
                      lambda2_var2,
                      lambda2_var2_sd,
                      coverage_var2,
                      coverage_var2_sd,
                      q_full_var2,
                      q_full_var2_sd,
                      q_zero_var2,
                      q_zero_var2_sd,
                      lambda1_new_var2,
                      lambda1_new_var2_sd,
                      gov_bonds_var2,
                      gov_bonds_var2_sd,
                      inflation_var2,
                      inflation_var2_sd,
                      gov_bonds_new_var2,                                                              
                      gov_bonds_new_var2_sd,                                                          
                      inflation_new_var2,                          
                      inflation_new_var2_sd,                      
                      costs_par_var2,
                      costs_par_var2_sd,
                      staff_time_var2,
                      staff_time_var2_sd,
                      counts_par_var2,
                      counts_par_var2_sd,
                      delta_ed_var2,
                      delta_ed_var2_sd,
                      delta_ed_ext_var2,
                      delta_ed_ext_var2_sd,
                      teach_sal_var2,
                      teach_sal_var2_sd,
                      teach_ben_var2,
                      teach_ben_var2_sd,
                      teach_sal_new_var2,
                      teach_sal_new_var2_sd,
                      teach_ben_new_var2,
                      teach_ben_new_var2_sd,
                      n_students_var2,
                      n_students_var2_sd,
                      unit_cost_local_var2,
                      unit_cost_local_var2_sd,
                      unit_cost_local_new_var2,
                      unit_cost_local_new_var2_sd,
                      years_of_treat_var2,
                      years_of_treat_var2_sd,
                      tax_var2,
                      tax_var2_sd,
                      periods_var2, 
                      costs_data_var2 = costs_data, 
                      new_costs_var2, 
                      countries_var2
                      ) {
    start_time <- Sys.time()
    ################
    ###### Draws
    ################
    set.seed(142857)
    #draw_vals <- function(all_the_sos){}
    #Defaoult dist: normal, default sd: 0.1* mean
    ## Data
    gov_bonds_sim <-        rnorm(n = nsims, mean = gov_bonds_var2, 
                                  sd = gov_bonds_var2_sd)
    inflation_sim <-        rnorm(nsims, inflation_var2, 
                                  inflation_var2_sd)

    gov_bonds_new_sim <-    rnorm(n = nsims, mean = gov_bonds_new_var2, 
                                  sd = gov_bonds_new_var2_sd)
    inflation_new_sim <-    rnorm(nsims, inflation_new_var2, 
                                  inflation_new_var2_sd)                  

    wage_ag_sim <-          rnorm(nsims, wage_ag_var2, wage_ag_var2_sd)
    wage_ww_sim <-          rnorm(nsims, wage_ww_var2, wage_ww_var2_sd)
    profits_se_sim <-       rnorm(nsims, profits_se_var2, profits_se_var2_sd)
    hours_se_cond_sim <-    rnorm(nsims, hours_se_cond_var2, 
                                  hours_se_cond_var2_sd)
    hours_ag_sim <-         rnorm(nsims, hours_ag_var2, hours_ag_var2_sd)
    hours_ww_sim <-         rnorm(nsims, hours_ww_var2, hours_ww_var2_sd)
    hours_se_sim <-         rnorm(nsims, hours_se_var2, hours_se_var2_sd)
    coverage_sim <-         rnorm(nsims, coverage_var2, coverage_var2_sd)
    growth_rate_sim <-      rnorm(nsims, growth_rate_var2, growth_rate_var2_sd)

    ex_rate_sim <-          rnorm(nsims, ex_rate_var2, ex_rate_var2_sd)
    # ex_rate_new_sim
    tax_sim <-              rnorm(nsims, tax_var2, tax_var2_sd)

    unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_var2, 
                                  unit_cost_local_var2_sd)
    
    unit_cost_local_new_sim <-  rnorm(nsims, unit_cost_local_new_var2, 
                              unit_cost_local_new_var2_sd)
        

    years_of_treat_sim <-   rnorm(nsims, years_of_treat_var2, 
                                  years_of_treat_var2_sd)

    ## Research
    aux1 <- 0.1 * c(lambda1_var2[1], 0.01)
    # Each list is a pair mean, sd.
    aux2 <- lapply(1:2,function(x) c(lambda1_var2[x], aux1[x] ) )
    lambda1_sim <- sapply(aux2, 
                          function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )
    lambda2_sim <-          rnorm(nsims, lambda2_var2,  lambda2_var2_sd)
    # New lambdas here
    aux3 <- lapply(1:3,
                   function(x) c(lambda1_new_var2[x], 
                                 lambda1_new_var2_sd[x] ) )
    lambda1_new_sim <- sapply(aux3, 
                              function(x)  rnorm(nsims, 
                                                 mean = x[1], sd = x[2]) )

    q_full_sim <-           rnorm(nsims, q_full_var2, q_full_var2_sd)
    q_zero_sim <-           rnorm(nsims, q_zero_var2, q_zero_var2_sd)

    # Prevalence here TO DO: draw from a beta instead of "truncated" normal
    prevalence_0_sim <- rnorm(nsims, prevalence_0_var2, prevalence_0_var2_sd)
    prevalence_0_sim <- ifelse(prevalence_0_sim > 1, yes = 1, 
                          no = ifelse(prevalence_0_sim < 0, 0, prevalence_0_sim) )

     aux4 <- lapply(countries_var2,                      #will have trouble when selecting no countries
                   function(x) c(prevalence_r_so[x], 
                                 prevalence_r_so[x]) )
    
    # first draw samples of prevalence for each country
    prevalence_r_sim <- sapply(aux4, 
                              function(x)  rnorm(nsims, 
                                                 mean = x[1] * prevalence_r_var2,
                                                 sd = x[2] * prevalence_r_var2_sd) )
    prevalence_r_sim <- ifelse(prevalence_r_sim > 1, yes = 1, 
                          no = ifelse(prevalence_r_sim < 0, 0, prevalence_r_sim) )
    colnames(prevalence_r_sim) <- as.character(countries_var2)  
    
                                                  
    
    # if there is a new entry of prevalence, draw from it. If there is not
    # then leave as null
    if (!is.null(new_prev_r_var2)){
          new_prev_r_sim <- rnorm(nsims, new_prev_r_var2, new_prev_r_var2 * 0.1)
          new_prev_r_sim <- ifelse(new_prev_r_sim > 1, yes = 1, 
                          no = ifelse(new_prev_r_sim < 0, 0, new_prev_r_sim) )
    } else if (is.null(new_prev_r_var2)){
          new_prev_r_sim <- NULL
    }
    ## Guess work
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #periods until individual join the labor force
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], 
                                                    sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims, teach_sal_var2, teach_sal_var2_sd)
    teach_ben_sim <-    rnorm(nsims, teach_ben_var2, teach_ben_var2_sd)

    teach_sal_new_sim <-    rnorm(nsims, teach_sal_new_var2, 
                                  teach_sal_new_var2_sd)
    teach_ben_new_sim <-    rnorm(nsims, teach_ben_new_var2, 
                                  teach_ben_new_var2_sd)

    n_students_sim <-   rnorm(nsims, n_students_var2, n_students_var2_sd)
    # TO DO: modify to have a scalar multlying the series, and have that 
    # scalar being N(1,0.1)
    delta_ed_sim <- sapply(delta_ed_so[,1], 
                           function(x) rnorm(
                             nsims,
                             mean = x * delta_ed_var2,
                             sd = delta_ed_var2_sd * sd(delta_ed_so[, 1])) )
    colnames(delta_ed_sim) <- 1999:2007
    # modify to have a scalar multlying the series, and have that scalar 
    # being N(1,0.1)
    delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1],
                               function(x)  {
                                 rnorm(
                                   nsims,
                                   mean = x * delta_ed_ext_var2,
                                   sd =  sd(delta_ed_ext_so[, 1]) * 
                                     delta_ed_ext_var2_sd
                                 )
                                 }
                               )
    colnames(delta_ed_ext_sim) <- 1999:2007

    ######    
    ######    

    counts_in <- costs_data_var2$total
    costs_no_staff_in <- costs_data_var2$costs_by_country

    # drawing samples form counts
    costs1_counts_sim <- sapply(counts_in,
                                function(x)  rnorm(nsims, 
                                                   mean = x * counts_par_var2,  
                                                   sd = x * counts_par_var2_sd) 
                                )
    # drawing samples from costs
    costs1_all_costs_sim <- sapply(costs_no_staff_in,
                                function(x)  rnorm(nsims, 
                                                   mean = x * costs_par_var2,  
                                                   sd = x * costs_par_var2_sd) 
                                )
    

    # drawing samples from staff time
    staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      
    
    #computing unit cost for each simulation draw
    costs1_df_sim <- NULL
    
    for (aux1_i in 1:nsims){
      costs1_df_sim[[aux1_i]] <- data.frame(
        "Country" = costs_data_var2$Country,
        "total" = costs1_counts_sim[aux1_i,],
        "costs_by_country" = costs1_all_costs_sim[aux1_i,]
        )
    }

    ################
    ###### Runs    
    ################

    baird1_sim           <- rep(NA, nsims)
    baird2_sim           <- rep(NA, nsims)
    baird3_sim           <- rep(NA, nsims)
    baird4_sim           <- rep(NA, nsims)
    klps4_1_sim          <- rep(NA, nsims)
    klps4_2_sim          <- rep(NA, nsims)
    ea1_sim              <- rep(NA, nsims)
    ea2_sim              <- rep(NA, nsims)
    ea3_sim              <- rep(NA, nsims)
    cea_no_ext_ea_sim    <- rep(NA, nsims)
    rcea_no_ext_ea_sim   <- rep(NA, nsims)

    for (i in 1:nsims) {
    # one_run, for the most part, does not include standard deviations   
      invisible( list2env(
        one_run(main_run_var1 = FALSE,              # HERE I NEED TO PLUG costs1_costs_sim
                run_sim_var = TRUE,
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
                new_prev_r_var1 = new_prev_r_sim[i],
                lambda2_var1 = lambda2_sim[i],
                coverage_var1 = coverage_sim[i],
                q_full_var1 = q_full_sim[i],
                q_zero_var1 = q_zero_sim[i],
                lambda1_new_var1 = lambda1_new_sim[i,],
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
                years_of_treat_var1 = years_of_treat_sim[i],
                tax_var1 = tax_sim[i],
                periods_var1 = periods_so,
                df_costs_var1 = costs1_df_sim[[i]], 
                new_costs_var1 = new_costs_var2, 
                countries_var1 = countries_var2
                ),.GlobalEnv) ) # add costs here
      #Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
      baird1_sim[i] <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
      #Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
      baird2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
      # Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
      baird3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
      # Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
      baird4_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
      #KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
      klps4_1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs2_in)
      #KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
      klps4_2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_in)
      # EA1: no externality NPV using EAs costs
      ea1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_prev_in, costs_var = costs2_ea_in)
      # EA2: yes externality NPV using EAs costs
      ea2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_prev_in, costs_var = costs2_ea_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      ea3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_prev_new, costs_var = costs2_ea_in)
      #CEA for EA
      cea_no_ext_ea_sim[i]  <- CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                        costs_var = costs2_ea_in, fudging_var = 0)
      rcea_no_ext_ea_sim[i]  <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                                              costs_var = costs2_ea_in, fudging_var = 0),
                                           CEA_cash_var = 744 )
    }

    total_time <- Sys.time() - start_time

    return( list(
      "baird1_sim"        = baird1_sim,         
      "baird2_sim"        = baird2_sim,         
      "baird3_sim"        = baird3_sim,         
      "baird4_sim"        = baird4_sim,         
      "klps4_1_sim"        = klps4_1_sim,        
      "klps4_2_sim"        = klps4_2_sim,        
      "ea1_sim"            = ea1_sim,            
      "ea2_sim"            = ea2_sim,            
      "ea3_sim"            = ea3_sim,            
      "cea_no_ext_ea_sim"  = cea_no_ext_ea_sim,  
      "rcea_no_ext_ea_sim" = rcea_no_ext_ea_sim,
      "total_time"         = total_time
    ) )
}

policy_estimates <- c(
  "baird1_sim",
  "baird2_sim"         ,
  "baird3_sim"         ,
  "baird4_sim"         ,
  "klps4_1_sim"        ,
  "klps4_2_sim"        ,
  "ea1_sim"            ,
  "ea2_sim"            ,
  "ea3_sim"            ,
  "cea_no_ext_ea_sim"  ,
  "rcea_no_ext_ea_sim"
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
  "A3. All income of A2. Main Policy Estimate",
  "Main Policy Estimate. CEA format", 
  "Main Policy Estimate. RCEA format"
  )

# c(
#   "Fiscal effects, 2016(W@W) B & C, no ext",
#   "Fiscal effects, 2016(W@W) B & C, yes ext",  
#   "Total effects, 2016(W@W) B & C, no ext",  
#   "Total effects, 2016(W@W) B & C, yes ext",  
#   "Fiscal effects, 2019(KLPS4) B & 2016(W@W) C, no ext",   
#   "Total effects, 2019(KLPS4) B & 2016(W@W) C, no ext",  
#   "Total effects, 2016(W@W) B & EA C, no ext",  
#   "Total effects, 2016(W@W) B & EA C, ext",  
#   "Total effects, 2019(KLPS4) B & EA C, no ext",
#   "CEA for total effects, 2019(KLPS4) B & EA C, no ext",
#   "RCEA to cash for total effects, 2019(KLPS4) B & EA C, no ext")



