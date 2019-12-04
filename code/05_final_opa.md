---
title: "A Unifying Open Policy Analysis for Deworming"
date: "03 December, 2019"
output:
  html_document:
    code_folding: hide
    code_download: true
    collapsed: yes
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    toc: yes
    toc_depth: 3
    toc_float: yes
  pdf_document: default
  word_document: default
editor_options:
  chunk_output_type: console
pdf_document:
  extra_dependencies: ["xcolor"]
bibliography: bibliography.bib  
link-citations: true 
---
\def\blue{\color{blue}}
\def\red{\color{red}}







```r
################
#####  Notes:
################
### Source ---------->  Input ---------->  Model ---------->  Policy Estimates (output)
###  (_so)              (_in)              (_mo)                (_pe)
### values            functions          functions              values
###                   & values           & values             
# - call_sources_f- tax_elasticity_in_f  - tax_revenue_mo_f     - ten_year_revenue_pe
# - policy_f      - est_billionares_in_f - total_rev_mo_f       - ten_year_top_tax_pe
#                                        - ten_years_mo_f       - total_rev_pe
### arguments in functions should used "_var" and functions should "_f"


# DESCRIBE FUNCTIONS STRUCTURE
# - inputs: list
# - outputs: list
#### function:  
#sample_function_f <- function(){
##########################################
##########################################  
#
# here goes the content
#
##########################################
##########################################  
#    return( )                                  # A list with all (most?) the elements
#}                                              # generated inside the function
#invisible( list2env(sample_function_f(),.GlobalEnv) )
#
```



```r
chunk_params <- function(){
###############################################################################
###############################################################################  
    #############
    ##### Data  
    #############
    gov_bonds_so <- 	0.1185	     #Kenyan interest on sovereign debt - Central Bank of Kenya
    inflation_so <-  0.02          #Kenyan inflation rate - World Bank Development Indicators
    wage_ag_so <- 	11.84	         #Mean hourly wage rate (KSH) - Suri 2011
    wage_ww_so <- 	14.5850933     #Control group hourly wage, ww (cond >=10 hrs per week) - Table 4, Panel B
    profits_se_so <- 1766          #Control group monthly self-employed profits - Table 4, Panel A  FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
    hours_se_cond_so <- 38.1       #Control group weekly self-employed hours, conditional on hrs >0 - Table D13, Panel D
    hours_ag_so <- 8.3             #Control group hrs per week, agriculture - Table 4, Panel D
    hours_ww_so <- 6.9             #Control group hrs per week, working for wages - Table 4, Panel B
    hours_se_so <- 3.3             #Control group hrs per week, self-employment - Table 4, Panel A
    ex_rate_so <- 74               #Exchange Rate - Central Bank of Kenya 74 , 85
    growth_rate_so <- 1.52/100     #Per-capita GDP growth, 2002-2011 (accessed 1/29/13) -	World Bank - see notes
    coverage_so  <- 0.681333333    # (R) Fraction of treated primary school students within 6 km - from W@W - see note
    tax_so <- 0.16575              #ADD INFO!
    unit_cost_local_so <- 43.66    #Deworm the World
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
    #alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    df_alpha_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
    #alphas_df <- 1 # a table with country, year and infection rates
    
    # new geographies
    
    
    ############# 
    ##### Research
    ############# 
    df_research_so <- read_csv("rawdata/research/research_params.csv")   
    lambda1_so <- c(3.49, 0)            #Hrs per week increase for men and women CONFIRM
    lambda2_so <- 10.2                  #Externality effect (proportional) - Table 3, Panel B
    lambda1_new_so <- c(86.54642,   # avg treatment effect from klps2 (already adjusted for ppp and inflation) - w@w
                               82.99311,   # avg treatment effect from klps3 (already adjusted for ppp and inflation) - w@w 
                               85.44088)   # avg treatment effect from klps4 (already adjusted for ppp and inflation) - w@w
    lambda1_new_sd_so <- c(43, 83, 172)  # ADD SOURCE
    q_full_so <- 0.75              #Take up rates with full subsidy. From Miguel and Kremmer (2007)
    q_zero_so <- 0                 #Take up rates with zero subsidy. From Miguel and Kremmer (2007)
    delta_ed_so <- c(-0.00176350949079451, 0.00696052250263997, 0.0258570306763183,     # (Delta E) Additional direct seconday schooling increase (from Joan)
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
    alpha_0_so <- 1#0.77 #0.77
    alpha_r_so <- 1#0.15 #0.15
    #############
    ##### Guess work   
    #############
    periods_so <- 50               #Total number of periods to forecast wages
    time_to_jm_so <- 10            #Time from intial period until individual join the labor force
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)	- see notes(0.1019575, -0.0010413), (0,0)
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    n_students_so <- 45            #Average pupils per teacher	45
    staff_time_so <- 0.3           #Added Deworming costs due to goverment staff time
    run_sim_so <- FALSE
    main_run_so <- TRUE
    rescale_so <- TRUE
    costs_par_so <- 1
    costs_par_sd_so <- 0.1
    counts_par_so <- 1
    counts_par_sd_so <- 0.1
    nsims_so <- 1e2
    # options: "baird1_sim","baird2_sim","baird3_sim", "baird4_sim", "klps4_1_sim",
    # "klps4_2_sim", "ea1_sim", "ea2_sim", "ea3_sim", "cea_no_ext_ea_sim", "rcea_no_ext_ea_sim"
    policy_estimate_so <- "ea3_sim"
    
    
    
    
    
    # TEMP

    ex_rate_2018        <-101.30    # Exchange rate (KES per international $) - https://data.worldbank.org/indicator/PA.NUS.FCRF?locations=KE
    ex_rate_2018_ppp_so <- 50.058   # KLPS4_E+_globals.do (originally from the World Bank)
    ex_rate_2017_ppp_so <- 49.773   # KLPS4_E+_globals.do (originally from the World Bank)

    
    cpi_2018_so <- 251.10           # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    cpi_2017_so <- 245.120          # KLPS4_E+_globals.do (originally from the Bureau of Labor Statistics)
    
    unit_cost_so <- 0.42                   # Unit cost of deworming (in 2018 USD) - from Evidence Action
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so

    
    teach_sal_new_so  <- 50000                 #Monthly secondary schooling compensation	(in 2017 KES) overestimated to account for benefits - news sources
                                              # https://www.tuko.co.ke/287766-secondary-school-teachers-salary-kenya.html
                                              # https://www.standardmedia.co.ke/article/2001249581/windfall-for-teachers-as-tsc-releases-new-salaries
    teach_sal_new_so <- 12*teach_sal_new_so 
    teach_sal_ppp_so <- teach_sal_new_so/ex_rate_2017_ppp_so
  # Adjust for inflation: convert all costs to 2017 USD
    
    unit_cost_2017usdppp_so <- unit_cost_ppp_so*cpi_2017_so/cpi_2018_so
    teach_sal_2017usdppp_so <- teach_sal_ppp_so*cpi_2017_so/cpi_2017_so # redundant, but for the sake of consistency

    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so
    teach_sal_ppp_so <- teach_sal_new_so/ex_rate_2017_ppp_so

    # Fix teach_sal_so       
    return( sapply( ls(pattern= "_so\\b"), function(x) get(x)) )
###############################################################################
###############################################################################    
}
invisible( list2env(chunk_params(),.GlobalEnv) )

#############
##### Notes:
#############
### Source ---->  Input ----> Model ----> Policy Estimates (output)
###  (_so)        (_in)       (_mo)        (_pe)
### values      functions   functions      values
###             & values    & values
### arguments in functions should used "_var" and functions should "_f"

#invisible( list2env(chunk_name(),.GlobalEnv) )

# on growth_rate_so: (http://data.worldbank.org/indicator/NY.GDP.PCAP.KD/), see calculation on "Kenya GDP per capita" tab. In W@W this equals 1.52%. ISSUE: This growth number should be updated to be 2002-2014, I think.
# on coef_exp_so: 1998/1999 Kenyan labor force survey; regression of earnings on age, age^2, female dummy, indicators for attained primary/secondary/beyond, and province dummies. Estimate used in W@W: (0.1019575, -0.0010413). ISSUE: For now assume no further life cycle adjustment beyond KLPS-3 (likely a conservative assumption).
# coverage_so: Overall Saturation (0.511) / 0.75 - not reported in table, average of T & C
```


# Introduction  

Presents and compares different approaches to quantify the costs and benefits of deworming. Mentioned the GW analysis.

Mass deworming has demonstrated to be a highly effective public health intervention in the past. Here we provide a policy analysis that compares benefits and costs of deworming for different potential new settings. The goal of this analysis is to provide the best empirical information for policy makers debating the implemention of a deworming policy. This document describes all the analytical steps required to reproduce the analysis, and displaying the actual computer code use in each step. In addition to this report, the reader can find all the materials to reproduce the findings presented here in [github.org/bitss/opa-deworming](https://github.org/bitss/opa-deworming). The main output, presented in the [results section](#policy-estimate) of this report, can also be explored interactively for different assumptions. 

The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:   
  1 - the original CBA produced by @baird2016worms,   
  2 - an updated version of such analysis by a symilar research team [@klps4], and   
  3 - a third approach that borrows some component of the previous two and a some specific components requested by the NGO Evidence Action (EA)[^1]. 

# Methodology  

We first describe the common elements across all three aproaches, and then describe each approach in detail.

## Common structure 

The starting point is a comparison of a stream of benefits and costs over the lifetime of the recepients of deworming. The final policy estimate is the discounted sum of all costs and benefits, known as the Net Present Value (NPV). 

\begin{equation}
NPV = \sum_{t = 0}^{T}\frac{1}{(1 + r)^t}\left( B_{t} - C_{t} \right)
\label{eq:1}
\tag{1}
\end{equation}




```r
# - inputs: total per capita benefits, total per capita costs, fudging factor
# - outputs: Cost-effectiveness ratio & ratio to cash CEA
chunk_policy_est <- function(){
###############################################################################
###############################################################################  

    CEA_pe_f <- function(benefits_var = 1, fudging_var = 0, costs_var = 1) {
        ( benefits_var * ( 1 + fudging_var ) ) / costs_var
    }
    RCEA_pe_f <- function(CEA_var = 1, CEA_cash_var = 1){
        CEA_var / CEA_cash_var
    }

    NPV_pe_f <- function(benefits_var = 1, costs_var = 1){
        benefits_var - costs_var
    }

###############################################################################
###############################################################################  
    return(list("CEA_pe_f" = CEA_pe_f,
                "RCEA_pe_f" = RCEA_pe_f,
                "NPV_pe_f" = NPV_pe_f))
}
invisible( list2env(chunk_policy_est(),.GlobalEnv) )
```


At a high level all three approaches focus on the same type of benefits: the increase in incomes over the lifetime of beneficiaries of deworming. This is probalby an under-estimate of the benefits as it does not quantify the non-pecuniary effects of improved health.  The costs can be separated into direct costs of implemening deworming policies, and indirect costs associated with the benefits of deworming.

The main differences across the three aproaches regarding benfits have to do how to predict the earnings profiles over a lifecycle, and wheather or not to account for different prevalence rates. Approach 1 and 2 use different earning profiles, and approach 3 combines both earning profiles and adjust for possible differences in prevalence rates of worm infections. 

The main differences in costs have to do with weather indirect costs are included, and what is the relevant unit cost for the analysis. The first two approaches include indirect costs and use the unit costs of a specific country (Kenya) while the third approach does not includes indirect costs and use unit costs of multiple countries. 


## Approach 1: @baird2016worms



### Benefits ("$B$")  

Bair et al. (2016) compute benefits like this:

\begin{equation}
B =   \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} E_{t}
\label{eq:5}
\tag{5}
\end{equation}

**Note:** The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender.



Where:   

 - $r$: is the discount rate  
 - $w_t$: are the earnings in period $t$.   
 - $\lambda_{1}$: is the direct effects of deworming on earnings.  
 - $\lambda_{2}$: is the indirect effects of deworming on earnings.   
 - $p$: saturation, measures the fraction of the population that is effectively usign the treatment.  
 - $R$: coverage, defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group.  



```r
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
```

#### "$r$"  

The real interest rate $r$ is obtained from the interest rate on goverment bonds (0.118) minus the inflation rate (0.02).


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
chunk_interest <- function(){
###############################################################################
###############################################################################  

    interest_f <- function(gov_bonds_var = gov_bonds_so , inflation_var = inflation_so) {  
        interest_in = gov_bonds_var - inflation_var
        return(list("interest_in" = interest_in))
    }

###############################################################################
###############################################################################  
    return(list("interest_f" = interest_f))
}

invisible( list2env(chunk_interest(),.GlobalEnv) )
interest <- as.numeric( interest_f() )
```


The resulting value is a $r$ = 9.85%


#### Earnings
Where $E_t$ represents earnings in period $t$. That can be computed in two ways.


##### numer one

\begin{equation}
E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)
\end{equation}


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings1_f <- function(wage_var = wage_in,
                          lambda1_var = lambda1_so,
                          lambda2_var = lambda2_so,
                          saturation_var = saturation, coverage_var) {  
        res1 <- wage_var * ( lambda1_var + saturation_var * lambda2_var / coverage_var )
        return(res1)
    }

###############################################################################
###############################################################################  
    return(list("earnings1_f" = earnings1_f))
}

invisible( list2env(chunk_earnings1(),.GlobalEnv) )
```

###### "$w_{t}$"

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50
\end{equation}

individuals in the data are assumed to enter the labor force 10 years after the (data) present day ($w_t = 0$ for $t<10$). Wage at time $t$ is the weekly starting wage in USD ($w_0$) that has a base growth rate equal to the per capita GDP growth ($g$) applied to however many years of work ($Xp$). In addition to this growth, the salaries are adjusted to represent a (concave) wage life cycle profile ($1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2$).

###### "$w_0$"

\begin{equation}
w_t =  \text{#weeks} \times \blue{w_0} (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l} \\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}  
\end{equation}

The initial wage in dollars ($w_{0}$) is a weighted average of wages for control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the average number of hours in each sector ($h_l$) relative to the sum of the average number of hours in each sector.

The wage in agriculture comes from research (Suri, 2011), the working wage comes from the data and its defined as  hourly wage for the control group for those who reported more than 10 hrs of work per week. The self-employed wage ($w_{se}$) was constructed as follows:

\begin{equation}
w_{se} =  \frac{ \text{Monthly self-employed profits} }{4.5 \times E[h_{se}|h_{se}>0] }
\end{equation}

Where both parameters (Monthly self-employed profits and self-employed hours for the control group, conditional on hrs >0 - $E[h_{se}|h_{se}>0]$ -) come from the data (ww paper).  The measure of hours in self employment used to compute wages is ($E[h_{se}|h_{se}>0]$) is different from the one is to compute the weights $\alpha_l$ above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of workin age in the sample (hence capturing the relative inportance of the self employed sector in the economy)



```r
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_mo) and function (wage_0_mo_f), Wage trayectory:
#  value (wage_t_mo) and function (wage_t_mo_f).
chunk_wages <- function(){
################################################################################
################################################################################  
    #close to value from spreadsheet (Assumps&Panel A Calcs!B137 = 0.1481084),
    #but I suspect diff due to computational precision

    wage_0_mo_f <- function(wage_ag_var, wage_ww_var, profits_se_var, hours_se_cond_var,
                            hours_ag_var, hours_ww_var, hours_se_var, ex_rate_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        wage_se <- profits_se_var / (4.5 * hours_se_cond_var)
        wage_ls <- c(wage_ag_var, wage_ww_var, wage_se)
        alpha_ls <- c(hours_ag_var, hours_ww_var, hours_se_var) / sum( c(hours_ag_var, hours_ww_var, hours_se_var) )
        res1 <- 1/ex_rate_var * sum( wage_ls * alpha_ls )
        return(res1)
    }

    wage_t_mo_f <- function(wage_0_var,
                       growth_rate_var,
                       coef_exp1_var,
                       coef_exp2_var) {
        experience_aux <- 0:periods_so - time_to_jm_so
        res1 <- 52 * wage_0_var *( ( 1 + growth_rate_var )^experience_aux ) *
          ( 1 + coef_exp1_var * experience_aux + coef_exp2_var * (experience_aux^2) ) *
          ifelse(0:periods_so >= time_to_jm_so, 1, 0)
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
```



### Costs

\begin{equation}
C =  K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{\gamma t}(S1,S2) + \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)
\end{equation}


```r
# - inputs:
# - outputs:
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

    cost2_f <- function(periods_var = periods_so, delta_ed_var = delta_ed_final_in,
               interest_r_var = interest, cost_of_schooling_var = cost_per_student_in,
               s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in) {
        index_t <- 0:periods_var
        delta_ed_s <- c(0, delta_ed_var, rep(0,41))
        sum( ( 1 / (1 + interest_r_var) )^index_t *
                delta_ed_s * cost_of_schooling_var) +
        (s2_var * q2_var  - s1_var * q1_var)
      }

###############################################################################
###############################################################################  
    return(list("cost2_f" = cost2_f))    # Try to return only functions
}
invisible( list2env(chunk_cost2(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:  
```


#### $K$ and $\Delta \overline{E}_{\gamma t}(S1,S2)$

$K$ represents the cost per student. This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}
\end{equation}

For $\Delta \overline{E}_{\gamma t}(S1,S2)$ we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from [need to define the source "from Joan" in `Assumps&Panel A Calcs!A93`].

This series does not take into account the externality effects. To incorporate it we need another series (same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


```r
# - inputs:
# - outputs:
chunk_edcosts <- function(){
###############################################################################
###############################################################################    

    cost_per_student_f <- function(teach_sal_var = teach_sal_so,
                                    teach_ben_var = teach_ben_so,
                                    n_students_var = n_students_so) {
        (teach_sal_var + teach_ben_var) / n_students_var
    }

    delta_ed_final_f <- function(include_ext_var = include_ext_so, delta_ed_var = delta_ed_so,
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
```

**Note:** need to understand better the date of each component (of the model, not only this section).

#### $\left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right)$

##### $S_{1}Q(S_{1}) = 0$
There is no subsidy for deworming under the status quo.   


##### $S_{2}$: complete subsidy to per capita costs of deworming.

With complete subsidy, $S_2$ represents the total direct costs of deworming in USD. Calculated as follows

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\
\end{equation}

##### $Q_{2}$
The take-up with full subsidy ($Q_2$) comes from a previous study (Miguel and Kremer 2007) and takes the value of 0.75.


```r
# - inputs:
# - outputs:
chunk_unit_costs2 <- function(){
###############################################################################
###############################################################################  

    s2_f <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so, years_of_treat_var = years_of_treat_so) {
      ( unit_cost_local_var / ex_rate_var ) * years_of_treat_var
    }

###############################################################################
###############################################################################  
    return(list("s2_f" = s2_f) )
}
invisible( list2env(chunk_unit_costs2(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:
```


- Describe.AQUI VOY


- Distinguish between total, fiscal. With and without externalities. 

Without externalities, they obtain total NPV of benefits of 142.43, with 12.9 in tax revenue for goverment (table 5, column 3, and rows 9, 10 respectively). 

Including externalities, they obtain total NPV of benefits of 766.81, with 102.97 in tax revenue for goverment (table 5, column 3, and rows 12, 13 respectively). 





```r
# 142.43 - 10.71 - 1.07                                   = 130.65
# (142.43 * 0.16575 - 10.71 = 12.89777) - 1.07            = 11.82777
# 766.81 - 10.71 - 13.42 - 1.07                           = 741.61
# (766.81 * 0.16575 - 10.71 - 13.42 = 102.9688) - 1.07    = 101.8988
```


-----------------


## Approach 2: @klps4 

### Welfare gains ($\Delta W_t$)





$\Delta W_t$ represents the treatment effect on welfare, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc.

We estimate treatment effects on total welfare by round. KLPS2 captures effects after 10 years; KLPS3 captures the effects after 15 years; and KLPS4 after 20 years. We will need to make assumptions about welfare gains from deworming after 20 years

#### Treatment effect timeframe

If we assume welfare gains persist for 5 years after KLPS4 (matching the time period between previous rounds), then disappear, we have

\begin{equation}
\Delta W_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(20 < t \leq 25)\alpha^{KLPS4}
\text{ for } t \leq 50
\end{equation}

However, if we assume that the effect on welfare identified 20 years after the intervention persists through one's working life, we have 

\begin{equation}
\Delta W_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t > 20)\alpha^{KLPS4}
\text{ for } t \leq 50
\end{equation}


Note that both expressions assume that there are no additional earnings gains for the treatment group for the first 10 years post-intervention. This model also disregards externality effects.

Using earnings gains to measure welfare, we substitute each $\alpha$ term with the average treatment effect on earnings in each round of data collection: 87, 83, 85 dollars per person per year.



```r
# - inputs: periods_so, lambda1_2017usdppp_so
# - outputs: 
chunk_delta_earnings <- function(){
############################################################################### 
############################################################################### 
    delta_welfare_in_f <- function(t_var = 0:periods_so, 
                                   welfarek1_var, 
                                   welfarek2_var, 
                                   welfarek3_var) {
      delta_welfare_in <- 1*(10 <= t_var & t_var < 15) * welfarek1_var + 
                            1*(15 <= t_var & t_var < 20) * welfarek2_var + 
                            1*(20 <= t_var & t_var < 25) * welfarek3_var
      return(delta_welfare_in)
    }
    
    delta_welfare_p_in_f <- function(t_var = 0:periods_so, 
                                      welfarek1_var, 
                                      welfarek2_var, 
                                      welfarek3_var) {
      delta_welfare_p_in <- 1*(10 <= t_var & t_var < 15) * welfarek1_var + 
                          1*(15 <= t_var & t_var < 20) * welfarek2_var + 
                          1*(20 <= t_var) * welfarek3_var
      return(delta_welfare_p_in)
    }
############################################################################### 
############################################################################### 
    return(list("delta_welfare_in_f" = delta_welfare_in_f, 
                "delta_welfare_p_in_f" = delta_welfare_p_in_f))
}

invisible( list2env(chunk_delta_earnings(),.GlobalEnv) )
```



```r
delta_earnings_in = delta_welfare_in_f(welfarek1_var = lambda1_new_so[1],
                                       welfarek2_var = lambda1_new_so[2],
                                       welfarek3_var = lambda1_new_so[3])

delta_earnings_p_in = delta_welfare_p_in_f(welfarek1_var = lambda1_new_so[1],
                                           welfarek2_var = lambda1_new_so[2],
                                           welfarek3_var = lambda1_new_so[3])
```


### Costs 
#### Cost of deworming medication

The costs of deworming medication is obtained by the sum of discounted costs of deworming over the treatment period. The average treatment period in this study was 2.41 years, which we round to 2.4. So starting year zero, we have

\begin{equation}
\sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]
\end{equation}

Since the analysis is discrete, and we can not sum over a non-integer, we find
\begin{equation}
\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + .4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]
\end{equation}

##### Current subsidy for deworming ($S_{1}Q(S_{1})$)
Since there is no subsidy for deworming under the status quo, we have $S_{1}Q(S_{1}) =0$.


##### Complete subsidy for deworming ($S_2Q(S_2)$)

With complete subsidy, $S_2$ represents the total direct costs of deworming each child in USD. Most recent (2018) data from Evidence Action reveals this cost to be $0.42. Adjusting for purchasing power and inflation, we get a per capita cost of $0.83.

The take-up with full subsidy ($Q_2$) comes from a previous study (Miguel and Kremer 2007) and takes the value of 0.75.


```r
# - inputs: 
# - outputs: 
q2_in <- q_full_so
# - inputs:
# - outputs:
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

    s2_f_new <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     interest_var = interest) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      sum(( unit_cost * (1 + interest_var)^(-(0:2)) ) * c(1,1,0.4))
    }

###############################################################################
###############################################################################  
    return(list("s2_f_new" = s2_f_new) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:

s2_in <- s2_f_new(interest_var = 0.05, unit_cost_local_var = 0.8296927, ex_rate_var = 1)
s2_new <- s2_in
```

**So we get an average cost of deworming each child over the entire treatment period, $1.44.**

#### Cost of schooling

We account for the cost of schooling since deworming medication increases school attendance and may put pressure on educational institutions. Schooling costs are given by the discounted sum of the additional cost of education per child as a result of deworming.

The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school as a result of deworming. Assuming pressure is added to educational institutions for a maximum of nine years, starting at year zero, we have

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)
\end{equation}

##### Cost per student ($K$)

$K$ represents the cost of schooling each child for an additional year ($267.88). It is calculated by dividing an estimate of annual teacher salary by the number of average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary}}{\text{number students}}
\end{equation}

Annual teacher salary ($12055) is based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FILL IN]**.

##### Additional years of education ($\Delta \overline{E}_t(S1,S2)$)

For $\Delta \overline{E}(S1,S2)$ we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from [need to define the source "from Joan" in `Assumps&Panel A Calcs!A93`].

This series does not take into account the externality effects. To incorporate the we need another series (same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


```r
# - inputs: coverage_so, q_full_so, q_zero_so 
# - outputs: saturation_in 
# ed_costs_in_f <- function(teach_sal_var = teach_sal_2017usdppp_so, 
#                           n_students_var = n_students_so,
#                           delta_ed_var = delta_ed_so[,1]){
#  ###############################################################################    
#     cost_per_student_in <- (teach_sal_var)/ n_students_var
#     delta_ed_in <- delta_ed_var
# ############################################################################### 
#     return(list("cost_per_student_in" = cost_per_student_in, "delta_ed_in" = delta_ed_in)) 
# } 
# invisible( list2env(ed_costs_in_f(),.GlobalEnv) )

delta_ed_in <- delta_ed_so[,1]
cost_per_student_in <- cost_per_student_f(teach_sal_var = (50000*12/49.77), teach_ben_var = 0, n_students_var = 45)
```

Over this nine year period, students attended school for an additional 0.15 years on average.

**Then we get an average cost of additional schooling per child over the nine-year period, $32.40.**




```r
# add suffix _var to args 
# - inputs: tax_rev_init_mo, top_tax_base_in  
# - outputs: total_rev_pe 
# npv_mo_f <- function(interest_r_var = interest_in,
#                 n_male_var = 1/2, n_female_var = 1/2, 
#                 delta_welfare_var,
#                 tax_var = tax_so,
#                 cost_of_schooling_var = cost_per_student_in,
#                 delta_ed_male_var = delta_ed_so[,1],
#                 delta_ed_female_var = delta_ed_so[,1],
#                 s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in,
#                 periods_var = periods_so, years_of_treat_var = years_of_treat_so) {
#   ns <- c(n_male_var, n_female_var)
#   l_index_t <- 0:periods_var
#   delta_ed_s <- cbind(delta_ed_male_var, delta_ed_female_var) 
#   delta_ed_s <- rbind(c(0,0), delta_ed_s, matrix(0,41, 2) )
# ############################################################################### 
#   benef <- matrix(NA, 51,2)
#   for (i in 1:2){
#   benef[,i] <- ( 1 / (1 + interest_r_var) )^l_index_t * delta_welfare_var
#   }
# 
#   res1 <- sum( ns * ( tax_var * apply(benef, 2, sum) -
#           apply( ( 1 / (1 + interest_r_var) )^l_index_t *
#                      delta_ed_s * cost_of_schooling_var, 2, sum) )) - 
#            (s2_var * q2_var  - s1_var * q1_var)
# ############################################################################### 
#   return(res1) 
# }

interest_in <- interest


# # Net Present Value (2017 USD PPP)
# npv_int05_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, tax_var = 1)
# npv_int05_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, tax_var = 1)
# npv_int10_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10, tax_var = 1)
# npv_int10_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10, tax_var = 1)
# 
# # Net Present Value of tax revenue (2017 USD PPP)
# tax_int05_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = .05)
# tax_int05_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = .05)
# tax_int10_persist <- npv_mo_f(delta_welfare_var = delta_earnings_p_in, interest_r_var = 0.10)
# tax_int10_die     <- npv_mo_f(delta_welfare_var = delta_earnings_in, interest_r_var = 0.10)
# 
# npv_mo_f(delta_welfare_var = delta_earnings_p_in, tax_var = 1, interest_r_var = 0.05) 
```



- Also distinguish between total and fiscal results. 

- Talk about total effects on earnings. Why not more externalities. New earnings profile. Describe diff between earnings and wages. 


-------------------




## Approach 3: Evidence Action

- Key elements: 
  - No costs on ed. 
  - Country specific costs. 
  - Prevalence.
  - Benefits from either Baird or KLPS4. 
  - Different format of output (CEA, and RCEA)


# Results  

# Main policy estimate {#policy-estimate}

# OLDKey policy estimates for policy makers  

The key policy estimate consists of a cost effectiveness analysis that compares the present
value of benefits and costs. The benefits quantified here are the effects on wages an the
costs are those of delivering the deworming treatment.  







The benefits will account for the direct effects of deworming and plus the indirect effects of deworming due to smaller pool of sick people in the community (herd inmunity). Effects are computed as a change in the earning profile of the population.


# Methodology {#methods}

This analaysis contains elements from GiveWell's cost effectiveness analaysis (see [here](https://docs.google.com/spreadsheets/d/1McptF0GVGv-QBlhWx_IoNVstWvt1z-RwVSu16ciypgs/edit#gid=1537947274), an editable version can be found [here](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1537947274))  and the cost benefit analysis described in [Baird et al., 2016](https://academic.oup.com/qje/article/131/4/1637/2468871).  

## Main Equation (the model)

The key result for policy makers is defined as the cost effectivness ratio (cell [`Deworming!B32`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B32)).

\begin{equation}
CEA_{deworming} = \frac{B (1 + \blue{F_{0}})}{C}
\end{equation}

 - $C$ is the costs per person dewormed (`F2, 4,B23` --> [`F1, 2, H16`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=H16)).     
 - $B$ is the benefits per person dewormed (`F2, 4,B22`).
 - $\blue{F_{0}}$ is a factor to account for leverage/fudging [not reviewed in this excercise] ([`F2, 6, D259`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1611790402&range=D259))


Also this quantity could be expressed in relative terms to the benchmark of cash transfers (cell [`Results!B9`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1034883018&range=B9)):

\begin{equation}
RCEA = \frac{CEA_{deworming}}{CEA_{cash}}
\end{equation}



## Sub-components:

We begin by describing the underlying analysis behind the costs. Through this excercise we use the following notation the letters $F, P, Q$ denote components
in percentages, monetary units (US dollars and local currency) and quantities respectively. Each new element will be tracked using a sub-index, and supra-indecis will be
used to track groups, like geographies, time, and other catergories. For example $Q^{i}_{2}$ represents the second quantity described in this analysis (total adjusted number childred dewormed per year) in location $i$. At the end of each description we will show in parenthesis the original location of the parameter in GiveWell's spreadsheets (using the notation `file, sheet number, cell`[^1]). When a parameter in an equation does not depend on any subsequent component, it is highlighted in bold.

### Costs ("$C$")

\begin{equation}
C = \sum_{i \in Countries } \omega_{i} c_{i}
\label{eq:2}
\tag{2}
\end{equation}

GiveWell estimates the cost per child dewormed in geographies where Evidence Action provides technical assistance.These costs include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such the cost of drugs donated by WHO.

Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The final cost is a weighted average of the unit cost across countries.

- $\omega_{i}$: Weight for the weighted average ([`F1, 2, C:G8`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=C8:G8)).  
- $c_{i}$: Total cost per child, per year in country $i$ (`F1, 2, C:G16`).  

Build $c_i$ as a function of three stakeholders: DtW, other donors, goverment.  
Each stakeholders spends on: line items.    
Incorporate currency.


```r
# - inputs: nothing
# - outputs: function that computs the weighted sum of country costs
chunk_cost1 <- function(){
###############################################################################
###############################################################################  

    costs1_f <- function(country_w_var = 1, country_cost_var = 1) {
        sum(country_w_var * country_cost_var)
    }

###############################################################################
###############################################################################  
    return(list("costs1_f" = costs1_f))
}


invisible( list2env(chunk_cost1(),.GlobalEnv) )
```



\begin{equation}
\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \\
c_{i} = \frac{C_{i}}{N_{i}} \\
\label{eq:3}
\tag{3}
\end{equation}


\begin{equation}
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \\
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}
\label{eq:4}
\tag{4}
\end{equation}




All costs are in USD.

GW original analysis weights each country to take into account the number of treatments provided as well as the proportion of costs incurred by DtWI in that geography. The analytical foundations for such weights are not clear. Also not clear why should only account for DtW costs.  


- $N_{i}$: Number of treated children in country $i$.  
- $Ex_{i}$: Exchange rate from country $i$ to USD.  
- $k$: Costs distribute across $k$ payers.   
- $l$: Each payers costs come from $l$ items.   



```r
# - inputs: nothing
# - outputs: function that computes the country weights used in the final costs
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
    costs1_costs_f <- function(df_costs_var = df_costs_so,
                               df_costs_cw_var = df_costs_cw_so,
                               staff_time_var = staff_time_so){
      # Add country
      df_costs_var <- df_costs_cw_var %>%
        right_join(df_costs_var, by = "Country/State") %>%
        select(-Country.y) %>% rename(Country = Country.x) %>%
        mutate(Country = tolower(Country))
      # values for last year with cost information
      df_costs_last <- df_costs_var %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_costs_var, by = "Country") %>%
        filter(Year == last_year)    

      # summing across payers and regions (last equation)
      costs_by_item_temp <- df_costs_last %>%
        filter(Payer != "Total") %>%
        group_by(Country, `Program Area`) %>%
        summarise("costs_by_region" = sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE))

      #sum across country/state and multiply by delta
      country_cost <- costs_by_item_temp %>%
        group_by(Country) %>%
        summarise("costs_by_country" = sum(costs_by_region) * (1 + staff_time_var))  

      return( list("cost_data" = country_cost) )
    }

    costs1_counts_f <- function(df_counts_var = df_counts_so,
                          df_costs_cw_var = df_costs_cw_so){
      # Add country
      df_counts_var <- df_costs_cw_var %>%
        right_join(df_counts_var, by = "Country/State") %>%
        mutate(Country = tolower(Country))

      # values for last year with cost information
      df_counts_last <- df_counts_var %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_counts_var, by = "Country") %>%
        filter(Year == last_year)    

      c_counts <- df_counts_last %>%
        group_by(Country, Year) %>%
        summarise("total" = sum(`# dewormed`))
        
      return( list("counts_data" = c_counts) )
    }

    costs1_ratios_in_f <- function(counts_var = costs1_counts_f()$counts_data,
                                   costs_var = costs1_costs_f()$cost_data){
      # create country weight
      c_weights <- counts_var %>% ungroup() %>%
        mutate(country_w = total / sum(total))

      # Compute the per capita cost for each country
      ratios_data <- costs_var %>%
         left_join(c_weights, by = "Country") %>%
         mutate("per_cap" = costs_by_country / total)

          return( list("ratios_data" = ratios_data) )
      }

###############################################################################
###############################################################################  
    return( list("costs1_costs_f" = costs1_costs_f,
                "costs1_counts_f" = costs1_counts_f,
                "costs1_ratios_in_f" = costs1_ratios_in_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )
```

#### Data required to compute costs.

$N_{i}, C_{i,k,l}, \delta_{g}$





##### Number two

\begin{equation}
E_t = \Delta Y_t = I(10 \leq t < 15) \lambda_{1}^{k1} + I(15 \leq t < 20) \lambda_{1}^{k2} + I(20 \leq t < 25) \lambda_{1}^{k3}  
\end{equation}

Where     

 - $I(10 \leq t < 15)$ represents ...  
 - $\lambda_{1}^{k1}$ represents ...  






```r
# - inputs:
# - outputs:
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
```



#### "$\lambda_{1}$"  and  "$\lambda_{2}$"

$\lambda_{1,\gamma}$ represents the estimated impact of deworming on hours of work for men a women. This two parameter are combined with a simple mean:

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female} \\
\lambda_{1,\gamma} = \alpha \lambda^{eff}_{1,\gamma} + (1 -  \alpha) \times 0
\end{equation}

Where:      

 - $\alpha$: represents the incidence of the condition.  
 - $\lambda_{1}^{eff}$: represents the effect of deworming over those affected with the condition.  
 - $\lambda_{2}^{eff}$: ?. **[discuss with Ted/Michael]**

**TO DO: add a section that discusses where are the $\alpha's$ comming from**   

```r
alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
df_alpha_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
```



In the original evaluation, $\alpha = 1$, hence $\lambda_{1}^{eff} = 1.75/0.92 = 1.94$. The value of $\lambda^{r}_{1}$ for each region $r$ will depend on that region's $\alpha^{r}$.  

Its components come from the W\@W paper.

$\lambda_{2,\gamma}$ the estimated externality effect (EXPLAIN) and comes from research (W\@W). Note that this parameter in not estimated by gender, so we repeat its value two times.


```r
# - inputs:
# - outputs:
chunk_lambdas<- function(){
###############################################################################
###############################################################################    

    lambda1_in_f <- function(lambda1_var = lambda1_so) {
        rep(0.5 * lambda1_var[1] + 0.5 *lambda1_var[2], 2)
    }

    lambda_r_f <- function(lambda1_var = lambda1_in_f(), alpha_0_var = alpha_0_so,
                         alpha_r_var=alpha_r_so){
        lambda1_eff_temp <- lambda1_var / alpha_0_var
        return( lambda1_eff_temp * alpha_r_var )
    }  

    lambda2_in_f <- function(lambda2_var = lambda2_so){
        rep(lambda2_var, 2)
    }

##############################################################################
###############################################################################  
    return(list("lambda_r_f" = lambda_r_f,     
                "lambda1_in_f" = lambda1_in_f,
                "lambda2_in_f" = lambda2_in_f ) )
}
invisible( list2env(chunk_lambdas(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_in <- lambda1_in_f()
lambda1_r_in <- lambda_r_f()
lambda2_in <- lambda2_in_f()
```


#### $R$ and $p$


The coverage, $R$, is defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group (last paragraph of page 9(1645) of paper). As the treatment was appplied to approximatedly two thirds of the population, $R$ is set to: $R  = 0.68$.  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively usign the treatment and is defined as:  

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)
\end{equation}

For this (or similar?) setting Miguel and Kremer 2007 [add page, table, col, row] estimate that there is almost no take-up without subsidy, hence $Q(0)$ is assinged the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy is $Q(full) = 0.75$.


```r
# - inputs: coverage_so, q_full_so, q_zero_so
# - outputs: saturation_in
chunk_coverage <- function(){
###############################################################################
###############################################################################  

    saturation_in_f <- function(coverage_var = coverage_so, q_full_var = q_full_so,
                                q_zero_var = q_zero_so){
        saturation_in <- coverage_so * q_full_so + ( 1 - coverage_so ) * q_zero_so
        return(list("saturation_in" = saturation_in))
    }

###############################################################################
###############################################################################  
    return(list("saturation_in_f" = saturation_in_f))    # Try to return only functions
}
invisible( list2env(chunk_coverage(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
```


# Main results


| Benfits                                   | Costs        | Name    |
|-------------------------------------------|--------------|---------|
| Baird w/tax and no externalities (no ext) | Baird no ext | Baird 1 |
| Baird w/t and ext                         | Baird ext    | Baird 2 |
| Baird all and no ext                      | Baird no ext | Baird 3 |
| Baird all and ext                         | Baird ext    | Baird 4 |
| KLPS4 w/t and no ext                      | Baird no ext | KLPS4_1 |
| KLPS4 all and no ext                      | Baird no ext | KLPS4_2 |
| Baird all and no ext                      | EA           | EA 1    |
| Baird all and ext                         | EA           | EA 2    |
| KLPS all and no ext                       | EA           | EA 3    |  




```r
# Function dependency is depicted as follows:
# f(g()) =
# f
# └──── g
#
#       ##     ###    ####
# 1     2       3     4
#       ##     ###    ####
# NPV_pe_f, CEA_pe_f, RCEA_pe_f
# ├──── pv_benef_f
# │      ├──── earnings1_f
# │      |      ├──── wage_t_mo_f
# │      |      |      └──── wage_0_mo_f
# |      |      ├──── lambda1_in_f
# │      |      |      └────lambda_r_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings2_f
# │      |      └────lambda_r_f
# │      └──── interest_f
# └──── cost1_f
# │      └──── costs1_ratios_in_f
# |             ├──── costs1_costs_f
# │             └──── costs1_counts_f
# └──── cost2_f = 11.63818
#        ├──── delta_ed_final_f
#        ├──── interest_f
#        ├──── s2_f
#        └──── cost_per_student_f
#       ##     ###    ####

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
            print(paste("Output has change at", deparse(substitute(to_test_var) ), " to ", text_val) )
        }
      }
}

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
           alpha_0_var1 = alpha_0_so,
           alpha_r_var1 = alpha_r_so,
           lambda2_var1 = lambda2_so,
           coverage_var1 = coverage_so,
           q_full_var1 = q_full_so,
           q_zero_var1 = q_zero_so,
           lambda1_new_var1 = lambda1_new_so,
           gov_bonds_var1 = gov_bonds_so,
           inflation_var1 = inflation_so,
           df_costs_var1 = df_costs_so,
           df_costs_cw_var1 = df_costs_cw_so,
           staff_time_var1 = staff_time_so,
           df_counts_var1 = df_counts_so,
           counts_sim_var1 = NULL,
           costs_sim_var1 = NULL,
           delta_ed_var1 = delta_ed_so,
           delta_ed_ext_var1 = delta_ed_ext_so,
           teach_sal_var1 = teach_sal_so,
           teach_ben_var1 = teach_ben_so,
           n_students_var1 = n_students_so,
           unit_cost_local_var1 = unit_cost_local_so,
           years_of_treat_var1 = years_of_treat_so,
           tax_var1 = tax_so,
           periods_var1 = periods_so) {
    ####------------ Inputs for wage_t ---------------------------------------------
    wage_0_in <- wage_0_mo_f(wage_ag_var = wage_ag_var1, wage_ww_var = wage_ww_var1,
                             profits_se_var = profits_se_var1, hours_se_cond_var = hours_se_cond_var1,  
                             hours_ag_var = hours_ag_var1, hours_ww_var = hours_ww_var1,
                             hours_se_var = hours_se_var1, ex_rate_var = ex_rate_var1)
    unit_test(wage_0_in, 0.1481084, main_run_var = main_run_var1)
    ###---------- Inputs for earnings1_f -------------------------------------------
    wage_t_in <- wage_t_mo_f(wage_0_var = wage_0_in, growth_rate_var = growth_rate_var1,
                             coef_exp1_var = coef_exp_var1, coef_exp2_var = coef_exp2_var1)

        lambda1_in <- lambda_r_f(lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
                             alpha_0_var = alpha_0_var1, alpha_r_var = alpha_r_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)

    saturation_in <- as.numeric(saturation_in_f(coverage_var = coverage_var1,
                                                q_full_var = q_full_var1,
                                                q_zero_var = q_zero_var1) )
    unit_test(wage_t_in, 4.572308, main_run_var = main_run_var1)
    # ADD UNIT TEST FOR SATURATION AN LAMBDAS    

    ###------------ Inputs for earnings2_f------------------------------------------
    lambda1_new_in <- lambda_r_f(lambda1_var = lambda1_new_var1,
                                 alpha_0_var = alpha_0_var1,
                                 alpha_r_var = alpha_r_var1)
    #ADD UNIT TEST FOR LAMBDAS    

    ##------------ Inputs for pv_benef_f -------------------------------------------
    # earnings1
    earnings_in_no_ext <- earnings1_f(wage_var = wage_t_in, lambda1_var = lambda1_in[1],
                                      lambda2_var = 0, saturation_var = saturation_in,
                                      coverage_var = coverage_var1)
    earnings_in_yes_ext <- earnings1_f(wage_var = wage_t_in, lambda1_var = lambda1_in[1],
                                       lambda2_var = lambda2_in[1], saturation_var = saturation_in,
                                       coverage_var = coverage_var1)

    # earnings2
    earnings_in_no_ext_new <- earnings2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1],
                                          lambda1k2_var = lambda1_new_in[2],
                                          lambda1k3_var = lambda1_new_in[3])
    # ADD UNIT TEST
    interest_in <- as.numeric( interest_f(gov_bonds_var = gov_bonds_var1,
                                          inflation_var = inflation_var1) )
    unit_test(earnings_in_no_ext, 7.978677, main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext, 42.95683, main_run_var = main_run_var1)
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    ###------------- Inputs for costs1_ratios_in_f----------------------------------
    costs1_counts_in <- costs1_counts_f(df_counts_var = df_counts_var1,
                                        df_costs_cw_var = df_costs_cw_var1)$counts_data
    costs1_costs_in <- costs1_costs_f(df_costs_var = df_costs_var1,
                                      df_costs_cw_var = df_costs_cw_var1,
                                      staff_time_var = staff_time_var1)$cost_data
    # ADD UNIT TEST
    # The following section only runs when running the simulations (later add: a skip to load the data only once)
    if (run_sim_var ==  TRUE) {
        costs1_counts_in$total <- counts_sim_var1
        costs1_costs_in$costs_by_country <- costs_sim_var1
    }
    ##-------------- Inputs for costs1_f--------------------------------------------
      costs1_country <-  costs1_ratios_in_f(counts_var = costs1_counts_in,
                                            costs_var =  costs1_costs_in)
      unit_test(unlist(costs1_country$ratios_data$costs_by_country),  
                6880801.84046596, main_run_var = main_run_var1)

    ##-------------- Inputs for costs2_f--------------------------------------------
    # Make explicit non-function inputs:
    delta_ed_final_in <- delta_ed_final_f(include_ext_var = FALSE,
                                          delta_ed_var = delta_ed_var1,
                                          delta_ed_ext_var = delta_ed_ext_var1)
    unit_test(delta_ed_final_in, 0.01134819, main_run_var = main_run_var1)

    delta_ed_final_in_x <- delta_ed_final_f(include_ext_var = TRUE,
                                            delta_ed_var = delta_ed_var1,
                                            delta_ed_ext_var = delta_ed_ext_var1)
    unit_test(delta_ed_final_in_x,  0.05911765, main_run_var = main_run_var1)

    interest_in <- as.numeric( interest_f(gov_bonds_var = gov_bonds_var1,
                                          inflation_var = inflation_var1) )
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)

    unit_test(cost_per_student_in,  116.8549, main_run_var = main_run_var1)
 
    s2_in <- s2_f(unit_cost_local_var = unit_cost_local_var1,
                  ex_rate_var = ex_rate_var1, years_of_treat_var = years_of_treat_var1)
    unit_test(s2_in, 1.237889, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f, CEA_pe_f and RCEA_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_nx_in, 11.02849, main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_yx_in, 59.37686, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_nx_in, 66.5368686659935, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_yx_in, 358.231450496853, main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new * tax_var1,
                                   interest_r_var = 0.05, periods_var = periods_var1)
    # ADD UNIT TEST
    # KLPS4 all and no ext
    pv_benef_all_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new,
                                   interest_r_var = 0.05, periods_var = periods_var1)
    # ADD UNIT TEST

    #Costs
    # costs1: EA costs no externalities
    cost1_in <- costs1_f(country_w_var = costs1_country$ratios_data$country_w,
                         country_cost_var = costs1_country$ratios_data$per_cap)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)
    # costs2: Baird no externalities
    costs2_in <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                         s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in, 11.63818, main_run_var = main_run_var1)

    # Baird yes externalities
    costs2_in_x <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in_x,
                           interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                           s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in_x,  25.05821, main_run_var = main_run_var1)

    # costs2: KLPS4
    costs_k <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = 0.05, cost_of_schooling_var = 267,
                         s1_var = 0, q1_var = 0, s2_var = s2_new, q2_var = q_full_var1)
    unit_test(costs_k, 11.63818, main_run_var = main_run_var1)
    
    return( list( "wage_0_in" = wage_0_in, "wage_t_in" = wage_t_in, "lambda1_in" = lambda1_in,
                  "lambda2_in" = lambda2_in, "saturation_in" = saturation_in,
                  "lambda1_new_in" = lambda1_new_in, "earnings_in_no_ext" = earnings_in_no_ext,
                  "earnings_in_yes_ext" = earnings_in_yes_ext,
                  "earnings_in_no_ext_new" = earnings_in_no_ext_new,
                  "interest_in" = interest_in, "costs1_counts_in" = costs1_counts_in,
                  "costs1_costs_in" = costs1_costs_in, "costs1_country" = costs1_country,
                  "delta_ed_final_in" = delta_ed_final_in, "delta_ed_final_in_x" = delta_ed_final_in_x,
                  "cost_per_student_in" = cost_per_student_in, "s2_in" = s2_in,  
                  "pv_benef_tax_nx_in"= pv_benef_tax_nx_in, "pv_benef_tax_yx_in" = pv_benef_tax_yx_in,
                  "pv_benef_all_nx_in" = pv_benef_all_nx_in,
                  "pv_benef_all_yx_in" =  pv_benef_all_yx_in, "pv_benef_tax_new" = pv_benef_tax_new,
                  "pv_benef_all_new" = pv_benef_all_new, "cost1_in" = cost1_in,
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k) )
  }

invisible( list2env(one_run(),.GlobalEnv) )
```

```
## [1] "Output has change at wage_0_in  to  0.170124466664436"
## [1] "Output has change at wage_t_in  to  17.8464946727946"
## [1] "Output has change at earnings_in_no_ext  to  31.1421332040266"
## [1] "Output has change at earnings_in_yes_ext  to  167.667817450905"
## [1] "Output has change at s2_in  to  1.4219"
## [1] "Output has change at pv_benef_tax_nx_in  to  23.6070893378784"
## [1] "Output has change at pv_benef_tax_yx_in  to  127.0994867217"
## [1] "Output has change at pv_benef_all_nx_in  to  142.42587835824"
## [1] "Output has change at pv_benef_all_yx_in  to  766.814399527604"
## [1] "Output has change at costs2_in  to  11.776188118988"
## [1] "Output has change at costs2_in_x  to  25.1962130559894"
## [1] "Output has change at costs_k  to  32.196059674958"
```




```r
#Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, -0.6096942)
```

```
## [1] "Output has change at baird1  to  11.8309012188904"
```

```r
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
baird2 <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
unit_test(baird2, 34.31866)
```

```
## [1] "Output has change at baird2  to  101.903273665711"
```

```r
# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
baird3 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test(baird3, 54.8986884881819)
```

```
## [1] "Output has change at baird3  to  130.649690239252"
```

```r
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
baird4 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
unit_test(baird4, 333.17324538204)
```

```
## [1] "Output has change at baird4  to  741.618186471615"
```

```r
#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1 <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs_k)
unit_test(klps4_1, 47.6017891133612)
```

```
## [1] "Output has change at klps4_1  to  125.305668466511"
```

```r
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs_k)
unit_test(klps4_2, 345.767366073607)
```

```
## [1] "Output has change at klps4_2  to  918.040610861811"
```

```r
# res_npv_no_ext_klps_eacosts <- NPV_pe_f(benefits_var = pv_benef_in_new, costs_var = cost1_in)
# unit_test(res_npv_no_ext_klps_eacosts, 59.15516)

# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = cost1_in)
unit_test(ea1, 66.4520618047856)
```

```
## [1] "Output has change at ea1  to  142.341071497033"
```

```r
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = cost1_in)
unit_test(ea2, 358.146643635645)
```

```
## [1] "Output has change at ea2  to  766.729592666396"
```

```r
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = cost1_in)
unit_test(ea3, 357.320739390211)
```

```
## [1] "Output has change at ea3  to  950.151863675561"
```

```r
#CEA for EA
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = cost1_in, fudging_var = 0)
unit_test(cea_no_ext_ea, 784.569405332587)
```

```
## [1] "Output has change at cea_no_ext_ea  to  1679.41457011498"
```

```r
rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = cost1_in, fudging_var = 0),
         CEA_cash_var = 744)
unit_test(rcea_no_ext_ea, 1.05452877060832)
```

```
## [1] "Output has change at rcea_no_ext_ea  to  2.257277648004"
```

```r
npv_table <- data.frame("no_ext" =  round( c(baird1, NA,
                                             NA), 1) ,
                        "yes_ext" = round( c(NA, baird2, NA), 1) ,
                        "no_ext_" = round( c(baird3, NA,
                                             ea1), 1) ,
                        "yes_ext_" = round( c(NA, baird4,
                                             ea2), 1) ,
                        "no_ext " = round( c(klps4_1, NA,
                                             NA), 1) ,
                        ".no_ext " = round( c(klps4_2, NA,
                                             ea3), 1) ,
                        row.names = c("no_ext", "yes_ext", "no_ext_"))

kable(npv_table, caption = "Caption of the table") %>%
  add_header_above(c(" ", "tax" = 2, "all" = 2, "tax" = 1, "all" = 1)) %>%
  add_header_above(c(" ", "Baird = EA" = 4, "KLPS4" = 2)) %>%
  add_header_above(c(" ", "Benefits" = 6)) %>%
  kable_styling("striped", full_width = F) %>%
  group_rows("Costs: Baird = KLPS4", 1, 2) %>%
  group_rows("Costs: EA", 3, 3)   # same result with group 1=4
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Caption of the table</caption>
 <thead>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="6"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Benefits</div></th>
</tr>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="4"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">Baird = EA</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">KLPS4</div></th>
</tr>
<tr>
<th style="border-bottom:hidden" colspan="1"></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">tax</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="2"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">all</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">tax</div></th>
<th style="border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px; ">all</div></th>
</tr>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> no_ext </th>
   <th style="text-align:right;"> yes_ext </th>
   <th style="text-align:right;"> no_ext_ </th>
   <th style="text-align:right;"> yes_ext_ </th>
   <th style="text-align:right;"> no_ext. </th>
   <th style="text-align:right;"> .no_ext. </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="2"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: Baird = KLPS4</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext </td>
   <td style="text-align:right;"> 11.8 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 130.6 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 125.3 </td>
   <td style="text-align:right;"> 918.0 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> yes_ext </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 101.9 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 741.6 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr grouplength="1"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: EA</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext_ </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 142.3 </td>
   <td style="text-align:right;"> 766.7 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 950.2 </td>
  </tr>
</tbody>
</table>


## Results for overall welfare (not only taxes)


- **NPV without externalities in Baird et al, 2016 ($\lambda_2 = 0$):** 130.6    

- **NPV with externalities in Baird et al, 2016 ($\lambda_2 = 10.2$ ):** 741.6

- **NPV without externalities in EA 2019 ($\lambda_2 = 0$):** 142.3    

- **NPV with externalities in EA 2019 ($\lambda_2 = 10.2$ ):** 766.7

- **NPV without externalities in KLPS 2019:** 918    

- **CEA without externalities in EA:** 1679.4    

- **RCEA without externalities in EA (relative to cash):** 2.3    


# Montecarlo simulations  


```r
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
                      alpha_0_var2,
                      alpha_0_var2_sd,
                      alpha_r_var2,
                      alpha_r_var2_sd,
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
                      n_students_var2,
                      n_students_var2_sd,
                      unit_cost_local_var2,
                      unit_cost_local_var2_sd,
                      years_of_treat_var2,
                      years_of_treat_var2_sd,
                      tax_var2,
                      tax_var2_sd,
                      periods_var2,
                      df_costs_var2 = df_costs_so,
                      df_counts_var2 = df_counts_so,
                      df_costs_cw_var2 = df_costs_cw_so) {
    start_time <- Sys.time()
    ################
    ###### Draws
    ################
    set.seed(142857)
    #draw_vals <- function(all_the_sos){}
    #Defaoult dist: normal, default sd: 0.1* mean
    ## Data
    gov_bonds_sim <-        rnorm(n = nsims, mean = gov_bonds_var2, sd = gov_bonds_var2_sd)
    inflation_sim <-        rnorm(nsims, inflation_var2, inflation_var2_sd)

    wage_ag_sim <-          rnorm(nsims, wage_ag_var2, wage_ag_var2_sd)
    wage_ww_sim <-          rnorm(nsims, wage_ww_var2, wage_ww_var2_sd)
    profits_se_sim <-       rnorm(nsims, profits_se_var2, profits_se_var2_sd)
    hours_se_cond_sim <-    rnorm(nsims, hours_se_cond_var2, hours_se_cond_var2_sd)
    hours_ag_sim <-         rnorm(nsims, hours_ag_var2, hours_ag_var2_sd)
    hours_ww_sim <-         rnorm(nsims, hours_ww_var2, hours_ww_var2_sd)
    hours_se_sim <-         rnorm(nsims, hours_se_var2, hours_se_var2_sd)
    coverage_sim <-         rnorm(nsims, coverage_var2, coverage_var2_sd)
    growth_rate_sim <-      rnorm(nsims, growth_rate_var2, growth_rate_var2_sd)

    ex_rate_sim <-          rnorm(nsims, ex_rate_var2, ex_rate_var2_sd)
    tax_sim <-              rnorm(nsims, tax_var2, tax_var2_sd)

    unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_var2, unit_cost_local_var2_sd)
    years_of_treat_sim <-   rnorm(nsims, years_of_treat_var2, years_of_treat_var2_sd)

    ## Research
    aux1 <- 0.1 * c(lambda1_var2[1], 0.01)
    # Each list is a pair mean, sd.
    aux2 <- lapply(1:2,function(x) c(lambda1_var2[x], aux1[x] ) )
    lambda1_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )
    lambda2_sim <-          rnorm(nsims, lambda2_var2,  lambda2_var2_sd)
    # New lambdas here
    aux3 <- lapply(1:3,function(x) c(lambda1_new_var2[x], lambda1_new_var2_sd[x] ) )
    lambda1_new_sim <- sapply(aux3, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )

    q_full_sim <-           rnorm(nsims, q_full_var2, q_full_var2_sd)
    q_zero_sim <-           rnorm(nsims, q_zero_var2, q_zero_var2_sd)

    # Prevalence here TO DO: draw from a beta instead of "truncated" normal
    alpha_0_sim <- rnorm(nsims, alpha_0_var2, alpha_0_var2_sd)
    alpha_0_sim <- ifelse(alpha_0_sim > 1, yes = 1, ifelse(alpha_0_sim < 0, 0, alpha_0_sim) )
    alpha_r_sim <- rnorm(nsims, alpha_r_var2, alpha_r_var2_sd)
    alpha_r_sim <- ifelse(alpha_r_sim > 1, yes = 1, ifelse(alpha_r_sim < 0, 0, alpha_r_sim) )

    ## Guess work
    periods_val <- 50           #Total number of periods to forecast wages
    time_to_jm_val <- 10        #Time from intial period until individual join the labor force
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims, teach_sal_var2, teach_sal_var2_sd)
    teach_ben_sim <-    rnorm(nsims, teach_ben_var2, teach_ben_var2_sd)
    n_students_sim <-   rnorm(nsims, n_students_var2, n_students_var2_sd)

    delta_ed_sim <- sapply(delta_ed_so[,1], function(x) rnorm(nsims, mean =
                                                                x * delta_ed_var2,
                                                              sd = delta_ed_var2_sd * sd(delta_ed_so[,1]) ) )
    colnames(delta_ed_sim) <- 1999:2007

    delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1],
                               function(x)  rnorm(nsims,
                                                  mean = x * delta_ed_ext_var2,
                                                  sd = delta_ed_ext_var2_sd * sd(delta_ed_ext_so[, 1])))
    # OLD (WORKING)
    #delta_ed_ext_sim <- sapply(delta_ed_ext_so[,1], function(x) rnorm(nsims,
    #                                                                   mean = x * 1,
    #                                                                   sd = 1 * sd(delta_ed_ext_so[,1])))

    colnames(delta_ed_ext_sim) <- 1999:2007
    #######    
    #######    
    costs1_counts_in <- costs1_counts_f(df_counts_var = df_counts_var2,
                                        df_costs_cw_var = df_costs_cw_var2)$counts_data
    costs1_counts_sim <- sapply(costs1_counts_in$total,
                                function(x)  rnorm(nsims, mean = x * counts_par_var2,  
                                                   sd = counts_par_var2_sd * x) )

    staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      

    costs1_costs_in <- lapply(staff_time_sim,
                              function(x) costs1_costs_f(df_costs_var = df_costs_var2,
                                                         df_costs_cw_var = df_costs_cw_var2,
                                                         staff_time_var = x)$cost_data)

    costs1_costs_sim <- t( sapply(costs1_costs_in, function(x)  {
        aux1 <- x$costs_by_country
        rnorm(length(aux1), mean = costs_par_var2 * aux1,  sd = costs_par_var2_sd * aux1)
    } )
    )

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
                alpha_0_var1 = alpha_0_sim[i],
                alpha_r_var1 = alpha_r_sim[i],
                lambda2_var1 = lambda2_sim[i],
                coverage_var1 = coverage_sim[i],
                q_full_var1 = q_full_sim[i],
                q_zero_var1 = q_zero_sim[i],
                lambda1_new_var1 = lambda1_new_sim[i,],
                gov_bonds_var1 = gov_bonds_sim[i],
                inflation_var1 = inflation_sim[i],
                staff_time_var1 = staff_time_sim[i],
                delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                teach_sal_var1 = teach_sal_sim[i],
                teach_ben_var1 = teach_ben_sim[i],
                n_students_var1 = n_students_sim[i],
                unit_cost_local_var1 = unit_cost_local_sim[i],
                years_of_treat_var1 = years_of_treat_sim[i],
                tax_var1 = tax_sim[i],
                periods_var1 = periods_so,
                df_costs_var1 = df_costs_var2,
                df_costs_cw_var1 = df_costs_cw_var2,
                df_counts_var1 = df_counts_var2,
                counts_sim_var1 = costs1_counts_sim[i,],
                costs_sim_var1 = costs1_costs_sim[i,]),.GlobalEnv) ) # add costs here
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
      ea1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = cost1_in)
      # EA2: yes externality NPV using EAs costs
      ea2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = cost1_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      ea3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = cost1_in)
      #CEA for EA
      cea_no_ext_ea_sim[i]  <- CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                        costs_var = cost1_in, fudging_var = 0)
      rcea_no_ext_ea_sim[i]  <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_nx_in,
                                                              costs_var = cost1_in, fudging_var = 0),
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



policy_estimates <- c("baird1_sim",          
"baird2_sim"         ,
"baird3_sim"         ,
"baird4_sim"         ,
"klps4_1_sim"        ,
"klps4_2_sim"        ,
"ea1_sim"            ,
"ea2_sim"            ,
"ea3_sim"            ,
"cea_no_ext_ea_sim"  ,
"rcea_no_ext_ea_sim" )

policy_estimates_text <- c(
  "Fiscal effects, 2016(W@W) B & C, no ext",
  "Fiscal effects, 2016(W@W) B & C, yes ext",  
  "Total effects, 2016(W@W) B & C, no ext",  
  "Total effects, 2016(W@W) B & C, yes ext",  
  "Fiscal effects, 2019(KLPS4) B & 2016(W@W) C, no ext",   
  "Total effects, 2019(KLPS4) B & 2016(W@W) C, no ext",  
  "Total effects, 2016(W@W) B & EA C, no ext",  
  "Total effects, 2016(W@W) B & EA C, ext",  
  "Total effects, 2019(KLPS4) B & EA C, no ext",
  "CEA for total effects, 2019(KLPS4) B & EA C, no ext",
  "RCEA to cash for total effects, 2019(KLPS4) B & EA C, no ext")
```



```r
npv_sim_all <-   sim.data1(nsims = nsims_so,                        
            gov_bonds_var2          = gov_bonds_so             ,                                    
            gov_bonds_var2_sd       = gov_bonds_so * 0.1          ,                                 
            inflation_var2          = inflation_so             ,                                    
            inflation_var2_sd       = inflation_so * 0.1          ,                                 
            wage_ag_var2            = wage_ag_so               ,                                      
            wage_ag_var2_sd         = wage_ag_so * 0.1            ,                                   
            wage_ww_var2            = wage_ww_so               ,                                      
            wage_ww_var2_sd         = wage_ww_so * 0.1            ,                                   
            profits_se_var2         = profits_se_so            ,                                   
            profits_se_var2_sd      = profits_se_so * 0.1         ,                                
            hours_se_cond_var2      = hours_se_cond_so         ,                                
            hours_se_cond_var2_sd   = hours_se_cond_so * 0.1      ,                             
            hours_ag_var2           = hours_ag_so              ,                                     
            hours_ag_var2_sd        = hours_ag_so * 0.1           ,                                  
            hours_ww_var2           = hours_ww_so              ,                                     
            hours_ww_var2_sd        = hours_ww_so * 0.1           ,                                  
            hours_se_var2           = hours_se_so              ,                                     
            hours_se_var2_sd        = hours_se_so * 0.1           ,                                  
            ex_rate_var2            = ex_rate_so               ,                                      
            ex_rate_var2_sd         = ex_rate_so * 0.1            ,                                   
            growth_rate_var2        = growth_rate_so           ,                                  
            growth_rate_var2_sd     = growth_rate_so * 0.1        ,
            coverage_var2           = coverage_so              ,
            coverage_var2_sd        = coverage_so * 0.1           ,  
            tax_var2                = tax_so                   ,                                             
            tax_var2_sd             = tax_so * 0.1                ,                                        
            unit_cost_local_var2    = unit_cost_local_so       ,                                     
            unit_cost_local_var2_sd = unit_cost_local_so * 0.1    ,                                       
            years_of_treat_var2     = years_of_treat_so        ,                                    
            years_of_treat_var2_sd  = years_of_treat_so * 0.1     ,                                      
            lambda1_var2            = lambda1_so,                                          
            lambda1_var2_sd         = rep(lambda1_so[1], 2) * 0.1 ,                                          
            lambda2_var2            = lambda2_so        ,                         
            lambda2_var2_sd         = lambda2_so * 0.1  ,                      
            q_full_var2             = q_full_so         ,                          
            q_full_var2_sd          = q_full_so * 0.1   ,                         
            coef_exp_var2           = coef_exp_so,                      
            #                      coef_exp_var2_sd = c(as.numeric(input$param21_1_1), as.numeric(input$param21_2_1)),                       
            teach_sal_var2          = teach_sal_so         ,                                          
            teach_sal_var2_sd       = teach_sal_so * 0.1      ,                                       
            teach_ben_var2          = teach_ben_so         ,                                          
            teach_ben_var2_sd       = teach_ben_so * 0.1      ,                                       
            n_students_var2         = n_students_so        ,                                         
            n_students_var2_sd      = n_students_so * 0.1     ,                                      
            delta_ed_var2           = delta_ed_par_so          ,                                           
            delta_ed_var2_sd        = delta_ed_par_so * 0.1       ,                                            
            delta_ed_ext_var2       = delta_ed_ext_par_so      ,                                           
            delta_ed_ext_var2_sd    = delta_ed_ext_par_so * 0.1   ,                                              
            q_zero_var2             = q_zero_so            ,                                             
            q_zero_var2_sd          = q_zero_so * 0.1         ,
            lambda1_new_var2        = lambda1_new_so,                   
            lambda1_new_var2_sd     = lambda1_new_sd_so,             
            alpha_0_var2            = alpha_0_so       ,  
            alpha_0_var2_sd         = alpha_0_so * 0.1    ,
            alpha_r_var2            = alpha_r_so       ,  
            alpha_r_var2_sd         = alpha_r_so * 0.1    ,                                                                         
            staff_time_var2         = staff_time_so    ,
            staff_time_var2_sd      = staff_time_so * 0.1,
            counts_par_var2         = counts_par_so    ,
            counts_par_var2_sd      = counts_par_sd_so ,
            costs_par_var2          = costs_par_so     ,
            costs_par_var2_sd       = costs_par_sd_so)


total_time <- npv_sim_all$total_time
position <- which( policy_estimates == policy_estimate_so )
npv_sim <- npv_sim_all[[ policy_estimates[position] ]]    
npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))

#Unit test the simulations for nsims = 100, 1000, 10000
all_res_100_sims <- c(
  4.36316094954722,
  23.2129229802102,
  26.0374491586364,
  140.04952241052,
  55.6356384691426,
  323.847303050562,
  26.4428542800085,
  141.17922793913,
  323.94495187629,
  329.968011932834,
  0.443505392382842
)
all_res_1000_sims <- c(
  5.32356462047516,
  27.6432480518217,
  26.163996006382,
  139.12854915303,
  55.296769054413,
  328.384415939951,
  26.2272821730184,
  138.95772521348,
  328.60765919238,
  323.313130629793,
  0.434560659448646
)
all_res_10000_sims <- c(
4.48502904005645,
23.7639919630348,
26.1966872755888,
140.753259914499,
56.0899202446023,
333.729000769758,
26.5531644269483,
141.80292767189,
333.852743072266,
331.992604782129,
0.446226619330819
)

k <- 0
for ( i in policy_estimates ) {
    k <- k + 1
    to_test <- npv_sim_all[[i]]
    if (nsims_so == 1e4){
        unit_test(to_test, all_res_10000_sims[k], main_run_var = TRUE)
    } else if (nsims_so == 1e3){
        unit_test(to_test, all_res_1000_sims[k], main_run_var = TRUE)
    } else if(nsims_so == 1e2){
        unit_test(to_test, all_res_100_sims[k], main_run_var = TRUE)
    }
}
```

```
## [1] "Output has change at to_test  to  8.7094550833253"
## [1] "Output has change at to_test  to  49.2632644048296"
## [1] "Output has change at to_test  to  49.4694221681446"
## [1] "Output has change at to_test  to  277.265576489817"
## [1] "Output has change at to_test  to  175.678254409271"
## [1] "Output has change at to_test  to  1022.37359604602"
## [1] "Output has change at to_test  to  49.8999424003478"
## [1] "Output has change at to_test  to  278.489912293979"
## [1] "Output has change at to_test  to  1022.26900808398"
## [1] "Output has change at to_test  to  679.520544084327"
## [1] "Output has change at to_test  to  0.913334064629472"
```

```r
################
###### Results/Viz
################

nsims <- nsims_so

npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))

rescale <- rescale_so

    plot1 <- ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-10, 400)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = paste0("Distribution of NPV of ", policy_estimates_text[position]
           ),
           subtitle = paste0("N = ", nsims, " simulations. Takes ",
                             round(total_time, 1)," ",attributes(total_time)$unit )  )+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.012, label = npv_for_text, size = 6)+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.004, label = npv_for_text2, size = 6)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    if (rescale == TRUE) {
      plot1 <- suppressMessages( plot1 + coord_cartesian(xlim = 1.2 * c( min( c(-1, npv_sim) ), max(npv_sim))) )
    }

print(plot1)
```

![](05_final_opa_files/figure-html/run-mc-1.png)<!-- -->

```r
#knitr::purl("code/05_final_opa.Rmd", "code/shiny_app/all_analysis.R")
```

# Sensitivity Analysis  



# References


[^1]: EAs version of the analysis follows a similar structure than the cost effectiveness analysis performed by the charity evaluator GiveWell [@givewell].


[^5]:`F1 = GiveWell's estimates of Deworm the World's cost per child dewormed per year [2018]` Original [here](https://docs.google.com/spreadsheets/d/1jzS693Y-ZAIloQejlzSc3e3t7iPHyor1qt7HBjSVXhQ/edit#gid=509033857), editable version [here](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=509033857)
`F2 = 2019 GiveWell Cost-effectiveness Analysis — Version 3`  
`F3 = 2018 Worm Intensity Workbook — Version 1` Sheets are named the first time and numbered thereafter.


[^2]: to account for several high-level activities Deworm the World does not include in its cost per treatment analyses, as they are not directly related to any particular program


[^3]: https://docs.google.com/document/d/1BkQLyLYQmy9O7FISge78PnWy9urMo0k31RwI5tOhJE4/edit
