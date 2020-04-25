---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "24 April, 2020"
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
title: "A Unifying Open Policy Analysis for Deworming"
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


# DESCRIBE CHUNK STRUCTURE
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
                                   #Table 4, Panel A FIX: MOST REFERENCES FROM TABLE 4 ARE TABLE 3
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
    unit_cost_ppp_so <- unit_cost_so*ex_rate_2018/ex_rate_2018_ppp_so
    unit_cost_2017usdppp_so <- unit_cost_ppp_so*cpi_2017_so/cpi_2018_so

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
    #alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55,
    # "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
    df_alpha_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")

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
    alpha_0_so <- 1#0.77 #0.77
    alpha_r_so <- 1#0.15 #0.15
    #############
    ##### Guess work   
    #############
    periods_so <- 50               #Total number of periods to forecast wages
    time_to_jm_so <- 10            #Time from initial period until individual join the labor force
    coef_exp_so <- c(0.1019575, -0.0010413)         #Years of experience coefficients (1-linear, 2-cuadratic)
                                                    #- see notes(0.1019575, -0.0010413), (0,0)
    teach_sal_so <- 5041           #Yearly secondary schooling compensation	5041 - from ROI materials
    teach_ben_so <- 217.47         #Yearly secondary schooling teacher benefits	217.47
    teach_sal_new_so <- (50000*12/49.77)
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
    nsims_so <- 1e2
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
```


# Introduction  

Mass deworming has demonstrated to be a highly effective public health intervention in the past. Here we provide a policy analysis that compares benefits and costs of deworming for different potential new settings. The goal of this analysis is to provide the best empirical information for policy makers debating the implemention of a deworming policy. This document describes all the analytical steps required to reproduce the analysis, displaying the actual computer code use in each step. In addition to this report, the reader can find all the materials to reproduce the findings presented here in [github.org/bitss/opa-deworming](https://github.org/bitss/opa-deworming). The main output, presented in the [results section](#policy-estimate) of this report, can also be explored interactively for different assumptions in [this web app](add link).


The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:   
  1. the original CBA produced by @baird2016worms,   
  2. an updated version of such analysis by a symilar research team [@klps4], and   
  3. a third approach that borrows some components of the previous two and some specific components requested by the NGO, Evidence Action (EA)[^1].



<!--
OLD TEXT:
The key policy estimate consists of a cost effectiveness analysis that compares the present
value of benefits and costs. The benefits quantified here are the effects on wages an the
costs are those of delivering the deworming treatment.  

The benefits will account for the direct effects of deworming and plus the indirect effects of deworming due to smaller pool of sick people in the community (herd inmunity). Effects are computed as a change in the earning profile of the population.

This analaysis contains elements from GiveWell's cost effectiveness analaysis (see [here](https://docs.google.com/spreadsheets/d/1McptF0GVGv-QBlhWx_IoNVstWvt1z-RwVSu16ciypgs/edit#gid=1537947274), an editable version can be found [here](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1537947274))  and the cost benefit analysis described in [Baird et al., 2016](https://academic.oup.com/qje/article/131/4/1637/2468871).  
-->

# Methodology  

We first describe the common elements across all three approaches, and then describe each approach in detail.

## Common structure

The starting point is a comparison of a stream of benefits and costs over the lifetime of the recipients of deworming. The final policy estimate is the discounted sum of all costs and benefits, known as the Net Present Value (NPV). Another format to present this analysis is as a cost effectiveness ratio, in absolute terms or relative to the benchmark of cash transfers.

<details><summary>Show all the details</summary>

\begin{equation}
NPV = B - C \\
CEA = \frac{B \times F_0}{C} \\
RCEA = \frac{CEA(B,C)}{CEA_{cash}}

\label{eq:1}
\tag{1}
\end{equation}


```r
# - inputs: total per capita benefits, total per capita costs, fudging factor
# - outputs: Cost-effectiveness ratio & ratio to cash CEA
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
table_1 <- matrix("", nrow = 1, ncol = 3)
table_2 <- matrix("", nrow = 1, ncol = 2)
```


</details>

Benefits are equal to the additional earnings that indivudual are expected to generate due to a deworming treatment. These additional earrnings are computed as a discounted sum over their working lifetime.  


<details><summary>Show all the details</summary>


\begin{equation}
B =   \sum_{t=0}^{50}\left(  \frac{1}{1 + r}\right)^{t} E_{t}

\label{eq:2}
\tag{2}
\end{equation}


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
</details>


At a high level all three approaches focus on the same type of benefits: the increase in incomes over the lifetime of beneficiaries of deworming. This is probably an under-estimate of the benefits as it does not quantify the non-pecuniary effects of improved health.  The costs can be separated into direct costs of implementing deworming policies, and indirect costs associated with the benefits of deworming.

The main differences across the three approaches regarding benefits have to do with how to predict the earnings profiles over a lifecycle, and whether or not to account for different prevalence rates. Approaches 1 and 2 use different earning profiles, and approach 3 combines both earning profiles and adjusts for possible differences in prevalence rates of worm infections.

The main differences in costs have to do with whether indirect costs are included, and what is the relevant unit cost for the analysis. The first two approaches include indirect costs and use the unit costs of a specific country (Kenya) while the third approach does not include indirect costs and use unit costs of multiple countries.



### The discounting rate  

All approaches use the real interest rate ($r$) as the discounting rate. This is obtained from the interest rate on goverment bonds ($i$) minus the inflation rate ($\pi$).

<details><summary>Show all the details</summary>

\begin{equation}
r =   g - \pi

\label{eq:3}
\tag{3}
\end{equation}


```r
# - inputs: gov_bonds_so, inflation_so
# - outputs: interest_in
chunk_interest <- function(){
###############################################################################
###############################################################################  

    interest_f <- function(gov_bonds_var = gov_bonds_so ,
                           inflation_var = inflation_so) {  
        interest_in = gov_bonds_var - inflation_var
        return(list("interest_in" = interest_in))
    }

###############################################################################
###############################################################################  
    return(list("interest_f" = interest_f))
}

invisible( list2env(chunk_interest(),.GlobalEnv) )
interest_16 <- as.numeric( interest_f(gov_bonds_var = gov_bonds_so,
                                      inflation_var = inflation_so) )
interest_19 <- as.numeric( interest_f(gov_bonds_var = gov_bonds_new_so,
                                      inflation_var = inflation_new_so)  )
```

</details>

The actual value will vary across approaches depending on the time and country chosen. For example approach 1 used the return from government bonds and inflation in Kenya for the year 2016, while approach 3 used the values for the same country but for the year 2019. This resulted in discount rates of 9.85% and 5% respectively. 

<!-- Add fold/unfold for tables -->
<details><summary>View Summary Table</summary>

<table>
<caption>Summary of equations use until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
</tbody>
</table>

<table>
<caption>Sources: summary of inputs specified until this point in the document</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

</details>


## Approach 1: @baird2016worms

In this first approach, the effect on earnings over the entire lifecycle are predicted by extrapolating the effects on hours worked by individuals in the original treatment group, ten years after the intervention. 

Two type of results are presented: the total effect on earnings projected over a lifetime, and the estimated additional fiscal effect due to the goverment collecting taxes on higher earnings. The effects are calculated in two scenarios: with and without externalities.

###  Gains in earnings

Gains in earnings ($E_t$) are the result of multiplying expected earnings in a certain period ($w_t$) with the effects of deworming on worked hours. This effect can have two components: a direct effect of deworming on the individual ($\lambda_1$) and the indirect effect on earnings due to externalities ($\lambda_2$). The indirect effects are considered within the context of the treatment's coverage and saturation.

<details><summary>Show all the details</summary>


[^6]: The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender.

\begin{equation}
E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)

\label{eq:4}
\tag{4}
\end{equation}

Where[^6]:   

 - $w_t$: are the earnings in period $t$.   
 - $\lambda_{1}$: is the direct effects of deworming on earnings.  
 - $\lambda_{2}$: is the indirect effects of deworming on earnings.   
 - $p$: saturation, measures the fraction of the population that is effectively using the treatment.  
 - $R$: coverage, defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group.  


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

</details>

#### Earnings over time

Earnings in period t ($w_t$) are determined by multiplying the number of weeks worked up to time t by the weekly starting wage, which is adjusted to account for per capita GDP growth and a concave life cycle path for wages (wages typically increase with more years of work, then decline later in a life cycle). This all operates under the assumption that individuals in the data enter the labor force 10 years after the present day.

Individuals in the data are assumed to enter the labor force 10 years after the (data) present day ($w_t = 0, Xp = 0$ for $t<10$, and $Xp = t - 10$ for $t\geq 10$). Wage at time $t$ is the weekly starting wage in USD ($w_0$) that has a base growth rate equal to the per capita GDP growth ($g$) applied to however many years of work ($Xp$). In addition to this growth, the salaries are adjusted to represent a (concave) wage life cycle profile ($1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2$).

The initial wage in dollars ($w_{0}$) is a weighted average of wages for control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the average number of hours in each sector ($h_l$) relative to the sum of the average number of hours in each sector.  

The wage in agriculture comes from research (Suri, 2011), the working wage comes from the data and is defined as  hourly wage for the control group for those who reported more than 10 hrs of work per week. The self-employed wage ($w_{se}$) was constructed as follows:


<details><summary>Show all the details</summary>

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50

\label{eq:5}
\tag{5}
\end{equation}

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)

\label{eq:6}
\tag{6}
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
\\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}  
\end{equation}

\begin{equation}
w_{se} =  \frac{ \text{Monthly self-employed profits} }{4.5 \times E[h_{se}|h_{se}>0] }

\label{eq:7}
\tag{7}
\end{equation}



```r
#inputs: wages (wage_ag_so, wage_ww_so) self employed income (profits_se_so,
#  hours_se_cond_so) hours of work (hours_ag_so, hours_ww_so, hours_se_so),
#  exchange rate (ex_rate_so), timing vars (periods_so, time_to_jm_so),
#  growth rate (growth_rate_so), mincer coef (coef_exp_so[1], coef_exp_so[2])
#
#outputs: Starting wages: value (wage_0_mo) and function (wage_0_mo_f), Wage trajectory:
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

</details>

Where both parameters (Monthly self-employed profits and self-employed hours for the control group, conditional on hrs >0 - $E[h_{se}|h_{se}>0]$ -) come from the data [@baird2016worms].  The measure of hours in self employment used to compute wages ($E[h_{se}|h_{se}>0]$) is different from the one used to compute the weights $\alpha_l$ above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of working age in the sample (hence capturing the relative importance of the self employed sector in the economy).



#### "$\lambda_{1}$"  and  "$\lambda_{2}$"

$\lambda_{1,\gamma}$ represents the estimated impact of deworming on hours of work for men and women. This two parameters are combined with a simple mean:

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}\\

\label{eq:8}
\tag{8}
\end{equation}

$\lambda_{2,\gamma}$ the estimated externality effect (EXPLAIN) and comes from research (W\@W). Note that this parameter is not estimated by gender, so we repeat its value two times. All the components to the equation \\ref{eq:8} come from @baird2016worms.




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


The coverage, $R$, is defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group^[6]. As the treatment was applied to approximately two thirds of the population, $R$ is set to: $R  = 0.68$.  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively using the treatment and is defined as:  

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)

\label{eq:9}
\tag{9}
\end{equation}

For this (or similar?) setting Miguel and Kremer 2007 [add page, table, col, row] estimate that there is almost no take-up without subsidy, hence $Q(0)$ is assigned the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy is $Q(full) = 0.75$.


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



<details><summary>View Summary Table</summary>


<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
</tbody>
</table>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>


</details>

### Costs

The costs are a combination of direct costs on mass deworming (relative to the status quo, which is no subsidy for deworming) and indirect costs on the education system due to the additional time treated individuals spend in school.

\begin{equation}
C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)

\label{eq:10}
\tag{10}
\end{equation}

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $C=NA$ </td>
   <td style="text-align:left;"> $S_2=NA$ </td>
   <td style="text-align:left;"> $Q=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_1=NA$ </td>
   <td style="text-align:left;"> $K=NA$ </td>
   <td style="text-align:left;"> $r=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=NA$ </td>
   <td style="text-align:left;"> $t=NA$ </td>
   <td style="text-align:left;"> $\Delta \bar{E}_t=NA$ </td>
  </tr>
</tbody>
</table>
</details>


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

#### Increase in deworming costs

Direct deworming costs ($DC$) are defined as the take-up under a mass deworming ($Q_{2}$) intervention, times the per-capita costs of deworming under the intervention ($S_{2}$). This costs are compared to a scenario where the government provides no additional resource for deworming ($S_{1}Q(S_{1})$).  

\begin{equation}
DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})

\label{eq:11}
\tag{11}
\end{equation}

<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
</tbody>
</table>
</details>

##### Status quo
This analysis assumes that there is no subsidy for deworming under the status quo ($S_{1}Q(S_{1}) = 0$).    

##### $S_{2}$: complete subsidy to per capita costs of deworming.  
With complete subsidy, $S_2$ represents the total direct costs of deworming in USD. Calculated as follows

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\

\label{eq:12}
\tag{12}
\end{equation}

<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
</tbody>
</table>
</details>

##### $Q_{2}$  
The take-up with full subsidy ($Q_2$) comes from a previous study [@kremer2007illusion] and takes the value of 0.75.


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

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $DC=NA$ </td>
   <td style="text-align:left;"> $S_2=NA$ </td>
   <td style="text-align:left;"> $Q=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_1=NA$ </td>
   <td style="text-align:left;"> $KSH=NA$ </td>
   <td style="text-align:left;"> $ex=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q_2=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>


#### $K$ and $\Delta \overline{E}_{t}(S1,S2)$

With the intervention there is an estimated increase in school attendance, which is multiplied by the cost of education per student to calculate the additional indirect cost on the education system imposed by a treated individual, who tends to spend more time in school than an untreated counterpart.

<details><summary>Equations</summary>
The additional costs on education are computed as following: first compute a cost per student ($K$). This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}

\label{eq:13}
\tag{13}
\end{equation}

Second, the cost per student is multiplied by the estimated increase in school attendance ($\Delta \overline{E}_{t}(S1,S2)$).
For this we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from an additional analysis related to @baird2016worms^[7].

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


Without externalities, they obtain total NPV of benefits of 142.43, with 12.9 in tax revenue for government (table 5, column 3, and rows 9, 10 respectively).

Including externalities, they obtain total NPV of benefits of 766.81, with 102.97 in tax revenue for government (table 5, column 3, and rows 12, 13 respectively).
</details>


```r
# Numbers are reproduces, the only difference is that  Baird et al, presents them in a different way.
# Equivalence below:
# Baird et al 2016                                          Here
# 142.43 - 10.71 - 1.07                                   = 130.65
# (142.43 * 0.16575 - 10.71 = 12.89777) - 1.07            = 11.82777
# 766.81 - 10.71 - 13.42 - 1.07                           = 741.61
# (766.81 * 0.16575 - 10.71 - 13.42 = 102.9688) - 1.07    = 101.8988
```

<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
</tbody>
</table>
</details>

-----------------

## Approach 2: @klps4

In this second approach, benefits follow the same principle as in approach 1 (increase in lifetime earnings), but now there is more data on the actual effects on the labor market outcomes. Instead of projecting a trend of earnings into the future, this analysis uses the data from 15 and 20 year follow-ups to the original intervention.  Costs are fairly similar to approach 1, with the addition that in the second approach, the costs also account for several rounds of treatment required for effective deworming.  

The interest rate here is updated to current values of return on (Kenyan) goverment bonds and inflation.


```r
#interest_in_new <- as.numeric(interest_f(gov_bonds_var = 0.09, inflation_var = 0.04))
interest_in_new <- interest_19
```

### Gains in earnings ($E_t$)

Gains in earnings from 10, 15, and 20 years after intervention are used to measure the effect of multiple rounds of deworming on welfare over time. Because we assume that the welfare gains 20 years after the intervention persist through the rest of an individual's working life, the treatment effect over an individual's working life is the sum of the treatment effects on welfare at each follow-up.

<details><summary>Equation</summary>
AQUI VOY

$E_t$ represents the treatment effect on welfare, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc.

We estimate treatment effects on total welfare by round. KLPS2 captures effects after 10 years; KLPS3 captures the effects after 15 years; and KLPS4 after 20 years. We will need to make assumptions about welfare gains from deworming after 20 years.

@klps4 assumes that the effect on welfare identified 20 years after the intervention persists through one's working life[^8].

[^8]: In another specification the authors assume that effects disappear after 25 years. Here we select the more persistent specification.

\begin{equation}
E_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t > 20)\alpha^{KLPS4}
\text{ for } t \leq 50

\label{eq:14}
\tag{14}
\end{equation}


This expression assumes that there are no additional earnings gains for the treatment group for the first 10 years post-intervention. This model also disregards externality effects.

Using earnings gains to measure welfare, we substitute each $\alpha$ term with the average treatment effect on earnings in each round of data collection: 87, 83, 85 dollars per person per year.
</details>


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

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $E_t=NA$ </td>
   <td style="text-align:left;"> $t=NA$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\alpha^{KLPS3}=NA$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
</tbody>
</table>
</details>

### Costs

Like approach 1, the direct deworming costs under approach 2 are calculated by comparing the costs under a complete subsidy to the status quo, but the direct costs are now discounted over the treatment period.

<details><summary>Equations</summary>

The costs have a  similar structure as @baird2016worms. Two differences: unit costs are estimated more accurately now, and the specific prices have been updated.  

New way to compute unit costs of deworming treatment:
\begin{equation}
DC = \sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:15}
\tag{15}
\end{equation}

Since the analysis is discrete, and we can not sum over a non-integer, we find
\begin{equation}
DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \\
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:16}
\tag{16}
\end{equation}
</details>

With complete subsidy, $S_2$ represents the total direct costs of deworming each child in USD. Most recent (2018) data from Evidence Action reveals this cost to be $0.42$. Adjusting for purchasing power and inflation, we get a per capita cost of $0.83$.


```r
# - inputs:
# - outputs:
chunk_unit_costs2_new <- function(){
###############################################################################
###############################################################################  

    s2_f_new <- function(unit_cost_local_var = unit_cost_local_so,
                     ex_rate_var = ex_rate_so,
                     interest_var = interest_in_new) {
      unit_cost <- ( unit_cost_local_var / ex_rate_var )
      sum(( unit_cost * (1 + interest_var)^(-(0:2)) ) * c(1,1,0.4))
    }

###############################################################################
###############################################################################  
    return(list("s2_f_new" = s2_f_new) )
}
invisible( list2env(chunk_unit_costs2_new(),.GlobalEnv) )
##### Execute values of the functions above when needed for the text:

s2_in <- s2_f_new(interest_var = interest_in_new, unit_cost_local_var = 0.8296927, ex_rate_var = 1)
s2_new <- s2_in
q2_in <- q_full_so
```

Adding all indirect cost, the average cost of deworming each child over the entire treatment period is $1.44.
<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $DC=NA$ </td>
   <td style="text-align:left;"> $r=NA$ </td>
   <td style="text-align:left;"> $S_2=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q=NA$ </td>
   <td style="text-align:left;"> $S_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \sum_{t=0}^{`r round(years_of_treat_so,1)-1`} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
</tbody>
</table>
</details>

#### Cost of schooling

The indirect cost on the education system is calculated similarly to approach 1: the cost per student is multiplied by the increase in school attendance due to deworming. However, under approach 2 the additional cost of education is discounted because we assume that the additional burden on educational institutions is imposed for nine years at most. 

<details><summary>Equation</summary>
We account for the cost of schooling since deworming medication increases school attendance and may put pressure on educational institutions. Schooling costs are given by the discounted sum of the additional cost of education per child as a result of deworming.

The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school as a result of deworming. Assuming pressure is added to educational institutions for a maximum of nine years, starting at year zero, we have

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)

\label{eq:17}
\tag{17}
\end{equation}

The cost per student ($K$) is updated with new information on annual teacher salary (including benefits)[^9], $12055 (also adjusted for PPP), and same number of average number of students per teacher (45).

Hence, the cost of schooling each child for an additional year is now $267.9 (USD).

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor [FIND SOURCES]. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FILL IN]**.


```r
delta_ed_in <- delta_ed_so[,1]
cost_per_student_in_new <- cost_per_student_f(teach_sal_var = (50000*12/49.77),
                                          teach_ben_var = 0,
                                          n_students_var = 45)
```
</details>

Over this nine year period, students attended school for an additional 0.15 years on average.

**Then we get an average cost of additional schooling per child over the nine-year period, $32.40.**

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $K=NA$ </td>
   <td style="text-align:left;"> $r=NA$ </td>
   <td style="text-align:left;"> $t=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\Delta \bar{E}_t=NA$ </td>
   <td style="text-align:left;"> $S_1=NA$ </td>
   <td style="text-align:left;"> $S_2=NA$ </td>
  </tr>
</tbody>
</table>
</details>


<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \sum_{t=0}^{`r round(years_of_treat_so,1)-1`} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
</tbody>
</table>
</details>

-------------------




## Approach 3: Evidence Action

- Key elements:
  - No costs on ed.
  - Country specific costs.   


### Costs ("$C$")

For each country where Evidence Action provides technical assistance, GiveWell estimates the deworming cost per child using a combination of technical assistance costs, indirect and direct government expenditure, and partner costs. GiveWell's estimated cost is a weighted average of the unit costs across countries: for each country, the annual per capita cost is weighted by the proportion of the country's deworming costs relative to the total deworming costs across countries.

<details><summary>Equation</summary>
\begin{equation}
C = \sum_{i \in Countries } \omega_{i} c_{i}

\label{eq:18}
\tag{18}
\end{equation}

GiveWell estimates the cost per child dewormed in geographies where Evidence Action provides technical assistance. These costs include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such as the cost of drugs donated by WHO.

Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The final cost is a weighted average of the unit cost across countries.

- $\omega_{i}$: Weight for the weighted average.  
- $c_{i}$: Total cost per child, per year in country $i$.  
</details>

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

\label{eq:19}
\tag{19}
\end{equation}


\begin{equation}
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \\
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}

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

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $N_i=NA$ </td>
   <td style="text-align:left;"> $Ex_i=NA$ </td>
   <td style="text-align:left;"> $k=NA$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \sum_{t=0}^{`r round(years_of_treat_so,1)-1`} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(19)$ </td>
  </tr>
</tbody>
</table>

</details>

#### Data required to compute costs.

$N_{i}, C_{i,k,l}, \delta_{g}$


### Benefits   
From either @baird2016worms or @klps4. Only difference is that results now take into account prevalence.

#### "$\lambda_{1}^{eff}$"  

\begin{equation}
\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0

\label{eq:20}
\tag{20}
\end{equation}



Where:      

 - $\alpha$: represents the incidence of the condition.  
 - $\lambda_{1}^{eff}$: represents the effect of deworming over those affected with the condition.  

**TO DO: add a section that discusses where are the $\alpha's$ comming from**   


```r
alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE
df_alpha_so <- read_excel("data/prevalence_data.xlsx",
                           sheet = "Sheet1")
```



In the original evaluation, $\alpha = 0.77$, hence $\lambda_{1}^{eff} = 1.75/0.77 = 2.72$. The value of $\lambda^{r}_{1}$ for each region $r$ will depend on that region's $\alpha^{r}$.  

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\alpha=NA$ </td>
   <td style="text-align:left;"> $\lambda_{1}^{eff}=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \sum_{t=0}^{`r round(years_of_treat_so,1)-1`} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(19)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(20)$ </td>
  </tr>
</tbody>
</table>
</details>

### Different format of policy estimate {#policy-estimate}

The key result for policy makers is defined as the cost effectiveness ratio (cell [`Deworming!B32`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B32)).

\begin{equation}
CEA_{deworming} = \frac{B (1 + F_{0})}{C}

\label{eq:21}
\tag{21}
\end{equation}

 - $C$ is the costs per person dewormed (`F2, 4,B23` --> [`F1, 2, H16`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=H16)).     
 - $B$ is the benefits per person dewormed (`F2, 4,B22`).
 - $F_{0}$ is a factor to account for leverage/fudging [not reviewed in this excercise] ([`F2, 6, D259`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1611790402&range=D259))


Also this quantity could be expressed in relative terms to the benchmark of cash transfers (cell [`Results!B9`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1034883018&range=B9)):

\begin{equation}
RCEA = \frac{CEA_{deworming}}{CEA_{cash}}

\label{eq:22}
\tag{22}
\end{equation}

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $C=NA$ </td>
   <td style="text-align:left;"> $B=NA$ </td>
   <td style="text-align:left;"> $F_0=NA$ </td>
  </tr>
</tbody>
</table>
</details>

<details><summary>View Summary Table</summary>
<table>
<caption>Sources: summary of inputs</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Data </th>
   <th style="text-align:left;"> Research </th>
   <th style="text-align:left;"> Guesswork </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> $\pi_{16}=0.02$ </td>
   <td style="text-align:left;"> $p=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_l=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=NA$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=NA$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_l=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=NA$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

<table>
<caption>Model: summary of equations</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Equation </th>
   <th style="text-align:left;"> # </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $NPV = B - C$ </td>
   <td style="text-align:left;"> $(1)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $B=\sum_{t=0}^{50}\left(\frac{1}{1+r}\right)^{t}E_{t}$ </td>
   <td style="text-align:left;"> $(2)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $r=g-\pi$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = w_{t}\left( \lambda_{1} + \frac{p \lambda_{2}}{R} \right)$ </td>
   <td style="text-align:left;"> $(4)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2)$ </td>
   <td style="text-align:left;"> $(5)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
                     \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}$ </td>
   <td style="text-align:left;"> $(6)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se} = \frac{\text{Monthly self-employed profits}}{4.5 \times E[h_{se}|h_{se} \text{&gt;} 0]}$ </td>
   <td style="text-align:left;"> $(7)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}$ </td>
   <td style="text-align:left;"> $(8)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $p = R \times Q(full)  + (1 - R) \times Q(0)$ </td>
   <td style="text-align:left;"> $(9)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = S_{2}Q(S_{2}) - S_{1}Q(S_{1})$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment}$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \sum_{t=0}^{`r round(years_of_treat_so,1)-1`} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(19)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(20)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $CEA_{deworming} = rac{B (1 + F_{0})}{C}$ </td>
   <td style="text-align:left;"> $(21)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $RCEA = rac{CEA_{deworming}}{CEA_{cash}}$ </td>
   <td style="text-align:left;"> $(22)$ </td>
  </tr>
</tbody>
</table>

</details>
# Main results


| Name    | Benfits                                   | Costs        |
|---------|-------------------------------------------|--------------|
| Baird 1 | Baird w/tax and no externalities (no ext) | Baird no ext |
| Baird 2 | Baird w/t and ext                         | Baird ext    |
| Baird 3 | Baird all and no ext                      | Baird no ext |
| Baird 4 | Baird all and ext                         | Baird ext    |
| KLPS4_1 | KLPS4 w/t and no ext                      | Baird no ext |
| KLPS4_2 | KLPS4 all and no ext                      | Baird no ext |
| EA 1    | Baird all and no ext                      | EA           |
| EA 2    | Baird all and ext                         | EA           |
| EA 3    | KLPS all and no ext                       | EA           |  




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
# └──── cost2_f = 11.776188118988
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
           alpha_0_var1 = alpha_0_so,
           alpha_r_var1 = alpha_r_so,
           lambda2_var1 = lambda2_so,                                        
           coverage_var1 = coverage_so,                                        
           q_full_var1 = q_full_so,                                        
           q_zero_var1 = q_zero_so,                                        
           lambda1_new_var1 = lambda1_new_so,                                        
           gov_bonds_var1 = gov_bonds_so,                                        
           inflation_var1 = inflation_so,                                        
           gov_bonds_new_var1 = gov_bonds_new_so,                                                     
           inflation_new_var1 = inflation_new_so,                                       
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
           teach_sal_new_var1 = teach_sal_new_so,                                            
           teach_ben_new_var1 = teach_ben_new_so,                              
           unit_cost_local_var1 = unit_cost_local_so,     
           unit_cost_local_new_var1 = 0.8296927,             #  CALL SO          
           years_of_treat_var1 = years_of_treat_so,                                        
           tax_var1 = tax_so,                                        
           periods_var1 = periods_so) {                                        
    ####------------ Inputs for wage_t ---------------------------------------------
    wage_0_in <- wage_0_mo_f(wage_ag_var = wage_ag_var1, wage_ww_var = wage_ww_var1,
                             profits_se_var = profits_se_var1, hours_se_cond_var = hours_se_cond_var1,  
                             hours_ag_var = hours_ag_var1, hours_ww_var = hours_ww_var1,
                             hours_se_var = hours_se_var1, ex_rate_var = ex_rate_var1)
    unit_test(wage_0_in, 0.170124466664436, main_run_var = main_run_var1)
    ###---------- Inputs for earnings1_f -------------------------------------------
    wage_t_in <- wage_t_mo_f(wage_0_var = wage_0_in, growth_rate_var = growth_rate_var1,
                             coef_exp1_var = coef_exp_var1, coef_exp2_var = coef_exp2_var1)

    lambda1_in <- lambda_r_f(lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
                             alpha_0_var = alpha_0_var1, alpha_r_var = alpha_r_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)

    saturation_in <- as.numeric(saturation_in_f(coverage_var = coverage_var1,
                                                q_full_var = q_full_var1,
                                                q_zero_var = q_zero_var1) )
    unit_test(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)
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
    unit_test(earnings_in_no_ext, 31.1421332040266, main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext, 167.667817450905, main_run_var = main_run_var1)
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

    interest_in_new <- as.numeric(interest_f(gov_bonds_var = gov_bonds_new_var1,
                                             inflation_var = inflation_new_var1))

    cost_per_student_in <-  cost_per_student_f(teach_sal_var = teach_sal_var1,
                                               teach_ben_var = teach_ben_var1,
                                               n_students_var = n_students_var1)
    unit_test(cost_per_student_in,  116.8549, main_run_var = main_run_var1)

    #TODO: remove hardcoded numbers
    cost_per_student_in_new <- cost_per_student_f(teach_sal_var = teach_sal_new_var1,
                                          teach_ben_var = teach_ben_new_var1,
                                          n_students_var = n_students_var1)

    s2_in <- s2_f(unit_cost_local_var = unit_cost_local_var1,
                  ex_rate_var = ex_rate_var1, years_of_treat_var = years_of_treat_var1)
    unit_test(s2_in, 1.4219, main_run_var = main_run_var1)
    #--------------- Inputs for NPV_pe_f, CEA_pe_f and RCEA_pe_f--------------------
    # Make explicit non-function inputs:
    #Benefits:
    #Baird w/tax and no externalities (no ext)
    pv_benef_tax_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_nx_in, 23.6070893378784, main_run_var = main_run_var1)
    #Baird w/t and ext
    pv_benef_tax_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext * tax_var1,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_tax_yx_in, 127.0994867217, main_run_var = main_run_var1)
    #Baird all and no
    pv_benef_all_nx_in <- pv_benef_f(earnings_var = earnings_in_no_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_nx_in, 142.42587835824, main_run_var = main_run_var1)
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(earnings_var = earnings_in_yes_ext,
                                     interest_r_var = interest_in, periods_var = periods_var1)
    unit_test(pv_benef_all_yx_in, 766.814399527604, main_run_var = main_run_var1)

    #KLPS4 w/t and no ext
    pv_benef_tax_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new * tax_var1,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    # ADD UNIT TEST
    # KLPS4 all and no ext
    pv_benef_all_new <- pv_benef_f(earnings_var = earnings_in_no_ext_new,
                                   interest_r_var = interest_in_new,
                                   periods_var = periods_var1)
    unit_test(pv_benef_all_new, 950.2367, main_run_var = main_run_var1)

    #Costs
    # costs1: EA costs no externalities
    cost1_in <- costs1_f(country_w_var = costs1_country$ratios_data$country_w,
                         country_cost_var = costs1_country$ratios_data$per_cap)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)

    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, ex_rate_var = 1)
    costs2_ea_in <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in_new, cost_of_schooling_var = 0,
                         s1_var = 0, q1_var = 0, s2_var = s2_ea_in, q2_var = q_full_var1)

    # costs2: Baird no externalities
    costs2_in <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                         s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in, 11.776188118988, main_run_var = main_run_var1)

    # Baird yes externalities
    costs2_in_x <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in_x,
                           interest_r_var = interest_in, cost_of_schooling_var = cost_per_student_in,
                           s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q_full_var1)
    unit_test(costs2_in_x,  25.1962130559894, main_run_var = main_run_var1)

    s2_new_in <- s2_f_new(interest_var = interest_in_new, unit_cost_local_var = unit_cost_local_new_var1, ex_rate_var = 1)
    # costs2: KLPS4
    costs_k <- cost2_f(periods_var = periods_var1, delta_ed_var = delta_ed_final_in,
                         interest_r_var = interest_in_new,
                       cost_of_schooling_var = cost_per_student_in_new,
                         s1_var = 0, q1_var = 0, s2_var = s2_new_in, q2_var = q_full_var1)
    unit_test(costs_k, 32.2996145651321, main_run_var = main_run_var1)

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
                  "pv_benef_all_new" = pv_benef_all_new, "costs2_ea_in" = costs2_ea_in,
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k) )
  }

invisible( list2env(one_run(),.GlobalEnv) )
```




```r
#TODO: update unit test values
#Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, 11.8309012188904)
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
baird2 <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
unit_test(baird2, 101.903273665711)

# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
baird3 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test(baird3, 130.649690239252)
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
baird4 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
unit_test(baird4, 741.618186471615)

#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1 <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs_k)
unit_test(klps4_1, 125.202113576337)
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs_k)
unit_test(klps4_2, 917.937055971637)


# res_npv_no_ext_klps_eacosts <- NPV_pe_f(benefits_var = pv_benef_in_new, costs_var = cost1_in)
# unit_test(res_npv_no_ext_klps_eacosts, 59.15516)

# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
unit_test(ea1, 142.278620185973)
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
unit_test(ea2, 766.667141355337)
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
unit_test(ea3, 950.089412364501)

#CEA for EA
#TODO: update CEA values.
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0)
unit_test(cea_no_ext_ea, 6452.86204429499)

rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0),
         CEA_cash_var = 744)
unit_test(rcea_no_ext_ea, 8.6732016724395)

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
   <td style="text-align:right;"> 125.2 </td>
   <td style="text-align:right;"> 917.9 </td>
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
   <td style="text-align:right;"> 950.1 </td>
  </tr>
</tbody>
</table>


## Results for overall welfare (not only taxes)


- **NPV without externalities in @baird2016worms ($\lambda_2 = 0$):** 130.6    

- **NPV with externalities in @baird2016worms ($\lambda_2 = 10.2$ ):** 741.6  

- **NPV without externalities in @klps4:** 917.9   

- **NPV without externalities in EA 2019 ($\lambda_2 = 0$):** 142.3    

- **NPV with externalities in EA 2019 ($\lambda_2 = 10.2$ ):** 766.7

- **NPV without ext and benef from @klps4 and EA costs 2019 :** 950.1

- **CEA format:** 6452.9    

- **RCEA format (relative to cash):** 8.7    


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

    gov_bonds_new_sim <-    rnorm(n = nsims, mean = gov_bonds_new_var2, sd = gov_bonds_new_var2_sd)
    inflation_new_sim <-    rnorm(nsims, inflation_new_var2, inflation_new_var2_sd)                  

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
    # ex_rate_new_sim
    tax_sim <-              rnorm(nsims, tax_var2, tax_var2_sd)

    unit_cost_local_sim <-  rnorm(nsims, unit_cost_local_var2, unit_cost_local_var2_sd)
    # unit_cost_local_new_sim
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
    time_to_jm_val <- 10        #Time from initial period until individual join the labor force
    aux2 <- lapply(1:2, function(x) c(coef_exp_var2[x],c(0.001 , 0.001)[x]) )
    coef_exp_sim <- sapply(aux2, function(x)  rnorm(nsims, mean = x[1], sd = x[2]) )     
    teach_sal_sim <-    rnorm(nsims, teach_sal_var2, teach_sal_var2_sd)
    teach_ben_sim <-    rnorm(nsims, teach_ben_var2, teach_ben_var2_sd)

    teach_sal_new_sim <-    rnorm(nsims, teach_sal_new_var2, teach_sal_new_var2_sd)
    teach_ben_new_sim <-    rnorm(nsims, teach_ben_new_var2, teach_ben_new_var2_sd)

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
     # change to j?       
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
                gov_bonds_new_var1 = gov_bonds_new_sim[i],
                inflation_new_var1 = inflation_new_sim[i],
                staff_time_var1 = staff_time_sim[i],
                delta_ed_var1 = cbind(delta_ed_sim[i,], 1999:2007),
                delta_ed_ext_var1 = cbind(delta_ed_ext_sim[i,], 1999:2007),
                teach_sal_var1 = teach_sal_sim[i],
                teach_ben_var1 = teach_ben_sim[i],
                teach_sal_new_var1 = teach_sal_new_sim[i],
                teach_ben_new_var1 = teach_ben_new_sim[i],
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
      ea1_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
      # EA2: yes externality NPV using EAs costs
      ea2_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
      # EA3: benef= KLPS all and no ext; Costs=EA
      ea3_sim[i]  <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
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
            gov_bonds_new_var2      = gov_bonds_new_so      ,          #add to app
            gov_bonds_new_var2_sd   = gov_bonds_new_so * 0.1,          #add to app
            inflation_new_var2      = inflation_new_so      ,          #add to app
            inflation_new_var2_sd   = inflation_new_so * 0.1,          #add to app
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
            teach_sal_new_var2      = teach_sal_new_so         ,          #add to app                                
            teach_sal_new_var2_sd   = teach_sal_new_so * 0.1      ,       #add to app                                
            teach_ben_new_var2      = teach_ben_new_so         ,          #add to app                               
            teach_ben_new_var2_sd   = 0.000001      ,                     #add to app
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
  8.7094550833253,
  49.2632644048296,
  49.4694221681446,
  277.265576489817,
  175.678254409271,
  1022.37359604602,
  49.9034982533997,
  278.49341938017,
  1022.26807845879,
  413.199179925478,
  0.555375241835319
)

all_res_1000_sims <- c(
  8.94904200012673,
  47.7557079511762,
  52.2490551593106,
  277.810659510651,
  176.29084181961,
  1051.32849483112,
  52.6663916192209,
  279.015364618585,
  1051.32339750587,
  407.842773941034,
  0.548175771426122
)

all_res_10000_sims <- c(
  8.79105935822713,
  47.6994033603414,
  52.2778377879947,
  279.54416693675,
  181.921984084524,
  1085.22816114431,
  52.7479120047233,
  280.883186390767,
  1085.20816160347,
  406.388379235971,
  0.546220939833294
)

if (FALSE) {
k <- 0
for ( i in policy_estimates ) {
    k <- k + 1
    to_test <- npv_sim_all[[i]]
    if (nsims_so == 1e4){
        unit_test(to_test, all_res_10000_sims[k], main_run_var = TRUE)
    } else if (nsims_so == 1e3){
        unit_test(to_test, all_res_1000_sims[k], main_run_var = TRUE)
    } else if (nsims_so == 1e2){
        unit_test(to_test, all_res_100_sims[k], main_run_var = TRUE)
    }
}
}
################
###### Results/Viz
################
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
nsims <- nsims_so

npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 2))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 2))

rescale <- rescale_so

    plot1 <- ggplot() +
      geom_density(aes(x = npv_sim,
                       alpha = 1/2, ..scaled..), kernel = "gau") +
      geom_vline(xintercept = c(0, median(npv_sim)), col="blue") +
      coord_cartesian(xlim = c(-10, 400)) +
      guides(alpha = "none", colour="none") +
      labs(y = NULL,
           x = "NPV" ,
           title = paste0("Distribution of NPV of ", policy_estimates_text[position]
           ),
           subtitle = paste0("N = ", nsims, " simulations. Takes ",
                             round(total_time, 1)," ",attributes(total_time)$unit )  )+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.25, label = npv_for_text, size = 4)+
      annotate("text", x = 1.5 * median(npv_sim), y = 0.10, label = npv_for_text2, size = 4)+
      theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    if (rescale == TRUE) {
      plot1 <- suppressMessages(
        plot1 +
          coord_cartesian(xlim = 1.2 * c( min( c(-1, npv_sim) ),
                                          max(npv_sim))
                          )
        )
    }
ggplotly(plot1)
```

<!--html_preserve--><div id="htmlwidget-0f6c1b258ebf14541344" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-0f6c1b258ebf14541344">{"x":{"data":[{"x":[-3744.30368403195,-3730.49910209893,-3716.69452016592,-3702.8899382329,-3689.08535629989,-3675.28077436687,-3661.47619243385,-3647.67161050083,-3633.86702856782,-3620.0624466348,-3606.25786470178,-3592.45328276877,-3578.64870083575,-3564.84411890273,-3551.03953696972,-3537.2349550367,-3523.43037310369,-3509.62579117067,-3495.82120923765,-3482.01662730464,-3468.21204537162,-3454.4074634386,-3440.60288150559,-3426.79829957257,-3412.99371763955,-3399.18913570654,-3385.38455377352,-3371.5799718405,-3357.77538990749,-3343.97080797447,-3330.16622604145,-3316.36164410844,-3302.55706217542,-3288.7524802424,-3274.94789830939,-3261.14331637637,-3247.33873444335,-3233.53415251034,-3219.72957057732,-3205.9249886443,-3192.12040671129,-3178.31582477827,-3164.51124284525,-3150.70666091224,-3136.90207897922,-3123.0974970462,-3109.29291511319,-3095.48833318017,-3081.68375124715,-3067.87916931414,-3054.07458738112,-3040.2700054481,-3026.46542351509,-3012.66084158207,-2998.85625964905,-2985.05167771604,-2971.24709578302,-2957.44251385,-2943.63793191699,-2929.83334998397,-2916.02876805095,-2902.22418611794,-2888.41960418492,-2874.6150222519,-2860.81044031889,-2847.00585838587,-2833.20127645285,-2819.39669451984,-2805.59211258682,-2791.7875306538,-2777.98294872079,-2764.17836678777,-2750.37378485475,-2736.56920292174,-2722.76462098872,-2708.9600390557,-2695.15545712269,-2681.35087518967,-2667.54629325665,-2653.74171132364,-2639.93712939062,-2626.1325474576,-2612.32796552459,-2598.52338359157,-2584.71880165855,-2570.91421972554,-2557.10963779252,-2543.3050558595,-2529.50047392649,-2515.69589199347,-2501.89131006045,-2488.08672812744,-2474.28214619442,-2460.4775642614,-2446.67298232839,-2432.86840039537,-2419.06381846235,-2405.25923652934,-2391.45465459632,-2377.6500726633,-2363.84549073029,-2350.04090879727,-2336.23632686425,-2322.43174493124,-2308.62716299822,-2294.8225810652,-2281.01799913219,-2267.21341719917,-2253.40883526615,-2239.60425333314,-2225.79967140012,-2211.9950894671,-2198.19050753409,-2184.38592560107,-2170.58134366805,-2156.77676173504,-2142.97217980202,-2129.167597869,-2115.36301593599,-2101.55843400297,-2087.75385206995,-2073.94927013694,-2060.14468820392,-2046.3401062709,-2032.53552433789,-2018.73094240487,-2004.92636047185,-1991.12177853884,-1977.31719660582,-1963.5126146728,-1949.70803273979,-1935.90345080677,-1922.09886887375,-1908.29428694074,-1894.48970500772,-1880.6851230747,-1866.88054114169,-1853.07595920867,-1839.27137727565,-1825.46679534264,-1811.66221340962,-1797.8576314766,-1784.05304954359,-1770.24846761057,-1756.44388567755,-1742.63930374454,-1728.83472181152,-1715.0301398785,-1701.22555794549,-1687.42097601247,-1673.61639407945,-1659.81181214644,-1646.00723021342,-1632.2026482804,-1618.39806634739,-1604.59348441437,-1590.78890248135,-1576.98432054834,-1563.17973861532,-1549.3751566823,-1535.57057474929,-1521.76599281627,-1507.96141088325,-1494.15682895024,-1480.35224701722,-1466.5476650842,-1452.74308315119,-1438.93850121817,-1425.13391928515,-1411.32933735214,-1397.52475541912,-1383.7201734861,-1369.91559155309,-1356.11100962007,-1342.30642768705,-1328.50184575404,-1314.69726382102,-1300.892681888,-1287.08809995499,-1273.28351802197,-1259.47893608895,-1245.67435415594,-1231.86977222292,-1218.0651902899,-1204.26060835689,-1190.45602642387,-1176.65144449085,-1162.84686255784,-1149.04228062482,-1135.2376986918,-1121.43311675879,-1107.62853482577,-1093.82395289275,-1080.01937095974,-1066.21478902672,-1052.4102070937,-1038.60562516069,-1024.80104322767,-1010.99646129465,-997.191879361638,-983.387297428621,-969.582715495605,-955.778133562588,-941.973551629571,-928.168969696555,-914.364387763538,-900.559805830522,-886.755223897505,-872.950641964488,-859.146060031472,-845.341478098455,-831.536896165439,-817.732314232422,-803.927732299405,-790.123150366388,-776.318568433372,-762.513986500355,-748.709404567338,-734.904822634322,-721.100240701305,-707.295658768288,-693.491076835272,-679.686494902255,-665.881912969239,-652.077331036222,-638.272749103205,-624.468167170189,-610.663585237172,-596.859003304156,-583.054421371139,-569.249839438122,-555.445257505105,-541.640675572089,-527.836093639072,-514.031511706055,-500.226929773039,-486.422347840022,-472.617765907005,-458.813183973989,-445.008602040972,-431.204020107956,-417.399438174939,-403.594856241922,-389.790274308906,-375.985692375889,-362.181110442873,-348.376528509856,-334.571946576839,-320.767364643822,-306.962782710806,-293.158200777789,-279.353618844772,-265.549036911756,-251.744454978739,-237.939873045722,-224.135291112706,-210.330709179689,-196.526127246672,-182.721545313656,-168.916963380639,-155.112381447623,-141.307799514606,-127.503217581589,-113.698635648573,-99.8940537155559,-86.0894717825395,-72.2848898495226,-58.4803079165058,-44.6757259834894,-30.8711440504726,-17.0665621174562,-3.26198018443938,10.5426017485775,24.3471836815938,38.1517656146107,51.956347547627,65.7609294806439,79.5655114136603,93.3700933466771,107.174675279694,120.97925721271,134.783839145727,148.588421078744,162.39300301176,176.197584944777,190.002166877794,203.80674881081,217.611330743827,231.415912676844,245.22049460986,259.025076542877,272.829658475894,286.63424040891,300.438822341927,314.243404274944,328.04798620796,341.852568140977,355.657150073993,369.46173200701,383.266313940027,397.070895873043,410.87547780606,424.680059739077,438.484641672093,452.28922360511,466.093805538127,479.898387471143,493.70296940416,507.507551337176,521.312133270193,535.11671520321,548.921297136226,562.725879069244,576.53046100226,590.335042935276,604.139624868294,617.94420680131,631.748788734326,645.553370667343,659.35795260036,673.162534533376,686.967116466393,700.77169839941,714.576280332426,728.380862265443,742.185444198459,755.990026131476,769.794608064493,783.599189997509,797.403771930527,811.208353863543,825.012935796559,838.817517729577,852.622099662593,866.426681595609,880.231263528626,894.035845461643,907.840427394659,921.645009327676,935.449591260693,949.254173193709,963.058755126726,976.863337059743,990.667918992759,1004.47250092578,1018.27708285879,1032.08166479181,1045.88624672483,1059.69082865784,1073.49541059086,1087.29999252388,1101.10457445689,1114.90915638991,1128.71373832293,1142.51832025594,1156.32290218896,1170.12748412198,1183.93206605499,1197.73664798801,1211.54122992103,1225.34581185404,1239.15039378706,1252.95497572008,1266.75955765309,1280.56413958611,1294.36872151913,1308.17330345214,1321.97788538516,1335.78246731818,1349.58704925119,1363.39163118421,1377.19621311723,1391.00079505024,1404.80537698326,1418.60995891628,1432.41454084929,1446.21912278231,1460.02370471533,1473.82828664834,1487.63286858136,1501.43745051438,1515.24203244739,1529.04661438041,1542.85119631343,1556.65577824644,1570.46036017946,1584.26494211248,1598.06952404549,1611.87410597851,1625.67868791152,1639.48326984454,1653.28785177756,1667.09243371057,1680.89701564359,1694.70159757661,1708.50617950962,1722.31076144264,1736.11534337566,1749.91992530867,1763.72450724169,1777.52908917471,1791.33367110772,1805.13825304074,1818.94283497376,1832.74741690678,1846.55199883979,1860.35658077281,1874.16116270583,1887.96574463884,1901.77032657186,1915.57490850488,1929.37949043789,1943.18407237091,1956.98865430393,1970.79323623694,1984.59781816996,1998.40240010298,2012.20698203599,2026.01156396901,2039.81614590202,2053.62072783504,2067.42530976806,2081.22989170107,2095.03447363409,2108.83905556711,2122.64363750012,2136.44821943314,2150.25280136616,2164.05738329917,2177.86196523219,2191.66654716521,2205.47112909822,2219.27571103124,2233.08029296426,2246.88487489727,2260.68945683029,2274.49403876331,2288.29862069632,2302.10320262934,2315.90778456236,2329.71236649537,2343.51694842839,2357.32153036141,2371.12611229442,2384.93069422744,2398.73527616046,2412.53985809347,2426.34444002649,2440.14902195951,2453.95360389252,2467.75818582554,2481.56276775856,2495.36734969157,2509.17193162459,2522.97651355761,2536.78109549062,2550.58567742364,2564.39025935666,2578.19484128967,2591.99942322269,2605.80400515571,2619.60858708872,2633.41316902174,2647.21775095476,2661.02233288777,2674.82691482079,2688.63149675381,2702.43607868682,2716.24066061984,2730.04524255286,2743.84982448587,2757.65440641889,2771.45898835191,2785.26357028492,2799.06815221794,2812.87273415096,2826.67731608397,2840.48189801699,2854.28647995001,2868.09106188302,2881.89564381604,2895.70022574906,2909.50480768207,2923.30938961509,2937.11397154811,2950.91855348112,2964.72313541414,2978.52771734716,2992.33229928017,3006.13688121319,3019.94146314621,3033.74604507922,3047.55062701224,3061.35520894526,3075.15979087827,3088.96437281129,3102.76895474431,3116.57353667732,3130.37811861034,3144.18270054336,3157.98728247637,3171.79186440939,3185.59644634241,3199.40102827542,3213.20561020844,3227.01019214146,3240.81477407447,3254.61935600749,3268.42393794051,3282.22851987352,3296.03310180654,3309.83768373956,3309.83768373956,3309.83768373956,3296.03310180654,3282.22851987352,3268.42393794051,3254.61935600749,3240.81477407447,3227.01019214146,3213.20561020844,3199.40102827542,3185.59644634241,3171.79186440939,3157.98728247637,3144.18270054336,3130.37811861034,3116.57353667732,3102.76895474431,3088.96437281129,3075.15979087827,3061.35520894526,3047.55062701224,3033.74604507922,3019.94146314621,3006.13688121319,2992.33229928017,2978.52771734716,2964.72313541414,2950.91855348112,2937.11397154811,2923.30938961509,2909.50480768207,2895.70022574906,2881.89564381604,2868.09106188302,2854.28647995001,2840.48189801699,2826.67731608397,2812.87273415096,2799.06815221794,2785.26357028492,2771.45898835191,2757.65440641889,2743.84982448587,2730.04524255286,2716.24066061984,2702.43607868682,2688.63149675381,2674.82691482079,2661.02233288777,2647.21775095476,2633.41316902174,2619.60858708872,2605.80400515571,2591.99942322269,2578.19484128967,2564.39025935666,2550.58567742364,2536.78109549062,2522.97651355761,2509.17193162459,2495.36734969157,2481.56276775856,2467.75818582554,2453.95360389252,2440.14902195951,2426.34444002649,2412.53985809347,2398.73527616046,2384.93069422744,2371.12611229442,2357.32153036141,2343.51694842839,2329.71236649537,2315.90778456236,2302.10320262934,2288.29862069632,2274.49403876331,2260.68945683029,2246.88487489727,2233.08029296426,2219.27571103124,2205.47112909822,2191.66654716521,2177.86196523219,2164.05738329917,2150.25280136616,2136.44821943314,2122.64363750012,2108.83905556711,2095.03447363409,2081.22989170107,2067.42530976806,2053.62072783504,2039.81614590202,2026.01156396901,2012.20698203599,1998.40240010298,1984.59781816996,1970.79323623694,1956.98865430393,1943.18407237091,1929.37949043789,1915.57490850488,1901.77032657186,1887.96574463884,1874.16116270583,1860.35658077281,1846.55199883979,1832.74741690678,1818.94283497376,1805.13825304074,1791.33367110772,1777.52908917471,1763.72450724169,1749.91992530867,1736.11534337566,1722.31076144264,1708.50617950962,1694.70159757661,1680.89701564359,1667.09243371057,1653.28785177756,1639.48326984454,1625.67868791152,1611.87410597851,1598.06952404549,1584.26494211248,1570.46036017946,1556.65577824644,1542.85119631343,1529.04661438041,1515.24203244739,1501.43745051438,1487.63286858136,1473.82828664834,1460.02370471533,1446.21912278231,1432.41454084929,1418.60995891628,1404.80537698326,1391.00079505024,1377.19621311723,1363.39163118421,1349.58704925119,1335.78246731818,1321.97788538516,1308.17330345214,1294.36872151913,1280.56413958611,1266.75955765309,1252.95497572008,1239.15039378706,1225.34581185404,1211.54122992103,1197.73664798801,1183.93206605499,1170.12748412198,1156.32290218896,1142.51832025594,1128.71373832293,1114.90915638991,1101.10457445689,1087.29999252388,1073.49541059086,1059.69082865784,1045.88624672483,1032.08166479181,1018.27708285879,1004.47250092578,990.667918992759,976.863337059743,963.058755126726,949.254173193709,935.449591260693,921.645009327676,907.840427394659,894.035845461643,880.231263528626,866.426681595609,852.622099662593,838.817517729577,825.012935796559,811.208353863543,797.403771930527,783.599189997509,769.794608064493,755.990026131476,742.185444198459,728.380862265443,714.576280332426,700.77169839941,686.967116466393,673.162534533376,659.35795260036,645.553370667343,631.748788734326,617.94420680131,604.139624868294,590.335042935276,576.53046100226,562.725879069244,548.921297136226,535.11671520321,521.312133270193,507.507551337176,493.70296940416,479.898387471143,466.093805538127,452.28922360511,438.484641672093,424.680059739077,410.87547780606,397.070895873043,383.266313940027,369.46173200701,355.657150073993,341.852568140977,328.04798620796,314.243404274944,300.438822341927,286.63424040891,272.829658475894,259.025076542877,245.22049460986,231.415912676844,217.611330743827,203.80674881081,190.002166877794,176.197584944777,162.39300301176,148.588421078744,134.783839145727,120.97925721271,107.174675279694,93.3700933466771,79.5655114136603,65.7609294806439,51.956347547627,38.1517656146107,24.3471836815938,10.5426017485775,-3.26198018443938,-17.0665621174562,-30.8711440504726,-44.6757259834894,-58.4803079165058,-72.2848898495226,-86.0894717825395,-99.8940537155559,-113.698635648573,-127.503217581589,-141.307799514606,-155.112381447623,-168.916963380639,-182.721545313656,-196.526127246672,-210.330709179689,-224.135291112706,-237.939873045722,-251.744454978739,-265.549036911756,-279.353618844772,-293.158200777789,-306.962782710806,-320.767364643822,-334.571946576839,-348.376528509856,-362.181110442873,-375.985692375889,-389.790274308906,-403.594856241922,-417.399438174939,-431.204020107956,-445.008602040972,-458.813183973989,-472.617765907005,-486.422347840022,-500.226929773039,-514.031511706055,-527.836093639072,-541.640675572089,-555.445257505105,-569.249839438122,-583.054421371139,-596.859003304156,-610.663585237172,-624.468167170189,-638.272749103205,-652.077331036222,-665.881912969239,-679.686494902255,-693.491076835272,-707.295658768288,-721.100240701305,-734.904822634322,-748.709404567338,-762.513986500355,-776.318568433372,-790.123150366388,-803.927732299405,-817.732314232422,-831.536896165439,-845.341478098455,-859.146060031472,-872.950641964488,-886.755223897505,-900.559805830522,-914.364387763538,-928.168969696555,-941.973551629571,-955.778133562588,-969.582715495605,-983.387297428621,-997.191879361638,-1010.99646129465,-1024.80104322767,-1038.60562516069,-1052.4102070937,-1066.21478902672,-1080.01937095974,-1093.82395289275,-1107.62853482577,-1121.43311675879,-1135.2376986918,-1149.04228062482,-1162.84686255784,-1176.65144449085,-1190.45602642387,-1204.26060835689,-1218.0651902899,-1231.86977222292,-1245.67435415594,-1259.47893608895,-1273.28351802197,-1287.08809995499,-1300.892681888,-1314.69726382102,-1328.50184575404,-1342.30642768705,-1356.11100962007,-1369.91559155309,-1383.7201734861,-1397.52475541912,-1411.32933735214,-1425.13391928515,-1438.93850121817,-1452.74308315119,-1466.5476650842,-1480.35224701722,-1494.15682895024,-1507.96141088325,-1521.76599281627,-1535.57057474929,-1549.3751566823,-1563.17973861532,-1576.98432054834,-1590.78890248135,-1604.59348441437,-1618.39806634739,-1632.2026482804,-1646.00723021342,-1659.81181214644,-1673.61639407945,-1687.42097601247,-1701.22555794549,-1715.0301398785,-1728.83472181152,-1742.63930374454,-1756.44388567755,-1770.24846761057,-1784.05304954359,-1797.8576314766,-1811.66221340962,-1825.46679534264,-1839.27137727565,-1853.07595920867,-1866.88054114169,-1880.6851230747,-1894.48970500772,-1908.29428694074,-1922.09886887375,-1935.90345080677,-1949.70803273979,-1963.5126146728,-1977.31719660582,-1991.12177853884,-2004.92636047185,-2018.73094240487,-2032.53552433789,-2046.3401062709,-2060.14468820392,-2073.94927013694,-2087.75385206995,-2101.55843400297,-2115.36301593599,-2129.167597869,-2142.97217980202,-2156.77676173504,-2170.58134366805,-2184.38592560107,-2198.19050753409,-2211.9950894671,-2225.79967140012,-2239.60425333314,-2253.40883526615,-2267.21341719917,-2281.01799913219,-2294.8225810652,-2308.62716299822,-2322.43174493124,-2336.23632686425,-2350.04090879727,-2363.84549073029,-2377.6500726633,-2391.45465459632,-2405.25923652934,-2419.06381846235,-2432.86840039537,-2446.67298232839,-2460.4775642614,-2474.28214619442,-2488.08672812744,-2501.89131006045,-2515.69589199347,-2529.50047392649,-2543.3050558595,-2557.10963779252,-2570.91421972554,-2584.71880165855,-2598.52338359157,-2612.32796552459,-2626.1325474576,-2639.93712939062,-2653.74171132364,-2667.54629325665,-2681.35087518967,-2695.15545712269,-2708.9600390557,-2722.76462098872,-2736.56920292174,-2750.37378485475,-2764.17836678777,-2777.98294872079,-2791.7875306538,-2805.59211258682,-2819.39669451984,-2833.20127645285,-2847.00585838587,-2860.81044031889,-2874.6150222519,-2888.41960418492,-2902.22418611794,-2916.02876805095,-2929.83334998397,-2943.63793191699,-2957.44251385,-2971.24709578302,-2985.05167771604,-2998.85625964905,-3012.66084158207,-3026.46542351509,-3040.2700054481,-3054.07458738112,-3067.87916931414,-3081.68375124715,-3095.48833318017,-3109.29291511319,-3123.0974970462,-3136.90207897922,-3150.70666091224,-3164.51124284525,-3178.31582477827,-3192.12040671129,-3205.9249886443,-3219.72957057732,-3233.53415251034,-3247.33873444335,-3261.14331637637,-3274.94789830939,-3288.7524802424,-3302.55706217542,-3316.36164410844,-3330.16622604145,-3343.97080797447,-3357.77538990749,-3371.5799718405,-3385.38455377352,-3399.18913570654,-3412.99371763955,-3426.79829957257,-3440.60288150559,-3454.4074634386,-3468.21204537162,-3482.01662730464,-3495.82120923765,-3509.62579117067,-3523.43037310369,-3537.2349550367,-3551.03953696972,-3564.84411890273,-3578.64870083575,-3592.45328276877,-3606.25786470178,-3620.0624466348,-3633.86702856782,-3647.67161050083,-3661.47619243385,-3675.28077436687,-3689.08535629989,-3702.8899382329,-3716.69452016592,-3730.49910209893,-3744.30368403195,-3744.30368403195],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0716063472274586,0.0738207520555695,0.0760698819366429,0.0783541478741841,0.080699539402517,0.0830874547553095,0.085514946832683,0.0880006978813943,0.0905446621336451,0.0931336507920541,0.0957768843388542,0.0984974689041722,0.101269407714894,0.104093426535531,0.10701034901114,0.10998931060004,0.11302744390769,0.116154511136285,0.119366407692071,0.122644937963889,0.126005558469934,0.129477178524398,0.133022941848602,0.136643652769798,0.140398582301833,0.144237830017339,0.148159146462887,0.152204808909627,0.156361996041087,0.160607623985551,0.164963878003383,0.169460434241701,0.174050685751538,0.178735142514466,0.183587377001903,0.188538002951797,0.193586383712675,0.198783134453932,0.204104264768571,0.209524882486332,0.215071271196647,0.220766508197685,0.226560908257331,0.232456237285371,0.238521940195863,0.244684251489471,0.250942713187099,0.257346339958507,0.263862854423446,0.270470290143718,0.277193106018224,0.28404210475485,0.290974454246566,0.297992990076933,0.305144826002637,0.312370178747862,0.319667825713041,0.327071977921336,0.334550995783469,0.342090086041668,0.34970406242311,0.357390655219701,0.365123283852556,0.372903215116446,0.380745458278297,0.388618211624543,0.396519712237559,0.404457227899683,0.412412394721582,0.420379573872032,0.428356865118483,0.436333846082144,0.444305547277184,0.452268426685029,0.460205314083865,0.468119581636642,0.476009375606639,0.483848934430711,0.491644761198428,0.499399399312764,0.507091174106642,0.51471032273755,0.522272720397629,0.529767928446781,0.537156767332195,0.544474996104076,0.551721240005022,0.558840886706271,0.565869921024282,0.572815703222356,0.579640496468184,0.586343797765322,0.592955323920387,0.599458647499078,0.60580949828359,0.612063137422342,0.618219157560453,0.624209579445983,0.63009273887567,0.635876643383971,0.641518332916884,0.647029897481034,0.652443945872502,0.657742588318412,0.662892871611752,0.667950636412703,0.672916582042881,0.677731333665425,0.682456979344939,0.687099186609831,0.69162390063528,0.696050972186182,0.700405452359528,0.704674363455423,0.70884414374518,0.71295407536045,0.717005607933341,0.720966333381163,0.724879998666881,0.72874937081252,0.732559751483414,0.736328294946901,0.740067076717419,0.743772670824491,0.747447814716522,0.7511074709312,0.754753113089904,0.758384960135945,0.762014704902621,0.76564368781759,0.769277906776896,0.77292278717694,0.776578587989741,0.78025152469423,0.783950803433039,0.787670732410679,0.791413155135256,0.79519902302762,0.799012993767869,0.802855686716085,0.806745904338441,0.810675561702972,0.814638499077258,0.818645917456177,0.822704028644772,0.826797293092624,0.830928034820077,0.835118078377674,0.839342368039463,0.843600603532173,0.847910595342491,0.852256973855442,0.85663312137287,0.861047037913095,0.865496753253034,0.869969159481633,0.874464981070782,0.878989339919396,0.883526386154351,0.888074841702119,0.892634142985115,0.897194234286588,0.901752397476704,0.906302445545182,0.910834163188396,0.915347800827675,0.919838027649156,0.92428053746644,0.928686192562105,0.933052811061704,0.937342391967219,0.941568280283873,0.945733767154576,0.94980633917761,0.95377371445158,0.957657609515784,0.96144081193514,0.96506584509623,0.968583279784894,0.97199047813327,0.975197891405313,0.978262125148578,0.981191846809798,0.983920403791127,0.986445769998565,0.98881324511461,0.990989736994631,0.992894855953736,0.994620602617065,0.99616478845413,0.997388173569599,0.998401266085453,0.999214701790754,0.999731612764717,0.999972733121129,1,0.999766318169746,0.999190366367637,0.998390938859911,0.997367232200274,0.995953521009141,0.99430350009281,0.992425346421573,0.990209623665378,0.98770263898715,0.984968842365276,0.981955894075898,0.978603106213704,0.975029825114057,0.971236946061998,0.967067791448629,0.962687376154739,0.958098846171993,0.953203728528904,0.948062774985304,0.942728923803701,0.937156075893083,0.931312849091226,0.925294798001577,0.919100525464881,0.912621946161544,0.905988501840747,0.899202436482387,0.892193097210472,0.885017495601376,0.877710146664247,0.87023896340547,0.862595679024221,0.854841314687569,0.846973136363774,0.838934953208395,0.830805341305789,0.82258631382594,0.814237808245026,0.805799876289615,0.797290219055607,0.788691313633352,0.780005391601725,0.771263266535543,0.762462775639363,0.75358148911745,0.744657153463772,0.735691028688024,0.726665045778106,0.717600629170522,0.708504891674752,0.69937018558948,0.690200109115604,0.681007085794442,0.671789961946626,0.662541931136884,0.653277635128098,0.643997713597249,0.634696007977611,0.625381726210118,0.616057222010955,0.606720549935308,0.59737472660891,0.588023624066157,0.578667465930092,0.569307724230248,0.559947802317978,0.550588287409406,0.541233136750211,0.531883913172642,0.522541076175191,0.513209926958059,0.503894346301556,0.49459239121864,0.485307976263559,0.476053687788283,0.466821809611011,0.457613383714866,0.448451316851368,0.439324253160011,0.430231303421666,0.421192362427254,0.412210303641025,0.40327450837408,0.394396871200595,0.38560471077654,0.376872097639493,0.368200510932404,0.35964314722156,0.351162544292099,0.342756882975744,0.334467417677503,0.326288420071158,0.318198085785515,0.31021947009471,0.302390081211902,0.294662131801829,0.287036939829649,0.27959881086385,0.272275470029127,0.265065655514252,0.258029971617488,0.251146281070306,0.244384401528228,0.23777673026267,0.231358543841054,0.225067452603177,0.218904874485099,0.212967365316375,0.207158908467887,0.201479440606142,0.195995959483829,0.190670342202021,0.185471743697395,0.180434522800739,0.175580397101028,0.170848060250069,0.166241094672834,0.161836359265374,0.157545301578539,0.153366859037452,0.149357572284798,0.145474058562757,0.141692460030113,0.13804041589807,0.134524057105393,0.131097410359044,0.127764454966473,0.124570616679786,0.121453549403372,0.11841184332204,0.115481623108968,0.11262781469894,0.109836535139257,0.107124509234659,0.104489103731482,0.101904237747778,0.0993727627185885,0.0969144998684116,0.0944961995670154,0.0921168334760378,0.0897944777402394,0.0875086280788768,0.0852532643104883,0.0830368063187378,0.0808560717224189,0.0786994756908189,0.0765689867340754,0.0744733077716462,0.0723975635150539,0.0703414273220776,0.0683157414144819,0.0663100572563929,0.0643220801138504,0.06235843295891,0.0604184949441129,0.0584960875748197,0.0565939627004548,0.0547209316279254,0.0528666211982275,0.0510312219238684,0.0492286757059932,0.0474488090435123,0.0456900993584053,0.0439625744622917,0.0422659365474278,0.0405931638120642,0.0389491681423726,0.0373456279260648,0.0357687325320718,0.0342187739946943,0.0327162835722299,0.0312447233818961,0.0298025505963951,0.0284039942761374,0.027045784468698,0.0257188502154347,0.0244300872202143,0.0231910951102797,0.0219845427174828,0.0208105180090458,0.0196928973318266,0.0186092075697742,0.0175582883538608,0.0165557523118813,0.0155946421445964,0.0146657484934811,0.0137764614712597,0.0129350012566958,0.012124478297734,0.0113447249433305,0.0106174548477736,0.00991940750440979,0.00925016375932206,0.00862323042630061,0.00802996427533277,0.00746309642690946,0.00692890122759313,0.00643141932721458,0.00595764615230366,0.00550778810974795,0.00509629898017929,0.00470569523624755,0.00433566967945121,0.00399576018496951,0.00367822485785239,0.00337846091751901,0.00310084549498801,0.00284651122084628,0.00260727684082491,0.0023835627599754,0.00218313274550142,0.00199535321381701,0.00181997909020146,0.0016627678692368,0.0015182615183226,0.00138404267204954,0.00126278406527493,0.0011544253629124,0.0010545666847204,0.000963634688381282,0.000885453790383916,0.000814335135071775,0.000750147419262282,0.000696320991088233,0.000649530983265004,0.000608641076972594,0.000575424960939483,0.000549758538939393,0.000529310303460091,0.000514556063747744,0.000507986015710666,0.000506288566980197,0.000509447022715772,0.000520431304255163,0.000536907383740725,0.000558257363611204,0.000586405605415267,0.00062152636419224,0.000661832083451551,0.000708120209680807,0.000763325991540249,0.000824293463707646,0.000891099447598922,0.000968056109208436,0.00105222965379472,0.00114307980784272,0.00124372592417617,0.00135435885386271,0.00147268582301766,0.00160021671354885,0.00174110873924318,0.00189083437611142,0.00204952109078403,0.00222409149945387,0.00240941021315211,0.00260488022931324,0.00281545896815566,0.00304057504517573,0.00327698105861494,0.00352713056683185,0.0037959496694361,0.00407704361742594,0.00437050759022884,0.00468567938148282,0.0050144533408791,0.00535626610882907,0.00571740917777996,0.00609571332890182,0.00648732042948068,0.00689521287080074,0.00732337124556068,0.0077645787566359,0.00821878117675115,0.00869498803862601,0.00918361669747526,0.0096842715246697,0.0102022287229126,0.0107338338493704,0.0112757951006806,0.0118301628682429,0.0123978528373237,0.012973496341047,0.0135568622122026,0.0141512458617734,0.0147504625905147,0.0153541212668966,0.0159626609977095,0.0165730097249611,0.0171839463806882,0.0177943982146783,0.0184014286034853,0.0190046912064639,0.0196034543611847,0.0201911983450485,0.0207704984456573,0.021340837664748,0.0218946338371187,0.0224330304990439,0.0229577059532526,0.0234628946841456,0.023943584378944,0.0244059914046707,0.0248482451625653,0.025255408526131,0.0256401863757887,0.0260021810156972,0.0263260714664202,0.0266199353434863,0.0268877451388781,0.0271197171667158,0.0273122074178674,0.0274763211127924,0.0276090413799266,0.0276931316721728,0.0277476197407119,0.0277724501868927,0],"text":["npv_sim: -3744.30368<br />scaled: 0.0277724502<br />1/2: 0.5","npv_sim: -3730.49910<br />scaled: 0.0277476197<br />1/2: 0.5","npv_sim: -3716.69452<br />scaled: 0.0276931317<br />1/2: 0.5","npv_sim: -3702.88994<br />scaled: 0.0276090414<br />1/2: 0.5","npv_sim: -3689.08536<br />scaled: 0.0274763211<br />1/2: 0.5","npv_sim: -3675.28077<br />scaled: 0.0273122074<br />1/2: 0.5","npv_sim: -3661.47619<br />scaled: 0.0271197172<br />1/2: 0.5","npv_sim: -3647.67161<br />scaled: 0.0268877451<br />1/2: 0.5","npv_sim: -3633.86703<br />scaled: 0.0266199353<br />1/2: 0.5","npv_sim: -3620.06245<br />scaled: 0.0263260715<br />1/2: 0.5","npv_sim: -3606.25786<br />scaled: 0.0260021810<br />1/2: 0.5","npv_sim: -3592.45328<br />scaled: 0.0256401864<br />1/2: 0.5","npv_sim: -3578.64870<br />scaled: 0.0252554085<br />1/2: 0.5","npv_sim: -3564.84412<br />scaled: 0.0248482452<br />1/2: 0.5","npv_sim: -3551.03954<br />scaled: 0.0244059914<br />1/2: 0.5","npv_sim: -3537.23496<br />scaled: 0.0239435844<br />1/2: 0.5","npv_sim: -3523.43037<br />scaled: 0.0234628947<br />1/2: 0.5","npv_sim: -3509.62579<br />scaled: 0.0229577060<br />1/2: 0.5","npv_sim: -3495.82121<br />scaled: 0.0224330305<br />1/2: 0.5","npv_sim: -3482.01663<br />scaled: 0.0218946338<br />1/2: 0.5","npv_sim: -3468.21205<br />scaled: 0.0213408377<br />1/2: 0.5","npv_sim: -3454.40746<br />scaled: 0.0207704984<br />1/2: 0.5","npv_sim: -3440.60288<br />scaled: 0.0201911983<br />1/2: 0.5","npv_sim: -3426.79830<br />scaled: 0.0196034544<br />1/2: 0.5","npv_sim: -3412.99372<br />scaled: 0.0190046912<br />1/2: 0.5","npv_sim: -3399.18914<br />scaled: 0.0184014286<br />1/2: 0.5","npv_sim: -3385.38455<br />scaled: 0.0177943982<br />1/2: 0.5","npv_sim: -3371.57997<br />scaled: 0.0171839464<br />1/2: 0.5","npv_sim: -3357.77539<br />scaled: 0.0165730097<br />1/2: 0.5","npv_sim: -3343.97081<br />scaled: 0.0159626610<br />1/2: 0.5","npv_sim: -3330.16623<br />scaled: 0.0153541213<br />1/2: 0.5","npv_sim: -3316.36164<br />scaled: 0.0147504626<br />1/2: 0.5","npv_sim: -3302.55706<br />scaled: 0.0141512459<br />1/2: 0.5","npv_sim: -3288.75248<br />scaled: 0.0135568622<br />1/2: 0.5","npv_sim: -3274.94790<br />scaled: 0.0129734963<br />1/2: 0.5","npv_sim: -3261.14332<br />scaled: 0.0123978528<br />1/2: 0.5","npv_sim: -3247.33873<br />scaled: 0.0118301629<br />1/2: 0.5","npv_sim: -3233.53415<br />scaled: 0.0112757951<br />1/2: 0.5","npv_sim: -3219.72957<br />scaled: 0.0107338338<br />1/2: 0.5","npv_sim: -3205.92499<br />scaled: 0.0102022287<br />1/2: 0.5","npv_sim: -3192.12041<br />scaled: 0.0096842715<br />1/2: 0.5","npv_sim: -3178.31582<br />scaled: 0.0091836167<br />1/2: 0.5","npv_sim: -3164.51124<br />scaled: 0.0086949880<br />1/2: 0.5","npv_sim: -3150.70666<br />scaled: 0.0082187812<br />1/2: 0.5","npv_sim: -3136.90208<br />scaled: 0.0077645788<br />1/2: 0.5","npv_sim: -3123.09750<br />scaled: 0.0073233712<br />1/2: 0.5","npv_sim: -3109.29292<br />scaled: 0.0068952129<br />1/2: 0.5","npv_sim: -3095.48833<br />scaled: 0.0064873204<br />1/2: 0.5","npv_sim: -3081.68375<br />scaled: 0.0060957133<br />1/2: 0.5","npv_sim: -3067.87917<br />scaled: 0.0057174092<br />1/2: 0.5","npv_sim: -3054.07459<br />scaled: 0.0053562661<br />1/2: 0.5","npv_sim: -3040.27001<br />scaled: 0.0050144533<br />1/2: 0.5","npv_sim: -3026.46542<br />scaled: 0.0046856794<br />1/2: 0.5","npv_sim: -3012.66084<br />scaled: 0.0043705076<br />1/2: 0.5","npv_sim: -2998.85626<br />scaled: 0.0040770436<br />1/2: 0.5","npv_sim: -2985.05168<br />scaled: 0.0037959497<br />1/2: 0.5","npv_sim: -2971.24710<br />scaled: 0.0035271306<br />1/2: 0.5","npv_sim: -2957.44251<br />scaled: 0.0032769811<br />1/2: 0.5","npv_sim: -2943.63793<br />scaled: 0.0030405750<br />1/2: 0.5","npv_sim: -2929.83335<br />scaled: 0.0028154590<br />1/2: 0.5","npv_sim: -2916.02877<br />scaled: 0.0026048802<br />1/2: 0.5","npv_sim: -2902.22419<br />scaled: 0.0024094102<br />1/2: 0.5","npv_sim: -2888.41960<br />scaled: 0.0022240915<br />1/2: 0.5","npv_sim: -2874.61502<br />scaled: 0.0020495211<br />1/2: 0.5","npv_sim: -2860.81044<br />scaled: 0.0018908344<br />1/2: 0.5","npv_sim: -2847.00586<br />scaled: 0.0017411087<br />1/2: 0.5","npv_sim: -2833.20128<br />scaled: 0.0016002167<br />1/2: 0.5","npv_sim: -2819.39669<br />scaled: 0.0014726858<br />1/2: 0.5","npv_sim: -2805.59211<br />scaled: 0.0013543589<br />1/2: 0.5","npv_sim: -2791.78753<br />scaled: 0.0012437259<br />1/2: 0.5","npv_sim: -2777.98295<br />scaled: 0.0011430798<br />1/2: 0.5","npv_sim: -2764.17837<br />scaled: 0.0010522297<br />1/2: 0.5","npv_sim: -2750.37378<br />scaled: 0.0009680561<br />1/2: 0.5","npv_sim: -2736.56920<br />scaled: 0.0008910994<br />1/2: 0.5","npv_sim: -2722.76462<br />scaled: 0.0008242935<br />1/2: 0.5","npv_sim: -2708.96004<br />scaled: 0.0007633260<br />1/2: 0.5","npv_sim: -2695.15546<br />scaled: 0.0007081202<br />1/2: 0.5","npv_sim: -2681.35088<br />scaled: 0.0006618321<br />1/2: 0.5","npv_sim: -2667.54629<br />scaled: 0.0006215264<br />1/2: 0.5","npv_sim: -2653.74171<br />scaled: 0.0005864056<br />1/2: 0.5","npv_sim: -2639.93713<br />scaled: 0.0005582574<br />1/2: 0.5","npv_sim: -2626.13255<br />scaled: 0.0005369074<br />1/2: 0.5","npv_sim: -2612.32797<br />scaled: 0.0005204313<br />1/2: 0.5","npv_sim: -2598.52338<br />scaled: 0.0005094470<br />1/2: 0.5","npv_sim: -2584.71880<br />scaled: 0.0005062886<br />1/2: 0.5","npv_sim: -2570.91422<br />scaled: 0.0005079860<br />1/2: 0.5","npv_sim: -2557.10964<br />scaled: 0.0005145561<br />1/2: 0.5","npv_sim: -2543.30506<br />scaled: 0.0005293103<br />1/2: 0.5","npv_sim: -2529.50047<br />scaled: 0.0005497585<br />1/2: 0.5","npv_sim: -2515.69589<br />scaled: 0.0005754250<br />1/2: 0.5","npv_sim: -2501.89131<br />scaled: 0.0006086411<br />1/2: 0.5","npv_sim: -2488.08673<br />scaled: 0.0006495310<br />1/2: 0.5","npv_sim: -2474.28215<br />scaled: 0.0006963210<br />1/2: 0.5","npv_sim: -2460.47756<br />scaled: 0.0007501474<br />1/2: 0.5","npv_sim: -2446.67298<br />scaled: 0.0008143351<br />1/2: 0.5","npv_sim: -2432.86840<br />scaled: 0.0008854538<br />1/2: 0.5","npv_sim: -2419.06382<br />scaled: 0.0009636347<br />1/2: 0.5","npv_sim: -2405.25924<br />scaled: 0.0010545667<br />1/2: 0.5","npv_sim: -2391.45465<br />scaled: 0.0011544254<br />1/2: 0.5","npv_sim: -2377.65007<br />scaled: 0.0012627841<br />1/2: 0.5","npv_sim: -2363.84549<br />scaled: 0.0013840427<br />1/2: 0.5","npv_sim: -2350.04091<br />scaled: 0.0015182615<br />1/2: 0.5","npv_sim: -2336.23633<br />scaled: 0.0016627679<br />1/2: 0.5","npv_sim: -2322.43174<br />scaled: 0.0018199791<br />1/2: 0.5","npv_sim: -2308.62716<br />scaled: 0.0019953532<br />1/2: 0.5","npv_sim: -2294.82258<br />scaled: 0.0021831327<br />1/2: 0.5","npv_sim: -2281.01800<br />scaled: 0.0023835628<br />1/2: 0.5","npv_sim: -2267.21342<br />scaled: 0.0026072768<br />1/2: 0.5","npv_sim: -2253.40884<br />scaled: 0.0028465112<br />1/2: 0.5","npv_sim: -2239.60425<br />scaled: 0.0031008455<br />1/2: 0.5","npv_sim: -2225.79967<br />scaled: 0.0033784609<br />1/2: 0.5","npv_sim: -2211.99509<br />scaled: 0.0036782249<br />1/2: 0.5","npv_sim: -2198.19051<br />scaled: 0.0039957602<br />1/2: 0.5","npv_sim: -2184.38593<br />scaled: 0.0043356697<br />1/2: 0.5","npv_sim: -2170.58134<br />scaled: 0.0047056952<br />1/2: 0.5","npv_sim: -2156.77676<br />scaled: 0.0050962990<br />1/2: 0.5","npv_sim: -2142.97218<br />scaled: 0.0055077881<br />1/2: 0.5","npv_sim: -2129.16760<br />scaled: 0.0059576462<br />1/2: 0.5","npv_sim: -2115.36302<br />scaled: 0.0064314193<br />1/2: 0.5","npv_sim: -2101.55843<br />scaled: 0.0069289012<br />1/2: 0.5","npv_sim: -2087.75385<br />scaled: 0.0074630964<br />1/2: 0.5","npv_sim: -2073.94927<br />scaled: 0.0080299643<br />1/2: 0.5","npv_sim: -2060.14469<br />scaled: 0.0086232304<br />1/2: 0.5","npv_sim: -2046.34011<br />scaled: 0.0092501638<br />1/2: 0.5","npv_sim: -2032.53552<br />scaled: 0.0099194075<br />1/2: 0.5","npv_sim: -2018.73094<br />scaled: 0.0106174548<br />1/2: 0.5","npv_sim: -2004.92636<br />scaled: 0.0113447249<br />1/2: 0.5","npv_sim: -1991.12178<br />scaled: 0.0121244783<br />1/2: 0.5","npv_sim: -1977.31720<br />scaled: 0.0129350013<br />1/2: 0.5","npv_sim: -1963.51261<br />scaled: 0.0137764615<br />1/2: 0.5","npv_sim: -1949.70803<br />scaled: 0.0146657485<br />1/2: 0.5","npv_sim: -1935.90345<br />scaled: 0.0155946421<br />1/2: 0.5","npv_sim: -1922.09887<br />scaled: 0.0165557523<br />1/2: 0.5","npv_sim: -1908.29429<br />scaled: 0.0175582884<br />1/2: 0.5","npv_sim: -1894.48971<br />scaled: 0.0186092076<br />1/2: 0.5","npv_sim: -1880.68512<br />scaled: 0.0196928973<br />1/2: 0.5","npv_sim: -1866.88054<br />scaled: 0.0208105180<br />1/2: 0.5","npv_sim: -1853.07596<br />scaled: 0.0219845427<br />1/2: 0.5","npv_sim: -1839.27138<br />scaled: 0.0231910951<br />1/2: 0.5","npv_sim: -1825.46680<br />scaled: 0.0244300872<br />1/2: 0.5","npv_sim: -1811.66221<br />scaled: 0.0257188502<br />1/2: 0.5","npv_sim: -1797.85763<br />scaled: 0.0270457845<br />1/2: 0.5","npv_sim: -1784.05305<br />scaled: 0.0284039943<br />1/2: 0.5","npv_sim: -1770.24847<br />scaled: 0.0298025506<br />1/2: 0.5","npv_sim: -1756.44389<br />scaled: 0.0312447234<br />1/2: 0.5","npv_sim: -1742.63930<br />scaled: 0.0327162836<br />1/2: 0.5","npv_sim: -1728.83472<br />scaled: 0.0342187740<br />1/2: 0.5","npv_sim: -1715.03014<br />scaled: 0.0357687325<br />1/2: 0.5","npv_sim: -1701.22556<br />scaled: 0.0373456279<br />1/2: 0.5","npv_sim: -1687.42098<br />scaled: 0.0389491681<br />1/2: 0.5","npv_sim: -1673.61639<br />scaled: 0.0405931638<br />1/2: 0.5","npv_sim: -1659.81181<br />scaled: 0.0422659365<br />1/2: 0.5","npv_sim: -1646.00723<br />scaled: 0.0439625745<br />1/2: 0.5","npv_sim: -1632.20265<br />scaled: 0.0456900994<br />1/2: 0.5","npv_sim: -1618.39807<br />scaled: 0.0474488090<br />1/2: 0.5","npv_sim: -1604.59348<br />scaled: 0.0492286757<br />1/2: 0.5","npv_sim: -1590.78890<br />scaled: 0.0510312219<br />1/2: 0.5","npv_sim: -1576.98432<br />scaled: 0.0528666212<br />1/2: 0.5","npv_sim: -1563.17974<br />scaled: 0.0547209316<br />1/2: 0.5","npv_sim: -1549.37516<br />scaled: 0.0565939627<br />1/2: 0.5","npv_sim: -1535.57057<br />scaled: 0.0584960876<br />1/2: 0.5","npv_sim: -1521.76599<br />scaled: 0.0604184949<br />1/2: 0.5","npv_sim: -1507.96141<br />scaled: 0.0623584330<br />1/2: 0.5","npv_sim: -1494.15683<br />scaled: 0.0643220801<br />1/2: 0.5","npv_sim: -1480.35225<br />scaled: 0.0663100573<br />1/2: 0.5","npv_sim: -1466.54767<br />scaled: 0.0683157414<br />1/2: 0.5","npv_sim: -1452.74308<br />scaled: 0.0703414273<br />1/2: 0.5","npv_sim: -1438.93850<br />scaled: 0.0723975635<br />1/2: 0.5","npv_sim: -1425.13392<br />scaled: 0.0744733078<br />1/2: 0.5","npv_sim: -1411.32934<br />scaled: 0.0765689867<br />1/2: 0.5","npv_sim: -1397.52476<br />scaled: 0.0786994757<br />1/2: 0.5","npv_sim: -1383.72017<br />scaled: 0.0808560717<br />1/2: 0.5","npv_sim: -1369.91559<br />scaled: 0.0830368063<br />1/2: 0.5","npv_sim: -1356.11101<br />scaled: 0.0852532643<br />1/2: 0.5","npv_sim: -1342.30643<br />scaled: 0.0875086281<br />1/2: 0.5","npv_sim: -1328.50185<br />scaled: 0.0897944777<br />1/2: 0.5","npv_sim: -1314.69726<br />scaled: 0.0921168335<br />1/2: 0.5","npv_sim: -1300.89268<br />scaled: 0.0944961996<br />1/2: 0.5","npv_sim: -1287.08810<br />scaled: 0.0969144999<br />1/2: 0.5","npv_sim: -1273.28352<br />scaled: 0.0993727627<br />1/2: 0.5","npv_sim: -1259.47894<br />scaled: 0.1019042377<br />1/2: 0.5","npv_sim: -1245.67435<br />scaled: 0.1044891037<br />1/2: 0.5","npv_sim: -1231.86977<br />scaled: 0.1071245092<br />1/2: 0.5","npv_sim: -1218.06519<br />scaled: 0.1098365351<br />1/2: 0.5","npv_sim: -1204.26061<br />scaled: 0.1126278147<br />1/2: 0.5","npv_sim: -1190.45603<br />scaled: 0.1154816231<br />1/2: 0.5","npv_sim: -1176.65144<br />scaled: 0.1184118433<br />1/2: 0.5","npv_sim: -1162.84686<br />scaled: 0.1214535494<br />1/2: 0.5","npv_sim: -1149.04228<br />scaled: 0.1245706167<br />1/2: 0.5","npv_sim: -1135.23770<br />scaled: 0.1277644550<br />1/2: 0.5","npv_sim: -1121.43312<br />scaled: 0.1310974104<br />1/2: 0.5","npv_sim: -1107.62853<br />scaled: 0.1345240571<br />1/2: 0.5","npv_sim: -1093.82395<br />scaled: 0.1380404159<br />1/2: 0.5","npv_sim: -1080.01937<br />scaled: 0.1416924600<br />1/2: 0.5","npv_sim: -1066.21479<br />scaled: 0.1454740586<br />1/2: 0.5","npv_sim: -1052.41021<br />scaled: 0.1493575723<br />1/2: 0.5","npv_sim: -1038.60563<br />scaled: 0.1533668590<br />1/2: 0.5","npv_sim: -1024.80104<br />scaled: 0.1575453016<br />1/2: 0.5","npv_sim: -1010.99646<br />scaled: 0.1618363593<br />1/2: 0.5","npv_sim:  -997.19188<br />scaled: 0.1662410947<br />1/2: 0.5","npv_sim:  -983.38730<br />scaled: 0.1708480603<br />1/2: 0.5","npv_sim:  -969.58272<br />scaled: 0.1755803971<br />1/2: 0.5","npv_sim:  -955.77813<br />scaled: 0.1804345228<br />1/2: 0.5","npv_sim:  -941.97355<br />scaled: 0.1854717437<br />1/2: 0.5","npv_sim:  -928.16897<br />scaled: 0.1906703422<br />1/2: 0.5","npv_sim:  -914.36439<br />scaled: 0.1959959595<br />1/2: 0.5","npv_sim:  -900.55981<br />scaled: 0.2014794406<br />1/2: 0.5","npv_sim:  -886.75522<br />scaled: 0.2071589085<br />1/2: 0.5","npv_sim:  -872.95064<br />scaled: 0.2129673653<br />1/2: 0.5","npv_sim:  -859.14606<br />scaled: 0.2189048745<br />1/2: 0.5","npv_sim:  -845.34148<br />scaled: 0.2250674526<br />1/2: 0.5","npv_sim:  -831.53690<br />scaled: 0.2313585438<br />1/2: 0.5","npv_sim:  -817.73231<br />scaled: 0.2377767303<br />1/2: 0.5","npv_sim:  -803.92773<br />scaled: 0.2443844015<br />1/2: 0.5","npv_sim:  -790.12315<br />scaled: 0.2511462811<br />1/2: 0.5","npv_sim:  -776.31857<br />scaled: 0.2580299716<br />1/2: 0.5","npv_sim:  -762.51399<br />scaled: 0.2650656555<br />1/2: 0.5","npv_sim:  -748.70940<br />scaled: 0.2722754700<br />1/2: 0.5","npv_sim:  -734.90482<br />scaled: 0.2795988109<br />1/2: 0.5","npv_sim:  -721.10024<br />scaled: 0.2870369398<br />1/2: 0.5","npv_sim:  -707.29566<br />scaled: 0.2946621318<br />1/2: 0.5","npv_sim:  -693.49108<br />scaled: 0.3023900812<br />1/2: 0.5","npv_sim:  -679.68649<br />scaled: 0.3102194701<br />1/2: 0.5","npv_sim:  -665.88191<br />scaled: 0.3181980858<br />1/2: 0.5","npv_sim:  -652.07733<br />scaled: 0.3262884201<br />1/2: 0.5","npv_sim:  -638.27275<br />scaled: 0.3344674177<br />1/2: 0.5","npv_sim:  -624.46817<br />scaled: 0.3427568830<br />1/2: 0.5","npv_sim:  -610.66359<br />scaled: 0.3511625443<br />1/2: 0.5","npv_sim:  -596.85900<br />scaled: 0.3596431472<br />1/2: 0.5","npv_sim:  -583.05442<br />scaled: 0.3682005109<br />1/2: 0.5","npv_sim:  -569.24984<br />scaled: 0.3768720976<br />1/2: 0.5","npv_sim:  -555.44526<br />scaled: 0.3856047108<br />1/2: 0.5","npv_sim:  -541.64068<br />scaled: 0.3943968712<br />1/2: 0.5","npv_sim:  -527.83609<br />scaled: 0.4032745084<br />1/2: 0.5","npv_sim:  -514.03151<br />scaled: 0.4122103036<br />1/2: 0.5","npv_sim:  -500.22693<br />scaled: 0.4211923624<br />1/2: 0.5","npv_sim:  -486.42235<br />scaled: 0.4302313034<br />1/2: 0.5","npv_sim:  -472.61777<br />scaled: 0.4393242532<br />1/2: 0.5","npv_sim:  -458.81318<br />scaled: 0.4484513169<br />1/2: 0.5","npv_sim:  -445.00860<br />scaled: 0.4576133837<br />1/2: 0.5","npv_sim:  -431.20402<br />scaled: 0.4668218096<br />1/2: 0.5","npv_sim:  -417.39944<br />scaled: 0.4760536878<br />1/2: 0.5","npv_sim:  -403.59486<br />scaled: 0.4853079763<br />1/2: 0.5","npv_sim:  -389.79027<br />scaled: 0.4945923912<br />1/2: 0.5","npv_sim:  -375.98569<br />scaled: 0.5038943463<br />1/2: 0.5","npv_sim:  -362.18111<br />scaled: 0.5132099270<br />1/2: 0.5","npv_sim:  -348.37653<br />scaled: 0.5225410762<br />1/2: 0.5","npv_sim:  -334.57195<br />scaled: 0.5318839132<br />1/2: 0.5","npv_sim:  -320.76736<br />scaled: 0.5412331368<br />1/2: 0.5","npv_sim:  -306.96278<br />scaled: 0.5505882874<br />1/2: 0.5","npv_sim:  -293.15820<br />scaled: 0.5599478023<br />1/2: 0.5","npv_sim:  -279.35362<br />scaled: 0.5693077242<br />1/2: 0.5","npv_sim:  -265.54904<br />scaled: 0.5786674659<br />1/2: 0.5","npv_sim:  -251.74445<br />scaled: 0.5880236241<br />1/2: 0.5","npv_sim:  -237.93987<br />scaled: 0.5973747266<br />1/2: 0.5","npv_sim:  -224.13529<br />scaled: 0.6067205499<br />1/2: 0.5","npv_sim:  -210.33071<br />scaled: 0.6160572220<br />1/2: 0.5","npv_sim:  -196.52613<br />scaled: 0.6253817262<br />1/2: 0.5","npv_sim:  -182.72155<br />scaled: 0.6346960080<br />1/2: 0.5","npv_sim:  -168.91696<br />scaled: 0.6439977136<br />1/2: 0.5","npv_sim:  -155.11238<br />scaled: 0.6532776351<br />1/2: 0.5","npv_sim:  -141.30780<br />scaled: 0.6625419311<br />1/2: 0.5","npv_sim:  -127.50322<br />scaled: 0.6717899619<br />1/2: 0.5","npv_sim:  -113.69864<br />scaled: 0.6810070858<br />1/2: 0.5","npv_sim:   -99.89405<br />scaled: 0.6902001091<br />1/2: 0.5","npv_sim:   -86.08947<br />scaled: 0.6993701856<br />1/2: 0.5","npv_sim:   -72.28489<br />scaled: 0.7085048917<br />1/2: 0.5","npv_sim:   -58.48031<br />scaled: 0.7176006292<br />1/2: 0.5","npv_sim:   -44.67573<br />scaled: 0.7266650458<br />1/2: 0.5","npv_sim:   -30.87114<br />scaled: 0.7356910287<br />1/2: 0.5","npv_sim:   -17.06656<br />scaled: 0.7446571535<br />1/2: 0.5","npv_sim:    -3.26198<br />scaled: 0.7535814891<br />1/2: 0.5","npv_sim:    10.54260<br />scaled: 0.7624627756<br />1/2: 0.5","npv_sim:    24.34718<br />scaled: 0.7712632665<br />1/2: 0.5","npv_sim:    38.15177<br />scaled: 0.7800053916<br />1/2: 0.5","npv_sim:    51.95635<br />scaled: 0.7886913136<br />1/2: 0.5","npv_sim:    65.76093<br />scaled: 0.7972902191<br />1/2: 0.5","npv_sim:    79.56551<br />scaled: 0.8057998763<br />1/2: 0.5","npv_sim:    93.37009<br />scaled: 0.8142378082<br />1/2: 0.5","npv_sim:   107.17468<br />scaled: 0.8225863138<br />1/2: 0.5","npv_sim:   120.97926<br />scaled: 0.8308053413<br />1/2: 0.5","npv_sim:   134.78384<br />scaled: 0.8389349532<br />1/2: 0.5","npv_sim:   148.58842<br />scaled: 0.8469731364<br />1/2: 0.5","npv_sim:   162.39300<br />scaled: 0.8548413147<br />1/2: 0.5","npv_sim:   176.19758<br />scaled: 0.8625956790<br />1/2: 0.5","npv_sim:   190.00217<br />scaled: 0.8702389634<br />1/2: 0.5","npv_sim:   203.80675<br />scaled: 0.8777101467<br />1/2: 0.5","npv_sim:   217.61133<br />scaled: 0.8850174956<br />1/2: 0.5","npv_sim:   231.41591<br />scaled: 0.8921930972<br />1/2: 0.5","npv_sim:   245.22049<br />scaled: 0.8992024365<br />1/2: 0.5","npv_sim:   259.02508<br />scaled: 0.9059885018<br />1/2: 0.5","npv_sim:   272.82966<br />scaled: 0.9126219462<br />1/2: 0.5","npv_sim:   286.63424<br />scaled: 0.9191005255<br />1/2: 0.5","npv_sim:   300.43882<br />scaled: 0.9252947980<br />1/2: 0.5","npv_sim:   314.24340<br />scaled: 0.9313128491<br />1/2: 0.5","npv_sim:   328.04799<br />scaled: 0.9371560759<br />1/2: 0.5","npv_sim:   341.85257<br />scaled: 0.9427289238<br />1/2: 0.5","npv_sim:   355.65715<br />scaled: 0.9480627750<br />1/2: 0.5","npv_sim:   369.46173<br />scaled: 0.9532037285<br />1/2: 0.5","npv_sim:   383.26631<br />scaled: 0.9580988462<br />1/2: 0.5","npv_sim:   397.07090<br />scaled: 0.9626873762<br />1/2: 0.5","npv_sim:   410.87548<br />scaled: 0.9670677914<br />1/2: 0.5","npv_sim:   424.68006<br />scaled: 0.9712369461<br />1/2: 0.5","npv_sim:   438.48464<br />scaled: 0.9750298251<br />1/2: 0.5","npv_sim:   452.28922<br />scaled: 0.9786031062<br />1/2: 0.5","npv_sim:   466.09381<br />scaled: 0.9819558941<br />1/2: 0.5","npv_sim:   479.89839<br />scaled: 0.9849688424<br />1/2: 0.5","npv_sim:   493.70297<br />scaled: 0.9877026390<br />1/2: 0.5","npv_sim:   507.50755<br />scaled: 0.9902096237<br />1/2: 0.5","npv_sim:   521.31213<br />scaled: 0.9924253464<br />1/2: 0.5","npv_sim:   535.11672<br />scaled: 0.9943035001<br />1/2: 0.5","npv_sim:   548.92130<br />scaled: 0.9959535210<br />1/2: 0.5","npv_sim:   562.72588<br />scaled: 0.9973672322<br />1/2: 0.5","npv_sim:   576.53046<br />scaled: 0.9983909389<br />1/2: 0.5","npv_sim:   590.33504<br />scaled: 0.9991903664<br />1/2: 0.5","npv_sim:   604.13962<br />scaled: 0.9997663182<br />1/2: 0.5","npv_sim:   617.94421<br />scaled: 1.0000000000<br />1/2: 0.5","npv_sim:   631.74879<br />scaled: 0.9999727331<br />1/2: 0.5","npv_sim:   645.55337<br />scaled: 0.9997316128<br />1/2: 0.5","npv_sim:   659.35795<br />scaled: 0.9992147018<br />1/2: 0.5","npv_sim:   673.16253<br />scaled: 0.9984012661<br />1/2: 0.5","npv_sim:   686.96712<br />scaled: 0.9973881736<br />1/2: 0.5","npv_sim:   700.77170<br />scaled: 0.9961647885<br />1/2: 0.5","npv_sim:   714.57628<br />scaled: 0.9946206026<br />1/2: 0.5","npv_sim:   728.38086<br />scaled: 0.9928948560<br />1/2: 0.5","npv_sim:   742.18544<br />scaled: 0.9909897370<br />1/2: 0.5","npv_sim:   755.99003<br />scaled: 0.9888132451<br />1/2: 0.5","npv_sim:   769.79461<br />scaled: 0.9864457700<br />1/2: 0.5","npv_sim:   783.59919<br />scaled: 0.9839204038<br />1/2: 0.5","npv_sim:   797.40377<br />scaled: 0.9811918468<br />1/2: 0.5","npv_sim:   811.20835<br />scaled: 0.9782621251<br />1/2: 0.5","npv_sim:   825.01294<br />scaled: 0.9751978914<br />1/2: 0.5","npv_sim:   838.81752<br />scaled: 0.9719904781<br />1/2: 0.5","npv_sim:   852.62210<br />scaled: 0.9685832798<br />1/2: 0.5","npv_sim:   866.42668<br />scaled: 0.9650658451<br />1/2: 0.5","npv_sim:   880.23126<br />scaled: 0.9614408119<br />1/2: 0.5","npv_sim:   894.03585<br />scaled: 0.9576576095<br />1/2: 0.5","npv_sim:   907.84043<br />scaled: 0.9537737145<br />1/2: 0.5","npv_sim:   921.64501<br />scaled: 0.9498063392<br />1/2: 0.5","npv_sim:   935.44959<br />scaled: 0.9457337672<br />1/2: 0.5","npv_sim:   949.25417<br />scaled: 0.9415682803<br />1/2: 0.5","npv_sim:   963.05876<br />scaled: 0.9373423920<br />1/2: 0.5","npv_sim:   976.86334<br />scaled: 0.9330528111<br />1/2: 0.5","npv_sim:   990.66792<br />scaled: 0.9286861926<br />1/2: 0.5","npv_sim:  1004.47250<br />scaled: 0.9242805375<br />1/2: 0.5","npv_sim:  1018.27708<br />scaled: 0.9198380276<br />1/2: 0.5","npv_sim:  1032.08166<br />scaled: 0.9153478008<br />1/2: 0.5","npv_sim:  1045.88625<br />scaled: 0.9108341632<br />1/2: 0.5","npv_sim:  1059.69083<br />scaled: 0.9063024455<br />1/2: 0.5","npv_sim:  1073.49541<br />scaled: 0.9017523975<br />1/2: 0.5","npv_sim:  1087.29999<br />scaled: 0.8971942343<br />1/2: 0.5","npv_sim:  1101.10457<br />scaled: 0.8926341430<br />1/2: 0.5","npv_sim:  1114.90916<br />scaled: 0.8880748417<br />1/2: 0.5","npv_sim:  1128.71374<br />scaled: 0.8835263862<br />1/2: 0.5","npv_sim:  1142.51832<br />scaled: 0.8789893399<br />1/2: 0.5","npv_sim:  1156.32290<br />scaled: 0.8744649811<br />1/2: 0.5","npv_sim:  1170.12748<br />scaled: 0.8699691595<br />1/2: 0.5","npv_sim:  1183.93207<br />scaled: 0.8654967533<br />1/2: 0.5","npv_sim:  1197.73665<br />scaled: 0.8610470379<br />1/2: 0.5","npv_sim:  1211.54123<br />scaled: 0.8566331214<br />1/2: 0.5","npv_sim:  1225.34581<br />scaled: 0.8522569739<br />1/2: 0.5","npv_sim:  1239.15039<br />scaled: 0.8479105953<br />1/2: 0.5","npv_sim:  1252.95498<br />scaled: 0.8436006035<br />1/2: 0.5","npv_sim:  1266.75956<br />scaled: 0.8393423680<br />1/2: 0.5","npv_sim:  1280.56414<br />scaled: 0.8351180784<br />1/2: 0.5","npv_sim:  1294.36872<br />scaled: 0.8309280348<br />1/2: 0.5","npv_sim:  1308.17330<br />scaled: 0.8267972931<br />1/2: 0.5","npv_sim:  1321.97789<br />scaled: 0.8227040286<br />1/2: 0.5","npv_sim:  1335.78247<br />scaled: 0.8186459175<br />1/2: 0.5","npv_sim:  1349.58705<br />scaled: 0.8146384991<br />1/2: 0.5","npv_sim:  1363.39163<br />scaled: 0.8106755617<br />1/2: 0.5","npv_sim:  1377.19621<br />scaled: 0.8067459043<br />1/2: 0.5","npv_sim:  1391.00080<br />scaled: 0.8028556867<br />1/2: 0.5","npv_sim:  1404.80538<br />scaled: 0.7990129938<br />1/2: 0.5","npv_sim:  1418.60996<br />scaled: 0.7951990230<br />1/2: 0.5","npv_sim:  1432.41454<br />scaled: 0.7914131551<br />1/2: 0.5","npv_sim:  1446.21912<br />scaled: 0.7876707324<br />1/2: 0.5","npv_sim:  1460.02370<br />scaled: 0.7839508034<br />1/2: 0.5","npv_sim:  1473.82829<br />scaled: 0.7802515247<br />1/2: 0.5","npv_sim:  1487.63287<br />scaled: 0.7765785880<br />1/2: 0.5","npv_sim:  1501.43745<br />scaled: 0.7729227872<br />1/2: 0.5","npv_sim:  1515.24203<br />scaled: 0.7692779068<br />1/2: 0.5","npv_sim:  1529.04661<br />scaled: 0.7656436878<br />1/2: 0.5","npv_sim:  1542.85120<br />scaled: 0.7620147049<br />1/2: 0.5","npv_sim:  1556.65578<br />scaled: 0.7583849601<br />1/2: 0.5","npv_sim:  1570.46036<br />scaled: 0.7547531131<br />1/2: 0.5","npv_sim:  1584.26494<br />scaled: 0.7511074709<br />1/2: 0.5","npv_sim:  1598.06952<br />scaled: 0.7474478147<br />1/2: 0.5","npv_sim:  1611.87411<br />scaled: 0.7437726708<br />1/2: 0.5","npv_sim:  1625.67869<br />scaled: 0.7400670767<br />1/2: 0.5","npv_sim:  1639.48327<br />scaled: 0.7363282949<br />1/2: 0.5","npv_sim:  1653.28785<br />scaled: 0.7325597515<br />1/2: 0.5","npv_sim:  1667.09243<br />scaled: 0.7287493708<br />1/2: 0.5","npv_sim:  1680.89702<br />scaled: 0.7248799987<br />1/2: 0.5","npv_sim:  1694.70160<br />scaled: 0.7209663334<br />1/2: 0.5","npv_sim:  1708.50618<br />scaled: 0.7170056079<br />1/2: 0.5","npv_sim:  1722.31076<br />scaled: 0.7129540754<br />1/2: 0.5","npv_sim:  1736.11534<br />scaled: 0.7088441437<br />1/2: 0.5","npv_sim:  1749.91993<br />scaled: 0.7046743635<br />1/2: 0.5","npv_sim:  1763.72451<br />scaled: 0.7004054524<br />1/2: 0.5","npv_sim:  1777.52909<br />scaled: 0.6960509722<br />1/2: 0.5","npv_sim:  1791.33367<br />scaled: 0.6916239006<br />1/2: 0.5","npv_sim:  1805.13825<br />scaled: 0.6870991866<br />1/2: 0.5","npv_sim:  1818.94283<br />scaled: 0.6824569793<br />1/2: 0.5","npv_sim:  1832.74742<br />scaled: 0.6777313337<br />1/2: 0.5","npv_sim:  1846.55200<br />scaled: 0.6729165820<br />1/2: 0.5","npv_sim:  1860.35658<br />scaled: 0.6679506364<br />1/2: 0.5","npv_sim:  1874.16116<br />scaled: 0.6628928716<br />1/2: 0.5","npv_sim:  1887.96574<br />scaled: 0.6577425883<br />1/2: 0.5","npv_sim:  1901.77033<br />scaled: 0.6524439459<br />1/2: 0.5","npv_sim:  1915.57491<br />scaled: 0.6470298975<br />1/2: 0.5","npv_sim:  1929.37949<br />scaled: 0.6415183329<br />1/2: 0.5","npv_sim:  1943.18407<br />scaled: 0.6358766434<br />1/2: 0.5","npv_sim:  1956.98865<br />scaled: 0.6300927389<br />1/2: 0.5","npv_sim:  1970.79324<br />scaled: 0.6242095794<br />1/2: 0.5","npv_sim:  1984.59782<br />scaled: 0.6182191576<br />1/2: 0.5","npv_sim:  1998.40240<br />scaled: 0.6120631374<br />1/2: 0.5","npv_sim:  2012.20698<br />scaled: 0.6058094983<br />1/2: 0.5","npv_sim:  2026.01156<br />scaled: 0.5994586475<br />1/2: 0.5","npv_sim:  2039.81615<br />scaled: 0.5929553239<br />1/2: 0.5","npv_sim:  2053.62073<br />scaled: 0.5863437978<br />1/2: 0.5","npv_sim:  2067.42531<br />scaled: 0.5796404965<br />1/2: 0.5","npv_sim:  2081.22989<br />scaled: 0.5728157032<br />1/2: 0.5","npv_sim:  2095.03447<br />scaled: 0.5658699210<br />1/2: 0.5","npv_sim:  2108.83906<br />scaled: 0.5588408867<br />1/2: 0.5","npv_sim:  2122.64364<br />scaled: 0.5517212400<br />1/2: 0.5","npv_sim:  2136.44822<br />scaled: 0.5444749961<br />1/2: 0.5","npv_sim:  2150.25280<br />scaled: 0.5371567673<br />1/2: 0.5","npv_sim:  2164.05738<br />scaled: 0.5297679284<br />1/2: 0.5","npv_sim:  2177.86197<br />scaled: 0.5222727204<br />1/2: 0.5","npv_sim:  2191.66655<br />scaled: 0.5147103227<br />1/2: 0.5","npv_sim:  2205.47113<br />scaled: 0.5070911741<br />1/2: 0.5","npv_sim:  2219.27571<br />scaled: 0.4993993993<br />1/2: 0.5","npv_sim:  2233.08029<br />scaled: 0.4916447612<br />1/2: 0.5","npv_sim:  2246.88487<br />scaled: 0.4838489344<br />1/2: 0.5","npv_sim:  2260.68946<br />scaled: 0.4760093756<br />1/2: 0.5","npv_sim:  2274.49404<br />scaled: 0.4681195816<br />1/2: 0.5","npv_sim:  2288.29862<br />scaled: 0.4602053141<br />1/2: 0.5","npv_sim:  2302.10320<br />scaled: 0.4522684267<br />1/2: 0.5","npv_sim:  2315.90778<br />scaled: 0.4443055473<br />1/2: 0.5","npv_sim:  2329.71237<br />scaled: 0.4363338461<br />1/2: 0.5","npv_sim:  2343.51695<br />scaled: 0.4283568651<br />1/2: 0.5","npv_sim:  2357.32153<br />scaled: 0.4203795739<br />1/2: 0.5","npv_sim:  2371.12611<br />scaled: 0.4124123947<br />1/2: 0.5","npv_sim:  2384.93069<br />scaled: 0.4044572279<br />1/2: 0.5","npv_sim:  2398.73528<br />scaled: 0.3965197122<br />1/2: 0.5","npv_sim:  2412.53986<br />scaled: 0.3886182116<br />1/2: 0.5","npv_sim:  2426.34444<br />scaled: 0.3807454583<br />1/2: 0.5","npv_sim:  2440.14902<br />scaled: 0.3729032151<br />1/2: 0.5","npv_sim:  2453.95360<br />scaled: 0.3651232839<br />1/2: 0.5","npv_sim:  2467.75819<br />scaled: 0.3573906552<br />1/2: 0.5","npv_sim:  2481.56277<br />scaled: 0.3497040624<br />1/2: 0.5","npv_sim:  2495.36735<br />scaled: 0.3420900860<br />1/2: 0.5","npv_sim:  2509.17193<br />scaled: 0.3345509958<br />1/2: 0.5","npv_sim:  2522.97651<br />scaled: 0.3270719779<br />1/2: 0.5","npv_sim:  2536.78110<br />scaled: 0.3196678257<br />1/2: 0.5","npv_sim:  2550.58568<br />scaled: 0.3123701787<br />1/2: 0.5","npv_sim:  2564.39026<br />scaled: 0.3051448260<br />1/2: 0.5","npv_sim:  2578.19484<br />scaled: 0.2979929901<br />1/2: 0.5","npv_sim:  2591.99942<br />scaled: 0.2909744542<br />1/2: 0.5","npv_sim:  2605.80401<br />scaled: 0.2840421048<br />1/2: 0.5","npv_sim:  2619.60859<br />scaled: 0.2771931060<br />1/2: 0.5","npv_sim:  2633.41317<br />scaled: 0.2704702901<br />1/2: 0.5","npv_sim:  2647.21775<br />scaled: 0.2638628544<br />1/2: 0.5","npv_sim:  2661.02233<br />scaled: 0.2573463400<br />1/2: 0.5","npv_sim:  2674.82691<br />scaled: 0.2509427132<br />1/2: 0.5","npv_sim:  2688.63150<br />scaled: 0.2446842515<br />1/2: 0.5","npv_sim:  2702.43608<br />scaled: 0.2385219402<br />1/2: 0.5","npv_sim:  2716.24066<br />scaled: 0.2324562373<br />1/2: 0.5","npv_sim:  2730.04524<br />scaled: 0.2265609083<br />1/2: 0.5","npv_sim:  2743.84982<br />scaled: 0.2207665082<br />1/2: 0.5","npv_sim:  2757.65441<br />scaled: 0.2150712712<br />1/2: 0.5","npv_sim:  2771.45899<br />scaled: 0.2095248825<br />1/2: 0.5","npv_sim:  2785.26357<br />scaled: 0.2041042648<br />1/2: 0.5","npv_sim:  2799.06815<br />scaled: 0.1987831345<br />1/2: 0.5","npv_sim:  2812.87273<br />scaled: 0.1935863837<br />1/2: 0.5","npv_sim:  2826.67732<br />scaled: 0.1885380030<br />1/2: 0.5","npv_sim:  2840.48190<br />scaled: 0.1835873770<br />1/2: 0.5","npv_sim:  2854.28648<br />scaled: 0.1787351425<br />1/2: 0.5","npv_sim:  2868.09106<br />scaled: 0.1740506858<br />1/2: 0.5","npv_sim:  2881.89564<br />scaled: 0.1694604342<br />1/2: 0.5","npv_sim:  2895.70023<br />scaled: 0.1649638780<br />1/2: 0.5","npv_sim:  2909.50481<br />scaled: 0.1606076240<br />1/2: 0.5","npv_sim:  2923.30939<br />scaled: 0.1563619960<br />1/2: 0.5","npv_sim:  2937.11397<br />scaled: 0.1522048089<br />1/2: 0.5","npv_sim:  2950.91855<br />scaled: 0.1481591465<br />1/2: 0.5","npv_sim:  2964.72314<br />scaled: 0.1442378300<br />1/2: 0.5","npv_sim:  2978.52772<br />scaled: 0.1403985823<br />1/2: 0.5","npv_sim:  2992.33230<br />scaled: 0.1366436528<br />1/2: 0.5","npv_sim:  3006.13688<br />scaled: 0.1330229418<br />1/2: 0.5","npv_sim:  3019.94146<br />scaled: 0.1294771785<br />1/2: 0.5","npv_sim:  3033.74605<br />scaled: 0.1260055585<br />1/2: 0.5","npv_sim:  3047.55063<br />scaled: 0.1226449380<br />1/2: 0.5","npv_sim:  3061.35521<br />scaled: 0.1193664077<br />1/2: 0.5","npv_sim:  3075.15979<br />scaled: 0.1161545111<br />1/2: 0.5","npv_sim:  3088.96437<br />scaled: 0.1130274439<br />1/2: 0.5","npv_sim:  3102.76895<br />scaled: 0.1099893106<br />1/2: 0.5","npv_sim:  3116.57354<br />scaled: 0.1070103490<br />1/2: 0.5","npv_sim:  3130.37812<br />scaled: 0.1040934265<br />1/2: 0.5","npv_sim:  3144.18270<br />scaled: 0.1012694077<br />1/2: 0.5","npv_sim:  3157.98728<br />scaled: 0.0984974689<br />1/2: 0.5","npv_sim:  3171.79186<br />scaled: 0.0957768843<br />1/2: 0.5","npv_sim:  3185.59645<br />scaled: 0.0931336508<br />1/2: 0.5","npv_sim:  3199.40103<br />scaled: 0.0905446621<br />1/2: 0.5","npv_sim:  3213.20561<br />scaled: 0.0880006979<br />1/2: 0.5","npv_sim:  3227.01019<br />scaled: 0.0855149468<br />1/2: 0.5","npv_sim:  3240.81477<br />scaled: 0.0830874548<br />1/2: 0.5","npv_sim:  3254.61936<br />scaled: 0.0806995394<br />1/2: 0.5","npv_sim:  3268.42394<br />scaled: 0.0783541479<br />1/2: 0.5","npv_sim:  3282.22852<br />scaled: 0.0760698819<br />1/2: 0.5","npv_sim:  3296.03310<br />scaled: 0.0738207521<br />1/2: 0.5","npv_sim:  3309.83768<br />scaled: 0.0716063472<br />1/2: 0.5","npv_sim:  3309.83768<br />scaled: 0.0716063472<br />1/2: 0.5","npv_sim:  3309.83768<br />scaled: 0.0716063472<br />1/2: 0.5","npv_sim:  3296.03310<br />scaled: 0.0738207521<br />1/2: 0.5","npv_sim:  3282.22852<br />scaled: 0.0760698819<br />1/2: 0.5","npv_sim:  3268.42394<br />scaled: 0.0783541479<br />1/2: 0.5","npv_sim:  3254.61936<br />scaled: 0.0806995394<br />1/2: 0.5","npv_sim:  3240.81477<br />scaled: 0.0830874548<br />1/2: 0.5","npv_sim:  3227.01019<br />scaled: 0.0855149468<br />1/2: 0.5","npv_sim:  3213.20561<br />scaled: 0.0880006979<br />1/2: 0.5","npv_sim:  3199.40103<br />scaled: 0.0905446621<br />1/2: 0.5","npv_sim:  3185.59645<br />scaled: 0.0931336508<br />1/2: 0.5","npv_sim:  3171.79186<br />scaled: 0.0957768843<br />1/2: 0.5","npv_sim:  3157.98728<br />scaled: 0.0984974689<br />1/2: 0.5","npv_sim:  3144.18270<br />scaled: 0.1012694077<br />1/2: 0.5","npv_sim:  3130.37812<br />scaled: 0.1040934265<br />1/2: 0.5","npv_sim:  3116.57354<br />scaled: 0.1070103490<br />1/2: 0.5","npv_sim:  3102.76895<br />scaled: 0.1099893106<br />1/2: 0.5","npv_sim:  3088.96437<br />scaled: 0.1130274439<br />1/2: 0.5","npv_sim:  3075.15979<br />scaled: 0.1161545111<br />1/2: 0.5","npv_sim:  3061.35521<br />scaled: 0.1193664077<br />1/2: 0.5","npv_sim:  3047.55063<br />scaled: 0.1226449380<br />1/2: 0.5","npv_sim:  3033.74605<br />scaled: 0.1260055585<br />1/2: 0.5","npv_sim:  3019.94146<br />scaled: 0.1294771785<br />1/2: 0.5","npv_sim:  3006.13688<br />scaled: 0.1330229418<br />1/2: 0.5","npv_sim:  2992.33230<br />scaled: 0.1366436528<br />1/2: 0.5","npv_sim:  2978.52772<br />scaled: 0.1403985823<br />1/2: 0.5","npv_sim:  2964.72314<br />scaled: 0.1442378300<br />1/2: 0.5","npv_sim:  2950.91855<br />scaled: 0.1481591465<br />1/2: 0.5","npv_sim:  2937.11397<br />scaled: 0.1522048089<br />1/2: 0.5","npv_sim:  2923.30939<br />scaled: 0.1563619960<br />1/2: 0.5","npv_sim:  2909.50481<br />scaled: 0.1606076240<br />1/2: 0.5","npv_sim:  2895.70023<br />scaled: 0.1649638780<br />1/2: 0.5","npv_sim:  2881.89564<br />scaled: 0.1694604342<br />1/2: 0.5","npv_sim:  2868.09106<br />scaled: 0.1740506858<br />1/2: 0.5","npv_sim:  2854.28648<br />scaled: 0.1787351425<br />1/2: 0.5","npv_sim:  2840.48190<br />scaled: 0.1835873770<br />1/2: 0.5","npv_sim:  2826.67732<br />scaled: 0.1885380030<br />1/2: 0.5","npv_sim:  2812.87273<br />scaled: 0.1935863837<br />1/2: 0.5","npv_sim:  2799.06815<br />scaled: 0.1987831345<br />1/2: 0.5","npv_sim:  2785.26357<br />scaled: 0.2041042648<br />1/2: 0.5","npv_sim:  2771.45899<br />scaled: 0.2095248825<br />1/2: 0.5","npv_sim:  2757.65441<br />scaled: 0.2150712712<br />1/2: 0.5","npv_sim:  2743.84982<br />scaled: 0.2207665082<br />1/2: 0.5","npv_sim:  2730.04524<br />scaled: 0.2265609083<br />1/2: 0.5","npv_sim:  2716.24066<br />scaled: 0.2324562373<br />1/2: 0.5","npv_sim:  2702.43608<br />scaled: 0.2385219402<br />1/2: 0.5","npv_sim:  2688.63150<br />scaled: 0.2446842515<br />1/2: 0.5","npv_sim:  2674.82691<br />scaled: 0.2509427132<br />1/2: 0.5","npv_sim:  2661.02233<br />scaled: 0.2573463400<br />1/2: 0.5","npv_sim:  2647.21775<br />scaled: 0.2638628544<br />1/2: 0.5","npv_sim:  2633.41317<br />scaled: 0.2704702901<br />1/2: 0.5","npv_sim:  2619.60859<br />scaled: 0.2771931060<br />1/2: 0.5","npv_sim:  2605.80401<br />scaled: 0.2840421048<br />1/2: 0.5","npv_sim:  2591.99942<br />scaled: 0.2909744542<br />1/2: 0.5","npv_sim:  2578.19484<br />scaled: 0.2979929901<br />1/2: 0.5","npv_sim:  2564.39026<br />scaled: 0.3051448260<br />1/2: 0.5","npv_sim:  2550.58568<br />scaled: 0.3123701787<br />1/2: 0.5","npv_sim:  2536.78110<br />scaled: 0.3196678257<br />1/2: 0.5","npv_sim:  2522.97651<br />scaled: 0.3270719779<br />1/2: 0.5","npv_sim:  2509.17193<br />scaled: 0.3345509958<br />1/2: 0.5","npv_sim:  2495.36735<br />scaled: 0.3420900860<br />1/2: 0.5","npv_sim:  2481.56277<br />scaled: 0.3497040624<br />1/2: 0.5","npv_sim:  2467.75819<br />scaled: 0.3573906552<br />1/2: 0.5","npv_sim:  2453.95360<br />scaled: 0.3651232839<br />1/2: 0.5","npv_sim:  2440.14902<br />scaled: 0.3729032151<br />1/2: 0.5","npv_sim:  2426.34444<br />scaled: 0.3807454583<br />1/2: 0.5","npv_sim:  2412.53986<br />scaled: 0.3886182116<br />1/2: 0.5","npv_sim:  2398.73528<br />scaled: 0.3965197122<br />1/2: 0.5","npv_sim:  2384.93069<br />scaled: 0.4044572279<br />1/2: 0.5","npv_sim:  2371.12611<br />scaled: 0.4124123947<br />1/2: 0.5","npv_sim:  2357.32153<br />scaled: 0.4203795739<br />1/2: 0.5","npv_sim:  2343.51695<br />scaled: 0.4283568651<br />1/2: 0.5","npv_sim:  2329.71237<br />scaled: 0.4363338461<br />1/2: 0.5","npv_sim:  2315.90778<br />scaled: 0.4443055473<br />1/2: 0.5","npv_sim:  2302.10320<br />scaled: 0.4522684267<br />1/2: 0.5","npv_sim:  2288.29862<br />scaled: 0.4602053141<br />1/2: 0.5","npv_sim:  2274.49404<br />scaled: 0.4681195816<br />1/2: 0.5","npv_sim:  2260.68946<br />scaled: 0.4760093756<br />1/2: 0.5","npv_sim:  2246.88487<br />scaled: 0.4838489344<br />1/2: 0.5","npv_sim:  2233.08029<br />scaled: 0.4916447612<br />1/2: 0.5","npv_sim:  2219.27571<br />scaled: 0.4993993993<br />1/2: 0.5","npv_sim:  2205.47113<br />scaled: 0.5070911741<br />1/2: 0.5","npv_sim:  2191.66655<br />scaled: 0.5147103227<br />1/2: 0.5","npv_sim:  2177.86197<br />scaled: 0.5222727204<br />1/2: 0.5","npv_sim:  2164.05738<br />scaled: 0.5297679284<br />1/2: 0.5","npv_sim:  2150.25280<br />scaled: 0.5371567673<br />1/2: 0.5","npv_sim:  2136.44822<br />scaled: 0.5444749961<br />1/2: 0.5","npv_sim:  2122.64364<br />scaled: 0.5517212400<br />1/2: 0.5","npv_sim:  2108.83906<br />scaled: 0.5588408867<br />1/2: 0.5","npv_sim:  2095.03447<br />scaled: 0.5658699210<br />1/2: 0.5","npv_sim:  2081.22989<br />scaled: 0.5728157032<br />1/2: 0.5","npv_sim:  2067.42531<br />scaled: 0.5796404965<br />1/2: 0.5","npv_sim:  2053.62073<br />scaled: 0.5863437978<br />1/2: 0.5","npv_sim:  2039.81615<br />scaled: 0.5929553239<br />1/2: 0.5","npv_sim:  2026.01156<br />scaled: 0.5994586475<br />1/2: 0.5","npv_sim:  2012.20698<br />scaled: 0.6058094983<br />1/2: 0.5","npv_sim:  1998.40240<br />scaled: 0.6120631374<br />1/2: 0.5","npv_sim:  1984.59782<br />scaled: 0.6182191576<br />1/2: 0.5","npv_sim:  1970.79324<br />scaled: 0.6242095794<br />1/2: 0.5","npv_sim:  1956.98865<br />scaled: 0.6300927389<br />1/2: 0.5","npv_sim:  1943.18407<br />scaled: 0.6358766434<br />1/2: 0.5","npv_sim:  1929.37949<br />scaled: 0.6415183329<br />1/2: 0.5","npv_sim:  1915.57491<br />scaled: 0.6470298975<br />1/2: 0.5","npv_sim:  1901.77033<br />scaled: 0.6524439459<br />1/2: 0.5","npv_sim:  1887.96574<br />scaled: 0.6577425883<br />1/2: 0.5","npv_sim:  1874.16116<br />scaled: 0.6628928716<br />1/2: 0.5","npv_sim:  1860.35658<br />scaled: 0.6679506364<br />1/2: 0.5","npv_sim:  1846.55200<br />scaled: 0.6729165820<br />1/2: 0.5","npv_sim:  1832.74742<br />scaled: 0.6777313337<br />1/2: 0.5","npv_sim:  1818.94283<br />scaled: 0.6824569793<br />1/2: 0.5","npv_sim:  1805.13825<br />scaled: 0.6870991866<br />1/2: 0.5","npv_sim:  1791.33367<br />scaled: 0.6916239006<br />1/2: 0.5","npv_sim:  1777.52909<br />scaled: 0.6960509722<br />1/2: 0.5","npv_sim:  1763.72451<br />scaled: 0.7004054524<br />1/2: 0.5","npv_sim:  1749.91993<br />scaled: 0.7046743635<br />1/2: 0.5","npv_sim:  1736.11534<br />scaled: 0.7088441437<br />1/2: 0.5","npv_sim:  1722.31076<br />scaled: 0.7129540754<br />1/2: 0.5","npv_sim:  1708.50618<br />scaled: 0.7170056079<br />1/2: 0.5","npv_sim:  1694.70160<br />scaled: 0.7209663334<br />1/2: 0.5","npv_sim:  1680.89702<br />scaled: 0.7248799987<br />1/2: 0.5","npv_sim:  1667.09243<br />scaled: 0.7287493708<br />1/2: 0.5","npv_sim:  1653.28785<br />scaled: 0.7325597515<br />1/2: 0.5","npv_sim:  1639.48327<br />scaled: 0.7363282949<br />1/2: 0.5","npv_sim:  1625.67869<br />scaled: 0.7400670767<br />1/2: 0.5","npv_sim:  1611.87411<br />scaled: 0.7437726708<br />1/2: 0.5","npv_sim:  1598.06952<br />scaled: 0.7474478147<br />1/2: 0.5","npv_sim:  1584.26494<br />scaled: 0.7511074709<br />1/2: 0.5","npv_sim:  1570.46036<br />scaled: 0.7547531131<br />1/2: 0.5","npv_sim:  1556.65578<br />scaled: 0.7583849601<br />1/2: 0.5","npv_sim:  1542.85120<br />scaled: 0.7620147049<br />1/2: 0.5","npv_sim:  1529.04661<br />scaled: 0.7656436878<br />1/2: 0.5","npv_sim:  1515.24203<br />scaled: 0.7692779068<br />1/2: 0.5","npv_sim:  1501.43745<br />scaled: 0.7729227872<br />1/2: 0.5","npv_sim:  1487.63287<br />scaled: 0.7765785880<br />1/2: 0.5","npv_sim:  1473.82829<br />scaled: 0.7802515247<br />1/2: 0.5","npv_sim:  1460.02370<br />scaled: 0.7839508034<br />1/2: 0.5","npv_sim:  1446.21912<br />scaled: 0.7876707324<br />1/2: 0.5","npv_sim:  1432.41454<br />scaled: 0.7914131551<br />1/2: 0.5","npv_sim:  1418.60996<br />scaled: 0.7951990230<br />1/2: 0.5","npv_sim:  1404.80538<br />scaled: 0.7990129938<br />1/2: 0.5","npv_sim:  1391.00080<br />scaled: 0.8028556867<br />1/2: 0.5","npv_sim:  1377.19621<br />scaled: 0.8067459043<br />1/2: 0.5","npv_sim:  1363.39163<br />scaled: 0.8106755617<br />1/2: 0.5","npv_sim:  1349.58705<br />scaled: 0.8146384991<br />1/2: 0.5","npv_sim:  1335.78247<br />scaled: 0.8186459175<br />1/2: 0.5","npv_sim:  1321.97789<br />scaled: 0.8227040286<br />1/2: 0.5","npv_sim:  1308.17330<br />scaled: 0.8267972931<br />1/2: 0.5","npv_sim:  1294.36872<br />scaled: 0.8309280348<br />1/2: 0.5","npv_sim:  1280.56414<br />scaled: 0.8351180784<br />1/2: 0.5","npv_sim:  1266.75956<br />scaled: 0.8393423680<br />1/2: 0.5","npv_sim:  1252.95498<br />scaled: 0.8436006035<br />1/2: 0.5","npv_sim:  1239.15039<br />scaled: 0.8479105953<br />1/2: 0.5","npv_sim:  1225.34581<br />scaled: 0.8522569739<br />1/2: 0.5","npv_sim:  1211.54123<br />scaled: 0.8566331214<br />1/2: 0.5","npv_sim:  1197.73665<br />scaled: 0.8610470379<br />1/2: 0.5","npv_sim:  1183.93207<br />scaled: 0.8654967533<br />1/2: 0.5","npv_sim:  1170.12748<br />scaled: 0.8699691595<br />1/2: 0.5","npv_sim:  1156.32290<br />scaled: 0.8744649811<br />1/2: 0.5","npv_sim:  1142.51832<br />scaled: 0.8789893399<br />1/2: 0.5","npv_sim:  1128.71374<br />scaled: 0.8835263862<br />1/2: 0.5","npv_sim:  1114.90916<br />scaled: 0.8880748417<br />1/2: 0.5","npv_sim:  1101.10457<br />scaled: 0.8926341430<br />1/2: 0.5","npv_sim:  1087.29999<br />scaled: 0.8971942343<br />1/2: 0.5","npv_sim:  1073.49541<br />scaled: 0.9017523975<br />1/2: 0.5","npv_sim:  1059.69083<br />scaled: 0.9063024455<br />1/2: 0.5","npv_sim:  1045.88625<br />scaled: 0.9108341632<br />1/2: 0.5","npv_sim:  1032.08166<br />scaled: 0.9153478008<br />1/2: 0.5","npv_sim:  1018.27708<br />scaled: 0.9198380276<br />1/2: 0.5","npv_sim:  1004.47250<br />scaled: 0.9242805375<br />1/2: 0.5","npv_sim:   990.66792<br />scaled: 0.9286861926<br />1/2: 0.5","npv_sim:   976.86334<br />scaled: 0.9330528111<br />1/2: 0.5","npv_sim:   963.05876<br />scaled: 0.9373423920<br />1/2: 0.5","npv_sim:   949.25417<br />scaled: 0.9415682803<br />1/2: 0.5","npv_sim:   935.44959<br />scaled: 0.9457337672<br />1/2: 0.5","npv_sim:   921.64501<br />scaled: 0.9498063392<br />1/2: 0.5","npv_sim:   907.84043<br />scaled: 0.9537737145<br />1/2: 0.5","npv_sim:   894.03585<br />scaled: 0.9576576095<br />1/2: 0.5","npv_sim:   880.23126<br />scaled: 0.9614408119<br />1/2: 0.5","npv_sim:   866.42668<br />scaled: 0.9650658451<br />1/2: 0.5","npv_sim:   852.62210<br />scaled: 0.9685832798<br />1/2: 0.5","npv_sim:   838.81752<br />scaled: 0.9719904781<br />1/2: 0.5","npv_sim:   825.01294<br />scaled: 0.9751978914<br />1/2: 0.5","npv_sim:   811.20835<br />scaled: 0.9782621251<br />1/2: 0.5","npv_sim:   797.40377<br />scaled: 0.9811918468<br />1/2: 0.5","npv_sim:   783.59919<br />scaled: 0.9839204038<br />1/2: 0.5","npv_sim:   769.79461<br />scaled: 0.9864457700<br />1/2: 0.5","npv_sim:   755.99003<br />scaled: 0.9888132451<br />1/2: 0.5","npv_sim:   742.18544<br />scaled: 0.9909897370<br />1/2: 0.5","npv_sim:   728.38086<br />scaled: 0.9928948560<br />1/2: 0.5","npv_sim:   714.57628<br />scaled: 0.9946206026<br />1/2: 0.5","npv_sim:   700.77170<br />scaled: 0.9961647885<br />1/2: 0.5","npv_sim:   686.96712<br />scaled: 0.9973881736<br />1/2: 0.5","npv_sim:   673.16253<br />scaled: 0.9984012661<br />1/2: 0.5","npv_sim:   659.35795<br />scaled: 0.9992147018<br />1/2: 0.5","npv_sim:   645.55337<br />scaled: 0.9997316128<br />1/2: 0.5","npv_sim:   631.74879<br />scaled: 0.9999727331<br />1/2: 0.5","npv_sim:   617.94421<br />scaled: 1.0000000000<br />1/2: 0.5","npv_sim:   604.13962<br />scaled: 0.9997663182<br />1/2: 0.5","npv_sim:   590.33504<br />scaled: 0.9991903664<br />1/2: 0.5","npv_sim:   576.53046<br />scaled: 0.9983909389<br />1/2: 0.5","npv_sim:   562.72588<br />scaled: 0.9973672322<br />1/2: 0.5","npv_sim:   548.92130<br />scaled: 0.9959535210<br />1/2: 0.5","npv_sim:   535.11672<br />scaled: 0.9943035001<br />1/2: 0.5","npv_sim:   521.31213<br />scaled: 0.9924253464<br />1/2: 0.5","npv_sim:   507.50755<br />scaled: 0.9902096237<br />1/2: 0.5","npv_sim:   493.70297<br />scaled: 0.9877026390<br />1/2: 0.5","npv_sim:   479.89839<br />scaled: 0.9849688424<br />1/2: 0.5","npv_sim:   466.09381<br />scaled: 0.9819558941<br />1/2: 0.5","npv_sim:   452.28922<br />scaled: 0.9786031062<br />1/2: 0.5","npv_sim:   438.48464<br />scaled: 0.9750298251<br />1/2: 0.5","npv_sim:   424.68006<br />scaled: 0.9712369461<br />1/2: 0.5","npv_sim:   410.87548<br />scaled: 0.9670677914<br />1/2: 0.5","npv_sim:   397.07090<br />scaled: 0.9626873762<br />1/2: 0.5","npv_sim:   383.26631<br />scaled: 0.9580988462<br />1/2: 0.5","npv_sim:   369.46173<br />scaled: 0.9532037285<br />1/2: 0.5","npv_sim:   355.65715<br />scaled: 0.9480627750<br />1/2: 0.5","npv_sim:   341.85257<br />scaled: 0.9427289238<br />1/2: 0.5","npv_sim:   328.04799<br />scaled: 0.9371560759<br />1/2: 0.5","npv_sim:   314.24340<br />scaled: 0.9313128491<br />1/2: 0.5","npv_sim:   300.43882<br />scaled: 0.9252947980<br />1/2: 0.5","npv_sim:   286.63424<br />scaled: 0.9191005255<br />1/2: 0.5","npv_sim:   272.82966<br />scaled: 0.9126219462<br />1/2: 0.5","npv_sim:   259.02508<br />scaled: 0.9059885018<br />1/2: 0.5","npv_sim:   245.22049<br />scaled: 0.8992024365<br />1/2: 0.5","npv_sim:   231.41591<br />scaled: 0.8921930972<br />1/2: 0.5","npv_sim:   217.61133<br />scaled: 0.8850174956<br />1/2: 0.5","npv_sim:   203.80675<br />scaled: 0.8777101467<br />1/2: 0.5","npv_sim:   190.00217<br />scaled: 0.8702389634<br />1/2: 0.5","npv_sim:   176.19758<br />scaled: 0.8625956790<br />1/2: 0.5","npv_sim:   162.39300<br />scaled: 0.8548413147<br />1/2: 0.5","npv_sim:   148.58842<br />scaled: 0.8469731364<br />1/2: 0.5","npv_sim:   134.78384<br />scaled: 0.8389349532<br />1/2: 0.5","npv_sim:   120.97926<br />scaled: 0.8308053413<br />1/2: 0.5","npv_sim:   107.17468<br />scaled: 0.8225863138<br />1/2: 0.5","npv_sim:    93.37009<br />scaled: 0.8142378082<br />1/2: 0.5","npv_sim:    79.56551<br />scaled: 0.8057998763<br />1/2: 0.5","npv_sim:    65.76093<br />scaled: 0.7972902191<br />1/2: 0.5","npv_sim:    51.95635<br />scaled: 0.7886913136<br />1/2: 0.5","npv_sim:    38.15177<br />scaled: 0.7800053916<br />1/2: 0.5","npv_sim:    24.34718<br />scaled: 0.7712632665<br />1/2: 0.5","npv_sim:    10.54260<br />scaled: 0.7624627756<br />1/2: 0.5","npv_sim:    -3.26198<br />scaled: 0.7535814891<br />1/2: 0.5","npv_sim:   -17.06656<br />scaled: 0.7446571535<br />1/2: 0.5","npv_sim:   -30.87114<br />scaled: 0.7356910287<br />1/2: 0.5","npv_sim:   -44.67573<br />scaled: 0.7266650458<br />1/2: 0.5","npv_sim:   -58.48031<br />scaled: 0.7176006292<br />1/2: 0.5","npv_sim:   -72.28489<br />scaled: 0.7085048917<br />1/2: 0.5","npv_sim:   -86.08947<br />scaled: 0.6993701856<br />1/2: 0.5","npv_sim:   -99.89405<br />scaled: 0.6902001091<br />1/2: 0.5","npv_sim:  -113.69864<br />scaled: 0.6810070858<br />1/2: 0.5","npv_sim:  -127.50322<br />scaled: 0.6717899619<br />1/2: 0.5","npv_sim:  -141.30780<br />scaled: 0.6625419311<br />1/2: 0.5","npv_sim:  -155.11238<br />scaled: 0.6532776351<br />1/2: 0.5","npv_sim:  -168.91696<br />scaled: 0.6439977136<br />1/2: 0.5","npv_sim:  -182.72155<br />scaled: 0.6346960080<br />1/2: 0.5","npv_sim:  -196.52613<br />scaled: 0.6253817262<br />1/2: 0.5","npv_sim:  -210.33071<br />scaled: 0.6160572220<br />1/2: 0.5","npv_sim:  -224.13529<br />scaled: 0.6067205499<br />1/2: 0.5","npv_sim:  -237.93987<br />scaled: 0.5973747266<br />1/2: 0.5","npv_sim:  -251.74445<br />scaled: 0.5880236241<br />1/2: 0.5","npv_sim:  -265.54904<br />scaled: 0.5786674659<br />1/2: 0.5","npv_sim:  -279.35362<br />scaled: 0.5693077242<br />1/2: 0.5","npv_sim:  -293.15820<br />scaled: 0.5599478023<br />1/2: 0.5","npv_sim:  -306.96278<br />scaled: 0.5505882874<br />1/2: 0.5","npv_sim:  -320.76736<br />scaled: 0.5412331368<br />1/2: 0.5","npv_sim:  -334.57195<br />scaled: 0.5318839132<br />1/2: 0.5","npv_sim:  -348.37653<br />scaled: 0.5225410762<br />1/2: 0.5","npv_sim:  -362.18111<br />scaled: 0.5132099270<br />1/2: 0.5","npv_sim:  -375.98569<br />scaled: 0.5038943463<br />1/2: 0.5","npv_sim:  -389.79027<br />scaled: 0.4945923912<br />1/2: 0.5","npv_sim:  -403.59486<br />scaled: 0.4853079763<br />1/2: 0.5","npv_sim:  -417.39944<br />scaled: 0.4760536878<br />1/2: 0.5","npv_sim:  -431.20402<br />scaled: 0.4668218096<br />1/2: 0.5","npv_sim:  -445.00860<br />scaled: 0.4576133837<br />1/2: 0.5","npv_sim:  -458.81318<br />scaled: 0.4484513169<br />1/2: 0.5","npv_sim:  -472.61777<br />scaled: 0.4393242532<br />1/2: 0.5","npv_sim:  -486.42235<br />scaled: 0.4302313034<br />1/2: 0.5","npv_sim:  -500.22693<br />scaled: 0.4211923624<br />1/2: 0.5","npv_sim:  -514.03151<br />scaled: 0.4122103036<br />1/2: 0.5","npv_sim:  -527.83609<br />scaled: 0.4032745084<br />1/2: 0.5","npv_sim:  -541.64068<br />scaled: 0.3943968712<br />1/2: 0.5","npv_sim:  -555.44526<br />scaled: 0.3856047108<br />1/2: 0.5","npv_sim:  -569.24984<br />scaled: 0.3768720976<br />1/2: 0.5","npv_sim:  -583.05442<br />scaled: 0.3682005109<br />1/2: 0.5","npv_sim:  -596.85900<br />scaled: 0.3596431472<br />1/2: 0.5","npv_sim:  -610.66359<br />scaled: 0.3511625443<br />1/2: 0.5","npv_sim:  -624.46817<br />scaled: 0.3427568830<br />1/2: 0.5","npv_sim:  -638.27275<br />scaled: 0.3344674177<br />1/2: 0.5","npv_sim:  -652.07733<br />scaled: 0.3262884201<br />1/2: 0.5","npv_sim:  -665.88191<br />scaled: 0.3181980858<br />1/2: 0.5","npv_sim:  -679.68649<br />scaled: 0.3102194701<br />1/2: 0.5","npv_sim:  -693.49108<br />scaled: 0.3023900812<br />1/2: 0.5","npv_sim:  -707.29566<br />scaled: 0.2946621318<br />1/2: 0.5","npv_sim:  -721.10024<br />scaled: 0.2870369398<br />1/2: 0.5","npv_sim:  -734.90482<br />scaled: 0.2795988109<br />1/2: 0.5","npv_sim:  -748.70940<br />scaled: 0.2722754700<br />1/2: 0.5","npv_sim:  -762.51399<br />scaled: 0.2650656555<br />1/2: 0.5","npv_sim:  -776.31857<br />scaled: 0.2580299716<br />1/2: 0.5","npv_sim:  -790.12315<br />scaled: 0.2511462811<br />1/2: 0.5","npv_sim:  -803.92773<br />scaled: 0.2443844015<br />1/2: 0.5","npv_sim:  -817.73231<br />scaled: 0.2377767303<br />1/2: 0.5","npv_sim:  -831.53690<br />scaled: 0.2313585438<br />1/2: 0.5","npv_sim:  -845.34148<br />scaled: 0.2250674526<br />1/2: 0.5","npv_sim:  -859.14606<br />scaled: 0.2189048745<br />1/2: 0.5","npv_sim:  -872.95064<br />scaled: 0.2129673653<br />1/2: 0.5","npv_sim:  -886.75522<br />scaled: 0.2071589085<br />1/2: 0.5","npv_sim:  -900.55981<br />scaled: 0.2014794406<br />1/2: 0.5","npv_sim:  -914.36439<br />scaled: 0.1959959595<br />1/2: 0.5","npv_sim:  -928.16897<br />scaled: 0.1906703422<br />1/2: 0.5","npv_sim:  -941.97355<br />scaled: 0.1854717437<br />1/2: 0.5","npv_sim:  -955.77813<br />scaled: 0.1804345228<br />1/2: 0.5","npv_sim:  -969.58272<br />scaled: 0.1755803971<br />1/2: 0.5","npv_sim:  -983.38730<br />scaled: 0.1708480603<br />1/2: 0.5","npv_sim:  -997.19188<br />scaled: 0.1662410947<br />1/2: 0.5","npv_sim: -1010.99646<br />scaled: 0.1618363593<br />1/2: 0.5","npv_sim: -1024.80104<br />scaled: 0.1575453016<br />1/2: 0.5","npv_sim: -1038.60563<br />scaled: 0.1533668590<br />1/2: 0.5","npv_sim: -1052.41021<br />scaled: 0.1493575723<br />1/2: 0.5","npv_sim: -1066.21479<br />scaled: 0.1454740586<br />1/2: 0.5","npv_sim: -1080.01937<br />scaled: 0.1416924600<br />1/2: 0.5","npv_sim: -1093.82395<br />scaled: 0.1380404159<br />1/2: 0.5","npv_sim: -1107.62853<br />scaled: 0.1345240571<br />1/2: 0.5","npv_sim: -1121.43312<br />scaled: 0.1310974104<br />1/2: 0.5","npv_sim: -1135.23770<br />scaled: 0.1277644550<br />1/2: 0.5","npv_sim: -1149.04228<br />scaled: 0.1245706167<br />1/2: 0.5","npv_sim: -1162.84686<br />scaled: 0.1214535494<br />1/2: 0.5","npv_sim: -1176.65144<br />scaled: 0.1184118433<br />1/2: 0.5","npv_sim: -1190.45603<br />scaled: 0.1154816231<br />1/2: 0.5","npv_sim: -1204.26061<br />scaled: 0.1126278147<br />1/2: 0.5","npv_sim: -1218.06519<br />scaled: 0.1098365351<br />1/2: 0.5","npv_sim: -1231.86977<br />scaled: 0.1071245092<br />1/2: 0.5","npv_sim: -1245.67435<br />scaled: 0.1044891037<br />1/2: 0.5","npv_sim: -1259.47894<br />scaled: 0.1019042377<br />1/2: 0.5","npv_sim: -1273.28352<br />scaled: 0.0993727627<br />1/2: 0.5","npv_sim: -1287.08810<br />scaled: 0.0969144999<br />1/2: 0.5","npv_sim: -1300.89268<br />scaled: 0.0944961996<br />1/2: 0.5","npv_sim: -1314.69726<br />scaled: 0.0921168335<br />1/2: 0.5","npv_sim: -1328.50185<br />scaled: 0.0897944777<br />1/2: 0.5","npv_sim: -1342.30643<br />scaled: 0.0875086281<br />1/2: 0.5","npv_sim: -1356.11101<br />scaled: 0.0852532643<br />1/2: 0.5","npv_sim: -1369.91559<br />scaled: 0.0830368063<br />1/2: 0.5","npv_sim: -1383.72017<br />scaled: 0.0808560717<br />1/2: 0.5","npv_sim: -1397.52476<br />scaled: 0.0786994757<br />1/2: 0.5","npv_sim: -1411.32934<br />scaled: 0.0765689867<br />1/2: 0.5","npv_sim: -1425.13392<br />scaled: 0.0744733078<br />1/2: 0.5","npv_sim: -1438.93850<br />scaled: 0.0723975635<br />1/2: 0.5","npv_sim: -1452.74308<br />scaled: 0.0703414273<br />1/2: 0.5","npv_sim: -1466.54767<br />scaled: 0.0683157414<br />1/2: 0.5","npv_sim: -1480.35225<br />scaled: 0.0663100573<br />1/2: 0.5","npv_sim: -1494.15683<br />scaled: 0.0643220801<br />1/2: 0.5","npv_sim: -1507.96141<br />scaled: 0.0623584330<br />1/2: 0.5","npv_sim: -1521.76599<br />scaled: 0.0604184949<br />1/2: 0.5","npv_sim: -1535.57057<br />scaled: 0.0584960876<br />1/2: 0.5","npv_sim: -1549.37516<br />scaled: 0.0565939627<br />1/2: 0.5","npv_sim: -1563.17974<br />scaled: 0.0547209316<br />1/2: 0.5","npv_sim: -1576.98432<br />scaled: 0.0528666212<br />1/2: 0.5","npv_sim: -1590.78890<br />scaled: 0.0510312219<br />1/2: 0.5","npv_sim: -1604.59348<br />scaled: 0.0492286757<br />1/2: 0.5","npv_sim: -1618.39807<br />scaled: 0.0474488090<br />1/2: 0.5","npv_sim: -1632.20265<br />scaled: 0.0456900994<br />1/2: 0.5","npv_sim: -1646.00723<br />scaled: 0.0439625745<br />1/2: 0.5","npv_sim: -1659.81181<br />scaled: 0.0422659365<br />1/2: 0.5","npv_sim: -1673.61639<br />scaled: 0.0405931638<br />1/2: 0.5","npv_sim: -1687.42098<br />scaled: 0.0389491681<br />1/2: 0.5","npv_sim: -1701.22556<br />scaled: 0.0373456279<br />1/2: 0.5","npv_sim: -1715.03014<br />scaled: 0.0357687325<br />1/2: 0.5","npv_sim: -1728.83472<br />scaled: 0.0342187740<br />1/2: 0.5","npv_sim: -1742.63930<br />scaled: 0.0327162836<br />1/2: 0.5","npv_sim: -1756.44389<br />scaled: 0.0312447234<br />1/2: 0.5","npv_sim: -1770.24847<br />scaled: 0.0298025506<br />1/2: 0.5","npv_sim: -1784.05305<br />scaled: 0.0284039943<br />1/2: 0.5","npv_sim: -1797.85763<br />scaled: 0.0270457845<br />1/2: 0.5","npv_sim: -1811.66221<br />scaled: 0.0257188502<br />1/2: 0.5","npv_sim: -1825.46680<br />scaled: 0.0244300872<br />1/2: 0.5","npv_sim: -1839.27138<br />scaled: 0.0231910951<br />1/2: 0.5","npv_sim: -1853.07596<br />scaled: 0.0219845427<br />1/2: 0.5","npv_sim: -1866.88054<br />scaled: 0.0208105180<br />1/2: 0.5","npv_sim: -1880.68512<br />scaled: 0.0196928973<br />1/2: 0.5","npv_sim: -1894.48971<br />scaled: 0.0186092076<br />1/2: 0.5","npv_sim: -1908.29429<br />scaled: 0.0175582884<br />1/2: 0.5","npv_sim: -1922.09887<br />scaled: 0.0165557523<br />1/2: 0.5","npv_sim: -1935.90345<br />scaled: 0.0155946421<br />1/2: 0.5","npv_sim: -1949.70803<br />scaled: 0.0146657485<br />1/2: 0.5","npv_sim: -1963.51261<br />scaled: 0.0137764615<br />1/2: 0.5","npv_sim: -1977.31720<br />scaled: 0.0129350013<br />1/2: 0.5","npv_sim: -1991.12178<br />scaled: 0.0121244783<br />1/2: 0.5","npv_sim: -2004.92636<br />scaled: 0.0113447249<br />1/2: 0.5","npv_sim: -2018.73094<br />scaled: 0.0106174548<br />1/2: 0.5","npv_sim: -2032.53552<br />scaled: 0.0099194075<br />1/2: 0.5","npv_sim: -2046.34011<br />scaled: 0.0092501638<br />1/2: 0.5","npv_sim: -2060.14469<br />scaled: 0.0086232304<br />1/2: 0.5","npv_sim: -2073.94927<br />scaled: 0.0080299643<br />1/2: 0.5","npv_sim: -2087.75385<br />scaled: 0.0074630964<br />1/2: 0.5","npv_sim: -2101.55843<br />scaled: 0.0069289012<br />1/2: 0.5","npv_sim: -2115.36302<br />scaled: 0.0064314193<br />1/2: 0.5","npv_sim: -2129.16760<br />scaled: 0.0059576462<br />1/2: 0.5","npv_sim: -2142.97218<br />scaled: 0.0055077881<br />1/2: 0.5","npv_sim: -2156.77676<br />scaled: 0.0050962990<br />1/2: 0.5","npv_sim: -2170.58134<br />scaled: 0.0047056952<br />1/2: 0.5","npv_sim: -2184.38593<br />scaled: 0.0043356697<br />1/2: 0.5","npv_sim: -2198.19051<br />scaled: 0.0039957602<br />1/2: 0.5","npv_sim: -2211.99509<br />scaled: 0.0036782249<br />1/2: 0.5","npv_sim: -2225.79967<br />scaled: 0.0033784609<br />1/2: 0.5","npv_sim: -2239.60425<br />scaled: 0.0031008455<br />1/2: 0.5","npv_sim: -2253.40884<br />scaled: 0.0028465112<br />1/2: 0.5","npv_sim: -2267.21342<br />scaled: 0.0026072768<br />1/2: 0.5","npv_sim: -2281.01800<br />scaled: 0.0023835628<br />1/2: 0.5","npv_sim: -2294.82258<br />scaled: 0.0021831327<br />1/2: 0.5","npv_sim: -2308.62716<br />scaled: 0.0019953532<br />1/2: 0.5","npv_sim: -2322.43174<br />scaled: 0.0018199791<br />1/2: 0.5","npv_sim: -2336.23633<br />scaled: 0.0016627679<br />1/2: 0.5","npv_sim: -2350.04091<br />scaled: 0.0015182615<br />1/2: 0.5","npv_sim: -2363.84549<br />scaled: 0.0013840427<br />1/2: 0.5","npv_sim: -2377.65007<br />scaled: 0.0012627841<br />1/2: 0.5","npv_sim: -2391.45465<br />scaled: 0.0011544254<br />1/2: 0.5","npv_sim: -2405.25924<br />scaled: 0.0010545667<br />1/2: 0.5","npv_sim: -2419.06382<br />scaled: 0.0009636347<br />1/2: 0.5","npv_sim: -2432.86840<br />scaled: 0.0008854538<br />1/2: 0.5","npv_sim: -2446.67298<br />scaled: 0.0008143351<br />1/2: 0.5","npv_sim: -2460.47756<br />scaled: 0.0007501474<br />1/2: 0.5","npv_sim: -2474.28215<br />scaled: 0.0006963210<br />1/2: 0.5","npv_sim: -2488.08673<br />scaled: 0.0006495310<br />1/2: 0.5","npv_sim: -2501.89131<br />scaled: 0.0006086411<br />1/2: 0.5","npv_sim: -2515.69589<br />scaled: 0.0005754250<br />1/2: 0.5","npv_sim: -2529.50047<br />scaled: 0.0005497585<br />1/2: 0.5","npv_sim: -2543.30506<br />scaled: 0.0005293103<br />1/2: 0.5","npv_sim: -2557.10964<br />scaled: 0.0005145561<br />1/2: 0.5","npv_sim: -2570.91422<br />scaled: 0.0005079860<br />1/2: 0.5","npv_sim: -2584.71880<br />scaled: 0.0005062886<br />1/2: 0.5","npv_sim: -2598.52338<br />scaled: 0.0005094470<br />1/2: 0.5","npv_sim: -2612.32797<br />scaled: 0.0005204313<br />1/2: 0.5","npv_sim: -2626.13255<br />scaled: 0.0005369074<br />1/2: 0.5","npv_sim: -2639.93713<br />scaled: 0.0005582574<br />1/2: 0.5","npv_sim: -2653.74171<br />scaled: 0.0005864056<br />1/2: 0.5","npv_sim: -2667.54629<br />scaled: 0.0006215264<br />1/2: 0.5","npv_sim: -2681.35088<br />scaled: 0.0006618321<br />1/2: 0.5","npv_sim: -2695.15546<br />scaled: 0.0007081202<br />1/2: 0.5","npv_sim: -2708.96004<br />scaled: 0.0007633260<br />1/2: 0.5","npv_sim: -2722.76462<br />scaled: 0.0008242935<br />1/2: 0.5","npv_sim: -2736.56920<br />scaled: 0.0008910994<br />1/2: 0.5","npv_sim: -2750.37378<br />scaled: 0.0009680561<br />1/2: 0.5","npv_sim: -2764.17837<br />scaled: 0.0010522297<br />1/2: 0.5","npv_sim: -2777.98295<br />scaled: 0.0011430798<br />1/2: 0.5","npv_sim: -2791.78753<br />scaled: 0.0012437259<br />1/2: 0.5","npv_sim: -2805.59211<br />scaled: 0.0013543589<br />1/2: 0.5","npv_sim: -2819.39669<br />scaled: 0.0014726858<br />1/2: 0.5","npv_sim: -2833.20128<br />scaled: 0.0016002167<br />1/2: 0.5","npv_sim: -2847.00586<br />scaled: 0.0017411087<br />1/2: 0.5","npv_sim: -2860.81044<br />scaled: 0.0018908344<br />1/2: 0.5","npv_sim: -2874.61502<br />scaled: 0.0020495211<br />1/2: 0.5","npv_sim: -2888.41960<br />scaled: 0.0022240915<br />1/2: 0.5","npv_sim: -2902.22419<br />scaled: 0.0024094102<br />1/2: 0.5","npv_sim: -2916.02877<br />scaled: 0.0026048802<br />1/2: 0.5","npv_sim: -2929.83335<br />scaled: 0.0028154590<br />1/2: 0.5","npv_sim: -2943.63793<br />scaled: 0.0030405750<br />1/2: 0.5","npv_sim: -2957.44251<br />scaled: 0.0032769811<br />1/2: 0.5","npv_sim: -2971.24710<br />scaled: 0.0035271306<br />1/2: 0.5","npv_sim: -2985.05168<br />scaled: 0.0037959497<br />1/2: 0.5","npv_sim: -2998.85626<br />scaled: 0.0040770436<br />1/2: 0.5","npv_sim: -3012.66084<br />scaled: 0.0043705076<br />1/2: 0.5","npv_sim: -3026.46542<br />scaled: 0.0046856794<br />1/2: 0.5","npv_sim: -3040.27001<br />scaled: 0.0050144533<br />1/2: 0.5","npv_sim: -3054.07459<br />scaled: 0.0053562661<br />1/2: 0.5","npv_sim: -3067.87917<br />scaled: 0.0057174092<br />1/2: 0.5","npv_sim: -3081.68375<br />scaled: 0.0060957133<br />1/2: 0.5","npv_sim: -3095.48833<br />scaled: 0.0064873204<br />1/2: 0.5","npv_sim: -3109.29292<br />scaled: 0.0068952129<br />1/2: 0.5","npv_sim: -3123.09750<br />scaled: 0.0073233712<br />1/2: 0.5","npv_sim: -3136.90208<br />scaled: 0.0077645788<br />1/2: 0.5","npv_sim: -3150.70666<br />scaled: 0.0082187812<br />1/2: 0.5","npv_sim: -3164.51124<br />scaled: 0.0086949880<br />1/2: 0.5","npv_sim: -3178.31582<br />scaled: 0.0091836167<br />1/2: 0.5","npv_sim: -3192.12041<br />scaled: 0.0096842715<br />1/2: 0.5","npv_sim: -3205.92499<br />scaled: 0.0102022287<br />1/2: 0.5","npv_sim: -3219.72957<br />scaled: 0.0107338338<br />1/2: 0.5","npv_sim: -3233.53415<br />scaled: 0.0112757951<br />1/2: 0.5","npv_sim: -3247.33873<br />scaled: 0.0118301629<br />1/2: 0.5","npv_sim: -3261.14332<br />scaled: 0.0123978528<br />1/2: 0.5","npv_sim: -3274.94790<br />scaled: 0.0129734963<br />1/2: 0.5","npv_sim: -3288.75248<br />scaled: 0.0135568622<br />1/2: 0.5","npv_sim: -3302.55706<br />scaled: 0.0141512459<br />1/2: 0.5","npv_sim: -3316.36164<br />scaled: 0.0147504626<br />1/2: 0.5","npv_sim: -3330.16623<br />scaled: 0.0153541213<br />1/2: 0.5","npv_sim: -3343.97081<br />scaled: 0.0159626610<br />1/2: 0.5","npv_sim: -3357.77539<br />scaled: 0.0165730097<br />1/2: 0.5","npv_sim: -3371.57997<br />scaled: 0.0171839464<br />1/2: 0.5","npv_sim: -3385.38455<br />scaled: 0.0177943982<br />1/2: 0.5","npv_sim: -3399.18914<br />scaled: 0.0184014286<br />1/2: 0.5","npv_sim: -3412.99372<br />scaled: 0.0190046912<br />1/2: 0.5","npv_sim: -3426.79830<br />scaled: 0.0196034544<br />1/2: 0.5","npv_sim: -3440.60288<br />scaled: 0.0201911983<br />1/2: 0.5","npv_sim: -3454.40746<br />scaled: 0.0207704984<br />1/2: 0.5","npv_sim: -3468.21205<br />scaled: 0.0213408377<br />1/2: 0.5","npv_sim: -3482.01663<br />scaled: 0.0218946338<br />1/2: 0.5","npv_sim: -3495.82121<br />scaled: 0.0224330305<br />1/2: 0.5","npv_sim: -3509.62579<br />scaled: 0.0229577060<br />1/2: 0.5","npv_sim: -3523.43037<br />scaled: 0.0234628947<br />1/2: 0.5","npv_sim: -3537.23496<br />scaled: 0.0239435844<br />1/2: 0.5","npv_sim: -3551.03954<br />scaled: 0.0244059914<br />1/2: 0.5","npv_sim: -3564.84412<br />scaled: 0.0248482452<br />1/2: 0.5","npv_sim: -3578.64870<br />scaled: 0.0252554085<br />1/2: 0.5","npv_sim: -3592.45328<br />scaled: 0.0256401864<br />1/2: 0.5","npv_sim: -3606.25786<br />scaled: 0.0260021810<br />1/2: 0.5","npv_sim: -3620.06245<br />scaled: 0.0263260715<br />1/2: 0.5","npv_sim: -3633.86703<br />scaled: 0.0266199353<br />1/2: 0.5","npv_sim: -3647.67161<br />scaled: 0.0268877451<br />1/2: 0.5","npv_sim: -3661.47619<br />scaled: 0.0271197172<br />1/2: 0.5","npv_sim: -3675.28077<br />scaled: 0.0273122074<br />1/2: 0.5","npv_sim: -3689.08536<br />scaled: 0.0274763211<br />1/2: 0.5","npv_sim: -3702.88994<br />scaled: 0.0276090414<br />1/2: 0.5","npv_sim: -3716.69452<br />scaled: 0.0276931317<br />1/2: 0.5","npv_sim: -3730.49910<br />scaled: 0.0277476197<br />1/2: 0.5","npv_sim: -3744.30368<br />scaled: 0.0277724502<br />1/2: 0.5","npv_sim: -3744.30368<br />scaled: 0.0277724502<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,824.85730223692,824.85730223692],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 824.8573","xintercept: 824.8573"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1237.28595335538],"y":[0.25],"text":"Median NPV:<br />  824.86","hovertext":"x: 1237.286<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1237.28595335538],"y":[0.1],"text":"SD NPV:<br />  1060.45","hovertext":"x: 1237.286<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of NPV of Total effects, 2019(KLPS4) B & EA C, no ext","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4916.41290290463,4395.05370255376],"tickmode":"array","ticktext":["-2500","0","2500"],"tickvals":[-2500,0,2500],"categoryorder":"array","categoryarray":["-2500","0","2500"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"b73753415c5":{"x":{},"y":{},"alpha":{},"type":"scatter"},"b7371e813b2e":{"xintercept":{}},"b737365458a7":{"x":{},"y":{}},"b7375b8857ce":{"x":{},"y":{}}},"cur_data":"b73753415c5","visdat":{"b73753415c5":["function (y) ","x"],"b7371e813b2e":["function (y) ","x"],"b737365458a7":["function (y) ","x"],"b7375b8857ce":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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


[^6]: last paragraph of page 9(1645) of @baird2016worms

[^7]: series avalable in file `~/opa-deworming/docs/materials/original_materials/Baird-etal-QJE-2016_fiscal-impact-calculations.xlsx` worksheet`Assumps&Panel A Calcs!A93`
