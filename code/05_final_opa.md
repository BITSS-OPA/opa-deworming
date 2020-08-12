---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "12 August, 2020"
output: 
  bookdown::html_document2:
    code_folding: hide
    code_download: true
    collapsed: yes
    keep_md: yes
    number_sections: yes
    smooth_scroll: no
    toc: yes
    toc_depth: 3
    toc_float: yes
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
# Types of objects:
### Source ------->  Input ------->  Model ------->  Policy Estimates (output)
###  (_so)           (_in)           (_mo)           (_pe)
### values           functions       functions       values
###                  & values        & values             
# Examples:                   
# - call_sou_f     - tax_elas_in_f   - tax_rev_mo_f  - ten_year_revenue_pe
# - policy_f       - est_bill_in_f   - tot_rev_mo_f  - ten_year_top_tax_pe
#                                    - ten_yrs_mo_f  - total_rev_pe
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
#    return( )                         # A list with all (most?) the elements
#}                                     # generated inside the function
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
    # alpha_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55, "Schisto mansoni" = 0.22) # from Draft Cost-Effectiveness Model.xlsx ADD ORIGINAL SOURCE

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
    nsims_so <- 1e1
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



<div class="figure" style="text-align: center">
<img src="/Users/fhoces/Desktop/sandbox/opa-deworming/code/main_pe.png" alt="Main Policy Estimate" width="70%" />
<p class="caption">(\#fig:main-pe-print)Main Policy Estimate</p>
</div>

# ADD A STANTARD DESCPRIPTION OF AN OPA REPORT {-}

- Complete narrative description. 
- Equations and code added to implement the narrated description. 
- Displayed in a layered fashion. Text should provide a good narrative explanation to policy makers, equations and code are meant to provide a full picture to analysts and researchers. 

# Introduction  

Mass deworming has demonstrated to be a highly effective public health intervention in the past. Here we provide a policy analysis that compares benefits and costs of deworming for different potential new settings. The goal of this analysis is to provide the best empirical information for policy makers debating the implemention of a deworming policy. This document describes all the analytical steps required to reproduce the analysis, displaying the actual computer code use in each step. In addition to this report, the reader can find all the materials to reproduce the findings presented here in [github.org/bitss/opa-deworming](https://github.org/bitss/opa-deworming). The main output presented in Figure \@ref(fig:main-pe-print), and described in the [results section](#policy-estimate) of this report, can also be explored interactively for different assumptions in [this web app](add link).


The Cost Benefit Analysis (CBA) of deworming is computed using three different approaches:     

  1. the original CBA produced by @baird2016worms,   
  2. an updated version of such analysis by a symilar research team [@klps4], and   
  3. a third approach that borrows some components of the previous two and some specific components added after consulting with a key stakeholder in this area, the non-governmental organization (NGO) Evidence Action (EA)[^1].



<!--
OLD TEXT (DELETE SOON):
The key policy estimate consists of a cost effectiveness analysis that compares the present
value of benefits and costs. The benefits quantified here are the effects on wages an the
costs are those of delivering the deworming treatment.  

The benefits will account for the direct effects of deworming and plus the indirect effects of deworming due to smaller pool of sick people in the community (herd inmunity). Effects are computed as a change in the earning profile of the population.

This analaysis contains elements from GiveWell's cost effectiveness analysis (see [here](https://docs.google.com/spreadsheets/d/1McptF0GVGv-QBlhWx_IoNVstWvt1z-RwVSu16ciypgs/edit#gid=1537947274), an editable version can be found [here](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1537947274))  and the cost benefit analysis described in [Baird et al., 2016](https://academic.oup.com/qje/article/131/4/1637/2468871).  
-->

# Methodology  

We first describe the common elements across all three approaches, and then describe each approach in detail.

## Common structure {-}

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
```


</details>
<br>

Benefits are equal to the additional earnings that individuals are expected to generate due to a deworming treatment. These additional earnings are computed as a discounted sum over their working lifetime.  


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
<br>

<!-- Emma: figure out how to add half a line break, as oppose to one full "<br>"-->



At a high level all three approaches focus on the same type of benefits: the increase in incomes over the lifetime of beneficiaries of deworming. This is probably an under-estimate of the benefits as it does not quantify the non-pecuniary effects of improved health.  The costs can be separated into direct costs of implementing deworming policies, and indirect costs associated with the benefits of deworming.

The main differences across the three approaches regarding benefits have to do with how to predict the earnings profiles over a lifecycle, and whether or not to account for different prevalence rates. Approaches 1 and 2 use different earning profiles, and approach 3 combines both earning profiles and adjusts for possible differences in prevalence rates of worm infections.

The main differences in costs have to do with whether indirect costs are included, and what is the relevant unit cost for the analysis. The first two approaches include indirect costs and use the unit costs of a specific country (Kenya) while the third approach does not include indirect costs and use unit costs of multiple countries. 

In summary, approaches 1 and 2 reflect different types of estimating earning profiles for one specific context (Kenya). And approach 3 incorporates both earning profile methodologies, focuses only on direct costs, and generalizes costs and prevalence for different settings. 

### The discounting rate  {-}

All approaches use the real interest rate ($r$) as the discounting rate. This is obtained from the interest rate on goverment bonds ($i$) minus the inflation rate ($\pi$).

<details><summary>Show all the details</summary>

\begin{equation}
r = \frac{1 + i}{1 + \pi} - \pi \\
r \approx i - \pi

\label{eq:3}
\tag{3}
\end{equation}

TO DO: after confirming that reproduction works, change all `interest_in` to `interest_exct_in`


```r
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
```

</details>
<br>

The actual value will vary across approaches depending on the time and country chosen. For example approach 1 used the return from government bonds and inflation in Kenya for the year 2016, while approach 3 used the values for the same country but for the year 2019. This resulted in discount rates of 9.85% and 5% for approach 1 and 3 respectively. 


-------

<details><summary>View Summary Table</summary>

<!-- Emma: figure out a way to remove "(#tab:sum_tables2)" from the caption of the summary tables. -->

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables2)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
   <td style="text-align:left;"> $(3)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables2)Sources: summary of inputs specified until this point in the document</caption>
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

Two types of results are presented: the total effect on earnings projected over a lifetime and the estimated fiscal effect due to the government collecting additional taxes on higher earnings. The effects are calculated in two scenarios: with and without externalities over the population of children who did not receive deworming interventions.


###  Gains in earnings

Gains in earnings ($E_t$) are the result of multiplying expected earnings in a certain period ($w_t$) with the effects of deworming on worked hours. This effect can have two components: a direct effect of deworming on the individual ($\lambda_1$) and the indirect effect on earnings due to externalities ($\lambda_2$). The indirect effects are considered within the context of the treatment's coverage and saturation.

<details><summary>Show all the details</summary>

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

[^6]: The original equation separates effects by gender. But the final calculation (behind table 5 in paper) does not separate by gender.



```r
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
```

</details>
<br>

#### Earnings over time

Wages in year $t$ correspond to the initial weekly wage ($w_0$) adjusted by an economy-wide increase in salaries and by an increase in salaries due to additional experience at the individual level. The economy-wide wage adjustment is assumed to be equal to the per capita GDP growth ($g$) applied to however many years of work ($Xp$). The life cycle path for wages increases at decreasing rates (wages typically increase with more years of work, then decline later in a life cycle). It is assumed that individuals enter the labor force 10 years after the treatment period. Weekly wages are multiplied by 52 weeks to obtain the annual rate. 

The initial wage in dollars ($w_{0}$) is a weighted average of wages for the control group in agriculture, working wage, and self-employed sectors ($ag, ww, se$). The weights correspond to the fraction of all the average worked hours dedicated to each sector ($h$).  

<!--Emma: pleas find the specific reference from in Suri (page, and table #,  location), and add using the @notation (you will need to edit the bibliography.bib file). If you cannot find the Suri reference, send me an email and I will look for it-->

The wage in agriculture comes from research (Suri, 2011), the working wage comes from the data and is defined as an hourly wage for the control group for those who reported more than 10 hrs of work per week. The self-employed wage ($w_{se}$) was constructed as the reported monthly earnings from self-employed profits, divided by the reported weekly number of hours work in self-employment for those who worked a positive number of hours (multiplied by 4.5 to obtain the monthly total). 

<!-- Emma, please record where are exactyl the values from below-->

The monthly self-employed profits and self-employed hours for the control group, for those with possitive hours, come from data [@baird2016worms]. The measure of hours in self employment used to compute wagesis different from the one used to compute the weights above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of working age in the sample (hence capturing the relative importance of the self employed sector in the economy).

<details><summary>Show all the details</summary>

The wages/earnings are determined by:  

\begin{equation}
w_t =  \text{#weeks} \times w_0 (1 + g)^{Xp}(1 + \hat{\beta_1} Xp + \hat{\beta_2} Xp^2) \quad \text{for } t=10, \dots, 50

\label{eq:5}
\tag{5}
\end{equation}

\begin{equation}
w_0 = \frac{1}{ex} \sum_{l \in \{ag, ww, se\}}w_{l}\alpha_{l}
\\ \quad \text{with: } \alpha_{l}= \frac{ h_{l}}{h_{ag} + h_{ww} + h_{se}}

\label{eq:6}
\tag{6}
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
```

</details>
<br>

#### Deworming effects: direct and externalities 

The estimated impact of deworming on hours worked come from @baird2016worms and are estimate separately for men ($\lambda_{1,male}$) and women ($\lambda_{1,female}$). This two parameters are combined with a simple mean in the analysis. 

The estimated externality effect ($\lambda_{2}$) reflects the additional hours worked due to individuals who did not recieve the treatment but still saw reductions in the likelihood of infection due to increased immunity in their community.  Note that this parameter is not estimated by gender, so we repeat its value two times. All the components to the equation \\ref{eq:8} come from @baird2016worms. The externalities effects are adjusted by the coverage and saturation of the original study. 

<details><summary>Show all the details</summary>

\begin{equation}
\lambda_{1} = \frac{1}{2} \lambda_{1,male} + \frac{1}{2} \lambda_{1,female}\\

\label{eq:8}
\tag{8}
\end{equation}



```r
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
```

</details>
<br>

#### Coverage and saturation of the original study [@baird2016worms]

The coverage ($R$) is defined as the fraction, among all neighboring schools (within 6 km), that belongs to the treatment group. As the treatment was applied to approximately two thirds of the population, $R$ is set to: $R  = 0.68$.  

The saturation of the intervention, $p$, measures the fraction of the population that is effectively using the treatment and is defined as a weighted average of the take-up under a full subsidy for deworming and the take-up under zero subsidy.   

<!-- Emma: find the reference below, let me know if you cannot find it--> 

For this (or similar?) setting @kremer2007illusion [add page, table, col, row] estimate that there is almost no take-up without subsidy ($Q(0)$), hence  is assigned the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy ($Q(full)$) was of 0.75.

<details><summary>Show all the details</summary>

\begin{equation}
p = R \times Q(full)  + (1 - R) \times Q(0)

\label{eq:9}
\tag{9}
\end{equation}


```r
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
```

</details>
<br>

-------

<details><summary>View Summary Table</summary>


<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables3)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables3)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
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

The costs are a combination of direct costs of mass deworming (relative to the status quo, which is no subsidy for deworming) and indirect costs on the education system due to the additional time treated individuals spend in school.

<details><summary>Show all the details</summary>
\begin{equation}
C =  \left( S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \right) + K \sum_{t=0}^{50} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_{t}(S1,S2)

\label{eq:10}
\tag{10}
\end{equation}


```r
# - inputs: periods_so, delta_ed_final_in, interest (varies by approach), cost_per_student_in, s2_in, q2_in
# - outputs: cost2_f
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

    cost2_f <- function(periods_var = periods_so, 
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
    return(list("cost2_f" = cost2_f))    # Try to return only functions
}
invisible( list2env(chunk_cost2(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:  
```

</details>
<br>

#### Direct costs: increase in deworming costs

Direct deworming costs ($DC$) are defined as the take-up under a mass deworming ($Q_{2}$) intervention, times the per-capita costs of deworming under the intervention ($S_{2}$). This costs are compared to a status quo scenario where the government does not provide any additional resource for deworming. This analysis assumes that there is no subsidy for deworming under the status quo.    

##### Complete subsidy to per capita costs of deworming

With complete subsidy, the relevant costs represent the total direct costs of deworming in USD. The take-up with full subsidy ($Q_2$) comes from a previous study [@kremer2007illusion] and takes the value of 0.75.

<details><summary>Show all the details</summary>

\begin{equation}
S_{2} = \frac{\text{Cost per person per year (KSH)}	}{ex}\times \text{Additional years of treatment} \\

\label{eq:11}
\tag{11}
\end{equation}


```r
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
```

</details>
<br>


#### Indirect costs: additional years of education and its costs for government

With the intervention there is an estimated increase in school attendance, which is multiplied by the cost of education per student to calculate the additional indirect cost on the education system imposed by a treated individual, who tends to spend more time in school than an untreated counterpart.

The additional costs on education are computed as following: first compute a cost per student ($K$). This is calculated as the salary of the teacher plus benefits, divided by the average number of students per teacher.

Second, the cost per student is multiplied by the estimated increase in school attendance ($\Delta \overline{E}_{t}(S1,S2)$).
For this we use a series of estimated effects the additional direct increase in secondary schooling from 1999 to 2007 obtained from an additional analysis related to @baird2016worms.

This series does not take into account the externality effects. To incorporate it we need another series (from the same source) that estimates the additional secondary schooling increase due to the externality and add it to the original series.


<details><summary>Show all the details</summary>

\begin{equation}
K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}

\label{eq:12}
\tag{12}
\end{equation}



```r
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
```

</details>
<br>

Without externalities, the original analysis obtains a total NPV of benefits of 142.43, with 12.9 in tax revenue for government (table 5, column 3, and rows 9, 10 respectively).

Including externalities, they obtain a total NPV of benefits of 766.81, with 102.97 in tax revenue for government (table 5, column 3, and rows 12, 13 respectively).





-----------------
<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables16)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables16)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;"> $K=116.85$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
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


## Approach 2: @klps4

In this second approach, benefits follow the same principle as in approach 1 (increase in lifetime earnings), but now there is more data on the actual effects on the labor market outcomes. Instead of projecting a trend of earnings into the future (after the estimated impact of the 10 year follow-up), this analysis uses additional data from 15 and 20 year follow-ups to the original intervention.  Costs are fairly similar to approach 1, with the addition that in the second approach, the costs also account for several rounds of treatment required for effective deworming.  Additionally. the interest rate here is updated to current values of return on (Kenyan) goverment bonds and inflation.


### Gains in earnings 

Gains in earnings ($E_t$) from 10, 15, and 20 years after intervention are used to measure the effect of multiple rounds of deworming on welfare over time. This is an important difference with approach 1, which only measures gains in earnings by year 10 and extrapolates into the future. To extrapolate after the 20-year measurement, the authors assume that the welfare gains 20 years after the intervention persist through the rest of an individual's working life. Hence the treatment effect over an individual's working life is the sum of the treatment effects on welfare at each follow-up.

Gains in earnings represents the treatment effect on welfare, so it implicitly takes into consideration the life cycle profile of wages, economywide growth, etc. @klps4 assumes that the effect on welfare identified 20 years after the intervention persists through one's working life[^8]. This approach also disregards externality effects. Their estimated treatment effects ($\alpha_{t}$) for years 10, 15, and 20, are $87, $83, $85 dollars per person per year.

[^8]: In another specification the authors assume that effects disappear after 25 years. Here we select the more persistent specification.

<details><summary>Show all the details</summary>

\begin{equation}
E_t = \mathbf{1}(10 < t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 < t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t > 20)\alpha^{KLPS4}
\text{ for } t \leq 50

\label{eq:13}
\tag{13}
\end{equation}



```r
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
```

</details>
<br>

----------

<details><summary>View Summary Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables17)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \lt t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \lt t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t \gt 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables17)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;"> $K=116.85$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=86.55$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS3}=82.99$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=85.44$ </td>
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

#### Direct costs: increase in deworming costs

Similiar to approach 1, the direct deworming costs under approach 2 are calculated by comparing the costs under a complete subsidy to the costs under the status quo of no subsidiy. The two main differences with the previous costs estimates are that now the direct costs now are summed, and discounted, over the treatment period, and prices have been updated.  


<details><summary>Show all the details</summary>

\begin{equation}
DC = \sum_{t=0}^{1.4} \left( \frac{1}{1 + r}\right)^{t} \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:14}
\tag{14}
\end{equation}

Since the analysis is discrete, and we can not sum over a non-integer, we find
\begin{equation}
DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \\
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]

\label{eq:15}
\tag{15}
\end{equation}




```r
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

s2_in <- s2_f_new(interest_var = interest_19, unit_cost_local_var = 0.8296927, ex_rate_var = 1)
s2_new <- s2_in
q2_in <- q_full_so
```

</details>
<br>

With complete subsidy, the costs of the intervention become the total direct costs of deworming each child (in USD). Most recent (2018) data from Evidence Action reveals this cost to be \$0.42 per year. Adjusting for purchasing power and inflation, we get a per capita cost of \$0.83. Adding all indirect cost over an average 2.4 years of treatment, the average cost of deworming each child over the entire treatment period is $1.44.


#### Indirect costs: additional years of education and its costs for government  

The indirect cost on the education system is calculated similarly to approach 1: the cost per student is multiplied by the increase in school attendance due to deworming. The cost of additional schooling is given by the product of the annual cost of schooling each child and number of additional years children attend school as a result of deworming. This analysis assumes that pressure is added to educational institutions for a maximum of nine years, starting at year zero. The cost per student ($K$) is updated with new information on annual teacher salary (including benefits)[^9], $12055 (also adjusted for PPP), and same average number of students per teacher (45).

Hence, the cost of schooling each child for an additional year is now $267.9 (USD).

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: @nyanchama2018 and @oduor2017. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FIND SOURCE]**.

<!-- Emma (w11): find sources for the above footnote-->


<details><summary>Show all the details</summary>

\begin{equation}
K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)

\label{eq:16}
\tag{16}
\end{equation}



```r
delta_ed_in <- delta_ed_so[,1]
cost_per_student_in_new <- cost_per_student_f(teach_sal_var = (50000*12/49.77),
                                          teach_ben_var = 0,
                                          n_students_var = 45)
```
</details>
<br>

Over this nine year period, students attended school for an additional 0.15 years on average. Then we get an average cost of additional schooling per child over the nine-year period, $32.40.

TO DO:  add a sentence with final NPV (analogous to last sentence in app1) 

-------------------


<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables20)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \lt t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \lt t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t \gt 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-tables20)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;"> $K=116.85$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=86.55$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS3}=82.99$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=85.44$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;"> $r_{16}=0.0985$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $r_{19}=0.05$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $S_2=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\overline{\Delta \overline{E}_{t}(S1,S2)}=0.02$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>
</details>
 

## Approach 3: Combination of Previous Approaches and Input From Policy Makers

In this third and final approach, we borrowed some methodological elements from @baird2016worms and @klps4 and worked in collaboration with a key policy maker in this area: the NGO Evidence Action (EA). EA provided insights on the relevant costs and benefits considered when making decisions on deworming interventions, as well as data on the main costs of implementing deworming interventions in different countries.

Under this approach, the benefits from deworming described in Approaches 1 and 2 are scaled to reflect differences in prevalence rates. Additionally, the relevant costs are constrained to direct costs alone, as implementation costs vary across countries. 

In the original deworming study conducted in Kenya in 1999, the prevalence rates of worm infections were up to XXX for the relevant population and costs were as high as XXXX. Today EA supports deworming interventions in XXX countries, with prevalence rates ranging from XXXX to XXXXX and costs ranging from XXXXX to XXXXX. 

### Benefits   

To account for different prevalence rates ($\eta$), the estimated treatment effect is decomposed in the impact of deworming on children who were treated and had a worm infection ($\lambda_{1}^{eff}$) and children who were treated and did not have worm infection. By construction, the effect on this last group should be zero. Hence the effective treatment of deworming on infected populations will be equal to the estimated treatment, divided by the proportion of the prevalence of infections. 

In the original evaluation, the prevalence rates where very high (1), hence the effect on the infected population was similar to that of the overall population. Currently deworming interventions are discussed in geographies with much lower prevalence rates, hence to obtain the expected effect over the new region, we need to multiply the effect on the infected population by the prevalence rate in the new region ($\eta_{r}$).


<details><summary>Show all the details</summary>

For approach 3, we will modify treatement effects of approaches 1 and 2 (equation 4 and 8 respectively) by the following:   

\begin{equation}
\lambda_{1} = \eta \lambda^{eff}_{1} + (1 -  \eta) \times 0 \\
\lambda^{r}_{1} = \eta_{r}\lambda^{eff}_{1}

\label{eq:17}
\tag{17}
\end{equation}


```r
# - inputs: lambda1_in_f(), alpha_0_so, alpha_r_so
# - outputs: lambda_eff_f
chunk_lambdas_eff<- function(){
###############################################################################
###############################################################################    

    lambda_eff_f <- function(lambda1_var = lambda1_in_f(), 
                           alpha_0_var = alpha_0_so, 
                           alpha_r_var = alpha_r_so){
        lambda1_eff_temp <- lambda1_var / alpha_0_var
        return( lambda1_eff_temp * alpha_r_var )
    }  

##############################################################################
###############################################################################  
    return( list("lambda_eff_f" = lambda_eff_f) )
}
invisible( list2env(chunk_lambdas_eff(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
lambda1_r_in <- lambda_eff_f()
```

</details>
<br>

**TO DO: add a section that discusses where are the prevalence rates are comming from**   

### Costs

To estimate the costs, we follow a similar approach to @givewell. The default cost is the per unit cost of treatment across all countries. This is obtained as the weighted average of per unit costs ($c_{i}$) in all countries were Evidence Action currently has data on implementation of deworming interventions. 

Costs per country include Evidence Action's technical assistance costs, government expenditure (including estimates of government staff time), and any other partner costs such as the cost of drugs donated by WHO. Costs can vary by geography due to factors of scale, treatment strategies, age of the program, and costs of "doing business."

The country weights are computed as the fraction of all treated individuals that correspond to a given country. The per capita cost of each country are obtained by dividing the country's total costs by the total number of treated individuals in a given period. Total costs for a country represent the total cost across county regions faced by three different payers: Evidence Action, local governments, and other partners.  

Country level costs at the payer level are computed as the sum of item-level costs with-in a specific country region. This items include: drug procurement and management, monitoring and evaluation, policy and advocacy, prevalence survey, program Management, planning public mobilization, community sensitization, and training & distribution.

<details><summary>Show all the details</summary>

\begin{equation}
C = \sum_{i \in Countries } \omega_{i} c_{i}

\label{eq:18}
\tag{18}
\end{equation}

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



```r
# - inputs: nothing
# - outputs: (1) function that computes the country weights used in the final 
# costs (2)  function that computes the weighted sum of country costs 
chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
    costs1_p2_f <- function(country_w_var = costs_data$country_w, 
                         country_cost_var = costs_data$per_cap) {
          sum(country_w_var * country_cost_var)
    }
  
    costs1_p1_f <- function(df_costs_var = df_costs_so,
                         df_costs_cw_var = df_costs_cw_so,
                         df_counts_var = df_counts_so,
                         staff_time_var = staff_time_so) {
      ## Counts
      # Data cleanning:
      # Add country
      df_counts_temp <- df_costs_cw_var %>%
        right_join(df_counts_var, by = "Country/State") %>%
        mutate(Country = tolower(Country))
      # keep only last year on record
      df_counts_last <- df_counts_temp %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_counts_temp, by = "Country") %>%
        filter(Year == last_year)          
      # compute counts as the sum with-in country-year of treatments
      c_counts <- df_counts_last %>%
        group_by(Country, Year) %>%
        summarise("total" = sum(`# dewormed`))
      # create country weight
      c_weights <- c_counts %>% 
        ungroup() %>% 
        mutate(country_w = total / sum(total))
        
      ## Costs 
      # Data cleaning:
      # Add country
      df_costs_temp <- df_costs_cw_var %>% 
        right_join(df_costs_var, by = "Country/State") %>%
        select(-Country.y) %>% rename(Country = Country.x) %>%
        mutate(Country = tolower(Country))
      # values for last year with cost information
      df_costs_last <- df_costs_temp %>%
        group_by(Country) %>%
        summarise("last_year" = max(Year)) %>%
        right_join(df_costs_temp, by = "Country") %>%
        filter(Year == last_year)    
      
      # summing across payers and regions (last equation)
      costs_by_payer <- df_costs_last %>%
        filter(Payer != "Total") %>%
        group_by(Country, Payer) %>% 
        summarise("costs_by_payer" = sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE)) 
    
      #sum across payers and multiply by delta (second to last)
      country_cost <- costs_by_payer %>%
        group_by(Country) %>%
        summarise("costs_by_country" = sum(costs_by_payer) * (1 + staff_time_var))  
      # Compute the per capita cost for each country (c_i and w_i)

      costs_data <- country_cost %>%
         left_join(c_weights, by = "Country") %>%
         mutate("per_cap" = costs_by_country / total)
    
      return( costs_data )
    }

###############################################################################
###############################################################################  
    return( list("costs1_p1_f" = costs1_p1_f, 
                 "costs1_p2_f" = costs1_p2_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )

##### Execute values of the functions above when needed for the text:
costs1_p1_in <- costs1_p1_f()
#costs1_p2_in <- costs1_p2_f()
```

</details>
<br>

-----

<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-table18)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \lt t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \lt t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t \gt 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-table18)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;"> $\delta_{g}=0.3$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;"> $K=116.85$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=86.55$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS3}=82.99$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=85.44$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;"> $r_{16}=0.0985$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\overline{{C}_{i}}=0.08$ </td>
   <td style="text-align:left;"> $r_{19}=0.05$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\overline{{N}_{i}}=65798181$ </td>
   <td style="text-align:left;"> $S_2=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\overline{\Delta \overline{E}_{t}(S1,S2)}=0.02$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\eta=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\eta_{r}=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
</tbody>
</table>

</details>


### Different format of policy estimate {#policy-estimate}

The key result for policy makers is defined as the cost effectiveness ratio (cell [`Deworming!B32`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=472531943&range=B32)).

\begin{equation}
CEA_{deworming} = \frac{B (1 + F_{0})}{C}

\label{eq:20}
\tag{20}
\end{equation}

 - $C$ is the costs per person dewormed (`F2, 4,B23` --> [`F1, 2, H16`](https://docs.google.com/spreadsheets/d/1hmijmJBeCJAKI1dT8n5iOLAAxfzWrKYJM_KfouFYI2w/edit#gid=1891183342&range=H16)).     
 - $B$ is the benefits per person dewormed (`F2, 4,B22`).
 - $F_{0}$ is a factor to account for leverage/fudging [not reviewed in this excercise] ([`F2, 6, D259`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1611790402&range=D259))


Also this quantity could be expressed in relative terms to the benchmark of cash transfers (cell [`Results!B9`](https://docs.google.com/spreadsheets/d/1rL8NPB8xnxqs1pr_MMEA0j27sAqEuAluwGSML7pREzk/edit#gid=1034883018&range=B9)):

\begin{equation}
RCEA = \frac{CEA_{deworming}}{CEA_{cash}}

\label{eq:21}
\tag{21}
\end{equation}


<details><summary>View Summary Table</summary>


<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-table21)Summary of equations used until this point in the document</caption>
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
   <td style="text-align:left;"> $r=\frac{1+i}{1+\pi}-1$ </td>
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
   <td style="text-align:left;"> $K = \frac{\text{teacher salary} + \text{teacher benefits}}{\text{# Students}}$ </td>
   <td style="text-align:left;"> $(10)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \lt t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \lt t \leq 20)\alpha^{KLPS3} + \mathbf{1}(t \gt 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $DC = \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \left( \frac{1}{1 + r}\right)\big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big] + \
.4\left( \frac{1}{1 + r}\right)^2 \big[S_{2}Q(S_{2}) - S_{1}Q(S_{1}) \big]$ </td>
   <td style="text-align:left;"> $(12)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $K \sum_{t=0}^{8} \left( \frac{1}{1 + r}\right)^{t} \Delta \overline{E}_t(S1,S2)$ </td>
   <td style="text-align:left;"> $(13)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $C = \sum_{i \in Countries } \omega_{i} c_{i}$ </td>
   <td style="text-align:left;"> $(14)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\omega_{i} = \frac{N_{i}}{\sum_{j}N_{j}} \
c_{i} = rac{C_{i}}{N_{i}} \
C_{i} = (1 + \delta_{g})\sum_{k \in payers}C_{i,k} \
C_{i,k} = \sum_{l \in items}\sum_{m \in regions}C_{i,k,l,m}$ </td>
   <td style="text-align:left;"> $(15)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\lambda_{1} = \alpha \lambda^{eff}_{1} + (1 -  \alpha) \times 0$ </td>
   <td style="text-align:left;"> $(16)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $CEA_{deworming} = \frac{B (1 + F_{0})}{C}$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $RCEA = \frac{CEA_{deworming}}{CEA_{cash}}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum-table21)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51$ </td>
   <td style="text-align:left;"> $\delta_{g}=0.3$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.68$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\pi_{19}=0.04$ </td>
   <td style="text-align:left;"> $\lambda_1=1.745$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{19}=0.09$ </td>
   <td style="text-align:left;"> $\lambda_2=10.2$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{ww}=14.59$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=0$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ag}=8.3$ </td>
   <td style="text-align:left;"> $K=116.85$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{ww}=6.9$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS2}=86.55$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $h_{se}=3.3$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS3}=82.99$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(full)=0.75$ </td>
   <td style="text-align:left;"> $\alpha^{KLPS4}=85.44$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $Q(0)=0$ </td>
   <td style="text-align:left;"> $r_{16}=0.0985$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\overline{{C}_{i}}=0.08$ </td>
   <td style="text-align:left;"> $r_{19}=0.05$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $\overline{{N}_{i}}=65798181$ </td>
   <td style="text-align:left;"> $S_2=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\overline{\Delta \overline{E}_{t}(S1,S2)}=0.02$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\eta=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $\eta_{r}=1$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> $F_{0}=0$ </td>
   <td style="text-align:left;">  </td>
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
# │      |      |      └────lambda_eff_f
# |      |      ├──── lambda2_in_f
# │      |      └──── saturation_in_f
# │      ├──── earnings2_f
# │      |      └────lambda_eff_f
# │      └──── interest_f
# └──── cost1_f = 0.08480686
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
            print(paste("Output has change at", 
                        deparse(substitute(to_test_var) ), 
                        " to ", text_val) )
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
    ###---------- Inputs for earnings1_f -------------------------------------------
    wage_t_in <- wage_t_mo_f(
      wage_0_var = wage_0_in,
      growth_rate_var = growth_rate_var1,
      coef_exp1_var = coef_exp_var1,
      coef_exp2_var = coef_exp2_var1
    )

    lambda1_in <- lambda_eff_f(
      lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
      alpha_0_var = alpha_0_var1,
      alpha_r_var = alpha_r_var1
    )

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)

    saturation_in <- saturation_in_f(coverage_var = coverage_var1,
                                     q_full_var = q_full_var1,
                                     q_zero_var = q_zero_var1)$saturation_in 
    unit_test(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)
    unit_test(lambda1_in[1], 1.745, main_run_var = main_run_var1)
    unit_test(lambda2_in[1], 10.2 , main_run_var = main_run_var1)
    unit_test(saturation_in, 0.511, main_run_var = main_run_var1)

    ###------------ Inputs for earnings2_f------------------------------------------
    lambda1_new_in <- lambda_eff_f(lambda1_var = lambda1_new_var1,
                                 alpha_0_var = alpha_0_var1,
                                 alpha_r_var = alpha_r_var1)
    unit_test(lambda1_new_in, 1.8184154558571, main_run_var = main_run_var1)

    ##------------ Inputs for pv_benef_f -------------------------------------------
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

    # earnings2
    earnings_in_no_ext_new <- earnings2_f(t_var = 0:50,
                                          lambda1k1_var = lambda1_new_in[1],
                                          lambda1k2_var = lambda1_new_in[2],
                                          lambda1k3_var = lambda1_new_in[3])
    # interest rate NEED TO UPDATE TO EXACT RESULT
    interest_in <- interest_f(gov_bonds_var = gov_bonds_var1,
                              inflation_var = inflation_var1)$interest_in
    unit_test(earnings_in_no_ext, 31.1421332040266, 
              main_run_var = main_run_var1)
    unit_test(earnings_in_yes_ext, 167.667817450905, 
              main_run_var = main_run_var1)
    unit_test(interest_in, 0.0985, main_run_var = main_run_var1)

    #TO DELETE:
if (FALSE){
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
}
    ##-------------- Inputs for costs2_f--------------------------------------------
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
    #Baird all and ext
    pv_benef_all_yx_in <- pv_benef_f(
      earnings_var = earnings_in_yes_ext,
      interest_r_var = interest_in,
      periods_var = periods_var1
    )
    unit_test(pv_benef_all_yx_in, 766.814399527604,
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

    #Costs
    # costs1: EA costs no externalities
    # AQUI VOY
    costs_data <- costs1_p1_f()
    cost1_in <- costs1_p2_f(country_w_var = costs_data$country_w,
                            country_cost_var = costs_data$per_cap)
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
                  "interest_in" = interest_in, "costs1_country" = costs_data,
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
<caption>(\#tab:main-results)Caption of the table</caption>
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
    
    costs1_counts_in <- costs1_country
    
    costs1_counts_sim <- sapply(costs1_counts_in$total,
                                function(x)  rnorm(nsims, 
                                                   mean = x * counts_par_var2,  
                                                   sd = counts_par_var2_sd * x) 
                                )

    staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      
    # Different costs for each staff time draw

    # Emma: figure out how to avoid "grouping" messages.     
    suppressMessages(
    costs1_data_in <- lapply(staff_time_sim,
                                function(x) costs1_p1_f(
                                  df_costs_var = df_costs_so,
                                  df_costs_cw_var = df_costs_cw_so,
                                  df_counts_var = df_counts_so,
                                  staff_time_var = x))
    )

    # draw total costs sameples from each costs data draw
    # TODO:Think how to make the code below more readable: 
    # For each costs_data in list: 
    # get the country level costs, 
    # and generate normal random draws from N(country costs, 0.1 * country costs)
    costs1_costs_sim <- t( sapply(costs1_data_in, function(x)  {
        aux1 <- x$costs_by_country
        rnorm(length(aux1), 
              mean = costs_par_var2 * aux1,  
              sd = costs_par_var2_sd * aux1)
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
            # coef_exp_var2_sd = c(as.numeric(input$param21_1_1), 
            # as.numeric(input$param21_2_1)),                       
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
           title = "Distribution of Economic of Effects From Deworming (NPV)",
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

<!--html_preserve--><div id="htmlwidget-43fbbdc7f92cf116cfd3" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-43fbbdc7f92cf116cfd3">{"x":{"data":[{"x":[-1236.88851920752,-1230.85281765468,-1224.81711610183,-1218.78141454899,-1212.74571299615,-1206.71001144331,-1200.67430989047,-1194.63860833763,-1188.60290678479,-1182.56720523194,-1176.5315036791,-1170.49580212626,-1164.46010057342,-1158.42439902058,-1152.38869746774,-1146.3529959149,-1140.31729436205,-1134.28159280921,-1128.24589125637,-1122.21018970353,-1116.17448815069,-1110.13878659785,-1104.10308504501,-1098.06738349216,-1092.03168193932,-1085.99598038648,-1079.96027883364,-1073.9245772808,-1067.88887572796,-1061.85317417512,-1055.81747262227,-1049.78177106943,-1043.74606951659,-1037.71036796375,-1031.67466641091,-1025.63896485807,-1019.60326330523,-1013.56756175238,-1007.53186019954,-1001.4961586467,-995.46045709386,-989.424755541019,-983.389053988177,-977.353352435336,-971.317650882494,-965.281949329653,-959.246247776811,-953.21054622397,-947.174844671129,-941.139143118287,-935.103441565446,-929.067740012604,-923.032038459763,-916.996336906921,-910.96063535408,-904.924933801238,-898.889232248397,-892.853530695555,-886.817829142714,-880.782127589873,-874.746426037031,-868.71072448419,-862.675022931348,-856.639321378507,-850.603619825665,-844.567918272824,-838.532216719982,-832.496515167141,-826.4608136143,-820.425112061458,-814.389410508617,-808.353708955775,-802.318007402934,-796.282305850092,-790.246604297251,-784.210902744409,-778.175201191568,-772.139499638727,-766.103798085885,-760.068096533044,-754.032394980202,-747.996693427361,-741.960991874519,-735.925290321678,-729.889588768836,-723.853887215995,-717.818185663154,-711.782484110312,-705.746782557471,-699.711081004629,-693.675379451788,-687.639677898946,-681.603976346105,-675.568274793263,-669.532573240422,-663.496871687581,-657.461170134739,-651.425468581898,-645.389767029056,-639.354065476215,-633.318363923373,-627.282662370532,-621.24696081769,-615.211259264849,-609.175557712008,-603.139856159166,-597.104154606325,-591.068453053483,-585.032751500642,-578.9970499478,-572.961348394959,-566.925646842117,-560.889945289276,-554.854243736434,-548.818542183593,-542.782840630752,-536.74713907791,-530.711437525069,-524.675735972227,-518.640034419386,-512.604332866544,-506.568631313703,-500.532929760862,-494.49722820802,-488.461526655179,-482.425825102337,-476.390123549496,-470.354421996654,-464.318720443813,-458.283018890971,-452.24731733813,-446.211615785288,-440.175914232447,-434.140212679606,-428.104511126764,-422.068809573923,-416.033108021081,-409.99740646824,-403.961704915398,-397.926003362557,-391.890301809715,-385.854600256874,-379.818898704033,-373.783197151191,-367.74749559835,-361.711794045508,-355.676092492667,-349.640390939825,-343.604689386984,-337.568987834142,-331.533286281301,-325.49758472846,-319.461883175618,-313.426181622777,-307.390480069935,-301.354778517094,-295.319076964252,-289.283375411411,-283.247673858569,-277.211972305728,-271.176270752887,-265.140569200045,-259.104867647204,-253.069166094362,-247.033464541521,-240.997762988679,-234.962061435838,-228.926359882996,-222.890658330155,-216.854956777314,-210.819255224472,-204.783553671631,-198.747852118789,-192.712150565948,-186.676449013106,-180.640747460265,-174.605045907423,-168.569344354582,-162.533642801741,-156.497941248899,-150.462239696058,-144.426538143216,-138.390836590375,-132.355135037533,-126.319433484692,-120.28373193185,-114.248030379009,-108.212328826168,-102.176627273326,-96.1409257204846,-90.1052241676432,-84.0695226148018,-78.0338210619602,-71.9981195091189,-65.9624179562775,-59.9267164034359,-53.8910148505945,-47.8553132977531,-41.8196117449115,-35.7839101920702,-29.7482086392288,-23.7125070863872,-17.6768055335458,-11.6411039807044,-5.60540242786283,0.430299124978546,6.46600067781992,12.5017022306613,18.5374037835029,24.5731053363443,30.6088068891856,36.6445084420272,42.6802099948686,48.71591154771,54.7516131005516,60.787314653393,66.8230162062343,72.8587177590759,78.8944193119173,84.9301208647587,90.9658224176003,97.0015239704417,103.037225523283,109.072927076125,115.108628628966,121.144330181807,127.180031734649,133.21573328749,139.251434840332,145.287136393173,151.322837946015,157.358539498856,163.394241051697,169.429942604539,175.46564415738,181.501345710222,187.537047263063,193.572748815905,199.608450368746,205.644151921588,211.679853474429,217.715555027271,223.751256580112,229.786958132953,235.822659685795,241.858361238636,247.894062791478,253.929764344319,259.965465897161,266.001167450002,272.036869002844,278.072570555685,284.108272108527,290.143973661368,296.179675214209,302.215376767051,308.251078319892,314.286779872734,320.322481425575,326.358182978417,332.393884531258,338.4295860841,344.465287636941,350.500989189782,356.536690742624,362.572392295465,368.608093848307,374.643795401148,380.67949695399,386.715198506831,392.750900059672,398.786601612514,404.822303165355,410.858004718197,416.893706271038,422.92940782388,428.965109376721,435.000810929563,441.036512482404,447.072214035245,453.107915588087,459.143617140928,465.17931869377,471.215020246611,477.250721799453,483.286423352294,489.322124905136,495.357826457977,501.393528010818,507.42922956366,513.464931116501,519.500632669343,525.536334222184,531.572035775026,537.607737327867,543.643438880709,549.67914043355,555.714841986392,561.750543539233,567.786245092075,573.821946644916,579.857648197757,585.893349750599,591.92905130344,597.964752856282,604.000454409123,610.036155961965,616.071857514806,622.107559067647,628.143260620489,634.17896217333,640.214663726172,646.250365279013,652.286066831855,658.321768384696,664.357469937538,670.393171490379,676.42887304322,682.464574596062,688.500276148903,694.535977701745,700.571679254586,706.607380807428,712.643082360269,718.678783913111,724.714485465952,730.750187018793,736.785888571635,742.821590124476,748.857291677318,754.892993230159,760.928694783001,766.964396335842,773.000097888684,779.035799441525,785.071500994366,791.107202547208,797.142904100049,803.178605652891,809.214307205732,815.250008758574,821.285710311415,827.321411864257,833.357113417098,839.392814969939,845.428516522781,851.464218075623,857.499919628464,863.535621181305,869.571322734147,875.607024286988,881.64272583983,887.678427392671,893.714128945512,899.749830498354,905.785532051196,911.821233604037,917.856935156878,923.89263670972,929.928338262561,935.964039815403,941.999741368244,948.035442921085,954.071144473927,960.106846026769,966.14254757961,972.178249132451,978.213950685293,984.249652238134,990.285353790976,996.321055343817,1002.35675689666,1008.3924584495,1014.42816000234,1020.46386155518,1026.49956310802,1032.53526466087,1038.57096621371,1044.60666776655,1050.64236931939,1056.67807087223,1062.71377242507,1068.74947397791,1074.78517553076,1080.8208770836,1086.85657863644,1092.89228018928,1098.92798174212,1104.96368329496,1110.9993848478,1117.03508640065,1123.07078795349,1129.10648950633,1135.14219105917,1141.17789261201,1147.21359416485,1153.24929571769,1159.28499727054,1165.32069882338,1171.35640037622,1177.39210192906,1183.4278034819,1189.46350503474,1195.49920658758,1201.53490814043,1207.57060969327,1213.60631124611,1219.64201279895,1225.67771435179,1231.71341590463,1237.74911745747,1243.78481901032,1249.82052056316,1255.856222116,1261.89192366884,1267.92762522168,1273.96332677452,1279.99902832736,1286.03472988021,1292.07043143305,1298.10613298589,1304.14183453873,1310.17753609157,1316.21323764441,1322.24893919726,1328.2846407501,1334.32034230294,1340.35604385578,1346.39174540862,1352.42744696146,1358.4631485143,1364.49885006715,1370.53455161999,1376.57025317283,1382.60595472567,1388.64165627851,1394.67735783135,1400.71305938419,1406.74876093704,1412.78446248988,1418.82016404272,1424.85586559556,1430.8915671484,1436.92726870124,1442.96297025408,1448.99867180693,1455.03437335977,1461.07007491261,1467.10577646545,1473.14147801829,1479.17717957113,1485.21288112397,1491.24858267682,1497.28428422966,1503.3199857825,1509.35568733534,1515.39138888818,1521.42709044102,1527.46279199386,1533.49849354671,1539.53419509955,1545.56989665239,1551.60559820523,1557.64129975807,1563.67700131091,1569.71270286375,1575.7484044166,1581.78410596944,1587.81980752228,1593.85550907512,1599.89121062796,1605.9269121808,1611.96261373364,1617.99831528649,1624.03401683933,1630.06971839217,1636.10541994501,1642.14112149785,1648.17682305069,1654.21252460353,1660.24822615638,1666.28392770922,1672.31962926206,1678.3553308149,1684.39103236774,1690.42673392058,1696.46243547342,1702.49813702627,1708.53383857911,1714.56954013195,1720.60524168479,1726.64094323763,1732.67664479047,1738.71234634331,1744.74804789616,1750.783749449,1756.81945100184,1762.85515255468,1768.89085410752,1774.92655566036,1780.96225721321,1786.99795876605,1793.03366031889,1799.06936187173,1805.10506342457,1811.14076497741,1817.17646653025,1823.2121680831,1829.24786963594,1835.28357118878,1841.31927274162,1847.35497429446,1847.35497429446,1847.35497429446,1841.31927274162,1835.28357118878,1829.24786963594,1823.2121680831,1817.17646653025,1811.14076497741,1805.10506342457,1799.06936187173,1793.03366031889,1786.99795876605,1780.96225721321,1774.92655566036,1768.89085410752,1762.85515255468,1756.81945100184,1750.783749449,1744.74804789616,1738.71234634331,1732.67664479047,1726.64094323763,1720.60524168479,1714.56954013195,1708.53383857911,1702.49813702627,1696.46243547342,1690.42673392058,1684.39103236774,1678.3553308149,1672.31962926206,1666.28392770922,1660.24822615638,1654.21252460353,1648.17682305069,1642.14112149785,1636.10541994501,1630.06971839217,1624.03401683933,1617.99831528649,1611.96261373364,1605.9269121808,1599.89121062796,1593.85550907512,1587.81980752228,1581.78410596944,1575.7484044166,1569.71270286375,1563.67700131091,1557.64129975807,1551.60559820523,1545.56989665239,1539.53419509955,1533.49849354671,1527.46279199386,1521.42709044102,1515.39138888818,1509.35568733534,1503.3199857825,1497.28428422966,1491.24858267682,1485.21288112397,1479.17717957113,1473.14147801829,1467.10577646545,1461.07007491261,1455.03437335977,1448.99867180693,1442.96297025408,1436.92726870124,1430.8915671484,1424.85586559556,1418.82016404272,1412.78446248988,1406.74876093704,1400.71305938419,1394.67735783135,1388.64165627851,1382.60595472567,1376.57025317283,1370.53455161999,1364.49885006715,1358.4631485143,1352.42744696146,1346.39174540862,1340.35604385578,1334.32034230294,1328.2846407501,1322.24893919726,1316.21323764441,1310.17753609157,1304.14183453873,1298.10613298589,1292.07043143305,1286.03472988021,1279.99902832736,1273.96332677452,1267.92762522168,1261.89192366884,1255.856222116,1249.82052056316,1243.78481901032,1237.74911745747,1231.71341590463,1225.67771435179,1219.64201279895,1213.60631124611,1207.57060969327,1201.53490814043,1195.49920658758,1189.46350503474,1183.4278034819,1177.39210192906,1171.35640037622,1165.32069882338,1159.28499727054,1153.24929571769,1147.21359416485,1141.17789261201,1135.14219105917,1129.10648950633,1123.07078795349,1117.03508640065,1110.9993848478,1104.96368329496,1098.92798174212,1092.89228018928,1086.85657863644,1080.8208770836,1074.78517553076,1068.74947397791,1062.71377242507,1056.67807087223,1050.64236931939,1044.60666776655,1038.57096621371,1032.53526466087,1026.49956310802,1020.46386155518,1014.42816000234,1008.3924584495,1002.35675689666,996.321055343817,990.285353790976,984.249652238134,978.213950685293,972.178249132451,966.14254757961,960.106846026769,954.071144473927,948.035442921085,941.999741368244,935.964039815403,929.928338262561,923.89263670972,917.856935156878,911.821233604037,905.785532051196,899.749830498354,893.714128945512,887.678427392671,881.64272583983,875.607024286988,869.571322734147,863.535621181305,857.499919628464,851.464218075623,845.428516522781,839.392814969939,833.357113417098,827.321411864257,821.285710311415,815.250008758574,809.214307205732,803.178605652891,797.142904100049,791.107202547208,785.071500994366,779.035799441525,773.000097888684,766.964396335842,760.928694783001,754.892993230159,748.857291677318,742.821590124476,736.785888571635,730.750187018793,724.714485465952,718.678783913111,712.643082360269,706.607380807428,700.571679254586,694.535977701745,688.500276148903,682.464574596062,676.42887304322,670.393171490379,664.357469937538,658.321768384696,652.286066831855,646.250365279013,640.214663726172,634.17896217333,628.143260620489,622.107559067647,616.071857514806,610.036155961965,604.000454409123,597.964752856282,591.92905130344,585.893349750599,579.857648197757,573.821946644916,567.786245092075,561.750543539233,555.714841986392,549.67914043355,543.643438880709,537.607737327867,531.572035775026,525.536334222184,519.500632669343,513.464931116501,507.42922956366,501.393528010818,495.357826457977,489.322124905136,483.286423352294,477.250721799453,471.215020246611,465.17931869377,459.143617140928,453.107915588087,447.072214035245,441.036512482404,435.000810929563,428.965109376721,422.92940782388,416.893706271038,410.858004718197,404.822303165355,398.786601612514,392.750900059672,386.715198506831,380.67949695399,374.643795401148,368.608093848307,362.572392295465,356.536690742624,350.500989189782,344.465287636941,338.4295860841,332.393884531258,326.358182978417,320.322481425575,314.286779872734,308.251078319892,302.215376767051,296.179675214209,290.143973661368,284.108272108527,278.072570555685,272.036869002844,266.001167450002,259.965465897161,253.929764344319,247.894062791478,241.858361238636,235.822659685795,229.786958132953,223.751256580112,217.715555027271,211.679853474429,205.644151921588,199.608450368746,193.572748815905,187.537047263063,181.501345710222,175.46564415738,169.429942604539,163.394241051697,157.358539498856,151.322837946015,145.287136393173,139.251434840332,133.21573328749,127.180031734649,121.144330181807,115.108628628966,109.072927076125,103.037225523283,97.0015239704417,90.9658224176003,84.9301208647587,78.8944193119173,72.8587177590759,66.8230162062343,60.787314653393,54.7516131005516,48.71591154771,42.6802099948686,36.6445084420272,30.6088068891856,24.5731053363443,18.5374037835029,12.5017022306613,6.46600067781992,0.430299124978546,-5.60540242786283,-11.6411039807044,-17.6768055335458,-23.7125070863872,-29.7482086392288,-35.7839101920702,-41.8196117449115,-47.8553132977531,-53.8910148505945,-59.9267164034359,-65.9624179562775,-71.9981195091189,-78.0338210619602,-84.0695226148018,-90.1052241676432,-96.1409257204846,-102.176627273326,-108.212328826168,-114.248030379009,-120.28373193185,-126.319433484692,-132.355135037533,-138.390836590375,-144.426538143216,-150.462239696058,-156.497941248899,-162.533642801741,-168.569344354582,-174.605045907423,-180.640747460265,-186.676449013106,-192.712150565948,-198.747852118789,-204.783553671631,-210.819255224472,-216.854956777314,-222.890658330155,-228.926359882996,-234.962061435838,-240.997762988679,-247.033464541521,-253.069166094362,-259.104867647204,-265.140569200045,-271.176270752887,-277.211972305728,-283.247673858569,-289.283375411411,-295.319076964252,-301.354778517094,-307.390480069935,-313.426181622777,-319.461883175618,-325.49758472846,-331.533286281301,-337.568987834142,-343.604689386984,-349.640390939825,-355.676092492667,-361.711794045508,-367.74749559835,-373.783197151191,-379.818898704033,-385.854600256874,-391.890301809715,-397.926003362557,-403.961704915398,-409.99740646824,-416.033108021081,-422.068809573923,-428.104511126764,-434.140212679606,-440.175914232447,-446.211615785288,-452.24731733813,-458.283018890971,-464.318720443813,-470.354421996654,-476.390123549496,-482.425825102337,-488.461526655179,-494.49722820802,-500.532929760862,-506.568631313703,-512.604332866544,-518.640034419386,-524.675735972227,-530.711437525069,-536.74713907791,-542.782840630752,-548.818542183593,-554.854243736434,-560.889945289276,-566.925646842117,-572.961348394959,-578.9970499478,-585.032751500642,-591.068453053483,-597.104154606325,-603.139856159166,-609.175557712008,-615.211259264849,-621.24696081769,-627.282662370532,-633.318363923373,-639.354065476215,-645.389767029056,-651.425468581898,-657.461170134739,-663.496871687581,-669.532573240422,-675.568274793263,-681.603976346105,-687.639677898946,-693.675379451788,-699.711081004629,-705.746782557471,-711.782484110312,-717.818185663154,-723.853887215995,-729.889588768836,-735.925290321678,-741.960991874519,-747.996693427361,-754.032394980202,-760.068096533044,-766.103798085885,-772.139499638727,-778.175201191568,-784.210902744409,-790.246604297251,-796.282305850092,-802.318007402934,-808.353708955775,-814.389410508617,-820.425112061458,-826.4608136143,-832.496515167141,-838.532216719982,-844.567918272824,-850.603619825665,-856.639321378507,-862.675022931348,-868.71072448419,-874.746426037031,-880.782127589873,-886.817829142714,-892.853530695555,-898.889232248397,-904.924933801238,-910.96063535408,-916.996336906921,-923.032038459763,-929.067740012604,-935.103441565446,-941.139143118287,-947.174844671129,-953.21054622397,-959.246247776811,-965.281949329653,-971.317650882494,-977.353352435336,-983.389053988177,-989.424755541019,-995.46045709386,-1001.4961586467,-1007.53186019954,-1013.56756175238,-1019.60326330523,-1025.63896485807,-1031.67466641091,-1037.71036796375,-1043.74606951659,-1049.78177106943,-1055.81747262227,-1061.85317417512,-1067.88887572796,-1073.9245772808,-1079.96027883364,-1085.99598038648,-1092.03168193932,-1098.06738349216,-1104.10308504501,-1110.13878659785,-1116.17448815069,-1122.21018970353,-1128.24589125637,-1134.28159280921,-1140.31729436205,-1146.3529959149,-1152.38869746774,-1158.42439902058,-1164.46010057342,-1170.49580212626,-1176.5315036791,-1182.56720523194,-1188.60290678479,-1194.63860833763,-1200.67430989047,-1206.71001144331,-1212.74571299615,-1218.78141454899,-1224.81711610183,-1230.85281765468,-1236.88851920752,-1236.88851920752],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.627169229310312,0.630734346533065,0.634291338227188,0.637782165445407,0.641272992663625,0.644702209332438,0.648117372856719,0.65149265362166,0.654830995561404,0.658151914367034,0.661412492989148,0.664673071611262,0.667860737749881,0.671042826889441,0.674174841775572,0.677277929310256,0.680354154758762,0.683377940164667,0.686398262110325,0.689342653122459,0.692287044134594,0.695172090846651,0.698037199252871,0.700866501168864,0.703652637771303,0.706426352834258,0.709134021617316,0.711841690400374,0.714482222373008,0.717112113921663,0.719698315482468,0.722251299699068,0.724783574105406,0.727260692277068,0.729737810448731,0.73214191890134,0.734544375173711,0.736896782165791,0.739225934482051,0.741527252403023,0.743784602969747,0.746035459832553,0.748222645176228,0.750409830519904,0.752542467160835,0.754661247840603,0.756746607932518,0.758798857975871,0.760837730748226,0.762825426877133,0.76481312300604,0.766743825010502,0.768669035725569,0.770557018364401,0.772421892954348,0.774268059439393,0.776074816982244,0.777880078235405,0.779630996656412,0.78138191507742,0.783093678501219,0.784791083750675,0.786466157927577,0.788112413339304,0.789751770545251,0.791349266433753,0.792946762322255,0.794505027142943,0.796056170693972,0.797583092998798,0.799090298504436,0.800586867053522,0.802052546538886,0.80351822602425,0.804946304464925,0.806372858749242,0.807774943821108,0.809164754051303,0.810541811265675,0.811897230955718,0.813250222092662,0.814573569696823,0.815896917300984,0.817197006065947,0.818490558108887,0.819770726059896,0.821036710841128,0.822297860832986,0.823538452717836,0.824779044602686,0.825998796259712,0.827216110563446,0.828420685451983,0.829616773918369,0.830806992218873,0.8319838390891,0.833160513207596,0.834320031870284,0.835479550532973,0.836627997089175,0.837772027297153,0.838910296554477,0.840040602189226,0.841169407289466,0.842287674649569,0.843405942009671,0.844515556630361,0.845623393216036,0.84672647184757,0.847825405609508,0.848922555267975,0.850014034285137,0.8511055133023,0.852191245371266,0.853276637919213,0.854358916312328,0.855439511280953,0.856518836319572,0.857595843936927,0.858672706979653,0.859747259796773,0.860821812613893,0.861895297101502,0.862968451191951,0.864041409485186,0.86511414581227,0.866186967116385,0.867260193020781,0.868333418925177,0.869407808669285,0.870482359430129,0.8715581292108,0.872634769634082,0.873712212996307,0.874791638985731,0.875871064975155,0.876953881629864,0.878036721697131,0.879122337574404,0.880209154248152,0.881297773788974,0.882389064884337,0.883480885226596,0.884577084940478,0.885673284654361,0.886773775560581,0.887875255361302,0.888979628306339,0.890086697590195,0.891195065638208,0.892307972129711,0.893420878621214,0.894539371140485,0.895658301017571,0.896781118074969,0.897906195805162,0.89903336744265,0.900164655321123,0.90129620441875,0.902433701804284,0.903571199189818,0.904713286544167,0.905856928794143,0.907003288875019,0.908152945985787,0.909303515017117,0.910458989981248,0.911614464945379,0.912774723664316,0.913935750561335,0.915099731882542,0.91626597372474,0.917433519622411,0.918604565982108,0.919775612341805,0.920950874518395,0.922126238967631,0.923304159954882,0.924483277352788,0.925663594082328,0.926845817749414,0.92802825753871,0.929212856360927,0.930397455183143,0.931583292949343,0.93276944845795,0.933955928454348,0.935142731930383,0.936329462635762,0.93751591229018,0.938702361944597,0.939887490033265,0.941072488315547,0.942255857368295,0.943438208457576,0.944619296236447,0.945797703769838,0.946975976456991,0.94814904154988,0.94932210664277,0.950490173623948,0.951656393236554,0.952818935182029,0.953976700885391,0.955133043829593,0.95628064102252,0.957428238215446,0.958565706494428,0.959701314120658,0.960829271629541,0.961950962441551,0.96306859260457,0.964174334017104,0.965280075429637,0.966368463691024,0.967456119263488,0.968530271967601,0.969597603550115,0.97065656702665,0.971701237599646,0.972744047104026,0.97376362433603,0.974783201568034,0.975781263787165,0.976773224329714,0.977750556967891,0.978712291500551,0.979667810157653,0.980596629189824,0.981525448221996,0.982422380004725,0.983315520434403,0.984185608874972,0.985040241297968,0.985882309266977,0.986695546023317,0.987508409784464,0.988277313721847,0.98904621765923,0.989781375080257,0.990502969007625,0.991203520382939,0.991874797180502,0.992539507873695,0.993157441219114,0.993775374564532,0.994346628112705,0.994908183776775,0.995438082331452,0.995940229990483,0.996427535616451,0.996867261122739,0.997306986629027,0.997685055059991,0.998059373106342,0.998389430248012,0.998695397357327,0.99897625642642,0.999210984192798,0.999441465490672,0.999602133994568,0.999762802498463,0.999864933781129,0.999948805097683,0.999995568283116,1,0.999990331009012,0.999912789667994,0.999835248326976,0.999683709656555,0.999521784031528,0.999309439348787,0.999060853472217,0.998786812534704,0.998449438384372,0.998112064234039,0.997684697571065,0.997256567329629,0.996763976593163,0.996243294394397,0.995684817156404,0.995069970234019,0.994444966176513,0.993734535334223,0.993024104491933,0.99223515382795,0.99142792315285,0.99057022102755,0.989665186772956,0.988738370506109,0.987734749074693,0.986731127643277,0.985635715674987,0.984532950638319,0.983367449193864,0.982165217290291,0.980929573809874,0.979627789868986,0.978321979250728,0.976920799999412,0.975519620748096,0.974044650919251,0.972544477631505,0.971000019116251,0.969401499076399,0.967787855339777,0.966091882096503,0.96439590885323,0.962617097376813,0.960824809773382,0.9589788877837,0.957091667757867,0.955179266230212,0.953198735385492,0.951218204540772,0.949148527854934,0.947076542813116,0.944943818359727,0.942782464912668,0.940587632843529,0.93833921893027,0.936083244142351,0.933750291685453,0.931417339228555,0.929019401162545,0.926604636805138,0.924150488394586,0.92165683318689,0.919147708807649,0.916578266898671,0.914008824989692,0.911373469434353,0.908731515830948,0.906047154693873,0.903336122155423,0.90060420799045,0.897827673132915,0.895049674645025,0.892211343333124,0.889373012021223,0.886492440295439,0.883596132406101,0.880676391806669,0.877726025450689,0.874768732907083,0.871768308199048,0.868767883491014,0.865728675114085,0.862682257583151,0.859612884336387,0.856524588072285,0.853426743828375,0.8503007144582,0.847174685088025,0.844016495017467,0.840856892609267,0.837677792806745,0.834488774795749,0.831290461712877,0.82807616594535,0.82486033257147,0.821624860559685,0.8183893885479,0.815140599855587,0.811888000335311,0.808629058478031,0.805363311357213,0.802095831434213,0.798820832218278,0.795545833002343,0.792265966044186,0.788985510819565,0.785703996554039,0.782421767607249,0.779140076488023,0.775859628632885,0.772579216406922,0.769303964087338,0.766028711767753,0.762759479110738,0.759492684340939,0.756230704280971,0.752975465479323,0.749721986576866,0.746481228343223,0.74324047010958,0.740013944708946,0.736790408589224,0.733577377944003,0.730373614219179,0.727175076526839,0.723993437056988,0.720811797587138,0.717653000265797,0.714495632413126,0.711355195842048,0.708224037490209,0.70510268092187,0.701999456602192,0.698897874668226,0.695824092796951,0.692750310925677,0.689699906965459,0.686656858189607,0.683628616998214,0.680617373641417,0.677611697516059,0.674633114109636,0.671654530703214,0.668705095051402,0.665759809930077,0.662834092462826,0.659922530398909,0.657020645325031,0.654143021169612,0.651265397014193,0.648421381873403,0.645577705168124,0.642757487138441,0.6399475676568,0.637150991484849,0.634374445657745,0.631601334678333,0.628857592854671,0.626113851031009,0.623396062021491,0.620684376511193,0.617988711988,0.615308165831693,0.612634214973855,0.60998373138081,0.607333247787764,0.604709416671289,0.602087769064958,0.599483404916268,0.596889227524872,0.594303727869718,0.591735526797911,0.589168274916659,0.586624439633244,0.584080604349829,0.581553617574399,0.579032432750397,0.576520600789009,0.574020258420122,0.571522828037594,0.569041439785169,0.566560051532743,0.564093256451264,0.56162886613634,0.559172919493251,0.556723515679017,0.55427758177397,0.551841110373084,0.549404638972198,0.546978725122441,0.544553101967046,0.542133409758303,0.539716533072281,0.537302199201219,0.534891961916144,0.532482123536176,0.530076425394633,0.527670727253089,0.52526697737969,0.522863736772026,0.520460698262048,0.518057863745231,0.515654704317515,0.51325026576756,0.510845827217606,0.508438156237679,0.506030155590054,0.503618812882238,0.501205354417721,0.49878958602919,0.496368846165878,0.493947906621299,0.491518143176735,0.489088379732171,0.486650854851435,0.484210415742209,0.481764694524404,0.47931202587401,0.476857486023834,0.474391139653042,0.471924793282251,0.469445810189334,0.466964450466832,0.464474219432145,0.461976629466713,0.459474683075462,0.456959770234444,0.454444857393426,0.451912455852977,0.449379256595281,0.446833416037873,0.444281100053187,0.441721530273829,0.439149403923111,0.436575830584646,0.43398333964029,0.431390848695934,0.428782235622844,0.426168967310166,0.423545578611284,0.420911262956141,0.41827301464557,0.415617525135997,0.412962035626424,0.410287703253923,0.407611056826636,0],"text":["npv_sim: -1236.8885192<br />scaled: 0.4076111<br />1/2: 0.5","npv_sim: -1230.8528177<br />scaled: 0.4102877<br />1/2: 0.5","npv_sim: -1224.8171161<br />scaled: 0.4129620<br />1/2: 0.5","npv_sim: -1218.7814145<br />scaled: 0.4156175<br />1/2: 0.5","npv_sim: -1212.7457130<br />scaled: 0.4182730<br />1/2: 0.5","npv_sim: -1206.7100114<br />scaled: 0.4209113<br />1/2: 0.5","npv_sim: -1200.6743099<br />scaled: 0.4235456<br />1/2: 0.5","npv_sim: -1194.6386083<br />scaled: 0.4261690<br />1/2: 0.5","npv_sim: -1188.6029068<br />scaled: 0.4287822<br />1/2: 0.5","npv_sim: -1182.5672052<br />scaled: 0.4313908<br />1/2: 0.5","npv_sim: -1176.5315037<br />scaled: 0.4339833<br />1/2: 0.5","npv_sim: -1170.4958021<br />scaled: 0.4365758<br />1/2: 0.5","npv_sim: -1164.4601006<br />scaled: 0.4391494<br />1/2: 0.5","npv_sim: -1158.4243990<br />scaled: 0.4417215<br />1/2: 0.5","npv_sim: -1152.3886975<br />scaled: 0.4442811<br />1/2: 0.5","npv_sim: -1146.3529959<br />scaled: 0.4468334<br />1/2: 0.5","npv_sim: -1140.3172944<br />scaled: 0.4493793<br />1/2: 0.5","npv_sim: -1134.2815928<br />scaled: 0.4519125<br />1/2: 0.5","npv_sim: -1128.2458913<br />scaled: 0.4544449<br />1/2: 0.5","npv_sim: -1122.2101897<br />scaled: 0.4569598<br />1/2: 0.5","npv_sim: -1116.1744882<br />scaled: 0.4594747<br />1/2: 0.5","npv_sim: -1110.1387866<br />scaled: 0.4619766<br />1/2: 0.5","npv_sim: -1104.1030850<br />scaled: 0.4644742<br />1/2: 0.5","npv_sim: -1098.0673835<br />scaled: 0.4669645<br />1/2: 0.5","npv_sim: -1092.0316819<br />scaled: 0.4694458<br />1/2: 0.5","npv_sim: -1085.9959804<br />scaled: 0.4719248<br />1/2: 0.5","npv_sim: -1079.9602788<br />scaled: 0.4743911<br />1/2: 0.5","npv_sim: -1073.9245773<br />scaled: 0.4768575<br />1/2: 0.5","npv_sim: -1067.8888757<br />scaled: 0.4793120<br />1/2: 0.5","npv_sim: -1061.8531742<br />scaled: 0.4817647<br />1/2: 0.5","npv_sim: -1055.8174726<br />scaled: 0.4842104<br />1/2: 0.5","npv_sim: -1049.7817711<br />scaled: 0.4866509<br />1/2: 0.5","npv_sim: -1043.7460695<br />scaled: 0.4890884<br />1/2: 0.5","npv_sim: -1037.7103680<br />scaled: 0.4915181<br />1/2: 0.5","npv_sim: -1031.6746664<br />scaled: 0.4939479<br />1/2: 0.5","npv_sim: -1025.6389649<br />scaled: 0.4963688<br />1/2: 0.5","npv_sim: -1019.6032633<br />scaled: 0.4987896<br />1/2: 0.5","npv_sim: -1013.5675618<br />scaled: 0.5012054<br />1/2: 0.5","npv_sim: -1007.5318602<br />scaled: 0.5036188<br />1/2: 0.5","npv_sim: -1001.4961586<br />scaled: 0.5060302<br />1/2: 0.5","npv_sim:  -995.4604571<br />scaled: 0.5084382<br />1/2: 0.5","npv_sim:  -989.4247555<br />scaled: 0.5108458<br />1/2: 0.5","npv_sim:  -983.3890540<br />scaled: 0.5132503<br />1/2: 0.5","npv_sim:  -977.3533524<br />scaled: 0.5156547<br />1/2: 0.5","npv_sim:  -971.3176509<br />scaled: 0.5180579<br />1/2: 0.5","npv_sim:  -965.2819493<br />scaled: 0.5204607<br />1/2: 0.5","npv_sim:  -959.2462478<br />scaled: 0.5228637<br />1/2: 0.5","npv_sim:  -953.2105462<br />scaled: 0.5252670<br />1/2: 0.5","npv_sim:  -947.1748447<br />scaled: 0.5276707<br />1/2: 0.5","npv_sim:  -941.1391431<br />scaled: 0.5300764<br />1/2: 0.5","npv_sim:  -935.1034416<br />scaled: 0.5324821<br />1/2: 0.5","npv_sim:  -929.0677400<br />scaled: 0.5348920<br />1/2: 0.5","npv_sim:  -923.0320385<br />scaled: 0.5373022<br />1/2: 0.5","npv_sim:  -916.9963369<br />scaled: 0.5397165<br />1/2: 0.5","npv_sim:  -910.9606354<br />scaled: 0.5421334<br />1/2: 0.5","npv_sim:  -904.9249338<br />scaled: 0.5445531<br />1/2: 0.5","npv_sim:  -898.8892322<br />scaled: 0.5469787<br />1/2: 0.5","npv_sim:  -892.8535307<br />scaled: 0.5494046<br />1/2: 0.5","npv_sim:  -886.8178291<br />scaled: 0.5518411<br />1/2: 0.5","npv_sim:  -880.7821276<br />scaled: 0.5542776<br />1/2: 0.5","npv_sim:  -874.7464260<br />scaled: 0.5567235<br />1/2: 0.5","npv_sim:  -868.7107245<br />scaled: 0.5591729<br />1/2: 0.5","npv_sim:  -862.6750229<br />scaled: 0.5616289<br />1/2: 0.5","npv_sim:  -856.6393214<br />scaled: 0.5640933<br />1/2: 0.5","npv_sim:  -850.6036198<br />scaled: 0.5665601<br />1/2: 0.5","npv_sim:  -844.5679183<br />scaled: 0.5690414<br />1/2: 0.5","npv_sim:  -838.5322167<br />scaled: 0.5715228<br />1/2: 0.5","npv_sim:  -832.4965152<br />scaled: 0.5740203<br />1/2: 0.5","npv_sim:  -826.4608136<br />scaled: 0.5765206<br />1/2: 0.5","npv_sim:  -820.4251121<br />scaled: 0.5790324<br />1/2: 0.5","npv_sim:  -814.3894105<br />scaled: 0.5815536<br />1/2: 0.5","npv_sim:  -808.3537090<br />scaled: 0.5840806<br />1/2: 0.5","npv_sim:  -802.3180074<br />scaled: 0.5866244<br />1/2: 0.5","npv_sim:  -796.2823059<br />scaled: 0.5891683<br />1/2: 0.5","npv_sim:  -790.2466043<br />scaled: 0.5917355<br />1/2: 0.5","npv_sim:  -784.2109027<br />scaled: 0.5943037<br />1/2: 0.5","npv_sim:  -778.1752012<br />scaled: 0.5968892<br />1/2: 0.5","npv_sim:  -772.1394996<br />scaled: 0.5994834<br />1/2: 0.5","npv_sim:  -766.1037981<br />scaled: 0.6020878<br />1/2: 0.5","npv_sim:  -760.0680965<br />scaled: 0.6047094<br />1/2: 0.5","npv_sim:  -754.0323950<br />scaled: 0.6073332<br />1/2: 0.5","npv_sim:  -747.9966934<br />scaled: 0.6099837<br />1/2: 0.5","npv_sim:  -741.9609919<br />scaled: 0.6126342<br />1/2: 0.5","npv_sim:  -735.9252903<br />scaled: 0.6153082<br />1/2: 0.5","npv_sim:  -729.8895888<br />scaled: 0.6179887<br />1/2: 0.5","npv_sim:  -723.8538872<br />scaled: 0.6206844<br />1/2: 0.5","npv_sim:  -717.8181857<br />scaled: 0.6233961<br />1/2: 0.5","npv_sim:  -711.7824841<br />scaled: 0.6261139<br />1/2: 0.5","npv_sim:  -705.7467826<br />scaled: 0.6288576<br />1/2: 0.5","npv_sim:  -699.7110810<br />scaled: 0.6316013<br />1/2: 0.5","npv_sim:  -693.6753795<br />scaled: 0.6343744<br />1/2: 0.5","npv_sim:  -687.6396779<br />scaled: 0.6371510<br />1/2: 0.5","npv_sim:  -681.6039763<br />scaled: 0.6399476<br />1/2: 0.5","npv_sim:  -675.5682748<br />scaled: 0.6427575<br />1/2: 0.5","npv_sim:  -669.5325732<br />scaled: 0.6455777<br />1/2: 0.5","npv_sim:  -663.4968717<br />scaled: 0.6484214<br />1/2: 0.5","npv_sim:  -657.4611701<br />scaled: 0.6512654<br />1/2: 0.5","npv_sim:  -651.4254686<br />scaled: 0.6541430<br />1/2: 0.5","npv_sim:  -645.3897670<br />scaled: 0.6570206<br />1/2: 0.5","npv_sim:  -639.3540655<br />scaled: 0.6599225<br />1/2: 0.5","npv_sim:  -633.3183639<br />scaled: 0.6628341<br />1/2: 0.5","npv_sim:  -627.2826624<br />scaled: 0.6657598<br />1/2: 0.5","npv_sim:  -621.2469608<br />scaled: 0.6687051<br />1/2: 0.5","npv_sim:  -615.2112593<br />scaled: 0.6716545<br />1/2: 0.5","npv_sim:  -609.1755577<br />scaled: 0.6746331<br />1/2: 0.5","npv_sim:  -603.1398562<br />scaled: 0.6776117<br />1/2: 0.5","npv_sim:  -597.1041546<br />scaled: 0.6806174<br />1/2: 0.5","npv_sim:  -591.0684531<br />scaled: 0.6836286<br />1/2: 0.5","npv_sim:  -585.0327515<br />scaled: 0.6866569<br />1/2: 0.5","npv_sim:  -578.9970499<br />scaled: 0.6896999<br />1/2: 0.5","npv_sim:  -572.9613484<br />scaled: 0.6927503<br />1/2: 0.5","npv_sim:  -566.9256468<br />scaled: 0.6958241<br />1/2: 0.5","npv_sim:  -560.8899453<br />scaled: 0.6988979<br />1/2: 0.5","npv_sim:  -554.8542437<br />scaled: 0.7019995<br />1/2: 0.5","npv_sim:  -548.8185422<br />scaled: 0.7051027<br />1/2: 0.5","npv_sim:  -542.7828406<br />scaled: 0.7082240<br />1/2: 0.5","npv_sim:  -536.7471391<br />scaled: 0.7113552<br />1/2: 0.5","npv_sim:  -530.7114375<br />scaled: 0.7144956<br />1/2: 0.5","npv_sim:  -524.6757360<br />scaled: 0.7176530<br />1/2: 0.5","npv_sim:  -518.6400344<br />scaled: 0.7208118<br />1/2: 0.5","npv_sim:  -512.6043329<br />scaled: 0.7239934<br />1/2: 0.5","npv_sim:  -506.5686313<br />scaled: 0.7271751<br />1/2: 0.5","npv_sim:  -500.5329298<br />scaled: 0.7303736<br />1/2: 0.5","npv_sim:  -494.4972282<br />scaled: 0.7335774<br />1/2: 0.5","npv_sim:  -488.4615267<br />scaled: 0.7367904<br />1/2: 0.5","npv_sim:  -482.4258251<br />scaled: 0.7400139<br />1/2: 0.5","npv_sim:  -476.3901235<br />scaled: 0.7432405<br />1/2: 0.5","npv_sim:  -470.3544220<br />scaled: 0.7464812<br />1/2: 0.5","npv_sim:  -464.3187204<br />scaled: 0.7497220<br />1/2: 0.5","npv_sim:  -458.2830189<br />scaled: 0.7529755<br />1/2: 0.5","npv_sim:  -452.2473173<br />scaled: 0.7562307<br />1/2: 0.5","npv_sim:  -446.2116158<br />scaled: 0.7594927<br />1/2: 0.5","npv_sim:  -440.1759142<br />scaled: 0.7627595<br />1/2: 0.5","npv_sim:  -434.1402127<br />scaled: 0.7660287<br />1/2: 0.5","npv_sim:  -428.1045111<br />scaled: 0.7693040<br />1/2: 0.5","npv_sim:  -422.0688096<br />scaled: 0.7725792<br />1/2: 0.5","npv_sim:  -416.0331080<br />scaled: 0.7758596<br />1/2: 0.5","npv_sim:  -409.9974065<br />scaled: 0.7791401<br />1/2: 0.5","npv_sim:  -403.9617049<br />scaled: 0.7824218<br />1/2: 0.5","npv_sim:  -397.9260034<br />scaled: 0.7857040<br />1/2: 0.5","npv_sim:  -391.8903018<br />scaled: 0.7889855<br />1/2: 0.5","npv_sim:  -385.8546003<br />scaled: 0.7922660<br />1/2: 0.5","npv_sim:  -379.8188987<br />scaled: 0.7955458<br />1/2: 0.5","npv_sim:  -373.7831972<br />scaled: 0.7988208<br />1/2: 0.5","npv_sim:  -367.7474956<br />scaled: 0.8020958<br />1/2: 0.5","npv_sim:  -361.7117940<br />scaled: 0.8053633<br />1/2: 0.5","npv_sim:  -355.6760925<br />scaled: 0.8086291<br />1/2: 0.5","npv_sim:  -349.6403909<br />scaled: 0.8118880<br />1/2: 0.5","npv_sim:  -343.6046894<br />scaled: 0.8151406<br />1/2: 0.5","npv_sim:  -337.5689878<br />scaled: 0.8183894<br />1/2: 0.5","npv_sim:  -331.5332863<br />scaled: 0.8216249<br />1/2: 0.5","npv_sim:  -325.4975847<br />scaled: 0.8248603<br />1/2: 0.5","npv_sim:  -319.4618832<br />scaled: 0.8280762<br />1/2: 0.5","npv_sim:  -313.4261816<br />scaled: 0.8312905<br />1/2: 0.5","npv_sim:  -307.3904801<br />scaled: 0.8344888<br />1/2: 0.5","npv_sim:  -301.3547785<br />scaled: 0.8376778<br />1/2: 0.5","npv_sim:  -295.3190770<br />scaled: 0.8408569<br />1/2: 0.5","npv_sim:  -289.2833754<br />scaled: 0.8440165<br />1/2: 0.5","npv_sim:  -283.2476739<br />scaled: 0.8471747<br />1/2: 0.5","npv_sim:  -277.2119723<br />scaled: 0.8503007<br />1/2: 0.5","npv_sim:  -271.1762708<br />scaled: 0.8534267<br />1/2: 0.5","npv_sim:  -265.1405692<br />scaled: 0.8565246<br />1/2: 0.5","npv_sim:  -259.1048676<br />scaled: 0.8596129<br />1/2: 0.5","npv_sim:  -253.0691661<br />scaled: 0.8626823<br />1/2: 0.5","npv_sim:  -247.0334645<br />scaled: 0.8657287<br />1/2: 0.5","npv_sim:  -240.9977630<br />scaled: 0.8687679<br />1/2: 0.5","npv_sim:  -234.9620614<br />scaled: 0.8717683<br />1/2: 0.5","npv_sim:  -228.9263599<br />scaled: 0.8747687<br />1/2: 0.5","npv_sim:  -222.8906583<br />scaled: 0.8777260<br />1/2: 0.5","npv_sim:  -216.8549568<br />scaled: 0.8806764<br />1/2: 0.5","npv_sim:  -210.8192552<br />scaled: 0.8835961<br />1/2: 0.5","npv_sim:  -204.7835537<br />scaled: 0.8864924<br />1/2: 0.5","npv_sim:  -198.7478521<br />scaled: 0.8893730<br />1/2: 0.5","npv_sim:  -192.7121506<br />scaled: 0.8922113<br />1/2: 0.5","npv_sim:  -186.6764490<br />scaled: 0.8950497<br />1/2: 0.5","npv_sim:  -180.6407475<br />scaled: 0.8978277<br />1/2: 0.5","npv_sim:  -174.6050459<br />scaled: 0.9006042<br />1/2: 0.5","npv_sim:  -168.5693444<br />scaled: 0.9033361<br />1/2: 0.5","npv_sim:  -162.5336428<br />scaled: 0.9060472<br />1/2: 0.5","npv_sim:  -156.4979412<br />scaled: 0.9087315<br />1/2: 0.5","npv_sim:  -150.4622397<br />scaled: 0.9113735<br />1/2: 0.5","npv_sim:  -144.4265381<br />scaled: 0.9140088<br />1/2: 0.5","npv_sim:  -138.3908366<br />scaled: 0.9165783<br />1/2: 0.5","npv_sim:  -132.3551350<br />scaled: 0.9191477<br />1/2: 0.5","npv_sim:  -126.3194335<br />scaled: 0.9216568<br />1/2: 0.5","npv_sim:  -120.2837319<br />scaled: 0.9241505<br />1/2: 0.5","npv_sim:  -114.2480304<br />scaled: 0.9266046<br />1/2: 0.5","npv_sim:  -108.2123288<br />scaled: 0.9290194<br />1/2: 0.5","npv_sim:  -102.1766273<br />scaled: 0.9314173<br />1/2: 0.5","npv_sim:   -96.1409257<br />scaled: 0.9337503<br />1/2: 0.5","npv_sim:   -90.1052242<br />scaled: 0.9360832<br />1/2: 0.5","npv_sim:   -84.0695226<br />scaled: 0.9383392<br />1/2: 0.5","npv_sim:   -78.0338211<br />scaled: 0.9405876<br />1/2: 0.5","npv_sim:   -71.9981195<br />scaled: 0.9427825<br />1/2: 0.5","npv_sim:   -65.9624180<br />scaled: 0.9449438<br />1/2: 0.5","npv_sim:   -59.9267164<br />scaled: 0.9470765<br />1/2: 0.5","npv_sim:   -53.8910149<br />scaled: 0.9491485<br />1/2: 0.5","npv_sim:   -47.8553133<br />scaled: 0.9512182<br />1/2: 0.5","npv_sim:   -41.8196117<br />scaled: 0.9531987<br />1/2: 0.5","npv_sim:   -35.7839102<br />scaled: 0.9551793<br />1/2: 0.5","npv_sim:   -29.7482086<br />scaled: 0.9570917<br />1/2: 0.5","npv_sim:   -23.7125071<br />scaled: 0.9589789<br />1/2: 0.5","npv_sim:   -17.6768055<br />scaled: 0.9608248<br />1/2: 0.5","npv_sim:   -11.6411040<br />scaled: 0.9626171<br />1/2: 0.5","npv_sim:    -5.6054024<br />scaled: 0.9643959<br />1/2: 0.5","npv_sim:     0.4302991<br />scaled: 0.9660919<br />1/2: 0.5","npv_sim:     6.4660007<br />scaled: 0.9677879<br />1/2: 0.5","npv_sim:    12.5017022<br />scaled: 0.9694015<br />1/2: 0.5","npv_sim:    18.5374038<br />scaled: 0.9710000<br />1/2: 0.5","npv_sim:    24.5731053<br />scaled: 0.9725445<br />1/2: 0.5","npv_sim:    30.6088069<br />scaled: 0.9740447<br />1/2: 0.5","npv_sim:    36.6445084<br />scaled: 0.9755196<br />1/2: 0.5","npv_sim:    42.6802100<br />scaled: 0.9769208<br />1/2: 0.5","npv_sim:    48.7159115<br />scaled: 0.9783220<br />1/2: 0.5","npv_sim:    54.7516131<br />scaled: 0.9796278<br />1/2: 0.5","npv_sim:    60.7873147<br />scaled: 0.9809296<br />1/2: 0.5","npv_sim:    66.8230162<br />scaled: 0.9821652<br />1/2: 0.5","npv_sim:    72.8587178<br />scaled: 0.9833674<br />1/2: 0.5","npv_sim:    78.8944193<br />scaled: 0.9845330<br />1/2: 0.5","npv_sim:    84.9301209<br />scaled: 0.9856357<br />1/2: 0.5","npv_sim:    90.9658224<br />scaled: 0.9867311<br />1/2: 0.5","npv_sim:    97.0015240<br />scaled: 0.9877347<br />1/2: 0.5","npv_sim:   103.0372255<br />scaled: 0.9887384<br />1/2: 0.5","npv_sim:   109.0729271<br />scaled: 0.9896652<br />1/2: 0.5","npv_sim:   115.1086286<br />scaled: 0.9905702<br />1/2: 0.5","npv_sim:   121.1443302<br />scaled: 0.9914279<br />1/2: 0.5","npv_sim:   127.1800317<br />scaled: 0.9922352<br />1/2: 0.5","npv_sim:   133.2157333<br />scaled: 0.9930241<br />1/2: 0.5","npv_sim:   139.2514348<br />scaled: 0.9937345<br />1/2: 0.5","npv_sim:   145.2871364<br />scaled: 0.9944450<br />1/2: 0.5","npv_sim:   151.3228379<br />scaled: 0.9950700<br />1/2: 0.5","npv_sim:   157.3585395<br />scaled: 0.9956848<br />1/2: 0.5","npv_sim:   163.3942411<br />scaled: 0.9962433<br />1/2: 0.5","npv_sim:   169.4299426<br />scaled: 0.9967640<br />1/2: 0.5","npv_sim:   175.4656442<br />scaled: 0.9972566<br />1/2: 0.5","npv_sim:   181.5013457<br />scaled: 0.9976847<br />1/2: 0.5","npv_sim:   187.5370473<br />scaled: 0.9981121<br />1/2: 0.5","npv_sim:   193.5727488<br />scaled: 0.9984494<br />1/2: 0.5","npv_sim:   199.6084504<br />scaled: 0.9987868<br />1/2: 0.5","npv_sim:   205.6441519<br />scaled: 0.9990609<br />1/2: 0.5","npv_sim:   211.6798535<br />scaled: 0.9993094<br />1/2: 0.5","npv_sim:   217.7155550<br />scaled: 0.9995218<br />1/2: 0.5","npv_sim:   223.7512566<br />scaled: 0.9996837<br />1/2: 0.5","npv_sim:   229.7869581<br />scaled: 0.9998352<br />1/2: 0.5","npv_sim:   235.8226597<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   241.8583612<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   247.8940628<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   253.9297643<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   259.9654659<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   266.0011675<br />scaled: 0.9998649<br />1/2: 0.5","npv_sim:   272.0368690<br />scaled: 0.9997628<br />1/2: 0.5","npv_sim:   278.0725706<br />scaled: 0.9996021<br />1/2: 0.5","npv_sim:   284.1082721<br />scaled: 0.9994415<br />1/2: 0.5","npv_sim:   290.1439737<br />scaled: 0.9992110<br />1/2: 0.5","npv_sim:   296.1796752<br />scaled: 0.9989763<br />1/2: 0.5","npv_sim:   302.2153768<br />scaled: 0.9986954<br />1/2: 0.5","npv_sim:   308.2510783<br />scaled: 0.9983894<br />1/2: 0.5","npv_sim:   314.2867799<br />scaled: 0.9980594<br />1/2: 0.5","npv_sim:   320.3224814<br />scaled: 0.9976851<br />1/2: 0.5","npv_sim:   326.3581830<br />scaled: 0.9973070<br />1/2: 0.5","npv_sim:   332.3938845<br />scaled: 0.9968673<br />1/2: 0.5","npv_sim:   338.4295861<br />scaled: 0.9964275<br />1/2: 0.5","npv_sim:   344.4652876<br />scaled: 0.9959402<br />1/2: 0.5","npv_sim:   350.5009892<br />scaled: 0.9954381<br />1/2: 0.5","npv_sim:   356.5366907<br />scaled: 0.9949082<br />1/2: 0.5","npv_sim:   362.5723923<br />scaled: 0.9943466<br />1/2: 0.5","npv_sim:   368.6080938<br />scaled: 0.9937754<br />1/2: 0.5","npv_sim:   374.6437954<br />scaled: 0.9931574<br />1/2: 0.5","npv_sim:   380.6794970<br />scaled: 0.9925395<br />1/2: 0.5","npv_sim:   386.7151985<br />scaled: 0.9918748<br />1/2: 0.5","npv_sim:   392.7509001<br />scaled: 0.9912035<br />1/2: 0.5","npv_sim:   398.7866016<br />scaled: 0.9905030<br />1/2: 0.5","npv_sim:   404.8223032<br />scaled: 0.9897814<br />1/2: 0.5","npv_sim:   410.8580047<br />scaled: 0.9890462<br />1/2: 0.5","npv_sim:   416.8937063<br />scaled: 0.9882773<br />1/2: 0.5","npv_sim:   422.9294078<br />scaled: 0.9875084<br />1/2: 0.5","npv_sim:   428.9651094<br />scaled: 0.9866955<br />1/2: 0.5","npv_sim:   435.0008109<br />scaled: 0.9858823<br />1/2: 0.5","npv_sim:   441.0365125<br />scaled: 0.9850402<br />1/2: 0.5","npv_sim:   447.0722140<br />scaled: 0.9841856<br />1/2: 0.5","npv_sim:   453.1079156<br />scaled: 0.9833155<br />1/2: 0.5","npv_sim:   459.1436171<br />scaled: 0.9824224<br />1/2: 0.5","npv_sim:   465.1793187<br />scaled: 0.9815254<br />1/2: 0.5","npv_sim:   471.2150202<br />scaled: 0.9805966<br />1/2: 0.5","npv_sim:   477.2507218<br />scaled: 0.9796678<br />1/2: 0.5","npv_sim:   483.2864234<br />scaled: 0.9787123<br />1/2: 0.5","npv_sim:   489.3221249<br />scaled: 0.9777506<br />1/2: 0.5","npv_sim:   495.3578265<br />scaled: 0.9767732<br />1/2: 0.5","npv_sim:   501.3935280<br />scaled: 0.9757813<br />1/2: 0.5","npv_sim:   507.4292296<br />scaled: 0.9747832<br />1/2: 0.5","npv_sim:   513.4649311<br />scaled: 0.9737636<br />1/2: 0.5","npv_sim:   519.5006327<br />scaled: 0.9727440<br />1/2: 0.5","npv_sim:   525.5363342<br />scaled: 0.9717012<br />1/2: 0.5","npv_sim:   531.5720358<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   537.6077373<br />scaled: 0.9695976<br />1/2: 0.5","npv_sim:   543.6434389<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   549.6791404<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   555.7148420<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   561.7505435<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   567.7862451<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   573.8219466<br />scaled: 0.9630686<br />1/2: 0.5","npv_sim:   579.8576482<br />scaled: 0.9619510<br />1/2: 0.5","npv_sim:   585.8933498<br />scaled: 0.9608293<br />1/2: 0.5","npv_sim:   591.9290513<br />scaled: 0.9597013<br />1/2: 0.5","npv_sim:   597.9647529<br />scaled: 0.9585657<br />1/2: 0.5","npv_sim:   604.0004544<br />scaled: 0.9574282<br />1/2: 0.5","npv_sim:   610.0361560<br />scaled: 0.9562806<br />1/2: 0.5","npv_sim:   616.0718575<br />scaled: 0.9551330<br />1/2: 0.5","npv_sim:   622.1075591<br />scaled: 0.9539767<br />1/2: 0.5","npv_sim:   628.1432606<br />scaled: 0.9528189<br />1/2: 0.5","npv_sim:   634.1789622<br />scaled: 0.9516564<br />1/2: 0.5","npv_sim:   640.2146637<br />scaled: 0.9504902<br />1/2: 0.5","npv_sim:   646.2503653<br />scaled: 0.9493221<br />1/2: 0.5","npv_sim:   652.2860668<br />scaled: 0.9481490<br />1/2: 0.5","npv_sim:   658.3217684<br />scaled: 0.9469760<br />1/2: 0.5","npv_sim:   664.3574699<br />scaled: 0.9457977<br />1/2: 0.5","npv_sim:   670.3931715<br />scaled: 0.9446193<br />1/2: 0.5","npv_sim:   676.4288730<br />scaled: 0.9434382<br />1/2: 0.5","npv_sim:   682.4645746<br />scaled: 0.9422559<br />1/2: 0.5","npv_sim:   688.5002761<br />scaled: 0.9410725<br />1/2: 0.5","npv_sim:   694.5359777<br />scaled: 0.9398875<br />1/2: 0.5","npv_sim:   700.5716793<br />scaled: 0.9387024<br />1/2: 0.5","npv_sim:   706.6073808<br />scaled: 0.9375159<br />1/2: 0.5","npv_sim:   712.6430824<br />scaled: 0.9363295<br />1/2: 0.5","npv_sim:   718.6787839<br />scaled: 0.9351427<br />1/2: 0.5","npv_sim:   724.7144855<br />scaled: 0.9339559<br />1/2: 0.5","npv_sim:   730.7501870<br />scaled: 0.9327694<br />1/2: 0.5","npv_sim:   736.7858886<br />scaled: 0.9315833<br />1/2: 0.5","npv_sim:   742.8215901<br />scaled: 0.9303975<br />1/2: 0.5","npv_sim:   748.8572917<br />scaled: 0.9292129<br />1/2: 0.5","npv_sim:   754.8929932<br />scaled: 0.9280283<br />1/2: 0.5","npv_sim:   760.9286948<br />scaled: 0.9268458<br />1/2: 0.5","npv_sim:   766.9643963<br />scaled: 0.9256636<br />1/2: 0.5","npv_sim:   773.0000979<br />scaled: 0.9244833<br />1/2: 0.5","npv_sim:   779.0357994<br />scaled: 0.9233042<br />1/2: 0.5","npv_sim:   785.0715010<br />scaled: 0.9221262<br />1/2: 0.5","npv_sim:   791.1072025<br />scaled: 0.9209509<br />1/2: 0.5","npv_sim:   797.1429041<br />scaled: 0.9197756<br />1/2: 0.5","npv_sim:   803.1786057<br />scaled: 0.9186046<br />1/2: 0.5","npv_sim:   809.2143072<br />scaled: 0.9174335<br />1/2: 0.5","npv_sim:   815.2500088<br />scaled: 0.9162660<br />1/2: 0.5","npv_sim:   821.2857103<br />scaled: 0.9150997<br />1/2: 0.5","npv_sim:   827.3214119<br />scaled: 0.9139358<br />1/2: 0.5","npv_sim:   833.3571134<br />scaled: 0.9127747<br />1/2: 0.5","npv_sim:   839.3928150<br />scaled: 0.9116145<br />1/2: 0.5","npv_sim:   845.4285165<br />scaled: 0.9104590<br />1/2: 0.5","npv_sim:   851.4642181<br />scaled: 0.9093035<br />1/2: 0.5","npv_sim:   857.4999196<br />scaled: 0.9081529<br />1/2: 0.5","npv_sim:   863.5356212<br />scaled: 0.9070033<br />1/2: 0.5","npv_sim:   869.5713227<br />scaled: 0.9058569<br />1/2: 0.5","npv_sim:   875.6070243<br />scaled: 0.9047133<br />1/2: 0.5","npv_sim:   881.6427258<br />scaled: 0.9035712<br />1/2: 0.5","npv_sim:   887.6784274<br />scaled: 0.9024337<br />1/2: 0.5","npv_sim:   893.7141289<br />scaled: 0.9012962<br />1/2: 0.5","npv_sim:   899.7498305<br />scaled: 0.9001647<br />1/2: 0.5","npv_sim:   905.7855321<br />scaled: 0.8990334<br />1/2: 0.5","npv_sim:   911.8212336<br />scaled: 0.8979062<br />1/2: 0.5","npv_sim:   917.8569352<br />scaled: 0.8967811<br />1/2: 0.5","npv_sim:   923.8926367<br />scaled: 0.8956583<br />1/2: 0.5","npv_sim:   929.9283383<br />scaled: 0.8945394<br />1/2: 0.5","npv_sim:   935.9640398<br />scaled: 0.8934209<br />1/2: 0.5","npv_sim:   941.9997414<br />scaled: 0.8923080<br />1/2: 0.5","npv_sim:   948.0354429<br />scaled: 0.8911951<br />1/2: 0.5","npv_sim:   954.0711445<br />scaled: 0.8900867<br />1/2: 0.5","npv_sim:   960.1068460<br />scaled: 0.8889796<br />1/2: 0.5","npv_sim:   966.1425476<br />scaled: 0.8878753<br />1/2: 0.5","npv_sim:   972.1782491<br />scaled: 0.8867738<br />1/2: 0.5","npv_sim:   978.2139507<br />scaled: 0.8856733<br />1/2: 0.5","npv_sim:   984.2496522<br />scaled: 0.8845771<br />1/2: 0.5","npv_sim:   990.2853538<br />scaled: 0.8834809<br />1/2: 0.5","npv_sim:   996.3210553<br />scaled: 0.8823891<br />1/2: 0.5","npv_sim:  1002.3567569<br />scaled: 0.8812978<br />1/2: 0.5","npv_sim:  1008.3924584<br />scaled: 0.8802092<br />1/2: 0.5","npv_sim:  1014.4281600<br />scaled: 0.8791223<br />1/2: 0.5","npv_sim:  1020.4638616<br />scaled: 0.8780367<br />1/2: 0.5","npv_sim:  1026.4995631<br />scaled: 0.8769539<br />1/2: 0.5","npv_sim:  1032.5352647<br />scaled: 0.8758711<br />1/2: 0.5","npv_sim:  1038.5709662<br />scaled: 0.8747916<br />1/2: 0.5","npv_sim:  1044.6066678<br />scaled: 0.8737122<br />1/2: 0.5","npv_sim:  1050.6423693<br />scaled: 0.8726348<br />1/2: 0.5","npv_sim:  1056.6780709<br />scaled: 0.8715581<br />1/2: 0.5","npv_sim:  1062.7137724<br />scaled: 0.8704824<br />1/2: 0.5","npv_sim:  1068.7494740<br />scaled: 0.8694078<br />1/2: 0.5","npv_sim:  1074.7851755<br />scaled: 0.8683334<br />1/2: 0.5","npv_sim:  1080.8208771<br />scaled: 0.8672602<br />1/2: 0.5","npv_sim:  1086.8565786<br />scaled: 0.8661870<br />1/2: 0.5","npv_sim:  1092.8922802<br />scaled: 0.8651141<br />1/2: 0.5","npv_sim:  1098.9279817<br />scaled: 0.8640414<br />1/2: 0.5","npv_sim:  1104.9636833<br />scaled: 0.8629685<br />1/2: 0.5","npv_sim:  1110.9993848<br />scaled: 0.8618953<br />1/2: 0.5","npv_sim:  1117.0350864<br />scaled: 0.8608218<br />1/2: 0.5","npv_sim:  1123.0707880<br />scaled: 0.8597473<br />1/2: 0.5","npv_sim:  1129.1064895<br />scaled: 0.8586727<br />1/2: 0.5","npv_sim:  1135.1421911<br />scaled: 0.8575958<br />1/2: 0.5","npv_sim:  1141.1778926<br />scaled: 0.8565188<br />1/2: 0.5","npv_sim:  1147.2135942<br />scaled: 0.8554395<br />1/2: 0.5","npv_sim:  1153.2492957<br />scaled: 0.8543589<br />1/2: 0.5","npv_sim:  1159.2849973<br />scaled: 0.8532766<br />1/2: 0.5","npv_sim:  1165.3206988<br />scaled: 0.8521912<br />1/2: 0.5","npv_sim:  1171.3564004<br />scaled: 0.8511055<br />1/2: 0.5","npv_sim:  1177.3921019<br />scaled: 0.8500140<br />1/2: 0.5","npv_sim:  1183.4278035<br />scaled: 0.8489226<br />1/2: 0.5","npv_sim:  1189.4635050<br />scaled: 0.8478254<br />1/2: 0.5","npv_sim:  1195.4992066<br />scaled: 0.8467265<br />1/2: 0.5","npv_sim:  1201.5349081<br />scaled: 0.8456234<br />1/2: 0.5","npv_sim:  1207.5706097<br />scaled: 0.8445156<br />1/2: 0.5","npv_sim:  1213.6063112<br />scaled: 0.8434059<br />1/2: 0.5","npv_sim:  1219.6420128<br />scaled: 0.8422877<br />1/2: 0.5","npv_sim:  1225.6777144<br />scaled: 0.8411694<br />1/2: 0.5","npv_sim:  1231.7134159<br />scaled: 0.8400406<br />1/2: 0.5","npv_sim:  1237.7491175<br />scaled: 0.8389103<br />1/2: 0.5","npv_sim:  1243.7848190<br />scaled: 0.8377720<br />1/2: 0.5","npv_sim:  1249.8205206<br />scaled: 0.8366280<br />1/2: 0.5","npv_sim:  1255.8562221<br />scaled: 0.8354796<br />1/2: 0.5","npv_sim:  1261.8919237<br />scaled: 0.8343200<br />1/2: 0.5","npv_sim:  1267.9276252<br />scaled: 0.8331605<br />1/2: 0.5","npv_sim:  1273.9633268<br />scaled: 0.8319838<br />1/2: 0.5","npv_sim:  1279.9990283<br />scaled: 0.8308070<br />1/2: 0.5","npv_sim:  1286.0347299<br />scaled: 0.8296168<br />1/2: 0.5","npv_sim:  1292.0704314<br />scaled: 0.8284207<br />1/2: 0.5","npv_sim:  1298.1061330<br />scaled: 0.8272161<br />1/2: 0.5","npv_sim:  1304.1418345<br />scaled: 0.8259988<br />1/2: 0.5","npv_sim:  1310.1775361<br />scaled: 0.8247790<br />1/2: 0.5","npv_sim:  1316.2132376<br />scaled: 0.8235385<br />1/2: 0.5","npv_sim:  1322.2489392<br />scaled: 0.8222979<br />1/2: 0.5","npv_sim:  1328.2846408<br />scaled: 0.8210367<br />1/2: 0.5","npv_sim:  1334.3203423<br />scaled: 0.8197707<br />1/2: 0.5","npv_sim:  1340.3560439<br />scaled: 0.8184906<br />1/2: 0.5","npv_sim:  1346.3917454<br />scaled: 0.8171970<br />1/2: 0.5","npv_sim:  1352.4274470<br />scaled: 0.8158969<br />1/2: 0.5","npv_sim:  1358.4631485<br />scaled: 0.8145736<br />1/2: 0.5","npv_sim:  1364.4988501<br />scaled: 0.8132502<br />1/2: 0.5","npv_sim:  1370.5345516<br />scaled: 0.8118972<br />1/2: 0.5","npv_sim:  1376.5702532<br />scaled: 0.8105418<br />1/2: 0.5","npv_sim:  1382.6059547<br />scaled: 0.8091648<br />1/2: 0.5","npv_sim:  1388.6416563<br />scaled: 0.8077749<br />1/2: 0.5","npv_sim:  1394.6773578<br />scaled: 0.8063729<br />1/2: 0.5","npv_sim:  1400.7130594<br />scaled: 0.8049463<br />1/2: 0.5","npv_sim:  1406.7487609<br />scaled: 0.8035182<br />1/2: 0.5","npv_sim:  1412.7844625<br />scaled: 0.8020525<br />1/2: 0.5","npv_sim:  1418.8201640<br />scaled: 0.8005869<br />1/2: 0.5","npv_sim:  1424.8558656<br />scaled: 0.7990903<br />1/2: 0.5","npv_sim:  1430.8915671<br />scaled: 0.7975831<br />1/2: 0.5","npv_sim:  1436.9272687<br />scaled: 0.7960562<br />1/2: 0.5","npv_sim:  1442.9629703<br />scaled: 0.7945050<br />1/2: 0.5","npv_sim:  1448.9986718<br />scaled: 0.7929468<br />1/2: 0.5","npv_sim:  1455.0343734<br />scaled: 0.7913493<br />1/2: 0.5","npv_sim:  1461.0700749<br />scaled: 0.7897518<br />1/2: 0.5","npv_sim:  1467.1057765<br />scaled: 0.7881124<br />1/2: 0.5","npv_sim:  1473.1414780<br />scaled: 0.7864662<br />1/2: 0.5","npv_sim:  1479.1771796<br />scaled: 0.7847911<br />1/2: 0.5","npv_sim:  1485.2128811<br />scaled: 0.7830937<br />1/2: 0.5","npv_sim:  1491.2485827<br />scaled: 0.7813819<br />1/2: 0.5","npv_sim:  1497.2842842<br />scaled: 0.7796310<br />1/2: 0.5","npv_sim:  1503.3199858<br />scaled: 0.7778801<br />1/2: 0.5","npv_sim:  1509.3556873<br />scaled: 0.7760748<br />1/2: 0.5","npv_sim:  1515.3913889<br />scaled: 0.7742681<br />1/2: 0.5","npv_sim:  1521.4270904<br />scaled: 0.7724219<br />1/2: 0.5","npv_sim:  1527.4627920<br />scaled: 0.7705570<br />1/2: 0.5","npv_sim:  1533.4984935<br />scaled: 0.7686690<br />1/2: 0.5","npv_sim:  1539.5341951<br />scaled: 0.7667438<br />1/2: 0.5","npv_sim:  1545.5698967<br />scaled: 0.7648131<br />1/2: 0.5","npv_sim:  1551.6055982<br />scaled: 0.7628254<br />1/2: 0.5","npv_sim:  1557.6412998<br />scaled: 0.7608377<br />1/2: 0.5","npv_sim:  1563.6770013<br />scaled: 0.7587989<br />1/2: 0.5","npv_sim:  1569.7127029<br />scaled: 0.7567466<br />1/2: 0.5","npv_sim:  1575.7484044<br />scaled: 0.7546612<br />1/2: 0.5","npv_sim:  1581.7841060<br />scaled: 0.7525425<br />1/2: 0.5","npv_sim:  1587.8198075<br />scaled: 0.7504098<br />1/2: 0.5","npv_sim:  1593.8555091<br />scaled: 0.7482226<br />1/2: 0.5","npv_sim:  1599.8912106<br />scaled: 0.7460355<br />1/2: 0.5","npv_sim:  1605.9269122<br />scaled: 0.7437846<br />1/2: 0.5","npv_sim:  1611.9626137<br />scaled: 0.7415273<br />1/2: 0.5","npv_sim:  1617.9983153<br />scaled: 0.7392259<br />1/2: 0.5","npv_sim:  1624.0340168<br />scaled: 0.7368968<br />1/2: 0.5","npv_sim:  1630.0697184<br />scaled: 0.7345444<br />1/2: 0.5","npv_sim:  1636.1054199<br />scaled: 0.7321419<br />1/2: 0.5","npv_sim:  1642.1411215<br />scaled: 0.7297378<br />1/2: 0.5","npv_sim:  1648.1768231<br />scaled: 0.7272607<br />1/2: 0.5","npv_sim:  1654.2125246<br />scaled: 0.7247836<br />1/2: 0.5","npv_sim:  1660.2482262<br />scaled: 0.7222513<br />1/2: 0.5","npv_sim:  1666.2839277<br />scaled: 0.7196983<br />1/2: 0.5","npv_sim:  1672.3196293<br />scaled: 0.7171121<br />1/2: 0.5","npv_sim:  1678.3553308<br />scaled: 0.7144822<br />1/2: 0.5","npv_sim:  1684.3910324<br />scaled: 0.7118417<br />1/2: 0.5","npv_sim:  1690.4267339<br />scaled: 0.7091340<br />1/2: 0.5","npv_sim:  1696.4624355<br />scaled: 0.7064264<br />1/2: 0.5","npv_sim:  1702.4981370<br />scaled: 0.7036526<br />1/2: 0.5","npv_sim:  1708.5338386<br />scaled: 0.7008665<br />1/2: 0.5","npv_sim:  1714.5695401<br />scaled: 0.6980372<br />1/2: 0.5","npv_sim:  1720.6052417<br />scaled: 0.6951721<br />1/2: 0.5","npv_sim:  1726.6409432<br />scaled: 0.6922870<br />1/2: 0.5","npv_sim:  1732.6766448<br />scaled: 0.6893427<br />1/2: 0.5","npv_sim:  1738.7123463<br />scaled: 0.6863983<br />1/2: 0.5","npv_sim:  1744.7480479<br />scaled: 0.6833779<br />1/2: 0.5","npv_sim:  1750.7837494<br />scaled: 0.6803542<br />1/2: 0.5","npv_sim:  1756.8194510<br />scaled: 0.6772779<br />1/2: 0.5","npv_sim:  1762.8551526<br />scaled: 0.6741748<br />1/2: 0.5","npv_sim:  1768.8908541<br />scaled: 0.6710428<br />1/2: 0.5","npv_sim:  1774.9265557<br />scaled: 0.6678607<br />1/2: 0.5","npv_sim:  1780.9622572<br />scaled: 0.6646731<br />1/2: 0.5","npv_sim:  1786.9979588<br />scaled: 0.6614125<br />1/2: 0.5","npv_sim:  1793.0336603<br />scaled: 0.6581519<br />1/2: 0.5","npv_sim:  1799.0693619<br />scaled: 0.6548310<br />1/2: 0.5","npv_sim:  1805.1050634<br />scaled: 0.6514927<br />1/2: 0.5","npv_sim:  1811.1407650<br />scaled: 0.6481174<br />1/2: 0.5","npv_sim:  1817.1764665<br />scaled: 0.6447022<br />1/2: 0.5","npv_sim:  1823.2121681<br />scaled: 0.6412730<br />1/2: 0.5","npv_sim:  1829.2478696<br />scaled: 0.6377822<br />1/2: 0.5","npv_sim:  1835.2835712<br />scaled: 0.6342913<br />1/2: 0.5","npv_sim:  1841.3192727<br />scaled: 0.6307343<br />1/2: 0.5","npv_sim:  1847.3549743<br />scaled: 0.6271692<br />1/2: 0.5","npv_sim:  1847.3549743<br />scaled: 0.6271692<br />1/2: 0.5","npv_sim:  1847.3549743<br />scaled: 0.6271692<br />1/2: 0.5","npv_sim:  1841.3192727<br />scaled: 0.6307343<br />1/2: 0.5","npv_sim:  1835.2835712<br />scaled: 0.6342913<br />1/2: 0.5","npv_sim:  1829.2478696<br />scaled: 0.6377822<br />1/2: 0.5","npv_sim:  1823.2121681<br />scaled: 0.6412730<br />1/2: 0.5","npv_sim:  1817.1764665<br />scaled: 0.6447022<br />1/2: 0.5","npv_sim:  1811.1407650<br />scaled: 0.6481174<br />1/2: 0.5","npv_sim:  1805.1050634<br />scaled: 0.6514927<br />1/2: 0.5","npv_sim:  1799.0693619<br />scaled: 0.6548310<br />1/2: 0.5","npv_sim:  1793.0336603<br />scaled: 0.6581519<br />1/2: 0.5","npv_sim:  1786.9979588<br />scaled: 0.6614125<br />1/2: 0.5","npv_sim:  1780.9622572<br />scaled: 0.6646731<br />1/2: 0.5","npv_sim:  1774.9265557<br />scaled: 0.6678607<br />1/2: 0.5","npv_sim:  1768.8908541<br />scaled: 0.6710428<br />1/2: 0.5","npv_sim:  1762.8551526<br />scaled: 0.6741748<br />1/2: 0.5","npv_sim:  1756.8194510<br />scaled: 0.6772779<br />1/2: 0.5","npv_sim:  1750.7837494<br />scaled: 0.6803542<br />1/2: 0.5","npv_sim:  1744.7480479<br />scaled: 0.6833779<br />1/2: 0.5","npv_sim:  1738.7123463<br />scaled: 0.6863983<br />1/2: 0.5","npv_sim:  1732.6766448<br />scaled: 0.6893427<br />1/2: 0.5","npv_sim:  1726.6409432<br />scaled: 0.6922870<br />1/2: 0.5","npv_sim:  1720.6052417<br />scaled: 0.6951721<br />1/2: 0.5","npv_sim:  1714.5695401<br />scaled: 0.6980372<br />1/2: 0.5","npv_sim:  1708.5338386<br />scaled: 0.7008665<br />1/2: 0.5","npv_sim:  1702.4981370<br />scaled: 0.7036526<br />1/2: 0.5","npv_sim:  1696.4624355<br />scaled: 0.7064264<br />1/2: 0.5","npv_sim:  1690.4267339<br />scaled: 0.7091340<br />1/2: 0.5","npv_sim:  1684.3910324<br />scaled: 0.7118417<br />1/2: 0.5","npv_sim:  1678.3553308<br />scaled: 0.7144822<br />1/2: 0.5","npv_sim:  1672.3196293<br />scaled: 0.7171121<br />1/2: 0.5","npv_sim:  1666.2839277<br />scaled: 0.7196983<br />1/2: 0.5","npv_sim:  1660.2482262<br />scaled: 0.7222513<br />1/2: 0.5","npv_sim:  1654.2125246<br />scaled: 0.7247836<br />1/2: 0.5","npv_sim:  1648.1768231<br />scaled: 0.7272607<br />1/2: 0.5","npv_sim:  1642.1411215<br />scaled: 0.7297378<br />1/2: 0.5","npv_sim:  1636.1054199<br />scaled: 0.7321419<br />1/2: 0.5","npv_sim:  1630.0697184<br />scaled: 0.7345444<br />1/2: 0.5","npv_sim:  1624.0340168<br />scaled: 0.7368968<br />1/2: 0.5","npv_sim:  1617.9983153<br />scaled: 0.7392259<br />1/2: 0.5","npv_sim:  1611.9626137<br />scaled: 0.7415273<br />1/2: 0.5","npv_sim:  1605.9269122<br />scaled: 0.7437846<br />1/2: 0.5","npv_sim:  1599.8912106<br />scaled: 0.7460355<br />1/2: 0.5","npv_sim:  1593.8555091<br />scaled: 0.7482226<br />1/2: 0.5","npv_sim:  1587.8198075<br />scaled: 0.7504098<br />1/2: 0.5","npv_sim:  1581.7841060<br />scaled: 0.7525425<br />1/2: 0.5","npv_sim:  1575.7484044<br />scaled: 0.7546612<br />1/2: 0.5","npv_sim:  1569.7127029<br />scaled: 0.7567466<br />1/2: 0.5","npv_sim:  1563.6770013<br />scaled: 0.7587989<br />1/2: 0.5","npv_sim:  1557.6412998<br />scaled: 0.7608377<br />1/2: 0.5","npv_sim:  1551.6055982<br />scaled: 0.7628254<br />1/2: 0.5","npv_sim:  1545.5698967<br />scaled: 0.7648131<br />1/2: 0.5","npv_sim:  1539.5341951<br />scaled: 0.7667438<br />1/2: 0.5","npv_sim:  1533.4984935<br />scaled: 0.7686690<br />1/2: 0.5","npv_sim:  1527.4627920<br />scaled: 0.7705570<br />1/2: 0.5","npv_sim:  1521.4270904<br />scaled: 0.7724219<br />1/2: 0.5","npv_sim:  1515.3913889<br />scaled: 0.7742681<br />1/2: 0.5","npv_sim:  1509.3556873<br />scaled: 0.7760748<br />1/2: 0.5","npv_sim:  1503.3199858<br />scaled: 0.7778801<br />1/2: 0.5","npv_sim:  1497.2842842<br />scaled: 0.7796310<br />1/2: 0.5","npv_sim:  1491.2485827<br />scaled: 0.7813819<br />1/2: 0.5","npv_sim:  1485.2128811<br />scaled: 0.7830937<br />1/2: 0.5","npv_sim:  1479.1771796<br />scaled: 0.7847911<br />1/2: 0.5","npv_sim:  1473.1414780<br />scaled: 0.7864662<br />1/2: 0.5","npv_sim:  1467.1057765<br />scaled: 0.7881124<br />1/2: 0.5","npv_sim:  1461.0700749<br />scaled: 0.7897518<br />1/2: 0.5","npv_sim:  1455.0343734<br />scaled: 0.7913493<br />1/2: 0.5","npv_sim:  1448.9986718<br />scaled: 0.7929468<br />1/2: 0.5","npv_sim:  1442.9629703<br />scaled: 0.7945050<br />1/2: 0.5","npv_sim:  1436.9272687<br />scaled: 0.7960562<br />1/2: 0.5","npv_sim:  1430.8915671<br />scaled: 0.7975831<br />1/2: 0.5","npv_sim:  1424.8558656<br />scaled: 0.7990903<br />1/2: 0.5","npv_sim:  1418.8201640<br />scaled: 0.8005869<br />1/2: 0.5","npv_sim:  1412.7844625<br />scaled: 0.8020525<br />1/2: 0.5","npv_sim:  1406.7487609<br />scaled: 0.8035182<br />1/2: 0.5","npv_sim:  1400.7130594<br />scaled: 0.8049463<br />1/2: 0.5","npv_sim:  1394.6773578<br />scaled: 0.8063729<br />1/2: 0.5","npv_sim:  1388.6416563<br />scaled: 0.8077749<br />1/2: 0.5","npv_sim:  1382.6059547<br />scaled: 0.8091648<br />1/2: 0.5","npv_sim:  1376.5702532<br />scaled: 0.8105418<br />1/2: 0.5","npv_sim:  1370.5345516<br />scaled: 0.8118972<br />1/2: 0.5","npv_sim:  1364.4988501<br />scaled: 0.8132502<br />1/2: 0.5","npv_sim:  1358.4631485<br />scaled: 0.8145736<br />1/2: 0.5","npv_sim:  1352.4274470<br />scaled: 0.8158969<br />1/2: 0.5","npv_sim:  1346.3917454<br />scaled: 0.8171970<br />1/2: 0.5","npv_sim:  1340.3560439<br />scaled: 0.8184906<br />1/2: 0.5","npv_sim:  1334.3203423<br />scaled: 0.8197707<br />1/2: 0.5","npv_sim:  1328.2846408<br />scaled: 0.8210367<br />1/2: 0.5","npv_sim:  1322.2489392<br />scaled: 0.8222979<br />1/2: 0.5","npv_sim:  1316.2132376<br />scaled: 0.8235385<br />1/2: 0.5","npv_sim:  1310.1775361<br />scaled: 0.8247790<br />1/2: 0.5","npv_sim:  1304.1418345<br />scaled: 0.8259988<br />1/2: 0.5","npv_sim:  1298.1061330<br />scaled: 0.8272161<br />1/2: 0.5","npv_sim:  1292.0704314<br />scaled: 0.8284207<br />1/2: 0.5","npv_sim:  1286.0347299<br />scaled: 0.8296168<br />1/2: 0.5","npv_sim:  1279.9990283<br />scaled: 0.8308070<br />1/2: 0.5","npv_sim:  1273.9633268<br />scaled: 0.8319838<br />1/2: 0.5","npv_sim:  1267.9276252<br />scaled: 0.8331605<br />1/2: 0.5","npv_sim:  1261.8919237<br />scaled: 0.8343200<br />1/2: 0.5","npv_sim:  1255.8562221<br />scaled: 0.8354796<br />1/2: 0.5","npv_sim:  1249.8205206<br />scaled: 0.8366280<br />1/2: 0.5","npv_sim:  1243.7848190<br />scaled: 0.8377720<br />1/2: 0.5","npv_sim:  1237.7491175<br />scaled: 0.8389103<br />1/2: 0.5","npv_sim:  1231.7134159<br />scaled: 0.8400406<br />1/2: 0.5","npv_sim:  1225.6777144<br />scaled: 0.8411694<br />1/2: 0.5","npv_sim:  1219.6420128<br />scaled: 0.8422877<br />1/2: 0.5","npv_sim:  1213.6063112<br />scaled: 0.8434059<br />1/2: 0.5","npv_sim:  1207.5706097<br />scaled: 0.8445156<br />1/2: 0.5","npv_sim:  1201.5349081<br />scaled: 0.8456234<br />1/2: 0.5","npv_sim:  1195.4992066<br />scaled: 0.8467265<br />1/2: 0.5","npv_sim:  1189.4635050<br />scaled: 0.8478254<br />1/2: 0.5","npv_sim:  1183.4278035<br />scaled: 0.8489226<br />1/2: 0.5","npv_sim:  1177.3921019<br />scaled: 0.8500140<br />1/2: 0.5","npv_sim:  1171.3564004<br />scaled: 0.8511055<br />1/2: 0.5","npv_sim:  1165.3206988<br />scaled: 0.8521912<br />1/2: 0.5","npv_sim:  1159.2849973<br />scaled: 0.8532766<br />1/2: 0.5","npv_sim:  1153.2492957<br />scaled: 0.8543589<br />1/2: 0.5","npv_sim:  1147.2135942<br />scaled: 0.8554395<br />1/2: 0.5","npv_sim:  1141.1778926<br />scaled: 0.8565188<br />1/2: 0.5","npv_sim:  1135.1421911<br />scaled: 0.8575958<br />1/2: 0.5","npv_sim:  1129.1064895<br />scaled: 0.8586727<br />1/2: 0.5","npv_sim:  1123.0707880<br />scaled: 0.8597473<br />1/2: 0.5","npv_sim:  1117.0350864<br />scaled: 0.8608218<br />1/2: 0.5","npv_sim:  1110.9993848<br />scaled: 0.8618953<br />1/2: 0.5","npv_sim:  1104.9636833<br />scaled: 0.8629685<br />1/2: 0.5","npv_sim:  1098.9279817<br />scaled: 0.8640414<br />1/2: 0.5","npv_sim:  1092.8922802<br />scaled: 0.8651141<br />1/2: 0.5","npv_sim:  1086.8565786<br />scaled: 0.8661870<br />1/2: 0.5","npv_sim:  1080.8208771<br />scaled: 0.8672602<br />1/2: 0.5","npv_sim:  1074.7851755<br />scaled: 0.8683334<br />1/2: 0.5","npv_sim:  1068.7494740<br />scaled: 0.8694078<br />1/2: 0.5","npv_sim:  1062.7137724<br />scaled: 0.8704824<br />1/2: 0.5","npv_sim:  1056.6780709<br />scaled: 0.8715581<br />1/2: 0.5","npv_sim:  1050.6423693<br />scaled: 0.8726348<br />1/2: 0.5","npv_sim:  1044.6066678<br />scaled: 0.8737122<br />1/2: 0.5","npv_sim:  1038.5709662<br />scaled: 0.8747916<br />1/2: 0.5","npv_sim:  1032.5352647<br />scaled: 0.8758711<br />1/2: 0.5","npv_sim:  1026.4995631<br />scaled: 0.8769539<br />1/2: 0.5","npv_sim:  1020.4638616<br />scaled: 0.8780367<br />1/2: 0.5","npv_sim:  1014.4281600<br />scaled: 0.8791223<br />1/2: 0.5","npv_sim:  1008.3924584<br />scaled: 0.8802092<br />1/2: 0.5","npv_sim:  1002.3567569<br />scaled: 0.8812978<br />1/2: 0.5","npv_sim:   996.3210553<br />scaled: 0.8823891<br />1/2: 0.5","npv_sim:   990.2853538<br />scaled: 0.8834809<br />1/2: 0.5","npv_sim:   984.2496522<br />scaled: 0.8845771<br />1/2: 0.5","npv_sim:   978.2139507<br />scaled: 0.8856733<br />1/2: 0.5","npv_sim:   972.1782491<br />scaled: 0.8867738<br />1/2: 0.5","npv_sim:   966.1425476<br />scaled: 0.8878753<br />1/2: 0.5","npv_sim:   960.1068460<br />scaled: 0.8889796<br />1/2: 0.5","npv_sim:   954.0711445<br />scaled: 0.8900867<br />1/2: 0.5","npv_sim:   948.0354429<br />scaled: 0.8911951<br />1/2: 0.5","npv_sim:   941.9997414<br />scaled: 0.8923080<br />1/2: 0.5","npv_sim:   935.9640398<br />scaled: 0.8934209<br />1/2: 0.5","npv_sim:   929.9283383<br />scaled: 0.8945394<br />1/2: 0.5","npv_sim:   923.8926367<br />scaled: 0.8956583<br />1/2: 0.5","npv_sim:   917.8569352<br />scaled: 0.8967811<br />1/2: 0.5","npv_sim:   911.8212336<br />scaled: 0.8979062<br />1/2: 0.5","npv_sim:   905.7855321<br />scaled: 0.8990334<br />1/2: 0.5","npv_sim:   899.7498305<br />scaled: 0.9001647<br />1/2: 0.5","npv_sim:   893.7141289<br />scaled: 0.9012962<br />1/2: 0.5","npv_sim:   887.6784274<br />scaled: 0.9024337<br />1/2: 0.5","npv_sim:   881.6427258<br />scaled: 0.9035712<br />1/2: 0.5","npv_sim:   875.6070243<br />scaled: 0.9047133<br />1/2: 0.5","npv_sim:   869.5713227<br />scaled: 0.9058569<br />1/2: 0.5","npv_sim:   863.5356212<br />scaled: 0.9070033<br />1/2: 0.5","npv_sim:   857.4999196<br />scaled: 0.9081529<br />1/2: 0.5","npv_sim:   851.4642181<br />scaled: 0.9093035<br />1/2: 0.5","npv_sim:   845.4285165<br />scaled: 0.9104590<br />1/2: 0.5","npv_sim:   839.3928150<br />scaled: 0.9116145<br />1/2: 0.5","npv_sim:   833.3571134<br />scaled: 0.9127747<br />1/2: 0.5","npv_sim:   827.3214119<br />scaled: 0.9139358<br />1/2: 0.5","npv_sim:   821.2857103<br />scaled: 0.9150997<br />1/2: 0.5","npv_sim:   815.2500088<br />scaled: 0.9162660<br />1/2: 0.5","npv_sim:   809.2143072<br />scaled: 0.9174335<br />1/2: 0.5","npv_sim:   803.1786057<br />scaled: 0.9186046<br />1/2: 0.5","npv_sim:   797.1429041<br />scaled: 0.9197756<br />1/2: 0.5","npv_sim:   791.1072025<br />scaled: 0.9209509<br />1/2: 0.5","npv_sim:   785.0715010<br />scaled: 0.9221262<br />1/2: 0.5","npv_sim:   779.0357994<br />scaled: 0.9233042<br />1/2: 0.5","npv_sim:   773.0000979<br />scaled: 0.9244833<br />1/2: 0.5","npv_sim:   766.9643963<br />scaled: 0.9256636<br />1/2: 0.5","npv_sim:   760.9286948<br />scaled: 0.9268458<br />1/2: 0.5","npv_sim:   754.8929932<br />scaled: 0.9280283<br />1/2: 0.5","npv_sim:   748.8572917<br />scaled: 0.9292129<br />1/2: 0.5","npv_sim:   742.8215901<br />scaled: 0.9303975<br />1/2: 0.5","npv_sim:   736.7858886<br />scaled: 0.9315833<br />1/2: 0.5","npv_sim:   730.7501870<br />scaled: 0.9327694<br />1/2: 0.5","npv_sim:   724.7144855<br />scaled: 0.9339559<br />1/2: 0.5","npv_sim:   718.6787839<br />scaled: 0.9351427<br />1/2: 0.5","npv_sim:   712.6430824<br />scaled: 0.9363295<br />1/2: 0.5","npv_sim:   706.6073808<br />scaled: 0.9375159<br />1/2: 0.5","npv_sim:   700.5716793<br />scaled: 0.9387024<br />1/2: 0.5","npv_sim:   694.5359777<br />scaled: 0.9398875<br />1/2: 0.5","npv_sim:   688.5002761<br />scaled: 0.9410725<br />1/2: 0.5","npv_sim:   682.4645746<br />scaled: 0.9422559<br />1/2: 0.5","npv_sim:   676.4288730<br />scaled: 0.9434382<br />1/2: 0.5","npv_sim:   670.3931715<br />scaled: 0.9446193<br />1/2: 0.5","npv_sim:   664.3574699<br />scaled: 0.9457977<br />1/2: 0.5","npv_sim:   658.3217684<br />scaled: 0.9469760<br />1/2: 0.5","npv_sim:   652.2860668<br />scaled: 0.9481490<br />1/2: 0.5","npv_sim:   646.2503653<br />scaled: 0.9493221<br />1/2: 0.5","npv_sim:   640.2146637<br />scaled: 0.9504902<br />1/2: 0.5","npv_sim:   634.1789622<br />scaled: 0.9516564<br />1/2: 0.5","npv_sim:   628.1432606<br />scaled: 0.9528189<br />1/2: 0.5","npv_sim:   622.1075591<br />scaled: 0.9539767<br />1/2: 0.5","npv_sim:   616.0718575<br />scaled: 0.9551330<br />1/2: 0.5","npv_sim:   610.0361560<br />scaled: 0.9562806<br />1/2: 0.5","npv_sim:   604.0004544<br />scaled: 0.9574282<br />1/2: 0.5","npv_sim:   597.9647529<br />scaled: 0.9585657<br />1/2: 0.5","npv_sim:   591.9290513<br />scaled: 0.9597013<br />1/2: 0.5","npv_sim:   585.8933498<br />scaled: 0.9608293<br />1/2: 0.5","npv_sim:   579.8576482<br />scaled: 0.9619510<br />1/2: 0.5","npv_sim:   573.8219466<br />scaled: 0.9630686<br />1/2: 0.5","npv_sim:   567.7862451<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   561.7505435<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   555.7148420<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   549.6791404<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   543.6434389<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   537.6077373<br />scaled: 0.9695976<br />1/2: 0.5","npv_sim:   531.5720358<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   525.5363342<br />scaled: 0.9717012<br />1/2: 0.5","npv_sim:   519.5006327<br />scaled: 0.9727440<br />1/2: 0.5","npv_sim:   513.4649311<br />scaled: 0.9737636<br />1/2: 0.5","npv_sim:   507.4292296<br />scaled: 0.9747832<br />1/2: 0.5","npv_sim:   501.3935280<br />scaled: 0.9757813<br />1/2: 0.5","npv_sim:   495.3578265<br />scaled: 0.9767732<br />1/2: 0.5","npv_sim:   489.3221249<br />scaled: 0.9777506<br />1/2: 0.5","npv_sim:   483.2864234<br />scaled: 0.9787123<br />1/2: 0.5","npv_sim:   477.2507218<br />scaled: 0.9796678<br />1/2: 0.5","npv_sim:   471.2150202<br />scaled: 0.9805966<br />1/2: 0.5","npv_sim:   465.1793187<br />scaled: 0.9815254<br />1/2: 0.5","npv_sim:   459.1436171<br />scaled: 0.9824224<br />1/2: 0.5","npv_sim:   453.1079156<br />scaled: 0.9833155<br />1/2: 0.5","npv_sim:   447.0722140<br />scaled: 0.9841856<br />1/2: 0.5","npv_sim:   441.0365125<br />scaled: 0.9850402<br />1/2: 0.5","npv_sim:   435.0008109<br />scaled: 0.9858823<br />1/2: 0.5","npv_sim:   428.9651094<br />scaled: 0.9866955<br />1/2: 0.5","npv_sim:   422.9294078<br />scaled: 0.9875084<br />1/2: 0.5","npv_sim:   416.8937063<br />scaled: 0.9882773<br />1/2: 0.5","npv_sim:   410.8580047<br />scaled: 0.9890462<br />1/2: 0.5","npv_sim:   404.8223032<br />scaled: 0.9897814<br />1/2: 0.5","npv_sim:   398.7866016<br />scaled: 0.9905030<br />1/2: 0.5","npv_sim:   392.7509001<br />scaled: 0.9912035<br />1/2: 0.5","npv_sim:   386.7151985<br />scaled: 0.9918748<br />1/2: 0.5","npv_sim:   380.6794970<br />scaled: 0.9925395<br />1/2: 0.5","npv_sim:   374.6437954<br />scaled: 0.9931574<br />1/2: 0.5","npv_sim:   368.6080938<br />scaled: 0.9937754<br />1/2: 0.5","npv_sim:   362.5723923<br />scaled: 0.9943466<br />1/2: 0.5","npv_sim:   356.5366907<br />scaled: 0.9949082<br />1/2: 0.5","npv_sim:   350.5009892<br />scaled: 0.9954381<br />1/2: 0.5","npv_sim:   344.4652876<br />scaled: 0.9959402<br />1/2: 0.5","npv_sim:   338.4295861<br />scaled: 0.9964275<br />1/2: 0.5","npv_sim:   332.3938845<br />scaled: 0.9968673<br />1/2: 0.5","npv_sim:   326.3581830<br />scaled: 0.9973070<br />1/2: 0.5","npv_sim:   320.3224814<br />scaled: 0.9976851<br />1/2: 0.5","npv_sim:   314.2867799<br />scaled: 0.9980594<br />1/2: 0.5","npv_sim:   308.2510783<br />scaled: 0.9983894<br />1/2: 0.5","npv_sim:   302.2153768<br />scaled: 0.9986954<br />1/2: 0.5","npv_sim:   296.1796752<br />scaled: 0.9989763<br />1/2: 0.5","npv_sim:   290.1439737<br />scaled: 0.9992110<br />1/2: 0.5","npv_sim:   284.1082721<br />scaled: 0.9994415<br />1/2: 0.5","npv_sim:   278.0725706<br />scaled: 0.9996021<br />1/2: 0.5","npv_sim:   272.0368690<br />scaled: 0.9997628<br />1/2: 0.5","npv_sim:   266.0011675<br />scaled: 0.9998649<br />1/2: 0.5","npv_sim:   259.9654659<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   253.9297643<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   247.8940628<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   241.8583612<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   235.8226597<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   229.7869581<br />scaled: 0.9998352<br />1/2: 0.5","npv_sim:   223.7512566<br />scaled: 0.9996837<br />1/2: 0.5","npv_sim:   217.7155550<br />scaled: 0.9995218<br />1/2: 0.5","npv_sim:   211.6798535<br />scaled: 0.9993094<br />1/2: 0.5","npv_sim:   205.6441519<br />scaled: 0.9990609<br />1/2: 0.5","npv_sim:   199.6084504<br />scaled: 0.9987868<br />1/2: 0.5","npv_sim:   193.5727488<br />scaled: 0.9984494<br />1/2: 0.5","npv_sim:   187.5370473<br />scaled: 0.9981121<br />1/2: 0.5","npv_sim:   181.5013457<br />scaled: 0.9976847<br />1/2: 0.5","npv_sim:   175.4656442<br />scaled: 0.9972566<br />1/2: 0.5","npv_sim:   169.4299426<br />scaled: 0.9967640<br />1/2: 0.5","npv_sim:   163.3942411<br />scaled: 0.9962433<br />1/2: 0.5","npv_sim:   157.3585395<br />scaled: 0.9956848<br />1/2: 0.5","npv_sim:   151.3228379<br />scaled: 0.9950700<br />1/2: 0.5","npv_sim:   145.2871364<br />scaled: 0.9944450<br />1/2: 0.5","npv_sim:   139.2514348<br />scaled: 0.9937345<br />1/2: 0.5","npv_sim:   133.2157333<br />scaled: 0.9930241<br />1/2: 0.5","npv_sim:   127.1800317<br />scaled: 0.9922352<br />1/2: 0.5","npv_sim:   121.1443302<br />scaled: 0.9914279<br />1/2: 0.5","npv_sim:   115.1086286<br />scaled: 0.9905702<br />1/2: 0.5","npv_sim:   109.0729271<br />scaled: 0.9896652<br />1/2: 0.5","npv_sim:   103.0372255<br />scaled: 0.9887384<br />1/2: 0.5","npv_sim:    97.0015240<br />scaled: 0.9877347<br />1/2: 0.5","npv_sim:    90.9658224<br />scaled: 0.9867311<br />1/2: 0.5","npv_sim:    84.9301209<br />scaled: 0.9856357<br />1/2: 0.5","npv_sim:    78.8944193<br />scaled: 0.9845330<br />1/2: 0.5","npv_sim:    72.8587178<br />scaled: 0.9833674<br />1/2: 0.5","npv_sim:    66.8230162<br />scaled: 0.9821652<br />1/2: 0.5","npv_sim:    60.7873147<br />scaled: 0.9809296<br />1/2: 0.5","npv_sim:    54.7516131<br />scaled: 0.9796278<br />1/2: 0.5","npv_sim:    48.7159115<br />scaled: 0.9783220<br />1/2: 0.5","npv_sim:    42.6802100<br />scaled: 0.9769208<br />1/2: 0.5","npv_sim:    36.6445084<br />scaled: 0.9755196<br />1/2: 0.5","npv_sim:    30.6088069<br />scaled: 0.9740447<br />1/2: 0.5","npv_sim:    24.5731053<br />scaled: 0.9725445<br />1/2: 0.5","npv_sim:    18.5374038<br />scaled: 0.9710000<br />1/2: 0.5","npv_sim:    12.5017022<br />scaled: 0.9694015<br />1/2: 0.5","npv_sim:     6.4660007<br />scaled: 0.9677879<br />1/2: 0.5","npv_sim:     0.4302991<br />scaled: 0.9660919<br />1/2: 0.5","npv_sim:    -5.6054024<br />scaled: 0.9643959<br />1/2: 0.5","npv_sim:   -11.6411040<br />scaled: 0.9626171<br />1/2: 0.5","npv_sim:   -17.6768055<br />scaled: 0.9608248<br />1/2: 0.5","npv_sim:   -23.7125071<br />scaled: 0.9589789<br />1/2: 0.5","npv_sim:   -29.7482086<br />scaled: 0.9570917<br />1/2: 0.5","npv_sim:   -35.7839102<br />scaled: 0.9551793<br />1/2: 0.5","npv_sim:   -41.8196117<br />scaled: 0.9531987<br />1/2: 0.5","npv_sim:   -47.8553133<br />scaled: 0.9512182<br />1/2: 0.5","npv_sim:   -53.8910149<br />scaled: 0.9491485<br />1/2: 0.5","npv_sim:   -59.9267164<br />scaled: 0.9470765<br />1/2: 0.5","npv_sim:   -65.9624180<br />scaled: 0.9449438<br />1/2: 0.5","npv_sim:   -71.9981195<br />scaled: 0.9427825<br />1/2: 0.5","npv_sim:   -78.0338211<br />scaled: 0.9405876<br />1/2: 0.5","npv_sim:   -84.0695226<br />scaled: 0.9383392<br />1/2: 0.5","npv_sim:   -90.1052242<br />scaled: 0.9360832<br />1/2: 0.5","npv_sim:   -96.1409257<br />scaled: 0.9337503<br />1/2: 0.5","npv_sim:  -102.1766273<br />scaled: 0.9314173<br />1/2: 0.5","npv_sim:  -108.2123288<br />scaled: 0.9290194<br />1/2: 0.5","npv_sim:  -114.2480304<br />scaled: 0.9266046<br />1/2: 0.5","npv_sim:  -120.2837319<br />scaled: 0.9241505<br />1/2: 0.5","npv_sim:  -126.3194335<br />scaled: 0.9216568<br />1/2: 0.5","npv_sim:  -132.3551350<br />scaled: 0.9191477<br />1/2: 0.5","npv_sim:  -138.3908366<br />scaled: 0.9165783<br />1/2: 0.5","npv_sim:  -144.4265381<br />scaled: 0.9140088<br />1/2: 0.5","npv_sim:  -150.4622397<br />scaled: 0.9113735<br />1/2: 0.5","npv_sim:  -156.4979412<br />scaled: 0.9087315<br />1/2: 0.5","npv_sim:  -162.5336428<br />scaled: 0.9060472<br />1/2: 0.5","npv_sim:  -168.5693444<br />scaled: 0.9033361<br />1/2: 0.5","npv_sim:  -174.6050459<br />scaled: 0.9006042<br />1/2: 0.5","npv_sim:  -180.6407475<br />scaled: 0.8978277<br />1/2: 0.5","npv_sim:  -186.6764490<br />scaled: 0.8950497<br />1/2: 0.5","npv_sim:  -192.7121506<br />scaled: 0.8922113<br />1/2: 0.5","npv_sim:  -198.7478521<br />scaled: 0.8893730<br />1/2: 0.5","npv_sim:  -204.7835537<br />scaled: 0.8864924<br />1/2: 0.5","npv_sim:  -210.8192552<br />scaled: 0.8835961<br />1/2: 0.5","npv_sim:  -216.8549568<br />scaled: 0.8806764<br />1/2: 0.5","npv_sim:  -222.8906583<br />scaled: 0.8777260<br />1/2: 0.5","npv_sim:  -228.9263599<br />scaled: 0.8747687<br />1/2: 0.5","npv_sim:  -234.9620614<br />scaled: 0.8717683<br />1/2: 0.5","npv_sim:  -240.9977630<br />scaled: 0.8687679<br />1/2: 0.5","npv_sim:  -247.0334645<br />scaled: 0.8657287<br />1/2: 0.5","npv_sim:  -253.0691661<br />scaled: 0.8626823<br />1/2: 0.5","npv_sim:  -259.1048676<br />scaled: 0.8596129<br />1/2: 0.5","npv_sim:  -265.1405692<br />scaled: 0.8565246<br />1/2: 0.5","npv_sim:  -271.1762708<br />scaled: 0.8534267<br />1/2: 0.5","npv_sim:  -277.2119723<br />scaled: 0.8503007<br />1/2: 0.5","npv_sim:  -283.2476739<br />scaled: 0.8471747<br />1/2: 0.5","npv_sim:  -289.2833754<br />scaled: 0.8440165<br />1/2: 0.5","npv_sim:  -295.3190770<br />scaled: 0.8408569<br />1/2: 0.5","npv_sim:  -301.3547785<br />scaled: 0.8376778<br />1/2: 0.5","npv_sim:  -307.3904801<br />scaled: 0.8344888<br />1/2: 0.5","npv_sim:  -313.4261816<br />scaled: 0.8312905<br />1/2: 0.5","npv_sim:  -319.4618832<br />scaled: 0.8280762<br />1/2: 0.5","npv_sim:  -325.4975847<br />scaled: 0.8248603<br />1/2: 0.5","npv_sim:  -331.5332863<br />scaled: 0.8216249<br />1/2: 0.5","npv_sim:  -337.5689878<br />scaled: 0.8183894<br />1/2: 0.5","npv_sim:  -343.6046894<br />scaled: 0.8151406<br />1/2: 0.5","npv_sim:  -349.6403909<br />scaled: 0.8118880<br />1/2: 0.5","npv_sim:  -355.6760925<br />scaled: 0.8086291<br />1/2: 0.5","npv_sim:  -361.7117940<br />scaled: 0.8053633<br />1/2: 0.5","npv_sim:  -367.7474956<br />scaled: 0.8020958<br />1/2: 0.5","npv_sim:  -373.7831972<br />scaled: 0.7988208<br />1/2: 0.5","npv_sim:  -379.8188987<br />scaled: 0.7955458<br />1/2: 0.5","npv_sim:  -385.8546003<br />scaled: 0.7922660<br />1/2: 0.5","npv_sim:  -391.8903018<br />scaled: 0.7889855<br />1/2: 0.5","npv_sim:  -397.9260034<br />scaled: 0.7857040<br />1/2: 0.5","npv_sim:  -403.9617049<br />scaled: 0.7824218<br />1/2: 0.5","npv_sim:  -409.9974065<br />scaled: 0.7791401<br />1/2: 0.5","npv_sim:  -416.0331080<br />scaled: 0.7758596<br />1/2: 0.5","npv_sim:  -422.0688096<br />scaled: 0.7725792<br />1/2: 0.5","npv_sim:  -428.1045111<br />scaled: 0.7693040<br />1/2: 0.5","npv_sim:  -434.1402127<br />scaled: 0.7660287<br />1/2: 0.5","npv_sim:  -440.1759142<br />scaled: 0.7627595<br />1/2: 0.5","npv_sim:  -446.2116158<br />scaled: 0.7594927<br />1/2: 0.5","npv_sim:  -452.2473173<br />scaled: 0.7562307<br />1/2: 0.5","npv_sim:  -458.2830189<br />scaled: 0.7529755<br />1/2: 0.5","npv_sim:  -464.3187204<br />scaled: 0.7497220<br />1/2: 0.5","npv_sim:  -470.3544220<br />scaled: 0.7464812<br />1/2: 0.5","npv_sim:  -476.3901235<br />scaled: 0.7432405<br />1/2: 0.5","npv_sim:  -482.4258251<br />scaled: 0.7400139<br />1/2: 0.5","npv_sim:  -488.4615267<br />scaled: 0.7367904<br />1/2: 0.5","npv_sim:  -494.4972282<br />scaled: 0.7335774<br />1/2: 0.5","npv_sim:  -500.5329298<br />scaled: 0.7303736<br />1/2: 0.5","npv_sim:  -506.5686313<br />scaled: 0.7271751<br />1/2: 0.5","npv_sim:  -512.6043329<br />scaled: 0.7239934<br />1/2: 0.5","npv_sim:  -518.6400344<br />scaled: 0.7208118<br />1/2: 0.5","npv_sim:  -524.6757360<br />scaled: 0.7176530<br />1/2: 0.5","npv_sim:  -530.7114375<br />scaled: 0.7144956<br />1/2: 0.5","npv_sim:  -536.7471391<br />scaled: 0.7113552<br />1/2: 0.5","npv_sim:  -542.7828406<br />scaled: 0.7082240<br />1/2: 0.5","npv_sim:  -548.8185422<br />scaled: 0.7051027<br />1/2: 0.5","npv_sim:  -554.8542437<br />scaled: 0.7019995<br />1/2: 0.5","npv_sim:  -560.8899453<br />scaled: 0.6988979<br />1/2: 0.5","npv_sim:  -566.9256468<br />scaled: 0.6958241<br />1/2: 0.5","npv_sim:  -572.9613484<br />scaled: 0.6927503<br />1/2: 0.5","npv_sim:  -578.9970499<br />scaled: 0.6896999<br />1/2: 0.5","npv_sim:  -585.0327515<br />scaled: 0.6866569<br />1/2: 0.5","npv_sim:  -591.0684531<br />scaled: 0.6836286<br />1/2: 0.5","npv_sim:  -597.1041546<br />scaled: 0.6806174<br />1/2: 0.5","npv_sim:  -603.1398562<br />scaled: 0.6776117<br />1/2: 0.5","npv_sim:  -609.1755577<br />scaled: 0.6746331<br />1/2: 0.5","npv_sim:  -615.2112593<br />scaled: 0.6716545<br />1/2: 0.5","npv_sim:  -621.2469608<br />scaled: 0.6687051<br />1/2: 0.5","npv_sim:  -627.2826624<br />scaled: 0.6657598<br />1/2: 0.5","npv_sim:  -633.3183639<br />scaled: 0.6628341<br />1/2: 0.5","npv_sim:  -639.3540655<br />scaled: 0.6599225<br />1/2: 0.5","npv_sim:  -645.3897670<br />scaled: 0.6570206<br />1/2: 0.5","npv_sim:  -651.4254686<br />scaled: 0.6541430<br />1/2: 0.5","npv_sim:  -657.4611701<br />scaled: 0.6512654<br />1/2: 0.5","npv_sim:  -663.4968717<br />scaled: 0.6484214<br />1/2: 0.5","npv_sim:  -669.5325732<br />scaled: 0.6455777<br />1/2: 0.5","npv_sim:  -675.5682748<br />scaled: 0.6427575<br />1/2: 0.5","npv_sim:  -681.6039763<br />scaled: 0.6399476<br />1/2: 0.5","npv_sim:  -687.6396779<br />scaled: 0.6371510<br />1/2: 0.5","npv_sim:  -693.6753795<br />scaled: 0.6343744<br />1/2: 0.5","npv_sim:  -699.7110810<br />scaled: 0.6316013<br />1/2: 0.5","npv_sim:  -705.7467826<br />scaled: 0.6288576<br />1/2: 0.5","npv_sim:  -711.7824841<br />scaled: 0.6261139<br />1/2: 0.5","npv_sim:  -717.8181857<br />scaled: 0.6233961<br />1/2: 0.5","npv_sim:  -723.8538872<br />scaled: 0.6206844<br />1/2: 0.5","npv_sim:  -729.8895888<br />scaled: 0.6179887<br />1/2: 0.5","npv_sim:  -735.9252903<br />scaled: 0.6153082<br />1/2: 0.5","npv_sim:  -741.9609919<br />scaled: 0.6126342<br />1/2: 0.5","npv_sim:  -747.9966934<br />scaled: 0.6099837<br />1/2: 0.5","npv_sim:  -754.0323950<br />scaled: 0.6073332<br />1/2: 0.5","npv_sim:  -760.0680965<br />scaled: 0.6047094<br />1/2: 0.5","npv_sim:  -766.1037981<br />scaled: 0.6020878<br />1/2: 0.5","npv_sim:  -772.1394996<br />scaled: 0.5994834<br />1/2: 0.5","npv_sim:  -778.1752012<br />scaled: 0.5968892<br />1/2: 0.5","npv_sim:  -784.2109027<br />scaled: 0.5943037<br />1/2: 0.5","npv_sim:  -790.2466043<br />scaled: 0.5917355<br />1/2: 0.5","npv_sim:  -796.2823059<br />scaled: 0.5891683<br />1/2: 0.5","npv_sim:  -802.3180074<br />scaled: 0.5866244<br />1/2: 0.5","npv_sim:  -808.3537090<br />scaled: 0.5840806<br />1/2: 0.5","npv_sim:  -814.3894105<br />scaled: 0.5815536<br />1/2: 0.5","npv_sim:  -820.4251121<br />scaled: 0.5790324<br />1/2: 0.5","npv_sim:  -826.4608136<br />scaled: 0.5765206<br />1/2: 0.5","npv_sim:  -832.4965152<br />scaled: 0.5740203<br />1/2: 0.5","npv_sim:  -838.5322167<br />scaled: 0.5715228<br />1/2: 0.5","npv_sim:  -844.5679183<br />scaled: 0.5690414<br />1/2: 0.5","npv_sim:  -850.6036198<br />scaled: 0.5665601<br />1/2: 0.5","npv_sim:  -856.6393214<br />scaled: 0.5640933<br />1/2: 0.5","npv_sim:  -862.6750229<br />scaled: 0.5616289<br />1/2: 0.5","npv_sim:  -868.7107245<br />scaled: 0.5591729<br />1/2: 0.5","npv_sim:  -874.7464260<br />scaled: 0.5567235<br />1/2: 0.5","npv_sim:  -880.7821276<br />scaled: 0.5542776<br />1/2: 0.5","npv_sim:  -886.8178291<br />scaled: 0.5518411<br />1/2: 0.5","npv_sim:  -892.8535307<br />scaled: 0.5494046<br />1/2: 0.5","npv_sim:  -898.8892322<br />scaled: 0.5469787<br />1/2: 0.5","npv_sim:  -904.9249338<br />scaled: 0.5445531<br />1/2: 0.5","npv_sim:  -910.9606354<br />scaled: 0.5421334<br />1/2: 0.5","npv_sim:  -916.9963369<br />scaled: 0.5397165<br />1/2: 0.5","npv_sim:  -923.0320385<br />scaled: 0.5373022<br />1/2: 0.5","npv_sim:  -929.0677400<br />scaled: 0.5348920<br />1/2: 0.5","npv_sim:  -935.1034416<br />scaled: 0.5324821<br />1/2: 0.5","npv_sim:  -941.1391431<br />scaled: 0.5300764<br />1/2: 0.5","npv_sim:  -947.1748447<br />scaled: 0.5276707<br />1/2: 0.5","npv_sim:  -953.2105462<br />scaled: 0.5252670<br />1/2: 0.5","npv_sim:  -959.2462478<br />scaled: 0.5228637<br />1/2: 0.5","npv_sim:  -965.2819493<br />scaled: 0.5204607<br />1/2: 0.5","npv_sim:  -971.3176509<br />scaled: 0.5180579<br />1/2: 0.5","npv_sim:  -977.3533524<br />scaled: 0.5156547<br />1/2: 0.5","npv_sim:  -983.3890540<br />scaled: 0.5132503<br />1/2: 0.5","npv_sim:  -989.4247555<br />scaled: 0.5108458<br />1/2: 0.5","npv_sim:  -995.4604571<br />scaled: 0.5084382<br />1/2: 0.5","npv_sim: -1001.4961586<br />scaled: 0.5060302<br />1/2: 0.5","npv_sim: -1007.5318602<br />scaled: 0.5036188<br />1/2: 0.5","npv_sim: -1013.5675618<br />scaled: 0.5012054<br />1/2: 0.5","npv_sim: -1019.6032633<br />scaled: 0.4987896<br />1/2: 0.5","npv_sim: -1025.6389649<br />scaled: 0.4963688<br />1/2: 0.5","npv_sim: -1031.6746664<br />scaled: 0.4939479<br />1/2: 0.5","npv_sim: -1037.7103680<br />scaled: 0.4915181<br />1/2: 0.5","npv_sim: -1043.7460695<br />scaled: 0.4890884<br />1/2: 0.5","npv_sim: -1049.7817711<br />scaled: 0.4866509<br />1/2: 0.5","npv_sim: -1055.8174726<br />scaled: 0.4842104<br />1/2: 0.5","npv_sim: -1061.8531742<br />scaled: 0.4817647<br />1/2: 0.5","npv_sim: -1067.8888757<br />scaled: 0.4793120<br />1/2: 0.5","npv_sim: -1073.9245773<br />scaled: 0.4768575<br />1/2: 0.5","npv_sim: -1079.9602788<br />scaled: 0.4743911<br />1/2: 0.5","npv_sim: -1085.9959804<br />scaled: 0.4719248<br />1/2: 0.5","npv_sim: -1092.0316819<br />scaled: 0.4694458<br />1/2: 0.5","npv_sim: -1098.0673835<br />scaled: 0.4669645<br />1/2: 0.5","npv_sim: -1104.1030850<br />scaled: 0.4644742<br />1/2: 0.5","npv_sim: -1110.1387866<br />scaled: 0.4619766<br />1/2: 0.5","npv_sim: -1116.1744882<br />scaled: 0.4594747<br />1/2: 0.5","npv_sim: -1122.2101897<br />scaled: 0.4569598<br />1/2: 0.5","npv_sim: -1128.2458913<br />scaled: 0.4544449<br />1/2: 0.5","npv_sim: -1134.2815928<br />scaled: 0.4519125<br />1/2: 0.5","npv_sim: -1140.3172944<br />scaled: 0.4493793<br />1/2: 0.5","npv_sim: -1146.3529959<br />scaled: 0.4468334<br />1/2: 0.5","npv_sim: -1152.3886975<br />scaled: 0.4442811<br />1/2: 0.5","npv_sim: -1158.4243990<br />scaled: 0.4417215<br />1/2: 0.5","npv_sim: -1164.4601006<br />scaled: 0.4391494<br />1/2: 0.5","npv_sim: -1170.4958021<br />scaled: 0.4365758<br />1/2: 0.5","npv_sim: -1176.5315037<br />scaled: 0.4339833<br />1/2: 0.5","npv_sim: -1182.5672052<br />scaled: 0.4313908<br />1/2: 0.5","npv_sim: -1188.6029068<br />scaled: 0.4287822<br />1/2: 0.5","npv_sim: -1194.6386083<br />scaled: 0.4261690<br />1/2: 0.5","npv_sim: -1200.6743099<br />scaled: 0.4235456<br />1/2: 0.5","npv_sim: -1206.7100114<br />scaled: 0.4209113<br />1/2: 0.5","npv_sim: -1212.7457130<br />scaled: 0.4182730<br />1/2: 0.5","npv_sim: -1218.7814145<br />scaled: 0.4156175<br />1/2: 0.5","npv_sim: -1224.8171161<br />scaled: 0.4129620<br />1/2: 0.5","npv_sim: -1230.8528177<br />scaled: 0.4102877<br />1/2: 0.5","npv_sim: -1236.8885192<br />scaled: 0.4076111<br />1/2: 0.5","npv_sim: -1236.8885192<br />scaled: 0.4076111<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,394.144458504669,394.144458504669],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 394.1445","xintercept: 394.1445"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.216687757004],"y":[0.25],"text":"Median NPV:<br />  394.14","hovertext":"x: 591.2167<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.216687757004],"y":[0.1],"text":"SD NPV:<br />  1008.86","hovertext":"x: 591.2167<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic of Effects From Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1669.32083265914,2401.88057876347],"tickmode":"array","ticktext":["-1000","0","1000","2000"],"tickvals":[-1000,0,1000,2000],"categoryorder":"array","categoryarray":["-1000","0","1000","2000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"3dfd46956f1f":{"x":{},"y":{},"alpha":{},"type":"scatter"},"3dfd7cb2806c":{"xintercept":{}},"3dfd2b0d7269":{"x":{},"y":{}},"3dfd7fd26193":{"x":{},"y":{}}},"cur_data":"3dfd46956f1f","visdat":{"3dfd46956f1f":["function (y) ","x"],"3dfd7cb2806c":["function (y) ","x"],"3dfd2b0d7269":["function (y) ","x"],"3dfd7fd26193":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
# dens <- density(npv_sim)
# fig <- plot_ly(x=~dens$x, y = ~dens$y, type='scatter', mode='lines', fill='tozeroy')
# fig <- fig %>% 
#   layout(xaxis=list(title='NPV'), yaxis=list(title='Distribution Density'), title=paste0("Distribution of NPV of ", policy_estimates_text[position]))
# fig
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
