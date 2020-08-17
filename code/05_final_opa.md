---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "16 August, 2020"
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
    alpha_0_so <- 0.77#0.77 #0.77
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

The monthly self-employed profits and self-employed hours for the control group, for those with positive hours, come from data [@baird2016worms] (Page 1168, Table 4, Panel C, Column 5, Row 1). The measure of hours in self employment used to compute wages is different from the one used to compute the weights above. The first one captures hours of work among those actively employed in the self-employed sector, and the second one captures the average hours of work in self-employed among all the population of working age in the sample (hence capturing the relative importance of the self employed sector in the economy).

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

For this (or similar?) setting @kremer2007illusion (Page 55, Table 7, Regression 1, Row 1 - FERNANDO please verify) estimate that there is almost no take-up without subsidy ($Q(0)$), hence  is assigned the value of 0. The same article (Page 48, Table 1, Panel C, Col 1, Row 3) estimates that take-up with full subsidy ($Q(full)$) was of 0.75.

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

In the original evaluation, the prevalence rates where very high (0.77), hence the effect on the infected population was similar to that of the overall population. Currently deworming interventions are discussed in geographies with much lower prevalence rates, hence to obtain the expected effect over the new region, we need to multiply the effect on the infected population by the prevalence rate in the new region ($\eta_{r}$).


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
                         staff_time_var = staff_time_so) {
      # create country weight
      c_weights <- country_total_var / sum(country_total_var)
      per_cap <- country_cost_var * (1 + staff_time_var) / country_total_var
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
   <td style="text-align:left;"> $\eta=0.77$ </td>
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
   <td style="text-align:left;"> $\eta=0.77$ </td>
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
# └──── costs1_p2_f
#        └──── costs1_p1_f
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
           unit_cost_local_new_var1 = 0.8296927,             #  CALL SO          
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

    ###------------ Inputs for earnings2_f--------------------------------------
    lambda1_new_in <- lambda_eff_f(lambda1_var = lambda1_new_var1,
                                 alpha_0_var = alpha_0_var1,
                                 alpha_r_var = alpha_r_var1)
    unit_test(lambda1_new_in, 1.8184154558571, main_run_var = main_run_var1)

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
    cost1_in <- costs1_p2_f(country_total_var = df_costs_var1$total,
                            country_cost_var = df_costs_var1$costs_by_country,
                              staff_time_var = staff_time_var1)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)
# s2_ea_in <-- cost1_in (costs1_p2_f) <-- cost_data (costs1_p1_f())
    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, 
                      ex_rate_var = 1)
    costs2_ea_in <- cost2_f(
      periods_var = periods_var1,
      delta_ed_var = delta_ed_final_in,
      interest_r_var = interest_in_new,
      cost_of_schooling_var = 0,
      s1_var = 0,
      q1_var = 0,
      s2_var = s2_ea_in,
      q2_var = q_full_var1
    )

    # costs2: Baird no externalities
    costs2_in <- cost2_f(
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
    costs2_in_x <- cost2_f(
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
    costs_k <- cost2_f(
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
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k, "cost1_in" = cost1_in) )
  }

invisible( list2env(one_run(),.GlobalEnv) )
```

```
## [1] "Output has change at lambda1_in[1]  to  2.26623376623377"
## [1] "Output has change at lambda1_new_in  to  2.36157851410014"
## [1] "Output has change at earnings_in_no_ext  to  40.4443288363982"
## [1] "Output has change at earnings_in_yes_ext  to  176.970013083277"
## [1] "Output has change at pv_benef_tax_nx_in  to  30.6585575816602"
## [1] "Output has change at pv_benef_tax_yx_in  to  134.150954965482"
## [1] "Output has change at pv_benef_all_nx_in  to  184.96867319252"
## [1] "Output has change at pv_benef_all_yx_in  to  809.357194361884"
## [1] "Output has change at pv_benef_tax_new  to  204.547698885025"
## [1] "Output has change at pv_benef_all_new  to  1234.0735980997"
```




```r
#TODO: update unit test values after updating interest rates to exact formula
#Baird 1: Costs = Baird w/tax and no externalities (no ext); 
#Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, 11.8309012188904)
```

```
## [1] "Output has change at baird1  to  18.8823694626722"
```

```r
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); 
#Benef = Baird yes ext
baird2 <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
unit_test(baird2, 101.903273665711)
```

```
## [1] "Output has change at baird2  to  108.954741909493"
```

```r
# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
baird3 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test(baird3, 130.649690239252)
```

```
## [1] "Output has change at baird3  to  173.192485073532"
```

```r
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
baird4 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
unit_test(baird4, 741.618186471615)
```

```
## [1] "Output has change at baird4  to  784.160981305894"
```

```r
#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1 <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs_k)
unit_test(klps4_1, 125.202113576337)
```

```
## [1] "Output has change at klps4_1  to  172.248084319893"
```

```r
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs_k)
unit_test(klps4_2, 917.937055971637)
```

```
## [1] "Output has change at klps4_2  to  1201.77398353457"
```

```r
# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
unit_test(ea1, 142.278620185973)
```

```
## [1] "Output has change at ea1  to  184.821415020253"
```

```r
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
unit_test(ea2, 766.667141355337)
```

```
## [1] "Output has change at ea2  to  809.209936189616"
```

```r
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
unit_test(ea3, 950.089412364501)
```

```
## [1] "Output has change at ea3  to  1233.92633992743"
```

```r
# CEA for EA
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_new, 
                          costs_var = costs2_ea_in, 
                          fudging_var = 0)
unit_test(cea_no_ext_ea, 6452.86204429499)
```

```
## [1] "Output has change at cea_no_ext_ea  to  8380.34031726622"
```

```r
rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_new, 
                                                costs_var = costs2_ea_in, 
                                                fudging_var = 0),
                             CEA_cash_var = 744)
unit_test(rcea_no_ext_ea, 8.6732016724395)
```

```
## [1] "Output has change at rcea_no_ext_ea  to  11.2638982758955"
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
   <td style="text-align:right;"> 18.9 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 173.2 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 172.2 </td>
   <td style="text-align:right;"> 1201.8 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> yes_ext </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 109 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 784.2 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr grouplength="1"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: EA</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext_ </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 184.8 </td>
   <td style="text-align:right;"> 809.2 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1233.9 </td>
  </tr>
</tbody>
</table>


## Results for overall welfare (not only taxes)


- **NPV without externalities in @baird2016worms ($\lambda_2 = 0$):** 173.2    

- **NPV with externalities in @baird2016worms ($\lambda_2 = 10.2$ ):** 784.2  

- **NPV without externalities in @klps4:** 1201.8   

- **NPV without externalities in EA 2019 ($\lambda_2 = 0$):** 184.8    

- **NPV with externalities in EA 2019 ($\lambda_2 = 10.2$ ):** 809.2

- **NPV without ext and benef from @klps4 and EA costs 2019 :** 1233.9

- **CEA format:** 8380.3    

- **RCEA format (relative to cash):** 11.3    


# Montecarlo simulations  


```r
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
                      costs_data_var2 = costs_data
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
    # unit_cost_local_new_sim
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
    alpha_0_sim <- rnorm(nsims, alpha_0_var2, alpha_0_var2_sd)
    alpha_0_sim <- ifelse(alpha_0_sim > 1, yes = 1, 
                          no = ifelse(alpha_0_sim < 0, 0, alpha_0_sim) )

    alpha_r_sim <- rnorm(nsims, alpha_r_var2, alpha_r_var2_sd)
    alpha_r_sim <- ifelse(alpha_r_sim > 1, yes = 1, 
                          no = ifelse(alpha_r_sim < 0, 0, alpha_r_sim) )
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

    # Get costs with no staff time
    # TO DO: remove this step out of the sim loop, should be called only once. 

            #           costs_par_var2,
            #           costs_par_var2_sd,
            #           staff_time_var2,
            #           staff_time_var2_sd,
            #           counts_par_var2,  
    
#      AQUI VOY
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
                df_costs_var1 = costs1_df_sim[[i]]),.GlobalEnv) ) # add costs here
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



### TEMP
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
npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 1))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 1))
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
           title = "Distribution of Economic Effects of Deworming (NPV)",
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
<script type="application/json" data-for="htmlwidget-43fbbdc7f92cf116cfd3">{"x":{"data":[{"x":[-1594.4687725938,-1586.65342965016,-1578.83808670653,-1571.0227437629,-1563.20740081927,-1555.39205787563,-1547.576714932,-1539.76137198837,-1531.94602904474,-1524.1306861011,-1516.31534315747,-1508.50000021384,-1500.68465727021,-1492.86931432657,-1485.05397138294,-1477.23862843931,-1469.42328549568,-1461.60794255204,-1453.79259960841,-1445.97725666478,-1438.16191372115,-1430.34657077751,-1422.53122783388,-1414.71588489025,-1406.90054194662,-1399.08519900298,-1391.26985605935,-1383.45451311572,-1375.63917017208,-1367.82382722845,-1360.00848428482,-1352.19314134119,-1344.37779839755,-1336.56245545392,-1328.74711251029,-1320.93176956666,-1313.11642662302,-1305.30108367939,-1297.48574073576,-1289.67039779213,-1281.85505484849,-1274.03971190486,-1266.22436896123,-1258.4090260176,-1250.59368307396,-1242.77834013033,-1234.9629971867,-1227.14765424307,-1219.33231129943,-1211.5169683558,-1203.70162541217,-1195.88628246854,-1188.0709395249,-1180.25559658127,-1172.44025363764,-1164.62491069401,-1156.80956775037,-1148.99422480674,-1141.17888186311,-1133.36353891948,-1125.54819597584,-1117.73285303221,-1109.91751008858,-1102.10216714495,-1094.28682420131,-1086.47148125768,-1078.65613831405,-1070.84079537042,-1063.02545242678,-1055.21010948315,-1047.39476653952,-1039.57942359589,-1031.76408065225,-1023.94873770862,-1016.13339476499,-1008.31805182136,-1000.50270887772,-992.687365934091,-984.872022990458,-977.056680046826,-969.241337103193,-961.425994159561,-953.610651215928,-945.795308272296,-937.979965328663,-930.16462238503,-922.349279441398,-914.533936497765,-906.718593554133,-898.9032506105,-891.087907666868,-883.272564723235,-875.457221779603,-867.64187883597,-859.826535892338,-852.011192948705,-844.195850005073,-836.38050706144,-828.565164117808,-820.749821174175,-812.934478230542,-805.11913528691,-797.303792343277,-789.488449399645,-781.673106456012,-773.85776351238,-766.042420568747,-758.227077625115,-750.411734681482,-742.59639173785,-734.781048794217,-726.965705850585,-719.150362906952,-711.335019963319,-703.519677019687,-695.704334076054,-687.888991132422,-680.073648188789,-672.258305245157,-664.442962301524,-656.627619357892,-648.812276414259,-640.996933470627,-633.181590526994,-625.366247583362,-617.550904639729,-609.735561696096,-601.920218752464,-594.104875808831,-586.289532865199,-578.474189921566,-570.658846977934,-562.843504034301,-555.028161090669,-547.212818147036,-539.397475203404,-531.582132259771,-523.766789316139,-515.951446372506,-508.136103428873,-500.320760485241,-492.505417541608,-484.690074597976,-476.874731654343,-469.059388710711,-461.244045767078,-453.428702823446,-445.613359879813,-437.798016936181,-429.982673992548,-422.167331048915,-414.351988105283,-406.536645161651,-398.721302218018,-390.905959274385,-383.090616330753,-375.27527338712,-367.459930443488,-359.644587499855,-351.829244556223,-344.01390161259,-336.198558668958,-328.383215725325,-320.567872781693,-312.75252983806,-304.937186894427,-297.121843950795,-289.306501007163,-281.49115806353,-273.675815119897,-265.860472176265,-258.045129232632,-250.229786289,-242.414443345367,-234.599100401735,-226.783757458102,-218.96841451447,-211.153071570837,-203.337728627205,-195.522385683572,-187.707042739939,-179.891699796307,-172.076356852674,-164.261013909042,-156.445670965409,-148.630328021777,-140.814985078144,-132.999642134512,-125.184299190879,-117.368956247247,-109.553613303614,-101.738270359981,-93.922927416349,-86.1075844727166,-78.2922415290839,-70.4768985854514,-62.6615556418189,-54.8462126981863,-47.0308697545538,-39.2155268109213,-31.4001838672887,-23.5848409236562,-15.7694979800237,-7.95415503639106,-0.138812092758599,7.67653085087386,15.4918737945065,23.307216738139,31.1225596817717,38.9379026254041,46.7532455690366,54.5685885126693,62.3839314563018,70.1992743999342,78.0146173435669,85.8299602871994,93.6453032308318,101.460646174465,109.275989118097,117.091332061729,124.906675005362,132.722017948995,140.537360892627,148.35270383626,156.168046779892,163.983389723525,171.798732667157,179.61407561079,187.429418554422,195.244761498055,203.060104441687,210.87544738532,218.690790328953,226.506133272585,234.321476216217,242.13681915985,249.952162103483,257.767505047115,265.582847990748,273.39819093438,281.213533878013,289.028876821645,296.844219765278,304.65956270891,312.474905652543,320.290248596175,328.105591539808,335.920934483441,343.736277427073,351.551620370706,359.366963314338,367.182306257971,374.997649201603,382.812992145236,390.628335088868,398.443678032501,406.259020976133,414.074363919766,421.889706863398,429.705049807031,437.520392750663,445.335735694296,453.151078637929,460.966421581561,468.781764525193,476.597107468826,484.412450412459,492.227793356091,500.043136299724,507.858479243356,515.673822186989,523.489165130622,531.304508074254,539.119851017887,546.935193961519,554.750536905151,562.565879848784,570.381222792417,578.196565736049,586.011908679682,593.827251623314,601.642594566947,609.457937510579,617.273280454212,625.088623397844,632.903966341477,640.719309285109,648.534652228742,656.349995172375,664.165338116007,671.98068105964,679.796024003272,687.611366946905,695.426709890537,703.24205283417,711.057395777802,718.872738721435,726.688081665067,734.5034246087,742.318767552332,750.134110495965,757.949453439598,765.76479638323,773.580139326863,781.395482270495,789.210825214127,797.02616815776,804.841511101393,812.656854045025,820.472196988658,828.28753993229,836.102882875923,843.918225819556,851.733568763188,859.54891170682,867.364254650453,875.179597594085,882.994940537718,890.810283481351,898.625626424983,906.440969368616,914.256312312248,922.071655255881,929.886998199514,937.702341143146,945.517684086778,953.333027030411,961.148369974043,968.963712917676,976.779055861309,984.594398804941,992.409741748574,1000.22508469221,1008.04042763584,1015.85577057947,1023.6711135231,1031.48645646674,1039.30179941037,1047.117142354,1054.93248529763,1062.74782824127,1070.5631711849,1078.37851412853,1086.19385707216,1094.0092000158,1101.82454295943,1109.63988590306,1117.45522884669,1125.27057179033,1133.08591473396,1140.90125767759,1148.71660062122,1156.53194356486,1164.34728650849,1172.16262945212,1179.97797239575,1187.79331533939,1195.60865828302,1203.42400122665,1211.23934417028,1219.05468711392,1226.87003005755,1234.68537300118,1242.50071594481,1250.31605888845,1258.13140183208,1265.94674477571,1273.76208771934,1281.57743066298,1289.39277360661,1297.20811655024,1305.02345949388,1312.83880243751,1320.65414538114,1328.46948832477,1336.28483126841,1344.10017421204,1351.91551715567,1359.7308600993,1367.54620304294,1375.36154598657,1383.1768889302,1390.99223187383,1398.80757481747,1406.6229177611,1414.43826070473,1422.25360364836,1430.068946592,1437.88428953563,1445.69963247926,1453.51497542289,1461.33031836653,1469.14566131016,1476.96100425379,1484.77634719742,1492.59169014106,1500.40703308469,1508.22237602832,1516.03771897195,1523.85306191559,1531.66840485922,1539.48374780285,1547.29909074648,1555.11443369012,1562.92977663375,1570.74511957738,1578.56046252101,1586.37580546465,1594.19114840828,1602.00649135191,1609.82183429554,1617.63717723918,1625.45252018281,1633.26786312644,1641.08320607007,1648.89854901371,1656.71389195734,1664.52923490097,1672.3445778446,1680.15992078824,1687.97526373187,1695.7906066755,1703.60594961913,1711.42129256277,1719.2366355064,1727.05197845003,1734.86732139366,1742.6826643373,1750.49800728093,1758.31335022456,1766.12869316819,1773.94403611183,1781.75937905546,1789.57472199909,1797.39006494273,1805.20540788636,1813.02075082999,1820.83609377362,1828.65143671725,1836.46677966089,1844.28212260452,1852.09746554815,1859.91280849179,1867.72815143542,1875.54349437905,1883.35883732268,1891.17418026632,1898.98952320995,1906.80486615358,1914.62020909721,1922.43555204085,1930.25089498448,1938.06623792811,1945.88158087174,1953.69692381538,1961.51226675901,1969.32760970264,1977.14295264627,1984.95829558991,1992.77363853354,2000.58898147717,2008.4043244208,2016.21966736444,2024.03501030807,2031.8503532517,2039.66569619533,2047.48103913897,2055.2963820826,2063.11172502623,2070.92706796986,2078.7424109135,2086.55775385713,2094.37309680076,2102.18843974439,2110.00378268803,2117.81912563166,2125.63446857529,2133.44981151892,2141.26515446256,2149.08049740619,2156.89584034982,2164.71118329345,2172.52652623709,2180.34186918072,2188.15721212435,2195.97255506798,2203.78789801162,2211.60324095525,2219.41858389888,2227.23392684251,2235.04926978615,2242.86461272978,2250.67995567341,2258.49529861704,2266.31064156068,2274.12598450431,2281.94132744794,2289.75667039157,2297.57201333521,2305.38735627884,2313.20269922247,2321.0180421661,2328.83338510974,2336.64872805337,2344.464070997,2352.27941394064,2360.09475688427,2367.9100998279,2375.72544277153,2383.54078571516,2391.3561286588,2399.17147160243,2399.17147160243,2399.17147160243,2391.3561286588,2383.54078571516,2375.72544277153,2367.9100998279,2360.09475688427,2352.27941394064,2344.464070997,2336.64872805337,2328.83338510974,2321.0180421661,2313.20269922247,2305.38735627884,2297.57201333521,2289.75667039157,2281.94132744794,2274.12598450431,2266.31064156068,2258.49529861704,2250.67995567341,2242.86461272978,2235.04926978615,2227.23392684251,2219.41858389888,2211.60324095525,2203.78789801162,2195.97255506798,2188.15721212435,2180.34186918072,2172.52652623709,2164.71118329345,2156.89584034982,2149.08049740619,2141.26515446256,2133.44981151892,2125.63446857529,2117.81912563166,2110.00378268803,2102.18843974439,2094.37309680076,2086.55775385713,2078.7424109135,2070.92706796986,2063.11172502623,2055.2963820826,2047.48103913897,2039.66569619533,2031.8503532517,2024.03501030807,2016.21966736444,2008.4043244208,2000.58898147717,1992.77363853354,1984.95829558991,1977.14295264627,1969.32760970264,1961.51226675901,1953.69692381538,1945.88158087174,1938.06623792811,1930.25089498448,1922.43555204085,1914.62020909721,1906.80486615358,1898.98952320995,1891.17418026632,1883.35883732268,1875.54349437905,1867.72815143542,1859.91280849179,1852.09746554815,1844.28212260452,1836.46677966089,1828.65143671725,1820.83609377362,1813.02075082999,1805.20540788636,1797.39006494273,1789.57472199909,1781.75937905546,1773.94403611183,1766.12869316819,1758.31335022456,1750.49800728093,1742.6826643373,1734.86732139366,1727.05197845003,1719.2366355064,1711.42129256277,1703.60594961913,1695.7906066755,1687.97526373187,1680.15992078824,1672.3445778446,1664.52923490097,1656.71389195734,1648.89854901371,1641.08320607007,1633.26786312644,1625.45252018281,1617.63717723918,1609.82183429554,1602.00649135191,1594.19114840828,1586.37580546465,1578.56046252101,1570.74511957738,1562.92977663375,1555.11443369012,1547.29909074648,1539.48374780285,1531.66840485922,1523.85306191559,1516.03771897195,1508.22237602832,1500.40703308469,1492.59169014106,1484.77634719742,1476.96100425379,1469.14566131016,1461.33031836653,1453.51497542289,1445.69963247926,1437.88428953563,1430.068946592,1422.25360364836,1414.43826070473,1406.6229177611,1398.80757481747,1390.99223187383,1383.1768889302,1375.36154598657,1367.54620304294,1359.7308600993,1351.91551715567,1344.10017421204,1336.28483126841,1328.46948832477,1320.65414538114,1312.83880243751,1305.02345949388,1297.20811655024,1289.39277360661,1281.57743066298,1273.76208771934,1265.94674477571,1258.13140183208,1250.31605888845,1242.50071594481,1234.68537300118,1226.87003005755,1219.05468711392,1211.23934417028,1203.42400122665,1195.60865828302,1187.79331533939,1179.97797239575,1172.16262945212,1164.34728650849,1156.53194356486,1148.71660062122,1140.90125767759,1133.08591473396,1125.27057179033,1117.45522884669,1109.63988590306,1101.82454295943,1094.0092000158,1086.19385707216,1078.37851412853,1070.5631711849,1062.74782824127,1054.93248529763,1047.117142354,1039.30179941037,1031.48645646674,1023.6711135231,1015.85577057947,1008.04042763584,1000.22508469221,992.409741748574,984.594398804941,976.779055861309,968.963712917676,961.148369974043,953.333027030411,945.517684086778,937.702341143146,929.886998199514,922.071655255881,914.256312312248,906.440969368616,898.625626424983,890.810283481351,882.994940537718,875.179597594085,867.364254650453,859.54891170682,851.733568763188,843.918225819556,836.102882875923,828.28753993229,820.472196988658,812.656854045025,804.841511101393,797.02616815776,789.210825214127,781.395482270495,773.580139326863,765.76479638323,757.949453439598,750.134110495965,742.318767552332,734.5034246087,726.688081665067,718.872738721435,711.057395777802,703.24205283417,695.426709890537,687.611366946905,679.796024003272,671.98068105964,664.165338116007,656.349995172375,648.534652228742,640.719309285109,632.903966341477,625.088623397844,617.273280454212,609.457937510579,601.642594566947,593.827251623314,586.011908679682,578.196565736049,570.381222792417,562.565879848784,554.750536905151,546.935193961519,539.119851017887,531.304508074254,523.489165130622,515.673822186989,507.858479243356,500.043136299724,492.227793356091,484.412450412459,476.597107468826,468.781764525193,460.966421581561,453.151078637929,445.335735694296,437.520392750663,429.705049807031,421.889706863398,414.074363919766,406.259020976133,398.443678032501,390.628335088868,382.812992145236,374.997649201603,367.182306257971,359.366963314338,351.551620370706,343.736277427073,335.920934483441,328.105591539808,320.290248596175,312.474905652543,304.65956270891,296.844219765278,289.028876821645,281.213533878013,273.39819093438,265.582847990748,257.767505047115,249.952162103483,242.13681915985,234.321476216217,226.506133272585,218.690790328953,210.87544738532,203.060104441687,195.244761498055,187.429418554422,179.61407561079,171.798732667157,163.983389723525,156.168046779892,148.35270383626,140.537360892627,132.722017948995,124.906675005362,117.091332061729,109.275989118097,101.460646174465,93.6453032308318,85.8299602871994,78.0146173435669,70.1992743999342,62.3839314563018,54.5685885126693,46.7532455690366,38.9379026254041,31.1225596817717,23.307216738139,15.4918737945065,7.67653085087386,-0.138812092758599,-7.95415503639106,-15.7694979800237,-23.5848409236562,-31.4001838672887,-39.2155268109213,-47.0308697545538,-54.8462126981863,-62.6615556418189,-70.4768985854514,-78.2922415290839,-86.1075844727166,-93.922927416349,-101.738270359981,-109.553613303614,-117.368956247247,-125.184299190879,-132.999642134512,-140.814985078144,-148.630328021777,-156.445670965409,-164.261013909042,-172.076356852674,-179.891699796307,-187.707042739939,-195.522385683572,-203.337728627205,-211.153071570837,-218.96841451447,-226.783757458102,-234.599100401735,-242.414443345367,-250.229786289,-258.045129232632,-265.860472176265,-273.675815119897,-281.49115806353,-289.306501007163,-297.121843950795,-304.937186894427,-312.75252983806,-320.567872781693,-328.383215725325,-336.198558668958,-344.01390161259,-351.829244556223,-359.644587499855,-367.459930443488,-375.27527338712,-383.090616330753,-390.905959274385,-398.721302218018,-406.536645161651,-414.351988105283,-422.167331048915,-429.982673992548,-437.798016936181,-445.613359879813,-453.428702823446,-461.244045767078,-469.059388710711,-476.874731654343,-484.690074597976,-492.505417541608,-500.320760485241,-508.136103428873,-515.951446372506,-523.766789316139,-531.582132259771,-539.397475203404,-547.212818147036,-555.028161090669,-562.843504034301,-570.658846977934,-578.474189921566,-586.289532865199,-594.104875808831,-601.920218752464,-609.735561696096,-617.550904639729,-625.366247583362,-633.181590526994,-640.996933470627,-648.812276414259,-656.627619357892,-664.442962301524,-672.258305245157,-680.073648188789,-687.888991132422,-695.704334076054,-703.519677019687,-711.335019963319,-719.150362906952,-726.965705850585,-734.781048794217,-742.59639173785,-750.411734681482,-758.227077625115,-766.042420568747,-773.85776351238,-781.673106456012,-789.488449399645,-797.303792343277,-805.11913528691,-812.934478230542,-820.749821174175,-828.565164117808,-836.38050706144,-844.195850005073,-852.011192948705,-859.826535892338,-867.64187883597,-875.457221779603,-883.272564723235,-891.087907666868,-898.9032506105,-906.718593554133,-914.533936497765,-922.349279441398,-930.16462238503,-937.979965328663,-945.795308272296,-953.610651215928,-961.425994159561,-969.241337103193,-977.056680046826,-984.872022990458,-992.687365934091,-1000.50270887772,-1008.31805182136,-1016.13339476499,-1023.94873770862,-1031.76408065225,-1039.57942359589,-1047.39476653952,-1055.21010948315,-1063.02545242678,-1070.84079537042,-1078.65613831405,-1086.47148125768,-1094.28682420131,-1102.10216714495,-1109.91751008858,-1117.73285303221,-1125.54819597584,-1133.36353891948,-1141.17888186311,-1148.99422480674,-1156.80956775037,-1164.62491069401,-1172.44025363764,-1180.25559658127,-1188.0709395249,-1195.88628246854,-1203.70162541217,-1211.5169683558,-1219.33231129943,-1227.14765424307,-1234.9629971867,-1242.77834013033,-1250.59368307396,-1258.4090260176,-1266.22436896123,-1274.03971190486,-1281.85505484849,-1289.67039779213,-1297.48574073576,-1305.30108367939,-1313.11642662302,-1320.93176956666,-1328.74711251029,-1336.56245545392,-1344.37779839755,-1352.19314134119,-1360.00848428482,-1367.82382722845,-1375.63917017208,-1383.45451311572,-1391.26985605935,-1399.08519900298,-1406.90054194662,-1414.71588489025,-1422.53122783388,-1430.34657077751,-1438.16191372115,-1445.97725666478,-1453.79259960841,-1461.60794255204,-1469.42328549568,-1477.23862843931,-1485.05397138294,-1492.86931432657,-1500.68465727021,-1508.50000021384,-1516.31534315747,-1524.1306861011,-1531.94602904474,-1539.76137198837,-1547.576714932,-1555.39205787563,-1563.20740081927,-1571.0227437629,-1578.83808670653,-1586.65342965016,-1594.4687725938,-1594.4687725938],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.532379680618882,0.536002330658266,0.539602478011145,0.543160125932045,0.546705440734843,0.55019670692175,0.55368625848913,0.55711001408388,0.560533769678631,0.563898369904488,0.567253738163564,0.570560479791871,0.573846836093878,0.577095431349467,0.580312401847889,0.583502692027416,0.58665015095302,0.589782105159437,0.592860170786092,0.595933884399892,0.598942913696973,0.601951942994053,0.604899188588007,0.607839770591856,0.6107301509898,0.613603098935944,0.616437291681802,0.619243633948573,0.622022423913929,0.624763393836838,0.627487669292975,0.630164693917494,0.632835442404799,0.6354501302531,0.638064818101401,0.640622562184807,0.64317669011936,0.645685093853117,0.648180593126862,0.650641054803512,0.653079996392464,0.655493979778943,0.65787855909856,0.660247587805715,0.662580108917074,0.664905760683355,0.667188620099102,0.669471479514849,0.671708191195616,0.673943861398729,0.676143022528355,0.678334035318996,0.680497393531366,0.682646323318411,0.684775640084384,0.686885087238613,0.688982131959265,0.69105470634469,0.69312125050007,0.695159555298426,0.697197366656122,0.69920398259716,0.701210598538199,0.703192289592317,0.705169759699245,0.707128710198183,0.709079525153423,0.711017391397334,0.712943975628705,0.714862374644398,0.716767073225958,0.718667578264511,0.720552644719597,0.722436780936903,0.724304366023533,0.726171951110162,0.728025747898353,0.729877889449041,0.731720122968125,0.733558736847461,0.735390634107269,0.73721750634137,0.739040224249637,0.74085700437183,0.742671627663293,0.7444798233103,0.746287362725593,0.748088335508018,0.749889308290444,0.751684688884319,0.753479651544624,0.755270804059209,0.757060818919528,0.758848373026067,0.760634351717404,0.762418859350826,0.764201563804377,0.765983499874584,0.76776354454526,0.769543307887252,0.771321163169077,0.773099018450902,0.774875074513643,0.776651071296571,0.77842572608904,0.780200061397057,0.781973366391609,0.783746110032767,0.785518054392304,0.787289156542703,0.78905967011588,0.790828969751632,0.792597926220494,0.794365160304445,0.79613238048097,0.797897193804463,0.799662007127955,0.801424404647477,0.80318636022184,0.804946010438981,0.806704600326681,0.808461126878423,0.810215783350031,0.811968782991326,0.813718889891684,0.815467936587894,0.817212840793046,0.818957489825199,0.820696512670924,0.822435535516649,0.824168753312325,0.825901201875406,0.827628396807915,0.829353574922619,0.831074278303522,0.832791497198176,0.834505248251624,0.836213836693587,0.837920186047977,0.839619499889748,0.841318012942545,0.843007443967168,0.844696874991791,0.846376688088815,0.8480556720591,0.849726323662013,0.851394347485323,0.853055523492672,0.854712131421102,0.856363549752548,0.858008348547194,0.8596497606882,0.861282423685738,0.862913616008762,0.864533886038733,0.866154156068703,0.867762372010435,0.869370063122351,0.870967626580166,0.872562624550715,0.87414950320202,0.875731764804145,0.877307962218202,0.878877513245653,0.880443067783122,0.881999999835994,0.883554983305849,0.885099449354649,0.88664391540345,0.888176174186825,0.889708382944568,0.891230565716958,0.892750774860617,0.894263084379429,0.895771592673649,0.897274248427148,0.898771386814342,0.900264621488378,0.901750743220099,0.903234798991478,0.904710268886198,0.906185393546231,0.907650576468215,0.9091157593902,0.910572268041715,0.912027516704548,0.913475918132379,0.914921560338716,0.916362056136291,0.917798381572562,0.919231148255998,0.920658394966942,0.922083579083109,0.923501919756188,0.924919632962667,0.926329161156435,0.927738689350205,0.929140191731627,0.930540908185088,0.931934932960658,0.933326732122369,0.934713137150025,0.936095794066295,0.93747436983623,0.938847527537084,0.940217992821373,0.941581150341061,0.942943147983498,0.944295649149249,0.945648150315,0.946989765119039,0.948330788237425,0.949661980688839,0.950990529253615,0.952310507670066,0.953625402208703,0.954933276754474,0.956233147922834,0.957527928547676,0.958811211573471,0.960091806231868,0.961356736797718,0.962621667363569,0.963866561598337,0.96511117324455,0.966337216127017,0.967559339242608,0.968764922407862,0.969962184866095,0.971145596052906,0.972315425534609,0.973474850009919,0.974614478044409,0.975748000370894,0.976854468110375,0.977960074257326,0.979030239986403,0.98010040571548,0.981136368153489,0.982166916521002,0.983167171052021,0.984154623997893,0.985116726822084,0.986057456157418,0.986978891002997,0.987869132587356,0.988747316130798,0.989583185576635,0.990415473160179,0.991192982980804,0.991970492801429,0.992691752729845,0.993406830834298,0.994072608868915,0.994721118294765,0.9953285790122,0.995906338775669,0.996452633080253,0.996955440012397,0.997437713182071,0.997861364609983,0.99827676449473,0.998617081618082,0.998957398741434,0.999215618791423,0.999468470597151,0.999650095367756,0.999811422919408,0.999913755190558,0.999979596123813,1,0.999966513052224,0.999902422712866,0.999765912112269,0.999614840510322,0.999371780049414,0.999128719588505,0.998778384170174,0.998425440788742,0.997980303836748,0.997514360541568,0.996972461055959,0.996390639007793,0.995750149989067,0.995049829592292,0.994309065213616,0.99348790604992,0.992645328574107,0.991701288029126,0.990755514464772,0.989686864905181,0.98861821534559,0.987442017763605,0.986247362132026,0.984964639403712,0.983642925109723,0.982253152242325,0.980803683239755,0.979306524008493,0.977728970962028,0.97612428113146,0.97441868958917,0.972706519736098,0.970873316238772,0.969040112741445,0.967093910245458,0.965133906318447,0.963082117007606,0.960996510996128,0.958840169237016,0.956630545961385,0.954370885593575,0.952039213816516,0.949677666700683,0.947226294414605,0.944764488551003,0.942196136108681,0.939627783666358,0.936953644453283,0.934271395580787,0.931504268410221,0.928711559155888,0.925853984123164,0.922954589810912,0.920009276338153,0.917007296607321,0.91397711755908,0.910876959586096,0.907764945038589,0.90457130504814,0.901377665057692,0.89809847890045,0.894816322085158,0.891467018199962,0.888101557177993,0.884685821035323,0.881242492984628,0.877764114894477,0.874248557510475,0.870711423672636,0.867129450103101,0.863537533481129,0.859895106397671,0.856252457422526,0.852555662392477,0.848858867362427,0.845121417827102,0.841376436152663,0.837602799037588,0.833815880230892,0.830010321463839,0.826187755527043,0.82235455198447,0.818502641663183,0.814646071253302,0.810771104429601,0.806895436209891,0.803003658597889,0.799111880985886,0.795210731236215,0.791308319538716,0.787402625687301,0.78349566136655,0.779589486364177,0.77568393017312,0.771781264512652,0.767882931925637,0.763987685082632,0.760102222756062,0.756218214842992,0.752351078227289,0.748483941611585,0.744638464081122,0.74079489629552,0.736973008575828,0.733158020647067,0.729362976511627,0.725581329066705,0.721816245032734,0.718072431524609,0.714340281284593,0.710638512818129,0.706942121994758,0.703286313808499,0.699630505622241,0.696022114683175,0.692415874330722,0.688851720279812,0.685298337542799,0.681780447497728,0.678282886050762,0.674813114812505,0.671374005648992,0.66795403389951,0.664575670516173,0.661207003362388,0.657891338689537,0.654575674016685,0.651323949607483,0.648072594661806,0.644875923688328,0.641690147915215,0.638549163904762,0.635429897167971,0.632345059305846,0.629292895473778,0.626264490431131,0.623279692501087,0.620307836554669,0.617390343249279,0.614474984690445,0.611624418454019,0.608773852217594,0.605981016601811,0.603196692918653,0.600458777469366,0.597739715421151,0.59505589709946,0.5924008310746,0.589770144121579,0.587177536586059,0.584598877322815,0.582066932559317,0.579539064372189,0.577065743092335,0.57459242181248,0.572170336333054,0.569753371066346,0.567376745893076,0.565013657055518,0.562680693018552,0.560368805563964,0.558077609417929,0.555814069975603,0.553562660646884,0.551344455050397,0.549130769951858,0.546954741065592,0.544778712179326,0.542639508227126,0.540502373979138,0.538393161258472,0.536291532790979,0.53420995407666,0.532140353928307,0.530084014467931,0.528042894705281,0.526009368678428,0.523993128897622,0.521979965838447,0.519984970961744,0.51798997608504,0.516012299937894,0.514034897733728,0.512068982966601,0.510105521225862,0.508148898351367,0.506195741656782,0.504245958104256,0.502299504133967,0.500354129914694,0.498410825225418,0.496467458485601,0.494523813724983,0.492580086184659,0.490632690686691,0.488685295188724,0.486731808683814,0.484777344405354,0.482815670252,0.480850925012537,0.478878945479389,0.476900825470253,0.474916488710106,0.472922030243199,0.470923354330944,0.468909734800921,0.466894811614392,0.464859359651918,0.462823907689445,0.460766563391704,0.45870676818897,0.45662725474788,0.454540774541046,0.452437603608236,0.450322273304806,0.448194051020776,0.446047888963115,0.443893318157351,0.441714532016689,0.439532414235331,0.43731940603358,0.43510639783183,0.43286001365398,0.430611383913614,0.42833416155276,0.426048712577781,0.423739964364588,0.421416702658358,0.4190758475788,0.416713985407126,0.414340549413493,0.41193950552393,0.409533121680557,0.407092521191375,0.404651920702194,0.402172603130241,0.399692276604951,0.397179632694004,0.394659614419671,0.392113799022932,0.389554324512853,0],"text":["npv_sim: -1594.4687726<br />scaled: 0.3895543<br />1/2: 0.5","npv_sim: -1586.6534297<br />scaled: 0.3921138<br />1/2: 0.5","npv_sim: -1578.8380867<br />scaled: 0.3946596<br />1/2: 0.5","npv_sim: -1571.0227438<br />scaled: 0.3971796<br />1/2: 0.5","npv_sim: -1563.2074008<br />scaled: 0.3996923<br />1/2: 0.5","npv_sim: -1555.3920579<br />scaled: 0.4021726<br />1/2: 0.5","npv_sim: -1547.5767149<br />scaled: 0.4046519<br />1/2: 0.5","npv_sim: -1539.7613720<br />scaled: 0.4070925<br />1/2: 0.5","npv_sim: -1531.9460290<br />scaled: 0.4095331<br />1/2: 0.5","npv_sim: -1524.1306861<br />scaled: 0.4119395<br />1/2: 0.5","npv_sim: -1516.3153432<br />scaled: 0.4143405<br />1/2: 0.5","npv_sim: -1508.5000002<br />scaled: 0.4167140<br />1/2: 0.5","npv_sim: -1500.6846573<br />scaled: 0.4190758<br />1/2: 0.5","npv_sim: -1492.8693143<br />scaled: 0.4214167<br />1/2: 0.5","npv_sim: -1485.0539714<br />scaled: 0.4237400<br />1/2: 0.5","npv_sim: -1477.2386284<br />scaled: 0.4260487<br />1/2: 0.5","npv_sim: -1469.4232855<br />scaled: 0.4283342<br />1/2: 0.5","npv_sim: -1461.6079426<br />scaled: 0.4306114<br />1/2: 0.5","npv_sim: -1453.7925996<br />scaled: 0.4328600<br />1/2: 0.5","npv_sim: -1445.9772567<br />scaled: 0.4351064<br />1/2: 0.5","npv_sim: -1438.1619137<br />scaled: 0.4373194<br />1/2: 0.5","npv_sim: -1430.3465708<br />scaled: 0.4395324<br />1/2: 0.5","npv_sim: -1422.5312278<br />scaled: 0.4417145<br />1/2: 0.5","npv_sim: -1414.7158849<br />scaled: 0.4438933<br />1/2: 0.5","npv_sim: -1406.9005419<br />scaled: 0.4460479<br />1/2: 0.5","npv_sim: -1399.0851990<br />scaled: 0.4481941<br />1/2: 0.5","npv_sim: -1391.2698561<br />scaled: 0.4503223<br />1/2: 0.5","npv_sim: -1383.4545131<br />scaled: 0.4524376<br />1/2: 0.5","npv_sim: -1375.6391702<br />scaled: 0.4545408<br />1/2: 0.5","npv_sim: -1367.8238272<br />scaled: 0.4566273<br />1/2: 0.5","npv_sim: -1360.0084843<br />scaled: 0.4587068<br />1/2: 0.5","npv_sim: -1352.1931413<br />scaled: 0.4607666<br />1/2: 0.5","npv_sim: -1344.3777984<br />scaled: 0.4628239<br />1/2: 0.5","npv_sim: -1336.5624555<br />scaled: 0.4648594<br />1/2: 0.5","npv_sim: -1328.7471125<br />scaled: 0.4668948<br />1/2: 0.5","npv_sim: -1320.9317696<br />scaled: 0.4689097<br />1/2: 0.5","npv_sim: -1313.1164266<br />scaled: 0.4709234<br />1/2: 0.5","npv_sim: -1305.3010837<br />scaled: 0.4729220<br />1/2: 0.5","npv_sim: -1297.4857407<br />scaled: 0.4749165<br />1/2: 0.5","npv_sim: -1289.6703978<br />scaled: 0.4769008<br />1/2: 0.5","npv_sim: -1281.8550548<br />scaled: 0.4788789<br />1/2: 0.5","npv_sim: -1274.0397119<br />scaled: 0.4808509<br />1/2: 0.5","npv_sim: -1266.2243690<br />scaled: 0.4828157<br />1/2: 0.5","npv_sim: -1258.4090260<br />scaled: 0.4847773<br />1/2: 0.5","npv_sim: -1250.5936831<br />scaled: 0.4867318<br />1/2: 0.5","npv_sim: -1242.7783401<br />scaled: 0.4886853<br />1/2: 0.5","npv_sim: -1234.9629972<br />scaled: 0.4906327<br />1/2: 0.5","npv_sim: -1227.1476542<br />scaled: 0.4925801<br />1/2: 0.5","npv_sim: -1219.3323113<br />scaled: 0.4945238<br />1/2: 0.5","npv_sim: -1211.5169684<br />scaled: 0.4964675<br />1/2: 0.5","npv_sim: -1203.7016254<br />scaled: 0.4984108<br />1/2: 0.5","npv_sim: -1195.8862825<br />scaled: 0.5003541<br />1/2: 0.5","npv_sim: -1188.0709395<br />scaled: 0.5022995<br />1/2: 0.5","npv_sim: -1180.2555966<br />scaled: 0.5042460<br />1/2: 0.5","npv_sim: -1172.4402536<br />scaled: 0.5061957<br />1/2: 0.5","npv_sim: -1164.6249107<br />scaled: 0.5081489<br />1/2: 0.5","npv_sim: -1156.8095678<br />scaled: 0.5101055<br />1/2: 0.5","npv_sim: -1148.9942248<br />scaled: 0.5120690<br />1/2: 0.5","npv_sim: -1141.1788819<br />scaled: 0.5140349<br />1/2: 0.5","npv_sim: -1133.3635389<br />scaled: 0.5160123<br />1/2: 0.5","npv_sim: -1125.5481960<br />scaled: 0.5179900<br />1/2: 0.5","npv_sim: -1117.7328530<br />scaled: 0.5199850<br />1/2: 0.5","npv_sim: -1109.9175101<br />scaled: 0.5219800<br />1/2: 0.5","npv_sim: -1102.1021671<br />scaled: 0.5239931<br />1/2: 0.5","npv_sim: -1094.2868242<br />scaled: 0.5260094<br />1/2: 0.5","npv_sim: -1086.4714813<br />scaled: 0.5280429<br />1/2: 0.5","npv_sim: -1078.6561383<br />scaled: 0.5300840<br />1/2: 0.5","npv_sim: -1070.8407954<br />scaled: 0.5321404<br />1/2: 0.5","npv_sim: -1063.0254524<br />scaled: 0.5342100<br />1/2: 0.5","npv_sim: -1055.2101095<br />scaled: 0.5362915<br />1/2: 0.5","npv_sim: -1047.3947665<br />scaled: 0.5383932<br />1/2: 0.5","npv_sim: -1039.5794236<br />scaled: 0.5405024<br />1/2: 0.5","npv_sim: -1031.7640807<br />scaled: 0.5426395<br />1/2: 0.5","npv_sim: -1023.9487377<br />scaled: 0.5447787<br />1/2: 0.5","npv_sim: -1016.1333948<br />scaled: 0.5469547<br />1/2: 0.5","npv_sim: -1008.3180518<br />scaled: 0.5491308<br />1/2: 0.5","npv_sim: -1000.5027089<br />scaled: 0.5513445<br />1/2: 0.5","npv_sim:  -992.6873659<br />scaled: 0.5535627<br />1/2: 0.5","npv_sim:  -984.8720230<br />scaled: 0.5558141<br />1/2: 0.5","npv_sim:  -977.0566800<br />scaled: 0.5580776<br />1/2: 0.5","npv_sim:  -969.2413371<br />scaled: 0.5603688<br />1/2: 0.5","npv_sim:  -961.4259942<br />scaled: 0.5626807<br />1/2: 0.5","npv_sim:  -953.6106512<br />scaled: 0.5650137<br />1/2: 0.5","npv_sim:  -945.7953083<br />scaled: 0.5673767<br />1/2: 0.5","npv_sim:  -937.9799653<br />scaled: 0.5697534<br />1/2: 0.5","npv_sim:  -930.1646224<br />scaled: 0.5721703<br />1/2: 0.5","npv_sim:  -922.3492794<br />scaled: 0.5745924<br />1/2: 0.5","npv_sim:  -914.5339365<br />scaled: 0.5770657<br />1/2: 0.5","npv_sim:  -906.7185936<br />scaled: 0.5795391<br />1/2: 0.5","npv_sim:  -898.9032506<br />scaled: 0.5820669<br />1/2: 0.5","npv_sim:  -891.0879077<br />scaled: 0.5845989<br />1/2: 0.5","npv_sim:  -883.2725647<br />scaled: 0.5871775<br />1/2: 0.5","npv_sim:  -875.4572218<br />scaled: 0.5897701<br />1/2: 0.5","npv_sim:  -867.6418788<br />scaled: 0.5924008<br />1/2: 0.5","npv_sim:  -859.8265359<br />scaled: 0.5950559<br />1/2: 0.5","npv_sim:  -852.0111929<br />scaled: 0.5977397<br />1/2: 0.5","npv_sim:  -844.1958500<br />scaled: 0.6004588<br />1/2: 0.5","npv_sim:  -836.3805071<br />scaled: 0.6031967<br />1/2: 0.5","npv_sim:  -828.5651641<br />scaled: 0.6059810<br />1/2: 0.5","npv_sim:  -820.7498212<br />scaled: 0.6087739<br />1/2: 0.5","npv_sim:  -812.9344782<br />scaled: 0.6116244<br />1/2: 0.5","npv_sim:  -805.1191353<br />scaled: 0.6144750<br />1/2: 0.5","npv_sim:  -797.3037923<br />scaled: 0.6173903<br />1/2: 0.5","npv_sim:  -789.4884494<br />scaled: 0.6203078<br />1/2: 0.5","npv_sim:  -781.6731065<br />scaled: 0.6232797<br />1/2: 0.5","npv_sim:  -773.8577635<br />scaled: 0.6262645<br />1/2: 0.5","npv_sim:  -766.0424206<br />scaled: 0.6292929<br />1/2: 0.5","npv_sim:  -758.2270776<br />scaled: 0.6323451<br />1/2: 0.5","npv_sim:  -750.4117347<br />scaled: 0.6354299<br />1/2: 0.5","npv_sim:  -742.5963917<br />scaled: 0.6385492<br />1/2: 0.5","npv_sim:  -734.7810488<br />scaled: 0.6416901<br />1/2: 0.5","npv_sim:  -726.9657059<br />scaled: 0.6448759<br />1/2: 0.5","npv_sim:  -719.1503629<br />scaled: 0.6480726<br />1/2: 0.5","npv_sim:  -711.3350200<br />scaled: 0.6513239<br />1/2: 0.5","npv_sim:  -703.5196770<br />scaled: 0.6545757<br />1/2: 0.5","npv_sim:  -695.7043341<br />scaled: 0.6578913<br />1/2: 0.5","npv_sim:  -687.8889911<br />scaled: 0.6612070<br />1/2: 0.5","npv_sim:  -680.0736482<br />scaled: 0.6645757<br />1/2: 0.5","npv_sim:  -672.2583052<br />scaled: 0.6679540<br />1/2: 0.5","npv_sim:  -664.4429623<br />scaled: 0.6713740<br />1/2: 0.5","npv_sim:  -656.6276194<br />scaled: 0.6748131<br />1/2: 0.5","npv_sim:  -648.8122764<br />scaled: 0.6782829<br />1/2: 0.5","npv_sim:  -640.9969335<br />scaled: 0.6817804<br />1/2: 0.5","npv_sim:  -633.1815905<br />scaled: 0.6852983<br />1/2: 0.5","npv_sim:  -625.3662476<br />scaled: 0.6888517<br />1/2: 0.5","npv_sim:  -617.5509046<br />scaled: 0.6924159<br />1/2: 0.5","npv_sim:  -609.7355617<br />scaled: 0.6960221<br />1/2: 0.5","npv_sim:  -601.9202188<br />scaled: 0.6996305<br />1/2: 0.5","npv_sim:  -594.1048758<br />scaled: 0.7032863<br />1/2: 0.5","npv_sim:  -586.2895329<br />scaled: 0.7069421<br />1/2: 0.5","npv_sim:  -578.4741899<br />scaled: 0.7106385<br />1/2: 0.5","npv_sim:  -570.6588470<br />scaled: 0.7143403<br />1/2: 0.5","npv_sim:  -562.8435040<br />scaled: 0.7180724<br />1/2: 0.5","npv_sim:  -555.0281611<br />scaled: 0.7218162<br />1/2: 0.5","npv_sim:  -547.2128181<br />scaled: 0.7255813<br />1/2: 0.5","npv_sim:  -539.3974752<br />scaled: 0.7293630<br />1/2: 0.5","npv_sim:  -531.5821323<br />scaled: 0.7331580<br />1/2: 0.5","npv_sim:  -523.7667893<br />scaled: 0.7369730<br />1/2: 0.5","npv_sim:  -515.9514464<br />scaled: 0.7407949<br />1/2: 0.5","npv_sim:  -508.1361034<br />scaled: 0.7446385<br />1/2: 0.5","npv_sim:  -500.3207605<br />scaled: 0.7484839<br />1/2: 0.5","npv_sim:  -492.5054175<br />scaled: 0.7523511<br />1/2: 0.5","npv_sim:  -484.6900746<br />scaled: 0.7562182<br />1/2: 0.5","npv_sim:  -476.8747317<br />scaled: 0.7601022<br />1/2: 0.5","npv_sim:  -469.0593887<br />scaled: 0.7639877<br />1/2: 0.5","npv_sim:  -461.2440458<br />scaled: 0.7678829<br />1/2: 0.5","npv_sim:  -453.4287028<br />scaled: 0.7717813<br />1/2: 0.5","npv_sim:  -445.6133599<br />scaled: 0.7756839<br />1/2: 0.5","npv_sim:  -437.7980169<br />scaled: 0.7795895<br />1/2: 0.5","npv_sim:  -429.9826740<br />scaled: 0.7834957<br />1/2: 0.5","npv_sim:  -422.1673310<br />scaled: 0.7874026<br />1/2: 0.5","npv_sim:  -414.3519881<br />scaled: 0.7913083<br />1/2: 0.5","npv_sim:  -406.5366452<br />scaled: 0.7952107<br />1/2: 0.5","npv_sim:  -398.7213022<br />scaled: 0.7991119<br />1/2: 0.5","npv_sim:  -390.9059593<br />scaled: 0.8030037<br />1/2: 0.5","npv_sim:  -383.0906163<br />scaled: 0.8068954<br />1/2: 0.5","npv_sim:  -375.2752734<br />scaled: 0.8107711<br />1/2: 0.5","npv_sim:  -367.4599304<br />scaled: 0.8146461<br />1/2: 0.5","npv_sim:  -359.6445875<br />scaled: 0.8185026<br />1/2: 0.5","npv_sim:  -351.8292446<br />scaled: 0.8223546<br />1/2: 0.5","npv_sim:  -344.0139016<br />scaled: 0.8261878<br />1/2: 0.5","npv_sim:  -336.1985587<br />scaled: 0.8300103<br />1/2: 0.5","npv_sim:  -328.3832157<br />scaled: 0.8338159<br />1/2: 0.5","npv_sim:  -320.5678728<br />scaled: 0.8376028<br />1/2: 0.5","npv_sim:  -312.7525298<br />scaled: 0.8413764<br />1/2: 0.5","npv_sim:  -304.9371869<br />scaled: 0.8451214<br />1/2: 0.5","npv_sim:  -297.1218440<br />scaled: 0.8488589<br />1/2: 0.5","npv_sim:  -289.3065010<br />scaled: 0.8525557<br />1/2: 0.5","npv_sim:  -281.4911581<br />scaled: 0.8562525<br />1/2: 0.5","npv_sim:  -273.6758151<br />scaled: 0.8598951<br />1/2: 0.5","npv_sim:  -265.8604722<br />scaled: 0.8635375<br />1/2: 0.5","npv_sim:  -258.0451292<br />scaled: 0.8671295<br />1/2: 0.5","npv_sim:  -250.2297863<br />scaled: 0.8707114<br />1/2: 0.5","npv_sim:  -242.4144433<br />scaled: 0.8742486<br />1/2: 0.5","npv_sim:  -234.5991004<br />scaled: 0.8777641<br />1/2: 0.5","npv_sim:  -226.7837575<br />scaled: 0.8812425<br />1/2: 0.5","npv_sim:  -218.9684145<br />scaled: 0.8846858<br />1/2: 0.5","npv_sim:  -211.1530716<br />scaled: 0.8881016<br />1/2: 0.5","npv_sim:  -203.3377286<br />scaled: 0.8914670<br />1/2: 0.5","npv_sim:  -195.5223857<br />scaled: 0.8948163<br />1/2: 0.5","npv_sim:  -187.7070427<br />scaled: 0.8980985<br />1/2: 0.5","npv_sim:  -179.8916998<br />scaled: 0.9013777<br />1/2: 0.5","npv_sim:  -172.0763569<br />scaled: 0.9045713<br />1/2: 0.5","npv_sim:  -164.2610139<br />scaled: 0.9077649<br />1/2: 0.5","npv_sim:  -156.4456710<br />scaled: 0.9108770<br />1/2: 0.5","npv_sim:  -148.6303280<br />scaled: 0.9139771<br />1/2: 0.5","npv_sim:  -140.8149851<br />scaled: 0.9170073<br />1/2: 0.5","npv_sim:  -132.9996421<br />scaled: 0.9200093<br />1/2: 0.5","npv_sim:  -125.1842992<br />scaled: 0.9229546<br />1/2: 0.5","npv_sim:  -117.3689562<br />scaled: 0.9258540<br />1/2: 0.5","npv_sim:  -109.5536133<br />scaled: 0.9287116<br />1/2: 0.5","npv_sim:  -101.7382704<br />scaled: 0.9315043<br />1/2: 0.5","npv_sim:   -93.9229274<br />scaled: 0.9342714<br />1/2: 0.5","npv_sim:   -86.1075845<br />scaled: 0.9369536<br />1/2: 0.5","npv_sim:   -78.2922415<br />scaled: 0.9396278<br />1/2: 0.5","npv_sim:   -70.4768986<br />scaled: 0.9421961<br />1/2: 0.5","npv_sim:   -62.6615556<br />scaled: 0.9447645<br />1/2: 0.5","npv_sim:   -54.8462127<br />scaled: 0.9472263<br />1/2: 0.5","npv_sim:   -47.0308698<br />scaled: 0.9496777<br />1/2: 0.5","npv_sim:   -39.2155268<br />scaled: 0.9520392<br />1/2: 0.5","npv_sim:   -31.4001839<br />scaled: 0.9543709<br />1/2: 0.5","npv_sim:   -23.5848409<br />scaled: 0.9566305<br />1/2: 0.5","npv_sim:   -15.7694980<br />scaled: 0.9588402<br />1/2: 0.5","npv_sim:    -7.9541550<br />scaled: 0.9609965<br />1/2: 0.5","npv_sim:    -0.1388121<br />scaled: 0.9630821<br />1/2: 0.5","npv_sim:     7.6765309<br />scaled: 0.9651339<br />1/2: 0.5","npv_sim:    15.4918738<br />scaled: 0.9670939<br />1/2: 0.5","npv_sim:    23.3072167<br />scaled: 0.9690401<br />1/2: 0.5","npv_sim:    31.1225597<br />scaled: 0.9708733<br />1/2: 0.5","npv_sim:    38.9379026<br />scaled: 0.9727065<br />1/2: 0.5","npv_sim:    46.7532456<br />scaled: 0.9744187<br />1/2: 0.5","npv_sim:    54.5685885<br />scaled: 0.9761243<br />1/2: 0.5","npv_sim:    62.3839315<br />scaled: 0.9777290<br />1/2: 0.5","npv_sim:    70.1992744<br />scaled: 0.9793065<br />1/2: 0.5","npv_sim:    78.0146173<br />scaled: 0.9808037<br />1/2: 0.5","npv_sim:    85.8299603<br />scaled: 0.9822532<br />1/2: 0.5","npv_sim:    93.6453032<br />scaled: 0.9836429<br />1/2: 0.5","npv_sim:   101.4606462<br />scaled: 0.9849646<br />1/2: 0.5","npv_sim:   109.2759891<br />scaled: 0.9862474<br />1/2: 0.5","npv_sim:   117.0913321<br />scaled: 0.9874420<br />1/2: 0.5","npv_sim:   124.9066750<br />scaled: 0.9886182<br />1/2: 0.5","npv_sim:   132.7220179<br />scaled: 0.9896869<br />1/2: 0.5","npv_sim:   140.5373609<br />scaled: 0.9907555<br />1/2: 0.5","npv_sim:   148.3527038<br />scaled: 0.9917013<br />1/2: 0.5","npv_sim:   156.1680468<br />scaled: 0.9926453<br />1/2: 0.5","npv_sim:   163.9833897<br />scaled: 0.9934879<br />1/2: 0.5","npv_sim:   171.7987327<br />scaled: 0.9943091<br />1/2: 0.5","npv_sim:   179.6140756<br />scaled: 0.9950498<br />1/2: 0.5","npv_sim:   187.4294186<br />scaled: 0.9957501<br />1/2: 0.5","npv_sim:   195.2447615<br />scaled: 0.9963906<br />1/2: 0.5","npv_sim:   203.0601044<br />scaled: 0.9969725<br />1/2: 0.5","npv_sim:   210.8754474<br />scaled: 0.9975144<br />1/2: 0.5","npv_sim:   218.6907903<br />scaled: 0.9979803<br />1/2: 0.5","npv_sim:   226.5061333<br />scaled: 0.9984254<br />1/2: 0.5","npv_sim:   234.3214762<br />scaled: 0.9987784<br />1/2: 0.5","npv_sim:   242.1368192<br />scaled: 0.9991287<br />1/2: 0.5","npv_sim:   249.9521621<br />scaled: 0.9993718<br />1/2: 0.5","npv_sim:   257.7675050<br />scaled: 0.9996148<br />1/2: 0.5","npv_sim:   265.5828480<br />scaled: 0.9997659<br />1/2: 0.5","npv_sim:   273.3981909<br />scaled: 0.9999024<br />1/2: 0.5","npv_sim:   281.2135339<br />scaled: 0.9999665<br />1/2: 0.5","npv_sim:   289.0288768<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   296.8442198<br />scaled: 0.9999796<br />1/2: 0.5","npv_sim:   304.6595627<br />scaled: 0.9999138<br />1/2: 0.5","npv_sim:   312.4749057<br />scaled: 0.9998114<br />1/2: 0.5","npv_sim:   320.2902486<br />scaled: 0.9996501<br />1/2: 0.5","npv_sim:   328.1055915<br />scaled: 0.9994685<br />1/2: 0.5","npv_sim:   335.9209345<br />scaled: 0.9992156<br />1/2: 0.5","npv_sim:   343.7362774<br />scaled: 0.9989574<br />1/2: 0.5","npv_sim:   351.5516204<br />scaled: 0.9986171<br />1/2: 0.5","npv_sim:   359.3669633<br />scaled: 0.9982768<br />1/2: 0.5","npv_sim:   367.1823063<br />scaled: 0.9978614<br />1/2: 0.5","npv_sim:   374.9976492<br />scaled: 0.9974377<br />1/2: 0.5","npv_sim:   382.8129921<br />scaled: 0.9969554<br />1/2: 0.5","npv_sim:   390.6283351<br />scaled: 0.9964526<br />1/2: 0.5","npv_sim:   398.4436780<br />scaled: 0.9959063<br />1/2: 0.5","npv_sim:   406.2590210<br />scaled: 0.9953286<br />1/2: 0.5","npv_sim:   414.0743639<br />scaled: 0.9947211<br />1/2: 0.5","npv_sim:   421.8897069<br />scaled: 0.9940726<br />1/2: 0.5","npv_sim:   429.7050498<br />scaled: 0.9934068<br />1/2: 0.5","npv_sim:   437.5203928<br />scaled: 0.9926918<br />1/2: 0.5","npv_sim:   445.3357357<br />scaled: 0.9919705<br />1/2: 0.5","npv_sim:   453.1510786<br />scaled: 0.9911930<br />1/2: 0.5","npv_sim:   460.9664216<br />scaled: 0.9904155<br />1/2: 0.5","npv_sim:   468.7817645<br />scaled: 0.9895832<br />1/2: 0.5","npv_sim:   476.5971075<br />scaled: 0.9887473<br />1/2: 0.5","npv_sim:   484.4124504<br />scaled: 0.9878691<br />1/2: 0.5","npv_sim:   492.2277934<br />scaled: 0.9869789<br />1/2: 0.5","npv_sim:   500.0431363<br />scaled: 0.9860575<br />1/2: 0.5","npv_sim:   507.8584792<br />scaled: 0.9851167<br />1/2: 0.5","npv_sim:   515.6738222<br />scaled: 0.9841546<br />1/2: 0.5","npv_sim:   523.4891651<br />scaled: 0.9831672<br />1/2: 0.5","npv_sim:   531.3045081<br />scaled: 0.9821669<br />1/2: 0.5","npv_sim:   539.1198510<br />scaled: 0.9811364<br />1/2: 0.5","npv_sim:   546.9351940<br />scaled: 0.9801004<br />1/2: 0.5","npv_sim:   554.7505369<br />scaled: 0.9790302<br />1/2: 0.5","npv_sim:   562.5658798<br />scaled: 0.9779601<br />1/2: 0.5","npv_sim:   570.3812228<br />scaled: 0.9768545<br />1/2: 0.5","npv_sim:   578.1965657<br />scaled: 0.9757480<br />1/2: 0.5","npv_sim:   586.0119087<br />scaled: 0.9746145<br />1/2: 0.5","npv_sim:   593.8272516<br />scaled: 0.9734749<br />1/2: 0.5","npv_sim:   601.6425946<br />scaled: 0.9723154<br />1/2: 0.5","npv_sim:   609.4579375<br />scaled: 0.9711456<br />1/2: 0.5","npv_sim:   617.2732805<br />scaled: 0.9699622<br />1/2: 0.5","npv_sim:   625.0886234<br />scaled: 0.9687649<br />1/2: 0.5","npv_sim:   632.9039663<br />scaled: 0.9675593<br />1/2: 0.5","npv_sim:   640.7193093<br />scaled: 0.9663372<br />1/2: 0.5","npv_sim:   648.5346522<br />scaled: 0.9651112<br />1/2: 0.5","npv_sim:   656.3499952<br />scaled: 0.9638666<br />1/2: 0.5","npv_sim:   664.1653381<br />scaled: 0.9626217<br />1/2: 0.5","npv_sim:   671.9806811<br />scaled: 0.9613567<br />1/2: 0.5","npv_sim:   679.7960240<br />scaled: 0.9600918<br />1/2: 0.5","npv_sim:   687.6113669<br />scaled: 0.9588112<br />1/2: 0.5","npv_sim:   695.4267099<br />scaled: 0.9575279<br />1/2: 0.5","npv_sim:   703.2420528<br />scaled: 0.9562331<br />1/2: 0.5","npv_sim:   711.0573958<br />scaled: 0.9549333<br />1/2: 0.5","npv_sim:   718.8727387<br />scaled: 0.9536254<br />1/2: 0.5","npv_sim:   726.6880817<br />scaled: 0.9523105<br />1/2: 0.5","npv_sim:   734.5034246<br />scaled: 0.9509905<br />1/2: 0.5","npv_sim:   742.3187676<br />scaled: 0.9496620<br />1/2: 0.5","npv_sim:   750.1341105<br />scaled: 0.9483308<br />1/2: 0.5","npv_sim:   757.9494534<br />scaled: 0.9469898<br />1/2: 0.5","npv_sim:   765.7647964<br />scaled: 0.9456482<br />1/2: 0.5","npv_sim:   773.5801393<br />scaled: 0.9442956<br />1/2: 0.5","npv_sim:   781.3954823<br />scaled: 0.9429431<br />1/2: 0.5","npv_sim:   789.2108252<br />scaled: 0.9415812<br />1/2: 0.5","npv_sim:   797.0261682<br />scaled: 0.9402180<br />1/2: 0.5","npv_sim:   804.8415111<br />scaled: 0.9388475<br />1/2: 0.5","npv_sim:   812.6568540<br />scaled: 0.9374744<br />1/2: 0.5","npv_sim:   820.4721970<br />scaled: 0.9360958<br />1/2: 0.5","npv_sim:   828.2875399<br />scaled: 0.9347131<br />1/2: 0.5","npv_sim:   836.1028829<br />scaled: 0.9333267<br />1/2: 0.5","npv_sim:   843.9182258<br />scaled: 0.9319349<br />1/2: 0.5","npv_sim:   851.7335688<br />scaled: 0.9305409<br />1/2: 0.5","npv_sim:   859.5489117<br />scaled: 0.9291402<br />1/2: 0.5","npv_sim:   867.3642547<br />scaled: 0.9277387<br />1/2: 0.5","npv_sim:   875.1795976<br />scaled: 0.9263292<br />1/2: 0.5","npv_sim:   882.9949405<br />scaled: 0.9249196<br />1/2: 0.5","npv_sim:   890.8102835<br />scaled: 0.9235019<br />1/2: 0.5","npv_sim:   898.6256264<br />scaled: 0.9220836<br />1/2: 0.5","npv_sim:   906.4409694<br />scaled: 0.9206584<br />1/2: 0.5","npv_sim:   914.2563123<br />scaled: 0.9192311<br />1/2: 0.5","npv_sim:   922.0716553<br />scaled: 0.9177984<br />1/2: 0.5","npv_sim:   929.8869982<br />scaled: 0.9163621<br />1/2: 0.5","npv_sim:   937.7023411<br />scaled: 0.9149216<br />1/2: 0.5","npv_sim:   945.5176841<br />scaled: 0.9134759<br />1/2: 0.5","npv_sim:   953.3330270<br />scaled: 0.9120275<br />1/2: 0.5","npv_sim:   961.1483700<br />scaled: 0.9105723<br />1/2: 0.5","npv_sim:   968.9637129<br />scaled: 0.9091158<br />1/2: 0.5","npv_sim:   976.7790559<br />scaled: 0.9076506<br />1/2: 0.5","npv_sim:   984.5943988<br />scaled: 0.9061854<br />1/2: 0.5","npv_sim:   992.4097417<br />scaled: 0.9047103<br />1/2: 0.5","npv_sim:  1000.2250847<br />scaled: 0.9032348<br />1/2: 0.5","npv_sim:  1008.0404276<br />scaled: 0.9017507<br />1/2: 0.5","npv_sim:  1015.8557706<br />scaled: 0.9002646<br />1/2: 0.5","npv_sim:  1023.6711135<br />scaled: 0.8987714<br />1/2: 0.5","npv_sim:  1031.4864565<br />scaled: 0.8972742<br />1/2: 0.5","npv_sim:  1039.3017994<br />scaled: 0.8957716<br />1/2: 0.5","npv_sim:  1047.1171424<br />scaled: 0.8942631<br />1/2: 0.5","npv_sim:  1054.9324853<br />scaled: 0.8927508<br />1/2: 0.5","npv_sim:  1062.7478282<br />scaled: 0.8912306<br />1/2: 0.5","npv_sim:  1070.5631712<br />scaled: 0.8897084<br />1/2: 0.5","npv_sim:  1078.3785141<br />scaled: 0.8881762<br />1/2: 0.5","npv_sim:  1086.1938571<br />scaled: 0.8866439<br />1/2: 0.5","npv_sim:  1094.0092000<br />scaled: 0.8850994<br />1/2: 0.5","npv_sim:  1101.8245430<br />scaled: 0.8835550<br />1/2: 0.5","npv_sim:  1109.6398859<br />scaled: 0.8820000<br />1/2: 0.5","npv_sim:  1117.4552288<br />scaled: 0.8804431<br />1/2: 0.5","npv_sim:  1125.2705718<br />scaled: 0.8788775<br />1/2: 0.5","npv_sim:  1133.0859147<br />scaled: 0.8773080<br />1/2: 0.5","npv_sim:  1140.9012577<br />scaled: 0.8757318<br />1/2: 0.5","npv_sim:  1148.7166006<br />scaled: 0.8741495<br />1/2: 0.5","npv_sim:  1156.5319436<br />scaled: 0.8725626<br />1/2: 0.5","npv_sim:  1164.3472865<br />scaled: 0.8709676<br />1/2: 0.5","npv_sim:  1172.1626295<br />scaled: 0.8693701<br />1/2: 0.5","npv_sim:  1179.9779724<br />scaled: 0.8677624<br />1/2: 0.5","npv_sim:  1187.7933153<br />scaled: 0.8661542<br />1/2: 0.5","npv_sim:  1195.6086583<br />scaled: 0.8645339<br />1/2: 0.5","npv_sim:  1203.4240012<br />scaled: 0.8629136<br />1/2: 0.5","npv_sim:  1211.2393442<br />scaled: 0.8612824<br />1/2: 0.5","npv_sim:  1219.0546871<br />scaled: 0.8596498<br />1/2: 0.5","npv_sim:  1226.8700301<br />scaled: 0.8580083<br />1/2: 0.5","npv_sim:  1234.6853730<br />scaled: 0.8563635<br />1/2: 0.5","npv_sim:  1242.5007159<br />scaled: 0.8547121<br />1/2: 0.5","npv_sim:  1250.3160589<br />scaled: 0.8530555<br />1/2: 0.5","npv_sim:  1258.1314018<br />scaled: 0.8513943<br />1/2: 0.5","npv_sim:  1265.9467448<br />scaled: 0.8497263<br />1/2: 0.5","npv_sim:  1273.7620877<br />scaled: 0.8480557<br />1/2: 0.5","npv_sim:  1281.5774307<br />scaled: 0.8463767<br />1/2: 0.5","npv_sim:  1289.3927736<br />scaled: 0.8446969<br />1/2: 0.5","npv_sim:  1297.2081166<br />scaled: 0.8430074<br />1/2: 0.5","npv_sim:  1305.0234595<br />scaled: 0.8413180<br />1/2: 0.5","npv_sim:  1312.8388024<br />scaled: 0.8396195<br />1/2: 0.5","npv_sim:  1320.6541454<br />scaled: 0.8379202<br />1/2: 0.5","npv_sim:  1328.4694883<br />scaled: 0.8362138<br />1/2: 0.5","npv_sim:  1336.2848313<br />scaled: 0.8345052<br />1/2: 0.5","npv_sim:  1344.1001742<br />scaled: 0.8327915<br />1/2: 0.5","npv_sim:  1351.9155172<br />scaled: 0.8310743<br />1/2: 0.5","npv_sim:  1359.7308601<br />scaled: 0.8293536<br />1/2: 0.5","npv_sim:  1367.5462030<br />scaled: 0.8276284<br />1/2: 0.5","npv_sim:  1375.3615460<br />scaled: 0.8259012<br />1/2: 0.5","npv_sim:  1383.1768889<br />scaled: 0.8241688<br />1/2: 0.5","npv_sim:  1390.9922319<br />scaled: 0.8224355<br />1/2: 0.5","npv_sim:  1398.8075748<br />scaled: 0.8206965<br />1/2: 0.5","npv_sim:  1406.6229178<br />scaled: 0.8189575<br />1/2: 0.5","npv_sim:  1414.4382607<br />scaled: 0.8172128<br />1/2: 0.5","npv_sim:  1422.2536036<br />scaled: 0.8154679<br />1/2: 0.5","npv_sim:  1430.0689466<br />scaled: 0.8137189<br />1/2: 0.5","npv_sim:  1437.8842895<br />scaled: 0.8119688<br />1/2: 0.5","npv_sim:  1445.6996325<br />scaled: 0.8102158<br />1/2: 0.5","npv_sim:  1453.5149754<br />scaled: 0.8084611<br />1/2: 0.5","npv_sim:  1461.3303184<br />scaled: 0.8067046<br />1/2: 0.5","npv_sim:  1469.1456613<br />scaled: 0.8049460<br />1/2: 0.5","npv_sim:  1476.9610043<br />scaled: 0.8031864<br />1/2: 0.5","npv_sim:  1484.7763472<br />scaled: 0.8014244<br />1/2: 0.5","npv_sim:  1492.5916901<br />scaled: 0.7996620<br />1/2: 0.5","npv_sim:  1500.4070331<br />scaled: 0.7978972<br />1/2: 0.5","npv_sim:  1508.2223760<br />scaled: 0.7961324<br />1/2: 0.5","npv_sim:  1516.0377190<br />scaled: 0.7943652<br />1/2: 0.5","npv_sim:  1523.8530619<br />scaled: 0.7925979<br />1/2: 0.5","npv_sim:  1531.6684049<br />scaled: 0.7908290<br />1/2: 0.5","npv_sim:  1539.4837478<br />scaled: 0.7890597<br />1/2: 0.5","npv_sim:  1547.2990907<br />scaled: 0.7872892<br />1/2: 0.5","npv_sim:  1555.1144337<br />scaled: 0.7855181<br />1/2: 0.5","npv_sim:  1562.9297766<br />scaled: 0.7837461<br />1/2: 0.5","npv_sim:  1570.7451196<br />scaled: 0.7819734<br />1/2: 0.5","npv_sim:  1578.5604625<br />scaled: 0.7802001<br />1/2: 0.5","npv_sim:  1586.3758055<br />scaled: 0.7784257<br />1/2: 0.5","npv_sim:  1594.1911484<br />scaled: 0.7766511<br />1/2: 0.5","npv_sim:  1602.0064914<br />scaled: 0.7748751<br />1/2: 0.5","npv_sim:  1609.8218343<br />scaled: 0.7730990<br />1/2: 0.5","npv_sim:  1617.6371772<br />scaled: 0.7713212<br />1/2: 0.5","npv_sim:  1625.4525202<br />scaled: 0.7695433<br />1/2: 0.5","npv_sim:  1633.2678631<br />scaled: 0.7677635<br />1/2: 0.5","npv_sim:  1641.0832061<br />scaled: 0.7659835<br />1/2: 0.5","npv_sim:  1648.8985490<br />scaled: 0.7642016<br />1/2: 0.5","npv_sim:  1656.7138920<br />scaled: 0.7624189<br />1/2: 0.5","npv_sim:  1664.5292349<br />scaled: 0.7606344<br />1/2: 0.5","npv_sim:  1672.3445778<br />scaled: 0.7588484<br />1/2: 0.5","npv_sim:  1680.1599208<br />scaled: 0.7570608<br />1/2: 0.5","npv_sim:  1687.9752637<br />scaled: 0.7552708<br />1/2: 0.5","npv_sim:  1695.7906067<br />scaled: 0.7534797<br />1/2: 0.5","npv_sim:  1703.6059496<br />scaled: 0.7516847<br />1/2: 0.5","npv_sim:  1711.4212926<br />scaled: 0.7498893<br />1/2: 0.5","npv_sim:  1719.2366355<br />scaled: 0.7480883<br />1/2: 0.5","npv_sim:  1727.0519785<br />scaled: 0.7462874<br />1/2: 0.5","npv_sim:  1734.8673214<br />scaled: 0.7444798<br />1/2: 0.5","npv_sim:  1742.6826643<br />scaled: 0.7426716<br />1/2: 0.5","npv_sim:  1750.4980073<br />scaled: 0.7408570<br />1/2: 0.5","npv_sim:  1758.3133502<br />scaled: 0.7390402<br />1/2: 0.5","npv_sim:  1766.1286932<br />scaled: 0.7372175<br />1/2: 0.5","npv_sim:  1773.9440361<br />scaled: 0.7353906<br />1/2: 0.5","npv_sim:  1781.7593791<br />scaled: 0.7335587<br />1/2: 0.5","npv_sim:  1789.5747220<br />scaled: 0.7317201<br />1/2: 0.5","npv_sim:  1797.3900649<br />scaled: 0.7298779<br />1/2: 0.5","npv_sim:  1805.2054079<br />scaled: 0.7280257<br />1/2: 0.5","npv_sim:  1813.0207508<br />scaled: 0.7261720<br />1/2: 0.5","npv_sim:  1820.8360938<br />scaled: 0.7243044<br />1/2: 0.5","npv_sim:  1828.6514367<br />scaled: 0.7224368<br />1/2: 0.5","npv_sim:  1836.4667797<br />scaled: 0.7205526<br />1/2: 0.5","npv_sim:  1844.2821226<br />scaled: 0.7186676<br />1/2: 0.5","npv_sim:  1852.0974655<br />scaled: 0.7167671<br />1/2: 0.5","npv_sim:  1859.9128085<br />scaled: 0.7148624<br />1/2: 0.5","npv_sim:  1867.7281514<br />scaled: 0.7129440<br />1/2: 0.5","npv_sim:  1875.5434944<br />scaled: 0.7110174<br />1/2: 0.5","npv_sim:  1883.3588373<br />scaled: 0.7090795<br />1/2: 0.5","npv_sim:  1891.1741803<br />scaled: 0.7071287<br />1/2: 0.5","npv_sim:  1898.9895232<br />scaled: 0.7051698<br />1/2: 0.5","npv_sim:  1906.8048662<br />scaled: 0.7031923<br />1/2: 0.5","npv_sim:  1914.6202091<br />scaled: 0.7012106<br />1/2: 0.5","npv_sim:  1922.4355520<br />scaled: 0.6992040<br />1/2: 0.5","npv_sim:  1930.2508950<br />scaled: 0.6971974<br />1/2: 0.5","npv_sim:  1938.0662379<br />scaled: 0.6951596<br />1/2: 0.5","npv_sim:  1945.8815809<br />scaled: 0.6931213<br />1/2: 0.5","npv_sim:  1953.6969238<br />scaled: 0.6910547<br />1/2: 0.5","npv_sim:  1961.5122668<br />scaled: 0.6889821<br />1/2: 0.5","npv_sim:  1969.3276097<br />scaled: 0.6868851<br />1/2: 0.5","npv_sim:  1977.1429526<br />scaled: 0.6847756<br />1/2: 0.5","npv_sim:  1984.9582956<br />scaled: 0.6826463<br />1/2: 0.5","npv_sim:  1992.7736385<br />scaled: 0.6804974<br />1/2: 0.5","npv_sim:  2000.5889815<br />scaled: 0.6783340<br />1/2: 0.5","npv_sim:  2008.4043244<br />scaled: 0.6761430<br />1/2: 0.5","npv_sim:  2016.2196674<br />scaled: 0.6739439<br />1/2: 0.5","npv_sim:  2024.0350103<br />scaled: 0.6717082<br />1/2: 0.5","npv_sim:  2031.8503533<br />scaled: 0.6694715<br />1/2: 0.5","npv_sim:  2039.6656962<br />scaled: 0.6671886<br />1/2: 0.5","npv_sim:  2047.4810391<br />scaled: 0.6649058<br />1/2: 0.5","npv_sim:  2055.2963821<br />scaled: 0.6625801<br />1/2: 0.5","npv_sim:  2063.1117250<br />scaled: 0.6602476<br />1/2: 0.5","npv_sim:  2070.9270680<br />scaled: 0.6578786<br />1/2: 0.5","npv_sim:  2078.7424109<br />scaled: 0.6554940<br />1/2: 0.5","npv_sim:  2086.5577539<br />scaled: 0.6530800<br />1/2: 0.5","npv_sim:  2094.3730968<br />scaled: 0.6506411<br />1/2: 0.5","npv_sim:  2102.1884397<br />scaled: 0.6481806<br />1/2: 0.5","npv_sim:  2110.0037827<br />scaled: 0.6456851<br />1/2: 0.5","npv_sim:  2117.8191256<br />scaled: 0.6431767<br />1/2: 0.5","npv_sim:  2125.6344686<br />scaled: 0.6406226<br />1/2: 0.5","npv_sim:  2133.4498115<br />scaled: 0.6380648<br />1/2: 0.5","npv_sim:  2141.2651545<br />scaled: 0.6354501<br />1/2: 0.5","npv_sim:  2149.0804974<br />scaled: 0.6328354<br />1/2: 0.5","npv_sim:  2156.8958403<br />scaled: 0.6301647<br />1/2: 0.5","npv_sim:  2164.7111833<br />scaled: 0.6274877<br />1/2: 0.5","npv_sim:  2172.5265262<br />scaled: 0.6247634<br />1/2: 0.5","npv_sim:  2180.3418692<br />scaled: 0.6220224<br />1/2: 0.5","npv_sim:  2188.1572121<br />scaled: 0.6192436<br />1/2: 0.5","npv_sim:  2195.9725551<br />scaled: 0.6164373<br />1/2: 0.5","npv_sim:  2203.7878980<br />scaled: 0.6136031<br />1/2: 0.5","npv_sim:  2211.6032410<br />scaled: 0.6107302<br />1/2: 0.5","npv_sim:  2219.4185839<br />scaled: 0.6078398<br />1/2: 0.5","npv_sim:  2227.2339268<br />scaled: 0.6048992<br />1/2: 0.5","npv_sim:  2235.0492698<br />scaled: 0.6019519<br />1/2: 0.5","npv_sim:  2242.8646127<br />scaled: 0.5989429<br />1/2: 0.5","npv_sim:  2250.6799557<br />scaled: 0.5959339<br />1/2: 0.5","npv_sim:  2258.4952986<br />scaled: 0.5928602<br />1/2: 0.5","npv_sim:  2266.3106416<br />scaled: 0.5897821<br />1/2: 0.5","npv_sim:  2274.1259845<br />scaled: 0.5866502<br />1/2: 0.5","npv_sim:  2281.9413274<br />scaled: 0.5835027<br />1/2: 0.5","npv_sim:  2289.7566704<br />scaled: 0.5803124<br />1/2: 0.5","npv_sim:  2297.5720133<br />scaled: 0.5770954<br />1/2: 0.5","npv_sim:  2305.3873563<br />scaled: 0.5738468<br />1/2: 0.5","npv_sim:  2313.2026992<br />scaled: 0.5705605<br />1/2: 0.5","npv_sim:  2321.0180422<br />scaled: 0.5672537<br />1/2: 0.5","npv_sim:  2328.8333851<br />scaled: 0.5638984<br />1/2: 0.5","npv_sim:  2336.6487281<br />scaled: 0.5605338<br />1/2: 0.5","npv_sim:  2344.4640710<br />scaled: 0.5571100<br />1/2: 0.5","npv_sim:  2352.2794139<br />scaled: 0.5536863<br />1/2: 0.5","npv_sim:  2360.0947569<br />scaled: 0.5501967<br />1/2: 0.5","npv_sim:  2367.9100998<br />scaled: 0.5467054<br />1/2: 0.5","npv_sim:  2375.7254428<br />scaled: 0.5431601<br />1/2: 0.5","npv_sim:  2383.5407857<br />scaled: 0.5396025<br />1/2: 0.5","npv_sim:  2391.3561287<br />scaled: 0.5360023<br />1/2: 0.5","npv_sim:  2399.1714716<br />scaled: 0.5323797<br />1/2: 0.5","npv_sim:  2399.1714716<br />scaled: 0.5323797<br />1/2: 0.5","npv_sim:  2399.1714716<br />scaled: 0.5323797<br />1/2: 0.5","npv_sim:  2391.3561287<br />scaled: 0.5360023<br />1/2: 0.5","npv_sim:  2383.5407857<br />scaled: 0.5396025<br />1/2: 0.5","npv_sim:  2375.7254428<br />scaled: 0.5431601<br />1/2: 0.5","npv_sim:  2367.9100998<br />scaled: 0.5467054<br />1/2: 0.5","npv_sim:  2360.0947569<br />scaled: 0.5501967<br />1/2: 0.5","npv_sim:  2352.2794139<br />scaled: 0.5536863<br />1/2: 0.5","npv_sim:  2344.4640710<br />scaled: 0.5571100<br />1/2: 0.5","npv_sim:  2336.6487281<br />scaled: 0.5605338<br />1/2: 0.5","npv_sim:  2328.8333851<br />scaled: 0.5638984<br />1/2: 0.5","npv_sim:  2321.0180422<br />scaled: 0.5672537<br />1/2: 0.5","npv_sim:  2313.2026992<br />scaled: 0.5705605<br />1/2: 0.5","npv_sim:  2305.3873563<br />scaled: 0.5738468<br />1/2: 0.5","npv_sim:  2297.5720133<br />scaled: 0.5770954<br />1/2: 0.5","npv_sim:  2289.7566704<br />scaled: 0.5803124<br />1/2: 0.5","npv_sim:  2281.9413274<br />scaled: 0.5835027<br />1/2: 0.5","npv_sim:  2274.1259845<br />scaled: 0.5866502<br />1/2: 0.5","npv_sim:  2266.3106416<br />scaled: 0.5897821<br />1/2: 0.5","npv_sim:  2258.4952986<br />scaled: 0.5928602<br />1/2: 0.5","npv_sim:  2250.6799557<br />scaled: 0.5959339<br />1/2: 0.5","npv_sim:  2242.8646127<br />scaled: 0.5989429<br />1/2: 0.5","npv_sim:  2235.0492698<br />scaled: 0.6019519<br />1/2: 0.5","npv_sim:  2227.2339268<br />scaled: 0.6048992<br />1/2: 0.5","npv_sim:  2219.4185839<br />scaled: 0.6078398<br />1/2: 0.5","npv_sim:  2211.6032410<br />scaled: 0.6107302<br />1/2: 0.5","npv_sim:  2203.7878980<br />scaled: 0.6136031<br />1/2: 0.5","npv_sim:  2195.9725551<br />scaled: 0.6164373<br />1/2: 0.5","npv_sim:  2188.1572121<br />scaled: 0.6192436<br />1/2: 0.5","npv_sim:  2180.3418692<br />scaled: 0.6220224<br />1/2: 0.5","npv_sim:  2172.5265262<br />scaled: 0.6247634<br />1/2: 0.5","npv_sim:  2164.7111833<br />scaled: 0.6274877<br />1/2: 0.5","npv_sim:  2156.8958403<br />scaled: 0.6301647<br />1/2: 0.5","npv_sim:  2149.0804974<br />scaled: 0.6328354<br />1/2: 0.5","npv_sim:  2141.2651545<br />scaled: 0.6354501<br />1/2: 0.5","npv_sim:  2133.4498115<br />scaled: 0.6380648<br />1/2: 0.5","npv_sim:  2125.6344686<br />scaled: 0.6406226<br />1/2: 0.5","npv_sim:  2117.8191256<br />scaled: 0.6431767<br />1/2: 0.5","npv_sim:  2110.0037827<br />scaled: 0.6456851<br />1/2: 0.5","npv_sim:  2102.1884397<br />scaled: 0.6481806<br />1/2: 0.5","npv_sim:  2094.3730968<br />scaled: 0.6506411<br />1/2: 0.5","npv_sim:  2086.5577539<br />scaled: 0.6530800<br />1/2: 0.5","npv_sim:  2078.7424109<br />scaled: 0.6554940<br />1/2: 0.5","npv_sim:  2070.9270680<br />scaled: 0.6578786<br />1/2: 0.5","npv_sim:  2063.1117250<br />scaled: 0.6602476<br />1/2: 0.5","npv_sim:  2055.2963821<br />scaled: 0.6625801<br />1/2: 0.5","npv_sim:  2047.4810391<br />scaled: 0.6649058<br />1/2: 0.5","npv_sim:  2039.6656962<br />scaled: 0.6671886<br />1/2: 0.5","npv_sim:  2031.8503533<br />scaled: 0.6694715<br />1/2: 0.5","npv_sim:  2024.0350103<br />scaled: 0.6717082<br />1/2: 0.5","npv_sim:  2016.2196674<br />scaled: 0.6739439<br />1/2: 0.5","npv_sim:  2008.4043244<br />scaled: 0.6761430<br />1/2: 0.5","npv_sim:  2000.5889815<br />scaled: 0.6783340<br />1/2: 0.5","npv_sim:  1992.7736385<br />scaled: 0.6804974<br />1/2: 0.5","npv_sim:  1984.9582956<br />scaled: 0.6826463<br />1/2: 0.5","npv_sim:  1977.1429526<br />scaled: 0.6847756<br />1/2: 0.5","npv_sim:  1969.3276097<br />scaled: 0.6868851<br />1/2: 0.5","npv_sim:  1961.5122668<br />scaled: 0.6889821<br />1/2: 0.5","npv_sim:  1953.6969238<br />scaled: 0.6910547<br />1/2: 0.5","npv_sim:  1945.8815809<br />scaled: 0.6931213<br />1/2: 0.5","npv_sim:  1938.0662379<br />scaled: 0.6951596<br />1/2: 0.5","npv_sim:  1930.2508950<br />scaled: 0.6971974<br />1/2: 0.5","npv_sim:  1922.4355520<br />scaled: 0.6992040<br />1/2: 0.5","npv_sim:  1914.6202091<br />scaled: 0.7012106<br />1/2: 0.5","npv_sim:  1906.8048662<br />scaled: 0.7031923<br />1/2: 0.5","npv_sim:  1898.9895232<br />scaled: 0.7051698<br />1/2: 0.5","npv_sim:  1891.1741803<br />scaled: 0.7071287<br />1/2: 0.5","npv_sim:  1883.3588373<br />scaled: 0.7090795<br />1/2: 0.5","npv_sim:  1875.5434944<br />scaled: 0.7110174<br />1/2: 0.5","npv_sim:  1867.7281514<br />scaled: 0.7129440<br />1/2: 0.5","npv_sim:  1859.9128085<br />scaled: 0.7148624<br />1/2: 0.5","npv_sim:  1852.0974655<br />scaled: 0.7167671<br />1/2: 0.5","npv_sim:  1844.2821226<br />scaled: 0.7186676<br />1/2: 0.5","npv_sim:  1836.4667797<br />scaled: 0.7205526<br />1/2: 0.5","npv_sim:  1828.6514367<br />scaled: 0.7224368<br />1/2: 0.5","npv_sim:  1820.8360938<br />scaled: 0.7243044<br />1/2: 0.5","npv_sim:  1813.0207508<br />scaled: 0.7261720<br />1/2: 0.5","npv_sim:  1805.2054079<br />scaled: 0.7280257<br />1/2: 0.5","npv_sim:  1797.3900649<br />scaled: 0.7298779<br />1/2: 0.5","npv_sim:  1789.5747220<br />scaled: 0.7317201<br />1/2: 0.5","npv_sim:  1781.7593791<br />scaled: 0.7335587<br />1/2: 0.5","npv_sim:  1773.9440361<br />scaled: 0.7353906<br />1/2: 0.5","npv_sim:  1766.1286932<br />scaled: 0.7372175<br />1/2: 0.5","npv_sim:  1758.3133502<br />scaled: 0.7390402<br />1/2: 0.5","npv_sim:  1750.4980073<br />scaled: 0.7408570<br />1/2: 0.5","npv_sim:  1742.6826643<br />scaled: 0.7426716<br />1/2: 0.5","npv_sim:  1734.8673214<br />scaled: 0.7444798<br />1/2: 0.5","npv_sim:  1727.0519785<br />scaled: 0.7462874<br />1/2: 0.5","npv_sim:  1719.2366355<br />scaled: 0.7480883<br />1/2: 0.5","npv_sim:  1711.4212926<br />scaled: 0.7498893<br />1/2: 0.5","npv_sim:  1703.6059496<br />scaled: 0.7516847<br />1/2: 0.5","npv_sim:  1695.7906067<br />scaled: 0.7534797<br />1/2: 0.5","npv_sim:  1687.9752637<br />scaled: 0.7552708<br />1/2: 0.5","npv_sim:  1680.1599208<br />scaled: 0.7570608<br />1/2: 0.5","npv_sim:  1672.3445778<br />scaled: 0.7588484<br />1/2: 0.5","npv_sim:  1664.5292349<br />scaled: 0.7606344<br />1/2: 0.5","npv_sim:  1656.7138920<br />scaled: 0.7624189<br />1/2: 0.5","npv_sim:  1648.8985490<br />scaled: 0.7642016<br />1/2: 0.5","npv_sim:  1641.0832061<br />scaled: 0.7659835<br />1/2: 0.5","npv_sim:  1633.2678631<br />scaled: 0.7677635<br />1/2: 0.5","npv_sim:  1625.4525202<br />scaled: 0.7695433<br />1/2: 0.5","npv_sim:  1617.6371772<br />scaled: 0.7713212<br />1/2: 0.5","npv_sim:  1609.8218343<br />scaled: 0.7730990<br />1/2: 0.5","npv_sim:  1602.0064914<br />scaled: 0.7748751<br />1/2: 0.5","npv_sim:  1594.1911484<br />scaled: 0.7766511<br />1/2: 0.5","npv_sim:  1586.3758055<br />scaled: 0.7784257<br />1/2: 0.5","npv_sim:  1578.5604625<br />scaled: 0.7802001<br />1/2: 0.5","npv_sim:  1570.7451196<br />scaled: 0.7819734<br />1/2: 0.5","npv_sim:  1562.9297766<br />scaled: 0.7837461<br />1/2: 0.5","npv_sim:  1555.1144337<br />scaled: 0.7855181<br />1/2: 0.5","npv_sim:  1547.2990907<br />scaled: 0.7872892<br />1/2: 0.5","npv_sim:  1539.4837478<br />scaled: 0.7890597<br />1/2: 0.5","npv_sim:  1531.6684049<br />scaled: 0.7908290<br />1/2: 0.5","npv_sim:  1523.8530619<br />scaled: 0.7925979<br />1/2: 0.5","npv_sim:  1516.0377190<br />scaled: 0.7943652<br />1/2: 0.5","npv_sim:  1508.2223760<br />scaled: 0.7961324<br />1/2: 0.5","npv_sim:  1500.4070331<br />scaled: 0.7978972<br />1/2: 0.5","npv_sim:  1492.5916901<br />scaled: 0.7996620<br />1/2: 0.5","npv_sim:  1484.7763472<br />scaled: 0.8014244<br />1/2: 0.5","npv_sim:  1476.9610043<br />scaled: 0.8031864<br />1/2: 0.5","npv_sim:  1469.1456613<br />scaled: 0.8049460<br />1/2: 0.5","npv_sim:  1461.3303184<br />scaled: 0.8067046<br />1/2: 0.5","npv_sim:  1453.5149754<br />scaled: 0.8084611<br />1/2: 0.5","npv_sim:  1445.6996325<br />scaled: 0.8102158<br />1/2: 0.5","npv_sim:  1437.8842895<br />scaled: 0.8119688<br />1/2: 0.5","npv_sim:  1430.0689466<br />scaled: 0.8137189<br />1/2: 0.5","npv_sim:  1422.2536036<br />scaled: 0.8154679<br />1/2: 0.5","npv_sim:  1414.4382607<br />scaled: 0.8172128<br />1/2: 0.5","npv_sim:  1406.6229178<br />scaled: 0.8189575<br />1/2: 0.5","npv_sim:  1398.8075748<br />scaled: 0.8206965<br />1/2: 0.5","npv_sim:  1390.9922319<br />scaled: 0.8224355<br />1/2: 0.5","npv_sim:  1383.1768889<br />scaled: 0.8241688<br />1/2: 0.5","npv_sim:  1375.3615460<br />scaled: 0.8259012<br />1/2: 0.5","npv_sim:  1367.5462030<br />scaled: 0.8276284<br />1/2: 0.5","npv_sim:  1359.7308601<br />scaled: 0.8293536<br />1/2: 0.5","npv_sim:  1351.9155172<br />scaled: 0.8310743<br />1/2: 0.5","npv_sim:  1344.1001742<br />scaled: 0.8327915<br />1/2: 0.5","npv_sim:  1336.2848313<br />scaled: 0.8345052<br />1/2: 0.5","npv_sim:  1328.4694883<br />scaled: 0.8362138<br />1/2: 0.5","npv_sim:  1320.6541454<br />scaled: 0.8379202<br />1/2: 0.5","npv_sim:  1312.8388024<br />scaled: 0.8396195<br />1/2: 0.5","npv_sim:  1305.0234595<br />scaled: 0.8413180<br />1/2: 0.5","npv_sim:  1297.2081166<br />scaled: 0.8430074<br />1/2: 0.5","npv_sim:  1289.3927736<br />scaled: 0.8446969<br />1/2: 0.5","npv_sim:  1281.5774307<br />scaled: 0.8463767<br />1/2: 0.5","npv_sim:  1273.7620877<br />scaled: 0.8480557<br />1/2: 0.5","npv_sim:  1265.9467448<br />scaled: 0.8497263<br />1/2: 0.5","npv_sim:  1258.1314018<br />scaled: 0.8513943<br />1/2: 0.5","npv_sim:  1250.3160589<br />scaled: 0.8530555<br />1/2: 0.5","npv_sim:  1242.5007159<br />scaled: 0.8547121<br />1/2: 0.5","npv_sim:  1234.6853730<br />scaled: 0.8563635<br />1/2: 0.5","npv_sim:  1226.8700301<br />scaled: 0.8580083<br />1/2: 0.5","npv_sim:  1219.0546871<br />scaled: 0.8596498<br />1/2: 0.5","npv_sim:  1211.2393442<br />scaled: 0.8612824<br />1/2: 0.5","npv_sim:  1203.4240012<br />scaled: 0.8629136<br />1/2: 0.5","npv_sim:  1195.6086583<br />scaled: 0.8645339<br />1/2: 0.5","npv_sim:  1187.7933153<br />scaled: 0.8661542<br />1/2: 0.5","npv_sim:  1179.9779724<br />scaled: 0.8677624<br />1/2: 0.5","npv_sim:  1172.1626295<br />scaled: 0.8693701<br />1/2: 0.5","npv_sim:  1164.3472865<br />scaled: 0.8709676<br />1/2: 0.5","npv_sim:  1156.5319436<br />scaled: 0.8725626<br />1/2: 0.5","npv_sim:  1148.7166006<br />scaled: 0.8741495<br />1/2: 0.5","npv_sim:  1140.9012577<br />scaled: 0.8757318<br />1/2: 0.5","npv_sim:  1133.0859147<br />scaled: 0.8773080<br />1/2: 0.5","npv_sim:  1125.2705718<br />scaled: 0.8788775<br />1/2: 0.5","npv_sim:  1117.4552288<br />scaled: 0.8804431<br />1/2: 0.5","npv_sim:  1109.6398859<br />scaled: 0.8820000<br />1/2: 0.5","npv_sim:  1101.8245430<br />scaled: 0.8835550<br />1/2: 0.5","npv_sim:  1094.0092000<br />scaled: 0.8850994<br />1/2: 0.5","npv_sim:  1086.1938571<br />scaled: 0.8866439<br />1/2: 0.5","npv_sim:  1078.3785141<br />scaled: 0.8881762<br />1/2: 0.5","npv_sim:  1070.5631712<br />scaled: 0.8897084<br />1/2: 0.5","npv_sim:  1062.7478282<br />scaled: 0.8912306<br />1/2: 0.5","npv_sim:  1054.9324853<br />scaled: 0.8927508<br />1/2: 0.5","npv_sim:  1047.1171424<br />scaled: 0.8942631<br />1/2: 0.5","npv_sim:  1039.3017994<br />scaled: 0.8957716<br />1/2: 0.5","npv_sim:  1031.4864565<br />scaled: 0.8972742<br />1/2: 0.5","npv_sim:  1023.6711135<br />scaled: 0.8987714<br />1/2: 0.5","npv_sim:  1015.8557706<br />scaled: 0.9002646<br />1/2: 0.5","npv_sim:  1008.0404276<br />scaled: 0.9017507<br />1/2: 0.5","npv_sim:  1000.2250847<br />scaled: 0.9032348<br />1/2: 0.5","npv_sim:   992.4097417<br />scaled: 0.9047103<br />1/2: 0.5","npv_sim:   984.5943988<br />scaled: 0.9061854<br />1/2: 0.5","npv_sim:   976.7790559<br />scaled: 0.9076506<br />1/2: 0.5","npv_sim:   968.9637129<br />scaled: 0.9091158<br />1/2: 0.5","npv_sim:   961.1483700<br />scaled: 0.9105723<br />1/2: 0.5","npv_sim:   953.3330270<br />scaled: 0.9120275<br />1/2: 0.5","npv_sim:   945.5176841<br />scaled: 0.9134759<br />1/2: 0.5","npv_sim:   937.7023411<br />scaled: 0.9149216<br />1/2: 0.5","npv_sim:   929.8869982<br />scaled: 0.9163621<br />1/2: 0.5","npv_sim:   922.0716553<br />scaled: 0.9177984<br />1/2: 0.5","npv_sim:   914.2563123<br />scaled: 0.9192311<br />1/2: 0.5","npv_sim:   906.4409694<br />scaled: 0.9206584<br />1/2: 0.5","npv_sim:   898.6256264<br />scaled: 0.9220836<br />1/2: 0.5","npv_sim:   890.8102835<br />scaled: 0.9235019<br />1/2: 0.5","npv_sim:   882.9949405<br />scaled: 0.9249196<br />1/2: 0.5","npv_sim:   875.1795976<br />scaled: 0.9263292<br />1/2: 0.5","npv_sim:   867.3642547<br />scaled: 0.9277387<br />1/2: 0.5","npv_sim:   859.5489117<br />scaled: 0.9291402<br />1/2: 0.5","npv_sim:   851.7335688<br />scaled: 0.9305409<br />1/2: 0.5","npv_sim:   843.9182258<br />scaled: 0.9319349<br />1/2: 0.5","npv_sim:   836.1028829<br />scaled: 0.9333267<br />1/2: 0.5","npv_sim:   828.2875399<br />scaled: 0.9347131<br />1/2: 0.5","npv_sim:   820.4721970<br />scaled: 0.9360958<br />1/2: 0.5","npv_sim:   812.6568540<br />scaled: 0.9374744<br />1/2: 0.5","npv_sim:   804.8415111<br />scaled: 0.9388475<br />1/2: 0.5","npv_sim:   797.0261682<br />scaled: 0.9402180<br />1/2: 0.5","npv_sim:   789.2108252<br />scaled: 0.9415812<br />1/2: 0.5","npv_sim:   781.3954823<br />scaled: 0.9429431<br />1/2: 0.5","npv_sim:   773.5801393<br />scaled: 0.9442956<br />1/2: 0.5","npv_sim:   765.7647964<br />scaled: 0.9456482<br />1/2: 0.5","npv_sim:   757.9494534<br />scaled: 0.9469898<br />1/2: 0.5","npv_sim:   750.1341105<br />scaled: 0.9483308<br />1/2: 0.5","npv_sim:   742.3187676<br />scaled: 0.9496620<br />1/2: 0.5","npv_sim:   734.5034246<br />scaled: 0.9509905<br />1/2: 0.5","npv_sim:   726.6880817<br />scaled: 0.9523105<br />1/2: 0.5","npv_sim:   718.8727387<br />scaled: 0.9536254<br />1/2: 0.5","npv_sim:   711.0573958<br />scaled: 0.9549333<br />1/2: 0.5","npv_sim:   703.2420528<br />scaled: 0.9562331<br />1/2: 0.5","npv_sim:   695.4267099<br />scaled: 0.9575279<br />1/2: 0.5","npv_sim:   687.6113669<br />scaled: 0.9588112<br />1/2: 0.5","npv_sim:   679.7960240<br />scaled: 0.9600918<br />1/2: 0.5","npv_sim:   671.9806811<br />scaled: 0.9613567<br />1/2: 0.5","npv_sim:   664.1653381<br />scaled: 0.9626217<br />1/2: 0.5","npv_sim:   656.3499952<br />scaled: 0.9638666<br />1/2: 0.5","npv_sim:   648.5346522<br />scaled: 0.9651112<br />1/2: 0.5","npv_sim:   640.7193093<br />scaled: 0.9663372<br />1/2: 0.5","npv_sim:   632.9039663<br />scaled: 0.9675593<br />1/2: 0.5","npv_sim:   625.0886234<br />scaled: 0.9687649<br />1/2: 0.5","npv_sim:   617.2732805<br />scaled: 0.9699622<br />1/2: 0.5","npv_sim:   609.4579375<br />scaled: 0.9711456<br />1/2: 0.5","npv_sim:   601.6425946<br />scaled: 0.9723154<br />1/2: 0.5","npv_sim:   593.8272516<br />scaled: 0.9734749<br />1/2: 0.5","npv_sim:   586.0119087<br />scaled: 0.9746145<br />1/2: 0.5","npv_sim:   578.1965657<br />scaled: 0.9757480<br />1/2: 0.5","npv_sim:   570.3812228<br />scaled: 0.9768545<br />1/2: 0.5","npv_sim:   562.5658798<br />scaled: 0.9779601<br />1/2: 0.5","npv_sim:   554.7505369<br />scaled: 0.9790302<br />1/2: 0.5","npv_sim:   546.9351940<br />scaled: 0.9801004<br />1/2: 0.5","npv_sim:   539.1198510<br />scaled: 0.9811364<br />1/2: 0.5","npv_sim:   531.3045081<br />scaled: 0.9821669<br />1/2: 0.5","npv_sim:   523.4891651<br />scaled: 0.9831672<br />1/2: 0.5","npv_sim:   515.6738222<br />scaled: 0.9841546<br />1/2: 0.5","npv_sim:   507.8584792<br />scaled: 0.9851167<br />1/2: 0.5","npv_sim:   500.0431363<br />scaled: 0.9860575<br />1/2: 0.5","npv_sim:   492.2277934<br />scaled: 0.9869789<br />1/2: 0.5","npv_sim:   484.4124504<br />scaled: 0.9878691<br />1/2: 0.5","npv_sim:   476.5971075<br />scaled: 0.9887473<br />1/2: 0.5","npv_sim:   468.7817645<br />scaled: 0.9895832<br />1/2: 0.5","npv_sim:   460.9664216<br />scaled: 0.9904155<br />1/2: 0.5","npv_sim:   453.1510786<br />scaled: 0.9911930<br />1/2: 0.5","npv_sim:   445.3357357<br />scaled: 0.9919705<br />1/2: 0.5","npv_sim:   437.5203928<br />scaled: 0.9926918<br />1/2: 0.5","npv_sim:   429.7050498<br />scaled: 0.9934068<br />1/2: 0.5","npv_sim:   421.8897069<br />scaled: 0.9940726<br />1/2: 0.5","npv_sim:   414.0743639<br />scaled: 0.9947211<br />1/2: 0.5","npv_sim:   406.2590210<br />scaled: 0.9953286<br />1/2: 0.5","npv_sim:   398.4436780<br />scaled: 0.9959063<br />1/2: 0.5","npv_sim:   390.6283351<br />scaled: 0.9964526<br />1/2: 0.5","npv_sim:   382.8129921<br />scaled: 0.9969554<br />1/2: 0.5","npv_sim:   374.9976492<br />scaled: 0.9974377<br />1/2: 0.5","npv_sim:   367.1823063<br />scaled: 0.9978614<br />1/2: 0.5","npv_sim:   359.3669633<br />scaled: 0.9982768<br />1/2: 0.5","npv_sim:   351.5516204<br />scaled: 0.9986171<br />1/2: 0.5","npv_sim:   343.7362774<br />scaled: 0.9989574<br />1/2: 0.5","npv_sim:   335.9209345<br />scaled: 0.9992156<br />1/2: 0.5","npv_sim:   328.1055915<br />scaled: 0.9994685<br />1/2: 0.5","npv_sim:   320.2902486<br />scaled: 0.9996501<br />1/2: 0.5","npv_sim:   312.4749057<br />scaled: 0.9998114<br />1/2: 0.5","npv_sim:   304.6595627<br />scaled: 0.9999138<br />1/2: 0.5","npv_sim:   296.8442198<br />scaled: 0.9999796<br />1/2: 0.5","npv_sim:   289.0288768<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   281.2135339<br />scaled: 0.9999665<br />1/2: 0.5","npv_sim:   273.3981909<br />scaled: 0.9999024<br />1/2: 0.5","npv_sim:   265.5828480<br />scaled: 0.9997659<br />1/2: 0.5","npv_sim:   257.7675050<br />scaled: 0.9996148<br />1/2: 0.5","npv_sim:   249.9521621<br />scaled: 0.9993718<br />1/2: 0.5","npv_sim:   242.1368192<br />scaled: 0.9991287<br />1/2: 0.5","npv_sim:   234.3214762<br />scaled: 0.9987784<br />1/2: 0.5","npv_sim:   226.5061333<br />scaled: 0.9984254<br />1/2: 0.5","npv_sim:   218.6907903<br />scaled: 0.9979803<br />1/2: 0.5","npv_sim:   210.8754474<br />scaled: 0.9975144<br />1/2: 0.5","npv_sim:   203.0601044<br />scaled: 0.9969725<br />1/2: 0.5","npv_sim:   195.2447615<br />scaled: 0.9963906<br />1/2: 0.5","npv_sim:   187.4294186<br />scaled: 0.9957501<br />1/2: 0.5","npv_sim:   179.6140756<br />scaled: 0.9950498<br />1/2: 0.5","npv_sim:   171.7987327<br />scaled: 0.9943091<br />1/2: 0.5","npv_sim:   163.9833897<br />scaled: 0.9934879<br />1/2: 0.5","npv_sim:   156.1680468<br />scaled: 0.9926453<br />1/2: 0.5","npv_sim:   148.3527038<br />scaled: 0.9917013<br />1/2: 0.5","npv_sim:   140.5373609<br />scaled: 0.9907555<br />1/2: 0.5","npv_sim:   132.7220179<br />scaled: 0.9896869<br />1/2: 0.5","npv_sim:   124.9066750<br />scaled: 0.9886182<br />1/2: 0.5","npv_sim:   117.0913321<br />scaled: 0.9874420<br />1/2: 0.5","npv_sim:   109.2759891<br />scaled: 0.9862474<br />1/2: 0.5","npv_sim:   101.4606462<br />scaled: 0.9849646<br />1/2: 0.5","npv_sim:    93.6453032<br />scaled: 0.9836429<br />1/2: 0.5","npv_sim:    85.8299603<br />scaled: 0.9822532<br />1/2: 0.5","npv_sim:    78.0146173<br />scaled: 0.9808037<br />1/2: 0.5","npv_sim:    70.1992744<br />scaled: 0.9793065<br />1/2: 0.5","npv_sim:    62.3839315<br />scaled: 0.9777290<br />1/2: 0.5","npv_sim:    54.5685885<br />scaled: 0.9761243<br />1/2: 0.5","npv_sim:    46.7532456<br />scaled: 0.9744187<br />1/2: 0.5","npv_sim:    38.9379026<br />scaled: 0.9727065<br />1/2: 0.5","npv_sim:    31.1225597<br />scaled: 0.9708733<br />1/2: 0.5","npv_sim:    23.3072167<br />scaled: 0.9690401<br />1/2: 0.5","npv_sim:    15.4918738<br />scaled: 0.9670939<br />1/2: 0.5","npv_sim:     7.6765309<br />scaled: 0.9651339<br />1/2: 0.5","npv_sim:    -0.1388121<br />scaled: 0.9630821<br />1/2: 0.5","npv_sim:    -7.9541550<br />scaled: 0.9609965<br />1/2: 0.5","npv_sim:   -15.7694980<br />scaled: 0.9588402<br />1/2: 0.5","npv_sim:   -23.5848409<br />scaled: 0.9566305<br />1/2: 0.5","npv_sim:   -31.4001839<br />scaled: 0.9543709<br />1/2: 0.5","npv_sim:   -39.2155268<br />scaled: 0.9520392<br />1/2: 0.5","npv_sim:   -47.0308698<br />scaled: 0.9496777<br />1/2: 0.5","npv_sim:   -54.8462127<br />scaled: 0.9472263<br />1/2: 0.5","npv_sim:   -62.6615556<br />scaled: 0.9447645<br />1/2: 0.5","npv_sim:   -70.4768986<br />scaled: 0.9421961<br />1/2: 0.5","npv_sim:   -78.2922415<br />scaled: 0.9396278<br />1/2: 0.5","npv_sim:   -86.1075845<br />scaled: 0.9369536<br />1/2: 0.5","npv_sim:   -93.9229274<br />scaled: 0.9342714<br />1/2: 0.5","npv_sim:  -101.7382704<br />scaled: 0.9315043<br />1/2: 0.5","npv_sim:  -109.5536133<br />scaled: 0.9287116<br />1/2: 0.5","npv_sim:  -117.3689562<br />scaled: 0.9258540<br />1/2: 0.5","npv_sim:  -125.1842992<br />scaled: 0.9229546<br />1/2: 0.5","npv_sim:  -132.9996421<br />scaled: 0.9200093<br />1/2: 0.5","npv_sim:  -140.8149851<br />scaled: 0.9170073<br />1/2: 0.5","npv_sim:  -148.6303280<br />scaled: 0.9139771<br />1/2: 0.5","npv_sim:  -156.4456710<br />scaled: 0.9108770<br />1/2: 0.5","npv_sim:  -164.2610139<br />scaled: 0.9077649<br />1/2: 0.5","npv_sim:  -172.0763569<br />scaled: 0.9045713<br />1/2: 0.5","npv_sim:  -179.8916998<br />scaled: 0.9013777<br />1/2: 0.5","npv_sim:  -187.7070427<br />scaled: 0.8980985<br />1/2: 0.5","npv_sim:  -195.5223857<br />scaled: 0.8948163<br />1/2: 0.5","npv_sim:  -203.3377286<br />scaled: 0.8914670<br />1/2: 0.5","npv_sim:  -211.1530716<br />scaled: 0.8881016<br />1/2: 0.5","npv_sim:  -218.9684145<br />scaled: 0.8846858<br />1/2: 0.5","npv_sim:  -226.7837575<br />scaled: 0.8812425<br />1/2: 0.5","npv_sim:  -234.5991004<br />scaled: 0.8777641<br />1/2: 0.5","npv_sim:  -242.4144433<br />scaled: 0.8742486<br />1/2: 0.5","npv_sim:  -250.2297863<br />scaled: 0.8707114<br />1/2: 0.5","npv_sim:  -258.0451292<br />scaled: 0.8671295<br />1/2: 0.5","npv_sim:  -265.8604722<br />scaled: 0.8635375<br />1/2: 0.5","npv_sim:  -273.6758151<br />scaled: 0.8598951<br />1/2: 0.5","npv_sim:  -281.4911581<br />scaled: 0.8562525<br />1/2: 0.5","npv_sim:  -289.3065010<br />scaled: 0.8525557<br />1/2: 0.5","npv_sim:  -297.1218440<br />scaled: 0.8488589<br />1/2: 0.5","npv_sim:  -304.9371869<br />scaled: 0.8451214<br />1/2: 0.5","npv_sim:  -312.7525298<br />scaled: 0.8413764<br />1/2: 0.5","npv_sim:  -320.5678728<br />scaled: 0.8376028<br />1/2: 0.5","npv_sim:  -328.3832157<br />scaled: 0.8338159<br />1/2: 0.5","npv_sim:  -336.1985587<br />scaled: 0.8300103<br />1/2: 0.5","npv_sim:  -344.0139016<br />scaled: 0.8261878<br />1/2: 0.5","npv_sim:  -351.8292446<br />scaled: 0.8223546<br />1/2: 0.5","npv_sim:  -359.6445875<br />scaled: 0.8185026<br />1/2: 0.5","npv_sim:  -367.4599304<br />scaled: 0.8146461<br />1/2: 0.5","npv_sim:  -375.2752734<br />scaled: 0.8107711<br />1/2: 0.5","npv_sim:  -383.0906163<br />scaled: 0.8068954<br />1/2: 0.5","npv_sim:  -390.9059593<br />scaled: 0.8030037<br />1/2: 0.5","npv_sim:  -398.7213022<br />scaled: 0.7991119<br />1/2: 0.5","npv_sim:  -406.5366452<br />scaled: 0.7952107<br />1/2: 0.5","npv_sim:  -414.3519881<br />scaled: 0.7913083<br />1/2: 0.5","npv_sim:  -422.1673310<br />scaled: 0.7874026<br />1/2: 0.5","npv_sim:  -429.9826740<br />scaled: 0.7834957<br />1/2: 0.5","npv_sim:  -437.7980169<br />scaled: 0.7795895<br />1/2: 0.5","npv_sim:  -445.6133599<br />scaled: 0.7756839<br />1/2: 0.5","npv_sim:  -453.4287028<br />scaled: 0.7717813<br />1/2: 0.5","npv_sim:  -461.2440458<br />scaled: 0.7678829<br />1/2: 0.5","npv_sim:  -469.0593887<br />scaled: 0.7639877<br />1/2: 0.5","npv_sim:  -476.8747317<br />scaled: 0.7601022<br />1/2: 0.5","npv_sim:  -484.6900746<br />scaled: 0.7562182<br />1/2: 0.5","npv_sim:  -492.5054175<br />scaled: 0.7523511<br />1/2: 0.5","npv_sim:  -500.3207605<br />scaled: 0.7484839<br />1/2: 0.5","npv_sim:  -508.1361034<br />scaled: 0.7446385<br />1/2: 0.5","npv_sim:  -515.9514464<br />scaled: 0.7407949<br />1/2: 0.5","npv_sim:  -523.7667893<br />scaled: 0.7369730<br />1/2: 0.5","npv_sim:  -531.5821323<br />scaled: 0.7331580<br />1/2: 0.5","npv_sim:  -539.3974752<br />scaled: 0.7293630<br />1/2: 0.5","npv_sim:  -547.2128181<br />scaled: 0.7255813<br />1/2: 0.5","npv_sim:  -555.0281611<br />scaled: 0.7218162<br />1/2: 0.5","npv_sim:  -562.8435040<br />scaled: 0.7180724<br />1/2: 0.5","npv_sim:  -570.6588470<br />scaled: 0.7143403<br />1/2: 0.5","npv_sim:  -578.4741899<br />scaled: 0.7106385<br />1/2: 0.5","npv_sim:  -586.2895329<br />scaled: 0.7069421<br />1/2: 0.5","npv_sim:  -594.1048758<br />scaled: 0.7032863<br />1/2: 0.5","npv_sim:  -601.9202188<br />scaled: 0.6996305<br />1/2: 0.5","npv_sim:  -609.7355617<br />scaled: 0.6960221<br />1/2: 0.5","npv_sim:  -617.5509046<br />scaled: 0.6924159<br />1/2: 0.5","npv_sim:  -625.3662476<br />scaled: 0.6888517<br />1/2: 0.5","npv_sim:  -633.1815905<br />scaled: 0.6852983<br />1/2: 0.5","npv_sim:  -640.9969335<br />scaled: 0.6817804<br />1/2: 0.5","npv_sim:  -648.8122764<br />scaled: 0.6782829<br />1/2: 0.5","npv_sim:  -656.6276194<br />scaled: 0.6748131<br />1/2: 0.5","npv_sim:  -664.4429623<br />scaled: 0.6713740<br />1/2: 0.5","npv_sim:  -672.2583052<br />scaled: 0.6679540<br />1/2: 0.5","npv_sim:  -680.0736482<br />scaled: 0.6645757<br />1/2: 0.5","npv_sim:  -687.8889911<br />scaled: 0.6612070<br />1/2: 0.5","npv_sim:  -695.7043341<br />scaled: 0.6578913<br />1/2: 0.5","npv_sim:  -703.5196770<br />scaled: 0.6545757<br />1/2: 0.5","npv_sim:  -711.3350200<br />scaled: 0.6513239<br />1/2: 0.5","npv_sim:  -719.1503629<br />scaled: 0.6480726<br />1/2: 0.5","npv_sim:  -726.9657059<br />scaled: 0.6448759<br />1/2: 0.5","npv_sim:  -734.7810488<br />scaled: 0.6416901<br />1/2: 0.5","npv_sim:  -742.5963917<br />scaled: 0.6385492<br />1/2: 0.5","npv_sim:  -750.4117347<br />scaled: 0.6354299<br />1/2: 0.5","npv_sim:  -758.2270776<br />scaled: 0.6323451<br />1/2: 0.5","npv_sim:  -766.0424206<br />scaled: 0.6292929<br />1/2: 0.5","npv_sim:  -773.8577635<br />scaled: 0.6262645<br />1/2: 0.5","npv_sim:  -781.6731065<br />scaled: 0.6232797<br />1/2: 0.5","npv_sim:  -789.4884494<br />scaled: 0.6203078<br />1/2: 0.5","npv_sim:  -797.3037923<br />scaled: 0.6173903<br />1/2: 0.5","npv_sim:  -805.1191353<br />scaled: 0.6144750<br />1/2: 0.5","npv_sim:  -812.9344782<br />scaled: 0.6116244<br />1/2: 0.5","npv_sim:  -820.7498212<br />scaled: 0.6087739<br />1/2: 0.5","npv_sim:  -828.5651641<br />scaled: 0.6059810<br />1/2: 0.5","npv_sim:  -836.3805071<br />scaled: 0.6031967<br />1/2: 0.5","npv_sim:  -844.1958500<br />scaled: 0.6004588<br />1/2: 0.5","npv_sim:  -852.0111929<br />scaled: 0.5977397<br />1/2: 0.5","npv_sim:  -859.8265359<br />scaled: 0.5950559<br />1/2: 0.5","npv_sim:  -867.6418788<br />scaled: 0.5924008<br />1/2: 0.5","npv_sim:  -875.4572218<br />scaled: 0.5897701<br />1/2: 0.5","npv_sim:  -883.2725647<br />scaled: 0.5871775<br />1/2: 0.5","npv_sim:  -891.0879077<br />scaled: 0.5845989<br />1/2: 0.5","npv_sim:  -898.9032506<br />scaled: 0.5820669<br />1/2: 0.5","npv_sim:  -906.7185936<br />scaled: 0.5795391<br />1/2: 0.5","npv_sim:  -914.5339365<br />scaled: 0.5770657<br />1/2: 0.5","npv_sim:  -922.3492794<br />scaled: 0.5745924<br />1/2: 0.5","npv_sim:  -930.1646224<br />scaled: 0.5721703<br />1/2: 0.5","npv_sim:  -937.9799653<br />scaled: 0.5697534<br />1/2: 0.5","npv_sim:  -945.7953083<br />scaled: 0.5673767<br />1/2: 0.5","npv_sim:  -953.6106512<br />scaled: 0.5650137<br />1/2: 0.5","npv_sim:  -961.4259942<br />scaled: 0.5626807<br />1/2: 0.5","npv_sim:  -969.2413371<br />scaled: 0.5603688<br />1/2: 0.5","npv_sim:  -977.0566800<br />scaled: 0.5580776<br />1/2: 0.5","npv_sim:  -984.8720230<br />scaled: 0.5558141<br />1/2: 0.5","npv_sim:  -992.6873659<br />scaled: 0.5535627<br />1/2: 0.5","npv_sim: -1000.5027089<br />scaled: 0.5513445<br />1/2: 0.5","npv_sim: -1008.3180518<br />scaled: 0.5491308<br />1/2: 0.5","npv_sim: -1016.1333948<br />scaled: 0.5469547<br />1/2: 0.5","npv_sim: -1023.9487377<br />scaled: 0.5447787<br />1/2: 0.5","npv_sim: -1031.7640807<br />scaled: 0.5426395<br />1/2: 0.5","npv_sim: -1039.5794236<br />scaled: 0.5405024<br />1/2: 0.5","npv_sim: -1047.3947665<br />scaled: 0.5383932<br />1/2: 0.5","npv_sim: -1055.2101095<br />scaled: 0.5362915<br />1/2: 0.5","npv_sim: -1063.0254524<br />scaled: 0.5342100<br />1/2: 0.5","npv_sim: -1070.8407954<br />scaled: 0.5321404<br />1/2: 0.5","npv_sim: -1078.6561383<br />scaled: 0.5300840<br />1/2: 0.5","npv_sim: -1086.4714813<br />scaled: 0.5280429<br />1/2: 0.5","npv_sim: -1094.2868242<br />scaled: 0.5260094<br />1/2: 0.5","npv_sim: -1102.1021671<br />scaled: 0.5239931<br />1/2: 0.5","npv_sim: -1109.9175101<br />scaled: 0.5219800<br />1/2: 0.5","npv_sim: -1117.7328530<br />scaled: 0.5199850<br />1/2: 0.5","npv_sim: -1125.5481960<br />scaled: 0.5179900<br />1/2: 0.5","npv_sim: -1133.3635389<br />scaled: 0.5160123<br />1/2: 0.5","npv_sim: -1141.1788819<br />scaled: 0.5140349<br />1/2: 0.5","npv_sim: -1148.9942248<br />scaled: 0.5120690<br />1/2: 0.5","npv_sim: -1156.8095678<br />scaled: 0.5101055<br />1/2: 0.5","npv_sim: -1164.6249107<br />scaled: 0.5081489<br />1/2: 0.5","npv_sim: -1172.4402536<br />scaled: 0.5061957<br />1/2: 0.5","npv_sim: -1180.2555966<br />scaled: 0.5042460<br />1/2: 0.5","npv_sim: -1188.0709395<br />scaled: 0.5022995<br />1/2: 0.5","npv_sim: -1195.8862825<br />scaled: 0.5003541<br />1/2: 0.5","npv_sim: -1203.7016254<br />scaled: 0.4984108<br />1/2: 0.5","npv_sim: -1211.5169684<br />scaled: 0.4964675<br />1/2: 0.5","npv_sim: -1219.3323113<br />scaled: 0.4945238<br />1/2: 0.5","npv_sim: -1227.1476542<br />scaled: 0.4925801<br />1/2: 0.5","npv_sim: -1234.9629972<br />scaled: 0.4906327<br />1/2: 0.5","npv_sim: -1242.7783401<br />scaled: 0.4886853<br />1/2: 0.5","npv_sim: -1250.5936831<br />scaled: 0.4867318<br />1/2: 0.5","npv_sim: -1258.4090260<br />scaled: 0.4847773<br />1/2: 0.5","npv_sim: -1266.2243690<br />scaled: 0.4828157<br />1/2: 0.5","npv_sim: -1274.0397119<br />scaled: 0.4808509<br />1/2: 0.5","npv_sim: -1281.8550548<br />scaled: 0.4788789<br />1/2: 0.5","npv_sim: -1289.6703978<br />scaled: 0.4769008<br />1/2: 0.5","npv_sim: -1297.4857407<br />scaled: 0.4749165<br />1/2: 0.5","npv_sim: -1305.3010837<br />scaled: 0.4729220<br />1/2: 0.5","npv_sim: -1313.1164266<br />scaled: 0.4709234<br />1/2: 0.5","npv_sim: -1320.9317696<br />scaled: 0.4689097<br />1/2: 0.5","npv_sim: -1328.7471125<br />scaled: 0.4668948<br />1/2: 0.5","npv_sim: -1336.5624555<br />scaled: 0.4648594<br />1/2: 0.5","npv_sim: -1344.3777984<br />scaled: 0.4628239<br />1/2: 0.5","npv_sim: -1352.1931413<br />scaled: 0.4607666<br />1/2: 0.5","npv_sim: -1360.0084843<br />scaled: 0.4587068<br />1/2: 0.5","npv_sim: -1367.8238272<br />scaled: 0.4566273<br />1/2: 0.5","npv_sim: -1375.6391702<br />scaled: 0.4545408<br />1/2: 0.5","npv_sim: -1383.4545131<br />scaled: 0.4524376<br />1/2: 0.5","npv_sim: -1391.2698561<br />scaled: 0.4503223<br />1/2: 0.5","npv_sim: -1399.0851990<br />scaled: 0.4481941<br />1/2: 0.5","npv_sim: -1406.9005419<br />scaled: 0.4460479<br />1/2: 0.5","npv_sim: -1414.7158849<br />scaled: 0.4438933<br />1/2: 0.5","npv_sim: -1422.5312278<br />scaled: 0.4417145<br />1/2: 0.5","npv_sim: -1430.3465708<br />scaled: 0.4395324<br />1/2: 0.5","npv_sim: -1438.1619137<br />scaled: 0.4373194<br />1/2: 0.5","npv_sim: -1445.9772567<br />scaled: 0.4351064<br />1/2: 0.5","npv_sim: -1453.7925996<br />scaled: 0.4328600<br />1/2: 0.5","npv_sim: -1461.6079426<br />scaled: 0.4306114<br />1/2: 0.5","npv_sim: -1469.4232855<br />scaled: 0.4283342<br />1/2: 0.5","npv_sim: -1477.2386284<br />scaled: 0.4260487<br />1/2: 0.5","npv_sim: -1485.0539714<br />scaled: 0.4237400<br />1/2: 0.5","npv_sim: -1492.8693143<br />scaled: 0.4214167<br />1/2: 0.5","npv_sim: -1500.6846573<br />scaled: 0.4190758<br />1/2: 0.5","npv_sim: -1508.5000002<br />scaled: 0.4167140<br />1/2: 0.5","npv_sim: -1516.3153432<br />scaled: 0.4143405<br />1/2: 0.5","npv_sim: -1524.1306861<br />scaled: 0.4119395<br />1/2: 0.5","npv_sim: -1531.9460290<br />scaled: 0.4095331<br />1/2: 0.5","npv_sim: -1539.7613720<br />scaled: 0.4070925<br />1/2: 0.5","npv_sim: -1547.5767149<br />scaled: 0.4046519<br />1/2: 0.5","npv_sim: -1555.3920579<br />scaled: 0.4021726<br />1/2: 0.5","npv_sim: -1563.2074008<br />scaled: 0.3996923<br />1/2: 0.5","npv_sim: -1571.0227438<br />scaled: 0.3971796<br />1/2: 0.5","npv_sim: -1578.8380867<br />scaled: 0.3946596<br />1/2: 0.5","npv_sim: -1586.6534297<br />scaled: 0.3921138<br />1/2: 0.5","npv_sim: -1594.4687726<br />scaled: 0.3895543<br />1/2: 0.5","npv_sim: -1594.4687726<br />scaled: 0.3895543<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,442.917690302212,442.917690302212],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 442.9177","xintercept: 442.9177"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[664.376535453317],"y":[0.25],"text":"Median NPV:<br />  442.92","hovertext":"x: 664.3765<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[664.376535453317],"y":[0.1],"text":"SD NPV:<br />  1265.48","hovertext":"x: 664.3765<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic Effects of Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2152.98094176433,3118.62418057469],"tickmode":"array","ticktext":["-2000","-1000","0","1000","2000","3000"],"tickvals":[-2000,-1000,0,1000,2000,3000],"categoryorder":"array","categoryarray":["-2000","-1000","0","1000","2000","3000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"b7c64d246fa":{"x":{},"y":{},"alpha":{},"type":"scatter"},"b7c6931c98f":{"xintercept":{}},"b7c623a7d000":{"x":{},"y":{}},"b7c65e48c249":{"x":{},"y":{}}},"cur_data":"b7c64d246fa","visdat":{"b7c64d246fa":["function (y) ","x"],"b7c6931c98f":["function (y) ","x"],"b7c623a7d000":["function (y) ","x"],"b7c65e48c249":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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
