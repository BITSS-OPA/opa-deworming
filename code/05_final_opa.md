---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "15 August, 2020"
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
      #anoying message: https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override

      ## Counts
      # Data cleanning:asd
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
                    sum(costs_by_payer) * (1 + staff_time_var))  
      # Compute the per capita cost for each country (c_i and w_i)
  )
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
    costs_data <- costs1_p1_f(df_costs_var = df_costs_var1,
                              df_costs_cw_var = df_costs_cw_var1,
                              df_counts_var = df_counts_var1,
                              staff_time_var = staff_time_var1)
    
    cost1_in <- costs1_p2_f(country_w_var = costs_data$country_w,
                            country_cost_var = costs_data$per_cap)
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
                  "costs2_in" = costs2_in, "costs2_in_x" = costs2_in_x, "costs_k" = costs_k) )
  }

invisible( list2env(one_run(),.GlobalEnv) )
```




```r
#TODO: update unit test values after updating interest rates to exact formula
#Baird 1: Costs = Baird w/tax and no externalities (no ext); 
#Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, 11.8309012188904)
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); 
#Benef = Baird yes ext
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

# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
unit_test(ea1, 142.278620185973)
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
unit_test(ea2, 766.667141355337)
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
unit_test(ea3, 950.089412364501)

# CEA for EA
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_new, 
                          costs_var = costs2_ea_in, 
                          fudging_var = 0)
unit_test(cea_no_ext_ea, 6452.86204429499)

rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_new, 
                                                costs_var = costs2_ea_in, 
                                                fudging_var = 0),
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
# EXPLAIN
# This function takes as inputs means and standard deviations of source 
# parameters and simualte draws of each source. When the source is a scalar, 
# it generates a draw from a noromal dist (mean, sd). When it is a "small"
# (less than 4 elements) vector, generates independent multivariate normals, 
# when its a large vector...
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
    #browser()
    # Get costs with no staff time
    costs1_data_in <- costs1_p1_f(
      df_costs_var = df_costs_var2,
      df_costs_cw_var = df_costs_cw_var2,
      df_counts_var = df_counts_var2,
      staff_time_var = 0
    )
    
    counts_in <- costs1_data_in$total
    costs_no_staff_in <- costs1_data_in$costs_by_country
    
    costs1_counts_sim <- sapply(counts_in,
                                function(x)  rnorm(nsims, 
                                                   mean = x * counts_par_var2,  
                                                   sd = x * counts_par_var2_sd) 
                                )
    costs1_costs_sim <- sapply(costs_no_staff_in,
                                function(x)  rnorm(nsims, 
                                                   mean = x * costs_par_var2,  
                                                   sd = x * costs_par_var2_sd) 
                                )

    staff_time_sim <- rnorm(nsims, staff_time_var2, staff_time_var2_sd)      
    # Different costs for each staff time draw

    # Emma: figure out how to avoid "grouping" messages.     
    suppressMessages(
    costs1_data_in <- lapply(staff_time_sim,
                                function(x) costs1_p1_f(
                                  df_costs_var = df_costs_var2,
                                  df_costs_cw_var = df_costs_cw_var2,
                                  df_counts_var = df_counts_var2,
                                  staff_time_var = x))
    )

    # draw total costs sameples from each costs data draw
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
                df_costs_var1 = df_costs_var2, #CHECK
                df_costs_cw_var1 = df_costs_cw_var2,#CHECK
                df_counts_var1 = df_counts_var2,#CHECK
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

<!--html_preserve--><div id="htmlwidget-543c486b1f4708fd4b9d" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-543c486b1f4708fd4b9d">{"x":{"data":[{"x":[-1236.8890444924,-1230.85333619709,-1224.81762790178,-1218.78191960647,-1212.74621131115,-1206.71050301584,-1200.67479472053,-1194.63908642522,-1188.60337812991,-1182.5676698346,-1176.53196153928,-1170.49625324397,-1164.46054494866,-1158.42483665335,-1152.38912835804,-1146.35342006273,-1140.31771176741,-1134.2820034721,-1128.24629517679,-1122.21058688148,-1116.17487858617,-1110.13917029085,-1104.10346199554,-1098.06775370023,-1092.03204540492,-1085.99633710961,-1079.9606288143,-1073.92492051898,-1067.88921222367,-1061.85350392836,-1055.81779563305,-1049.78208733774,-1043.74637904243,-1037.71067074711,-1031.6749624518,-1025.63925415649,-1019.60354586118,-1013.56783756587,-1007.53212927056,-1001.49642097524,-995.460712679932,-989.42500438462,-983.389296089308,-977.353587793996,-971.317879498685,-965.282171203373,-959.246462908061,-953.210754612749,-947.175046317438,-941.139338022126,-935.103629726814,-929.067921431503,-923.032213136191,-916.996504840879,-910.960796545567,-904.925088250256,-898.889379954944,-892.853671659632,-886.81796336432,-880.782255069009,-874.746546773697,-868.710838478385,-862.675130183073,-856.639421887762,-850.60371359245,-844.568005297138,-838.532297001826,-832.496588706515,-826.460880411203,-820.425172115891,-814.38946382058,-808.353755525268,-802.318047229956,-796.282338934644,-790.246630639333,-784.210922344021,-778.175214048709,-772.139505753397,-766.103797458086,-760.068089162774,-754.032380867462,-747.99667257215,-741.960964276839,-735.925255981527,-729.889547686215,-723.853839390904,-717.818131095592,-711.78242280028,-705.746714504968,-699.711006209657,-693.675297914345,-687.639589619033,-681.603881323721,-675.56817302841,-669.532464733098,-663.496756437786,-657.461048142474,-651.425339847163,-645.389631551851,-639.353923256539,-633.318214961227,-627.282506665916,-621.246798370604,-615.211090075292,-609.175381779981,-603.139673484669,-597.103965189357,-591.068256894045,-585.032548598734,-578.996840303422,-572.96113200811,-566.925423712798,-560.889715417487,-554.854007122175,-548.818298826863,-542.782590531551,-536.74688223624,-530.711173940928,-524.675465645616,-518.639757350305,-512.604049054993,-506.568340759681,-500.532632464369,-494.496924169058,-488.461215873746,-482.425507578434,-476.389799283122,-470.354090987811,-464.318382692499,-458.282674397187,-452.246966101875,-446.211257806564,-440.175549511252,-434.13984121594,-428.104132920628,-422.068424625317,-416.032716330005,-409.997008034693,-403.961299739382,-397.92559144407,-391.889883148758,-385.854174853446,-379.818466558135,-373.782758262823,-367.747049967511,-361.711341672199,-355.675633376888,-349.639925081576,-343.604216786264,-337.568508490952,-331.532800195641,-325.497091900329,-319.461383605017,-313.425675309705,-307.389967014394,-301.354258719082,-295.31855042377,-289.282842128458,-283.247133833147,-277.211425537835,-271.175717242523,-265.140008947212,-259.1043006519,-253.068592356588,-247.032884061276,-240.997175765965,-234.961467470653,-228.925759175341,-222.890050880029,-216.854342584718,-210.818634289406,-204.782925994094,-198.747217698782,-192.711509403471,-186.675801108159,-180.640092812847,-174.604384517535,-168.568676222224,-162.532967926912,-156.4972596316,-150.461551336288,-144.425843040977,-138.390134745665,-132.354426450353,-126.318718155042,-120.28300985973,-114.247301564418,-108.211593269107,-102.175884973795,-96.1401766784829,-90.1044683831713,-84.0687600878596,-78.0330517925477,-71.997343497236,-65.9616352019243,-59.9259269066124,-53.8902186113007,-47.854510315989,-41.8188020206774,-35.7830937253655,-29.7473854300538,-23.7116771347421,-17.6759688394304,-11.6402605441185,-5.60455224880684,0.431156046504839,6.46686434181652,12.5025726371284,18.5382809324401,24.5739892277518,30.6096975230635,36.6454058183754,42.681114113687,48.7168224089987,54.7525307043104,60.7882389996223,66.823947294934,72.8596555902457,78.8953638855573,84.9310721808693,90.9667804761809,97.0024887714926,103.038197066804,109.073905362116,115.109613657428,121.14532195274,127.181030248051,133.216738543363,139.252446838675,145.288155133986,151.323863429298,157.35957172461,163.395280019922,169.430988315233,175.466696610545,181.502404905857,187.538113201169,193.57382149648,199.609529791792,205.645238087104,211.680946382416,217.716654677727,223.752362973039,229.788071268351,235.823779563663,241.859487858974,247.895196154286,253.930904449598,259.96661274491,266.002321040221,272.038029335533,278.073737630845,284.109445926156,290.145154221468,296.18086251678,302.216570812092,308.252279107403,314.287987402715,320.323695698027,326.359403993339,332.39511228865,338.430820583962,344.466528879274,350.502237174586,356.537945469897,362.573653765209,368.609362060521,374.645070355833,380.680778651144,386.716486946456,392.752195241768,398.78790353708,404.823611832391,410.859320127703,416.895028423015,422.930736718326,428.966445013638,435.00215330895,441.037861604262,447.073569899573,453.109278194885,459.144986490197,465.180694785509,471.21640308082,477.252111376132,483.287819671444,489.323527966756,495.359236262067,501.394944557379,507.430652852691,513.466361148003,519.502069443314,525.537777738626,531.573486033938,537.609194329249,543.644902624561,549.680610919873,555.716319215185,561.752027510496,567.787735805808,573.82344410112,579.859152396431,585.894860691743,591.930568987055,597.966277282367,604.001985577678,610.03769387299,616.073402168302,622.109110463614,628.144818758925,634.180527054237,640.216235349549,646.251943644861,652.287651940172,658.323360235484,664.359068530796,670.394776826108,676.430485121419,682.466193416731,688.501901712043,694.537610007354,700.573318302666,706.609026597978,712.64473489329,718.680443188601,724.716151483913,730.751859779225,736.787568074537,742.823276369848,748.85898466516,754.894692960472,760.930401255784,766.966109551095,773.001817846407,779.037526141719,785.073234437031,791.108942732342,797.144651027654,803.180359322966,809.216067618278,815.251775913589,821.287484208901,827.323192504213,833.358900799524,839.394609094836,845.430317390148,851.46602568546,857.501733980772,863.537442276083,869.573150571395,875.608858866706,881.644567162018,887.68027545733,893.715983752642,899.751692047954,905.787400343266,911.823108638577,917.858816933889,923.8945252292,929.930233524512,935.965941819824,942.001650115136,948.037358410447,954.073066705759,960.108775001071,966.144483296383,972.180191591694,978.215899887006,984.251608182318,990.287316477629,996.323024772941,1002.35873306825,1008.39444136356,1014.43014965888,1020.46585795419,1026.5015662495,1032.53727454481,1038.57298284012,1044.60869113544,1050.64439943075,1056.68010772606,1062.71581602137,1068.75152431668,1074.78723261199,1080.82294090731,1086.85864920262,1092.89435749793,1098.93006579324,1104.96577408855,1111.00148238386,1117.03719067918,1123.07289897449,1129.1086072698,1135.14431556511,1141.18002386042,1147.21573215573,1153.25144045105,1159.28714874636,1165.32285704167,1171.35856533698,1177.39427363229,1183.42998192761,1189.46569022292,1195.50139851823,1201.53710681354,1207.57281510885,1213.60852340416,1219.64423169948,1225.67993999479,1231.7156482901,1237.75135658541,1243.78706488072,1249.82277317603,1255.85848147135,1261.89418976666,1267.92989806197,1273.96560635728,1280.00131465259,1286.0370229479,1292.07273124322,1298.10843953853,1304.14414783384,1310.17985612915,1316.21556442446,1322.25127271978,1328.28698101509,1334.3226893104,1340.35839760571,1346.39410590102,1352.42981419633,1358.46552249165,1364.50123078696,1370.53693908227,1376.57264737758,1382.60835567289,1388.6440639682,1394.67977226352,1400.71548055883,1406.75118885414,1412.78689714945,1418.82260544476,1424.85831374007,1430.89402203539,1436.9297303307,1442.96543862601,1449.00114692132,1455.03685521663,1461.07256351194,1467.10827180726,1473.14398010257,1479.17968839788,1485.21539669319,1491.2511049885,1497.28681328382,1503.32252157913,1509.35822987444,1515.39393816975,1521.42964646506,1527.46535476037,1533.50106305569,1539.536771351,1545.57247964631,1551.60818794162,1557.64389623693,1563.67960453224,1569.71531282756,1575.75102112287,1581.78672941818,1587.82243771349,1593.8581460088,1599.89385430412,1605.92956259943,1611.96527089474,1618.00097919005,1624.03668748536,1630.07239578067,1636.10810407599,1642.1438123713,1648.17952066661,1654.21522896192,1660.25093725723,1666.28664555254,1672.32235384786,1678.35806214317,1684.39377043848,1690.42947873379,1696.4651870291,1702.50089532441,1708.53660361973,1714.57231191504,1720.60802021035,1726.64372850566,1732.67943680097,1738.71514509628,1744.7508533916,1750.78656168691,1756.82226998222,1762.85797827753,1768.89368657284,1774.92939486816,1780.96510316347,1787.00081145878,1793.03651975409,1799.0722280494,1805.10793634471,1811.14364464003,1817.17935293534,1823.21506123065,1829.25076952596,1835.28647782127,1841.32218611658,1847.3578944119,1847.3578944119,1847.3578944119,1841.32218611658,1835.28647782127,1829.25076952596,1823.21506123065,1817.17935293534,1811.14364464003,1805.10793634471,1799.0722280494,1793.03651975409,1787.00081145878,1780.96510316347,1774.92939486816,1768.89368657284,1762.85797827753,1756.82226998222,1750.78656168691,1744.7508533916,1738.71514509628,1732.67943680097,1726.64372850566,1720.60802021035,1714.57231191504,1708.53660361973,1702.50089532441,1696.4651870291,1690.42947873379,1684.39377043848,1678.35806214317,1672.32235384786,1666.28664555254,1660.25093725723,1654.21522896192,1648.17952066661,1642.1438123713,1636.10810407599,1630.07239578067,1624.03668748536,1618.00097919005,1611.96527089474,1605.92956259943,1599.89385430412,1593.8581460088,1587.82243771349,1581.78672941818,1575.75102112287,1569.71531282756,1563.67960453224,1557.64389623693,1551.60818794162,1545.57247964631,1539.536771351,1533.50106305569,1527.46535476037,1521.42964646506,1515.39393816975,1509.35822987444,1503.32252157913,1497.28681328382,1491.2511049885,1485.21539669319,1479.17968839788,1473.14398010257,1467.10827180726,1461.07256351194,1455.03685521663,1449.00114692132,1442.96543862601,1436.9297303307,1430.89402203539,1424.85831374007,1418.82260544476,1412.78689714945,1406.75118885414,1400.71548055883,1394.67977226352,1388.6440639682,1382.60835567289,1376.57264737758,1370.53693908227,1364.50123078696,1358.46552249165,1352.42981419633,1346.39410590102,1340.35839760571,1334.3226893104,1328.28698101509,1322.25127271978,1316.21556442446,1310.17985612915,1304.14414783384,1298.10843953853,1292.07273124322,1286.0370229479,1280.00131465259,1273.96560635728,1267.92989806197,1261.89418976666,1255.85848147135,1249.82277317603,1243.78706488072,1237.75135658541,1231.7156482901,1225.67993999479,1219.64423169948,1213.60852340416,1207.57281510885,1201.53710681354,1195.50139851823,1189.46569022292,1183.42998192761,1177.39427363229,1171.35856533698,1165.32285704167,1159.28714874636,1153.25144045105,1147.21573215573,1141.18002386042,1135.14431556511,1129.1086072698,1123.07289897449,1117.03719067918,1111.00148238386,1104.96577408855,1098.93006579324,1092.89435749793,1086.85864920262,1080.82294090731,1074.78723261199,1068.75152431668,1062.71581602137,1056.68010772606,1050.64439943075,1044.60869113544,1038.57298284012,1032.53727454481,1026.5015662495,1020.46585795419,1014.43014965888,1008.39444136356,1002.35873306825,996.323024772941,990.287316477629,984.251608182318,978.215899887006,972.180191591694,966.144483296383,960.108775001071,954.073066705759,948.037358410447,942.001650115136,935.965941819824,929.930233524512,923.8945252292,917.858816933889,911.823108638577,905.787400343266,899.751692047954,893.715983752642,887.68027545733,881.644567162018,875.608858866706,869.573150571395,863.537442276083,857.501733980772,851.46602568546,845.430317390148,839.394609094836,833.358900799524,827.323192504213,821.287484208901,815.251775913589,809.216067618278,803.180359322966,797.144651027654,791.108942732342,785.073234437031,779.037526141719,773.001817846407,766.966109551095,760.930401255784,754.894692960472,748.85898466516,742.823276369848,736.787568074537,730.751859779225,724.716151483913,718.680443188601,712.64473489329,706.609026597978,700.573318302666,694.537610007354,688.501901712043,682.466193416731,676.430485121419,670.394776826108,664.359068530796,658.323360235484,652.287651940172,646.251943644861,640.216235349549,634.180527054237,628.144818758925,622.109110463614,616.073402168302,610.03769387299,604.001985577678,597.966277282367,591.930568987055,585.894860691743,579.859152396431,573.82344410112,567.787735805808,561.752027510496,555.716319215185,549.680610919873,543.644902624561,537.609194329249,531.573486033938,525.537777738626,519.502069443314,513.466361148003,507.430652852691,501.394944557379,495.359236262067,489.323527966756,483.287819671444,477.252111376132,471.21640308082,465.180694785509,459.144986490197,453.109278194885,447.073569899573,441.037861604262,435.00215330895,428.966445013638,422.930736718326,416.895028423015,410.859320127703,404.823611832391,398.78790353708,392.752195241768,386.716486946456,380.680778651144,374.645070355833,368.609362060521,362.573653765209,356.537945469897,350.502237174586,344.466528879274,338.430820583962,332.39511228865,326.359403993339,320.323695698027,314.287987402715,308.252279107403,302.216570812092,296.18086251678,290.145154221468,284.109445926156,278.073737630845,272.038029335533,266.002321040221,259.96661274491,253.930904449598,247.895196154286,241.859487858974,235.823779563663,229.788071268351,223.752362973039,217.716654677727,211.680946382416,205.645238087104,199.609529791792,193.57382149648,187.538113201169,181.502404905857,175.466696610545,169.430988315233,163.395280019922,157.35957172461,151.323863429298,145.288155133986,139.252446838675,133.216738543363,127.181030248051,121.14532195274,115.109613657428,109.073905362116,103.038197066804,97.0024887714926,90.9667804761809,84.9310721808693,78.8953638855573,72.8596555902457,66.823947294934,60.7882389996223,54.7525307043104,48.7168224089987,42.681114113687,36.6454058183754,30.6096975230635,24.5739892277518,18.5382809324401,12.5025726371284,6.46686434181652,0.431156046504839,-5.60455224880684,-11.6402605441185,-17.6759688394304,-23.7116771347421,-29.7473854300538,-35.7830937253655,-41.8188020206774,-47.854510315989,-53.8902186113007,-59.9259269066124,-65.9616352019243,-71.997343497236,-78.0330517925477,-84.0687600878596,-90.1044683831713,-96.1401766784829,-102.175884973795,-108.211593269107,-114.247301564418,-120.28300985973,-126.318718155042,-132.354426450353,-138.390134745665,-144.425843040977,-150.461551336288,-156.4972596316,-162.532967926912,-168.568676222224,-174.604384517535,-180.640092812847,-186.675801108159,-192.711509403471,-198.747217698782,-204.782925994094,-210.818634289406,-216.854342584718,-222.890050880029,-228.925759175341,-234.961467470653,-240.997175765965,-247.032884061276,-253.068592356588,-259.1043006519,-265.140008947212,-271.175717242523,-277.211425537835,-283.247133833147,-289.282842128458,-295.31855042377,-301.354258719082,-307.389967014394,-313.425675309705,-319.461383605017,-325.497091900329,-331.532800195641,-337.568508490952,-343.604216786264,-349.639925081576,-355.675633376888,-361.711341672199,-367.747049967511,-373.782758262823,-379.818466558135,-385.854174853446,-391.889883148758,-397.92559144407,-403.961299739382,-409.997008034693,-416.032716330005,-422.068424625317,-428.104132920628,-434.13984121594,-440.175549511252,-446.211257806564,-452.246966101875,-458.282674397187,-464.318382692499,-470.354090987811,-476.389799283122,-482.425507578434,-488.461215873746,-494.496924169058,-500.532632464369,-506.568340759681,-512.604049054993,-518.639757350305,-524.675465645616,-530.711173940928,-536.74688223624,-542.782590531551,-548.818298826863,-554.854007122175,-560.889715417487,-566.925423712798,-572.96113200811,-578.996840303422,-585.032548598734,-591.068256894045,-597.103965189357,-603.139673484669,-609.175381779981,-615.211090075292,-621.246798370604,-627.282506665916,-633.318214961227,-639.353923256539,-645.389631551851,-651.425339847163,-657.461048142474,-663.496756437786,-669.532464733098,-675.56817302841,-681.603881323721,-687.639589619033,-693.675297914345,-699.711006209657,-705.746714504968,-711.78242280028,-717.818131095592,-723.853839390904,-729.889547686215,-735.925255981527,-741.960964276839,-747.99667257215,-754.032380867462,-760.068089162774,-766.103797458086,-772.139505753397,-778.175214048709,-784.210922344021,-790.246630639333,-796.282338934644,-802.318047229956,-808.353755525268,-814.38946382058,-820.425172115891,-826.460880411203,-832.496588706515,-838.532297001826,-844.568005297138,-850.60371359245,-856.639421887762,-862.675130183073,-868.710838478385,-874.746546773697,-880.782255069009,-886.81796336432,-892.853671659632,-898.889379954944,-904.925088250256,-910.960796545567,-916.996504840879,-923.032213136191,-929.067921431503,-935.103629726814,-941.139338022126,-947.175046317438,-953.210754612749,-959.246462908061,-965.282171203373,-971.317879498685,-977.353587793996,-983.389296089308,-989.42500438462,-995.460712679932,-1001.49642097524,-1007.53212927056,-1013.56783756587,-1019.60354586118,-1025.63925415649,-1031.6749624518,-1037.71067074711,-1043.74637904243,-1049.78208733774,-1055.81779563305,-1061.85350392836,-1067.88921222367,-1073.92492051898,-1079.9606288143,-1085.99633710961,-1092.03204540492,-1098.06775370023,-1104.10346199554,-1110.13917029085,-1116.17487858617,-1122.21058688148,-1128.24629517679,-1134.2820034721,-1140.31771176741,-1146.35342006273,-1152.38912835804,-1158.42483665335,-1164.46054494866,-1170.49625324397,-1176.53196153928,-1182.5676698346,-1188.60337812991,-1194.63908642522,-1200.67479472053,-1206.71050301584,-1212.74621131115,-1218.78191960647,-1224.81762790178,-1230.85333619709,-1236.8890444924,-1236.8890444924],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.627170972909221,0.630736115726362,0.634293072713835,0.637783926412643,0.641274780111452,0.644703962932618,0.648119153711134,0.651494400745573,0.654832770596749,0.658153655928557,0.661414262999502,0.664674870070446,0.667862503540604,0.671044621543798,0.674176604154624,0.67727972084294,0.680355914527837,0.683379729250956,0.686400020055868,0.689344440421397,0.692288860786925,0.695173876992643,0.698039014660672,0.700868286763303,0.703654452408483,0.706428138437235,0.709135835917404,0.711843533397573,0.714484036721661,0.717113956496808,0.719700130210184,0.722253142059878,0.724785389480518,0.72726253457194,0.729739679663363,0.732143761213295,0.734546243575629,0.736898624506795,0.739227801970835,0.741529094708738,0.743786469373336,0.746037301957562,0.748224510246718,0.750411718535874,0.752544330570775,0.754663132946912,0.75674846927175,0.758800739671926,0.760839589521307,0.762827304583809,0.764815019646311,0.766745698068618,0.768670926217566,0.770558886033804,0.772423776489022,0.774269920898668,0.77607669267723,0.777881932582236,0.779632863556704,0.781383794531172,0.783095535580455,0.784792951656948,0.786468004089956,0.788114268566853,0.789753604628068,0.791351107793326,0.792948610958584,0.794506853390552,0.796058002412378,0.797584902839559,0.799092112001549,0.800588659146045,0.802054340473889,0.803520021801733,0.80494807746443,0.80637463178694,0.807776694484484,0.809166502967076,0.810543538170808,0.811898954354686,0.813251923802119,0.814575266177323,0.815898608552528,0.817198674225798,0.818492219361032,0.819772364502941,0.821038340749722,0.822299468175878,0.823540049959135,0.824780631742392,0.826000359536955,0.827217662238757,0.828422213502069,0.829618288939736,0.8308084838191,0.831985316313637,0.833161967182416,0.834321470208,0.835480973233583,0.836629395509131,0.837773408909328,0.838911654090837,0.840041941842468,0.841170723047382,0.842288971548354,0.843407220049327,0.844516810059188,0.845624626912401,0.846727681175733,0.847826594437261,0.848923719954028,0.850015177810262,0.851106635666496,0.852192343254485,0.853277714089786,0.854359968312149,0.855440541125914,0.856519842294903,0.857596827424711,0.85867366689196,0.859748196997734,0.860822727103507,0.861896188188923,0.862969319451817,0.864042254735802,0.865114968224769,0.866187766910384,0.867260970069187,0.86833417322799,0.869408540936638,0.870483069144333,0.871558817378273,0.872635435537034,0.873712857841059,0.874792261946493,0.875871666051927,0.876954462606033,0.878037281256958,0.879122877608474,0.880209673415392,0.881298274004232,0.882389544858459,0.883481346822577,0.884577526991134,0.885673707159691,0.886774181022294,0.887875642037219,0.888979998572839,0.890087049887457,0.891195402156429,0.892308291545363,0.893421180934297,0.89453965921137,0.895658572895558,0.896781376371034,0.897906438853767,0.899033597560068,0.900164871164977,0.901296407972903,0.902433892079893,0.903571376186883,0.90471345289283,0.905857082873105,0.907003432935737,0.908153078792259,0.909303638419541,0.910459103144416,0.911614567869291,0.912774818790161,0.91393583645557,0.915099810547229,0.916266044151473,0.917433583365893,0.918604622460951,0.919775661556008,0.920950918520588,0.922126276652135,0.923304192897829,0.924483304892986,0.925663617324504,0.926845836466541,0.92802827237498,0.929212867507827,0.930397462640674,0.931583297706712,0.93276945031531,0.933955927927476,0.935142729242758,0.936329457851805,0.937515906031367,0.938702354210928,0.939887481094043,0.941072478531311,0.942255846555376,0.943438197371301,0.944619284272013,0.945797692043974,0.94697596397883,0.94814902976152,0.94932209554421,0.950490162266157,0.951656382958723,0.952818924665761,0.95397669177838,0.955133034486202,0.956280633357603,0.957428232229006,0.95856570046771,0.959701309982898,0.960829267366142,0.961950960220751,0.963068590163337,0.964174333717679,0.965280077272021,0.966368465259223,0.967456123021186,0.968530275297629,0.96959760906919,0.970656571967184,0.971701244684447,0.972744053465083,0.973763632756455,0.974783212047827,0.975781273286189,0.976773235767652,0.977750567269323,0.97871230358977,0.979667820974054,0.98059664161736,0.981525462260667,0.982422392459777,0.983315534303782,0.98418562105718,0.985040254683082,0.985882320894662,0.986695558633538,0.987508420602778,0.988277325299113,0.989046229995447,0.989781385407088,0.990502979872364,0.99120352929021,0.991874806412898,0.992539515248095,0.993157448719756,0.993775382191417,0.994346633850609,0.994908189461445,0.995438086350575,0.995940233801624,0.996427538041713,0.996867263215089,0.997306988388465,0.997685055679541,0.998059373302689,0.998389429736581,0.998695396371538,0.99897625522557,0.999210982509711,0.999441464144938,0.999602132205742,0.999762800266547,0.99986493258693,0.999948803549877,0.999995568494041,1,0.999990333546238,0.999912792192549,0.999835250838859,0.999683715794725,0.999521790412718,0.9993094503004,0.999060864981316,0.998786829608006,0.998449456389209,0.998112083170411,0.997684723545011,0.997256594668994,0.996764012110676,0.996243331770754,0.995684863887821,0.995070019376395,0.994445025882117,0.993734598060199,0.993024170238281,0.992235232036499,0.991428005046546,0.990570316691243,0.989665286839474,0.988738485662729,0.987734869401752,0.986731253140775,0.985635858403131,0.984533099351,0.983367616506582,0.982165391444215,0.980929767923312,0.97962799171852,0.978322202403052,0.976921031816199,0.975519861229347,0.974044914980928,0.972544751314379,0.971000317694986,0.969401808255992,0.967788190690691,0.966092229045138,0.964396267399584,0.962617484332175,0.96082520933456,0.958979316937035,0.957092110530178,0.955179739714662,0.953199223500815,0.951218707286968,0.949149063369144,0.947077093962402,0.944944403241435,0.942783066419463,0.940588268963196,0.938339872644332,0.936083933261842,0.93375099934147,0.931418065421098,0.929020164370257,0.926605419457925,0.92415130862951,0.92165767373558,0.919148587402448,0.916579166629958,0.914009745857467,0.911374429478234,0.908732497782172,0.906048176016681,0.903337166098916,0.9006052913887,0.897828779802315,0.895050820740084,0.89221251328189,0.889374205823696,0.886493673892738,0.883597390367081,0.880677689234174,0.877727347674977,0.874770094156674,0.871769694597787,0.8687692950389,0.865730125403832,0.862683733290242,0.85961439803912,0.856526127373425,0.853428320272437,0.850302316592108,0.847176312911779,0.844018159029089,0.840858582310559,0.837679517550506,0.834490525135897,0.831292245856819,0.828077975498309,0.824862174602234,0.82162672771754,0.818391280832845,0.815142522834863,0.811889948065245,0.808631035326763,0.805365312486219,0.80209786003974,0.798822884540805,0.795547909041869,0.792268067203998,0.788987635041764,0.785706144053772,0.782423937425456,0.779142267699288,0.775861841331906,0.772581448580886,0.76930621683448,0.766030985088075,0.762761768967243,0.759494993775556,0.756233028213776,0.752977807918457,0.749724341470643,0.74648360059898,0.743242859727317,0.740016343527903,0.736792823557744,0.733579800019914,0.730376051168988,0.727177518513375,0.723995892583623,0.720814266653871,0.717655470942809,0.714498115243678,0.711357678233874,0.708226530601856,0.705105171596519,0.702001956521586,0.698900370209733,0.695826596072732,0.692752821935732,0.689702410180547,0.686659367599195,0.683631116781717,0.680619878056539,0.677614190554752,0.674635610200216,0.67165702984568,0.66870757956232,0.665762295902592,0.662836562224398,0.659925000027526,0.657023097262895,0.654145471381973,0.651267845501051,0.648423809707942,0.645580129692824,0.642759889752904,0.639949965391425,0.637153366164102,0.634376813907503,0.631603678841438,0.62885992906484,0.626116179288242,0.623398363783584,0.62068666882883,0.617990977045153,0.615310419990309,0.612636441225427,0.609985945322516,0.607335449419605,0.604711588503312,0.602089927223621,0.599485532913733,0.596891340538153,0.594305810477629,0.591737593168064,0.589170310751097,0.586626458037897,0.584082605324697,0.581555586866697,0.579034383486438,0.576522519996991,0.574022158014639,0.571524696362834,0.569043287512399,0.566561878661963,0.564095051759987,0.561630639938249,0.559174662003717,0.556725235852364,0.554279271274169,0.55184277678678,0.549406282299391,0.546980337810889,0.544554690902623,0.542134968916134,0.539718067895647,0.537303705177496,0.534893443062561,0.532483576828098,0.530077853448591,0.527672130069083,0.525268353067826,0.522865086902142,0.520462022445665,0.518059162139253,0.515655977984741,0.513251513502205,0.510847049019668,0.508439354508129,0.506031327873314,0.503619962993418,0.501206478574962,0.498790689386834,0.49636992368987,0.493948964722856,0.491519175650847,0.489089386578839,0.486651843940146,0.484211379494198,0.481765641963755,0.479312948350199,0.476858393611873,0.474392022732377,0.47192565185288,0.469446655801047,0.466965272103002,0.464475029546077,0.461977416214533,0.459475459692505,0.456960524168414,0.454445588644324,0.451913179066395,0.4493799578794,0.446834110633465,0.44428177354091,0.441722198354641,0.439150051781881,0.436576474245233,0.433983964025623,0.431391453806013,0.428782838669692,0.426169552085918,0.423546162425762,0.420911829557322,0.41827358129709,0.415618075681798,0.412962570066505,0.410288239815416,0.407611578435796,0],"text":["npv_sim: -1236.889044<br />scaled: 0.4076116<br />1/2: 0.5","npv_sim: -1230.853336<br />scaled: 0.4102882<br />1/2: 0.5","npv_sim: -1224.817628<br />scaled: 0.4129626<br />1/2: 0.5","npv_sim: -1218.781920<br />scaled: 0.4156181<br />1/2: 0.5","npv_sim: -1212.746211<br />scaled: 0.4182736<br />1/2: 0.5","npv_sim: -1206.710503<br />scaled: 0.4209118<br />1/2: 0.5","npv_sim: -1200.674795<br />scaled: 0.4235462<br />1/2: 0.5","npv_sim: -1194.639086<br />scaled: 0.4261696<br />1/2: 0.5","npv_sim: -1188.603378<br />scaled: 0.4287828<br />1/2: 0.5","npv_sim: -1182.567670<br />scaled: 0.4313915<br />1/2: 0.5","npv_sim: -1176.531962<br />scaled: 0.4339840<br />1/2: 0.5","npv_sim: -1170.496253<br />scaled: 0.4365765<br />1/2: 0.5","npv_sim: -1164.460545<br />scaled: 0.4391501<br />1/2: 0.5","npv_sim: -1158.424837<br />scaled: 0.4417222<br />1/2: 0.5","npv_sim: -1152.389128<br />scaled: 0.4442818<br />1/2: 0.5","npv_sim: -1146.353420<br />scaled: 0.4468341<br />1/2: 0.5","npv_sim: -1140.317712<br />scaled: 0.4493800<br />1/2: 0.5","npv_sim: -1134.282003<br />scaled: 0.4519132<br />1/2: 0.5","npv_sim: -1128.246295<br />scaled: 0.4544456<br />1/2: 0.5","npv_sim: -1122.210587<br />scaled: 0.4569605<br />1/2: 0.5","npv_sim: -1116.174879<br />scaled: 0.4594755<br />1/2: 0.5","npv_sim: -1110.139170<br />scaled: 0.4619774<br />1/2: 0.5","npv_sim: -1104.103462<br />scaled: 0.4644750<br />1/2: 0.5","npv_sim: -1098.067754<br />scaled: 0.4669653<br />1/2: 0.5","npv_sim: -1092.032045<br />scaled: 0.4694467<br />1/2: 0.5","npv_sim: -1085.996337<br />scaled: 0.4719257<br />1/2: 0.5","npv_sim: -1079.960629<br />scaled: 0.4743920<br />1/2: 0.5","npv_sim: -1073.924921<br />scaled: 0.4768584<br />1/2: 0.5","npv_sim: -1067.889212<br />scaled: 0.4793129<br />1/2: 0.5","npv_sim: -1061.853504<br />scaled: 0.4817656<br />1/2: 0.5","npv_sim: -1055.817796<br />scaled: 0.4842114<br />1/2: 0.5","npv_sim: -1049.782087<br />scaled: 0.4866518<br />1/2: 0.5","npv_sim: -1043.746379<br />scaled: 0.4890894<br />1/2: 0.5","npv_sim: -1037.710671<br />scaled: 0.4915192<br />1/2: 0.5","npv_sim: -1031.674962<br />scaled: 0.4939490<br />1/2: 0.5","npv_sim: -1025.639254<br />scaled: 0.4963699<br />1/2: 0.5","npv_sim: -1019.603546<br />scaled: 0.4987907<br />1/2: 0.5","npv_sim: -1013.567838<br />scaled: 0.5012065<br />1/2: 0.5","npv_sim: -1007.532129<br />scaled: 0.5036200<br />1/2: 0.5","npv_sim: -1001.496421<br />scaled: 0.5060313<br />1/2: 0.5","npv_sim:  -995.460713<br />scaled: 0.5084394<br />1/2: 0.5","npv_sim:  -989.425004<br />scaled: 0.5108470<br />1/2: 0.5","npv_sim:  -983.389296<br />scaled: 0.5132515<br />1/2: 0.5","npv_sim:  -977.353588<br />scaled: 0.5156560<br />1/2: 0.5","npv_sim:  -971.317879<br />scaled: 0.5180592<br />1/2: 0.5","npv_sim:  -965.282171<br />scaled: 0.5204620<br />1/2: 0.5","npv_sim:  -959.246463<br />scaled: 0.5228651<br />1/2: 0.5","npv_sim:  -953.210755<br />scaled: 0.5252684<br />1/2: 0.5","npv_sim:  -947.175046<br />scaled: 0.5276721<br />1/2: 0.5","npv_sim:  -941.139338<br />scaled: 0.5300779<br />1/2: 0.5","npv_sim:  -935.103630<br />scaled: 0.5324836<br />1/2: 0.5","npv_sim:  -929.067921<br />scaled: 0.5348934<br />1/2: 0.5","npv_sim:  -923.032213<br />scaled: 0.5373037<br />1/2: 0.5","npv_sim:  -916.996505<br />scaled: 0.5397181<br />1/2: 0.5","npv_sim:  -910.960797<br />scaled: 0.5421350<br />1/2: 0.5","npv_sim:  -904.925088<br />scaled: 0.5445547<br />1/2: 0.5","npv_sim:  -898.889380<br />scaled: 0.5469803<br />1/2: 0.5","npv_sim:  -892.853672<br />scaled: 0.5494063<br />1/2: 0.5","npv_sim:  -886.817963<br />scaled: 0.5518428<br />1/2: 0.5","npv_sim:  -880.782255<br />scaled: 0.5542793<br />1/2: 0.5","npv_sim:  -874.746547<br />scaled: 0.5567252<br />1/2: 0.5","npv_sim:  -868.710838<br />scaled: 0.5591747<br />1/2: 0.5","npv_sim:  -862.675130<br />scaled: 0.5616306<br />1/2: 0.5","npv_sim:  -856.639422<br />scaled: 0.5640951<br />1/2: 0.5","npv_sim:  -850.603714<br />scaled: 0.5665619<br />1/2: 0.5","npv_sim:  -844.568005<br />scaled: 0.5690433<br />1/2: 0.5","npv_sim:  -838.532297<br />scaled: 0.5715247<br />1/2: 0.5","npv_sim:  -832.496589<br />scaled: 0.5740222<br />1/2: 0.5","npv_sim:  -826.460880<br />scaled: 0.5765225<br />1/2: 0.5","npv_sim:  -820.425172<br />scaled: 0.5790344<br />1/2: 0.5","npv_sim:  -814.389464<br />scaled: 0.5815556<br />1/2: 0.5","npv_sim:  -808.353756<br />scaled: 0.5840826<br />1/2: 0.5","npv_sim:  -802.318047<br />scaled: 0.5866265<br />1/2: 0.5","npv_sim:  -796.282339<br />scaled: 0.5891703<br />1/2: 0.5","npv_sim:  -790.246631<br />scaled: 0.5917376<br />1/2: 0.5","npv_sim:  -784.210922<br />scaled: 0.5943058<br />1/2: 0.5","npv_sim:  -778.175214<br />scaled: 0.5968913<br />1/2: 0.5","npv_sim:  -772.139506<br />scaled: 0.5994855<br />1/2: 0.5","npv_sim:  -766.103797<br />scaled: 0.6020899<br />1/2: 0.5","npv_sim:  -760.068089<br />scaled: 0.6047116<br />1/2: 0.5","npv_sim:  -754.032381<br />scaled: 0.6073354<br />1/2: 0.5","npv_sim:  -747.996673<br />scaled: 0.6099859<br />1/2: 0.5","npv_sim:  -741.960964<br />scaled: 0.6126364<br />1/2: 0.5","npv_sim:  -735.925256<br />scaled: 0.6153104<br />1/2: 0.5","npv_sim:  -729.889548<br />scaled: 0.6179910<br />1/2: 0.5","npv_sim:  -723.853839<br />scaled: 0.6206867<br />1/2: 0.5","npv_sim:  -717.818131<br />scaled: 0.6233984<br />1/2: 0.5","npv_sim:  -711.782423<br />scaled: 0.6261162<br />1/2: 0.5","npv_sim:  -705.746715<br />scaled: 0.6288599<br />1/2: 0.5","npv_sim:  -699.711006<br />scaled: 0.6316037<br />1/2: 0.5","npv_sim:  -693.675298<br />scaled: 0.6343768<br />1/2: 0.5","npv_sim:  -687.639590<br />scaled: 0.6371534<br />1/2: 0.5","npv_sim:  -681.603881<br />scaled: 0.6399500<br />1/2: 0.5","npv_sim:  -675.568173<br />scaled: 0.6427599<br />1/2: 0.5","npv_sim:  -669.532465<br />scaled: 0.6455801<br />1/2: 0.5","npv_sim:  -663.496756<br />scaled: 0.6484238<br />1/2: 0.5","npv_sim:  -657.461048<br />scaled: 0.6512678<br />1/2: 0.5","npv_sim:  -651.425340<br />scaled: 0.6541455<br />1/2: 0.5","npv_sim:  -645.389632<br />scaled: 0.6570231<br />1/2: 0.5","npv_sim:  -639.353923<br />scaled: 0.6599250<br />1/2: 0.5","npv_sim:  -633.318215<br />scaled: 0.6628366<br />1/2: 0.5","npv_sim:  -627.282507<br />scaled: 0.6657623<br />1/2: 0.5","npv_sim:  -621.246798<br />scaled: 0.6687076<br />1/2: 0.5","npv_sim:  -615.211090<br />scaled: 0.6716570<br />1/2: 0.5","npv_sim:  -609.175382<br />scaled: 0.6746356<br />1/2: 0.5","npv_sim:  -603.139673<br />scaled: 0.6776142<br />1/2: 0.5","npv_sim:  -597.103965<br />scaled: 0.6806199<br />1/2: 0.5","npv_sim:  -591.068257<br />scaled: 0.6836311<br />1/2: 0.5","npv_sim:  -585.032549<br />scaled: 0.6866594<br />1/2: 0.5","npv_sim:  -578.996840<br />scaled: 0.6897024<br />1/2: 0.5","npv_sim:  -572.961132<br />scaled: 0.6927528<br />1/2: 0.5","npv_sim:  -566.925424<br />scaled: 0.6958266<br />1/2: 0.5","npv_sim:  -560.889715<br />scaled: 0.6989004<br />1/2: 0.5","npv_sim:  -554.854007<br />scaled: 0.7020020<br />1/2: 0.5","npv_sim:  -548.818299<br />scaled: 0.7051052<br />1/2: 0.5","npv_sim:  -542.782591<br />scaled: 0.7082265<br />1/2: 0.5","npv_sim:  -536.746882<br />scaled: 0.7113577<br />1/2: 0.5","npv_sim:  -530.711174<br />scaled: 0.7144981<br />1/2: 0.5","npv_sim:  -524.675466<br />scaled: 0.7176555<br />1/2: 0.5","npv_sim:  -518.639757<br />scaled: 0.7208143<br />1/2: 0.5","npv_sim:  -512.604049<br />scaled: 0.7239959<br />1/2: 0.5","npv_sim:  -506.568341<br />scaled: 0.7271775<br />1/2: 0.5","npv_sim:  -500.532632<br />scaled: 0.7303761<br />1/2: 0.5","npv_sim:  -494.496924<br />scaled: 0.7335798<br />1/2: 0.5","npv_sim:  -488.461216<br />scaled: 0.7367928<br />1/2: 0.5","npv_sim:  -482.425508<br />scaled: 0.7400163<br />1/2: 0.5","npv_sim:  -476.389799<br />scaled: 0.7432429<br />1/2: 0.5","npv_sim:  -470.354091<br />scaled: 0.7464836<br />1/2: 0.5","npv_sim:  -464.318383<br />scaled: 0.7497243<br />1/2: 0.5","npv_sim:  -458.282674<br />scaled: 0.7529778<br />1/2: 0.5","npv_sim:  -452.246966<br />scaled: 0.7562330<br />1/2: 0.5","npv_sim:  -446.211258<br />scaled: 0.7594950<br />1/2: 0.5","npv_sim:  -440.175550<br />scaled: 0.7627618<br />1/2: 0.5","npv_sim:  -434.139841<br />scaled: 0.7660310<br />1/2: 0.5","npv_sim:  -428.104133<br />scaled: 0.7693062<br />1/2: 0.5","npv_sim:  -422.068425<br />scaled: 0.7725814<br />1/2: 0.5","npv_sim:  -416.032716<br />scaled: 0.7758618<br />1/2: 0.5","npv_sim:  -409.997008<br />scaled: 0.7791423<br />1/2: 0.5","npv_sim:  -403.961300<br />scaled: 0.7824239<br />1/2: 0.5","npv_sim:  -397.925591<br />scaled: 0.7857061<br />1/2: 0.5","npv_sim:  -391.889883<br />scaled: 0.7889876<br />1/2: 0.5","npv_sim:  -385.854175<br />scaled: 0.7922681<br />1/2: 0.5","npv_sim:  -379.818467<br />scaled: 0.7955479<br />1/2: 0.5","npv_sim:  -373.782758<br />scaled: 0.7988229<br />1/2: 0.5","npv_sim:  -367.747050<br />scaled: 0.8020979<br />1/2: 0.5","npv_sim:  -361.711342<br />scaled: 0.8053653<br />1/2: 0.5","npv_sim:  -355.675633<br />scaled: 0.8086310<br />1/2: 0.5","npv_sim:  -349.639925<br />scaled: 0.8118899<br />1/2: 0.5","npv_sim:  -343.604217<br />scaled: 0.8151425<br />1/2: 0.5","npv_sim:  -337.568508<br />scaled: 0.8183913<br />1/2: 0.5","npv_sim:  -331.532800<br />scaled: 0.8216267<br />1/2: 0.5","npv_sim:  -325.497092<br />scaled: 0.8248622<br />1/2: 0.5","npv_sim:  -319.461384<br />scaled: 0.8280780<br />1/2: 0.5","npv_sim:  -313.425675<br />scaled: 0.8312922<br />1/2: 0.5","npv_sim:  -307.389967<br />scaled: 0.8344905<br />1/2: 0.5","npv_sim:  -301.354259<br />scaled: 0.8376795<br />1/2: 0.5","npv_sim:  -295.318550<br />scaled: 0.8408586<br />1/2: 0.5","npv_sim:  -289.282842<br />scaled: 0.8440182<br />1/2: 0.5","npv_sim:  -283.247134<br />scaled: 0.8471763<br />1/2: 0.5","npv_sim:  -277.211426<br />scaled: 0.8503023<br />1/2: 0.5","npv_sim:  -271.175717<br />scaled: 0.8534283<br />1/2: 0.5","npv_sim:  -265.140009<br />scaled: 0.8565261<br />1/2: 0.5","npv_sim:  -259.104301<br />scaled: 0.8596144<br />1/2: 0.5","npv_sim:  -253.068592<br />scaled: 0.8626837<br />1/2: 0.5","npv_sim:  -247.032884<br />scaled: 0.8657301<br />1/2: 0.5","npv_sim:  -240.997176<br />scaled: 0.8687693<br />1/2: 0.5","npv_sim:  -234.961467<br />scaled: 0.8717697<br />1/2: 0.5","npv_sim:  -228.925759<br />scaled: 0.8747701<br />1/2: 0.5","npv_sim:  -222.890051<br />scaled: 0.8777273<br />1/2: 0.5","npv_sim:  -216.854343<br />scaled: 0.8806777<br />1/2: 0.5","npv_sim:  -210.818634<br />scaled: 0.8835974<br />1/2: 0.5","npv_sim:  -204.782926<br />scaled: 0.8864937<br />1/2: 0.5","npv_sim:  -198.747218<br />scaled: 0.8893742<br />1/2: 0.5","npv_sim:  -192.711509<br />scaled: 0.8922125<br />1/2: 0.5","npv_sim:  -186.675801<br />scaled: 0.8950508<br />1/2: 0.5","npv_sim:  -180.640093<br />scaled: 0.8978288<br />1/2: 0.5","npv_sim:  -174.604385<br />scaled: 0.9006053<br />1/2: 0.5","npv_sim:  -168.568676<br />scaled: 0.9033372<br />1/2: 0.5","npv_sim:  -162.532968<br />scaled: 0.9060482<br />1/2: 0.5","npv_sim:  -156.497260<br />scaled: 0.9087325<br />1/2: 0.5","npv_sim:  -150.461551<br />scaled: 0.9113744<br />1/2: 0.5","npv_sim:  -144.425843<br />scaled: 0.9140097<br />1/2: 0.5","npv_sim:  -138.390135<br />scaled: 0.9165792<br />1/2: 0.5","npv_sim:  -132.354426<br />scaled: 0.9191486<br />1/2: 0.5","npv_sim:  -126.318718<br />scaled: 0.9216577<br />1/2: 0.5","npv_sim:  -120.283010<br />scaled: 0.9241513<br />1/2: 0.5","npv_sim:  -114.247302<br />scaled: 0.9266054<br />1/2: 0.5","npv_sim:  -108.211593<br />scaled: 0.9290202<br />1/2: 0.5","npv_sim:  -102.175885<br />scaled: 0.9314181<br />1/2: 0.5","npv_sim:   -96.140177<br />scaled: 0.9337510<br />1/2: 0.5","npv_sim:   -90.104468<br />scaled: 0.9360839<br />1/2: 0.5","npv_sim:   -84.068760<br />scaled: 0.9383399<br />1/2: 0.5","npv_sim:   -78.033052<br />scaled: 0.9405883<br />1/2: 0.5","npv_sim:   -71.997343<br />scaled: 0.9427831<br />1/2: 0.5","npv_sim:   -65.961635<br />scaled: 0.9449444<br />1/2: 0.5","npv_sim:   -59.925927<br />scaled: 0.9470771<br />1/2: 0.5","npv_sim:   -53.890219<br />scaled: 0.9491491<br />1/2: 0.5","npv_sim:   -47.854510<br />scaled: 0.9512187<br />1/2: 0.5","npv_sim:   -41.818802<br />scaled: 0.9531992<br />1/2: 0.5","npv_sim:   -35.783094<br />scaled: 0.9551797<br />1/2: 0.5","npv_sim:   -29.747385<br />scaled: 0.9570921<br />1/2: 0.5","npv_sim:   -23.711677<br />scaled: 0.9589793<br />1/2: 0.5","npv_sim:   -17.675969<br />scaled: 0.9608252<br />1/2: 0.5","npv_sim:   -11.640261<br />scaled: 0.9626175<br />1/2: 0.5","npv_sim:    -5.604552<br />scaled: 0.9643963<br />1/2: 0.5","npv_sim:     0.431156<br />scaled: 0.9660922<br />1/2: 0.5","npv_sim:     6.466864<br />scaled: 0.9677882<br />1/2: 0.5","npv_sim:    12.502573<br />scaled: 0.9694018<br />1/2: 0.5","npv_sim:    18.538281<br />scaled: 0.9710003<br />1/2: 0.5","npv_sim:    24.573989<br />scaled: 0.9725448<br />1/2: 0.5","npv_sim:    30.609698<br />scaled: 0.9740449<br />1/2: 0.5","npv_sim:    36.645406<br />scaled: 0.9755199<br />1/2: 0.5","npv_sim:    42.681114<br />scaled: 0.9769210<br />1/2: 0.5","npv_sim:    48.716822<br />scaled: 0.9783222<br />1/2: 0.5","npv_sim:    54.752531<br />scaled: 0.9796280<br />1/2: 0.5","npv_sim:    60.788239<br />scaled: 0.9809298<br />1/2: 0.5","npv_sim:    66.823947<br />scaled: 0.9821654<br />1/2: 0.5","npv_sim:    72.859656<br />scaled: 0.9833676<br />1/2: 0.5","npv_sim:    78.895364<br />scaled: 0.9845331<br />1/2: 0.5","npv_sim:    84.931072<br />scaled: 0.9856359<br />1/2: 0.5","npv_sim:    90.966780<br />scaled: 0.9867313<br />1/2: 0.5","npv_sim:    97.002489<br />scaled: 0.9877349<br />1/2: 0.5","npv_sim:   103.038197<br />scaled: 0.9887385<br />1/2: 0.5","npv_sim:   109.073905<br />scaled: 0.9896653<br />1/2: 0.5","npv_sim:   115.109614<br />scaled: 0.9905703<br />1/2: 0.5","npv_sim:   121.145322<br />scaled: 0.9914280<br />1/2: 0.5","npv_sim:   127.181030<br />scaled: 0.9922352<br />1/2: 0.5","npv_sim:   133.216739<br />scaled: 0.9930242<br />1/2: 0.5","npv_sim:   139.252447<br />scaled: 0.9937346<br />1/2: 0.5","npv_sim:   145.288155<br />scaled: 0.9944450<br />1/2: 0.5","npv_sim:   151.323863<br />scaled: 0.9950700<br />1/2: 0.5","npv_sim:   157.359572<br />scaled: 0.9956849<br />1/2: 0.5","npv_sim:   163.395280<br />scaled: 0.9962433<br />1/2: 0.5","npv_sim:   169.430988<br />scaled: 0.9967640<br />1/2: 0.5","npv_sim:   175.466697<br />scaled: 0.9972566<br />1/2: 0.5","npv_sim:   181.502405<br />scaled: 0.9976847<br />1/2: 0.5","npv_sim:   187.538113<br />scaled: 0.9981121<br />1/2: 0.5","npv_sim:   193.573821<br />scaled: 0.9984495<br />1/2: 0.5","npv_sim:   199.609530<br />scaled: 0.9987868<br />1/2: 0.5","npv_sim:   205.645238<br />scaled: 0.9990609<br />1/2: 0.5","npv_sim:   211.680946<br />scaled: 0.9993095<br />1/2: 0.5","npv_sim:   217.716655<br />scaled: 0.9995218<br />1/2: 0.5","npv_sim:   223.752363<br />scaled: 0.9996837<br />1/2: 0.5","npv_sim:   229.788071<br />scaled: 0.9998353<br />1/2: 0.5","npv_sim:   235.823780<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   241.859488<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   247.895196<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   253.930904<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   259.966613<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   266.002321<br />scaled: 0.9998649<br />1/2: 0.5","npv_sim:   272.038029<br />scaled: 0.9997628<br />1/2: 0.5","npv_sim:   278.073738<br />scaled: 0.9996021<br />1/2: 0.5","npv_sim:   284.109446<br />scaled: 0.9994415<br />1/2: 0.5","npv_sim:   290.145154<br />scaled: 0.9992110<br />1/2: 0.5","npv_sim:   296.180863<br />scaled: 0.9989763<br />1/2: 0.5","npv_sim:   302.216571<br />scaled: 0.9986954<br />1/2: 0.5","npv_sim:   308.252279<br />scaled: 0.9983894<br />1/2: 0.5","npv_sim:   314.287987<br />scaled: 0.9980594<br />1/2: 0.5","npv_sim:   320.323696<br />scaled: 0.9976851<br />1/2: 0.5","npv_sim:   326.359404<br />scaled: 0.9973070<br />1/2: 0.5","npv_sim:   332.395112<br />scaled: 0.9968673<br />1/2: 0.5","npv_sim:   338.430821<br />scaled: 0.9964275<br />1/2: 0.5","npv_sim:   344.466529<br />scaled: 0.9959402<br />1/2: 0.5","npv_sim:   350.502237<br />scaled: 0.9954381<br />1/2: 0.5","npv_sim:   356.537945<br />scaled: 0.9949082<br />1/2: 0.5","npv_sim:   362.573654<br />scaled: 0.9943466<br />1/2: 0.5","npv_sim:   368.609362<br />scaled: 0.9937754<br />1/2: 0.5","npv_sim:   374.645070<br />scaled: 0.9931574<br />1/2: 0.5","npv_sim:   380.680779<br />scaled: 0.9925395<br />1/2: 0.5","npv_sim:   386.716487<br />scaled: 0.9918748<br />1/2: 0.5","npv_sim:   392.752195<br />scaled: 0.9912035<br />1/2: 0.5","npv_sim:   398.787904<br />scaled: 0.9905030<br />1/2: 0.5","npv_sim:   404.823612<br />scaled: 0.9897814<br />1/2: 0.5","npv_sim:   410.859320<br />scaled: 0.9890462<br />1/2: 0.5","npv_sim:   416.895028<br />scaled: 0.9882773<br />1/2: 0.5","npv_sim:   422.930737<br />scaled: 0.9875084<br />1/2: 0.5","npv_sim:   428.966445<br />scaled: 0.9866956<br />1/2: 0.5","npv_sim:   435.002153<br />scaled: 0.9858823<br />1/2: 0.5","npv_sim:   441.037862<br />scaled: 0.9850403<br />1/2: 0.5","npv_sim:   447.073570<br />scaled: 0.9841856<br />1/2: 0.5","npv_sim:   453.109278<br />scaled: 0.9833155<br />1/2: 0.5","npv_sim:   459.144986<br />scaled: 0.9824224<br />1/2: 0.5","npv_sim:   465.180695<br />scaled: 0.9815255<br />1/2: 0.5","npv_sim:   471.216403<br />scaled: 0.9805966<br />1/2: 0.5","npv_sim:   477.252111<br />scaled: 0.9796678<br />1/2: 0.5","npv_sim:   483.287820<br />scaled: 0.9787123<br />1/2: 0.5","npv_sim:   489.323528<br />scaled: 0.9777506<br />1/2: 0.5","npv_sim:   495.359236<br />scaled: 0.9767732<br />1/2: 0.5","npv_sim:   501.394945<br />scaled: 0.9757813<br />1/2: 0.5","npv_sim:   507.430653<br />scaled: 0.9747832<br />1/2: 0.5","npv_sim:   513.466361<br />scaled: 0.9737636<br />1/2: 0.5","npv_sim:   519.502069<br />scaled: 0.9727441<br />1/2: 0.5","npv_sim:   525.537778<br />scaled: 0.9717012<br />1/2: 0.5","npv_sim:   531.573486<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   537.609194<br />scaled: 0.9695976<br />1/2: 0.5","npv_sim:   543.644903<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   549.680611<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   555.716319<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   561.752028<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   567.787736<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   573.823444<br />scaled: 0.9630686<br />1/2: 0.5","npv_sim:   579.859152<br />scaled: 0.9619510<br />1/2: 0.5","npv_sim:   585.894861<br />scaled: 0.9608293<br />1/2: 0.5","npv_sim:   591.930569<br />scaled: 0.9597013<br />1/2: 0.5","npv_sim:   597.966277<br />scaled: 0.9585657<br />1/2: 0.5","npv_sim:   604.001986<br />scaled: 0.9574282<br />1/2: 0.5","npv_sim:   610.037694<br />scaled: 0.9562806<br />1/2: 0.5","npv_sim:   616.073402<br />scaled: 0.9551330<br />1/2: 0.5","npv_sim:   622.109110<br />scaled: 0.9539767<br />1/2: 0.5","npv_sim:   628.144819<br />scaled: 0.9528189<br />1/2: 0.5","npv_sim:   634.180527<br />scaled: 0.9516564<br />1/2: 0.5","npv_sim:   640.216235<br />scaled: 0.9504902<br />1/2: 0.5","npv_sim:   646.251944<br />scaled: 0.9493221<br />1/2: 0.5","npv_sim:   652.287652<br />scaled: 0.9481490<br />1/2: 0.5","npv_sim:   658.323360<br />scaled: 0.9469760<br />1/2: 0.5","npv_sim:   664.359069<br />scaled: 0.9457977<br />1/2: 0.5","npv_sim:   670.394777<br />scaled: 0.9446193<br />1/2: 0.5","npv_sim:   676.430485<br />scaled: 0.9434382<br />1/2: 0.5","npv_sim:   682.466193<br />scaled: 0.9422558<br />1/2: 0.5","npv_sim:   688.501902<br />scaled: 0.9410725<br />1/2: 0.5","npv_sim:   694.537610<br />scaled: 0.9398875<br />1/2: 0.5","npv_sim:   700.573318<br />scaled: 0.9387024<br />1/2: 0.5","npv_sim:   706.609027<br />scaled: 0.9375159<br />1/2: 0.5","npv_sim:   712.644735<br />scaled: 0.9363295<br />1/2: 0.5","npv_sim:   718.680443<br />scaled: 0.9351427<br />1/2: 0.5","npv_sim:   724.716151<br />scaled: 0.9339559<br />1/2: 0.5","npv_sim:   730.751860<br />scaled: 0.9327695<br />1/2: 0.5","npv_sim:   736.787568<br />scaled: 0.9315833<br />1/2: 0.5","npv_sim:   742.823276<br />scaled: 0.9303975<br />1/2: 0.5","npv_sim:   748.858985<br />scaled: 0.9292129<br />1/2: 0.5","npv_sim:   754.894693<br />scaled: 0.9280283<br />1/2: 0.5","npv_sim:   760.930401<br />scaled: 0.9268458<br />1/2: 0.5","npv_sim:   766.966110<br />scaled: 0.9256636<br />1/2: 0.5","npv_sim:   773.001818<br />scaled: 0.9244833<br />1/2: 0.5","npv_sim:   779.037526<br />scaled: 0.9233042<br />1/2: 0.5","npv_sim:   785.073234<br />scaled: 0.9221263<br />1/2: 0.5","npv_sim:   791.108943<br />scaled: 0.9209509<br />1/2: 0.5","npv_sim:   797.144651<br />scaled: 0.9197757<br />1/2: 0.5","npv_sim:   803.180359<br />scaled: 0.9186046<br />1/2: 0.5","npv_sim:   809.216068<br />scaled: 0.9174336<br />1/2: 0.5","npv_sim:   815.251776<br />scaled: 0.9162660<br />1/2: 0.5","npv_sim:   821.287484<br />scaled: 0.9150998<br />1/2: 0.5","npv_sim:   827.323193<br />scaled: 0.9139358<br />1/2: 0.5","npv_sim:   833.358901<br />scaled: 0.9127748<br />1/2: 0.5","npv_sim:   839.394609<br />scaled: 0.9116146<br />1/2: 0.5","npv_sim:   845.430317<br />scaled: 0.9104591<br />1/2: 0.5","npv_sim:   851.466026<br />scaled: 0.9093036<br />1/2: 0.5","npv_sim:   857.501734<br />scaled: 0.9081531<br />1/2: 0.5","npv_sim:   863.537442<br />scaled: 0.9070034<br />1/2: 0.5","npv_sim:   869.573151<br />scaled: 0.9058571<br />1/2: 0.5","npv_sim:   875.608859<br />scaled: 0.9047135<br />1/2: 0.5","npv_sim:   881.644567<br />scaled: 0.9035714<br />1/2: 0.5","npv_sim:   887.680275<br />scaled: 0.9024339<br />1/2: 0.5","npv_sim:   893.715984<br />scaled: 0.9012964<br />1/2: 0.5","npv_sim:   899.751692<br />scaled: 0.9001649<br />1/2: 0.5","npv_sim:   905.787400<br />scaled: 0.8990336<br />1/2: 0.5","npv_sim:   911.823109<br />scaled: 0.8979064<br />1/2: 0.5","npv_sim:   917.858817<br />scaled: 0.8967814<br />1/2: 0.5","npv_sim:   923.894525<br />scaled: 0.8956586<br />1/2: 0.5","npv_sim:   929.930234<br />scaled: 0.8945397<br />1/2: 0.5","npv_sim:   935.965942<br />scaled: 0.8934212<br />1/2: 0.5","npv_sim:   942.001650<br />scaled: 0.8923083<br />1/2: 0.5","npv_sim:   948.037358<br />scaled: 0.8911954<br />1/2: 0.5","npv_sim:   954.073067<br />scaled: 0.8900870<br />1/2: 0.5","npv_sim:   960.108775<br />scaled: 0.8889800<br />1/2: 0.5","npv_sim:   966.144483<br />scaled: 0.8878756<br />1/2: 0.5","npv_sim:   972.180192<br />scaled: 0.8867742<br />1/2: 0.5","npv_sim:   978.215900<br />scaled: 0.8856737<br />1/2: 0.5","npv_sim:   984.251608<br />scaled: 0.8845775<br />1/2: 0.5","npv_sim:   990.287316<br />scaled: 0.8834813<br />1/2: 0.5","npv_sim:   996.323025<br />scaled: 0.8823895<br />1/2: 0.5","npv_sim:  1002.358733<br />scaled: 0.8812983<br />1/2: 0.5","npv_sim:  1008.394441<br />scaled: 0.8802097<br />1/2: 0.5","npv_sim:  1014.430150<br />scaled: 0.8791229<br />1/2: 0.5","npv_sim:  1020.465858<br />scaled: 0.8780373<br />1/2: 0.5","npv_sim:  1026.501566<br />scaled: 0.8769545<br />1/2: 0.5","npv_sim:  1032.537275<br />scaled: 0.8758717<br />1/2: 0.5","npv_sim:  1038.572983<br />scaled: 0.8747923<br />1/2: 0.5","npv_sim:  1044.608691<br />scaled: 0.8737129<br />1/2: 0.5","npv_sim:  1050.644399<br />scaled: 0.8726354<br />1/2: 0.5","npv_sim:  1056.680108<br />scaled: 0.8715588<br />1/2: 0.5","npv_sim:  1062.715816<br />scaled: 0.8704831<br />1/2: 0.5","npv_sim:  1068.751524<br />scaled: 0.8694085<br />1/2: 0.5","npv_sim:  1074.787233<br />scaled: 0.8683342<br />1/2: 0.5","npv_sim:  1080.822941<br />scaled: 0.8672610<br />1/2: 0.5","npv_sim:  1086.858649<br />scaled: 0.8661878<br />1/2: 0.5","npv_sim:  1092.894357<br />scaled: 0.8651150<br />1/2: 0.5","npv_sim:  1098.930066<br />scaled: 0.8640423<br />1/2: 0.5","npv_sim:  1104.965774<br />scaled: 0.8629693<br />1/2: 0.5","npv_sim:  1111.001482<br />scaled: 0.8618962<br />1/2: 0.5","npv_sim:  1117.037191<br />scaled: 0.8608227<br />1/2: 0.5","npv_sim:  1123.072899<br />scaled: 0.8597482<br />1/2: 0.5","npv_sim:  1129.108607<br />scaled: 0.8586737<br />1/2: 0.5","npv_sim:  1135.144316<br />scaled: 0.8575968<br />1/2: 0.5","npv_sim:  1141.180024<br />scaled: 0.8565198<br />1/2: 0.5","npv_sim:  1147.215732<br />scaled: 0.8554405<br />1/2: 0.5","npv_sim:  1153.251440<br />scaled: 0.8543600<br />1/2: 0.5","npv_sim:  1159.287149<br />scaled: 0.8532777<br />1/2: 0.5","npv_sim:  1165.322857<br />scaled: 0.8521923<br />1/2: 0.5","npv_sim:  1171.358565<br />scaled: 0.8511066<br />1/2: 0.5","npv_sim:  1177.394274<br />scaled: 0.8500152<br />1/2: 0.5","npv_sim:  1183.429982<br />scaled: 0.8489237<br />1/2: 0.5","npv_sim:  1189.465690<br />scaled: 0.8478266<br />1/2: 0.5","npv_sim:  1195.501399<br />scaled: 0.8467277<br />1/2: 0.5","npv_sim:  1201.537107<br />scaled: 0.8456246<br />1/2: 0.5","npv_sim:  1207.572815<br />scaled: 0.8445168<br />1/2: 0.5","npv_sim:  1213.608523<br />scaled: 0.8434072<br />1/2: 0.5","npv_sim:  1219.644232<br />scaled: 0.8422890<br />1/2: 0.5","npv_sim:  1225.679940<br />scaled: 0.8411707<br />1/2: 0.5","npv_sim:  1231.715648<br />scaled: 0.8400419<br />1/2: 0.5","npv_sim:  1237.751357<br />scaled: 0.8389117<br />1/2: 0.5","npv_sim:  1243.787065<br />scaled: 0.8377734<br />1/2: 0.5","npv_sim:  1249.822773<br />scaled: 0.8366294<br />1/2: 0.5","npv_sim:  1255.858481<br />scaled: 0.8354810<br />1/2: 0.5","npv_sim:  1261.894190<br />scaled: 0.8343215<br />1/2: 0.5","npv_sim:  1267.929898<br />scaled: 0.8331620<br />1/2: 0.5","npv_sim:  1273.965606<br />scaled: 0.8319853<br />1/2: 0.5","npv_sim:  1280.001315<br />scaled: 0.8308085<br />1/2: 0.5","npv_sim:  1286.037023<br />scaled: 0.8296183<br />1/2: 0.5","npv_sim:  1292.072731<br />scaled: 0.8284222<br />1/2: 0.5","npv_sim:  1298.108440<br />scaled: 0.8272177<br />1/2: 0.5","npv_sim:  1304.144148<br />scaled: 0.8260004<br />1/2: 0.5","npv_sim:  1310.179856<br />scaled: 0.8247806<br />1/2: 0.5","npv_sim:  1316.215564<br />scaled: 0.8235400<br />1/2: 0.5","npv_sim:  1322.251273<br />scaled: 0.8222995<br />1/2: 0.5","npv_sim:  1328.286981<br />scaled: 0.8210383<br />1/2: 0.5","npv_sim:  1334.322689<br />scaled: 0.8197724<br />1/2: 0.5","npv_sim:  1340.358398<br />scaled: 0.8184922<br />1/2: 0.5","npv_sim:  1346.394106<br />scaled: 0.8171987<br />1/2: 0.5","npv_sim:  1352.429814<br />scaled: 0.8158986<br />1/2: 0.5","npv_sim:  1358.465522<br />scaled: 0.8145753<br />1/2: 0.5","npv_sim:  1364.501231<br />scaled: 0.8132519<br />1/2: 0.5","npv_sim:  1370.536939<br />scaled: 0.8118990<br />1/2: 0.5","npv_sim:  1376.572647<br />scaled: 0.8105435<br />1/2: 0.5","npv_sim:  1382.608356<br />scaled: 0.8091665<br />1/2: 0.5","npv_sim:  1388.644064<br />scaled: 0.8077767<br />1/2: 0.5","npv_sim:  1394.679772<br />scaled: 0.8063746<br />1/2: 0.5","npv_sim:  1400.715481<br />scaled: 0.8049481<br />1/2: 0.5","npv_sim:  1406.751189<br />scaled: 0.8035200<br />1/2: 0.5","npv_sim:  1412.786897<br />scaled: 0.8020543<br />1/2: 0.5","npv_sim:  1418.822605<br />scaled: 0.8005887<br />1/2: 0.5","npv_sim:  1424.858314<br />scaled: 0.7990921<br />1/2: 0.5","npv_sim:  1430.894022<br />scaled: 0.7975849<br />1/2: 0.5","npv_sim:  1436.929730<br />scaled: 0.7960580<br />1/2: 0.5","npv_sim:  1442.965439<br />scaled: 0.7945069<br />1/2: 0.5","npv_sim:  1449.001147<br />scaled: 0.7929486<br />1/2: 0.5","npv_sim:  1455.036855<br />scaled: 0.7913511<br />1/2: 0.5","npv_sim:  1461.072564<br />scaled: 0.7897536<br />1/2: 0.5","npv_sim:  1467.108272<br />scaled: 0.7881143<br />1/2: 0.5","npv_sim:  1473.143980<br />scaled: 0.7864680<br />1/2: 0.5","npv_sim:  1479.179688<br />scaled: 0.7847930<br />1/2: 0.5","npv_sim:  1485.215397<br />scaled: 0.7830955<br />1/2: 0.5","npv_sim:  1491.251105<br />scaled: 0.7813838<br />1/2: 0.5","npv_sim:  1497.286813<br />scaled: 0.7796329<br />1/2: 0.5","npv_sim:  1503.322522<br />scaled: 0.7778819<br />1/2: 0.5","npv_sim:  1509.358230<br />scaled: 0.7760767<br />1/2: 0.5","npv_sim:  1515.393938<br />scaled: 0.7742699<br />1/2: 0.5","npv_sim:  1521.429646<br />scaled: 0.7724238<br />1/2: 0.5","npv_sim:  1527.465355<br />scaled: 0.7705589<br />1/2: 0.5","npv_sim:  1533.501063<br />scaled: 0.7686709<br />1/2: 0.5","npv_sim:  1539.536771<br />scaled: 0.7667457<br />1/2: 0.5","npv_sim:  1545.572480<br />scaled: 0.7648150<br />1/2: 0.5","npv_sim:  1551.608188<br />scaled: 0.7628273<br />1/2: 0.5","npv_sim:  1557.643896<br />scaled: 0.7608396<br />1/2: 0.5","npv_sim:  1563.679605<br />scaled: 0.7588007<br />1/2: 0.5","npv_sim:  1569.715313<br />scaled: 0.7567485<br />1/2: 0.5","npv_sim:  1575.751021<br />scaled: 0.7546631<br />1/2: 0.5","npv_sim:  1581.786729<br />scaled: 0.7525443<br />1/2: 0.5","npv_sim:  1587.822438<br />scaled: 0.7504117<br />1/2: 0.5","npv_sim:  1593.858146<br />scaled: 0.7482245<br />1/2: 0.5","npv_sim:  1599.893854<br />scaled: 0.7460373<br />1/2: 0.5","npv_sim:  1605.929563<br />scaled: 0.7437865<br />1/2: 0.5","npv_sim:  1611.965271<br />scaled: 0.7415291<br />1/2: 0.5","npv_sim:  1618.000979<br />scaled: 0.7392278<br />1/2: 0.5","npv_sim:  1624.036687<br />scaled: 0.7368986<br />1/2: 0.5","npv_sim:  1630.072396<br />scaled: 0.7345462<br />1/2: 0.5","npv_sim:  1636.108104<br />scaled: 0.7321438<br />1/2: 0.5","npv_sim:  1642.143812<br />scaled: 0.7297397<br />1/2: 0.5","npv_sim:  1648.179521<br />scaled: 0.7272625<br />1/2: 0.5","npv_sim:  1654.215229<br />scaled: 0.7247854<br />1/2: 0.5","npv_sim:  1660.250937<br />scaled: 0.7222531<br />1/2: 0.5","npv_sim:  1666.286646<br />scaled: 0.7197001<br />1/2: 0.5","npv_sim:  1672.322354<br />scaled: 0.7171140<br />1/2: 0.5","npv_sim:  1678.358062<br />scaled: 0.7144840<br />1/2: 0.5","npv_sim:  1684.393770<br />scaled: 0.7118435<br />1/2: 0.5","npv_sim:  1690.429479<br />scaled: 0.7091358<br />1/2: 0.5","npv_sim:  1696.465187<br />scaled: 0.7064281<br />1/2: 0.5","npv_sim:  1702.500895<br />scaled: 0.7036545<br />1/2: 0.5","npv_sim:  1708.536604<br />scaled: 0.7008683<br />1/2: 0.5","npv_sim:  1714.572312<br />scaled: 0.6980390<br />1/2: 0.5","npv_sim:  1720.608020<br />scaled: 0.6951739<br />1/2: 0.5","npv_sim:  1726.643729<br />scaled: 0.6922889<br />1/2: 0.5","npv_sim:  1732.679437<br />scaled: 0.6893444<br />1/2: 0.5","npv_sim:  1738.715145<br />scaled: 0.6864000<br />1/2: 0.5","npv_sim:  1744.750853<br />scaled: 0.6833797<br />1/2: 0.5","npv_sim:  1750.786562<br />scaled: 0.6803559<br />1/2: 0.5","npv_sim:  1756.822270<br />scaled: 0.6772797<br />1/2: 0.5","npv_sim:  1762.857978<br />scaled: 0.6741766<br />1/2: 0.5","npv_sim:  1768.893687<br />scaled: 0.6710446<br />1/2: 0.5","npv_sim:  1774.929395<br />scaled: 0.6678625<br />1/2: 0.5","npv_sim:  1780.965103<br />scaled: 0.6646749<br />1/2: 0.5","npv_sim:  1787.000811<br />scaled: 0.6614143<br />1/2: 0.5","npv_sim:  1793.036520<br />scaled: 0.6581537<br />1/2: 0.5","npv_sim:  1799.072228<br />scaled: 0.6548328<br />1/2: 0.5","npv_sim:  1805.107936<br />scaled: 0.6514944<br />1/2: 0.5","npv_sim:  1811.143645<br />scaled: 0.6481192<br />1/2: 0.5","npv_sim:  1817.179353<br />scaled: 0.6447040<br />1/2: 0.5","npv_sim:  1823.215061<br />scaled: 0.6412748<br />1/2: 0.5","npv_sim:  1829.250770<br />scaled: 0.6377839<br />1/2: 0.5","npv_sim:  1835.286478<br />scaled: 0.6342931<br />1/2: 0.5","npv_sim:  1841.322186<br />scaled: 0.6307361<br />1/2: 0.5","npv_sim:  1847.357894<br />scaled: 0.6271710<br />1/2: 0.5","npv_sim:  1847.357894<br />scaled: 0.6271710<br />1/2: 0.5","npv_sim:  1847.357894<br />scaled: 0.6271710<br />1/2: 0.5","npv_sim:  1841.322186<br />scaled: 0.6307361<br />1/2: 0.5","npv_sim:  1835.286478<br />scaled: 0.6342931<br />1/2: 0.5","npv_sim:  1829.250770<br />scaled: 0.6377839<br />1/2: 0.5","npv_sim:  1823.215061<br />scaled: 0.6412748<br />1/2: 0.5","npv_sim:  1817.179353<br />scaled: 0.6447040<br />1/2: 0.5","npv_sim:  1811.143645<br />scaled: 0.6481192<br />1/2: 0.5","npv_sim:  1805.107936<br />scaled: 0.6514944<br />1/2: 0.5","npv_sim:  1799.072228<br />scaled: 0.6548328<br />1/2: 0.5","npv_sim:  1793.036520<br />scaled: 0.6581537<br />1/2: 0.5","npv_sim:  1787.000811<br />scaled: 0.6614143<br />1/2: 0.5","npv_sim:  1780.965103<br />scaled: 0.6646749<br />1/2: 0.5","npv_sim:  1774.929395<br />scaled: 0.6678625<br />1/2: 0.5","npv_sim:  1768.893687<br />scaled: 0.6710446<br />1/2: 0.5","npv_sim:  1762.857978<br />scaled: 0.6741766<br />1/2: 0.5","npv_sim:  1756.822270<br />scaled: 0.6772797<br />1/2: 0.5","npv_sim:  1750.786562<br />scaled: 0.6803559<br />1/2: 0.5","npv_sim:  1744.750853<br />scaled: 0.6833797<br />1/2: 0.5","npv_sim:  1738.715145<br />scaled: 0.6864000<br />1/2: 0.5","npv_sim:  1732.679437<br />scaled: 0.6893444<br />1/2: 0.5","npv_sim:  1726.643729<br />scaled: 0.6922889<br />1/2: 0.5","npv_sim:  1720.608020<br />scaled: 0.6951739<br />1/2: 0.5","npv_sim:  1714.572312<br />scaled: 0.6980390<br />1/2: 0.5","npv_sim:  1708.536604<br />scaled: 0.7008683<br />1/2: 0.5","npv_sim:  1702.500895<br />scaled: 0.7036545<br />1/2: 0.5","npv_sim:  1696.465187<br />scaled: 0.7064281<br />1/2: 0.5","npv_sim:  1690.429479<br />scaled: 0.7091358<br />1/2: 0.5","npv_sim:  1684.393770<br />scaled: 0.7118435<br />1/2: 0.5","npv_sim:  1678.358062<br />scaled: 0.7144840<br />1/2: 0.5","npv_sim:  1672.322354<br />scaled: 0.7171140<br />1/2: 0.5","npv_sim:  1666.286646<br />scaled: 0.7197001<br />1/2: 0.5","npv_sim:  1660.250937<br />scaled: 0.7222531<br />1/2: 0.5","npv_sim:  1654.215229<br />scaled: 0.7247854<br />1/2: 0.5","npv_sim:  1648.179521<br />scaled: 0.7272625<br />1/2: 0.5","npv_sim:  1642.143812<br />scaled: 0.7297397<br />1/2: 0.5","npv_sim:  1636.108104<br />scaled: 0.7321438<br />1/2: 0.5","npv_sim:  1630.072396<br />scaled: 0.7345462<br />1/2: 0.5","npv_sim:  1624.036687<br />scaled: 0.7368986<br />1/2: 0.5","npv_sim:  1618.000979<br />scaled: 0.7392278<br />1/2: 0.5","npv_sim:  1611.965271<br />scaled: 0.7415291<br />1/2: 0.5","npv_sim:  1605.929563<br />scaled: 0.7437865<br />1/2: 0.5","npv_sim:  1599.893854<br />scaled: 0.7460373<br />1/2: 0.5","npv_sim:  1593.858146<br />scaled: 0.7482245<br />1/2: 0.5","npv_sim:  1587.822438<br />scaled: 0.7504117<br />1/2: 0.5","npv_sim:  1581.786729<br />scaled: 0.7525443<br />1/2: 0.5","npv_sim:  1575.751021<br />scaled: 0.7546631<br />1/2: 0.5","npv_sim:  1569.715313<br />scaled: 0.7567485<br />1/2: 0.5","npv_sim:  1563.679605<br />scaled: 0.7588007<br />1/2: 0.5","npv_sim:  1557.643896<br />scaled: 0.7608396<br />1/2: 0.5","npv_sim:  1551.608188<br />scaled: 0.7628273<br />1/2: 0.5","npv_sim:  1545.572480<br />scaled: 0.7648150<br />1/2: 0.5","npv_sim:  1539.536771<br />scaled: 0.7667457<br />1/2: 0.5","npv_sim:  1533.501063<br />scaled: 0.7686709<br />1/2: 0.5","npv_sim:  1527.465355<br />scaled: 0.7705589<br />1/2: 0.5","npv_sim:  1521.429646<br />scaled: 0.7724238<br />1/2: 0.5","npv_sim:  1515.393938<br />scaled: 0.7742699<br />1/2: 0.5","npv_sim:  1509.358230<br />scaled: 0.7760767<br />1/2: 0.5","npv_sim:  1503.322522<br />scaled: 0.7778819<br />1/2: 0.5","npv_sim:  1497.286813<br />scaled: 0.7796329<br />1/2: 0.5","npv_sim:  1491.251105<br />scaled: 0.7813838<br />1/2: 0.5","npv_sim:  1485.215397<br />scaled: 0.7830955<br />1/2: 0.5","npv_sim:  1479.179688<br />scaled: 0.7847930<br />1/2: 0.5","npv_sim:  1473.143980<br />scaled: 0.7864680<br />1/2: 0.5","npv_sim:  1467.108272<br />scaled: 0.7881143<br />1/2: 0.5","npv_sim:  1461.072564<br />scaled: 0.7897536<br />1/2: 0.5","npv_sim:  1455.036855<br />scaled: 0.7913511<br />1/2: 0.5","npv_sim:  1449.001147<br />scaled: 0.7929486<br />1/2: 0.5","npv_sim:  1442.965439<br />scaled: 0.7945069<br />1/2: 0.5","npv_sim:  1436.929730<br />scaled: 0.7960580<br />1/2: 0.5","npv_sim:  1430.894022<br />scaled: 0.7975849<br />1/2: 0.5","npv_sim:  1424.858314<br />scaled: 0.7990921<br />1/2: 0.5","npv_sim:  1418.822605<br />scaled: 0.8005887<br />1/2: 0.5","npv_sim:  1412.786897<br />scaled: 0.8020543<br />1/2: 0.5","npv_sim:  1406.751189<br />scaled: 0.8035200<br />1/2: 0.5","npv_sim:  1400.715481<br />scaled: 0.8049481<br />1/2: 0.5","npv_sim:  1394.679772<br />scaled: 0.8063746<br />1/2: 0.5","npv_sim:  1388.644064<br />scaled: 0.8077767<br />1/2: 0.5","npv_sim:  1382.608356<br />scaled: 0.8091665<br />1/2: 0.5","npv_sim:  1376.572647<br />scaled: 0.8105435<br />1/2: 0.5","npv_sim:  1370.536939<br />scaled: 0.8118990<br />1/2: 0.5","npv_sim:  1364.501231<br />scaled: 0.8132519<br />1/2: 0.5","npv_sim:  1358.465522<br />scaled: 0.8145753<br />1/2: 0.5","npv_sim:  1352.429814<br />scaled: 0.8158986<br />1/2: 0.5","npv_sim:  1346.394106<br />scaled: 0.8171987<br />1/2: 0.5","npv_sim:  1340.358398<br />scaled: 0.8184922<br />1/2: 0.5","npv_sim:  1334.322689<br />scaled: 0.8197724<br />1/2: 0.5","npv_sim:  1328.286981<br />scaled: 0.8210383<br />1/2: 0.5","npv_sim:  1322.251273<br />scaled: 0.8222995<br />1/2: 0.5","npv_sim:  1316.215564<br />scaled: 0.8235400<br />1/2: 0.5","npv_sim:  1310.179856<br />scaled: 0.8247806<br />1/2: 0.5","npv_sim:  1304.144148<br />scaled: 0.8260004<br />1/2: 0.5","npv_sim:  1298.108440<br />scaled: 0.8272177<br />1/2: 0.5","npv_sim:  1292.072731<br />scaled: 0.8284222<br />1/2: 0.5","npv_sim:  1286.037023<br />scaled: 0.8296183<br />1/2: 0.5","npv_sim:  1280.001315<br />scaled: 0.8308085<br />1/2: 0.5","npv_sim:  1273.965606<br />scaled: 0.8319853<br />1/2: 0.5","npv_sim:  1267.929898<br />scaled: 0.8331620<br />1/2: 0.5","npv_sim:  1261.894190<br />scaled: 0.8343215<br />1/2: 0.5","npv_sim:  1255.858481<br />scaled: 0.8354810<br />1/2: 0.5","npv_sim:  1249.822773<br />scaled: 0.8366294<br />1/2: 0.5","npv_sim:  1243.787065<br />scaled: 0.8377734<br />1/2: 0.5","npv_sim:  1237.751357<br />scaled: 0.8389117<br />1/2: 0.5","npv_sim:  1231.715648<br />scaled: 0.8400419<br />1/2: 0.5","npv_sim:  1225.679940<br />scaled: 0.8411707<br />1/2: 0.5","npv_sim:  1219.644232<br />scaled: 0.8422890<br />1/2: 0.5","npv_sim:  1213.608523<br />scaled: 0.8434072<br />1/2: 0.5","npv_sim:  1207.572815<br />scaled: 0.8445168<br />1/2: 0.5","npv_sim:  1201.537107<br />scaled: 0.8456246<br />1/2: 0.5","npv_sim:  1195.501399<br />scaled: 0.8467277<br />1/2: 0.5","npv_sim:  1189.465690<br />scaled: 0.8478266<br />1/2: 0.5","npv_sim:  1183.429982<br />scaled: 0.8489237<br />1/2: 0.5","npv_sim:  1177.394274<br />scaled: 0.8500152<br />1/2: 0.5","npv_sim:  1171.358565<br />scaled: 0.8511066<br />1/2: 0.5","npv_sim:  1165.322857<br />scaled: 0.8521923<br />1/2: 0.5","npv_sim:  1159.287149<br />scaled: 0.8532777<br />1/2: 0.5","npv_sim:  1153.251440<br />scaled: 0.8543600<br />1/2: 0.5","npv_sim:  1147.215732<br />scaled: 0.8554405<br />1/2: 0.5","npv_sim:  1141.180024<br />scaled: 0.8565198<br />1/2: 0.5","npv_sim:  1135.144316<br />scaled: 0.8575968<br />1/2: 0.5","npv_sim:  1129.108607<br />scaled: 0.8586737<br />1/2: 0.5","npv_sim:  1123.072899<br />scaled: 0.8597482<br />1/2: 0.5","npv_sim:  1117.037191<br />scaled: 0.8608227<br />1/2: 0.5","npv_sim:  1111.001482<br />scaled: 0.8618962<br />1/2: 0.5","npv_sim:  1104.965774<br />scaled: 0.8629693<br />1/2: 0.5","npv_sim:  1098.930066<br />scaled: 0.8640423<br />1/2: 0.5","npv_sim:  1092.894357<br />scaled: 0.8651150<br />1/2: 0.5","npv_sim:  1086.858649<br />scaled: 0.8661878<br />1/2: 0.5","npv_sim:  1080.822941<br />scaled: 0.8672610<br />1/2: 0.5","npv_sim:  1074.787233<br />scaled: 0.8683342<br />1/2: 0.5","npv_sim:  1068.751524<br />scaled: 0.8694085<br />1/2: 0.5","npv_sim:  1062.715816<br />scaled: 0.8704831<br />1/2: 0.5","npv_sim:  1056.680108<br />scaled: 0.8715588<br />1/2: 0.5","npv_sim:  1050.644399<br />scaled: 0.8726354<br />1/2: 0.5","npv_sim:  1044.608691<br />scaled: 0.8737129<br />1/2: 0.5","npv_sim:  1038.572983<br />scaled: 0.8747923<br />1/2: 0.5","npv_sim:  1032.537275<br />scaled: 0.8758717<br />1/2: 0.5","npv_sim:  1026.501566<br />scaled: 0.8769545<br />1/2: 0.5","npv_sim:  1020.465858<br />scaled: 0.8780373<br />1/2: 0.5","npv_sim:  1014.430150<br />scaled: 0.8791229<br />1/2: 0.5","npv_sim:  1008.394441<br />scaled: 0.8802097<br />1/2: 0.5","npv_sim:  1002.358733<br />scaled: 0.8812983<br />1/2: 0.5","npv_sim:   996.323025<br />scaled: 0.8823895<br />1/2: 0.5","npv_sim:   990.287316<br />scaled: 0.8834813<br />1/2: 0.5","npv_sim:   984.251608<br />scaled: 0.8845775<br />1/2: 0.5","npv_sim:   978.215900<br />scaled: 0.8856737<br />1/2: 0.5","npv_sim:   972.180192<br />scaled: 0.8867742<br />1/2: 0.5","npv_sim:   966.144483<br />scaled: 0.8878756<br />1/2: 0.5","npv_sim:   960.108775<br />scaled: 0.8889800<br />1/2: 0.5","npv_sim:   954.073067<br />scaled: 0.8900870<br />1/2: 0.5","npv_sim:   948.037358<br />scaled: 0.8911954<br />1/2: 0.5","npv_sim:   942.001650<br />scaled: 0.8923083<br />1/2: 0.5","npv_sim:   935.965942<br />scaled: 0.8934212<br />1/2: 0.5","npv_sim:   929.930234<br />scaled: 0.8945397<br />1/2: 0.5","npv_sim:   923.894525<br />scaled: 0.8956586<br />1/2: 0.5","npv_sim:   917.858817<br />scaled: 0.8967814<br />1/2: 0.5","npv_sim:   911.823109<br />scaled: 0.8979064<br />1/2: 0.5","npv_sim:   905.787400<br />scaled: 0.8990336<br />1/2: 0.5","npv_sim:   899.751692<br />scaled: 0.9001649<br />1/2: 0.5","npv_sim:   893.715984<br />scaled: 0.9012964<br />1/2: 0.5","npv_sim:   887.680275<br />scaled: 0.9024339<br />1/2: 0.5","npv_sim:   881.644567<br />scaled: 0.9035714<br />1/2: 0.5","npv_sim:   875.608859<br />scaled: 0.9047135<br />1/2: 0.5","npv_sim:   869.573151<br />scaled: 0.9058571<br />1/2: 0.5","npv_sim:   863.537442<br />scaled: 0.9070034<br />1/2: 0.5","npv_sim:   857.501734<br />scaled: 0.9081531<br />1/2: 0.5","npv_sim:   851.466026<br />scaled: 0.9093036<br />1/2: 0.5","npv_sim:   845.430317<br />scaled: 0.9104591<br />1/2: 0.5","npv_sim:   839.394609<br />scaled: 0.9116146<br />1/2: 0.5","npv_sim:   833.358901<br />scaled: 0.9127748<br />1/2: 0.5","npv_sim:   827.323193<br />scaled: 0.9139358<br />1/2: 0.5","npv_sim:   821.287484<br />scaled: 0.9150998<br />1/2: 0.5","npv_sim:   815.251776<br />scaled: 0.9162660<br />1/2: 0.5","npv_sim:   809.216068<br />scaled: 0.9174336<br />1/2: 0.5","npv_sim:   803.180359<br />scaled: 0.9186046<br />1/2: 0.5","npv_sim:   797.144651<br />scaled: 0.9197757<br />1/2: 0.5","npv_sim:   791.108943<br />scaled: 0.9209509<br />1/2: 0.5","npv_sim:   785.073234<br />scaled: 0.9221263<br />1/2: 0.5","npv_sim:   779.037526<br />scaled: 0.9233042<br />1/2: 0.5","npv_sim:   773.001818<br />scaled: 0.9244833<br />1/2: 0.5","npv_sim:   766.966110<br />scaled: 0.9256636<br />1/2: 0.5","npv_sim:   760.930401<br />scaled: 0.9268458<br />1/2: 0.5","npv_sim:   754.894693<br />scaled: 0.9280283<br />1/2: 0.5","npv_sim:   748.858985<br />scaled: 0.9292129<br />1/2: 0.5","npv_sim:   742.823276<br />scaled: 0.9303975<br />1/2: 0.5","npv_sim:   736.787568<br />scaled: 0.9315833<br />1/2: 0.5","npv_sim:   730.751860<br />scaled: 0.9327695<br />1/2: 0.5","npv_sim:   724.716151<br />scaled: 0.9339559<br />1/2: 0.5","npv_sim:   718.680443<br />scaled: 0.9351427<br />1/2: 0.5","npv_sim:   712.644735<br />scaled: 0.9363295<br />1/2: 0.5","npv_sim:   706.609027<br />scaled: 0.9375159<br />1/2: 0.5","npv_sim:   700.573318<br />scaled: 0.9387024<br />1/2: 0.5","npv_sim:   694.537610<br />scaled: 0.9398875<br />1/2: 0.5","npv_sim:   688.501902<br />scaled: 0.9410725<br />1/2: 0.5","npv_sim:   682.466193<br />scaled: 0.9422558<br />1/2: 0.5","npv_sim:   676.430485<br />scaled: 0.9434382<br />1/2: 0.5","npv_sim:   670.394777<br />scaled: 0.9446193<br />1/2: 0.5","npv_sim:   664.359069<br />scaled: 0.9457977<br />1/2: 0.5","npv_sim:   658.323360<br />scaled: 0.9469760<br />1/2: 0.5","npv_sim:   652.287652<br />scaled: 0.9481490<br />1/2: 0.5","npv_sim:   646.251944<br />scaled: 0.9493221<br />1/2: 0.5","npv_sim:   640.216235<br />scaled: 0.9504902<br />1/2: 0.5","npv_sim:   634.180527<br />scaled: 0.9516564<br />1/2: 0.5","npv_sim:   628.144819<br />scaled: 0.9528189<br />1/2: 0.5","npv_sim:   622.109110<br />scaled: 0.9539767<br />1/2: 0.5","npv_sim:   616.073402<br />scaled: 0.9551330<br />1/2: 0.5","npv_sim:   610.037694<br />scaled: 0.9562806<br />1/2: 0.5","npv_sim:   604.001986<br />scaled: 0.9574282<br />1/2: 0.5","npv_sim:   597.966277<br />scaled: 0.9585657<br />1/2: 0.5","npv_sim:   591.930569<br />scaled: 0.9597013<br />1/2: 0.5","npv_sim:   585.894861<br />scaled: 0.9608293<br />1/2: 0.5","npv_sim:   579.859152<br />scaled: 0.9619510<br />1/2: 0.5","npv_sim:   573.823444<br />scaled: 0.9630686<br />1/2: 0.5","npv_sim:   567.787736<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   561.752028<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   555.716319<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   549.680611<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   543.644903<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   537.609194<br />scaled: 0.9695976<br />1/2: 0.5","npv_sim:   531.573486<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   525.537778<br />scaled: 0.9717012<br />1/2: 0.5","npv_sim:   519.502069<br />scaled: 0.9727441<br />1/2: 0.5","npv_sim:   513.466361<br />scaled: 0.9737636<br />1/2: 0.5","npv_sim:   507.430653<br />scaled: 0.9747832<br />1/2: 0.5","npv_sim:   501.394945<br />scaled: 0.9757813<br />1/2: 0.5","npv_sim:   495.359236<br />scaled: 0.9767732<br />1/2: 0.5","npv_sim:   489.323528<br />scaled: 0.9777506<br />1/2: 0.5","npv_sim:   483.287820<br />scaled: 0.9787123<br />1/2: 0.5","npv_sim:   477.252111<br />scaled: 0.9796678<br />1/2: 0.5","npv_sim:   471.216403<br />scaled: 0.9805966<br />1/2: 0.5","npv_sim:   465.180695<br />scaled: 0.9815255<br />1/2: 0.5","npv_sim:   459.144986<br />scaled: 0.9824224<br />1/2: 0.5","npv_sim:   453.109278<br />scaled: 0.9833155<br />1/2: 0.5","npv_sim:   447.073570<br />scaled: 0.9841856<br />1/2: 0.5","npv_sim:   441.037862<br />scaled: 0.9850403<br />1/2: 0.5","npv_sim:   435.002153<br />scaled: 0.9858823<br />1/2: 0.5","npv_sim:   428.966445<br />scaled: 0.9866956<br />1/2: 0.5","npv_sim:   422.930737<br />scaled: 0.9875084<br />1/2: 0.5","npv_sim:   416.895028<br />scaled: 0.9882773<br />1/2: 0.5","npv_sim:   410.859320<br />scaled: 0.9890462<br />1/2: 0.5","npv_sim:   404.823612<br />scaled: 0.9897814<br />1/2: 0.5","npv_sim:   398.787904<br />scaled: 0.9905030<br />1/2: 0.5","npv_sim:   392.752195<br />scaled: 0.9912035<br />1/2: 0.5","npv_sim:   386.716487<br />scaled: 0.9918748<br />1/2: 0.5","npv_sim:   380.680779<br />scaled: 0.9925395<br />1/2: 0.5","npv_sim:   374.645070<br />scaled: 0.9931574<br />1/2: 0.5","npv_sim:   368.609362<br />scaled: 0.9937754<br />1/2: 0.5","npv_sim:   362.573654<br />scaled: 0.9943466<br />1/2: 0.5","npv_sim:   356.537945<br />scaled: 0.9949082<br />1/2: 0.5","npv_sim:   350.502237<br />scaled: 0.9954381<br />1/2: 0.5","npv_sim:   344.466529<br />scaled: 0.9959402<br />1/2: 0.5","npv_sim:   338.430821<br />scaled: 0.9964275<br />1/2: 0.5","npv_sim:   332.395112<br />scaled: 0.9968673<br />1/2: 0.5","npv_sim:   326.359404<br />scaled: 0.9973070<br />1/2: 0.5","npv_sim:   320.323696<br />scaled: 0.9976851<br />1/2: 0.5","npv_sim:   314.287987<br />scaled: 0.9980594<br />1/2: 0.5","npv_sim:   308.252279<br />scaled: 0.9983894<br />1/2: 0.5","npv_sim:   302.216571<br />scaled: 0.9986954<br />1/2: 0.5","npv_sim:   296.180863<br />scaled: 0.9989763<br />1/2: 0.5","npv_sim:   290.145154<br />scaled: 0.9992110<br />1/2: 0.5","npv_sim:   284.109446<br />scaled: 0.9994415<br />1/2: 0.5","npv_sim:   278.073738<br />scaled: 0.9996021<br />1/2: 0.5","npv_sim:   272.038029<br />scaled: 0.9997628<br />1/2: 0.5","npv_sim:   266.002321<br />scaled: 0.9998649<br />1/2: 0.5","npv_sim:   259.966613<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   253.930904<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   247.895196<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   241.859488<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   235.823780<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   229.788071<br />scaled: 0.9998353<br />1/2: 0.5","npv_sim:   223.752363<br />scaled: 0.9996837<br />1/2: 0.5","npv_sim:   217.716655<br />scaled: 0.9995218<br />1/2: 0.5","npv_sim:   211.680946<br />scaled: 0.9993095<br />1/2: 0.5","npv_sim:   205.645238<br />scaled: 0.9990609<br />1/2: 0.5","npv_sim:   199.609530<br />scaled: 0.9987868<br />1/2: 0.5","npv_sim:   193.573821<br />scaled: 0.9984495<br />1/2: 0.5","npv_sim:   187.538113<br />scaled: 0.9981121<br />1/2: 0.5","npv_sim:   181.502405<br />scaled: 0.9976847<br />1/2: 0.5","npv_sim:   175.466697<br />scaled: 0.9972566<br />1/2: 0.5","npv_sim:   169.430988<br />scaled: 0.9967640<br />1/2: 0.5","npv_sim:   163.395280<br />scaled: 0.9962433<br />1/2: 0.5","npv_sim:   157.359572<br />scaled: 0.9956849<br />1/2: 0.5","npv_sim:   151.323863<br />scaled: 0.9950700<br />1/2: 0.5","npv_sim:   145.288155<br />scaled: 0.9944450<br />1/2: 0.5","npv_sim:   139.252447<br />scaled: 0.9937346<br />1/2: 0.5","npv_sim:   133.216739<br />scaled: 0.9930242<br />1/2: 0.5","npv_sim:   127.181030<br />scaled: 0.9922352<br />1/2: 0.5","npv_sim:   121.145322<br />scaled: 0.9914280<br />1/2: 0.5","npv_sim:   115.109614<br />scaled: 0.9905703<br />1/2: 0.5","npv_sim:   109.073905<br />scaled: 0.9896653<br />1/2: 0.5","npv_sim:   103.038197<br />scaled: 0.9887385<br />1/2: 0.5","npv_sim:    97.002489<br />scaled: 0.9877349<br />1/2: 0.5","npv_sim:    90.966780<br />scaled: 0.9867313<br />1/2: 0.5","npv_sim:    84.931072<br />scaled: 0.9856359<br />1/2: 0.5","npv_sim:    78.895364<br />scaled: 0.9845331<br />1/2: 0.5","npv_sim:    72.859656<br />scaled: 0.9833676<br />1/2: 0.5","npv_sim:    66.823947<br />scaled: 0.9821654<br />1/2: 0.5","npv_sim:    60.788239<br />scaled: 0.9809298<br />1/2: 0.5","npv_sim:    54.752531<br />scaled: 0.9796280<br />1/2: 0.5","npv_sim:    48.716822<br />scaled: 0.9783222<br />1/2: 0.5","npv_sim:    42.681114<br />scaled: 0.9769210<br />1/2: 0.5","npv_sim:    36.645406<br />scaled: 0.9755199<br />1/2: 0.5","npv_sim:    30.609698<br />scaled: 0.9740449<br />1/2: 0.5","npv_sim:    24.573989<br />scaled: 0.9725448<br />1/2: 0.5","npv_sim:    18.538281<br />scaled: 0.9710003<br />1/2: 0.5","npv_sim:    12.502573<br />scaled: 0.9694018<br />1/2: 0.5","npv_sim:     6.466864<br />scaled: 0.9677882<br />1/2: 0.5","npv_sim:     0.431156<br />scaled: 0.9660922<br />1/2: 0.5","npv_sim:    -5.604552<br />scaled: 0.9643963<br />1/2: 0.5","npv_sim:   -11.640261<br />scaled: 0.9626175<br />1/2: 0.5","npv_sim:   -17.675969<br />scaled: 0.9608252<br />1/2: 0.5","npv_sim:   -23.711677<br />scaled: 0.9589793<br />1/2: 0.5","npv_sim:   -29.747385<br />scaled: 0.9570921<br />1/2: 0.5","npv_sim:   -35.783094<br />scaled: 0.9551797<br />1/2: 0.5","npv_sim:   -41.818802<br />scaled: 0.9531992<br />1/2: 0.5","npv_sim:   -47.854510<br />scaled: 0.9512187<br />1/2: 0.5","npv_sim:   -53.890219<br />scaled: 0.9491491<br />1/2: 0.5","npv_sim:   -59.925927<br />scaled: 0.9470771<br />1/2: 0.5","npv_sim:   -65.961635<br />scaled: 0.9449444<br />1/2: 0.5","npv_sim:   -71.997343<br />scaled: 0.9427831<br />1/2: 0.5","npv_sim:   -78.033052<br />scaled: 0.9405883<br />1/2: 0.5","npv_sim:   -84.068760<br />scaled: 0.9383399<br />1/2: 0.5","npv_sim:   -90.104468<br />scaled: 0.9360839<br />1/2: 0.5","npv_sim:   -96.140177<br />scaled: 0.9337510<br />1/2: 0.5","npv_sim:  -102.175885<br />scaled: 0.9314181<br />1/2: 0.5","npv_sim:  -108.211593<br />scaled: 0.9290202<br />1/2: 0.5","npv_sim:  -114.247302<br />scaled: 0.9266054<br />1/2: 0.5","npv_sim:  -120.283010<br />scaled: 0.9241513<br />1/2: 0.5","npv_sim:  -126.318718<br />scaled: 0.9216577<br />1/2: 0.5","npv_sim:  -132.354426<br />scaled: 0.9191486<br />1/2: 0.5","npv_sim:  -138.390135<br />scaled: 0.9165792<br />1/2: 0.5","npv_sim:  -144.425843<br />scaled: 0.9140097<br />1/2: 0.5","npv_sim:  -150.461551<br />scaled: 0.9113744<br />1/2: 0.5","npv_sim:  -156.497260<br />scaled: 0.9087325<br />1/2: 0.5","npv_sim:  -162.532968<br />scaled: 0.9060482<br />1/2: 0.5","npv_sim:  -168.568676<br />scaled: 0.9033372<br />1/2: 0.5","npv_sim:  -174.604385<br />scaled: 0.9006053<br />1/2: 0.5","npv_sim:  -180.640093<br />scaled: 0.8978288<br />1/2: 0.5","npv_sim:  -186.675801<br />scaled: 0.8950508<br />1/2: 0.5","npv_sim:  -192.711509<br />scaled: 0.8922125<br />1/2: 0.5","npv_sim:  -198.747218<br />scaled: 0.8893742<br />1/2: 0.5","npv_sim:  -204.782926<br />scaled: 0.8864937<br />1/2: 0.5","npv_sim:  -210.818634<br />scaled: 0.8835974<br />1/2: 0.5","npv_sim:  -216.854343<br />scaled: 0.8806777<br />1/2: 0.5","npv_sim:  -222.890051<br />scaled: 0.8777273<br />1/2: 0.5","npv_sim:  -228.925759<br />scaled: 0.8747701<br />1/2: 0.5","npv_sim:  -234.961467<br />scaled: 0.8717697<br />1/2: 0.5","npv_sim:  -240.997176<br />scaled: 0.8687693<br />1/2: 0.5","npv_sim:  -247.032884<br />scaled: 0.8657301<br />1/2: 0.5","npv_sim:  -253.068592<br />scaled: 0.8626837<br />1/2: 0.5","npv_sim:  -259.104301<br />scaled: 0.8596144<br />1/2: 0.5","npv_sim:  -265.140009<br />scaled: 0.8565261<br />1/2: 0.5","npv_sim:  -271.175717<br />scaled: 0.8534283<br />1/2: 0.5","npv_sim:  -277.211426<br />scaled: 0.8503023<br />1/2: 0.5","npv_sim:  -283.247134<br />scaled: 0.8471763<br />1/2: 0.5","npv_sim:  -289.282842<br />scaled: 0.8440182<br />1/2: 0.5","npv_sim:  -295.318550<br />scaled: 0.8408586<br />1/2: 0.5","npv_sim:  -301.354259<br />scaled: 0.8376795<br />1/2: 0.5","npv_sim:  -307.389967<br />scaled: 0.8344905<br />1/2: 0.5","npv_sim:  -313.425675<br />scaled: 0.8312922<br />1/2: 0.5","npv_sim:  -319.461384<br />scaled: 0.8280780<br />1/2: 0.5","npv_sim:  -325.497092<br />scaled: 0.8248622<br />1/2: 0.5","npv_sim:  -331.532800<br />scaled: 0.8216267<br />1/2: 0.5","npv_sim:  -337.568508<br />scaled: 0.8183913<br />1/2: 0.5","npv_sim:  -343.604217<br />scaled: 0.8151425<br />1/2: 0.5","npv_sim:  -349.639925<br />scaled: 0.8118899<br />1/2: 0.5","npv_sim:  -355.675633<br />scaled: 0.8086310<br />1/2: 0.5","npv_sim:  -361.711342<br />scaled: 0.8053653<br />1/2: 0.5","npv_sim:  -367.747050<br />scaled: 0.8020979<br />1/2: 0.5","npv_sim:  -373.782758<br />scaled: 0.7988229<br />1/2: 0.5","npv_sim:  -379.818467<br />scaled: 0.7955479<br />1/2: 0.5","npv_sim:  -385.854175<br />scaled: 0.7922681<br />1/2: 0.5","npv_sim:  -391.889883<br />scaled: 0.7889876<br />1/2: 0.5","npv_sim:  -397.925591<br />scaled: 0.7857061<br />1/2: 0.5","npv_sim:  -403.961300<br />scaled: 0.7824239<br />1/2: 0.5","npv_sim:  -409.997008<br />scaled: 0.7791423<br />1/2: 0.5","npv_sim:  -416.032716<br />scaled: 0.7758618<br />1/2: 0.5","npv_sim:  -422.068425<br />scaled: 0.7725814<br />1/2: 0.5","npv_sim:  -428.104133<br />scaled: 0.7693062<br />1/2: 0.5","npv_sim:  -434.139841<br />scaled: 0.7660310<br />1/2: 0.5","npv_sim:  -440.175550<br />scaled: 0.7627618<br />1/2: 0.5","npv_sim:  -446.211258<br />scaled: 0.7594950<br />1/2: 0.5","npv_sim:  -452.246966<br />scaled: 0.7562330<br />1/2: 0.5","npv_sim:  -458.282674<br />scaled: 0.7529778<br />1/2: 0.5","npv_sim:  -464.318383<br />scaled: 0.7497243<br />1/2: 0.5","npv_sim:  -470.354091<br />scaled: 0.7464836<br />1/2: 0.5","npv_sim:  -476.389799<br />scaled: 0.7432429<br />1/2: 0.5","npv_sim:  -482.425508<br />scaled: 0.7400163<br />1/2: 0.5","npv_sim:  -488.461216<br />scaled: 0.7367928<br />1/2: 0.5","npv_sim:  -494.496924<br />scaled: 0.7335798<br />1/2: 0.5","npv_sim:  -500.532632<br />scaled: 0.7303761<br />1/2: 0.5","npv_sim:  -506.568341<br />scaled: 0.7271775<br />1/2: 0.5","npv_sim:  -512.604049<br />scaled: 0.7239959<br />1/2: 0.5","npv_sim:  -518.639757<br />scaled: 0.7208143<br />1/2: 0.5","npv_sim:  -524.675466<br />scaled: 0.7176555<br />1/2: 0.5","npv_sim:  -530.711174<br />scaled: 0.7144981<br />1/2: 0.5","npv_sim:  -536.746882<br />scaled: 0.7113577<br />1/2: 0.5","npv_sim:  -542.782591<br />scaled: 0.7082265<br />1/2: 0.5","npv_sim:  -548.818299<br />scaled: 0.7051052<br />1/2: 0.5","npv_sim:  -554.854007<br />scaled: 0.7020020<br />1/2: 0.5","npv_sim:  -560.889715<br />scaled: 0.6989004<br />1/2: 0.5","npv_sim:  -566.925424<br />scaled: 0.6958266<br />1/2: 0.5","npv_sim:  -572.961132<br />scaled: 0.6927528<br />1/2: 0.5","npv_sim:  -578.996840<br />scaled: 0.6897024<br />1/2: 0.5","npv_sim:  -585.032549<br />scaled: 0.6866594<br />1/2: 0.5","npv_sim:  -591.068257<br />scaled: 0.6836311<br />1/2: 0.5","npv_sim:  -597.103965<br />scaled: 0.6806199<br />1/2: 0.5","npv_sim:  -603.139673<br />scaled: 0.6776142<br />1/2: 0.5","npv_sim:  -609.175382<br />scaled: 0.6746356<br />1/2: 0.5","npv_sim:  -615.211090<br />scaled: 0.6716570<br />1/2: 0.5","npv_sim:  -621.246798<br />scaled: 0.6687076<br />1/2: 0.5","npv_sim:  -627.282507<br />scaled: 0.6657623<br />1/2: 0.5","npv_sim:  -633.318215<br />scaled: 0.6628366<br />1/2: 0.5","npv_sim:  -639.353923<br />scaled: 0.6599250<br />1/2: 0.5","npv_sim:  -645.389632<br />scaled: 0.6570231<br />1/2: 0.5","npv_sim:  -651.425340<br />scaled: 0.6541455<br />1/2: 0.5","npv_sim:  -657.461048<br />scaled: 0.6512678<br />1/2: 0.5","npv_sim:  -663.496756<br />scaled: 0.6484238<br />1/2: 0.5","npv_sim:  -669.532465<br />scaled: 0.6455801<br />1/2: 0.5","npv_sim:  -675.568173<br />scaled: 0.6427599<br />1/2: 0.5","npv_sim:  -681.603881<br />scaled: 0.6399500<br />1/2: 0.5","npv_sim:  -687.639590<br />scaled: 0.6371534<br />1/2: 0.5","npv_sim:  -693.675298<br />scaled: 0.6343768<br />1/2: 0.5","npv_sim:  -699.711006<br />scaled: 0.6316037<br />1/2: 0.5","npv_sim:  -705.746715<br />scaled: 0.6288599<br />1/2: 0.5","npv_sim:  -711.782423<br />scaled: 0.6261162<br />1/2: 0.5","npv_sim:  -717.818131<br />scaled: 0.6233984<br />1/2: 0.5","npv_sim:  -723.853839<br />scaled: 0.6206867<br />1/2: 0.5","npv_sim:  -729.889548<br />scaled: 0.6179910<br />1/2: 0.5","npv_sim:  -735.925256<br />scaled: 0.6153104<br />1/2: 0.5","npv_sim:  -741.960964<br />scaled: 0.6126364<br />1/2: 0.5","npv_sim:  -747.996673<br />scaled: 0.6099859<br />1/2: 0.5","npv_sim:  -754.032381<br />scaled: 0.6073354<br />1/2: 0.5","npv_sim:  -760.068089<br />scaled: 0.6047116<br />1/2: 0.5","npv_sim:  -766.103797<br />scaled: 0.6020899<br />1/2: 0.5","npv_sim:  -772.139506<br />scaled: 0.5994855<br />1/2: 0.5","npv_sim:  -778.175214<br />scaled: 0.5968913<br />1/2: 0.5","npv_sim:  -784.210922<br />scaled: 0.5943058<br />1/2: 0.5","npv_sim:  -790.246631<br />scaled: 0.5917376<br />1/2: 0.5","npv_sim:  -796.282339<br />scaled: 0.5891703<br />1/2: 0.5","npv_sim:  -802.318047<br />scaled: 0.5866265<br />1/2: 0.5","npv_sim:  -808.353756<br />scaled: 0.5840826<br />1/2: 0.5","npv_sim:  -814.389464<br />scaled: 0.5815556<br />1/2: 0.5","npv_sim:  -820.425172<br />scaled: 0.5790344<br />1/2: 0.5","npv_sim:  -826.460880<br />scaled: 0.5765225<br />1/2: 0.5","npv_sim:  -832.496589<br />scaled: 0.5740222<br />1/2: 0.5","npv_sim:  -838.532297<br />scaled: 0.5715247<br />1/2: 0.5","npv_sim:  -844.568005<br />scaled: 0.5690433<br />1/2: 0.5","npv_sim:  -850.603714<br />scaled: 0.5665619<br />1/2: 0.5","npv_sim:  -856.639422<br />scaled: 0.5640951<br />1/2: 0.5","npv_sim:  -862.675130<br />scaled: 0.5616306<br />1/2: 0.5","npv_sim:  -868.710838<br />scaled: 0.5591747<br />1/2: 0.5","npv_sim:  -874.746547<br />scaled: 0.5567252<br />1/2: 0.5","npv_sim:  -880.782255<br />scaled: 0.5542793<br />1/2: 0.5","npv_sim:  -886.817963<br />scaled: 0.5518428<br />1/2: 0.5","npv_sim:  -892.853672<br />scaled: 0.5494063<br />1/2: 0.5","npv_sim:  -898.889380<br />scaled: 0.5469803<br />1/2: 0.5","npv_sim:  -904.925088<br />scaled: 0.5445547<br />1/2: 0.5","npv_sim:  -910.960797<br />scaled: 0.5421350<br />1/2: 0.5","npv_sim:  -916.996505<br />scaled: 0.5397181<br />1/2: 0.5","npv_sim:  -923.032213<br />scaled: 0.5373037<br />1/2: 0.5","npv_sim:  -929.067921<br />scaled: 0.5348934<br />1/2: 0.5","npv_sim:  -935.103630<br />scaled: 0.5324836<br />1/2: 0.5","npv_sim:  -941.139338<br />scaled: 0.5300779<br />1/2: 0.5","npv_sim:  -947.175046<br />scaled: 0.5276721<br />1/2: 0.5","npv_sim:  -953.210755<br />scaled: 0.5252684<br />1/2: 0.5","npv_sim:  -959.246463<br />scaled: 0.5228651<br />1/2: 0.5","npv_sim:  -965.282171<br />scaled: 0.5204620<br />1/2: 0.5","npv_sim:  -971.317879<br />scaled: 0.5180592<br />1/2: 0.5","npv_sim:  -977.353588<br />scaled: 0.5156560<br />1/2: 0.5","npv_sim:  -983.389296<br />scaled: 0.5132515<br />1/2: 0.5","npv_sim:  -989.425004<br />scaled: 0.5108470<br />1/2: 0.5","npv_sim:  -995.460713<br />scaled: 0.5084394<br />1/2: 0.5","npv_sim: -1001.496421<br />scaled: 0.5060313<br />1/2: 0.5","npv_sim: -1007.532129<br />scaled: 0.5036200<br />1/2: 0.5","npv_sim: -1013.567838<br />scaled: 0.5012065<br />1/2: 0.5","npv_sim: -1019.603546<br />scaled: 0.4987907<br />1/2: 0.5","npv_sim: -1025.639254<br />scaled: 0.4963699<br />1/2: 0.5","npv_sim: -1031.674962<br />scaled: 0.4939490<br />1/2: 0.5","npv_sim: -1037.710671<br />scaled: 0.4915192<br />1/2: 0.5","npv_sim: -1043.746379<br />scaled: 0.4890894<br />1/2: 0.5","npv_sim: -1049.782087<br />scaled: 0.4866518<br />1/2: 0.5","npv_sim: -1055.817796<br />scaled: 0.4842114<br />1/2: 0.5","npv_sim: -1061.853504<br />scaled: 0.4817656<br />1/2: 0.5","npv_sim: -1067.889212<br />scaled: 0.4793129<br />1/2: 0.5","npv_sim: -1073.924921<br />scaled: 0.4768584<br />1/2: 0.5","npv_sim: -1079.960629<br />scaled: 0.4743920<br />1/2: 0.5","npv_sim: -1085.996337<br />scaled: 0.4719257<br />1/2: 0.5","npv_sim: -1092.032045<br />scaled: 0.4694467<br />1/2: 0.5","npv_sim: -1098.067754<br />scaled: 0.4669653<br />1/2: 0.5","npv_sim: -1104.103462<br />scaled: 0.4644750<br />1/2: 0.5","npv_sim: -1110.139170<br />scaled: 0.4619774<br />1/2: 0.5","npv_sim: -1116.174879<br />scaled: 0.4594755<br />1/2: 0.5","npv_sim: -1122.210587<br />scaled: 0.4569605<br />1/2: 0.5","npv_sim: -1128.246295<br />scaled: 0.4544456<br />1/2: 0.5","npv_sim: -1134.282003<br />scaled: 0.4519132<br />1/2: 0.5","npv_sim: -1140.317712<br />scaled: 0.4493800<br />1/2: 0.5","npv_sim: -1146.353420<br />scaled: 0.4468341<br />1/2: 0.5","npv_sim: -1152.389128<br />scaled: 0.4442818<br />1/2: 0.5","npv_sim: -1158.424837<br />scaled: 0.4417222<br />1/2: 0.5","npv_sim: -1164.460545<br />scaled: 0.4391501<br />1/2: 0.5","npv_sim: -1170.496253<br />scaled: 0.4365765<br />1/2: 0.5","npv_sim: -1176.531962<br />scaled: 0.4339840<br />1/2: 0.5","npv_sim: -1182.567670<br />scaled: 0.4313915<br />1/2: 0.5","npv_sim: -1188.603378<br />scaled: 0.4287828<br />1/2: 0.5","npv_sim: -1194.639086<br />scaled: 0.4261696<br />1/2: 0.5","npv_sim: -1200.674795<br />scaled: 0.4235462<br />1/2: 0.5","npv_sim: -1206.710503<br />scaled: 0.4209118<br />1/2: 0.5","npv_sim: -1212.746211<br />scaled: 0.4182736<br />1/2: 0.5","npv_sim: -1218.781920<br />scaled: 0.4156181<br />1/2: 0.5","npv_sim: -1224.817628<br />scaled: 0.4129626<br />1/2: 0.5","npv_sim: -1230.853336<br />scaled: 0.4102882<br />1/2: 0.5","npv_sim: -1236.889044<br />scaled: 0.4076116<br />1/2: 0.5","npv_sim: -1236.889044<br />scaled: 0.4076116<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,394.14456275297,394.14456275297],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 394.1446","xintercept: 394.1446"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.216844129454],"y":[0.25],"text":"Median NPV:<br />  394.14","hovertext":"x: 591.2168<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.216844129454],"y":[0.1],"text":"SD NPV:<br />  1008.86","hovertext":"x: 591.2168<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic Effects of Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1669.32166972514,2401.88428962853],"tickmode":"array","ticktext":["-1000","0","1000","2000"],"tickvals":[-1000,0,1000,2000],"categoryorder":"array","categoryarray":["-1000","0","1000","2000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"103062f744463":{"x":{},"y":{},"alpha":{},"type":"scatter"},"103067a35d7eb":{"xintercept":{}},"1030664f2c3fb":{"x":{},"y":{}},"103067e20c783":{"x":{},"y":{}}},"cur_data":"103062f744463","visdat":{"103062f744463":["function (y) ","x"],"103067a35d7eb":["function (y) ","x"],"1030664f2c3fb":["function (y) ","x"],"103067e20c783":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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
