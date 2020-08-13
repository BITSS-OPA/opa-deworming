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
    costs_data <- costs1_p1_f(df_costs_var = df_costs_var1,
                              df_costs_cw_var = df_costs_cw_var1,
                              df_counts_var = df_counts_var1,
                              staff_time_var = staff_time_var1)
    
    cost1_in <- costs1_p2_f(country_w_var = costs_data$country_w,
                            country_cost_var = costs_data$per_cap)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)

    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, ex_rate_var = 1)
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
<script type="application/json" data-for="htmlwidget-43fbbdc7f92cf116cfd3">{"x":{"data":[{"x":[-1236.89293396248,-1230.85722679493,-1224.82151962738,-1218.78581245983,-1212.75010529228,-1206.71439812473,-1200.67869095718,-1194.64298378963,-1188.60727662208,-1182.57156945453,-1176.53586228698,-1170.50015511943,-1164.46444795188,-1158.42874078432,-1152.39303361677,-1146.35732644922,-1140.32161928167,-1134.28591211412,-1128.25020494657,-1122.21449777902,-1116.17879061147,-1110.14308344392,-1104.10737627637,-1098.07166910882,-1092.03596194127,-1086.00025477372,-1079.96454760617,-1073.92884043862,-1067.89313327107,-1061.85742610352,-1055.82171893597,-1049.78601176842,-1043.75030460086,-1037.71459743331,-1031.67889026576,-1025.64318309821,-1019.60747593066,-1013.57176876311,-1007.53606159556,-1001.50035442801,-995.464647260461,-989.42894009291,-983.39323292536,-977.357525757809,-971.321818590259,-965.286111422708,-959.250404255158,-953.214697087607,-947.178989920057,-941.143282752506,-935.107575584956,-929.071868417405,-923.036161249855,-917.000454082304,-910.964746914754,-904.929039747203,-898.893332579652,-892.857625412102,-886.821918244552,-880.786211077001,-874.75050390945,-868.7147967419,-862.67908957435,-856.643382406799,-850.607675239248,-844.571968071698,-838.536260904147,-832.500553736597,-826.464846569046,-820.429139401496,-814.393432233945,-808.357725066395,-802.322017898844,-796.286310731294,-790.250603563743,-784.214896396193,-778.179189228642,-772.143482061092,-766.107774893541,-760.072067725991,-754.03636055844,-748.00065339089,-741.964946223339,-735.929239055789,-729.893531888238,-723.857824720688,-717.822117553137,-711.786410385587,-705.750703218036,-699.714996050486,-693.679288882935,-687.643581715385,-681.607874547834,-675.572167380284,-669.536460212733,-663.500753045182,-657.465045877632,-651.429338710081,-645.393631542531,-639.35792437498,-633.32221720743,-627.286510039879,-621.250802872329,-615.215095704778,-609.179388537228,-603.143681369677,-597.107974202127,-591.072267034576,-585.036559867026,-579.000852699475,-572.965145531925,-566.929438364374,-560.893731196824,-554.858024029273,-548.822316861723,-542.786609694172,-536.750902526622,-530.715195359071,-524.679488191521,-518.64378102397,-512.60807385642,-506.572366688869,-500.536659521319,-494.500952353768,-488.465245186218,-482.429538018667,-476.393830851117,-470.358123683566,-464.322416516016,-458.286709348465,-452.251002180914,-446.215295013364,-440.179587845813,-434.143880678263,-428.108173510713,-422.072466343162,-416.036759175611,-410.001052008061,-403.96534484051,-397.92963767296,-391.893930505409,-385.858223337859,-379.822516170308,-373.786809002758,-367.751101835207,-361.715394667657,-355.679687500106,-349.643980332556,-343.608273165005,-337.572565997455,-331.536858829904,-325.501151662354,-319.465444494803,-313.429737327253,-307.394030159702,-301.358322992152,-295.322615824601,-289.286908657051,-283.2512014895,-277.21549432195,-271.179787154399,-265.144079986849,-259.108372819298,-253.072665651748,-247.036958484197,-241.001251316647,-234.965544149096,-228.929836981546,-222.894129813995,-216.858422646445,-210.822715478894,-204.787008311343,-198.751301143793,-192.715593976242,-186.679886808692,-180.644179641141,-174.608472473591,-168.57276530604,-162.53705813849,-156.501350970939,-150.465643803389,-144.429936635838,-138.394229468288,-132.358522300737,-126.322815133187,-120.287107965636,-114.251400798086,-108.215693630535,-102.179986462985,-96.1442792954342,-90.1085721278837,-84.0728649603332,-78.0371577927826,-72.0014506252321,-65.9657434576816,-59.9300362901311,-53.8943291225805,-47.8586219550302,-41.8229147874797,-35.7872076199292,-29.7515004523787,-23.7157932848281,-17.6800861172776,-11.6443789497271,-5.60867178217654,0.427035385373983,6.46274255292451,12.498449720475,18.5341568880256,24.5698640555761,30.6055712231266,36.6412783906771,42.6769855582277,48.7126927257782,54.7483998933285,60.784107060879,66.8198142284295,72.8555213959801,78.8912285635306,84.9269357310811,90.9626428986317,96.9983500661822,103.034057233733,109.069764401283,115.105471568834,121.141178736384,127.176885903935,133.212593071485,139.248300239036,145.284007406586,151.319714574137,157.355421741687,163.391128909238,169.426836076788,175.462543244339,181.498250411889,187.53395757944,193.56966474699,199.605371914541,205.641079082091,211.676786249642,217.712493417192,223.748200584743,229.783907752294,235.819614919844,241.855322087395,247.891029254945,253.926736422496,259.962443590046,265.998150757596,272.033857925147,278.069565092698,284.105272260248,290.140979427799,296.176686595349,302.2123937629,308.24810093045,314.283808098001,320.319515265551,326.355222433102,332.390929600652,338.426636768203,344.462343935753,350.498051103304,356.533758270854,362.569465438405,368.605172605955,374.640879773506,380.676586941056,386.712294108607,392.748001276157,398.783708443708,404.819415611258,410.855122778809,416.890829946359,422.92653711391,428.96224428146,434.997951449011,441.033658616562,447.069365784112,453.105072951663,459.140780119213,465.176487286763,471.212194454314,477.247901621864,483.283608789415,489.319315956966,495.355023124516,501.390730292067,507.426437459617,513.462144627168,519.497851794718,525.533558962269,531.569266129819,537.60497329737,543.64068046492,549.676387632471,555.712094800021,561.747801967572,567.783509135122,573.819216302673,579.854923470223,585.890630637774,591.926337805324,597.962044972875,603.997752140425,610.033459307976,616.069166475526,622.104873643077,628.140580810627,634.176287978178,640.211995145728,646.247702313279,652.28340948083,658.31911664838,664.354823815931,670.390530983481,676.426238151031,682.461945318582,688.497652486132,694.533359653683,700.569066821233,706.604773988784,712.640481156335,718.676188323885,724.711895491436,730.747602658986,736.783309826537,742.819016994087,748.854724161638,754.890431329188,760.926138496739,766.961845664289,772.99755283184,779.03325999939,785.068967166941,791.104674334491,797.140381502042,803.176088669592,809.211795837143,815.247503004693,821.283210172244,827.318917339794,833.354624507345,839.390331674895,845.426038842446,851.461746009996,857.497453177547,863.533160345097,869.568867512648,875.604574680199,881.640281847749,887.6759890153,893.71169618285,899.747403350401,905.783110517951,911.818817685502,917.854524853052,923.890232020603,929.925939188153,935.961646355703,941.997353523254,948.033060690804,954.068767858355,960.104475025905,966.140182193456,972.175889361007,978.211596528557,984.247303696108,990.283010863658,996.318718031209,1002.35442519876,1008.39013236631,1014.42583953386,1020.46154670141,1026.49725386896,1032.53296103651,1038.56866820406,1044.60437537161,1050.64008253916,1056.67578970671,1062.71149687426,1068.74720404181,1074.78291120937,1080.81861837692,1086.85432554447,1092.89003271202,1098.92573987957,1104.96144704712,1110.99715421467,1117.03286138222,1123.06856854977,1129.10427571732,1135.13998288487,1141.17569005242,1147.21139721997,1153.24710438752,1159.28281155507,1165.31851872262,1171.35422589017,1177.38993305772,1183.42564022527,1189.46134739283,1195.49705456038,1201.53276172793,1207.56846889548,1213.60417606303,1219.63988323058,1225.67559039813,1231.71129756568,1237.74700473323,1243.78271190078,1249.81841906833,1255.85412623588,1261.88983340343,1267.92554057098,1273.96124773853,1279.99695490608,1286.03266207363,1292.06836924118,1298.10407640873,1304.13978357628,1310.17549074384,1316.21119791139,1322.24690507894,1328.28261224649,1334.31831941404,1340.35402658159,1346.38973374914,1352.42544091669,1358.46114808424,1364.49685525179,1370.53256241934,1376.56826958689,1382.60397675444,1388.63968392199,1394.67539108954,1400.71109825709,1406.74680542464,1412.78251259219,1418.81821975974,1424.8539269273,1430.88963409485,1436.9253412624,1442.96104842995,1448.9967555975,1455.03246276505,1461.0681699326,1467.10387710015,1473.1395842677,1479.17529143525,1485.2109986028,1491.24670577035,1497.2824129379,1503.31812010545,1509.353827273,1515.38953444055,1521.4252416081,1527.46094877565,1533.4966559432,1539.53236311076,1545.56807027831,1551.60377744586,1557.63948461341,1563.67519178096,1569.71089894851,1575.74660611606,1581.78231328361,1587.81802045116,1593.85372761871,1599.88943478626,1605.92514195381,1611.96084912136,1617.99655628891,1624.03226345646,1630.06797062401,1636.10367779156,1642.13938495911,1648.17509212666,1654.21079929421,1660.24650646177,1666.28221362932,1672.31792079687,1678.35362796442,1684.38933513197,1690.42504229952,1696.46074946707,1702.49645663462,1708.53216380217,1714.56787096972,1720.60357813727,1726.63928530482,1732.67499247237,1738.71069963992,1744.74640680747,1750.78211397502,1756.81782114257,1762.85352831012,1768.88923547767,1774.92494264522,1780.96064981278,1786.99635698033,1793.03206414788,1799.06777131543,1805.10347848298,1811.13918565053,1817.17489281808,1823.21059998563,1829.24630715318,1835.28201432073,1841.31772148828,1847.35342865583,1847.35342865583,1847.35342865583,1841.31772148828,1835.28201432073,1829.24630715318,1823.21059998563,1817.17489281808,1811.13918565053,1805.10347848298,1799.06777131543,1793.03206414788,1786.99635698033,1780.96064981278,1774.92494264522,1768.88923547767,1762.85352831012,1756.81782114257,1750.78211397502,1744.74640680747,1738.71069963992,1732.67499247237,1726.63928530482,1720.60357813727,1714.56787096972,1708.53216380217,1702.49645663462,1696.46074946707,1690.42504229952,1684.38933513197,1678.35362796442,1672.31792079687,1666.28221362932,1660.24650646177,1654.21079929421,1648.17509212666,1642.13938495911,1636.10367779156,1630.06797062401,1624.03226345646,1617.99655628891,1611.96084912136,1605.92514195381,1599.88943478626,1593.85372761871,1587.81802045116,1581.78231328361,1575.74660611606,1569.71089894851,1563.67519178096,1557.63948461341,1551.60377744586,1545.56807027831,1539.53236311076,1533.4966559432,1527.46094877565,1521.4252416081,1515.38953444055,1509.353827273,1503.31812010545,1497.2824129379,1491.24670577035,1485.2109986028,1479.17529143525,1473.1395842677,1467.10387710015,1461.0681699326,1455.03246276505,1448.9967555975,1442.96104842995,1436.9253412624,1430.88963409485,1424.8539269273,1418.81821975974,1412.78251259219,1406.74680542464,1400.71109825709,1394.67539108954,1388.63968392199,1382.60397675444,1376.56826958689,1370.53256241934,1364.49685525179,1358.46114808424,1352.42544091669,1346.38973374914,1340.35402658159,1334.31831941404,1328.28261224649,1322.24690507894,1316.21119791139,1310.17549074384,1304.13978357628,1298.10407640873,1292.06836924118,1286.03266207363,1279.99695490608,1273.96124773853,1267.92554057098,1261.88983340343,1255.85412623588,1249.81841906833,1243.78271190078,1237.74700473323,1231.71129756568,1225.67559039813,1219.63988323058,1213.60417606303,1207.56846889548,1201.53276172793,1195.49705456038,1189.46134739283,1183.42564022527,1177.38993305772,1171.35422589017,1165.31851872262,1159.28281155507,1153.24710438752,1147.21139721997,1141.17569005242,1135.13998288487,1129.10427571732,1123.06856854977,1117.03286138222,1110.99715421467,1104.96144704712,1098.92573987957,1092.89003271202,1086.85432554447,1080.81861837692,1074.78291120937,1068.74720404181,1062.71149687426,1056.67578970671,1050.64008253916,1044.60437537161,1038.56866820406,1032.53296103651,1026.49725386896,1020.46154670141,1014.42583953386,1008.39013236631,1002.35442519876,996.318718031209,990.283010863658,984.247303696108,978.211596528557,972.175889361007,966.140182193456,960.104475025905,954.068767858355,948.033060690804,941.997353523254,935.961646355703,929.925939188153,923.890232020603,917.854524853052,911.818817685502,905.783110517951,899.747403350401,893.71169618285,887.6759890153,881.640281847749,875.604574680199,869.568867512648,863.533160345097,857.497453177547,851.461746009996,845.426038842446,839.390331674895,833.354624507345,827.318917339794,821.283210172244,815.247503004693,809.211795837143,803.176088669592,797.140381502042,791.104674334491,785.068967166941,779.03325999939,772.99755283184,766.961845664289,760.926138496739,754.890431329188,748.854724161638,742.819016994087,736.783309826537,730.747602658986,724.711895491436,718.676188323885,712.640481156335,706.604773988784,700.569066821233,694.533359653683,688.497652486132,682.461945318582,676.426238151031,670.390530983481,664.354823815931,658.31911664838,652.28340948083,646.247702313279,640.211995145728,634.176287978178,628.140580810627,622.104873643077,616.069166475526,610.033459307976,603.997752140425,597.962044972875,591.926337805324,585.890630637774,579.854923470223,573.819216302673,567.783509135122,561.747801967572,555.712094800021,549.676387632471,543.64068046492,537.60497329737,531.569266129819,525.533558962269,519.497851794718,513.462144627168,507.426437459617,501.390730292067,495.355023124516,489.319315956966,483.283608789415,477.247901621864,471.212194454314,465.176487286763,459.140780119213,453.105072951663,447.069365784112,441.033658616562,434.997951449011,428.96224428146,422.92653711391,416.890829946359,410.855122778809,404.819415611258,398.783708443708,392.748001276157,386.712294108607,380.676586941056,374.640879773506,368.605172605955,362.569465438405,356.533758270854,350.498051103304,344.462343935753,338.426636768203,332.390929600652,326.355222433102,320.319515265551,314.283808098001,308.24810093045,302.2123937629,296.176686595349,290.140979427799,284.105272260248,278.069565092698,272.033857925147,265.998150757596,259.962443590046,253.926736422496,247.891029254945,241.855322087395,235.819614919844,229.783907752294,223.748200584743,217.712493417192,211.676786249642,205.641079082091,199.605371914541,193.56966474699,187.53395757944,181.498250411889,175.462543244339,169.426836076788,163.391128909238,157.355421741687,151.319714574137,145.284007406586,139.248300239036,133.212593071485,127.176885903935,121.141178736384,115.105471568834,109.069764401283,103.034057233733,96.9983500661822,90.9626428986317,84.9269357310811,78.8912285635306,72.8555213959801,66.8198142284295,60.784107060879,54.7483998933285,48.7126927257782,42.6769855582277,36.6412783906771,30.6055712231266,24.5698640555761,18.5341568880256,12.498449720475,6.46274255292451,0.427035385373983,-5.60867178217654,-11.6443789497271,-17.6800861172776,-23.7157932848281,-29.7515004523787,-35.7872076199292,-41.8229147874797,-47.8586219550302,-53.8943291225805,-59.9300362901311,-65.9657434576816,-72.0014506252321,-78.0371577927826,-84.0728649603332,-90.1085721278837,-96.1442792954342,-102.179986462985,-108.215693630535,-114.251400798086,-120.287107965636,-126.322815133187,-132.358522300737,-138.394229468288,-144.429936635838,-150.465643803389,-156.501350970939,-162.53705813849,-168.57276530604,-174.608472473591,-180.644179641141,-186.679886808692,-192.715593976242,-198.751301143793,-204.787008311343,-210.822715478894,-216.858422646445,-222.894129813995,-228.929836981546,-234.965544149096,-241.001251316647,-247.036958484197,-253.072665651748,-259.108372819298,-265.144079986849,-271.179787154399,-277.21549432195,-283.2512014895,-289.286908657051,-295.322615824601,-301.358322992152,-307.394030159702,-313.429737327253,-319.465444494803,-325.501151662354,-331.536858829904,-337.572565997455,-343.608273165005,-349.643980332556,-355.679687500106,-361.715394667657,-367.751101835207,-373.786809002758,-379.822516170308,-385.858223337859,-391.893930505409,-397.92963767296,-403.96534484051,-410.001052008061,-416.036759175611,-422.072466343162,-428.108173510713,-434.143880678263,-440.179587845813,-446.215295013364,-452.251002180914,-458.286709348465,-464.322416516016,-470.358123683566,-476.393830851117,-482.429538018667,-488.465245186218,-494.500952353768,-500.536659521319,-506.572366688869,-512.60807385642,-518.64378102397,-524.679488191521,-530.715195359071,-536.750902526622,-542.786609694172,-548.822316861723,-554.858024029273,-560.893731196824,-566.929438364374,-572.965145531925,-579.000852699475,-585.036559867026,-591.072267034576,-597.107974202127,-603.143681369677,-609.179388537228,-615.215095704778,-621.250802872329,-627.286510039879,-633.32221720743,-639.35792437498,-645.393631542531,-651.429338710081,-657.465045877632,-663.500753045182,-669.536460212733,-675.572167380284,-681.607874547834,-687.643581715385,-693.679288882935,-699.714996050486,-705.750703218036,-711.786410385587,-717.822117553137,-723.857824720688,-729.893531888238,-735.929239055789,-741.964946223339,-748.00065339089,-754.03636055844,-760.072067725991,-766.107774893541,-772.143482061092,-778.179189228642,-784.214896396193,-790.250603563743,-796.286310731294,-802.322017898844,-808.357725066395,-814.393432233945,-820.429139401496,-826.464846569046,-832.500553736597,-838.536260904147,-844.571968071698,-850.607675239248,-856.643382406799,-862.67908957435,-868.7147967419,-874.75050390945,-880.786211077001,-886.821918244552,-892.857625412102,-898.893332579652,-904.929039747203,-910.964746914754,-917.000454082304,-923.036161249855,-929.071868417405,-935.107575584956,-941.143282752506,-947.178989920057,-953.214697087607,-959.250404255158,-965.286111422708,-971.321818590259,-977.357525757809,-983.39323292536,-989.42894009291,-995.464647260461,-1001.50035442801,-1007.53606159556,-1013.57176876311,-1019.60747593066,-1025.64318309821,-1031.67889026576,-1037.71459743331,-1043.75030460086,-1049.78601176842,-1055.82171893597,-1061.85742610352,-1067.89313327107,-1073.92884043862,-1079.96454760617,-1086.00025477372,-1092.03596194127,-1098.07166910882,-1104.10737627637,-1110.14308344392,-1116.17879061147,-1122.21449777902,-1128.25020494657,-1134.28591211412,-1140.32161928167,-1146.35732644922,-1152.39303361677,-1158.42874078432,-1164.46444795188,-1170.50015511943,-1176.53586228698,-1182.57156945453,-1188.60727662208,-1194.64298378963,-1200.67869095718,-1206.71439812473,-1212.75010529228,-1218.78581245983,-1224.82151962738,-1230.85722679493,-1236.89293396248,-1236.89293396248],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.627171710248382,0.630736786718905,0.634293704504102,0.63778449010823,0.641275275712359,0.644704416738018,0.648119537837605,0.65149474221461,0.654833040972647,0.658153882770938,0.661414417510182,0.664674952249426,0.667862540240941,0.67104458485449,0.674176521276817,0.67727956370246,0.680355710488954,0.683379450264901,0.686399693465109,0.689344038390391,0.692288383315673,0.695173350918541,0.698038412846849,0.70086763585502,0.703653725656041,0.706427362116171,0.709134983843468,0.711842605570766,0.71448305916076,0.717112903469989,0.719699027218013,0.722251964083597,0.724784161337194,0.727261232119619,0.729738302902044,0.732142334983215,0.734544743902666,0.736897075423554,0.739226180498807,0.74152742392414,0.743784727438292,0.746035510850924,0.748222649407791,0.750409787964659,0.752542352609217,0.754661086845935,0.756746376189377,0.758798580211964,0.760837383538341,0.762825034147682,0.764812684757023,0.76674331924783,0.768668485023685,0.770556401624608,0.772421231934936,0.774267333897547,0.776074047899534,0.777879246180794,0.779630121878617,0.781380997576439,0.783092700505699,0.784790063928158,0.786465079296104,0.788111293854938,0.789750593959367,0.791348050046219,0.792945506133072,0.794503716705203,0.796054821581996,0.797581691453495,0.799088859487317,0.800585377428507,0.802051020718104,0.803516664007702,0.804944695021235,0.80637121445789,0.807773254010089,0.809163030810987,0.810540044413402,0.811895432160754,0.813248381591487,0.814571698805566,0.815895016019645,0.817195066596447,0.818488589865447,0.819768721585649,0.821034679269915,0.822295794978773,0.823536361501523,0.824776928024272,0.825996649124988,0.827213939856206,0.828418486165286,0.829614552900047,0.830804744582162,0.831981571609314,0.833158221054162,0.834317721806094,0.835477222558025,0.836625648340675,0.837769662609867,0.83890791304039,0.840038204743629,0.841166992942727,0.842285248409322,0.843403503875918,0.844513105564105,0.845620932320215,0.846723999924225,0.84782292594148,0.848920066452002,0.850011539825104,0.851103013198206,0.852188740084776,0.853274129099637,0.854356404145232,0.855436997696538,0.856516321192897,0.857593329507767,0.858670192784405,0.859744748407078,0.860819304029752,0.861892792594179,0.862965951585431,0.86403891565635,0.865111658960597,0.866184487709794,0.86725772264264,0.868330957575486,0.869405358419107,0.870479920228368,0.871555702672994,0.872632356127051,0.873709813679439,0.874789254638086,0.875868695596734,0.876951529818252,0.87803438674299,0.87912002158159,0.880206856944606,0.881295496797952,0.882386808351747,0.88347865031471,0.884574872187468,0.885671094060226,0.8867716094783,0.887873113063661,0.888977511662783,0.890084606276558,0.891193001072616,0.892305934353218,0.893418867633821,0.894537389314613,0.895656347348482,0.896779194468388,0.897904301626397,0.899031504174238,0.900162822650044,0.901294403452799,0.902431932499098,0.903569461545397,0.904711582299679,0.905855257163542,0.907001651218634,0.908151341782241,0.90930194530513,0.910457454443408,0.911612963581687,0.912773257853256,0.913934319525038,0.915098336679113,0.916264613773382,0.917432195724962,0.918603277688584,0.919774359652206,0.920949658269549,0.922125058547443,0.923303015950535,0.924482169273545,0.925662522334745,0.926844781894448,0.928027257872535,0.929211892425828,0.930396526979121,0.931582400442907,0.932768591392416,0.933955106700015,0.935141945199112,0.936328710773568,0.937515194907061,0.938701679040554,0.939886840833313,0.941071872926608,0.942255275011177,0.943437659121957,0.944618779210527,0.945797218857621,0.946975523083888,0.94814861926811,0.949321715452331,0.950489812022227,0.951656061593454,0.952818632158605,0.953976426582284,0.95513279713792,0.95628042171251,0.957428046287101,0.958565539698571,0.959701173273177,0.960829154744769,0.9619508699834,0.963068522912024,0.964174287147543,0.965280051383062,0.966368459509219,0.967456136226136,0.968530307482046,0.969597658462926,0.970656639164894,0.971701327330283,0.972744152722659,0.973763745689155,0.974783338655651,0.975781413514903,0.976773387890321,0.977750731780452,0.978712478209053,0.979668006734078,0.980596835698535,0.981525664662991,0.982422602954309,0.983315751334589,0.984185844905388,0.985040483288055,0.98588255502838,0.986695795753018,0.987508661948026,0.988277567870676,0.989046473793327,0.989781630368169,0.990503224314846,0.991203773580754,0.991875048457352,0.992539755813281,0.993157685331603,0.993775614849924,0.994346862006046,0.994908411979285,0.995438303039392,0.995940443194263,0.996427740270205,0.996867456517127,0.997307172764048,0.997685229947681,0.99805953704497,0.998389582052726,0.998695536595806,0.998976382700048,0.999211096362148,0.999441563919867,0.999602216866735,0.999762869813603,0.999864985274522,0.999948839672276,0.999995586467532,1,0.999990314109916,0.999912753419674,0.999835192729432,0.999683635649592,0.999521689615432,0.99930932623867,0.999060719001476,0.998786659144472,0.998449262793371,0.99811186644227,0.997684479965125,0.997256326796447,0.996763716248319,0.996243010511223,0.995684513518522,0.9950696425632,0.994444618859749,0.993734163606875,0.993023708354001,0.992234737941165,0.99142748259466,0.990569761040992,0.989664701970254,0.988737866638026,0.987734220360468,0.986730574082909,0.985635143534332,0.984532353734339,0.983366834258335,0.982164577784578,0.980928916851362,0.979627108640717,0.97832128117489,0.976920078057971,0.975518874941052,0.974043889396766,0.972543692747243,0.970999219259786,0.969400676457137,0.967787018503235,0.966091023186344,0.964395027869453,0.962616203712997,0.960823894809982,0.958977960987683,0.957090720515437,0.955178307998609,0.953197757633754,0.951217207268898,0.949147521423318,0.947075517854432,0.944942785127951,0.942781414207123,0.940586574732221,0.938338144451966,0.936082163103754,0.933749195433195,0.931416227762635,0.929018285150562,0.926603506772363,0.924149354643455,0.921655686640275,0.91914655933547,0.916577105881629,0.914007652427788,0.911372295972158,0.908730332092721,0.906045970785474,0.903334929250979,0.90060301560067,0.89782647303128,0.895048475697162,0.892210137954984,0.889371800212805,0.886491231506346,0.883594918459053,0.880675181410735,0.877724811153433,0.87476752264117,0.871767095266569,0.868766667891969,0.865727465182913,0.862681046192379,0.859611678960129,0.85652338240999,0.853425544473803,0.850299515952473,0.847173487431143,0.844015305001287,0.840855704533207,0.83767661251555,0.834487597487795,0.831289292282713,0.828075000489235,0.824859175037692,0.821623707934706,0.81838824083172,0.815139460936272,0.811886867200164,0.808627934034352,0.805362193510792,0.802094722132316,0.798819730261999,0.795544738391682,0.79226488047485,0.78898443327746,0.78570292776598,0.782420707459718,0.779139024775702,0.77585858610494,0.772578181967933,0.769302939306428,0.766027696644923,0.762758472110579,0.759491687402152,0.756229715005711,0.752974486598469,0.749721014885566,0.746480267309666,0.743239519733766,0.740013001232673,0.736789475965284,0.733576451661002,0.730372698915681,0.727174167004656,0.723992538575749,0.720810910146842,0.717652118047936,0.714494761233947,0.711354329274138,0.708223181897357,0.70510182934214,0.701998615875179,0.698897037372794,0.695823266176086,0.692749494979378,0.689699093658319,0.686656055326233,0.683627816186044,0.680616582992942,0.677610908360069,0.674632334791576,0.671653761223083,0.668704326169152,0.665759050518044,0.662833333117818,0.659921780118347,0.657019894620675,0.654142279089879,0.651264663559084,0.648420647075647,0.645576978525213,0.642756758721115,0.639946846898749,0.637150268568932,0.634373729883941,0.63160061641223,0.628856881196144,0.626113145980057,0.623395353645471,0.620683674195044,0.617988006102112,0.615307465448974,0.612633510829931,0.609983032178476,0.607332553527021,0.604708717947976,0.602087074720822,0.599482706025243,0.596888532453719,0.594303028221418,0.591734830417118,0.589167573980825,0.586623741423187,0.584079908865549,0.581552917080035,0.579031734454262,0.576519897621043,0.5740195569404,0.571522121873238,0.569040734819843,0.566559347766448,0.564092547768208,0.561628158186883,0.559172206930015,0.556722803410547,0.554276865237188,0.551840393721433,0.549403922205679,0.546978004093957,0.544552380445647,0.542132684428323,0.539715806905088,0.537301469712214,0.534891231281311,0.532481390095586,0.530075690536981,0.527669990978377,0.525266238608025,0.522862996350913,0.5204599559462,0.518057119587626,0.515653958885147,0.513249518342194,0.510845077799241,0.508437406068458,0.506029403318421,0.503618060544401,0.501204599910413,0.498788832145305,0.496368090088115,0.493947151857118,0.491517386236997,0.489087620616875,0.486650097796984,0.484209656573103,0.48176393813673,0.479311267474811,0.476856731114039,0.47439038287652,0.471924034639,0.46944505591247,0.466963694508945,0.464473468551298,0.461975877130514,0.459473936501554,0.456959022469694,0.454444108437834,0.451911713617951,0.449378513471473,0.446832680294508,0.444280363759196,0.441720801982985,0.439148675454296,0.436575110701483,0.433982619984576,0.431390129267669,0.428781525756127,0.426168258107221,0.423544879493002,0.420910564966886,0.418272327214468,0.415616839326034,0.4129613514376,0.410287030561969,0.407610386272624,0],"text":["npv_sim: -1236.8929340<br />scaled: 0.4076104<br />1/2: 0.5","npv_sim: -1230.8572268<br />scaled: 0.4102870<br />1/2: 0.5","npv_sim: -1224.8215196<br />scaled: 0.4129614<br />1/2: 0.5","npv_sim: -1218.7858125<br />scaled: 0.4156168<br />1/2: 0.5","npv_sim: -1212.7501053<br />scaled: 0.4182723<br />1/2: 0.5","npv_sim: -1206.7143981<br />scaled: 0.4209106<br />1/2: 0.5","npv_sim: -1200.6786910<br />scaled: 0.4235449<br />1/2: 0.5","npv_sim: -1194.6429838<br />scaled: 0.4261683<br />1/2: 0.5","npv_sim: -1188.6072766<br />scaled: 0.4287815<br />1/2: 0.5","npv_sim: -1182.5715695<br />scaled: 0.4313901<br />1/2: 0.5","npv_sim: -1176.5358623<br />scaled: 0.4339826<br />1/2: 0.5","npv_sim: -1170.5001551<br />scaled: 0.4365751<br />1/2: 0.5","npv_sim: -1164.4644480<br />scaled: 0.4391487<br />1/2: 0.5","npv_sim: -1158.4287408<br />scaled: 0.4417208<br />1/2: 0.5","npv_sim: -1152.3930336<br />scaled: 0.4442804<br />1/2: 0.5","npv_sim: -1146.3573264<br />scaled: 0.4468327<br />1/2: 0.5","npv_sim: -1140.3216193<br />scaled: 0.4493785<br />1/2: 0.5","npv_sim: -1134.2859121<br />scaled: 0.4519117<br />1/2: 0.5","npv_sim: -1128.2502049<br />scaled: 0.4544441<br />1/2: 0.5","npv_sim: -1122.2144978<br />scaled: 0.4569590<br />1/2: 0.5","npv_sim: -1116.1787906<br />scaled: 0.4594739<br />1/2: 0.5","npv_sim: -1110.1430834<br />scaled: 0.4619759<br />1/2: 0.5","npv_sim: -1104.1073763<br />scaled: 0.4644735<br />1/2: 0.5","npv_sim: -1098.0716691<br />scaled: 0.4669637<br />1/2: 0.5","npv_sim: -1092.0359619<br />scaled: 0.4694451<br />1/2: 0.5","npv_sim: -1086.0002548<br />scaled: 0.4719240<br />1/2: 0.5","npv_sim: -1079.9645476<br />scaled: 0.4743904<br />1/2: 0.5","npv_sim: -1073.9288404<br />scaled: 0.4768567<br />1/2: 0.5","npv_sim: -1067.8931333<br />scaled: 0.4793113<br />1/2: 0.5","npv_sim: -1061.8574261<br />scaled: 0.4817639<br />1/2: 0.5","npv_sim: -1055.8217189<br />scaled: 0.4842097<br />1/2: 0.5","npv_sim: -1049.7860118<br />scaled: 0.4866501<br />1/2: 0.5","npv_sim: -1043.7503046<br />scaled: 0.4890876<br />1/2: 0.5","npv_sim: -1037.7145974<br />scaled: 0.4915174<br />1/2: 0.5","npv_sim: -1031.6788903<br />scaled: 0.4939472<br />1/2: 0.5","npv_sim: -1025.6431831<br />scaled: 0.4963681<br />1/2: 0.5","npv_sim: -1019.6074759<br />scaled: 0.4987888<br />1/2: 0.5","npv_sim: -1013.5717688<br />scaled: 0.5012046<br />1/2: 0.5","npv_sim: -1007.5360616<br />scaled: 0.5036181<br />1/2: 0.5","npv_sim: -1001.5003544<br />scaled: 0.5060294<br />1/2: 0.5","npv_sim:  -995.4646473<br />scaled: 0.5084374<br />1/2: 0.5","npv_sim:  -989.4289401<br />scaled: 0.5108451<br />1/2: 0.5","npv_sim:  -983.3932329<br />scaled: 0.5132495<br />1/2: 0.5","npv_sim:  -977.3575258<br />scaled: 0.5156540<br />1/2: 0.5","npv_sim:  -971.3218186<br />scaled: 0.5180571<br />1/2: 0.5","npv_sim:  -965.2861114<br />scaled: 0.5204600<br />1/2: 0.5","npv_sim:  -959.2504043<br />scaled: 0.5228630<br />1/2: 0.5","npv_sim:  -953.2146971<br />scaled: 0.5252662<br />1/2: 0.5","npv_sim:  -947.1789899<br />scaled: 0.5276700<br />1/2: 0.5","npv_sim:  -941.1432828<br />scaled: 0.5300757<br />1/2: 0.5","npv_sim:  -935.1075756<br />scaled: 0.5324814<br />1/2: 0.5","npv_sim:  -929.0718684<br />scaled: 0.5348912<br />1/2: 0.5","npv_sim:  -923.0361612<br />scaled: 0.5373015<br />1/2: 0.5","npv_sim:  -917.0004541<br />scaled: 0.5397158<br />1/2: 0.5","npv_sim:  -910.9647469<br />scaled: 0.5421327<br />1/2: 0.5","npv_sim:  -904.9290397<br />scaled: 0.5445524<br />1/2: 0.5","npv_sim:  -898.8933326<br />scaled: 0.5469780<br />1/2: 0.5","npv_sim:  -892.8576254<br />scaled: 0.5494039<br />1/2: 0.5","npv_sim:  -886.8219182<br />scaled: 0.5518404<br />1/2: 0.5","npv_sim:  -880.7862111<br />scaled: 0.5542769<br />1/2: 0.5","npv_sim:  -874.7505039<br />scaled: 0.5567228<br />1/2: 0.5","npv_sim:  -868.7147967<br />scaled: 0.5591722<br />1/2: 0.5","npv_sim:  -862.6790896<br />scaled: 0.5616282<br />1/2: 0.5","npv_sim:  -856.6433824<br />scaled: 0.5640925<br />1/2: 0.5","npv_sim:  -850.6076752<br />scaled: 0.5665593<br />1/2: 0.5","npv_sim:  -844.5719681<br />scaled: 0.5690407<br />1/2: 0.5","npv_sim:  -838.5362609<br />scaled: 0.5715221<br />1/2: 0.5","npv_sim:  -832.5005537<br />scaled: 0.5740196<br />1/2: 0.5","npv_sim:  -826.4648466<br />scaled: 0.5765199<br />1/2: 0.5","npv_sim:  -820.4291394<br />scaled: 0.5790317<br />1/2: 0.5","npv_sim:  -814.3934322<br />scaled: 0.5815529<br />1/2: 0.5","npv_sim:  -808.3577251<br />scaled: 0.5840799<br />1/2: 0.5","npv_sim:  -802.3220179<br />scaled: 0.5866237<br />1/2: 0.5","npv_sim:  -796.2863107<br />scaled: 0.5891676<br />1/2: 0.5","npv_sim:  -790.2506036<br />scaled: 0.5917348<br />1/2: 0.5","npv_sim:  -784.2148964<br />scaled: 0.5943030<br />1/2: 0.5","npv_sim:  -778.1791892<br />scaled: 0.5968885<br />1/2: 0.5","npv_sim:  -772.1434821<br />scaled: 0.5994827<br />1/2: 0.5","npv_sim:  -766.1077749<br />scaled: 0.6020871<br />1/2: 0.5","npv_sim:  -760.0720677<br />scaled: 0.6047087<br />1/2: 0.5","npv_sim:  -754.0363606<br />scaled: 0.6073326<br />1/2: 0.5","npv_sim:  -748.0006534<br />scaled: 0.6099830<br />1/2: 0.5","npv_sim:  -741.9649462<br />scaled: 0.6126335<br />1/2: 0.5","npv_sim:  -735.9292391<br />scaled: 0.6153075<br />1/2: 0.5","npv_sim:  -729.8935319<br />scaled: 0.6179880<br />1/2: 0.5","npv_sim:  -723.8578247<br />scaled: 0.6206837<br />1/2: 0.5","npv_sim:  -717.8221176<br />scaled: 0.6233954<br />1/2: 0.5","npv_sim:  -711.7864104<br />scaled: 0.6261131<br />1/2: 0.5","npv_sim:  -705.7507032<br />scaled: 0.6288569<br />1/2: 0.5","npv_sim:  -699.7149961<br />scaled: 0.6316006<br />1/2: 0.5","npv_sim:  -693.6792889<br />scaled: 0.6343737<br />1/2: 0.5","npv_sim:  -687.6435817<br />scaled: 0.6371503<br />1/2: 0.5","npv_sim:  -681.6078745<br />scaled: 0.6399468<br />1/2: 0.5","npv_sim:  -675.5721674<br />scaled: 0.6427568<br />1/2: 0.5","npv_sim:  -669.5364602<br />scaled: 0.6455770<br />1/2: 0.5","npv_sim:  -663.5007530<br />scaled: 0.6484206<br />1/2: 0.5","npv_sim:  -657.4650459<br />scaled: 0.6512647<br />1/2: 0.5","npv_sim:  -651.4293387<br />scaled: 0.6541423<br />1/2: 0.5","npv_sim:  -645.3936315<br />scaled: 0.6570199<br />1/2: 0.5","npv_sim:  -639.3579244<br />scaled: 0.6599218<br />1/2: 0.5","npv_sim:  -633.3222172<br />scaled: 0.6628333<br />1/2: 0.5","npv_sim:  -627.2865100<br />scaled: 0.6657591<br />1/2: 0.5","npv_sim:  -621.2508029<br />scaled: 0.6687043<br />1/2: 0.5","npv_sim:  -615.2150957<br />scaled: 0.6716538<br />1/2: 0.5","npv_sim:  -609.1793885<br />scaled: 0.6746323<br />1/2: 0.5","npv_sim:  -603.1436814<br />scaled: 0.6776109<br />1/2: 0.5","npv_sim:  -597.1079742<br />scaled: 0.6806166<br />1/2: 0.5","npv_sim:  -591.0722670<br />scaled: 0.6836278<br />1/2: 0.5","npv_sim:  -585.0365599<br />scaled: 0.6866561<br />1/2: 0.5","npv_sim:  -579.0008527<br />scaled: 0.6896991<br />1/2: 0.5","npv_sim:  -572.9651455<br />scaled: 0.6927495<br />1/2: 0.5","npv_sim:  -566.9294384<br />scaled: 0.6958233<br />1/2: 0.5","npv_sim:  -560.8937312<br />scaled: 0.6988970<br />1/2: 0.5","npv_sim:  -554.8580240<br />scaled: 0.7019986<br />1/2: 0.5","npv_sim:  -548.8223169<br />scaled: 0.7051018<br />1/2: 0.5","npv_sim:  -542.7866097<br />scaled: 0.7082232<br />1/2: 0.5","npv_sim:  -536.7509025<br />scaled: 0.7113543<br />1/2: 0.5","npv_sim:  -530.7151954<br />scaled: 0.7144948<br />1/2: 0.5","npv_sim:  -524.6794882<br />scaled: 0.7176521<br />1/2: 0.5","npv_sim:  -518.6437810<br />scaled: 0.7208109<br />1/2: 0.5","npv_sim:  -512.6080739<br />scaled: 0.7239925<br />1/2: 0.5","npv_sim:  -506.5723667<br />scaled: 0.7271742<br />1/2: 0.5","npv_sim:  -500.5366595<br />scaled: 0.7303727<br />1/2: 0.5","npv_sim:  -494.5009524<br />scaled: 0.7335765<br />1/2: 0.5","npv_sim:  -488.4652452<br />scaled: 0.7367895<br />1/2: 0.5","npv_sim:  -482.4295380<br />scaled: 0.7400130<br />1/2: 0.5","npv_sim:  -476.3938309<br />scaled: 0.7432395<br />1/2: 0.5","npv_sim:  -470.3581237<br />scaled: 0.7464803<br />1/2: 0.5","npv_sim:  -464.3224165<br />scaled: 0.7497210<br />1/2: 0.5","npv_sim:  -458.2867093<br />scaled: 0.7529745<br />1/2: 0.5","npv_sim:  -452.2510022<br />scaled: 0.7562297<br />1/2: 0.5","npv_sim:  -446.2152950<br />scaled: 0.7594917<br />1/2: 0.5","npv_sim:  -440.1795878<br />scaled: 0.7627585<br />1/2: 0.5","npv_sim:  -434.1438807<br />scaled: 0.7660277<br />1/2: 0.5","npv_sim:  -428.1081735<br />scaled: 0.7693029<br />1/2: 0.5","npv_sim:  -422.0724663<br />scaled: 0.7725782<br />1/2: 0.5","npv_sim:  -416.0367592<br />scaled: 0.7758586<br />1/2: 0.5","npv_sim:  -410.0010520<br />scaled: 0.7791390<br />1/2: 0.5","npv_sim:  -403.9653448<br />scaled: 0.7824207<br />1/2: 0.5","npv_sim:  -397.9296377<br />scaled: 0.7857029<br />1/2: 0.5","npv_sim:  -391.8939305<br />scaled: 0.7889844<br />1/2: 0.5","npv_sim:  -385.8582233<br />scaled: 0.7922649<br />1/2: 0.5","npv_sim:  -379.8225162<br />scaled: 0.7955447<br />1/2: 0.5","npv_sim:  -373.7868090<br />scaled: 0.7988197<br />1/2: 0.5","npv_sim:  -367.7511018<br />scaled: 0.8020947<br />1/2: 0.5","npv_sim:  -361.7153947<br />scaled: 0.8053622<br />1/2: 0.5","npv_sim:  -355.6796875<br />scaled: 0.8086279<br />1/2: 0.5","npv_sim:  -349.6439803<br />scaled: 0.8118869<br />1/2: 0.5","npv_sim:  -343.6082732<br />scaled: 0.8151395<br />1/2: 0.5","npv_sim:  -337.5725660<br />scaled: 0.8183882<br />1/2: 0.5","npv_sim:  -331.5368588<br />scaled: 0.8216237<br />1/2: 0.5","npv_sim:  -325.5011517<br />scaled: 0.8248592<br />1/2: 0.5","npv_sim:  -319.4654445<br />scaled: 0.8280750<br />1/2: 0.5","npv_sim:  -313.4297373<br />scaled: 0.8312893<br />1/2: 0.5","npv_sim:  -307.3940302<br />scaled: 0.8344876<br />1/2: 0.5","npv_sim:  -301.3583230<br />scaled: 0.8376766<br />1/2: 0.5","npv_sim:  -295.3226158<br />scaled: 0.8408557<br />1/2: 0.5","npv_sim:  -289.2869087<br />scaled: 0.8440153<br />1/2: 0.5","npv_sim:  -283.2512015<br />scaled: 0.8471735<br />1/2: 0.5","npv_sim:  -277.2154943<br />scaled: 0.8502995<br />1/2: 0.5","npv_sim:  -271.1797872<br />scaled: 0.8534255<br />1/2: 0.5","npv_sim:  -265.1440800<br />scaled: 0.8565234<br />1/2: 0.5","npv_sim:  -259.1083728<br />scaled: 0.8596117<br />1/2: 0.5","npv_sim:  -253.0726657<br />scaled: 0.8626810<br />1/2: 0.5","npv_sim:  -247.0369585<br />scaled: 0.8657275<br />1/2: 0.5","npv_sim:  -241.0012513<br />scaled: 0.8687667<br />1/2: 0.5","npv_sim:  -234.9655441<br />scaled: 0.8717671<br />1/2: 0.5","npv_sim:  -228.9298370<br />scaled: 0.8747675<br />1/2: 0.5","npv_sim:  -222.8941298<br />scaled: 0.8777248<br />1/2: 0.5","npv_sim:  -216.8584226<br />scaled: 0.8806752<br />1/2: 0.5","npv_sim:  -210.8227155<br />scaled: 0.8835949<br />1/2: 0.5","npv_sim:  -204.7870083<br />scaled: 0.8864912<br />1/2: 0.5","npv_sim:  -198.7513011<br />scaled: 0.8893718<br />1/2: 0.5","npv_sim:  -192.7155940<br />scaled: 0.8922101<br />1/2: 0.5","npv_sim:  -186.6798868<br />scaled: 0.8950485<br />1/2: 0.5","npv_sim:  -180.6441796<br />scaled: 0.8978265<br />1/2: 0.5","npv_sim:  -174.6084725<br />scaled: 0.9006030<br />1/2: 0.5","npv_sim:  -168.5727653<br />scaled: 0.9033349<br />1/2: 0.5","npv_sim:  -162.5370581<br />scaled: 0.9060460<br />1/2: 0.5","npv_sim:  -156.5013510<br />scaled: 0.9087303<br />1/2: 0.5","npv_sim:  -150.4656438<br />scaled: 0.9113723<br />1/2: 0.5","npv_sim:  -144.4299366<br />scaled: 0.9140077<br />1/2: 0.5","npv_sim:  -138.3942295<br />scaled: 0.9165771<br />1/2: 0.5","npv_sim:  -132.3585223<br />scaled: 0.9191466<br />1/2: 0.5","npv_sim:  -126.3228151<br />scaled: 0.9216557<br />1/2: 0.5","npv_sim:  -120.2871080<br />scaled: 0.9241494<br />1/2: 0.5","npv_sim:  -114.2514008<br />scaled: 0.9266035<br />1/2: 0.5","npv_sim:  -108.2156936<br />scaled: 0.9290183<br />1/2: 0.5","npv_sim:  -102.1799865<br />scaled: 0.9314162<br />1/2: 0.5","npv_sim:   -96.1442793<br />scaled: 0.9337492<br />1/2: 0.5","npv_sim:   -90.1085721<br />scaled: 0.9360822<br />1/2: 0.5","npv_sim:   -84.0728650<br />scaled: 0.9383381<br />1/2: 0.5","npv_sim:   -78.0371578<br />scaled: 0.9405866<br />1/2: 0.5","npv_sim:   -72.0014506<br />scaled: 0.9427814<br />1/2: 0.5","npv_sim:   -65.9657435<br />scaled: 0.9449428<br />1/2: 0.5","npv_sim:   -59.9300363<br />scaled: 0.9470755<br />1/2: 0.5","npv_sim:   -53.8943291<br />scaled: 0.9491475<br />1/2: 0.5","npv_sim:   -47.8586220<br />scaled: 0.9512172<br />1/2: 0.5","npv_sim:   -41.8229148<br />scaled: 0.9531978<br />1/2: 0.5","npv_sim:   -35.7872076<br />scaled: 0.9551783<br />1/2: 0.5","npv_sim:   -29.7515005<br />scaled: 0.9570907<br />1/2: 0.5","npv_sim:   -23.7157933<br />scaled: 0.9589780<br />1/2: 0.5","npv_sim:   -17.6800861<br />scaled: 0.9608239<br />1/2: 0.5","npv_sim:   -11.6443789<br />scaled: 0.9626162<br />1/2: 0.5","npv_sim:    -5.6086718<br />scaled: 0.9643950<br />1/2: 0.5","npv_sim:     0.4270354<br />scaled: 0.9660910<br />1/2: 0.5","npv_sim:     6.4627426<br />scaled: 0.9677870<br />1/2: 0.5","npv_sim:    12.4984497<br />scaled: 0.9694007<br />1/2: 0.5","npv_sim:    18.5341569<br />scaled: 0.9709992<br />1/2: 0.5","npv_sim:    24.5698641<br />scaled: 0.9725437<br />1/2: 0.5","npv_sim:    30.6055712<br />scaled: 0.9740439<br />1/2: 0.5","npv_sim:    36.6412784<br />scaled: 0.9755189<br />1/2: 0.5","npv_sim:    42.6769856<br />scaled: 0.9769201<br />1/2: 0.5","npv_sim:    48.7126927<br />scaled: 0.9783213<br />1/2: 0.5","npv_sim:    54.7483999<br />scaled: 0.9796271<br />1/2: 0.5","npv_sim:    60.7841071<br />scaled: 0.9809289<br />1/2: 0.5","npv_sim:    66.8198142<br />scaled: 0.9821646<br />1/2: 0.5","npv_sim:    72.8555214<br />scaled: 0.9833668<br />1/2: 0.5","npv_sim:    78.8912286<br />scaled: 0.9845324<br />1/2: 0.5","npv_sim:    84.9269357<br />scaled: 0.9856351<br />1/2: 0.5","npv_sim:    90.9626429<br />scaled: 0.9867306<br />1/2: 0.5","npv_sim:    96.9983501<br />scaled: 0.9877342<br />1/2: 0.5","npv_sim:   103.0340572<br />scaled: 0.9887379<br />1/2: 0.5","npv_sim:   109.0697644<br />scaled: 0.9896647<br />1/2: 0.5","npv_sim:   115.1054716<br />scaled: 0.9905698<br />1/2: 0.5","npv_sim:   121.1411787<br />scaled: 0.9914275<br />1/2: 0.5","npv_sim:   127.1768859<br />scaled: 0.9922347<br />1/2: 0.5","npv_sim:   133.2125931<br />scaled: 0.9930237<br />1/2: 0.5","npv_sim:   139.2483002<br />scaled: 0.9937342<br />1/2: 0.5","npv_sim:   145.2840074<br />scaled: 0.9944446<br />1/2: 0.5","npv_sim:   151.3197146<br />scaled: 0.9950696<br />1/2: 0.5","npv_sim:   157.3554217<br />scaled: 0.9956845<br />1/2: 0.5","npv_sim:   163.3911289<br />scaled: 0.9962430<br />1/2: 0.5","npv_sim:   169.4268361<br />scaled: 0.9967637<br />1/2: 0.5","npv_sim:   175.4625432<br />scaled: 0.9972563<br />1/2: 0.5","npv_sim:   181.4982504<br />scaled: 0.9976845<br />1/2: 0.5","npv_sim:   187.5339576<br />scaled: 0.9981119<br />1/2: 0.5","npv_sim:   193.5696647<br />scaled: 0.9984493<br />1/2: 0.5","npv_sim:   199.6053719<br />scaled: 0.9987867<br />1/2: 0.5","npv_sim:   205.6410791<br />scaled: 0.9990607<br />1/2: 0.5","npv_sim:   211.6767862<br />scaled: 0.9993093<br />1/2: 0.5","npv_sim:   217.7124934<br />scaled: 0.9995217<br />1/2: 0.5","npv_sim:   223.7482006<br />scaled: 0.9996836<br />1/2: 0.5","npv_sim:   229.7839078<br />scaled: 0.9998352<br />1/2: 0.5","npv_sim:   235.8196149<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   241.8553221<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   247.8910293<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   253.9267364<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   259.9624436<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   265.9981508<br />scaled: 0.9998650<br />1/2: 0.5","npv_sim:   272.0338579<br />scaled: 0.9997629<br />1/2: 0.5","npv_sim:   278.0695651<br />scaled: 0.9996022<br />1/2: 0.5","npv_sim:   284.1052723<br />scaled: 0.9994416<br />1/2: 0.5","npv_sim:   290.1409794<br />scaled: 0.9992111<br />1/2: 0.5","npv_sim:   296.1766866<br />scaled: 0.9989764<br />1/2: 0.5","npv_sim:   302.2123938<br />scaled: 0.9986955<br />1/2: 0.5","npv_sim:   308.2481009<br />scaled: 0.9983896<br />1/2: 0.5","npv_sim:   314.2838081<br />scaled: 0.9980595<br />1/2: 0.5","npv_sim:   320.3195153<br />scaled: 0.9976852<br />1/2: 0.5","npv_sim:   326.3552224<br />scaled: 0.9973072<br />1/2: 0.5","npv_sim:   332.3909296<br />scaled: 0.9968675<br />1/2: 0.5","npv_sim:   338.4266368<br />scaled: 0.9964277<br />1/2: 0.5","npv_sim:   344.4623439<br />scaled: 0.9959404<br />1/2: 0.5","npv_sim:   350.4980511<br />scaled: 0.9954383<br />1/2: 0.5","npv_sim:   356.5337583<br />scaled: 0.9949084<br />1/2: 0.5","npv_sim:   362.5694654<br />scaled: 0.9943469<br />1/2: 0.5","npv_sim:   368.6051726<br />scaled: 0.9937756<br />1/2: 0.5","npv_sim:   374.6408798<br />scaled: 0.9931577<br />1/2: 0.5","npv_sim:   380.6765869<br />scaled: 0.9925398<br />1/2: 0.5","npv_sim:   386.7122941<br />scaled: 0.9918750<br />1/2: 0.5","npv_sim:   392.7480013<br />scaled: 0.9912038<br />1/2: 0.5","npv_sim:   398.7837084<br />scaled: 0.9905032<br />1/2: 0.5","npv_sim:   404.8194156<br />scaled: 0.9897816<br />1/2: 0.5","npv_sim:   410.8551228<br />scaled: 0.9890465<br />1/2: 0.5","npv_sim:   416.8908299<br />scaled: 0.9882776<br />1/2: 0.5","npv_sim:   422.9265371<br />scaled: 0.9875087<br />1/2: 0.5","npv_sim:   428.9622443<br />scaled: 0.9866958<br />1/2: 0.5","npv_sim:   434.9979514<br />scaled: 0.9858826<br />1/2: 0.5","npv_sim:   441.0336586<br />scaled: 0.9850405<br />1/2: 0.5","npv_sim:   447.0693658<br />scaled: 0.9841858<br />1/2: 0.5","npv_sim:   453.1050730<br />scaled: 0.9833158<br />1/2: 0.5","npv_sim:   459.1407801<br />scaled: 0.9824226<br />1/2: 0.5","npv_sim:   465.1764873<br />scaled: 0.9815257<br />1/2: 0.5","npv_sim:   471.2121945<br />scaled: 0.9805968<br />1/2: 0.5","npv_sim:   477.2479016<br />scaled: 0.9796680<br />1/2: 0.5","npv_sim:   483.2836088<br />scaled: 0.9787125<br />1/2: 0.5","npv_sim:   489.3193160<br />scaled: 0.9777507<br />1/2: 0.5","npv_sim:   495.3550231<br />scaled: 0.9767734<br />1/2: 0.5","npv_sim:   501.3907303<br />scaled: 0.9757814<br />1/2: 0.5","npv_sim:   507.4264375<br />scaled: 0.9747833<br />1/2: 0.5","npv_sim:   513.4621446<br />scaled: 0.9737637<br />1/2: 0.5","npv_sim:   519.4978518<br />scaled: 0.9727442<br />1/2: 0.5","npv_sim:   525.5335590<br />scaled: 0.9717013<br />1/2: 0.5","npv_sim:   531.5692661<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   537.6049733<br />scaled: 0.9695977<br />1/2: 0.5","npv_sim:   543.6406805<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   549.6763876<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   555.7120948<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   561.7478020<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   567.7835091<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   573.8192163<br />scaled: 0.9630685<br />1/2: 0.5","npv_sim:   579.8549235<br />scaled: 0.9619509<br />1/2: 0.5","npv_sim:   585.8906306<br />scaled: 0.9608292<br />1/2: 0.5","npv_sim:   591.9263378<br />scaled: 0.9597012<br />1/2: 0.5","npv_sim:   597.9620450<br />scaled: 0.9585655<br />1/2: 0.5","npv_sim:   603.9977521<br />scaled: 0.9574280<br />1/2: 0.5","npv_sim:   610.0334593<br />scaled: 0.9562804<br />1/2: 0.5","npv_sim:   616.0691665<br />scaled: 0.9551328<br />1/2: 0.5","npv_sim:   622.1048736<br />scaled: 0.9539764<br />1/2: 0.5","npv_sim:   628.1405808<br />scaled: 0.9528186<br />1/2: 0.5","npv_sim:   634.1762880<br />scaled: 0.9516561<br />1/2: 0.5","npv_sim:   640.2119951<br />scaled: 0.9504898<br />1/2: 0.5","npv_sim:   646.2477023<br />scaled: 0.9493217<br />1/2: 0.5","npv_sim:   652.2834095<br />scaled: 0.9481486<br />1/2: 0.5","npv_sim:   658.3191166<br />scaled: 0.9469755<br />1/2: 0.5","npv_sim:   664.3548238<br />scaled: 0.9457972<br />1/2: 0.5","npv_sim:   670.3905310<br />scaled: 0.9446188<br />1/2: 0.5","npv_sim:   676.4262382<br />scaled: 0.9434377<br />1/2: 0.5","npv_sim:   682.4619453<br />scaled: 0.9422553<br />1/2: 0.5","npv_sim:   688.4976525<br />scaled: 0.9410719<br />1/2: 0.5","npv_sim:   694.5333597<br />scaled: 0.9398868<br />1/2: 0.5","npv_sim:   700.5690668<br />scaled: 0.9387017<br />1/2: 0.5","npv_sim:   706.6047740<br />scaled: 0.9375152<br />1/2: 0.5","npv_sim:   712.6404812<br />scaled: 0.9363287<br />1/2: 0.5","npv_sim:   718.6761883<br />scaled: 0.9351419<br />1/2: 0.5","npv_sim:   724.7118955<br />scaled: 0.9339551<br />1/2: 0.5","npv_sim:   730.7476027<br />scaled: 0.9327686<br />1/2: 0.5","npv_sim:   736.7833098<br />scaled: 0.9315824<br />1/2: 0.5","npv_sim:   742.8190170<br />scaled: 0.9303965<br />1/2: 0.5","npv_sim:   748.8547242<br />scaled: 0.9292119<br />1/2: 0.5","npv_sim:   754.8904313<br />scaled: 0.9280273<br />1/2: 0.5","npv_sim:   760.9261385<br />scaled: 0.9268448<br />1/2: 0.5","npv_sim:   766.9618457<br />scaled: 0.9256625<br />1/2: 0.5","npv_sim:   772.9975528<br />scaled: 0.9244822<br />1/2: 0.5","npv_sim:   779.0332600<br />scaled: 0.9233030<br />1/2: 0.5","npv_sim:   785.0689672<br />scaled: 0.9221251<br />1/2: 0.5","npv_sim:   791.1046743<br />scaled: 0.9209497<br />1/2: 0.5","npv_sim:   797.1403815<br />scaled: 0.9197744<br />1/2: 0.5","npv_sim:   803.1760887<br />scaled: 0.9186033<br />1/2: 0.5","npv_sim:   809.2117958<br />scaled: 0.9174322<br />1/2: 0.5","npv_sim:   815.2475030<br />scaled: 0.9162646<br />1/2: 0.5","npv_sim:   821.2832102<br />scaled: 0.9150983<br />1/2: 0.5","npv_sim:   827.3189173<br />scaled: 0.9139343<br />1/2: 0.5","npv_sim:   833.3546245<br />scaled: 0.9127733<br />1/2: 0.5","npv_sim:   839.3903317<br />scaled: 0.9116130<br />1/2: 0.5","npv_sim:   845.4260388<br />scaled: 0.9104575<br />1/2: 0.5","npv_sim:   851.4617460<br />scaled: 0.9093019<br />1/2: 0.5","npv_sim:   857.4974532<br />scaled: 0.9081513<br />1/2: 0.5","npv_sim:   863.5331603<br />scaled: 0.9070017<br />1/2: 0.5","npv_sim:   869.5688675<br />scaled: 0.9058553<br />1/2: 0.5","npv_sim:   875.6045747<br />scaled: 0.9047116<br />1/2: 0.5","npv_sim:   881.6402818<br />scaled: 0.9035695<br />1/2: 0.5","npv_sim:   887.6759890<br />scaled: 0.9024319<br />1/2: 0.5","npv_sim:   893.7116962<br />scaled: 0.9012944<br />1/2: 0.5","npv_sim:   899.7474034<br />scaled: 0.9001628<br />1/2: 0.5","npv_sim:   905.7831105<br />scaled: 0.8990315<br />1/2: 0.5","npv_sim:   911.8188177<br />scaled: 0.8979043<br />1/2: 0.5","npv_sim:   917.8545249<br />scaled: 0.8967792<br />1/2: 0.5","npv_sim:   923.8902320<br />scaled: 0.8956563<br />1/2: 0.5","npv_sim:   929.9259392<br />scaled: 0.8945374<br />1/2: 0.5","npv_sim:   935.9616464<br />scaled: 0.8934189<br />1/2: 0.5","npv_sim:   941.9973535<br />scaled: 0.8923059<br />1/2: 0.5","npv_sim:   948.0330607<br />scaled: 0.8911930<br />1/2: 0.5","npv_sim:   954.0687679<br />scaled: 0.8900846<br />1/2: 0.5","npv_sim:   960.1044750<br />scaled: 0.8889775<br />1/2: 0.5","npv_sim:   966.1401822<br />scaled: 0.8878731<br />1/2: 0.5","npv_sim:   972.1758894<br />scaled: 0.8867716<br />1/2: 0.5","npv_sim:   978.2115965<br />scaled: 0.8856711<br />1/2: 0.5","npv_sim:   984.2473037<br />scaled: 0.8845749<br />1/2: 0.5","npv_sim:   990.2830109<br />scaled: 0.8834787<br />1/2: 0.5","npv_sim:   996.3187180<br />scaled: 0.8823868<br />1/2: 0.5","npv_sim:  1002.3544252<br />scaled: 0.8812955<br />1/2: 0.5","npv_sim:  1008.3901324<br />scaled: 0.8802069<br />1/2: 0.5","npv_sim:  1014.4258395<br />scaled: 0.8791200<br />1/2: 0.5","npv_sim:  1020.4615467<br />scaled: 0.8780344<br />1/2: 0.5","npv_sim:  1026.4972539<br />scaled: 0.8769515<br />1/2: 0.5","npv_sim:  1032.5329610<br />scaled: 0.8758687<br />1/2: 0.5","npv_sim:  1038.5686682<br />scaled: 0.8747893<br />1/2: 0.5","npv_sim:  1044.6043754<br />scaled: 0.8737098<br />1/2: 0.5","npv_sim:  1050.6400825<br />scaled: 0.8726324<br />1/2: 0.5","npv_sim:  1056.6757897<br />scaled: 0.8715557<br />1/2: 0.5","npv_sim:  1062.7114969<br />scaled: 0.8704799<br />1/2: 0.5","npv_sim:  1068.7472040<br />scaled: 0.8694054<br />1/2: 0.5","npv_sim:  1074.7829112<br />scaled: 0.8683310<br />1/2: 0.5","npv_sim:  1080.8186184<br />scaled: 0.8672577<br />1/2: 0.5","npv_sim:  1086.8543255<br />scaled: 0.8661845<br />1/2: 0.5","npv_sim:  1092.8900327<br />scaled: 0.8651117<br />1/2: 0.5","npv_sim:  1098.9257399<br />scaled: 0.8640389<br />1/2: 0.5","npv_sim:  1104.9614470<br />scaled: 0.8629660<br />1/2: 0.5","npv_sim:  1110.9971542<br />scaled: 0.8618928<br />1/2: 0.5","npv_sim:  1117.0328614<br />scaled: 0.8608193<br />1/2: 0.5","npv_sim:  1123.0685685<br />scaled: 0.8597447<br />1/2: 0.5","npv_sim:  1129.1042757<br />scaled: 0.8586702<br />1/2: 0.5","npv_sim:  1135.1399829<br />scaled: 0.8575933<br />1/2: 0.5","npv_sim:  1141.1756901<br />scaled: 0.8565163<br />1/2: 0.5","npv_sim:  1147.2113972<br />scaled: 0.8554370<br />1/2: 0.5","npv_sim:  1153.2471044<br />scaled: 0.8543564<br />1/2: 0.5","npv_sim:  1159.2828116<br />scaled: 0.8532741<br />1/2: 0.5","npv_sim:  1165.3185187<br />scaled: 0.8521887<br />1/2: 0.5","npv_sim:  1171.3542259<br />scaled: 0.8511030<br />1/2: 0.5","npv_sim:  1177.3899331<br />scaled: 0.8500115<br />1/2: 0.5","npv_sim:  1183.4256402<br />scaled: 0.8489201<br />1/2: 0.5","npv_sim:  1189.4613474<br />scaled: 0.8478229<br />1/2: 0.5","npv_sim:  1195.4970546<br />scaled: 0.8467240<br />1/2: 0.5","npv_sim:  1201.5327617<br />scaled: 0.8456209<br />1/2: 0.5","npv_sim:  1207.5684689<br />scaled: 0.8445131<br />1/2: 0.5","npv_sim:  1213.6041761<br />scaled: 0.8434035<br />1/2: 0.5","npv_sim:  1219.6398832<br />scaled: 0.8422852<br />1/2: 0.5","npv_sim:  1225.6755904<br />scaled: 0.8411670<br />1/2: 0.5","npv_sim:  1231.7112976<br />scaled: 0.8400382<br />1/2: 0.5","npv_sim:  1237.7470047<br />scaled: 0.8389079<br />1/2: 0.5","npv_sim:  1243.7827119<br />scaled: 0.8377697<br />1/2: 0.5","npv_sim:  1249.8184191<br />scaled: 0.8366256<br />1/2: 0.5","npv_sim:  1255.8541262<br />scaled: 0.8354772<br />1/2: 0.5","npv_sim:  1261.8898334<br />scaled: 0.8343177<br />1/2: 0.5","npv_sim:  1267.9255406<br />scaled: 0.8331582<br />1/2: 0.5","npv_sim:  1273.9612477<br />scaled: 0.8319816<br />1/2: 0.5","npv_sim:  1279.9969549<br />scaled: 0.8308047<br />1/2: 0.5","npv_sim:  1286.0326621<br />scaled: 0.8296146<br />1/2: 0.5","npv_sim:  1292.0683692<br />scaled: 0.8284185<br />1/2: 0.5","npv_sim:  1298.1040764<br />scaled: 0.8272139<br />1/2: 0.5","npv_sim:  1304.1397836<br />scaled: 0.8259966<br />1/2: 0.5","npv_sim:  1310.1754907<br />scaled: 0.8247769<br />1/2: 0.5","npv_sim:  1316.2111979<br />scaled: 0.8235364<br />1/2: 0.5","npv_sim:  1322.2469051<br />scaled: 0.8222958<br />1/2: 0.5","npv_sim:  1328.2826122<br />scaled: 0.8210347<br />1/2: 0.5","npv_sim:  1334.3183194<br />scaled: 0.8197687<br />1/2: 0.5","npv_sim:  1340.3540266<br />scaled: 0.8184886<br />1/2: 0.5","npv_sim:  1346.3897337<br />scaled: 0.8171951<br />1/2: 0.5","npv_sim:  1352.4254409<br />scaled: 0.8158950<br />1/2: 0.5","npv_sim:  1358.4611481<br />scaled: 0.8145717<br />1/2: 0.5","npv_sim:  1364.4968553<br />scaled: 0.8132484<br />1/2: 0.5","npv_sim:  1370.5325624<br />scaled: 0.8118954<br />1/2: 0.5","npv_sim:  1376.5682696<br />scaled: 0.8105400<br />1/2: 0.5","npv_sim:  1382.6039768<br />scaled: 0.8091630<br />1/2: 0.5","npv_sim:  1388.6396839<br />scaled: 0.8077733<br />1/2: 0.5","npv_sim:  1394.6753911<br />scaled: 0.8063712<br />1/2: 0.5","npv_sim:  1400.7110983<br />scaled: 0.8049447<br />1/2: 0.5","npv_sim:  1406.7468054<br />scaled: 0.8035167<br />1/2: 0.5","npv_sim:  1412.7825126<br />scaled: 0.8020510<br />1/2: 0.5","npv_sim:  1418.8182198<br />scaled: 0.8005854<br />1/2: 0.5","npv_sim:  1424.8539269<br />scaled: 0.7990889<br />1/2: 0.5","npv_sim:  1430.8896341<br />scaled: 0.7975817<br />1/2: 0.5","npv_sim:  1436.9253413<br />scaled: 0.7960548<br />1/2: 0.5","npv_sim:  1442.9610484<br />scaled: 0.7945037<br />1/2: 0.5","npv_sim:  1448.9967556<br />scaled: 0.7929455<br />1/2: 0.5","npv_sim:  1455.0324628<br />scaled: 0.7913481<br />1/2: 0.5","npv_sim:  1461.0681699<br />scaled: 0.7897506<br />1/2: 0.5","npv_sim:  1467.1038771<br />scaled: 0.7881113<br />1/2: 0.5","npv_sim:  1473.1395843<br />scaled: 0.7864651<br />1/2: 0.5","npv_sim:  1479.1752914<br />scaled: 0.7847901<br />1/2: 0.5","npv_sim:  1485.2109986<br />scaled: 0.7830927<br />1/2: 0.5","npv_sim:  1491.2467058<br />scaled: 0.7813810<br />1/2: 0.5","npv_sim:  1497.2824129<br />scaled: 0.7796301<br />1/2: 0.5","npv_sim:  1503.3181201<br />scaled: 0.7778792<br />1/2: 0.5","npv_sim:  1509.3538273<br />scaled: 0.7760740<br />1/2: 0.5","npv_sim:  1515.3895344<br />scaled: 0.7742673<br />1/2: 0.5","npv_sim:  1521.4252416<br />scaled: 0.7724212<br />1/2: 0.5","npv_sim:  1527.4609488<br />scaled: 0.7705564<br />1/2: 0.5","npv_sim:  1533.4966559<br />scaled: 0.7686685<br />1/2: 0.5","npv_sim:  1539.5323631<br />scaled: 0.7667433<br />1/2: 0.5","npv_sim:  1545.5680703<br />scaled: 0.7648127<br />1/2: 0.5","npv_sim:  1551.6037774<br />scaled: 0.7628250<br />1/2: 0.5","npv_sim:  1557.6394846<br />scaled: 0.7608374<br />1/2: 0.5","npv_sim:  1563.6751918<br />scaled: 0.7587986<br />1/2: 0.5","npv_sim:  1569.7108989<br />scaled: 0.7567464<br />1/2: 0.5","npv_sim:  1575.7466061<br />scaled: 0.7546611<br />1/2: 0.5","npv_sim:  1581.7823133<br />scaled: 0.7525424<br />1/2: 0.5","npv_sim:  1587.8180205<br />scaled: 0.7504098<br />1/2: 0.5","npv_sim:  1593.8537276<br />scaled: 0.7482226<br />1/2: 0.5","npv_sim:  1599.8894348<br />scaled: 0.7460355<br />1/2: 0.5","npv_sim:  1605.9251420<br />scaled: 0.7437847<br />1/2: 0.5","npv_sim:  1611.9608491<br />scaled: 0.7415274<br />1/2: 0.5","npv_sim:  1617.9965563<br />scaled: 0.7392262<br />1/2: 0.5","npv_sim:  1624.0322635<br />scaled: 0.7368971<br />1/2: 0.5","npv_sim:  1630.0679706<br />scaled: 0.7345447<br />1/2: 0.5","npv_sim:  1636.1036778<br />scaled: 0.7321423<br />1/2: 0.5","npv_sim:  1642.1393850<br />scaled: 0.7297383<br />1/2: 0.5","npv_sim:  1648.1750921<br />scaled: 0.7272612<br />1/2: 0.5","npv_sim:  1654.2107993<br />scaled: 0.7247842<br />1/2: 0.5","npv_sim:  1660.2465065<br />scaled: 0.7222520<br />1/2: 0.5","npv_sim:  1666.2822136<br />scaled: 0.7196990<br />1/2: 0.5","npv_sim:  1672.3179208<br />scaled: 0.7171129<br />1/2: 0.5","npv_sim:  1678.3536280<br />scaled: 0.7144831<br />1/2: 0.5","npv_sim:  1684.3893351<br />scaled: 0.7118426<br />1/2: 0.5","npv_sim:  1690.4250423<br />scaled: 0.7091350<br />1/2: 0.5","npv_sim:  1696.4607495<br />scaled: 0.7064274<br />1/2: 0.5","npv_sim:  1702.4964566<br />scaled: 0.7036537<br />1/2: 0.5","npv_sim:  1708.5321638<br />scaled: 0.7008676<br />1/2: 0.5","npv_sim:  1714.5678710<br />scaled: 0.6980384<br />1/2: 0.5","npv_sim:  1720.6035781<br />scaled: 0.6951734<br />1/2: 0.5","npv_sim:  1726.6392853<br />scaled: 0.6922884<br />1/2: 0.5","npv_sim:  1732.6749925<br />scaled: 0.6893440<br />1/2: 0.5","npv_sim:  1738.7106996<br />scaled: 0.6863997<br />1/2: 0.5","npv_sim:  1744.7464068<br />scaled: 0.6833795<br />1/2: 0.5","npv_sim:  1750.7821140<br />scaled: 0.6803557<br />1/2: 0.5","npv_sim:  1756.8178211<br />scaled: 0.6772796<br />1/2: 0.5","npv_sim:  1762.8535283<br />scaled: 0.6741765<br />1/2: 0.5","npv_sim:  1768.8892355<br />scaled: 0.6710446<br />1/2: 0.5","npv_sim:  1774.9249426<br />scaled: 0.6678625<br />1/2: 0.5","npv_sim:  1780.9606498<br />scaled: 0.6646750<br />1/2: 0.5","npv_sim:  1786.9963570<br />scaled: 0.6614144<br />1/2: 0.5","npv_sim:  1793.0320641<br />scaled: 0.6581539<br />1/2: 0.5","npv_sim:  1799.0677713<br />scaled: 0.6548330<br />1/2: 0.5","npv_sim:  1805.1034785<br />scaled: 0.6514947<br />1/2: 0.5","npv_sim:  1811.1391857<br />scaled: 0.6481195<br />1/2: 0.5","npv_sim:  1817.1748928<br />scaled: 0.6447044<br />1/2: 0.5","npv_sim:  1823.2106000<br />scaled: 0.6412753<br />1/2: 0.5","npv_sim:  1829.2463072<br />scaled: 0.6377845<br />1/2: 0.5","npv_sim:  1835.2820143<br />scaled: 0.6342937<br />1/2: 0.5","npv_sim:  1841.3177215<br />scaled: 0.6307368<br />1/2: 0.5","npv_sim:  1847.3534287<br />scaled: 0.6271717<br />1/2: 0.5","npv_sim:  1847.3534287<br />scaled: 0.6271717<br />1/2: 0.5","npv_sim:  1847.3534287<br />scaled: 0.6271717<br />1/2: 0.5","npv_sim:  1841.3177215<br />scaled: 0.6307368<br />1/2: 0.5","npv_sim:  1835.2820143<br />scaled: 0.6342937<br />1/2: 0.5","npv_sim:  1829.2463072<br />scaled: 0.6377845<br />1/2: 0.5","npv_sim:  1823.2106000<br />scaled: 0.6412753<br />1/2: 0.5","npv_sim:  1817.1748928<br />scaled: 0.6447044<br />1/2: 0.5","npv_sim:  1811.1391857<br />scaled: 0.6481195<br />1/2: 0.5","npv_sim:  1805.1034785<br />scaled: 0.6514947<br />1/2: 0.5","npv_sim:  1799.0677713<br />scaled: 0.6548330<br />1/2: 0.5","npv_sim:  1793.0320641<br />scaled: 0.6581539<br />1/2: 0.5","npv_sim:  1786.9963570<br />scaled: 0.6614144<br />1/2: 0.5","npv_sim:  1780.9606498<br />scaled: 0.6646750<br />1/2: 0.5","npv_sim:  1774.9249426<br />scaled: 0.6678625<br />1/2: 0.5","npv_sim:  1768.8892355<br />scaled: 0.6710446<br />1/2: 0.5","npv_sim:  1762.8535283<br />scaled: 0.6741765<br />1/2: 0.5","npv_sim:  1756.8178211<br />scaled: 0.6772796<br />1/2: 0.5","npv_sim:  1750.7821140<br />scaled: 0.6803557<br />1/2: 0.5","npv_sim:  1744.7464068<br />scaled: 0.6833795<br />1/2: 0.5","npv_sim:  1738.7106996<br />scaled: 0.6863997<br />1/2: 0.5","npv_sim:  1732.6749925<br />scaled: 0.6893440<br />1/2: 0.5","npv_sim:  1726.6392853<br />scaled: 0.6922884<br />1/2: 0.5","npv_sim:  1720.6035781<br />scaled: 0.6951734<br />1/2: 0.5","npv_sim:  1714.5678710<br />scaled: 0.6980384<br />1/2: 0.5","npv_sim:  1708.5321638<br />scaled: 0.7008676<br />1/2: 0.5","npv_sim:  1702.4964566<br />scaled: 0.7036537<br />1/2: 0.5","npv_sim:  1696.4607495<br />scaled: 0.7064274<br />1/2: 0.5","npv_sim:  1690.4250423<br />scaled: 0.7091350<br />1/2: 0.5","npv_sim:  1684.3893351<br />scaled: 0.7118426<br />1/2: 0.5","npv_sim:  1678.3536280<br />scaled: 0.7144831<br />1/2: 0.5","npv_sim:  1672.3179208<br />scaled: 0.7171129<br />1/2: 0.5","npv_sim:  1666.2822136<br />scaled: 0.7196990<br />1/2: 0.5","npv_sim:  1660.2465065<br />scaled: 0.7222520<br />1/2: 0.5","npv_sim:  1654.2107993<br />scaled: 0.7247842<br />1/2: 0.5","npv_sim:  1648.1750921<br />scaled: 0.7272612<br />1/2: 0.5","npv_sim:  1642.1393850<br />scaled: 0.7297383<br />1/2: 0.5","npv_sim:  1636.1036778<br />scaled: 0.7321423<br />1/2: 0.5","npv_sim:  1630.0679706<br />scaled: 0.7345447<br />1/2: 0.5","npv_sim:  1624.0322635<br />scaled: 0.7368971<br />1/2: 0.5","npv_sim:  1617.9965563<br />scaled: 0.7392262<br />1/2: 0.5","npv_sim:  1611.9608491<br />scaled: 0.7415274<br />1/2: 0.5","npv_sim:  1605.9251420<br />scaled: 0.7437847<br />1/2: 0.5","npv_sim:  1599.8894348<br />scaled: 0.7460355<br />1/2: 0.5","npv_sim:  1593.8537276<br />scaled: 0.7482226<br />1/2: 0.5","npv_sim:  1587.8180205<br />scaled: 0.7504098<br />1/2: 0.5","npv_sim:  1581.7823133<br />scaled: 0.7525424<br />1/2: 0.5","npv_sim:  1575.7466061<br />scaled: 0.7546611<br />1/2: 0.5","npv_sim:  1569.7108989<br />scaled: 0.7567464<br />1/2: 0.5","npv_sim:  1563.6751918<br />scaled: 0.7587986<br />1/2: 0.5","npv_sim:  1557.6394846<br />scaled: 0.7608374<br />1/2: 0.5","npv_sim:  1551.6037774<br />scaled: 0.7628250<br />1/2: 0.5","npv_sim:  1545.5680703<br />scaled: 0.7648127<br />1/2: 0.5","npv_sim:  1539.5323631<br />scaled: 0.7667433<br />1/2: 0.5","npv_sim:  1533.4966559<br />scaled: 0.7686685<br />1/2: 0.5","npv_sim:  1527.4609488<br />scaled: 0.7705564<br />1/2: 0.5","npv_sim:  1521.4252416<br />scaled: 0.7724212<br />1/2: 0.5","npv_sim:  1515.3895344<br />scaled: 0.7742673<br />1/2: 0.5","npv_sim:  1509.3538273<br />scaled: 0.7760740<br />1/2: 0.5","npv_sim:  1503.3181201<br />scaled: 0.7778792<br />1/2: 0.5","npv_sim:  1497.2824129<br />scaled: 0.7796301<br />1/2: 0.5","npv_sim:  1491.2467058<br />scaled: 0.7813810<br />1/2: 0.5","npv_sim:  1485.2109986<br />scaled: 0.7830927<br />1/2: 0.5","npv_sim:  1479.1752914<br />scaled: 0.7847901<br />1/2: 0.5","npv_sim:  1473.1395843<br />scaled: 0.7864651<br />1/2: 0.5","npv_sim:  1467.1038771<br />scaled: 0.7881113<br />1/2: 0.5","npv_sim:  1461.0681699<br />scaled: 0.7897506<br />1/2: 0.5","npv_sim:  1455.0324628<br />scaled: 0.7913481<br />1/2: 0.5","npv_sim:  1448.9967556<br />scaled: 0.7929455<br />1/2: 0.5","npv_sim:  1442.9610484<br />scaled: 0.7945037<br />1/2: 0.5","npv_sim:  1436.9253413<br />scaled: 0.7960548<br />1/2: 0.5","npv_sim:  1430.8896341<br />scaled: 0.7975817<br />1/2: 0.5","npv_sim:  1424.8539269<br />scaled: 0.7990889<br />1/2: 0.5","npv_sim:  1418.8182198<br />scaled: 0.8005854<br />1/2: 0.5","npv_sim:  1412.7825126<br />scaled: 0.8020510<br />1/2: 0.5","npv_sim:  1406.7468054<br />scaled: 0.8035167<br />1/2: 0.5","npv_sim:  1400.7110983<br />scaled: 0.8049447<br />1/2: 0.5","npv_sim:  1394.6753911<br />scaled: 0.8063712<br />1/2: 0.5","npv_sim:  1388.6396839<br />scaled: 0.8077733<br />1/2: 0.5","npv_sim:  1382.6039768<br />scaled: 0.8091630<br />1/2: 0.5","npv_sim:  1376.5682696<br />scaled: 0.8105400<br />1/2: 0.5","npv_sim:  1370.5325624<br />scaled: 0.8118954<br />1/2: 0.5","npv_sim:  1364.4968553<br />scaled: 0.8132484<br />1/2: 0.5","npv_sim:  1358.4611481<br />scaled: 0.8145717<br />1/2: 0.5","npv_sim:  1352.4254409<br />scaled: 0.8158950<br />1/2: 0.5","npv_sim:  1346.3897337<br />scaled: 0.8171951<br />1/2: 0.5","npv_sim:  1340.3540266<br />scaled: 0.8184886<br />1/2: 0.5","npv_sim:  1334.3183194<br />scaled: 0.8197687<br />1/2: 0.5","npv_sim:  1328.2826122<br />scaled: 0.8210347<br />1/2: 0.5","npv_sim:  1322.2469051<br />scaled: 0.8222958<br />1/2: 0.5","npv_sim:  1316.2111979<br />scaled: 0.8235364<br />1/2: 0.5","npv_sim:  1310.1754907<br />scaled: 0.8247769<br />1/2: 0.5","npv_sim:  1304.1397836<br />scaled: 0.8259966<br />1/2: 0.5","npv_sim:  1298.1040764<br />scaled: 0.8272139<br />1/2: 0.5","npv_sim:  1292.0683692<br />scaled: 0.8284185<br />1/2: 0.5","npv_sim:  1286.0326621<br />scaled: 0.8296146<br />1/2: 0.5","npv_sim:  1279.9969549<br />scaled: 0.8308047<br />1/2: 0.5","npv_sim:  1273.9612477<br />scaled: 0.8319816<br />1/2: 0.5","npv_sim:  1267.9255406<br />scaled: 0.8331582<br />1/2: 0.5","npv_sim:  1261.8898334<br />scaled: 0.8343177<br />1/2: 0.5","npv_sim:  1255.8541262<br />scaled: 0.8354772<br />1/2: 0.5","npv_sim:  1249.8184191<br />scaled: 0.8366256<br />1/2: 0.5","npv_sim:  1243.7827119<br />scaled: 0.8377697<br />1/2: 0.5","npv_sim:  1237.7470047<br />scaled: 0.8389079<br />1/2: 0.5","npv_sim:  1231.7112976<br />scaled: 0.8400382<br />1/2: 0.5","npv_sim:  1225.6755904<br />scaled: 0.8411670<br />1/2: 0.5","npv_sim:  1219.6398832<br />scaled: 0.8422852<br />1/2: 0.5","npv_sim:  1213.6041761<br />scaled: 0.8434035<br />1/2: 0.5","npv_sim:  1207.5684689<br />scaled: 0.8445131<br />1/2: 0.5","npv_sim:  1201.5327617<br />scaled: 0.8456209<br />1/2: 0.5","npv_sim:  1195.4970546<br />scaled: 0.8467240<br />1/2: 0.5","npv_sim:  1189.4613474<br />scaled: 0.8478229<br />1/2: 0.5","npv_sim:  1183.4256402<br />scaled: 0.8489201<br />1/2: 0.5","npv_sim:  1177.3899331<br />scaled: 0.8500115<br />1/2: 0.5","npv_sim:  1171.3542259<br />scaled: 0.8511030<br />1/2: 0.5","npv_sim:  1165.3185187<br />scaled: 0.8521887<br />1/2: 0.5","npv_sim:  1159.2828116<br />scaled: 0.8532741<br />1/2: 0.5","npv_sim:  1153.2471044<br />scaled: 0.8543564<br />1/2: 0.5","npv_sim:  1147.2113972<br />scaled: 0.8554370<br />1/2: 0.5","npv_sim:  1141.1756901<br />scaled: 0.8565163<br />1/2: 0.5","npv_sim:  1135.1399829<br />scaled: 0.8575933<br />1/2: 0.5","npv_sim:  1129.1042757<br />scaled: 0.8586702<br />1/2: 0.5","npv_sim:  1123.0685685<br />scaled: 0.8597447<br />1/2: 0.5","npv_sim:  1117.0328614<br />scaled: 0.8608193<br />1/2: 0.5","npv_sim:  1110.9971542<br />scaled: 0.8618928<br />1/2: 0.5","npv_sim:  1104.9614470<br />scaled: 0.8629660<br />1/2: 0.5","npv_sim:  1098.9257399<br />scaled: 0.8640389<br />1/2: 0.5","npv_sim:  1092.8900327<br />scaled: 0.8651117<br />1/2: 0.5","npv_sim:  1086.8543255<br />scaled: 0.8661845<br />1/2: 0.5","npv_sim:  1080.8186184<br />scaled: 0.8672577<br />1/2: 0.5","npv_sim:  1074.7829112<br />scaled: 0.8683310<br />1/2: 0.5","npv_sim:  1068.7472040<br />scaled: 0.8694054<br />1/2: 0.5","npv_sim:  1062.7114969<br />scaled: 0.8704799<br />1/2: 0.5","npv_sim:  1056.6757897<br />scaled: 0.8715557<br />1/2: 0.5","npv_sim:  1050.6400825<br />scaled: 0.8726324<br />1/2: 0.5","npv_sim:  1044.6043754<br />scaled: 0.8737098<br />1/2: 0.5","npv_sim:  1038.5686682<br />scaled: 0.8747893<br />1/2: 0.5","npv_sim:  1032.5329610<br />scaled: 0.8758687<br />1/2: 0.5","npv_sim:  1026.4972539<br />scaled: 0.8769515<br />1/2: 0.5","npv_sim:  1020.4615467<br />scaled: 0.8780344<br />1/2: 0.5","npv_sim:  1014.4258395<br />scaled: 0.8791200<br />1/2: 0.5","npv_sim:  1008.3901324<br />scaled: 0.8802069<br />1/2: 0.5","npv_sim:  1002.3544252<br />scaled: 0.8812955<br />1/2: 0.5","npv_sim:   996.3187180<br />scaled: 0.8823868<br />1/2: 0.5","npv_sim:   990.2830109<br />scaled: 0.8834787<br />1/2: 0.5","npv_sim:   984.2473037<br />scaled: 0.8845749<br />1/2: 0.5","npv_sim:   978.2115965<br />scaled: 0.8856711<br />1/2: 0.5","npv_sim:   972.1758894<br />scaled: 0.8867716<br />1/2: 0.5","npv_sim:   966.1401822<br />scaled: 0.8878731<br />1/2: 0.5","npv_sim:   960.1044750<br />scaled: 0.8889775<br />1/2: 0.5","npv_sim:   954.0687679<br />scaled: 0.8900846<br />1/2: 0.5","npv_sim:   948.0330607<br />scaled: 0.8911930<br />1/2: 0.5","npv_sim:   941.9973535<br />scaled: 0.8923059<br />1/2: 0.5","npv_sim:   935.9616464<br />scaled: 0.8934189<br />1/2: 0.5","npv_sim:   929.9259392<br />scaled: 0.8945374<br />1/2: 0.5","npv_sim:   923.8902320<br />scaled: 0.8956563<br />1/2: 0.5","npv_sim:   917.8545249<br />scaled: 0.8967792<br />1/2: 0.5","npv_sim:   911.8188177<br />scaled: 0.8979043<br />1/2: 0.5","npv_sim:   905.7831105<br />scaled: 0.8990315<br />1/2: 0.5","npv_sim:   899.7474034<br />scaled: 0.9001628<br />1/2: 0.5","npv_sim:   893.7116962<br />scaled: 0.9012944<br />1/2: 0.5","npv_sim:   887.6759890<br />scaled: 0.9024319<br />1/2: 0.5","npv_sim:   881.6402818<br />scaled: 0.9035695<br />1/2: 0.5","npv_sim:   875.6045747<br />scaled: 0.9047116<br />1/2: 0.5","npv_sim:   869.5688675<br />scaled: 0.9058553<br />1/2: 0.5","npv_sim:   863.5331603<br />scaled: 0.9070017<br />1/2: 0.5","npv_sim:   857.4974532<br />scaled: 0.9081513<br />1/2: 0.5","npv_sim:   851.4617460<br />scaled: 0.9093019<br />1/2: 0.5","npv_sim:   845.4260388<br />scaled: 0.9104575<br />1/2: 0.5","npv_sim:   839.3903317<br />scaled: 0.9116130<br />1/2: 0.5","npv_sim:   833.3546245<br />scaled: 0.9127733<br />1/2: 0.5","npv_sim:   827.3189173<br />scaled: 0.9139343<br />1/2: 0.5","npv_sim:   821.2832102<br />scaled: 0.9150983<br />1/2: 0.5","npv_sim:   815.2475030<br />scaled: 0.9162646<br />1/2: 0.5","npv_sim:   809.2117958<br />scaled: 0.9174322<br />1/2: 0.5","npv_sim:   803.1760887<br />scaled: 0.9186033<br />1/2: 0.5","npv_sim:   797.1403815<br />scaled: 0.9197744<br />1/2: 0.5","npv_sim:   791.1046743<br />scaled: 0.9209497<br />1/2: 0.5","npv_sim:   785.0689672<br />scaled: 0.9221251<br />1/2: 0.5","npv_sim:   779.0332600<br />scaled: 0.9233030<br />1/2: 0.5","npv_sim:   772.9975528<br />scaled: 0.9244822<br />1/2: 0.5","npv_sim:   766.9618457<br />scaled: 0.9256625<br />1/2: 0.5","npv_sim:   760.9261385<br />scaled: 0.9268448<br />1/2: 0.5","npv_sim:   754.8904313<br />scaled: 0.9280273<br />1/2: 0.5","npv_sim:   748.8547242<br />scaled: 0.9292119<br />1/2: 0.5","npv_sim:   742.8190170<br />scaled: 0.9303965<br />1/2: 0.5","npv_sim:   736.7833098<br />scaled: 0.9315824<br />1/2: 0.5","npv_sim:   730.7476027<br />scaled: 0.9327686<br />1/2: 0.5","npv_sim:   724.7118955<br />scaled: 0.9339551<br />1/2: 0.5","npv_sim:   718.6761883<br />scaled: 0.9351419<br />1/2: 0.5","npv_sim:   712.6404812<br />scaled: 0.9363287<br />1/2: 0.5","npv_sim:   706.6047740<br />scaled: 0.9375152<br />1/2: 0.5","npv_sim:   700.5690668<br />scaled: 0.9387017<br />1/2: 0.5","npv_sim:   694.5333597<br />scaled: 0.9398868<br />1/2: 0.5","npv_sim:   688.4976525<br />scaled: 0.9410719<br />1/2: 0.5","npv_sim:   682.4619453<br />scaled: 0.9422553<br />1/2: 0.5","npv_sim:   676.4262382<br />scaled: 0.9434377<br />1/2: 0.5","npv_sim:   670.3905310<br />scaled: 0.9446188<br />1/2: 0.5","npv_sim:   664.3548238<br />scaled: 0.9457972<br />1/2: 0.5","npv_sim:   658.3191166<br />scaled: 0.9469755<br />1/2: 0.5","npv_sim:   652.2834095<br />scaled: 0.9481486<br />1/2: 0.5","npv_sim:   646.2477023<br />scaled: 0.9493217<br />1/2: 0.5","npv_sim:   640.2119951<br />scaled: 0.9504898<br />1/2: 0.5","npv_sim:   634.1762880<br />scaled: 0.9516561<br />1/2: 0.5","npv_sim:   628.1405808<br />scaled: 0.9528186<br />1/2: 0.5","npv_sim:   622.1048736<br />scaled: 0.9539764<br />1/2: 0.5","npv_sim:   616.0691665<br />scaled: 0.9551328<br />1/2: 0.5","npv_sim:   610.0334593<br />scaled: 0.9562804<br />1/2: 0.5","npv_sim:   603.9977521<br />scaled: 0.9574280<br />1/2: 0.5","npv_sim:   597.9620450<br />scaled: 0.9585655<br />1/2: 0.5","npv_sim:   591.9263378<br />scaled: 0.9597012<br />1/2: 0.5","npv_sim:   585.8906306<br />scaled: 0.9608292<br />1/2: 0.5","npv_sim:   579.8549235<br />scaled: 0.9619509<br />1/2: 0.5","npv_sim:   573.8192163<br />scaled: 0.9630685<br />1/2: 0.5","npv_sim:   567.7835091<br />scaled: 0.9641743<br />1/2: 0.5","npv_sim:   561.7478020<br />scaled: 0.9652801<br />1/2: 0.5","npv_sim:   555.7120948<br />scaled: 0.9663685<br />1/2: 0.5","npv_sim:   549.6763876<br />scaled: 0.9674561<br />1/2: 0.5","npv_sim:   543.6406805<br />scaled: 0.9685303<br />1/2: 0.5","npv_sim:   537.6049733<br />scaled: 0.9695977<br />1/2: 0.5","npv_sim:   531.5692661<br />scaled: 0.9706566<br />1/2: 0.5","npv_sim:   525.5335590<br />scaled: 0.9717013<br />1/2: 0.5","npv_sim:   519.4978518<br />scaled: 0.9727442<br />1/2: 0.5","npv_sim:   513.4621446<br />scaled: 0.9737637<br />1/2: 0.5","npv_sim:   507.4264375<br />scaled: 0.9747833<br />1/2: 0.5","npv_sim:   501.3907303<br />scaled: 0.9757814<br />1/2: 0.5","npv_sim:   495.3550231<br />scaled: 0.9767734<br />1/2: 0.5","npv_sim:   489.3193160<br />scaled: 0.9777507<br />1/2: 0.5","npv_sim:   483.2836088<br />scaled: 0.9787125<br />1/2: 0.5","npv_sim:   477.2479016<br />scaled: 0.9796680<br />1/2: 0.5","npv_sim:   471.2121945<br />scaled: 0.9805968<br />1/2: 0.5","npv_sim:   465.1764873<br />scaled: 0.9815257<br />1/2: 0.5","npv_sim:   459.1407801<br />scaled: 0.9824226<br />1/2: 0.5","npv_sim:   453.1050730<br />scaled: 0.9833158<br />1/2: 0.5","npv_sim:   447.0693658<br />scaled: 0.9841858<br />1/2: 0.5","npv_sim:   441.0336586<br />scaled: 0.9850405<br />1/2: 0.5","npv_sim:   434.9979514<br />scaled: 0.9858826<br />1/2: 0.5","npv_sim:   428.9622443<br />scaled: 0.9866958<br />1/2: 0.5","npv_sim:   422.9265371<br />scaled: 0.9875087<br />1/2: 0.5","npv_sim:   416.8908299<br />scaled: 0.9882776<br />1/2: 0.5","npv_sim:   410.8551228<br />scaled: 0.9890465<br />1/2: 0.5","npv_sim:   404.8194156<br />scaled: 0.9897816<br />1/2: 0.5","npv_sim:   398.7837084<br />scaled: 0.9905032<br />1/2: 0.5","npv_sim:   392.7480013<br />scaled: 0.9912038<br />1/2: 0.5","npv_sim:   386.7122941<br />scaled: 0.9918750<br />1/2: 0.5","npv_sim:   380.6765869<br />scaled: 0.9925398<br />1/2: 0.5","npv_sim:   374.6408798<br />scaled: 0.9931577<br />1/2: 0.5","npv_sim:   368.6051726<br />scaled: 0.9937756<br />1/2: 0.5","npv_sim:   362.5694654<br />scaled: 0.9943469<br />1/2: 0.5","npv_sim:   356.5337583<br />scaled: 0.9949084<br />1/2: 0.5","npv_sim:   350.4980511<br />scaled: 0.9954383<br />1/2: 0.5","npv_sim:   344.4623439<br />scaled: 0.9959404<br />1/2: 0.5","npv_sim:   338.4266368<br />scaled: 0.9964277<br />1/2: 0.5","npv_sim:   332.3909296<br />scaled: 0.9968675<br />1/2: 0.5","npv_sim:   326.3552224<br />scaled: 0.9973072<br />1/2: 0.5","npv_sim:   320.3195153<br />scaled: 0.9976852<br />1/2: 0.5","npv_sim:   314.2838081<br />scaled: 0.9980595<br />1/2: 0.5","npv_sim:   308.2481009<br />scaled: 0.9983896<br />1/2: 0.5","npv_sim:   302.2123938<br />scaled: 0.9986955<br />1/2: 0.5","npv_sim:   296.1766866<br />scaled: 0.9989764<br />1/2: 0.5","npv_sim:   290.1409794<br />scaled: 0.9992111<br />1/2: 0.5","npv_sim:   284.1052723<br />scaled: 0.9994416<br />1/2: 0.5","npv_sim:   278.0695651<br />scaled: 0.9996022<br />1/2: 0.5","npv_sim:   272.0338579<br />scaled: 0.9997629<br />1/2: 0.5","npv_sim:   265.9981508<br />scaled: 0.9998650<br />1/2: 0.5","npv_sim:   259.9624436<br />scaled: 0.9999488<br />1/2: 0.5","npv_sim:   253.9267364<br />scaled: 0.9999956<br />1/2: 0.5","npv_sim:   247.8910293<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   241.8553221<br />scaled: 0.9999903<br />1/2: 0.5","npv_sim:   235.8196149<br />scaled: 0.9999128<br />1/2: 0.5","npv_sim:   229.7839078<br />scaled: 0.9998352<br />1/2: 0.5","npv_sim:   223.7482006<br />scaled: 0.9996836<br />1/2: 0.5","npv_sim:   217.7124934<br />scaled: 0.9995217<br />1/2: 0.5","npv_sim:   211.6767862<br />scaled: 0.9993093<br />1/2: 0.5","npv_sim:   205.6410791<br />scaled: 0.9990607<br />1/2: 0.5","npv_sim:   199.6053719<br />scaled: 0.9987867<br />1/2: 0.5","npv_sim:   193.5696647<br />scaled: 0.9984493<br />1/2: 0.5","npv_sim:   187.5339576<br />scaled: 0.9981119<br />1/2: 0.5","npv_sim:   181.4982504<br />scaled: 0.9976845<br />1/2: 0.5","npv_sim:   175.4625432<br />scaled: 0.9972563<br />1/2: 0.5","npv_sim:   169.4268361<br />scaled: 0.9967637<br />1/2: 0.5","npv_sim:   163.3911289<br />scaled: 0.9962430<br />1/2: 0.5","npv_sim:   157.3554217<br />scaled: 0.9956845<br />1/2: 0.5","npv_sim:   151.3197146<br />scaled: 0.9950696<br />1/2: 0.5","npv_sim:   145.2840074<br />scaled: 0.9944446<br />1/2: 0.5","npv_sim:   139.2483002<br />scaled: 0.9937342<br />1/2: 0.5","npv_sim:   133.2125931<br />scaled: 0.9930237<br />1/2: 0.5","npv_sim:   127.1768859<br />scaled: 0.9922347<br />1/2: 0.5","npv_sim:   121.1411787<br />scaled: 0.9914275<br />1/2: 0.5","npv_sim:   115.1054716<br />scaled: 0.9905698<br />1/2: 0.5","npv_sim:   109.0697644<br />scaled: 0.9896647<br />1/2: 0.5","npv_sim:   103.0340572<br />scaled: 0.9887379<br />1/2: 0.5","npv_sim:    96.9983501<br />scaled: 0.9877342<br />1/2: 0.5","npv_sim:    90.9626429<br />scaled: 0.9867306<br />1/2: 0.5","npv_sim:    84.9269357<br />scaled: 0.9856351<br />1/2: 0.5","npv_sim:    78.8912286<br />scaled: 0.9845324<br />1/2: 0.5","npv_sim:    72.8555214<br />scaled: 0.9833668<br />1/2: 0.5","npv_sim:    66.8198142<br />scaled: 0.9821646<br />1/2: 0.5","npv_sim:    60.7841071<br />scaled: 0.9809289<br />1/2: 0.5","npv_sim:    54.7483999<br />scaled: 0.9796271<br />1/2: 0.5","npv_sim:    48.7126927<br />scaled: 0.9783213<br />1/2: 0.5","npv_sim:    42.6769856<br />scaled: 0.9769201<br />1/2: 0.5","npv_sim:    36.6412784<br />scaled: 0.9755189<br />1/2: 0.5","npv_sim:    30.6055712<br />scaled: 0.9740439<br />1/2: 0.5","npv_sim:    24.5698641<br />scaled: 0.9725437<br />1/2: 0.5","npv_sim:    18.5341569<br />scaled: 0.9709992<br />1/2: 0.5","npv_sim:    12.4984497<br />scaled: 0.9694007<br />1/2: 0.5","npv_sim:     6.4627426<br />scaled: 0.9677870<br />1/2: 0.5","npv_sim:     0.4270354<br />scaled: 0.9660910<br />1/2: 0.5","npv_sim:    -5.6086718<br />scaled: 0.9643950<br />1/2: 0.5","npv_sim:   -11.6443789<br />scaled: 0.9626162<br />1/2: 0.5","npv_sim:   -17.6800861<br />scaled: 0.9608239<br />1/2: 0.5","npv_sim:   -23.7157933<br />scaled: 0.9589780<br />1/2: 0.5","npv_sim:   -29.7515005<br />scaled: 0.9570907<br />1/2: 0.5","npv_sim:   -35.7872076<br />scaled: 0.9551783<br />1/2: 0.5","npv_sim:   -41.8229148<br />scaled: 0.9531978<br />1/2: 0.5","npv_sim:   -47.8586220<br />scaled: 0.9512172<br />1/2: 0.5","npv_sim:   -53.8943291<br />scaled: 0.9491475<br />1/2: 0.5","npv_sim:   -59.9300363<br />scaled: 0.9470755<br />1/2: 0.5","npv_sim:   -65.9657435<br />scaled: 0.9449428<br />1/2: 0.5","npv_sim:   -72.0014506<br />scaled: 0.9427814<br />1/2: 0.5","npv_sim:   -78.0371578<br />scaled: 0.9405866<br />1/2: 0.5","npv_sim:   -84.0728650<br />scaled: 0.9383381<br />1/2: 0.5","npv_sim:   -90.1085721<br />scaled: 0.9360822<br />1/2: 0.5","npv_sim:   -96.1442793<br />scaled: 0.9337492<br />1/2: 0.5","npv_sim:  -102.1799865<br />scaled: 0.9314162<br />1/2: 0.5","npv_sim:  -108.2156936<br />scaled: 0.9290183<br />1/2: 0.5","npv_sim:  -114.2514008<br />scaled: 0.9266035<br />1/2: 0.5","npv_sim:  -120.2871080<br />scaled: 0.9241494<br />1/2: 0.5","npv_sim:  -126.3228151<br />scaled: 0.9216557<br />1/2: 0.5","npv_sim:  -132.3585223<br />scaled: 0.9191466<br />1/2: 0.5","npv_sim:  -138.3942295<br />scaled: 0.9165771<br />1/2: 0.5","npv_sim:  -144.4299366<br />scaled: 0.9140077<br />1/2: 0.5","npv_sim:  -150.4656438<br />scaled: 0.9113723<br />1/2: 0.5","npv_sim:  -156.5013510<br />scaled: 0.9087303<br />1/2: 0.5","npv_sim:  -162.5370581<br />scaled: 0.9060460<br />1/2: 0.5","npv_sim:  -168.5727653<br />scaled: 0.9033349<br />1/2: 0.5","npv_sim:  -174.6084725<br />scaled: 0.9006030<br />1/2: 0.5","npv_sim:  -180.6441796<br />scaled: 0.8978265<br />1/2: 0.5","npv_sim:  -186.6798868<br />scaled: 0.8950485<br />1/2: 0.5","npv_sim:  -192.7155940<br />scaled: 0.8922101<br />1/2: 0.5","npv_sim:  -198.7513011<br />scaled: 0.8893718<br />1/2: 0.5","npv_sim:  -204.7870083<br />scaled: 0.8864912<br />1/2: 0.5","npv_sim:  -210.8227155<br />scaled: 0.8835949<br />1/2: 0.5","npv_sim:  -216.8584226<br />scaled: 0.8806752<br />1/2: 0.5","npv_sim:  -222.8941298<br />scaled: 0.8777248<br />1/2: 0.5","npv_sim:  -228.9298370<br />scaled: 0.8747675<br />1/2: 0.5","npv_sim:  -234.9655441<br />scaled: 0.8717671<br />1/2: 0.5","npv_sim:  -241.0012513<br />scaled: 0.8687667<br />1/2: 0.5","npv_sim:  -247.0369585<br />scaled: 0.8657275<br />1/2: 0.5","npv_sim:  -253.0726657<br />scaled: 0.8626810<br />1/2: 0.5","npv_sim:  -259.1083728<br />scaled: 0.8596117<br />1/2: 0.5","npv_sim:  -265.1440800<br />scaled: 0.8565234<br />1/2: 0.5","npv_sim:  -271.1797872<br />scaled: 0.8534255<br />1/2: 0.5","npv_sim:  -277.2154943<br />scaled: 0.8502995<br />1/2: 0.5","npv_sim:  -283.2512015<br />scaled: 0.8471735<br />1/2: 0.5","npv_sim:  -289.2869087<br />scaled: 0.8440153<br />1/2: 0.5","npv_sim:  -295.3226158<br />scaled: 0.8408557<br />1/2: 0.5","npv_sim:  -301.3583230<br />scaled: 0.8376766<br />1/2: 0.5","npv_sim:  -307.3940302<br />scaled: 0.8344876<br />1/2: 0.5","npv_sim:  -313.4297373<br />scaled: 0.8312893<br />1/2: 0.5","npv_sim:  -319.4654445<br />scaled: 0.8280750<br />1/2: 0.5","npv_sim:  -325.5011517<br />scaled: 0.8248592<br />1/2: 0.5","npv_sim:  -331.5368588<br />scaled: 0.8216237<br />1/2: 0.5","npv_sim:  -337.5725660<br />scaled: 0.8183882<br />1/2: 0.5","npv_sim:  -343.6082732<br />scaled: 0.8151395<br />1/2: 0.5","npv_sim:  -349.6439803<br />scaled: 0.8118869<br />1/2: 0.5","npv_sim:  -355.6796875<br />scaled: 0.8086279<br />1/2: 0.5","npv_sim:  -361.7153947<br />scaled: 0.8053622<br />1/2: 0.5","npv_sim:  -367.7511018<br />scaled: 0.8020947<br />1/2: 0.5","npv_sim:  -373.7868090<br />scaled: 0.7988197<br />1/2: 0.5","npv_sim:  -379.8225162<br />scaled: 0.7955447<br />1/2: 0.5","npv_sim:  -385.8582233<br />scaled: 0.7922649<br />1/2: 0.5","npv_sim:  -391.8939305<br />scaled: 0.7889844<br />1/2: 0.5","npv_sim:  -397.9296377<br />scaled: 0.7857029<br />1/2: 0.5","npv_sim:  -403.9653448<br />scaled: 0.7824207<br />1/2: 0.5","npv_sim:  -410.0010520<br />scaled: 0.7791390<br />1/2: 0.5","npv_sim:  -416.0367592<br />scaled: 0.7758586<br />1/2: 0.5","npv_sim:  -422.0724663<br />scaled: 0.7725782<br />1/2: 0.5","npv_sim:  -428.1081735<br />scaled: 0.7693029<br />1/2: 0.5","npv_sim:  -434.1438807<br />scaled: 0.7660277<br />1/2: 0.5","npv_sim:  -440.1795878<br />scaled: 0.7627585<br />1/2: 0.5","npv_sim:  -446.2152950<br />scaled: 0.7594917<br />1/2: 0.5","npv_sim:  -452.2510022<br />scaled: 0.7562297<br />1/2: 0.5","npv_sim:  -458.2867093<br />scaled: 0.7529745<br />1/2: 0.5","npv_sim:  -464.3224165<br />scaled: 0.7497210<br />1/2: 0.5","npv_sim:  -470.3581237<br />scaled: 0.7464803<br />1/2: 0.5","npv_sim:  -476.3938309<br />scaled: 0.7432395<br />1/2: 0.5","npv_sim:  -482.4295380<br />scaled: 0.7400130<br />1/2: 0.5","npv_sim:  -488.4652452<br />scaled: 0.7367895<br />1/2: 0.5","npv_sim:  -494.5009524<br />scaled: 0.7335765<br />1/2: 0.5","npv_sim:  -500.5366595<br />scaled: 0.7303727<br />1/2: 0.5","npv_sim:  -506.5723667<br />scaled: 0.7271742<br />1/2: 0.5","npv_sim:  -512.6080739<br />scaled: 0.7239925<br />1/2: 0.5","npv_sim:  -518.6437810<br />scaled: 0.7208109<br />1/2: 0.5","npv_sim:  -524.6794882<br />scaled: 0.7176521<br />1/2: 0.5","npv_sim:  -530.7151954<br />scaled: 0.7144948<br />1/2: 0.5","npv_sim:  -536.7509025<br />scaled: 0.7113543<br />1/2: 0.5","npv_sim:  -542.7866097<br />scaled: 0.7082232<br />1/2: 0.5","npv_sim:  -548.8223169<br />scaled: 0.7051018<br />1/2: 0.5","npv_sim:  -554.8580240<br />scaled: 0.7019986<br />1/2: 0.5","npv_sim:  -560.8937312<br />scaled: 0.6988970<br />1/2: 0.5","npv_sim:  -566.9294384<br />scaled: 0.6958233<br />1/2: 0.5","npv_sim:  -572.9651455<br />scaled: 0.6927495<br />1/2: 0.5","npv_sim:  -579.0008527<br />scaled: 0.6896991<br />1/2: 0.5","npv_sim:  -585.0365599<br />scaled: 0.6866561<br />1/2: 0.5","npv_sim:  -591.0722670<br />scaled: 0.6836278<br />1/2: 0.5","npv_sim:  -597.1079742<br />scaled: 0.6806166<br />1/2: 0.5","npv_sim:  -603.1436814<br />scaled: 0.6776109<br />1/2: 0.5","npv_sim:  -609.1793885<br />scaled: 0.6746323<br />1/2: 0.5","npv_sim:  -615.2150957<br />scaled: 0.6716538<br />1/2: 0.5","npv_sim:  -621.2508029<br />scaled: 0.6687043<br />1/2: 0.5","npv_sim:  -627.2865100<br />scaled: 0.6657591<br />1/2: 0.5","npv_sim:  -633.3222172<br />scaled: 0.6628333<br />1/2: 0.5","npv_sim:  -639.3579244<br />scaled: 0.6599218<br />1/2: 0.5","npv_sim:  -645.3936315<br />scaled: 0.6570199<br />1/2: 0.5","npv_sim:  -651.4293387<br />scaled: 0.6541423<br />1/2: 0.5","npv_sim:  -657.4650459<br />scaled: 0.6512647<br />1/2: 0.5","npv_sim:  -663.5007530<br />scaled: 0.6484206<br />1/2: 0.5","npv_sim:  -669.5364602<br />scaled: 0.6455770<br />1/2: 0.5","npv_sim:  -675.5721674<br />scaled: 0.6427568<br />1/2: 0.5","npv_sim:  -681.6078745<br />scaled: 0.6399468<br />1/2: 0.5","npv_sim:  -687.6435817<br />scaled: 0.6371503<br />1/2: 0.5","npv_sim:  -693.6792889<br />scaled: 0.6343737<br />1/2: 0.5","npv_sim:  -699.7149961<br />scaled: 0.6316006<br />1/2: 0.5","npv_sim:  -705.7507032<br />scaled: 0.6288569<br />1/2: 0.5","npv_sim:  -711.7864104<br />scaled: 0.6261131<br />1/2: 0.5","npv_sim:  -717.8221176<br />scaled: 0.6233954<br />1/2: 0.5","npv_sim:  -723.8578247<br />scaled: 0.6206837<br />1/2: 0.5","npv_sim:  -729.8935319<br />scaled: 0.6179880<br />1/2: 0.5","npv_sim:  -735.9292391<br />scaled: 0.6153075<br />1/2: 0.5","npv_sim:  -741.9649462<br />scaled: 0.6126335<br />1/2: 0.5","npv_sim:  -748.0006534<br />scaled: 0.6099830<br />1/2: 0.5","npv_sim:  -754.0363606<br />scaled: 0.6073326<br />1/2: 0.5","npv_sim:  -760.0720677<br />scaled: 0.6047087<br />1/2: 0.5","npv_sim:  -766.1077749<br />scaled: 0.6020871<br />1/2: 0.5","npv_sim:  -772.1434821<br />scaled: 0.5994827<br />1/2: 0.5","npv_sim:  -778.1791892<br />scaled: 0.5968885<br />1/2: 0.5","npv_sim:  -784.2148964<br />scaled: 0.5943030<br />1/2: 0.5","npv_sim:  -790.2506036<br />scaled: 0.5917348<br />1/2: 0.5","npv_sim:  -796.2863107<br />scaled: 0.5891676<br />1/2: 0.5","npv_sim:  -802.3220179<br />scaled: 0.5866237<br />1/2: 0.5","npv_sim:  -808.3577251<br />scaled: 0.5840799<br />1/2: 0.5","npv_sim:  -814.3934322<br />scaled: 0.5815529<br />1/2: 0.5","npv_sim:  -820.4291394<br />scaled: 0.5790317<br />1/2: 0.5","npv_sim:  -826.4648466<br />scaled: 0.5765199<br />1/2: 0.5","npv_sim:  -832.5005537<br />scaled: 0.5740196<br />1/2: 0.5","npv_sim:  -838.5362609<br />scaled: 0.5715221<br />1/2: 0.5","npv_sim:  -844.5719681<br />scaled: 0.5690407<br />1/2: 0.5","npv_sim:  -850.6076752<br />scaled: 0.5665593<br />1/2: 0.5","npv_sim:  -856.6433824<br />scaled: 0.5640925<br />1/2: 0.5","npv_sim:  -862.6790896<br />scaled: 0.5616282<br />1/2: 0.5","npv_sim:  -868.7147967<br />scaled: 0.5591722<br />1/2: 0.5","npv_sim:  -874.7505039<br />scaled: 0.5567228<br />1/2: 0.5","npv_sim:  -880.7862111<br />scaled: 0.5542769<br />1/2: 0.5","npv_sim:  -886.8219182<br />scaled: 0.5518404<br />1/2: 0.5","npv_sim:  -892.8576254<br />scaled: 0.5494039<br />1/2: 0.5","npv_sim:  -898.8933326<br />scaled: 0.5469780<br />1/2: 0.5","npv_sim:  -904.9290397<br />scaled: 0.5445524<br />1/2: 0.5","npv_sim:  -910.9647469<br />scaled: 0.5421327<br />1/2: 0.5","npv_sim:  -917.0004541<br />scaled: 0.5397158<br />1/2: 0.5","npv_sim:  -923.0361612<br />scaled: 0.5373015<br />1/2: 0.5","npv_sim:  -929.0718684<br />scaled: 0.5348912<br />1/2: 0.5","npv_sim:  -935.1075756<br />scaled: 0.5324814<br />1/2: 0.5","npv_sim:  -941.1432828<br />scaled: 0.5300757<br />1/2: 0.5","npv_sim:  -947.1789899<br />scaled: 0.5276700<br />1/2: 0.5","npv_sim:  -953.2146971<br />scaled: 0.5252662<br />1/2: 0.5","npv_sim:  -959.2504043<br />scaled: 0.5228630<br />1/2: 0.5","npv_sim:  -965.2861114<br />scaled: 0.5204600<br />1/2: 0.5","npv_sim:  -971.3218186<br />scaled: 0.5180571<br />1/2: 0.5","npv_sim:  -977.3575258<br />scaled: 0.5156540<br />1/2: 0.5","npv_sim:  -983.3932329<br />scaled: 0.5132495<br />1/2: 0.5","npv_sim:  -989.4289401<br />scaled: 0.5108451<br />1/2: 0.5","npv_sim:  -995.4646473<br />scaled: 0.5084374<br />1/2: 0.5","npv_sim: -1001.5003544<br />scaled: 0.5060294<br />1/2: 0.5","npv_sim: -1007.5360616<br />scaled: 0.5036181<br />1/2: 0.5","npv_sim: -1013.5717688<br />scaled: 0.5012046<br />1/2: 0.5","npv_sim: -1019.6074759<br />scaled: 0.4987888<br />1/2: 0.5","npv_sim: -1025.6431831<br />scaled: 0.4963681<br />1/2: 0.5","npv_sim: -1031.6788903<br />scaled: 0.4939472<br />1/2: 0.5","npv_sim: -1037.7145974<br />scaled: 0.4915174<br />1/2: 0.5","npv_sim: -1043.7503046<br />scaled: 0.4890876<br />1/2: 0.5","npv_sim: -1049.7860118<br />scaled: 0.4866501<br />1/2: 0.5","npv_sim: -1055.8217189<br />scaled: 0.4842097<br />1/2: 0.5","npv_sim: -1061.8574261<br />scaled: 0.4817639<br />1/2: 0.5","npv_sim: -1067.8931333<br />scaled: 0.4793113<br />1/2: 0.5","npv_sim: -1073.9288404<br />scaled: 0.4768567<br />1/2: 0.5","npv_sim: -1079.9645476<br />scaled: 0.4743904<br />1/2: 0.5","npv_sim: -1086.0002548<br />scaled: 0.4719240<br />1/2: 0.5","npv_sim: -1092.0359619<br />scaled: 0.4694451<br />1/2: 0.5","npv_sim: -1098.0716691<br />scaled: 0.4669637<br />1/2: 0.5","npv_sim: -1104.1073763<br />scaled: 0.4644735<br />1/2: 0.5","npv_sim: -1110.1430834<br />scaled: 0.4619759<br />1/2: 0.5","npv_sim: -1116.1787906<br />scaled: 0.4594739<br />1/2: 0.5","npv_sim: -1122.2144978<br />scaled: 0.4569590<br />1/2: 0.5","npv_sim: -1128.2502049<br />scaled: 0.4544441<br />1/2: 0.5","npv_sim: -1134.2859121<br />scaled: 0.4519117<br />1/2: 0.5","npv_sim: -1140.3216193<br />scaled: 0.4493785<br />1/2: 0.5","npv_sim: -1146.3573264<br />scaled: 0.4468327<br />1/2: 0.5","npv_sim: -1152.3930336<br />scaled: 0.4442804<br />1/2: 0.5","npv_sim: -1158.4287408<br />scaled: 0.4417208<br />1/2: 0.5","npv_sim: -1164.4644480<br />scaled: 0.4391487<br />1/2: 0.5","npv_sim: -1170.5001551<br />scaled: 0.4365751<br />1/2: 0.5","npv_sim: -1176.5358623<br />scaled: 0.4339826<br />1/2: 0.5","npv_sim: -1182.5715695<br />scaled: 0.4313901<br />1/2: 0.5","npv_sim: -1188.6072766<br />scaled: 0.4287815<br />1/2: 0.5","npv_sim: -1194.6429838<br />scaled: 0.4261683<br />1/2: 0.5","npv_sim: -1200.6786910<br />scaled: 0.4235449<br />1/2: 0.5","npv_sim: -1206.7143981<br />scaled: 0.4209106<br />1/2: 0.5","npv_sim: -1212.7501053<br />scaled: 0.4182723<br />1/2: 0.5","npv_sim: -1218.7858125<br />scaled: 0.4156168<br />1/2: 0.5","npv_sim: -1224.8215196<br />scaled: 0.4129614<br />1/2: 0.5","npv_sim: -1230.8572268<br />scaled: 0.4102870<br />1/2: 0.5","npv_sim: -1236.8929340<br />scaled: 0.4076104<br />1/2: 0.5","npv_sim: -1236.8929340<br />scaled: 0.4076104<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,394.143795532621,394.143795532621],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 394.1438","xintercept: 394.1438"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.215693298931],"y":[0.25],"text":"Median NPV:<br />  394.14","hovertext":"x: 591.2157<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[591.215693298931],"y":[0.1],"text":"SD NPV:<br />  1008.86","hovertext":"x: 591.2157<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic of Effects From Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1669.32630251208,2401.8788961441],"tickmode":"array","ticktext":["-1000","0","1000","2000"],"tickvals":[-1000,0,1000,2000],"categoryorder":"array","categoryarray":["-1000","0","1000","2000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"da85785e2545":{"x":{},"y":{},"alpha":{},"type":"scatter"},"da856ce112bf":{"xintercept":{}},"da852c95f171":{"x":{},"y":{}},"da85281e4895":{"x":{},"y":{}}},"cur_data":"da85785e2545","visdat":{"da85785e2545":["function (y) ","x"],"da856ce112bf":["function (y) ","x"],"da852c95f171":["function (y) ","x"],"da85281e4895":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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
