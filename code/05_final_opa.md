---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "31 July, 2020"
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
<caption>(\#tab:sum_tables2)Summary of equations use until this point in the document</caption>
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
<caption>(\#tab:sum_tables2)Sources: summary of inputs specified until this point in the document</caption>
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
# - inputs: wage_in, lambda1_so, lambda2_so, saturation, FERNANDO: coverage_var is not specified
# - outputs: earnings (no name specified)
chunk_earnings1 <- function(){
###############################################################################
###############################################################################  

    earnings1_f <- function(wage_var = wage_in,
                          lambda1_var = lambda1_so,
                          lambda2_var = lambda2_so,
                          saturation_var = saturation, coverage_var) {  
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

For this (or similar?) setting Miguel and Kremer 2007 [add page, table, col, row] estimate that there is almost no take-up without subsidy ($Q(0)$), hence  is assigned the value of 0. The same article [add page, table, col, row] estimates that take-up with full subsidy ($Q(full)$) was of 0.75.

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
<caption>(\#tab:sum_tables3)Summary of equations use until this point in the document</caption>
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
<caption>(\#tab:sum_tables3)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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
# - inputs: periods_so, delta_ed_final_in, interest FERNANDO: should this be interest_in or interest_19?, cost_per_student_in, s2_in, q2_in
# - outputs: cost2_f
chunk_cost2 <- function(){
###############################################################################
###############################################################################  

    cost2_f <- function(periods_var = periods_so, 
                        delta_ed_var = delta_ed_final_in,
                        interest_r_var = interest, 
                        cost_of_schooling_var = cost_per_student_in,
               s1_var = 0, q1_var = 0, s2_var = s2_in, q2_var = q2_in) {
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
<caption>(\#tab:sum_tables16)Summary of equations use until this point in the document</caption>
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
<caption>(\#tab:sum_tables16)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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

<details><summary>View summary table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Sources: summary of inputs</caption>
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

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Summary of equations use until this point in the document</caption>
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
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
   <td style="text-align:left;"> $(11)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables17)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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

[^9]: Based on the upper tier of monthly teacher salaries reported by two Kenyan news sources: Nyanchama (2018) and Oduor [FIND SOURCES]. Since compensation for teachers in rural villages where the treatment was administered is below the national average, we are overestimating the costs for a conservative analysis. The average number of students per teacher is 45, based on **[FIND SOURCE]**.

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


<details><summary>View summary table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables20)Summary of equations use until this point in the document</caption>
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
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
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
<caption>(\#tab:sum_tables20)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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
 

## Approach 3: Combinantion of Previous Approaches and Input From Policy Makers

In this third and final approach, we borrowed some methodological elements from @baird2016worms and @klps4 and worked in collaboration with a key policy maker in this area: the NGO Evidence Action (EA). EA provided feedback on what are the relevant costs and benefits that they, and other local policy makers like ministries of health, would consider when making decisions on deworming interventions. Additionally, EA provided insights and data on what are the key main costs of implementing a deworming intervention in different countries. 

In this final approach the benefits from deworming described in the previous approachces are scaled to reflect differences prevalence rates. Additionally, the relevant costs are constrained to be only the direct costs, with varying implementation costs for different countries. 


When the original deworming study was conducted in Kenya in 1999, the prevalence rates of the different types of worms infections where of up to XXX for the relevant population, and the costs of for this setting where of XXXX. Today Evidence Actions supports deworming interventions in XXX countries, with prevalence rates ranging from XXXX to XXXXX and costs ranging from XXXXX to XXXXX. 

<!-- Emma (11): please read the three paragraph above and improve writing -->

### Benefits   

In order to account for different prevalence rates, the estimated treatment effect are decomposed in the impact of deworming on children who were treated and had a worm infection ($\lambda_{1}^{eff}$) and children who where treated and did not had a worm infection. By construction, the effect on this last group should be zero. Hence the effective treatment of deworming on infected populations will be equal to the estimated treatement, divided by the proportion of the prevalence of infections. 

In the original evaluation, the prevalence rates where very high (1), hence the effect on the infected population was similar to that of the ovearll population. Currently deworming interventions are discussed in geographies with much lower prevalence rates, hence to obtain the expected effect over the new region, we need to multiply the effect on the infected population by the prevalence rate in the new region ($\eta_{r}$)

<details><summary>Show all the details</summary>

For approach 3, we will modify treatement effects of approaches 1 and 2 (equation 4 and 8 respectively) by the following:   

\begin{equation}
\lambda_{1} = \eta \lambda^{eff}_{1} + (1 -  \eta) \times 0 \\
\lambda^{r}_{1} = \eta_{r}\lambda^{eff}_{1}

\label{eq:17}
\tag{17}
\end{equation}

Where:      

 - $\eta$: represents the prevalence of any worm infection.
 - $\lambda_{1}^{eff}$: represents the effect of deworming over those infected.  


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

Flesh out points bellow, then review code (below), generate some intermediary result (table of costs and/or locations)

The country weights are computed as the fraction of all treated individuals that correspond to a given country. The per capita cost of each country are obtained by dividing the country's total costs by the total number of treated individuals in a given period. Total costs for a country represent the total cost across regions faced by Evidence Action, local governments, and other partners.  

Country level costs at the payer level are computed as the sum of item-level costs with-in a specific region of a country. 

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

All costs are in USD.

(Move to footnote) GW original analysis weights each country to take into account the number of treatments provided as well as the proportion of costs incurred by DtWI in that geography. The analytical foundations for such weights are not clear. Also not clear why should only account for DtW costs.  

- $N_{i}$: Number of treated children in country $i$.  
- $Ex_{i}$: Exchange rate from country $i$ to USD.  
- $k$: Costs distribute across $k$ payers.   
- $l$: Each payers costs come from $l$ items.   



```r
# - inputs: nothing
# - outputs: (1) function that computes the country weights used in the final 
# costs (2)  function that computes the weighted sum of country costs 

chunk_cost1_inp <- function(){
###############################################################################
###############################################################################  
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
      costs_by_item_temp <- df_costs_last %>%
        filter(Payer != "Total") %>%
        group_by(Country, Payer) %>% 
        summarise("costs_by_payer" = sum(suppressWarnings( as.numeric(Cost) ), na.rm = TRUE)) 
    
      #sum across payers and multiply by delta (second to last)
      country_cost <- costs_by_item_temp %>%
        group_by(Country) %>%
        summarise("costs_by_country" = sum(costs_by_payer) * (1 + staff_time_var))  

      # Compute the per capita cost for each country (c_i and w_i)
      costs_data <- country_cost %>%
         left_join(c_weights, by = "Country") %>%
         mutate("per_cap" = costs_by_country / total)
    
      return( costs_data )
    }

    costs1_p2_f <- function(country_w_var = costs_data$country_w, 
                         country_cost_var = costs_data$per_cap) {
        sum(country_w_var * country_cost_var)
    }

###############################################################################
###############################################################################  
    return( list("costs1_p1_f" = costs1_p1_f, 
                 "costs1_p2_f" = costs1_p2_f) )
}
invisible( list2env(chunk_cost1_inp(),.GlobalEnv) )
```

</details>
<br>

<details><summary>View Summary Table</summary>
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table18)Sources: summary of inputs</caption>
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

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table18)Summary of equations use until this point in the document</caption>
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
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
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
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table18)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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

#### Data required to compute costs.

$N_{i}, C_{i,k,l}, \delta_{g}$

<details><summary>Click Here to View Analysis Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables22)Sources: summary of inputs</caption>
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
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table20)Summary of equations use until this point in the document</caption>
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
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
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
<caption>(\#tab:sum_table20)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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

<!-- Add fold/unfold for tables -->
<details><summary>Click Here to View Analysis Table</summary>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_tables23)Sources: summary of inputs</caption>
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
<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table21)Summary of equations use until this point in the document</caption>
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
   <td style="text-align:left;"> $E_t = \mathbf{1}(10 \text{&lt;} t \leq 15)\alpha^{KLPS2} + \mathbf{1}(15 \text{&lt;} t \leq 20)lpha^{KLPS3} + \mathbf{1}(t \text{&gt;} 20)\alpha^{KLPS4}$ </td>
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
   <td style="text-align:left;"> $CEA_{deworming} = rac{B (1 + F_{0})}{C}$ </td>
   <td style="text-align:left;"> $(17)$ </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $RCEA = rac{CEA_{deworming}}{CEA_{cash}}$ </td>
   <td style="text-align:left;"> $(18)$ </td>
  </tr>
</tbody>
</table>

<table class="table table-striped table-hover table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:sum_table21)Sources: summary of inputs specified until this point in the document</caption>
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
   <td style="text-align:left;"> $p=0.51099999975$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $i_{16}=0.1185$ </td>
   <td style="text-align:left;"> $R=0.681333333$ </td>
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
   <td style="text-align:left;"> $w_{ww}=14.5850933$ </td>
   <td style="text-align:left;"> $\hat{\beta}_1=0.1019575$ </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> $w_{se}=10.3$ </td>
   <td style="text-align:left;"> $\hat{\beta}_2=-0.0010413$ </td>
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

    lambda1_in <- lambda_eff_f(lambda1_var = lambda1_in_f(lambda1_var = lambda1_var1),
                             alpha_0_var = alpha_0_var1, alpha_r_var = alpha_r_var1)

    lambda2_in <- lambda2_in_f(lambda2_var = lambda2_var1)

    saturation_in <- as.numeric(saturation_in_f(coverage_var = coverage_var1,
                                                q_full_var = q_full_var1,
                                                q_zero_var = q_zero_var1) )
    unit_test(wage_t_in, 17.8464946727946, main_run_var = main_run_var1)
    # ADD UNIT TEST FOR SATURATION AN LAMBDAS    

    ###------------ Inputs for earnings2_f------------------------------------------
    lambda1_new_in <- lambda_eff_f(lambda1_var = lambda1_new_var1,
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

    #TO DO: remove hardcoded numbers
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

```
## [1] "Output has change at interest_in  to  0.00136568662640923"
## [1] "Output has change at interest_in  to  0.00136568662640923"
## [1] "Output has change at pv_benef_tax_nx_in  to  24.1070969615811"
## [1] "Output has change at pv_benef_tax_yx_in  to  129.79150484473"
## [1] "Output has change at pv_benef_all_nx_in  to  145.442515605316"
## [1] "Output has change at pv_benef_all_yx_in  to  783.055836167301"
## [1] "Output has change at pv_benef_all_new  to  970.890973263529"
## [1] "Output has change at costs2_in  to  11.8331988887693"
## [1] "Output has change at costs2_in_x  to  25.3328219110922"
## [1] "Output has change at costs_k  to  32.4783248798294"
```




```r
#TODO: update unit test values
#Baird 1: Costs = Baird w/tax and no externalities (no ext); Benef = Baird no ext
baird1 <- NPV_pe_f(benefits_var = pv_benef_tax_nx_in, costs_var = costs2_in)
unit_test(baird1, 11.8309012188904)
```

```
## [1] "Output has change at baird1  to  12.2738980728118"
```

```r
#Baird 2: Costs = Baird w/tax and yes externalities (no ext); Benef = Baird yes ext
baird2 <- NPV_pe_f(benefits_var = pv_benef_tax_yx_in, costs_var = costs2_in_x)
unit_test(baird2, 101.903273665711)
```

```
## [1] "Output has change at baird2  to  104.458682933638"
```

```r
# Baird 3: Benefits = Baird all and no ext; Costs = Baird no ext
baird3 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_in)
unit_test(baird3, 130.649690239252)
```

```
## [1] "Output has change at baird3  to  133.609316716546"
```

```r
# Baird 4: Benefits = Baird all and yes ext; Costs = Baird yes ext
baird4 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_in_x)
unit_test(baird4, 741.618186471615)
```

```
## [1] "Output has change at baird4  to  757.723014256209"
```

```r
#KLPS4_1: benefits = KLPS4 w/t and no ext; Costs =	Baird no ext
klps4_1 <- NPV_pe_f(benefits_var = pv_benef_tax_new, costs_var = costs_k)
unit_test(klps4_1, 125.202113576337)
```

```
## [1] "Output has change at klps4_1  to  128.446853938601"
```

```r
#KLPS4_2:benefits = KLPS4 all and no ext; Costs =	Baird no ext
klps4_2 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs_k)
unit_test(klps4_2, 917.937055971637)
```

```
## [1] "Output has change at klps4_2  to  938.4126483837"
```

```r
# res_npv_no_ext_klps_eacosts <- NPV_pe_f(benefits_var = pv_benef_in_new, costs_var = cost1_in)
# unit_test(res_npv_no_ext_klps_eacosts, 59.15516)

# EA1: no externality NPV using EAs costs
ea1 <- NPV_pe_f(benefits_var = pv_benef_all_nx_in, costs_var = costs2_ea_in)
unit_test(ea1, 142.278620185973)
```

```
## [1] "Output has change at ea1  to  145.29514628382"
```

```r
# EA2: yes externality NPV using EAs costs
ea2 <- NPV_pe_f(benefits_var = pv_benef_all_yx_in, costs_var = costs2_ea_in)
unit_test(ea2, 766.667141355337)
```

```
## [1] "Output has change at ea2  to  782.908466845805"
```

```r
# EA3: benef= KLPS all and no ext; Costs=EA
ea3 <- NPV_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in)
unit_test(ea3, 950.089412364501)
```

```
## [1] "Output has change at ea3  to  970.743603942033"
```

```r
#CEA for EA
#TODO: update CEA values.
cea_no_ext_ea <- CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0)
unit_test(cea_no_ext_ea, 6452.86204429499)
```

```
## [1] "Output has change at cea_no_ext_ea  to  6588.14849257152"
```

```r
rcea_no_ext_ea <- RCEA_pe_f( CEA_var = CEA_pe_f(benefits_var = pv_benef_all_new, costs_var = costs2_ea_in, fudging_var = 0),
         CEA_cash_var = 744)
unit_test(rcea_no_ext_ea, 8.6732016724395)
```

```
## [1] "Output has change at rcea_no_ext_ea  to  8.8550382964671"
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
   <td style="text-align:right;"> 12.3 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 133.6 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 128.4 </td>
   <td style="text-align:right;"> 938.4 </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> yes_ext </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 104.5 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 757.7 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr grouplength="1"><td colspan="7" style="border-bottom: 1px solid;"><strong>Costs: EA</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> no_ext_ </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 145.3 </td>
   <td style="text-align:right;"> 782.9 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 970.7 </td>
  </tr>
</tbody>
</table>


## Results for overall welfare (not only taxes)


- **NPV without externalities in @baird2016worms ($\lambda_2 = 0$):** 133.6    

- **NPV with externalities in @baird2016worms ($\lambda_2 = 10.2$ ):** 757.7  

- **NPV without externalities in @klps4:** 938.4   

- **NPV without externalities in EA 2019 ($\lambda_2 = 0$):** 145.3    

- **NPV with externalities in EA 2019 ($\lambda_2 = 10.2$ ):** 782.9

- **NPV without ext and benef from @klps4 and EA costs 2019 :** 970.7

- **CEA format:** 6588.1    

- **RCEA format (relative to cash):** 8.9    


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
<script type="application/json" data-for="htmlwidget-43fbbdc7f92cf116cfd3">{"x":{"data":[{"x":[-1273.66674647454,-1267.45579343262,-1261.2448403907,-1255.03388734878,-1248.82293430686,-1242.61198126495,-1236.40102822303,-1230.19007518111,-1223.97912213919,-1217.76816909727,-1211.55721605535,-1205.34626301343,-1199.13530997151,-1192.92435692959,-1186.71340388767,-1180.50245084575,-1174.29149780384,-1168.08054476192,-1161.86959172,-1155.65863867808,-1149.44768563616,-1143.23673259424,-1137.02577955232,-1130.8148265104,-1124.60387346848,-1118.39292042656,-1112.18196738464,-1105.97101434272,-1099.76006130081,-1093.54910825889,-1087.33815521697,-1081.12720217505,-1074.91624913313,-1068.70529609121,-1062.49434304929,-1056.28339000737,-1050.07243696545,-1043.86148392353,-1037.65053088161,-1031.4395778397,-1025.22862479778,-1019.01767175586,-1012.80671871394,-1006.59576567202,-1000.3848126301,-994.173859588181,-987.962906546262,-981.751953504343,-975.541000462423,-969.330047420504,-963.119094378585,-956.908141336666,-950.697188294747,-944.486235252828,-938.275282210909,-932.06432916899,-925.853376127071,-919.642423085151,-913.431470043232,-907.220517001313,-901.009563959394,-894.798610917475,-888.587657875556,-882.376704833637,-876.165751791718,-869.954798749798,-863.743845707879,-857.53289266596,-851.321939624041,-845.110986582122,-838.900033540203,-832.689080498284,-826.478127456365,-820.267174414446,-814.056221372526,-807.845268330607,-801.634315288688,-795.423362246769,-789.21240920485,-783.001456162931,-776.790503121012,-770.579550079093,-764.368597037174,-758.157643995254,-751.946690953335,-745.735737911416,-739.524784869497,-733.313831827578,-727.102878785659,-720.89192574374,-714.680972701821,-708.470019659902,-702.259066617982,-696.048113576063,-689.837160534144,-683.626207492225,-677.415254450306,-671.204301408387,-664.993348366468,-658.782395324549,-652.57144228263,-646.36048924071,-640.149536198791,-633.938583156872,-627.727630114953,-621.516677073034,-615.305724031115,-609.094770989196,-602.883817947277,-596.672864905358,-590.461911863438,-584.250958821519,-578.0400057796,-571.829052737681,-565.618099695762,-559.407146653843,-553.196193611924,-546.985240570005,-540.774287528086,-534.563334486166,-528.352381444247,-522.141428402328,-515.930475360409,-509.71952231849,-503.508569276571,-497.297616234652,-491.086663192733,-484.875710150813,-478.664757108894,-472.453804066975,-466.242851025056,-460.031897983137,-453.820944941218,-447.609991899299,-441.39903885738,-435.188085815461,-428.977132773541,-422.766179731622,-416.555226689703,-410.344273647784,-404.133320605865,-397.922367563946,-391.711414522027,-385.500461480108,-379.289508438189,-373.07855539627,-366.86760235435,-360.656649312431,-354.445696270512,-348.234743228593,-342.023790186674,-335.812837144755,-329.601884102836,-323.390931060917,-317.179978018997,-310.969024977078,-304.758071935159,-298.54711889324,-292.336165851321,-286.125212809402,-279.914259767483,-273.703306725564,-267.492353683645,-261.281400641725,-255.070447599806,-248.859494557887,-242.648541515968,-236.437588474049,-230.22663543213,-224.015682390211,-217.804729348292,-211.593776306373,-205.382823264453,-199.171870222534,-192.960917180615,-186.749964138696,-180.539011096777,-174.328058054858,-168.117105012939,-161.90615197102,-155.695198929101,-149.484245887181,-143.273292845262,-137.062339803343,-130.851386761424,-124.640433719505,-118.429480677586,-112.218527635667,-106.007574593748,-99.7966215518286,-93.5856685099095,-87.3747154679902,-81.1637624260711,-74.952809384152,-68.7418563422329,-62.5309033003139,-56.3199502583948,-50.1089972164757,-43.8980441745564,-37.6870911326373,-31.4761380907182,-25.2651850487991,-19.0542320068801,-12.843278964961,-6.63232592304189,-0.421372881122807,5.7895801607965,12.0005332027156,18.2114862446347,24.4224392865538,30.6333923284728,36.8443453703919,43.055298412311,49.2662514542301,55.4772044961494,61.6881575380685,67.8991105799876,74.1100636219066,80.3210166638257,86.5319697057448,92.7429227476639,98.953875789583,105.164828831502,111.375781873421,117.58673491534,123.79768795726,130.008640999179,136.219594041098,142.430547083017,148.641500124936,154.852453166855,161.063406208774,167.274359250693,173.485312292612,179.696265334532,185.907218376451,192.11817141837,198.329124460289,204.540077502208,210.751030544127,216.961983586046,223.172936627965,229.383889669884,235.594842711803,241.805795753723,248.016748795642,254.227701837561,260.43865487948,266.649607921399,272.860560963318,279.071514005237,285.282467047156,291.493420089075,297.704373130995,303.915326172914,310.126279214833,316.337232256752,322.548185298671,328.75913834059,334.970091382509,341.181044424429,347.391997466348,353.602950508267,359.813903550186,366.024856592105,372.235809634024,378.446762675943,384.657715717862,390.868668759781,397.079621801701,403.29057484362,409.501527885539,415.712480927458,421.923433969377,428.134387011296,434.345340053215,440.556293095134,446.767246137053,452.978199178973,459.189152220892,465.400105262811,471.61105830473,477.822011346649,484.032964388568,490.243917430487,496.454870472406,502.665823514325,508.876776556245,515.087729598164,521.298682640083,527.509635682002,533.720588723921,539.93154176584,546.142494807759,552.353447849678,558.564400891597,564.775353933516,570.986306975436,577.197260017355,583.408213059274,589.619166101193,595.830119143112,602.041072185031,608.25202522695,614.462978268869,620.673931310788,626.884884352708,633.095837394627,639.306790436546,645.517743478465,651.728696520384,657.939649562303,664.150602604222,670.361555646141,676.57250868806,682.78346172998,688.994414771899,695.205367813818,701.416320855737,707.627273897656,713.838226939575,720.049179981494,726.260133023414,732.471086065333,738.682039107252,744.892992149171,751.10394519109,757.314898233009,763.525851274928,769.736804316847,775.947757358766,782.158710400686,788.369663442604,794.580616484524,800.791569526443,807.002522568362,813.213475610281,819.4244286522,825.635381694119,831.846334736038,838.057287777958,844.268240819876,850.479193861796,856.690146903715,862.901099945634,869.112052987553,875.323006029472,881.533959071391,887.74491211331,893.95586515523,900.166818197149,906.377771239068,912.588724280987,918.799677322906,925.010630364825,931.221583406744,937.432536448663,943.643489490583,949.854442532501,956.065395574421,962.27634861634,968.487301658259,974.698254700178,980.909207742097,987.120160784016,993.331113825935,999.542066867855,1005.75301990977,1011.96397295169,1018.17492599361,1024.38587903553,1030.59683207745,1036.80778511937,1043.01873816129,1049.22969120321,1055.44064424513,1061.65159728705,1067.86255032896,1074.07350337088,1080.2844564128,1086.49540945472,1092.70636249664,1098.91731553856,1105.12826858048,1111.3392216224,1117.55017466432,1123.76112770624,1129.97208074816,1136.18303379007,1142.39398683199,1148.60493987391,1154.81589291583,1161.02684595775,1167.23779899967,1173.44875204159,1179.65970508351,1185.87065812543,1192.08161116735,1198.29256420927,1204.50351725119,1210.7144702931,1216.92542333502,1223.13637637694,1229.34732941886,1235.55828246078,1241.7692355027,1247.98018854462,1254.19114158654,1260.40209462846,1266.61304767038,1272.8240007123,1279.03495375421,1285.24590679613,1291.45685983805,1297.66781287997,1303.87876592189,1310.08971896381,1316.30067200573,1322.51162504765,1328.72257808957,1334.93353113149,1341.14448417341,1347.35543721532,1353.56639025724,1359.77734329916,1365.98829634108,1372.199249383,1378.41020242492,1384.62115546684,1390.83210850876,1397.04306155068,1403.2540145926,1409.46496763452,1415.67592067643,1421.88687371835,1428.09782676027,1434.30877980219,1440.51973284411,1446.73068588603,1452.94163892795,1459.15259196987,1465.36354501179,1471.57449805371,1477.78545109563,1483.99640413755,1490.20735717946,1496.41831022138,1502.6292632633,1508.84021630522,1515.05116934714,1521.26212238906,1527.47307543098,1533.6840284729,1539.89498151482,1546.10593455674,1552.31688759866,1558.52784064057,1564.73879368249,1570.94974672441,1577.16069976633,1583.37165280825,1589.58260585017,1595.79355889209,1602.00451193401,1608.21546497593,1614.42641801785,1620.63737105977,1626.84832410168,1633.0592771436,1639.27023018552,1645.48118322744,1651.69213626936,1657.90308931128,1664.1140423532,1670.32499539512,1676.53594843704,1682.74690147896,1688.95785452088,1695.1688075628,1701.37976060471,1707.59071364663,1713.80166668855,1720.01261973047,1726.22357277239,1732.43452581431,1738.64547885623,1744.85643189815,1751.06738494007,1757.27833798199,1763.48929102391,1769.70024406582,1775.91119710774,1782.12215014966,1788.33310319158,1794.5440562335,1800.75500927542,1806.96596231734,1813.17691535926,1819.38786840118,1825.5988214431,1831.80977448502,1838.02072752693,1844.23168056885,1850.44263361077,1856.65358665269,1862.86453969461,1869.07549273653,1875.28644577845,1881.49739882037,1887.70835186229,1893.91930490421,1900.13025794613,1900.13025794613,1900.13025794613,1893.91930490421,1887.70835186229,1881.49739882037,1875.28644577845,1869.07549273653,1862.86453969461,1856.65358665269,1850.44263361077,1844.23168056885,1838.02072752693,1831.80977448502,1825.5988214431,1819.38786840118,1813.17691535926,1806.96596231734,1800.75500927542,1794.5440562335,1788.33310319158,1782.12215014966,1775.91119710774,1769.70024406582,1763.48929102391,1757.27833798199,1751.06738494007,1744.85643189815,1738.64547885623,1732.43452581431,1726.22357277239,1720.01261973047,1713.80166668855,1707.59071364663,1701.37976060471,1695.1688075628,1688.95785452088,1682.74690147896,1676.53594843704,1670.32499539512,1664.1140423532,1657.90308931128,1651.69213626936,1645.48118322744,1639.27023018552,1633.0592771436,1626.84832410168,1620.63737105977,1614.42641801785,1608.21546497593,1602.00451193401,1595.79355889209,1589.58260585017,1583.37165280825,1577.16069976633,1570.94974672441,1564.73879368249,1558.52784064057,1552.31688759866,1546.10593455674,1539.89498151482,1533.6840284729,1527.47307543098,1521.26212238906,1515.05116934714,1508.84021630522,1502.6292632633,1496.41831022138,1490.20735717946,1483.99640413755,1477.78545109563,1471.57449805371,1465.36354501179,1459.15259196987,1452.94163892795,1446.73068588603,1440.51973284411,1434.30877980219,1428.09782676027,1421.88687371835,1415.67592067643,1409.46496763452,1403.2540145926,1397.04306155068,1390.83210850876,1384.62115546684,1378.41020242492,1372.199249383,1365.98829634108,1359.77734329916,1353.56639025724,1347.35543721532,1341.14448417341,1334.93353113149,1328.72257808957,1322.51162504765,1316.30067200573,1310.08971896381,1303.87876592189,1297.66781287997,1291.45685983805,1285.24590679613,1279.03495375421,1272.8240007123,1266.61304767038,1260.40209462846,1254.19114158654,1247.98018854462,1241.7692355027,1235.55828246078,1229.34732941886,1223.13637637694,1216.92542333502,1210.7144702931,1204.50351725119,1198.29256420927,1192.08161116735,1185.87065812543,1179.65970508351,1173.44875204159,1167.23779899967,1161.02684595775,1154.81589291583,1148.60493987391,1142.39398683199,1136.18303379007,1129.97208074816,1123.76112770624,1117.55017466432,1111.3392216224,1105.12826858048,1098.91731553856,1092.70636249664,1086.49540945472,1080.2844564128,1074.07350337088,1067.86255032896,1061.65159728705,1055.44064424513,1049.22969120321,1043.01873816129,1036.80778511937,1030.59683207745,1024.38587903553,1018.17492599361,1011.96397295169,1005.75301990977,999.542066867855,993.331113825935,987.120160784016,980.909207742097,974.698254700178,968.487301658259,962.27634861634,956.065395574421,949.854442532501,943.643489490583,937.432536448663,931.221583406744,925.010630364825,918.799677322906,912.588724280987,906.377771239068,900.166818197149,893.95586515523,887.74491211331,881.533959071391,875.323006029472,869.112052987553,862.901099945634,856.690146903715,850.479193861796,844.268240819876,838.057287777958,831.846334736038,825.635381694119,819.4244286522,813.213475610281,807.002522568362,800.791569526443,794.580616484524,788.369663442604,782.158710400686,775.947757358766,769.736804316847,763.525851274928,757.314898233009,751.10394519109,744.892992149171,738.682039107252,732.471086065333,726.260133023414,720.049179981494,713.838226939575,707.627273897656,701.416320855737,695.205367813818,688.994414771899,682.78346172998,676.57250868806,670.361555646141,664.150602604222,657.939649562303,651.728696520384,645.517743478465,639.306790436546,633.095837394627,626.884884352708,620.673931310788,614.462978268869,608.25202522695,602.041072185031,595.830119143112,589.619166101193,583.408213059274,577.197260017355,570.986306975436,564.775353933516,558.564400891597,552.353447849678,546.142494807759,539.93154176584,533.720588723921,527.509635682002,521.298682640083,515.087729598164,508.876776556245,502.665823514325,496.454870472406,490.243917430487,484.032964388568,477.822011346649,471.61105830473,465.400105262811,459.189152220892,452.978199178973,446.767246137053,440.556293095134,434.345340053215,428.134387011296,421.923433969377,415.712480927458,409.501527885539,403.29057484362,397.079621801701,390.868668759781,384.657715717862,378.446762675943,372.235809634024,366.024856592105,359.813903550186,353.602950508267,347.391997466348,341.181044424429,334.970091382509,328.75913834059,322.548185298671,316.337232256752,310.126279214833,303.915326172914,297.704373130995,291.493420089075,285.282467047156,279.071514005237,272.860560963318,266.649607921399,260.43865487948,254.227701837561,248.016748795642,241.805795753723,235.594842711803,229.383889669884,223.172936627965,216.961983586046,210.751030544127,204.540077502208,198.329124460289,192.11817141837,185.907218376451,179.696265334532,173.485312292612,167.274359250693,161.063406208774,154.852453166855,148.641500124936,142.430547083017,136.219594041098,130.008640999179,123.79768795726,117.58673491534,111.375781873421,105.164828831502,98.953875789583,92.7429227476639,86.5319697057448,80.3210166638257,74.1100636219066,67.8991105799876,61.6881575380685,55.4772044961494,49.2662514542301,43.055298412311,36.8443453703919,30.6333923284728,24.4224392865538,18.2114862446347,12.0005332027156,5.7895801607965,-0.421372881122807,-6.63232592304189,-12.843278964961,-19.0542320068801,-25.2651850487991,-31.4761380907182,-37.6870911326373,-43.8980441745564,-50.1089972164757,-56.3199502583948,-62.5309033003139,-68.7418563422329,-74.952809384152,-81.1637624260711,-87.3747154679902,-93.5856685099095,-99.7966215518286,-106.007574593748,-112.218527635667,-118.429480677586,-124.640433719505,-130.851386761424,-137.062339803343,-143.273292845262,-149.484245887181,-155.695198929101,-161.90615197102,-168.117105012939,-174.328058054858,-180.539011096777,-186.749964138696,-192.960917180615,-199.171870222534,-205.382823264453,-211.593776306373,-217.804729348292,-224.015682390211,-230.22663543213,-236.437588474049,-242.648541515968,-248.859494557887,-255.070447599806,-261.281400641725,-267.492353683645,-273.703306725564,-279.914259767483,-286.125212809402,-292.336165851321,-298.54711889324,-304.758071935159,-310.969024977078,-317.179978018997,-323.390931060917,-329.601884102836,-335.812837144755,-342.023790186674,-348.234743228593,-354.445696270512,-360.656649312431,-366.86760235435,-373.07855539627,-379.289508438189,-385.500461480108,-391.711414522027,-397.922367563946,-404.133320605865,-410.344273647784,-416.555226689703,-422.766179731622,-428.977132773541,-435.188085815461,-441.39903885738,-447.609991899299,-453.820944941218,-460.031897983137,-466.242851025056,-472.453804066975,-478.664757108894,-484.875710150813,-491.086663192733,-497.297616234652,-503.508569276571,-509.71952231849,-515.930475360409,-522.141428402328,-528.352381444247,-534.563334486166,-540.774287528086,-546.985240570005,-553.196193611924,-559.407146653843,-565.618099695762,-571.829052737681,-578.0400057796,-584.250958821519,-590.461911863438,-596.672864905358,-602.883817947277,-609.094770989196,-615.305724031115,-621.516677073034,-627.727630114953,-633.938583156872,-640.149536198791,-646.36048924071,-652.57144228263,-658.782395324549,-664.993348366468,-671.204301408387,-677.415254450306,-683.626207492225,-689.837160534144,-696.048113576063,-702.259066617982,-708.470019659902,-714.680972701821,-720.89192574374,-727.102878785659,-733.313831827578,-739.524784869497,-745.735737911416,-751.946690953335,-758.157643995254,-764.368597037174,-770.579550079093,-776.790503121012,-783.001456162931,-789.21240920485,-795.423362246769,-801.634315288688,-807.845268330607,-814.056221372526,-820.267174414446,-826.478127456365,-832.689080498284,-838.900033540203,-845.110986582122,-851.321939624041,-857.53289266596,-863.743845707879,-869.954798749798,-876.165751791718,-882.376704833637,-888.587657875556,-894.798610917475,-901.009563959394,-907.220517001313,-913.431470043232,-919.642423085151,-925.853376127071,-932.06432916899,-938.275282210909,-944.486235252828,-950.697188294747,-956.908141336666,-963.119094378585,-969.330047420504,-975.541000462423,-981.751953504343,-987.962906546262,-994.173859588181,-1000.3848126301,-1006.59576567202,-1012.80671871394,-1019.01767175586,-1025.22862479778,-1031.4395778397,-1037.65053088161,-1043.86148392353,-1050.07243696545,-1056.28339000737,-1062.49434304929,-1068.70529609121,-1074.91624913313,-1081.12720217505,-1087.33815521697,-1093.54910825889,-1099.76006130081,-1105.97101434272,-1112.18196738464,-1118.39292042656,-1124.60387346848,-1130.8148265104,-1137.02577955232,-1143.23673259424,-1149.44768563616,-1155.65863867808,-1161.86959172,-1168.08054476192,-1174.29149780384,-1180.50245084575,-1186.71340388767,-1192.92435692959,-1199.13530997151,-1205.34626301343,-1211.55721605535,-1217.76816909727,-1223.97912213919,-1230.19007518111,-1236.40102822303,-1242.61198126495,-1248.82293430686,-1255.03388734878,-1261.2448403907,-1267.45579343262,-1273.66674647454,-1273.66674647454],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.624158694140593,0.627779884906794,0.63138398213028,0.634932577290413,0.638481172450546,0.641960154406214,0.645434630987594,0.648860607711752,0.652259652778003,0.655632162390841,0.658954673972007,0.662273278065095,0.665518365166233,0.668763452267372,0.67194963072229,0.675116612419316,0.67824760761846,0.681336011255648,0.684411663838603,0.687421222347719,0.690430780856835,0.69337204466838,0.696302693034774,0.69918849924365,0.702040370149862,0.704870825387389,0.707644244069756,0.710417662752123,0.713114954885223,0.715810433243657,0.718453345631687,0.721071575637175,0.723660456665117,0.72620230311349,0.728737519703381,0.731204012367545,0.733670505031709,0.736078276780052,0.7384706020161,0.740826839334676,0.743146331200087,0.745451605294162,0.747699736238634,0.749947867183107,0.752133005840023,0.754311377031329,0.75644846569862,0.758558797047263,0.760648570291456,0.762692690238756,0.764735894553016,0.766715729687618,0.76869556482222,0.770630690010401,0.772548254578076,0.774440438790887,0.776297824161815,0.778147927998444,0.779947292155044,0.781746656311644,0.783499742513293,0.785243299620039,0.786958316140666,0.788648326255204,0.790326202551241,0.791964961533607,0.793603720515974,0.795196461303774,0.796786290977507,0.798346104122306,0.799889342737892,0.801417183603006,0.802916176650345,0.804413000949247,0.805870092361769,0.807327183774291,0.808754381468833,0.810171905257492,0.811572320912511,0.812952593263297,0.814327166617747,0.815672478481483,0.817017790345218,0.818334757583536,0.819647367781726,0.820942587908923,0.82222471677429,0.823499082432418,0.824752905997534,0.826006729562651,0.82723494975775,0.82846259449993,0.829673804091203,0.830877342236596,0.832072363151658,0.8332538085363,0.834433455886188,0.835594760366729,0.83675606484727,0.837902893400555,0.839045943802465,0.840180829786823,0.841307445373187,0.842431116188113,0.843543046627658,0.844654977067203,0.845755145887966,0.846854069696804,0.847946061730246,0.849033585157957,0.850118032205119,0.851195688532305,0.85227321427103,0.853342463482355,0.854411712693682,0.8554759122273,0.856538141021387,0.857597955676256,0.858654477771487,0.85971043370961,0.860762490403185,0.861814547096759,0.862863866535425,0.863912627471654,0.864960214660914,0.866006778763957,0.867053064934709,0.868098461460619,0.869143857986529,0.870189059690465,0.871234249353619,0.872279882161688,0.87332575829134,0.874372163099921,0.875419552783809,0.876467063177405,0.877516728350993,0.878566393524582,0.879618308865562,0.880670947307624,0.881725247304249,0.882781493502069,0.883838426426033,0.884898852276491,0.885959278126949,0.887023774685357,0.888088890001531,0.889156942955495,0.890227195752175,0.891298970651644,0.89237474718718,0.893450523722715,0.894532027272265,0.89561365182855,0.896699449763059,0.897787184149996,0.898877358691237,0.899971401466832,0.901066027702787,0.902166513104929,0.903266998507072,0.904372655837492,0.905479652428504,0.906589892640351,0.907703401670439,0.908818211303703,0.909938164804523,0.911058118305343,0.912183781526841,0.913310040151376,0.914440010755681,0.91557236138552,0.916706528616721,0.917844681762569,0.918982926321456,0.920126513347208,0.92127010037296,0.922417278149413,0.923565848354389,0.924716305702624,0.925869323300501,0.927022830593721,0.928179671630298,0.929336512666875,0.930495939208773,0.931655888463901,0.932817068839242,0.933979316761006,0.935141897784465,0.936305537506019,0.937469177227574,0.938633179818905,0.939797204310138,0.940960759792151,0.942124059199645,0.94328667840783,0.944448037637296,0.945609219583393,0.946767316178925,0.947925412774458,0.949079950732485,0.950233353106567,0.951383875301929,0.952531041369127,0.953676900949019,0.95481617721474,0.95595545348046,0.957086336138276,0.958215957291783,0.959338969460842,0.960457058519062,0.961571404611428,0.962675973656768,0.963780542702108,0.964869797267586,0.96595874881923,0.967035503557623,0.968106632611477,0.969169948310282,0.97022094507799,0.971269871185108,0.97229832455738,0.973326777929651,0.974335300142269,0.975338701896189,0.976328295486672,0.977304045250767,0.978273633007989,0.979219043992614,0.980164454977239,0.981079843980097,0.981992149474277,0.982882506690343,0.983758867319586,0.984623015369777,0.985460527102635,0.986297281572732,0.98709298445709,0.987888687341447,0.988652039689057,0.989402927292317,0.990133445756624,0.990836475277235,0.991532916592596,0.992185019603741,0.992837122614886,0.993444229542627,0.994042323322476,0.994609764849276,0.995150764246033,0.995677285548553,0.996158115322648,0.996638945096744,0.997060069551027,0.997477677131317,0.99785235361198,0.998203722197735,0.998530738642667,0.998812900578518,0.999091051148161,0.99930110149508,0.999511151841998,0.999664294255944,0.999799404477821,0.999898539426302,0.999955971108748,1,0.999977118515694,0.999954237031388,0.999859240588383,0.999753528621682,0.999598869216453,0.999407940058998,0.99919268503714,0.998914295565964,0.998635906094788,0.998269591269923,0.997901654519989,0.99747100511199,0.997011602356278,0.996515906467521,0.995963298719632,0.995401865459335,0.994754504316141,0.994107143172947,0.993383199689789,0.99263973743491,0.991847593049071,0.99100689194185,0.990146127656612,0.989207268335787,0.988268409014962,0.987239777701955,0.986202066638354,0.985103586024173,0.983966561962423,0.982798119098769,0.981561558434208,0.980323066895219,0.978986987938832,0.977650908982446,0.976243053442323,0.974807719533595,0.973330231022798,0.971796152457759,0.970249271283772,0.968617206033234,0.966985140782697,0.965272153343564,0.963543106544444,0.961762541197845,0.959937763408568,0.958090188409694,0.956171172628869,0.954252156848044,0.952245660890002,0.950234138345756,0.948163816087528,0.94606175081308,0.943928482486674,0.94173806470582,0.939542755265527,0.937266393619609,0.93499003197369,0.932650287103467,0.930290599759096,0.927893522914427,0.925453327598644,0.923000098724786,0.920482401741376,0.917964704757965,0.915382207580449,0.912790191867802,0.910157319406806,0.907494331703919,0.904812489720377,0.902082026913297,0.899351564106217,0.896558332841025,0.89376402756721,0.890928445344394,0.888074050937069,0.885197688298237,0.882287063156828,0.879371500810986,0.876408592287051,0.873445683763117,0.870444252061636,0.867433079952476,0.864399728285353,0.861344368029485,0.85828076509641,0.855185330766509,0.852089896436608,0.848961778304815,0.845830405494416,0.842679534201053,0.839516362901332,0.836344421498922,0.833153579061274,0.8299622488473,0.826747833126118,0.823533417404937,0.820304859454542,0.817070922222726,0.813830334250883,0.81058086497807,0.807329862403628,0.804068772484065,0.800807682564501,0.797540072056805,0.794271179570129,0.791000079425497,0.787727094509568,0.784453986978206,0.781180497888501,0.777907008798796,0.774636312285129,0.771365772210187,0.768099302138341,0.764835016835327,0.761574063981986,0.758319180290684,0.755065017137884,0.751822512425402,0.74858000771292,0.74534906746289,0.742121740045081,0.738902696667516,0.735693157246674,0.732487041544075,0.729297705695865,0.726108369847655,0.722938609314146,0.719771691050055,0.716618864951533,0.713476371376425,0.710341238343465,0.707224965421132,0.704108692498798,0.701019790075329,0.697931319508258,0.694862922750595,0.691803619973693,0.688756200177819,0.685727213459398,0.682701217779256,0.679703478416416,0.676705739053577,0.673733553201631,0.670767776791171,0.667818337642439,0.664885026402728,0.661958495140395,0.659057941254186,0.656157387367978,0.653286745250675,0.650419035193728,0.647571436945596,0.644736456761197,0.6419117941263,0.639109235601342,0.636307378866379,0.633536746560639,0.630766114254899,0.628018162111407,0.625278781187979,0.622552459054006,0.619843483855621,0.617138425598228,0.614458848911345,0.611779272224461,0.609123331757459,0.606471994703584,0.603835224911883,0.601210827403556,0.59859266665221,0.595993778349502,0.593394890046795,0.59081872162496,0.588243793327129,0.585683406896737,0.58313078230289,0.580585469107521,0.578053396879732,0.57552243832142,0.573009084383724,0.570495730446028,0.567995209714576,0.565498669721593,0.563009065556243,0.560527377465884,0.558047882630506,0.55557903931621,0.553110196001913,0.550650801818306,0.548192763657622,0.545739782636809,0.543290490014737,0.540843080704686,0.538400466372099,0.535957852039513,0.533519787527952,0.531081788757599,0.528645561509558,0.526210131994499,0.523774919148554,0.521340040590512,0.518905023490622,0.516468716828519,0.514032410166417,0.51159341525971,0.509153751491603,0.506711451296361,0.50426656185111,0.501820209562609,0.499368296180849,0.496916382799089,0.49445649608125,0.491995840168561,0.489528797094422,0.487057768513032,0.484582935680396,0.482100000949246,0.479616755885837,0.477120485762354,0.474624215638871,0.472117292118837,0.469606368539363,0.46708861543832,0.4645618377977,0.462032782420436,0.459489073174864,0.456945363929292,0.454386665303384,0.451825074884151,0.44925334931378,0.446673060343097,0.444088008097626,0.441488338901164,0.438888669704702,0.436270082620092,0.433650490034298,0.43101761885649,0.428377700332057,0.425730431726538,0.423069926732661,0.420408163208434,0.417726953945641,0.415045744682847,0.412348725315969,0.409646836748322,0],"text":["npv_sim: -1273.6667465<br />scaled: 0.4096468<br />1/2: 0.5","npv_sim: -1267.4557934<br />scaled: 0.4123487<br />1/2: 0.5","npv_sim: -1261.2448404<br />scaled: 0.4150457<br />1/2: 0.5","npv_sim: -1255.0338873<br />scaled: 0.4177270<br />1/2: 0.5","npv_sim: -1248.8229343<br />scaled: 0.4204082<br />1/2: 0.5","npv_sim: -1242.6119813<br />scaled: 0.4230699<br />1/2: 0.5","npv_sim: -1236.4010282<br />scaled: 0.4257304<br />1/2: 0.5","npv_sim: -1230.1900752<br />scaled: 0.4283777<br />1/2: 0.5","npv_sim: -1223.9791221<br />scaled: 0.4310176<br />1/2: 0.5","npv_sim: -1217.7681691<br />scaled: 0.4336505<br />1/2: 0.5","npv_sim: -1211.5572161<br />scaled: 0.4362701<br />1/2: 0.5","npv_sim: -1205.3462630<br />scaled: 0.4388887<br />1/2: 0.5","npv_sim: -1199.1353100<br />scaled: 0.4414883<br />1/2: 0.5","npv_sim: -1192.9243569<br />scaled: 0.4440880<br />1/2: 0.5","npv_sim: -1186.7134039<br />scaled: 0.4466731<br />1/2: 0.5","npv_sim: -1180.5024508<br />scaled: 0.4492533<br />1/2: 0.5","npv_sim: -1174.2914978<br />scaled: 0.4518251<br />1/2: 0.5","npv_sim: -1168.0805448<br />scaled: 0.4543867<br />1/2: 0.5","npv_sim: -1161.8695917<br />scaled: 0.4569454<br />1/2: 0.5","npv_sim: -1155.6586387<br />scaled: 0.4594891<br />1/2: 0.5","npv_sim: -1149.4476856<br />scaled: 0.4620328<br />1/2: 0.5","npv_sim: -1143.2367326<br />scaled: 0.4645618<br />1/2: 0.5","npv_sim: -1137.0257796<br />scaled: 0.4670886<br />1/2: 0.5","npv_sim: -1130.8148265<br />scaled: 0.4696064<br />1/2: 0.5","npv_sim: -1124.6038735<br />scaled: 0.4721173<br />1/2: 0.5","npv_sim: -1118.3929204<br />scaled: 0.4746242<br />1/2: 0.5","npv_sim: -1112.1819674<br />scaled: 0.4771205<br />1/2: 0.5","npv_sim: -1105.9710143<br />scaled: 0.4796168<br />1/2: 0.5","npv_sim: -1099.7600613<br />scaled: 0.4821000<br />1/2: 0.5","npv_sim: -1093.5491083<br />scaled: 0.4845829<br />1/2: 0.5","npv_sim: -1087.3381552<br />scaled: 0.4870578<br />1/2: 0.5","npv_sim: -1081.1272022<br />scaled: 0.4895288<br />1/2: 0.5","npv_sim: -1074.9162491<br />scaled: 0.4919958<br />1/2: 0.5","npv_sim: -1068.7052961<br />scaled: 0.4944565<br />1/2: 0.5","npv_sim: -1062.4943430<br />scaled: 0.4969164<br />1/2: 0.5","npv_sim: -1056.2833900<br />scaled: 0.4993683<br />1/2: 0.5","npv_sim: -1050.0724370<br />scaled: 0.5018202<br />1/2: 0.5","npv_sim: -1043.8614839<br />scaled: 0.5042666<br />1/2: 0.5","npv_sim: -1037.6505309<br />scaled: 0.5067115<br />1/2: 0.5","npv_sim: -1031.4395778<br />scaled: 0.5091538<br />1/2: 0.5","npv_sim: -1025.2286248<br />scaled: 0.5115934<br />1/2: 0.5","npv_sim: -1019.0176718<br />scaled: 0.5140324<br />1/2: 0.5","npv_sim: -1012.8067187<br />scaled: 0.5164687<br />1/2: 0.5","npv_sim: -1006.5957657<br />scaled: 0.5189050<br />1/2: 0.5","npv_sim: -1000.3848126<br />scaled: 0.5213400<br />1/2: 0.5","npv_sim:  -994.1738596<br />scaled: 0.5237749<br />1/2: 0.5","npv_sim:  -987.9629065<br />scaled: 0.5262101<br />1/2: 0.5","npv_sim:  -981.7519535<br />scaled: 0.5286456<br />1/2: 0.5","npv_sim:  -975.5410005<br />scaled: 0.5310818<br />1/2: 0.5","npv_sim:  -969.3300474<br />scaled: 0.5335198<br />1/2: 0.5","npv_sim:  -963.1190944<br />scaled: 0.5359579<br />1/2: 0.5","npv_sim:  -956.9081413<br />scaled: 0.5384005<br />1/2: 0.5","npv_sim:  -950.6971883<br />scaled: 0.5408431<br />1/2: 0.5","npv_sim:  -944.4862353<br />scaled: 0.5432905<br />1/2: 0.5","npv_sim:  -938.2752822<br />scaled: 0.5457398<br />1/2: 0.5","npv_sim:  -932.0643292<br />scaled: 0.5481928<br />1/2: 0.5","npv_sim:  -925.8533761<br />scaled: 0.5506508<br />1/2: 0.5","npv_sim:  -919.6424231<br />scaled: 0.5531102<br />1/2: 0.5","npv_sim:  -913.4314700<br />scaled: 0.5555790<br />1/2: 0.5","npv_sim:  -907.2205170<br />scaled: 0.5580479<br />1/2: 0.5","npv_sim:  -901.0095640<br />scaled: 0.5605274<br />1/2: 0.5","npv_sim:  -894.7986109<br />scaled: 0.5630091<br />1/2: 0.5","npv_sim:  -888.5876579<br />scaled: 0.5654987<br />1/2: 0.5","npv_sim:  -882.3767048<br />scaled: 0.5679952<br />1/2: 0.5","npv_sim:  -876.1657518<br />scaled: 0.5704957<br />1/2: 0.5","npv_sim:  -869.9547987<br />scaled: 0.5730091<br />1/2: 0.5","npv_sim:  -863.7438457<br />scaled: 0.5755224<br />1/2: 0.5","npv_sim:  -857.5328927<br />scaled: 0.5780534<br />1/2: 0.5","npv_sim:  -851.3219396<br />scaled: 0.5805855<br />1/2: 0.5","npv_sim:  -845.1109866<br />scaled: 0.5831308<br />1/2: 0.5","npv_sim:  -838.9000335<br />scaled: 0.5856834<br />1/2: 0.5","npv_sim:  -832.6890805<br />scaled: 0.5882438<br />1/2: 0.5","npv_sim:  -826.4781275<br />scaled: 0.5908187<br />1/2: 0.5","npv_sim:  -820.2671744<br />scaled: 0.5933949<br />1/2: 0.5","npv_sim:  -814.0562214<br />scaled: 0.5959938<br />1/2: 0.5","npv_sim:  -807.8452683<br />scaled: 0.5985927<br />1/2: 0.5","npv_sim:  -801.6343153<br />scaled: 0.6012108<br />1/2: 0.5","npv_sim:  -795.4233622<br />scaled: 0.6038352<br />1/2: 0.5","npv_sim:  -789.2124092<br />scaled: 0.6064720<br />1/2: 0.5","npv_sim:  -783.0014562<br />scaled: 0.6091233<br />1/2: 0.5","npv_sim:  -776.7905031<br />scaled: 0.6117793<br />1/2: 0.5","npv_sim:  -770.5795501<br />scaled: 0.6144588<br />1/2: 0.5","npv_sim:  -764.3685970<br />scaled: 0.6171384<br />1/2: 0.5","npv_sim:  -758.1576440<br />scaled: 0.6198435<br />1/2: 0.5","npv_sim:  -751.9466910<br />scaled: 0.6225525<br />1/2: 0.5","npv_sim:  -745.7357379<br />scaled: 0.6252788<br />1/2: 0.5","npv_sim:  -739.5247849<br />scaled: 0.6280182<br />1/2: 0.5","npv_sim:  -733.3138318<br />scaled: 0.6307661<br />1/2: 0.5","npv_sim:  -727.1028788<br />scaled: 0.6335367<br />1/2: 0.5","npv_sim:  -720.8919257<br />scaled: 0.6363074<br />1/2: 0.5","npv_sim:  -714.6809727<br />scaled: 0.6391092<br />1/2: 0.5","npv_sim:  -708.4700197<br />scaled: 0.6419118<br />1/2: 0.5","npv_sim:  -702.2590666<br />scaled: 0.6447365<br />1/2: 0.5","npv_sim:  -696.0481136<br />scaled: 0.6475714<br />1/2: 0.5","npv_sim:  -689.8371605<br />scaled: 0.6504190<br />1/2: 0.5","npv_sim:  -683.6262075<br />scaled: 0.6532867<br />1/2: 0.5","npv_sim:  -677.4152545<br />scaled: 0.6561574<br />1/2: 0.5","npv_sim:  -671.2043014<br />scaled: 0.6590579<br />1/2: 0.5","npv_sim:  -664.9933484<br />scaled: 0.6619585<br />1/2: 0.5","npv_sim:  -658.7823953<br />scaled: 0.6648850<br />1/2: 0.5","npv_sim:  -652.5714423<br />scaled: 0.6678183<br />1/2: 0.5","npv_sim:  -646.3604892<br />scaled: 0.6707678<br />1/2: 0.5","npv_sim:  -640.1495362<br />scaled: 0.6737336<br />1/2: 0.5","npv_sim:  -633.9385832<br />scaled: 0.6767057<br />1/2: 0.5","npv_sim:  -627.7276301<br />scaled: 0.6797035<br />1/2: 0.5","npv_sim:  -621.5166771<br />scaled: 0.6827012<br />1/2: 0.5","npv_sim:  -615.3057240<br />scaled: 0.6857272<br />1/2: 0.5","npv_sim:  -609.0947710<br />scaled: 0.6887562<br />1/2: 0.5","npv_sim:  -602.8838179<br />scaled: 0.6918036<br />1/2: 0.5","npv_sim:  -596.6728649<br />scaled: 0.6948629<br />1/2: 0.5","npv_sim:  -590.4619119<br />scaled: 0.6979313<br />1/2: 0.5","npv_sim:  -584.2509588<br />scaled: 0.7010198<br />1/2: 0.5","npv_sim:  -578.0400058<br />scaled: 0.7041087<br />1/2: 0.5","npv_sim:  -571.8290527<br />scaled: 0.7072250<br />1/2: 0.5","npv_sim:  -565.6180997<br />scaled: 0.7103412<br />1/2: 0.5","npv_sim:  -559.4071467<br />scaled: 0.7134764<br />1/2: 0.5","npv_sim:  -553.1961936<br />scaled: 0.7166189<br />1/2: 0.5","npv_sim:  -546.9852406<br />scaled: 0.7197717<br />1/2: 0.5","npv_sim:  -540.7742875<br />scaled: 0.7229386<br />1/2: 0.5","npv_sim:  -534.5633345<br />scaled: 0.7261084<br />1/2: 0.5","npv_sim:  -528.3523814<br />scaled: 0.7292977<br />1/2: 0.5","npv_sim:  -522.1414284<br />scaled: 0.7324870<br />1/2: 0.5","npv_sim:  -515.9304754<br />scaled: 0.7356932<br />1/2: 0.5","npv_sim:  -509.7195223<br />scaled: 0.7389027<br />1/2: 0.5","npv_sim:  -503.5085693<br />scaled: 0.7421217<br />1/2: 0.5","npv_sim:  -497.2976162<br />scaled: 0.7453491<br />1/2: 0.5","npv_sim:  -491.0866632<br />scaled: 0.7485800<br />1/2: 0.5","npv_sim:  -484.8757102<br />scaled: 0.7518225<br />1/2: 0.5","npv_sim:  -478.6647571<br />scaled: 0.7550650<br />1/2: 0.5","npv_sim:  -472.4538041<br />scaled: 0.7583192<br />1/2: 0.5","npv_sim:  -466.2428510<br />scaled: 0.7615741<br />1/2: 0.5","npv_sim:  -460.0318980<br />scaled: 0.7648350<br />1/2: 0.5","npv_sim:  -453.8209449<br />scaled: 0.7680993<br />1/2: 0.5","npv_sim:  -447.6099919<br />scaled: 0.7713658<br />1/2: 0.5","npv_sim:  -441.3990389<br />scaled: 0.7746363<br />1/2: 0.5","npv_sim:  -435.1880858<br />scaled: 0.7779070<br />1/2: 0.5","npv_sim:  -428.9771328<br />scaled: 0.7811805<br />1/2: 0.5","npv_sim:  -422.7661797<br />scaled: 0.7844540<br />1/2: 0.5","npv_sim:  -416.5552267<br />scaled: 0.7877271<br />1/2: 0.5","npv_sim:  -410.3442736<br />scaled: 0.7910001<br />1/2: 0.5","npv_sim:  -404.1333206<br />scaled: 0.7942712<br />1/2: 0.5","npv_sim:  -397.9223676<br />scaled: 0.7975401<br />1/2: 0.5","npv_sim:  -391.7114145<br />scaled: 0.8008077<br />1/2: 0.5","npv_sim:  -385.5004615<br />scaled: 0.8040688<br />1/2: 0.5","npv_sim:  -379.2895084<br />scaled: 0.8073299<br />1/2: 0.5","npv_sim:  -373.0785554<br />scaled: 0.8105809<br />1/2: 0.5","npv_sim:  -366.8676024<br />scaled: 0.8138303<br />1/2: 0.5","npv_sim:  -360.6566493<br />scaled: 0.8170709<br />1/2: 0.5","npv_sim:  -354.4456963<br />scaled: 0.8203049<br />1/2: 0.5","npv_sim:  -348.2347432<br />scaled: 0.8235334<br />1/2: 0.5","npv_sim:  -342.0237902<br />scaled: 0.8267478<br />1/2: 0.5","npv_sim:  -335.8128371<br />scaled: 0.8299622<br />1/2: 0.5","npv_sim:  -329.6018841<br />scaled: 0.8331536<br />1/2: 0.5","npv_sim:  -323.3909311<br />scaled: 0.8363444<br />1/2: 0.5","npv_sim:  -317.1799780<br />scaled: 0.8395164<br />1/2: 0.5","npv_sim:  -310.9690250<br />scaled: 0.8426795<br />1/2: 0.5","npv_sim:  -304.7580719<br />scaled: 0.8458304<br />1/2: 0.5","npv_sim:  -298.5471189<br />scaled: 0.8489618<br />1/2: 0.5","npv_sim:  -292.3361659<br />scaled: 0.8520899<br />1/2: 0.5","npv_sim:  -286.1252128<br />scaled: 0.8551853<br />1/2: 0.5","npv_sim:  -279.9142598<br />scaled: 0.8582808<br />1/2: 0.5","npv_sim:  -273.7033067<br />scaled: 0.8613444<br />1/2: 0.5","npv_sim:  -267.4923537<br />scaled: 0.8643997<br />1/2: 0.5","npv_sim:  -261.2814006<br />scaled: 0.8674331<br />1/2: 0.5","npv_sim:  -255.0704476<br />scaled: 0.8704443<br />1/2: 0.5","npv_sim:  -248.8594946<br />scaled: 0.8734457<br />1/2: 0.5","npv_sim:  -242.6485415<br />scaled: 0.8764086<br />1/2: 0.5","npv_sim:  -236.4375885<br />scaled: 0.8793715<br />1/2: 0.5","npv_sim:  -230.2266354<br />scaled: 0.8822871<br />1/2: 0.5","npv_sim:  -224.0156824<br />scaled: 0.8851977<br />1/2: 0.5","npv_sim:  -217.8047293<br />scaled: 0.8880741<br />1/2: 0.5","npv_sim:  -211.5937763<br />scaled: 0.8909284<br />1/2: 0.5","npv_sim:  -205.3828233<br />scaled: 0.8937640<br />1/2: 0.5","npv_sim:  -199.1718702<br />scaled: 0.8965583<br />1/2: 0.5","npv_sim:  -192.9609172<br />scaled: 0.8993516<br />1/2: 0.5","npv_sim:  -186.7499641<br />scaled: 0.9020820<br />1/2: 0.5","npv_sim:  -180.5390111<br />scaled: 0.9048125<br />1/2: 0.5","npv_sim:  -174.3280581<br />scaled: 0.9074943<br />1/2: 0.5","npv_sim:  -168.1171050<br />scaled: 0.9101573<br />1/2: 0.5","npv_sim:  -161.9061520<br />scaled: 0.9127902<br />1/2: 0.5","npv_sim:  -155.6951989<br />scaled: 0.9153822<br />1/2: 0.5","npv_sim:  -149.4842459<br />scaled: 0.9179647<br />1/2: 0.5","npv_sim:  -143.2732928<br />scaled: 0.9204824<br />1/2: 0.5","npv_sim:  -137.0623398<br />scaled: 0.9230001<br />1/2: 0.5","npv_sim:  -130.8513868<br />scaled: 0.9254533<br />1/2: 0.5","npv_sim:  -124.6404337<br />scaled: 0.9278935<br />1/2: 0.5","npv_sim:  -118.4294807<br />scaled: 0.9302906<br />1/2: 0.5","npv_sim:  -112.2185276<br />scaled: 0.9326503<br />1/2: 0.5","npv_sim:  -106.0075746<br />scaled: 0.9349900<br />1/2: 0.5","npv_sim:   -99.7966216<br />scaled: 0.9372664<br />1/2: 0.5","npv_sim:   -93.5856685<br />scaled: 0.9395428<br />1/2: 0.5","npv_sim:   -87.3747155<br />scaled: 0.9417381<br />1/2: 0.5","npv_sim:   -81.1637624<br />scaled: 0.9439285<br />1/2: 0.5","npv_sim:   -74.9528094<br />scaled: 0.9460618<br />1/2: 0.5","npv_sim:   -68.7418563<br />scaled: 0.9481638<br />1/2: 0.5","npv_sim:   -62.5309033<br />scaled: 0.9502341<br />1/2: 0.5","npv_sim:   -56.3199503<br />scaled: 0.9522457<br />1/2: 0.5","npv_sim:   -50.1089972<br />scaled: 0.9542522<br />1/2: 0.5","npv_sim:   -43.8980442<br />scaled: 0.9561712<br />1/2: 0.5","npv_sim:   -37.6870911<br />scaled: 0.9580902<br />1/2: 0.5","npv_sim:   -31.4761381<br />scaled: 0.9599378<br />1/2: 0.5","npv_sim:   -25.2651850<br />scaled: 0.9617625<br />1/2: 0.5","npv_sim:   -19.0542320<br />scaled: 0.9635431<br />1/2: 0.5","npv_sim:   -12.8432790<br />scaled: 0.9652722<br />1/2: 0.5","npv_sim:    -6.6323259<br />scaled: 0.9669851<br />1/2: 0.5","npv_sim:    -0.4213729<br />scaled: 0.9686172<br />1/2: 0.5","npv_sim:     5.7895802<br />scaled: 0.9702493<br />1/2: 0.5","npv_sim:    12.0005332<br />scaled: 0.9717962<br />1/2: 0.5","npv_sim:    18.2114862<br />scaled: 0.9733302<br />1/2: 0.5","npv_sim:    24.4224393<br />scaled: 0.9748077<br />1/2: 0.5","npv_sim:    30.6333923<br />scaled: 0.9762431<br />1/2: 0.5","npv_sim:    36.8443454<br />scaled: 0.9776509<br />1/2: 0.5","npv_sim:    43.0552984<br />scaled: 0.9789870<br />1/2: 0.5","npv_sim:    49.2662515<br />scaled: 0.9803231<br />1/2: 0.5","npv_sim:    55.4772045<br />scaled: 0.9815616<br />1/2: 0.5","npv_sim:    61.6881575<br />scaled: 0.9827981<br />1/2: 0.5","npv_sim:    67.8991106<br />scaled: 0.9839666<br />1/2: 0.5","npv_sim:    74.1100636<br />scaled: 0.9851036<br />1/2: 0.5","npv_sim:    80.3210167<br />scaled: 0.9862021<br />1/2: 0.5","npv_sim:    86.5319697<br />scaled: 0.9872398<br />1/2: 0.5","npv_sim:    92.7429227<br />scaled: 0.9882684<br />1/2: 0.5","npv_sim:    98.9538758<br />scaled: 0.9892073<br />1/2: 0.5","npv_sim:   105.1648288<br />scaled: 0.9901461<br />1/2: 0.5","npv_sim:   111.3757819<br />scaled: 0.9910069<br />1/2: 0.5","npv_sim:   117.5867349<br />scaled: 0.9918476<br />1/2: 0.5","npv_sim:   123.7976880<br />scaled: 0.9926397<br />1/2: 0.5","npv_sim:   130.0086410<br />scaled: 0.9933832<br />1/2: 0.5","npv_sim:   136.2195940<br />scaled: 0.9941071<br />1/2: 0.5","npv_sim:   142.4305471<br />scaled: 0.9947545<br />1/2: 0.5","npv_sim:   148.6415001<br />scaled: 0.9954019<br />1/2: 0.5","npv_sim:   154.8524532<br />scaled: 0.9959633<br />1/2: 0.5","npv_sim:   161.0634062<br />scaled: 0.9965159<br />1/2: 0.5","npv_sim:   167.2743593<br />scaled: 0.9970116<br />1/2: 0.5","npv_sim:   173.4853123<br />scaled: 0.9974710<br />1/2: 0.5","npv_sim:   179.6962653<br />scaled: 0.9979017<br />1/2: 0.5","npv_sim:   185.9072184<br />scaled: 0.9982696<br />1/2: 0.5","npv_sim:   192.1181714<br />scaled: 0.9986359<br />1/2: 0.5","npv_sim:   198.3291245<br />scaled: 0.9989143<br />1/2: 0.5","npv_sim:   204.5400775<br />scaled: 0.9991927<br />1/2: 0.5","npv_sim:   210.7510305<br />scaled: 0.9994079<br />1/2: 0.5","npv_sim:   216.9619836<br />scaled: 0.9995989<br />1/2: 0.5","npv_sim:   223.1729366<br />scaled: 0.9997535<br />1/2: 0.5","npv_sim:   229.3838897<br />scaled: 0.9998592<br />1/2: 0.5","npv_sim:   235.5948427<br />scaled: 0.9999542<br />1/2: 0.5","npv_sim:   241.8057958<br />scaled: 0.9999771<br />1/2: 0.5","npv_sim:   248.0167488<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   254.2277018<br />scaled: 0.9999560<br />1/2: 0.5","npv_sim:   260.4386549<br />scaled: 0.9998985<br />1/2: 0.5","npv_sim:   266.6496079<br />scaled: 0.9997994<br />1/2: 0.5","npv_sim:   272.8605610<br />scaled: 0.9996643<br />1/2: 0.5","npv_sim:   279.0715140<br />scaled: 0.9995112<br />1/2: 0.5","npv_sim:   285.2824670<br />scaled: 0.9993011<br />1/2: 0.5","npv_sim:   291.4934201<br />scaled: 0.9990911<br />1/2: 0.5","npv_sim:   297.7043731<br />scaled: 0.9988129<br />1/2: 0.5","npv_sim:   303.9153262<br />scaled: 0.9985307<br />1/2: 0.5","npv_sim:   310.1262792<br />scaled: 0.9982037<br />1/2: 0.5","npv_sim:   316.3372323<br />scaled: 0.9978524<br />1/2: 0.5","npv_sim:   322.5481853<br />scaled: 0.9974777<br />1/2: 0.5","npv_sim:   328.7591383<br />scaled: 0.9970601<br />1/2: 0.5","npv_sim:   334.9700914<br />scaled: 0.9966389<br />1/2: 0.5","npv_sim:   341.1810444<br />scaled: 0.9961581<br />1/2: 0.5","npv_sim:   347.3919975<br />scaled: 0.9956773<br />1/2: 0.5","npv_sim:   353.6029505<br />scaled: 0.9951508<br />1/2: 0.5","npv_sim:   359.8139036<br />scaled: 0.9946098<br />1/2: 0.5","npv_sim:   366.0248566<br />scaled: 0.9940423<br />1/2: 0.5","npv_sim:   372.2358096<br />scaled: 0.9934442<br />1/2: 0.5","npv_sim:   378.4467627<br />scaled: 0.9928371<br />1/2: 0.5","npv_sim:   384.6577157<br />scaled: 0.9921850<br />1/2: 0.5","npv_sim:   390.8686688<br />scaled: 0.9915329<br />1/2: 0.5","npv_sim:   397.0796218<br />scaled: 0.9908365<br />1/2: 0.5","npv_sim:   403.2905748<br />scaled: 0.9901334<br />1/2: 0.5","npv_sim:   409.5015279<br />scaled: 0.9894029<br />1/2: 0.5","npv_sim:   415.7124809<br />scaled: 0.9886520<br />1/2: 0.5","npv_sim:   421.9234340<br />scaled: 0.9878887<br />1/2: 0.5","npv_sim:   428.1343870<br />scaled: 0.9870930<br />1/2: 0.5","npv_sim:   434.3453401<br />scaled: 0.9862973<br />1/2: 0.5","npv_sim:   440.5562931<br />scaled: 0.9854605<br />1/2: 0.5","npv_sim:   446.7672461<br />scaled: 0.9846230<br />1/2: 0.5","npv_sim:   452.9781992<br />scaled: 0.9837589<br />1/2: 0.5","npv_sim:   459.1891522<br />scaled: 0.9828825<br />1/2: 0.5","npv_sim:   465.4001053<br />scaled: 0.9819921<br />1/2: 0.5","npv_sim:   471.6110583<br />scaled: 0.9810798<br />1/2: 0.5","npv_sim:   477.8220113<br />scaled: 0.9801645<br />1/2: 0.5","npv_sim:   484.0329644<br />scaled: 0.9792190<br />1/2: 0.5","npv_sim:   490.2439174<br />scaled: 0.9782736<br />1/2: 0.5","npv_sim:   496.4548705<br />scaled: 0.9773040<br />1/2: 0.5","npv_sim:   502.6658235<br />scaled: 0.9763283<br />1/2: 0.5","npv_sim:   508.8767766<br />scaled: 0.9753387<br />1/2: 0.5","npv_sim:   515.0877296<br />scaled: 0.9743353<br />1/2: 0.5","npv_sim:   521.2986826<br />scaled: 0.9733268<br />1/2: 0.5","npv_sim:   527.5096357<br />scaled: 0.9722983<br />1/2: 0.5","npv_sim:   533.7205887<br />scaled: 0.9712699<br />1/2: 0.5","npv_sim:   539.9315418<br />scaled: 0.9702209<br />1/2: 0.5","npv_sim:   546.1424948<br />scaled: 0.9691699<br />1/2: 0.5","npv_sim:   552.3534478<br />scaled: 0.9681066<br />1/2: 0.5","npv_sim:   558.5644009<br />scaled: 0.9670355<br />1/2: 0.5","npv_sim:   564.7753539<br />scaled: 0.9659587<br />1/2: 0.5","npv_sim:   570.9863070<br />scaled: 0.9648698<br />1/2: 0.5","npv_sim:   577.1972600<br />scaled: 0.9637805<br />1/2: 0.5","npv_sim:   583.4082131<br />scaled: 0.9626760<br />1/2: 0.5","npv_sim:   589.6191661<br />scaled: 0.9615714<br />1/2: 0.5","npv_sim:   595.8301191<br />scaled: 0.9604571<br />1/2: 0.5","npv_sim:   602.0410722<br />scaled: 0.9593390<br />1/2: 0.5","npv_sim:   608.2520252<br />scaled: 0.9582160<br />1/2: 0.5","npv_sim:   614.4629783<br />scaled: 0.9570863<br />1/2: 0.5","npv_sim:   620.6739313<br />scaled: 0.9559555<br />1/2: 0.5","npv_sim:   626.8848844<br />scaled: 0.9548162<br />1/2: 0.5","npv_sim:   633.0958374<br />scaled: 0.9536769<br />1/2: 0.5","npv_sim:   639.3067904<br />scaled: 0.9525310<br />1/2: 0.5","npv_sim:   645.5177435<br />scaled: 0.9513839<br />1/2: 0.5","npv_sim:   651.7286965<br />scaled: 0.9502334<br />1/2: 0.5","npv_sim:   657.9396496<br />scaled: 0.9490800<br />1/2: 0.5","npv_sim:   664.1506026<br />scaled: 0.9479254<br />1/2: 0.5","npv_sim:   670.3615556<br />scaled: 0.9467673<br />1/2: 0.5","npv_sim:   676.5725087<br />scaled: 0.9456092<br />1/2: 0.5","npv_sim:   682.7834617<br />scaled: 0.9444480<br />1/2: 0.5","npv_sim:   688.9944148<br />scaled: 0.9432867<br />1/2: 0.5","npv_sim:   695.2053678<br />scaled: 0.9421241<br />1/2: 0.5","npv_sim:   701.4163209<br />scaled: 0.9409608<br />1/2: 0.5","npv_sim:   707.6272739<br />scaled: 0.9397972<br />1/2: 0.5","npv_sim:   713.8382269<br />scaled: 0.9386332<br />1/2: 0.5","npv_sim:   720.0491800<br />scaled: 0.9374692<br />1/2: 0.5","npv_sim:   726.2601330<br />scaled: 0.9363055<br />1/2: 0.5","npv_sim:   732.4710861<br />scaled: 0.9351419<br />1/2: 0.5","npv_sim:   738.6820391<br />scaled: 0.9339793<br />1/2: 0.5","npv_sim:   744.8929921<br />scaled: 0.9328171<br />1/2: 0.5","npv_sim:   751.1039452<br />scaled: 0.9316559<br />1/2: 0.5","npv_sim:   757.3148982<br />scaled: 0.9304959<br />1/2: 0.5","npv_sim:   763.5258513<br />scaled: 0.9293365<br />1/2: 0.5","npv_sim:   769.7368043<br />scaled: 0.9281797<br />1/2: 0.5","npv_sim:   775.9477574<br />scaled: 0.9270228<br />1/2: 0.5","npv_sim:   782.1587104<br />scaled: 0.9258693<br />1/2: 0.5","npv_sim:   788.3696634<br />scaled: 0.9247163<br />1/2: 0.5","npv_sim:   794.5806165<br />scaled: 0.9235658<br />1/2: 0.5","npv_sim:   800.7915695<br />scaled: 0.9224173<br />1/2: 0.5","npv_sim:   807.0025226<br />scaled: 0.9212701<br />1/2: 0.5","npv_sim:   813.2134756<br />scaled: 0.9201265<br />1/2: 0.5","npv_sim:   819.4244287<br />scaled: 0.9189829<br />1/2: 0.5","npv_sim:   825.6353817<br />scaled: 0.9178447<br />1/2: 0.5","npv_sim:   831.8463347<br />scaled: 0.9167065<br />1/2: 0.5","npv_sim:   838.0572878<br />scaled: 0.9155724<br />1/2: 0.5","npv_sim:   844.2682408<br />scaled: 0.9144400<br />1/2: 0.5","npv_sim:   850.4791939<br />scaled: 0.9133100<br />1/2: 0.5","npv_sim:   856.6901469<br />scaled: 0.9121838<br />1/2: 0.5","npv_sim:   862.9010999<br />scaled: 0.9110581<br />1/2: 0.5","npv_sim:   869.1120530<br />scaled: 0.9099382<br />1/2: 0.5","npv_sim:   875.3230060<br />scaled: 0.9088182<br />1/2: 0.5","npv_sim:   881.5339591<br />scaled: 0.9077034<br />1/2: 0.5","npv_sim:   887.7449121<br />scaled: 0.9065899<br />1/2: 0.5","npv_sim:   893.9558652<br />scaled: 0.9054797<br />1/2: 0.5","npv_sim:   900.1668182<br />scaled: 0.9043727<br />1/2: 0.5","npv_sim:   906.3777712<br />scaled: 0.9032670<br />1/2: 0.5","npv_sim:   912.5887243<br />scaled: 0.9021665<br />1/2: 0.5","npv_sim:   918.7996773<br />scaled: 0.9010660<br />1/2: 0.5","npv_sim:   925.0106304<br />scaled: 0.8999714<br />1/2: 0.5","npv_sim:   931.2215834<br />scaled: 0.8988774<br />1/2: 0.5","npv_sim:   937.4325364<br />scaled: 0.8977872<br />1/2: 0.5","npv_sim:   943.6434895<br />scaled: 0.8966994<br />1/2: 0.5","npv_sim:   949.8544425<br />scaled: 0.8956137<br />1/2: 0.5","npv_sim:   956.0653956<br />scaled: 0.8945320<br />1/2: 0.5","npv_sim:   962.2763486<br />scaled: 0.8934505<br />1/2: 0.5","npv_sim:   968.4873017<br />scaled: 0.8923747<br />1/2: 0.5","npv_sim:   974.6982547<br />scaled: 0.8912990<br />1/2: 0.5","npv_sim:   980.9092077<br />scaled: 0.8902272<br />1/2: 0.5","npv_sim:   987.1201608<br />scaled: 0.8891569<br />1/2: 0.5","npv_sim:   993.3311138<br />scaled: 0.8880889<br />1/2: 0.5","npv_sim:   999.5420669<br />scaled: 0.8870238<br />1/2: 0.5","npv_sim:  1005.7530199<br />scaled: 0.8859593<br />1/2: 0.5","npv_sim:  1011.9639730<br />scaled: 0.8848989<br />1/2: 0.5","npv_sim:  1018.1749260<br />scaled: 0.8838384<br />1/2: 0.5","npv_sim:  1024.3858790<br />scaled: 0.8827815<br />1/2: 0.5","npv_sim:  1030.5968321<br />scaled: 0.8817252<br />1/2: 0.5","npv_sim:  1036.8077851<br />scaled: 0.8806709<br />1/2: 0.5","npv_sim:  1043.0187382<br />scaled: 0.8796183<br />1/2: 0.5","npv_sim:  1049.2296912<br />scaled: 0.8785664<br />1/2: 0.5","npv_sim:  1055.4406442<br />scaled: 0.8775167<br />1/2: 0.5","npv_sim:  1061.6515973<br />scaled: 0.8764671<br />1/2: 0.5","npv_sim:  1067.8625503<br />scaled: 0.8754196<br />1/2: 0.5","npv_sim:  1074.0735034<br />scaled: 0.8743722<br />1/2: 0.5","npv_sim:  1080.2844564<br />scaled: 0.8733258<br />1/2: 0.5","npv_sim:  1086.4954095<br />scaled: 0.8722799<br />1/2: 0.5","npv_sim:  1092.7063625<br />scaled: 0.8712342<br />1/2: 0.5","npv_sim:  1098.9173155<br />scaled: 0.8701891<br />1/2: 0.5","npv_sim:  1105.1282686<br />scaled: 0.8691439<br />1/2: 0.5","npv_sim:  1111.3392216<br />scaled: 0.8680985<br />1/2: 0.5","npv_sim:  1117.5501747<br />scaled: 0.8670531<br />1/2: 0.5","npv_sim:  1123.7611277<br />scaled: 0.8660068<br />1/2: 0.5","npv_sim:  1129.9720807<br />scaled: 0.8649602<br />1/2: 0.5","npv_sim:  1136.1830338<br />scaled: 0.8639126<br />1/2: 0.5","npv_sim:  1142.3939868<br />scaled: 0.8628639<br />1/2: 0.5","npv_sim:  1148.6049399<br />scaled: 0.8618145<br />1/2: 0.5","npv_sim:  1154.8158929<br />scaled: 0.8607625<br />1/2: 0.5","npv_sim:  1161.0268460<br />scaled: 0.8597104<br />1/2: 0.5","npv_sim:  1167.2377990<br />scaled: 0.8586545<br />1/2: 0.5","npv_sim:  1173.4487520<br />scaled: 0.8575980<br />1/2: 0.5","npv_sim:  1179.6597051<br />scaled: 0.8565381<br />1/2: 0.5","npv_sim:  1185.8706581<br />scaled: 0.8554759<br />1/2: 0.5","npv_sim:  1192.0816112<br />scaled: 0.8544117<br />1/2: 0.5","npv_sim:  1198.2925642<br />scaled: 0.8533425<br />1/2: 0.5","npv_sim:  1204.5035173<br />scaled: 0.8522732<br />1/2: 0.5","npv_sim:  1210.7144703<br />scaled: 0.8511957<br />1/2: 0.5","npv_sim:  1216.9254233<br />scaled: 0.8501180<br />1/2: 0.5","npv_sim:  1223.1363764<br />scaled: 0.8490336<br />1/2: 0.5","npv_sim:  1229.3473294<br />scaled: 0.8479461<br />1/2: 0.5","npv_sim:  1235.5582825<br />scaled: 0.8468541<br />1/2: 0.5","npv_sim:  1241.7692355<br />scaled: 0.8457551<br />1/2: 0.5","npv_sim:  1247.9801885<br />scaled: 0.8446550<br />1/2: 0.5","npv_sim:  1254.1911416<br />scaled: 0.8435430<br />1/2: 0.5","npv_sim:  1260.4020946<br />scaled: 0.8424311<br />1/2: 0.5","npv_sim:  1266.6130477<br />scaled: 0.8413074<br />1/2: 0.5","npv_sim:  1272.8240007<br />scaled: 0.8401808<br />1/2: 0.5","npv_sim:  1279.0349538<br />scaled: 0.8390459<br />1/2: 0.5","npv_sim:  1285.2459068<br />scaled: 0.8379029<br />1/2: 0.5","npv_sim:  1291.4568598<br />scaled: 0.8367561<br />1/2: 0.5","npv_sim:  1297.6678129<br />scaled: 0.8355948<br />1/2: 0.5","npv_sim:  1303.8787659<br />scaled: 0.8344335<br />1/2: 0.5","npv_sim:  1310.0897190<br />scaled: 0.8332538<br />1/2: 0.5","npv_sim:  1316.3006720<br />scaled: 0.8320724<br />1/2: 0.5","npv_sim:  1322.5116250<br />scaled: 0.8308773<br />1/2: 0.5","npv_sim:  1328.7225781<br />scaled: 0.8296738<br />1/2: 0.5","npv_sim:  1334.9335311<br />scaled: 0.8284626<br />1/2: 0.5","npv_sim:  1341.1444842<br />scaled: 0.8272349<br />1/2: 0.5","npv_sim:  1347.3554372<br />scaled: 0.8260067<br />1/2: 0.5","npv_sim:  1353.5663903<br />scaled: 0.8247529<br />1/2: 0.5","npv_sim:  1359.7773433<br />scaled: 0.8234991<br />1/2: 0.5","npv_sim:  1365.9882963<br />scaled: 0.8222247<br />1/2: 0.5","npv_sim:  1372.1992494<br />scaled: 0.8209426<br />1/2: 0.5","npv_sim:  1378.4102024<br />scaled: 0.8196474<br />1/2: 0.5","npv_sim:  1384.6211555<br />scaled: 0.8183348<br />1/2: 0.5","npv_sim:  1390.8321085<br />scaled: 0.8170178<br />1/2: 0.5","npv_sim:  1397.0430616<br />scaled: 0.8156725<br />1/2: 0.5","npv_sim:  1403.2540146<br />scaled: 0.8143272<br />1/2: 0.5","npv_sim:  1409.4649676<br />scaled: 0.8129526<br />1/2: 0.5","npv_sim:  1415.6759207<br />scaled: 0.8115723<br />1/2: 0.5","npv_sim:  1421.8868737<br />scaled: 0.8101719<br />1/2: 0.5","npv_sim:  1428.0978268<br />scaled: 0.8087544<br />1/2: 0.5","npv_sim:  1434.3087798<br />scaled: 0.8073272<br />1/2: 0.5","npv_sim:  1440.5197328<br />scaled: 0.8058701<br />1/2: 0.5","npv_sim:  1446.7306859<br />scaled: 0.8044130<br />1/2: 0.5","npv_sim:  1452.9416389<br />scaled: 0.8029162<br />1/2: 0.5","npv_sim:  1459.1525920<br />scaled: 0.8014172<br />1/2: 0.5","npv_sim:  1465.3635450<br />scaled: 0.7998893<br />1/2: 0.5","npv_sim:  1471.5744981<br />scaled: 0.7983461<br />1/2: 0.5","npv_sim:  1477.7854511<br />scaled: 0.7967863<br />1/2: 0.5","npv_sim:  1483.9964041<br />scaled: 0.7951965<br />1/2: 0.5","npv_sim:  1490.2073572<br />scaled: 0.7936037<br />1/2: 0.5","npv_sim:  1496.4183102<br />scaled: 0.7919650<br />1/2: 0.5","npv_sim:  1502.6292633<br />scaled: 0.7903262<br />1/2: 0.5","npv_sim:  1508.8402163<br />scaled: 0.7886483<br />1/2: 0.5","npv_sim:  1515.0511693<br />scaled: 0.7869583<br />1/2: 0.5","npv_sim:  1521.2621224<br />scaled: 0.7852433<br />1/2: 0.5","npv_sim:  1527.4730754<br />scaled: 0.7834997<br />1/2: 0.5","npv_sim:  1533.6840285<br />scaled: 0.7817467<br />1/2: 0.5","npv_sim:  1539.8949815<br />scaled: 0.7799473<br />1/2: 0.5","npv_sim:  1546.1059346<br />scaled: 0.7781479<br />1/2: 0.5","npv_sim:  1552.3168876<br />scaled: 0.7762978<br />1/2: 0.5","npv_sim:  1558.5278406<br />scaled: 0.7744404<br />1/2: 0.5","npv_sim:  1564.7387937<br />scaled: 0.7725483<br />1/2: 0.5","npv_sim:  1570.9497467<br />scaled: 0.7706307<br />1/2: 0.5","npv_sim:  1577.1606998<br />scaled: 0.7686956<br />1/2: 0.5","npv_sim:  1583.3716528<br />scaled: 0.7667157<br />1/2: 0.5","npv_sim:  1589.5826059<br />scaled: 0.7647359<br />1/2: 0.5","npv_sim:  1595.7935589<br />scaled: 0.7626927<br />1/2: 0.5","npv_sim:  1602.0045119<br />scaled: 0.7606486<br />1/2: 0.5","npv_sim:  1608.2154650<br />scaled: 0.7585588<br />1/2: 0.5","npv_sim:  1614.4264180<br />scaled: 0.7564485<br />1/2: 0.5","npv_sim:  1620.6373711<br />scaled: 0.7543114<br />1/2: 0.5","npv_sim:  1626.8483241<br />scaled: 0.7521330<br />1/2: 0.5","npv_sim:  1633.0592771<br />scaled: 0.7499479<br />1/2: 0.5","npv_sim:  1639.2702302<br />scaled: 0.7476997<br />1/2: 0.5","npv_sim:  1645.4811832<br />scaled: 0.7454516<br />1/2: 0.5","npv_sim:  1651.6921363<br />scaled: 0.7431463<br />1/2: 0.5","npv_sim:  1657.9030893<br />scaled: 0.7408268<br />1/2: 0.5","npv_sim:  1664.1140424<br />scaled: 0.7384706<br />1/2: 0.5","npv_sim:  1670.3249954<br />scaled: 0.7360783<br />1/2: 0.5","npv_sim:  1676.5359484<br />scaled: 0.7336705<br />1/2: 0.5","npv_sim:  1682.7469015<br />scaled: 0.7312040<br />1/2: 0.5","npv_sim:  1688.9578545<br />scaled: 0.7287375<br />1/2: 0.5","npv_sim:  1695.1688076<br />scaled: 0.7262023<br />1/2: 0.5","npv_sim:  1701.3797606<br />scaled: 0.7236605<br />1/2: 0.5","npv_sim:  1707.5907136<br />scaled: 0.7210716<br />1/2: 0.5","npv_sim:  1713.8016667<br />scaled: 0.7184533<br />1/2: 0.5","npv_sim:  1720.0126197<br />scaled: 0.7158104<br />1/2: 0.5","npv_sim:  1726.2235728<br />scaled: 0.7131150<br />1/2: 0.5","npv_sim:  1732.4345258<br />scaled: 0.7104177<br />1/2: 0.5","npv_sim:  1738.6454789<br />scaled: 0.7076442<br />1/2: 0.5","npv_sim:  1744.8564319<br />scaled: 0.7048708<br />1/2: 0.5","npv_sim:  1751.0673849<br />scaled: 0.7020404<br />1/2: 0.5","npv_sim:  1757.2783380<br />scaled: 0.6991885<br />1/2: 0.5","npv_sim:  1763.4892910<br />scaled: 0.6963027<br />1/2: 0.5","npv_sim:  1769.7002441<br />scaled: 0.6933720<br />1/2: 0.5","npv_sim:  1775.9111971<br />scaled: 0.6904308<br />1/2: 0.5","npv_sim:  1782.1221501<br />scaled: 0.6874212<br />1/2: 0.5","npv_sim:  1788.3331032<br />scaled: 0.6844117<br />1/2: 0.5","npv_sim:  1794.5440562<br />scaled: 0.6813360<br />1/2: 0.5","npv_sim:  1800.7550093<br />scaled: 0.6782476<br />1/2: 0.5","npv_sim:  1806.9659623<br />scaled: 0.6751166<br />1/2: 0.5","npv_sim:  1813.1769154<br />scaled: 0.6719496<br />1/2: 0.5","npv_sim:  1819.3878684<br />scaled: 0.6687635<br />1/2: 0.5","npv_sim:  1825.5988214<br />scaled: 0.6655184<br />1/2: 0.5","npv_sim:  1831.8097745<br />scaled: 0.6622733<br />1/2: 0.5","npv_sim:  1838.0207275<br />scaled: 0.6589547<br />1/2: 0.5","npv_sim:  1844.2316806<br />scaled: 0.6556322<br />1/2: 0.5","npv_sim:  1850.4426336<br />scaled: 0.6522597<br />1/2: 0.5","npv_sim:  1856.6535867<br />scaled: 0.6488606<br />1/2: 0.5","npv_sim:  1862.8645397<br />scaled: 0.6454346<br />1/2: 0.5","npv_sim:  1869.0754927<br />scaled: 0.6419602<br />1/2: 0.5","npv_sim:  1875.2864458<br />scaled: 0.6384812<br />1/2: 0.5","npv_sim:  1881.4973988<br />scaled: 0.6349326<br />1/2: 0.5","npv_sim:  1887.7083519<br />scaled: 0.6313840<br />1/2: 0.5","npv_sim:  1893.9193049<br />scaled: 0.6277799<br />1/2: 0.5","npv_sim:  1900.1302579<br />scaled: 0.6241587<br />1/2: 0.5","npv_sim:  1900.1302579<br />scaled: 0.6241587<br />1/2: 0.5","npv_sim:  1900.1302579<br />scaled: 0.6241587<br />1/2: 0.5","npv_sim:  1893.9193049<br />scaled: 0.6277799<br />1/2: 0.5","npv_sim:  1887.7083519<br />scaled: 0.6313840<br />1/2: 0.5","npv_sim:  1881.4973988<br />scaled: 0.6349326<br />1/2: 0.5","npv_sim:  1875.2864458<br />scaled: 0.6384812<br />1/2: 0.5","npv_sim:  1869.0754927<br />scaled: 0.6419602<br />1/2: 0.5","npv_sim:  1862.8645397<br />scaled: 0.6454346<br />1/2: 0.5","npv_sim:  1856.6535867<br />scaled: 0.6488606<br />1/2: 0.5","npv_sim:  1850.4426336<br />scaled: 0.6522597<br />1/2: 0.5","npv_sim:  1844.2316806<br />scaled: 0.6556322<br />1/2: 0.5","npv_sim:  1838.0207275<br />scaled: 0.6589547<br />1/2: 0.5","npv_sim:  1831.8097745<br />scaled: 0.6622733<br />1/2: 0.5","npv_sim:  1825.5988214<br />scaled: 0.6655184<br />1/2: 0.5","npv_sim:  1819.3878684<br />scaled: 0.6687635<br />1/2: 0.5","npv_sim:  1813.1769154<br />scaled: 0.6719496<br />1/2: 0.5","npv_sim:  1806.9659623<br />scaled: 0.6751166<br />1/2: 0.5","npv_sim:  1800.7550093<br />scaled: 0.6782476<br />1/2: 0.5","npv_sim:  1794.5440562<br />scaled: 0.6813360<br />1/2: 0.5","npv_sim:  1788.3331032<br />scaled: 0.6844117<br />1/2: 0.5","npv_sim:  1782.1221501<br />scaled: 0.6874212<br />1/2: 0.5","npv_sim:  1775.9111971<br />scaled: 0.6904308<br />1/2: 0.5","npv_sim:  1769.7002441<br />scaled: 0.6933720<br />1/2: 0.5","npv_sim:  1763.4892910<br />scaled: 0.6963027<br />1/2: 0.5","npv_sim:  1757.2783380<br />scaled: 0.6991885<br />1/2: 0.5","npv_sim:  1751.0673849<br />scaled: 0.7020404<br />1/2: 0.5","npv_sim:  1744.8564319<br />scaled: 0.7048708<br />1/2: 0.5","npv_sim:  1738.6454789<br />scaled: 0.7076442<br />1/2: 0.5","npv_sim:  1732.4345258<br />scaled: 0.7104177<br />1/2: 0.5","npv_sim:  1726.2235728<br />scaled: 0.7131150<br />1/2: 0.5","npv_sim:  1720.0126197<br />scaled: 0.7158104<br />1/2: 0.5","npv_sim:  1713.8016667<br />scaled: 0.7184533<br />1/2: 0.5","npv_sim:  1707.5907136<br />scaled: 0.7210716<br />1/2: 0.5","npv_sim:  1701.3797606<br />scaled: 0.7236605<br />1/2: 0.5","npv_sim:  1695.1688076<br />scaled: 0.7262023<br />1/2: 0.5","npv_sim:  1688.9578545<br />scaled: 0.7287375<br />1/2: 0.5","npv_sim:  1682.7469015<br />scaled: 0.7312040<br />1/2: 0.5","npv_sim:  1676.5359484<br />scaled: 0.7336705<br />1/2: 0.5","npv_sim:  1670.3249954<br />scaled: 0.7360783<br />1/2: 0.5","npv_sim:  1664.1140424<br />scaled: 0.7384706<br />1/2: 0.5","npv_sim:  1657.9030893<br />scaled: 0.7408268<br />1/2: 0.5","npv_sim:  1651.6921363<br />scaled: 0.7431463<br />1/2: 0.5","npv_sim:  1645.4811832<br />scaled: 0.7454516<br />1/2: 0.5","npv_sim:  1639.2702302<br />scaled: 0.7476997<br />1/2: 0.5","npv_sim:  1633.0592771<br />scaled: 0.7499479<br />1/2: 0.5","npv_sim:  1626.8483241<br />scaled: 0.7521330<br />1/2: 0.5","npv_sim:  1620.6373711<br />scaled: 0.7543114<br />1/2: 0.5","npv_sim:  1614.4264180<br />scaled: 0.7564485<br />1/2: 0.5","npv_sim:  1608.2154650<br />scaled: 0.7585588<br />1/2: 0.5","npv_sim:  1602.0045119<br />scaled: 0.7606486<br />1/2: 0.5","npv_sim:  1595.7935589<br />scaled: 0.7626927<br />1/2: 0.5","npv_sim:  1589.5826059<br />scaled: 0.7647359<br />1/2: 0.5","npv_sim:  1583.3716528<br />scaled: 0.7667157<br />1/2: 0.5","npv_sim:  1577.1606998<br />scaled: 0.7686956<br />1/2: 0.5","npv_sim:  1570.9497467<br />scaled: 0.7706307<br />1/2: 0.5","npv_sim:  1564.7387937<br />scaled: 0.7725483<br />1/2: 0.5","npv_sim:  1558.5278406<br />scaled: 0.7744404<br />1/2: 0.5","npv_sim:  1552.3168876<br />scaled: 0.7762978<br />1/2: 0.5","npv_sim:  1546.1059346<br />scaled: 0.7781479<br />1/2: 0.5","npv_sim:  1539.8949815<br />scaled: 0.7799473<br />1/2: 0.5","npv_sim:  1533.6840285<br />scaled: 0.7817467<br />1/2: 0.5","npv_sim:  1527.4730754<br />scaled: 0.7834997<br />1/2: 0.5","npv_sim:  1521.2621224<br />scaled: 0.7852433<br />1/2: 0.5","npv_sim:  1515.0511693<br />scaled: 0.7869583<br />1/2: 0.5","npv_sim:  1508.8402163<br />scaled: 0.7886483<br />1/2: 0.5","npv_sim:  1502.6292633<br />scaled: 0.7903262<br />1/2: 0.5","npv_sim:  1496.4183102<br />scaled: 0.7919650<br />1/2: 0.5","npv_sim:  1490.2073572<br />scaled: 0.7936037<br />1/2: 0.5","npv_sim:  1483.9964041<br />scaled: 0.7951965<br />1/2: 0.5","npv_sim:  1477.7854511<br />scaled: 0.7967863<br />1/2: 0.5","npv_sim:  1471.5744981<br />scaled: 0.7983461<br />1/2: 0.5","npv_sim:  1465.3635450<br />scaled: 0.7998893<br />1/2: 0.5","npv_sim:  1459.1525920<br />scaled: 0.8014172<br />1/2: 0.5","npv_sim:  1452.9416389<br />scaled: 0.8029162<br />1/2: 0.5","npv_sim:  1446.7306859<br />scaled: 0.8044130<br />1/2: 0.5","npv_sim:  1440.5197328<br />scaled: 0.8058701<br />1/2: 0.5","npv_sim:  1434.3087798<br />scaled: 0.8073272<br />1/2: 0.5","npv_sim:  1428.0978268<br />scaled: 0.8087544<br />1/2: 0.5","npv_sim:  1421.8868737<br />scaled: 0.8101719<br />1/2: 0.5","npv_sim:  1415.6759207<br />scaled: 0.8115723<br />1/2: 0.5","npv_sim:  1409.4649676<br />scaled: 0.8129526<br />1/2: 0.5","npv_sim:  1403.2540146<br />scaled: 0.8143272<br />1/2: 0.5","npv_sim:  1397.0430616<br />scaled: 0.8156725<br />1/2: 0.5","npv_sim:  1390.8321085<br />scaled: 0.8170178<br />1/2: 0.5","npv_sim:  1384.6211555<br />scaled: 0.8183348<br />1/2: 0.5","npv_sim:  1378.4102024<br />scaled: 0.8196474<br />1/2: 0.5","npv_sim:  1372.1992494<br />scaled: 0.8209426<br />1/2: 0.5","npv_sim:  1365.9882963<br />scaled: 0.8222247<br />1/2: 0.5","npv_sim:  1359.7773433<br />scaled: 0.8234991<br />1/2: 0.5","npv_sim:  1353.5663903<br />scaled: 0.8247529<br />1/2: 0.5","npv_sim:  1347.3554372<br />scaled: 0.8260067<br />1/2: 0.5","npv_sim:  1341.1444842<br />scaled: 0.8272349<br />1/2: 0.5","npv_sim:  1334.9335311<br />scaled: 0.8284626<br />1/2: 0.5","npv_sim:  1328.7225781<br />scaled: 0.8296738<br />1/2: 0.5","npv_sim:  1322.5116250<br />scaled: 0.8308773<br />1/2: 0.5","npv_sim:  1316.3006720<br />scaled: 0.8320724<br />1/2: 0.5","npv_sim:  1310.0897190<br />scaled: 0.8332538<br />1/2: 0.5","npv_sim:  1303.8787659<br />scaled: 0.8344335<br />1/2: 0.5","npv_sim:  1297.6678129<br />scaled: 0.8355948<br />1/2: 0.5","npv_sim:  1291.4568598<br />scaled: 0.8367561<br />1/2: 0.5","npv_sim:  1285.2459068<br />scaled: 0.8379029<br />1/2: 0.5","npv_sim:  1279.0349538<br />scaled: 0.8390459<br />1/2: 0.5","npv_sim:  1272.8240007<br />scaled: 0.8401808<br />1/2: 0.5","npv_sim:  1266.6130477<br />scaled: 0.8413074<br />1/2: 0.5","npv_sim:  1260.4020946<br />scaled: 0.8424311<br />1/2: 0.5","npv_sim:  1254.1911416<br />scaled: 0.8435430<br />1/2: 0.5","npv_sim:  1247.9801885<br />scaled: 0.8446550<br />1/2: 0.5","npv_sim:  1241.7692355<br />scaled: 0.8457551<br />1/2: 0.5","npv_sim:  1235.5582825<br />scaled: 0.8468541<br />1/2: 0.5","npv_sim:  1229.3473294<br />scaled: 0.8479461<br />1/2: 0.5","npv_sim:  1223.1363764<br />scaled: 0.8490336<br />1/2: 0.5","npv_sim:  1216.9254233<br />scaled: 0.8501180<br />1/2: 0.5","npv_sim:  1210.7144703<br />scaled: 0.8511957<br />1/2: 0.5","npv_sim:  1204.5035173<br />scaled: 0.8522732<br />1/2: 0.5","npv_sim:  1198.2925642<br />scaled: 0.8533425<br />1/2: 0.5","npv_sim:  1192.0816112<br />scaled: 0.8544117<br />1/2: 0.5","npv_sim:  1185.8706581<br />scaled: 0.8554759<br />1/2: 0.5","npv_sim:  1179.6597051<br />scaled: 0.8565381<br />1/2: 0.5","npv_sim:  1173.4487520<br />scaled: 0.8575980<br />1/2: 0.5","npv_sim:  1167.2377990<br />scaled: 0.8586545<br />1/2: 0.5","npv_sim:  1161.0268460<br />scaled: 0.8597104<br />1/2: 0.5","npv_sim:  1154.8158929<br />scaled: 0.8607625<br />1/2: 0.5","npv_sim:  1148.6049399<br />scaled: 0.8618145<br />1/2: 0.5","npv_sim:  1142.3939868<br />scaled: 0.8628639<br />1/2: 0.5","npv_sim:  1136.1830338<br />scaled: 0.8639126<br />1/2: 0.5","npv_sim:  1129.9720807<br />scaled: 0.8649602<br />1/2: 0.5","npv_sim:  1123.7611277<br />scaled: 0.8660068<br />1/2: 0.5","npv_sim:  1117.5501747<br />scaled: 0.8670531<br />1/2: 0.5","npv_sim:  1111.3392216<br />scaled: 0.8680985<br />1/2: 0.5","npv_sim:  1105.1282686<br />scaled: 0.8691439<br />1/2: 0.5","npv_sim:  1098.9173155<br />scaled: 0.8701891<br />1/2: 0.5","npv_sim:  1092.7063625<br />scaled: 0.8712342<br />1/2: 0.5","npv_sim:  1086.4954095<br />scaled: 0.8722799<br />1/2: 0.5","npv_sim:  1080.2844564<br />scaled: 0.8733258<br />1/2: 0.5","npv_sim:  1074.0735034<br />scaled: 0.8743722<br />1/2: 0.5","npv_sim:  1067.8625503<br />scaled: 0.8754196<br />1/2: 0.5","npv_sim:  1061.6515973<br />scaled: 0.8764671<br />1/2: 0.5","npv_sim:  1055.4406442<br />scaled: 0.8775167<br />1/2: 0.5","npv_sim:  1049.2296912<br />scaled: 0.8785664<br />1/2: 0.5","npv_sim:  1043.0187382<br />scaled: 0.8796183<br />1/2: 0.5","npv_sim:  1036.8077851<br />scaled: 0.8806709<br />1/2: 0.5","npv_sim:  1030.5968321<br />scaled: 0.8817252<br />1/2: 0.5","npv_sim:  1024.3858790<br />scaled: 0.8827815<br />1/2: 0.5","npv_sim:  1018.1749260<br />scaled: 0.8838384<br />1/2: 0.5","npv_sim:  1011.9639730<br />scaled: 0.8848989<br />1/2: 0.5","npv_sim:  1005.7530199<br />scaled: 0.8859593<br />1/2: 0.5","npv_sim:   999.5420669<br />scaled: 0.8870238<br />1/2: 0.5","npv_sim:   993.3311138<br />scaled: 0.8880889<br />1/2: 0.5","npv_sim:   987.1201608<br />scaled: 0.8891569<br />1/2: 0.5","npv_sim:   980.9092077<br />scaled: 0.8902272<br />1/2: 0.5","npv_sim:   974.6982547<br />scaled: 0.8912990<br />1/2: 0.5","npv_sim:   968.4873017<br />scaled: 0.8923747<br />1/2: 0.5","npv_sim:   962.2763486<br />scaled: 0.8934505<br />1/2: 0.5","npv_sim:   956.0653956<br />scaled: 0.8945320<br />1/2: 0.5","npv_sim:   949.8544425<br />scaled: 0.8956137<br />1/2: 0.5","npv_sim:   943.6434895<br />scaled: 0.8966994<br />1/2: 0.5","npv_sim:   937.4325364<br />scaled: 0.8977872<br />1/2: 0.5","npv_sim:   931.2215834<br />scaled: 0.8988774<br />1/2: 0.5","npv_sim:   925.0106304<br />scaled: 0.8999714<br />1/2: 0.5","npv_sim:   918.7996773<br />scaled: 0.9010660<br />1/2: 0.5","npv_sim:   912.5887243<br />scaled: 0.9021665<br />1/2: 0.5","npv_sim:   906.3777712<br />scaled: 0.9032670<br />1/2: 0.5","npv_sim:   900.1668182<br />scaled: 0.9043727<br />1/2: 0.5","npv_sim:   893.9558652<br />scaled: 0.9054797<br />1/2: 0.5","npv_sim:   887.7449121<br />scaled: 0.9065899<br />1/2: 0.5","npv_sim:   881.5339591<br />scaled: 0.9077034<br />1/2: 0.5","npv_sim:   875.3230060<br />scaled: 0.9088182<br />1/2: 0.5","npv_sim:   869.1120530<br />scaled: 0.9099382<br />1/2: 0.5","npv_sim:   862.9010999<br />scaled: 0.9110581<br />1/2: 0.5","npv_sim:   856.6901469<br />scaled: 0.9121838<br />1/2: 0.5","npv_sim:   850.4791939<br />scaled: 0.9133100<br />1/2: 0.5","npv_sim:   844.2682408<br />scaled: 0.9144400<br />1/2: 0.5","npv_sim:   838.0572878<br />scaled: 0.9155724<br />1/2: 0.5","npv_sim:   831.8463347<br />scaled: 0.9167065<br />1/2: 0.5","npv_sim:   825.6353817<br />scaled: 0.9178447<br />1/2: 0.5","npv_sim:   819.4244287<br />scaled: 0.9189829<br />1/2: 0.5","npv_sim:   813.2134756<br />scaled: 0.9201265<br />1/2: 0.5","npv_sim:   807.0025226<br />scaled: 0.9212701<br />1/2: 0.5","npv_sim:   800.7915695<br />scaled: 0.9224173<br />1/2: 0.5","npv_sim:   794.5806165<br />scaled: 0.9235658<br />1/2: 0.5","npv_sim:   788.3696634<br />scaled: 0.9247163<br />1/2: 0.5","npv_sim:   782.1587104<br />scaled: 0.9258693<br />1/2: 0.5","npv_sim:   775.9477574<br />scaled: 0.9270228<br />1/2: 0.5","npv_sim:   769.7368043<br />scaled: 0.9281797<br />1/2: 0.5","npv_sim:   763.5258513<br />scaled: 0.9293365<br />1/2: 0.5","npv_sim:   757.3148982<br />scaled: 0.9304959<br />1/2: 0.5","npv_sim:   751.1039452<br />scaled: 0.9316559<br />1/2: 0.5","npv_sim:   744.8929921<br />scaled: 0.9328171<br />1/2: 0.5","npv_sim:   738.6820391<br />scaled: 0.9339793<br />1/2: 0.5","npv_sim:   732.4710861<br />scaled: 0.9351419<br />1/2: 0.5","npv_sim:   726.2601330<br />scaled: 0.9363055<br />1/2: 0.5","npv_sim:   720.0491800<br />scaled: 0.9374692<br />1/2: 0.5","npv_sim:   713.8382269<br />scaled: 0.9386332<br />1/2: 0.5","npv_sim:   707.6272739<br />scaled: 0.9397972<br />1/2: 0.5","npv_sim:   701.4163209<br />scaled: 0.9409608<br />1/2: 0.5","npv_sim:   695.2053678<br />scaled: 0.9421241<br />1/2: 0.5","npv_sim:   688.9944148<br />scaled: 0.9432867<br />1/2: 0.5","npv_sim:   682.7834617<br />scaled: 0.9444480<br />1/2: 0.5","npv_sim:   676.5725087<br />scaled: 0.9456092<br />1/2: 0.5","npv_sim:   670.3615556<br />scaled: 0.9467673<br />1/2: 0.5","npv_sim:   664.1506026<br />scaled: 0.9479254<br />1/2: 0.5","npv_sim:   657.9396496<br />scaled: 0.9490800<br />1/2: 0.5","npv_sim:   651.7286965<br />scaled: 0.9502334<br />1/2: 0.5","npv_sim:   645.5177435<br />scaled: 0.9513839<br />1/2: 0.5","npv_sim:   639.3067904<br />scaled: 0.9525310<br />1/2: 0.5","npv_sim:   633.0958374<br />scaled: 0.9536769<br />1/2: 0.5","npv_sim:   626.8848844<br />scaled: 0.9548162<br />1/2: 0.5","npv_sim:   620.6739313<br />scaled: 0.9559555<br />1/2: 0.5","npv_sim:   614.4629783<br />scaled: 0.9570863<br />1/2: 0.5","npv_sim:   608.2520252<br />scaled: 0.9582160<br />1/2: 0.5","npv_sim:   602.0410722<br />scaled: 0.9593390<br />1/2: 0.5","npv_sim:   595.8301191<br />scaled: 0.9604571<br />1/2: 0.5","npv_sim:   589.6191661<br />scaled: 0.9615714<br />1/2: 0.5","npv_sim:   583.4082131<br />scaled: 0.9626760<br />1/2: 0.5","npv_sim:   577.1972600<br />scaled: 0.9637805<br />1/2: 0.5","npv_sim:   570.9863070<br />scaled: 0.9648698<br />1/2: 0.5","npv_sim:   564.7753539<br />scaled: 0.9659587<br />1/2: 0.5","npv_sim:   558.5644009<br />scaled: 0.9670355<br />1/2: 0.5","npv_sim:   552.3534478<br />scaled: 0.9681066<br />1/2: 0.5","npv_sim:   546.1424948<br />scaled: 0.9691699<br />1/2: 0.5","npv_sim:   539.9315418<br />scaled: 0.9702209<br />1/2: 0.5","npv_sim:   533.7205887<br />scaled: 0.9712699<br />1/2: 0.5","npv_sim:   527.5096357<br />scaled: 0.9722983<br />1/2: 0.5","npv_sim:   521.2986826<br />scaled: 0.9733268<br />1/2: 0.5","npv_sim:   515.0877296<br />scaled: 0.9743353<br />1/2: 0.5","npv_sim:   508.8767766<br />scaled: 0.9753387<br />1/2: 0.5","npv_sim:   502.6658235<br />scaled: 0.9763283<br />1/2: 0.5","npv_sim:   496.4548705<br />scaled: 0.9773040<br />1/2: 0.5","npv_sim:   490.2439174<br />scaled: 0.9782736<br />1/2: 0.5","npv_sim:   484.0329644<br />scaled: 0.9792190<br />1/2: 0.5","npv_sim:   477.8220113<br />scaled: 0.9801645<br />1/2: 0.5","npv_sim:   471.6110583<br />scaled: 0.9810798<br />1/2: 0.5","npv_sim:   465.4001053<br />scaled: 0.9819921<br />1/2: 0.5","npv_sim:   459.1891522<br />scaled: 0.9828825<br />1/2: 0.5","npv_sim:   452.9781992<br />scaled: 0.9837589<br />1/2: 0.5","npv_sim:   446.7672461<br />scaled: 0.9846230<br />1/2: 0.5","npv_sim:   440.5562931<br />scaled: 0.9854605<br />1/2: 0.5","npv_sim:   434.3453401<br />scaled: 0.9862973<br />1/2: 0.5","npv_sim:   428.1343870<br />scaled: 0.9870930<br />1/2: 0.5","npv_sim:   421.9234340<br />scaled: 0.9878887<br />1/2: 0.5","npv_sim:   415.7124809<br />scaled: 0.9886520<br />1/2: 0.5","npv_sim:   409.5015279<br />scaled: 0.9894029<br />1/2: 0.5","npv_sim:   403.2905748<br />scaled: 0.9901334<br />1/2: 0.5","npv_sim:   397.0796218<br />scaled: 0.9908365<br />1/2: 0.5","npv_sim:   390.8686688<br />scaled: 0.9915329<br />1/2: 0.5","npv_sim:   384.6577157<br />scaled: 0.9921850<br />1/2: 0.5","npv_sim:   378.4467627<br />scaled: 0.9928371<br />1/2: 0.5","npv_sim:   372.2358096<br />scaled: 0.9934442<br />1/2: 0.5","npv_sim:   366.0248566<br />scaled: 0.9940423<br />1/2: 0.5","npv_sim:   359.8139036<br />scaled: 0.9946098<br />1/2: 0.5","npv_sim:   353.6029505<br />scaled: 0.9951508<br />1/2: 0.5","npv_sim:   347.3919975<br />scaled: 0.9956773<br />1/2: 0.5","npv_sim:   341.1810444<br />scaled: 0.9961581<br />1/2: 0.5","npv_sim:   334.9700914<br />scaled: 0.9966389<br />1/2: 0.5","npv_sim:   328.7591383<br />scaled: 0.9970601<br />1/2: 0.5","npv_sim:   322.5481853<br />scaled: 0.9974777<br />1/2: 0.5","npv_sim:   316.3372323<br />scaled: 0.9978524<br />1/2: 0.5","npv_sim:   310.1262792<br />scaled: 0.9982037<br />1/2: 0.5","npv_sim:   303.9153262<br />scaled: 0.9985307<br />1/2: 0.5","npv_sim:   297.7043731<br />scaled: 0.9988129<br />1/2: 0.5","npv_sim:   291.4934201<br />scaled: 0.9990911<br />1/2: 0.5","npv_sim:   285.2824670<br />scaled: 0.9993011<br />1/2: 0.5","npv_sim:   279.0715140<br />scaled: 0.9995112<br />1/2: 0.5","npv_sim:   272.8605610<br />scaled: 0.9996643<br />1/2: 0.5","npv_sim:   266.6496079<br />scaled: 0.9997994<br />1/2: 0.5","npv_sim:   260.4386549<br />scaled: 0.9998985<br />1/2: 0.5","npv_sim:   254.2277018<br />scaled: 0.9999560<br />1/2: 0.5","npv_sim:   248.0167488<br />scaled: 1.0000000<br />1/2: 0.5","npv_sim:   241.8057958<br />scaled: 0.9999771<br />1/2: 0.5","npv_sim:   235.5948427<br />scaled: 0.9999542<br />1/2: 0.5","npv_sim:   229.3838897<br />scaled: 0.9998592<br />1/2: 0.5","npv_sim:   223.1729366<br />scaled: 0.9997535<br />1/2: 0.5","npv_sim:   216.9619836<br />scaled: 0.9995989<br />1/2: 0.5","npv_sim:   210.7510305<br />scaled: 0.9994079<br />1/2: 0.5","npv_sim:   204.5400775<br />scaled: 0.9991927<br />1/2: 0.5","npv_sim:   198.3291245<br />scaled: 0.9989143<br />1/2: 0.5","npv_sim:   192.1181714<br />scaled: 0.9986359<br />1/2: 0.5","npv_sim:   185.9072184<br />scaled: 0.9982696<br />1/2: 0.5","npv_sim:   179.6962653<br />scaled: 0.9979017<br />1/2: 0.5","npv_sim:   173.4853123<br />scaled: 0.9974710<br />1/2: 0.5","npv_sim:   167.2743593<br />scaled: 0.9970116<br />1/2: 0.5","npv_sim:   161.0634062<br />scaled: 0.9965159<br />1/2: 0.5","npv_sim:   154.8524532<br />scaled: 0.9959633<br />1/2: 0.5","npv_sim:   148.6415001<br />scaled: 0.9954019<br />1/2: 0.5","npv_sim:   142.4305471<br />scaled: 0.9947545<br />1/2: 0.5","npv_sim:   136.2195940<br />scaled: 0.9941071<br />1/2: 0.5","npv_sim:   130.0086410<br />scaled: 0.9933832<br />1/2: 0.5","npv_sim:   123.7976880<br />scaled: 0.9926397<br />1/2: 0.5","npv_sim:   117.5867349<br />scaled: 0.9918476<br />1/2: 0.5","npv_sim:   111.3757819<br />scaled: 0.9910069<br />1/2: 0.5","npv_sim:   105.1648288<br />scaled: 0.9901461<br />1/2: 0.5","npv_sim:    98.9538758<br />scaled: 0.9892073<br />1/2: 0.5","npv_sim:    92.7429227<br />scaled: 0.9882684<br />1/2: 0.5","npv_sim:    86.5319697<br />scaled: 0.9872398<br />1/2: 0.5","npv_sim:    80.3210167<br />scaled: 0.9862021<br />1/2: 0.5","npv_sim:    74.1100636<br />scaled: 0.9851036<br />1/2: 0.5","npv_sim:    67.8991106<br />scaled: 0.9839666<br />1/2: 0.5","npv_sim:    61.6881575<br />scaled: 0.9827981<br />1/2: 0.5","npv_sim:    55.4772045<br />scaled: 0.9815616<br />1/2: 0.5","npv_sim:    49.2662515<br />scaled: 0.9803231<br />1/2: 0.5","npv_sim:    43.0552984<br />scaled: 0.9789870<br />1/2: 0.5","npv_sim:    36.8443454<br />scaled: 0.9776509<br />1/2: 0.5","npv_sim:    30.6333923<br />scaled: 0.9762431<br />1/2: 0.5","npv_sim:    24.4224393<br />scaled: 0.9748077<br />1/2: 0.5","npv_sim:    18.2114862<br />scaled: 0.9733302<br />1/2: 0.5","npv_sim:    12.0005332<br />scaled: 0.9717962<br />1/2: 0.5","npv_sim:     5.7895802<br />scaled: 0.9702493<br />1/2: 0.5","npv_sim:    -0.4213729<br />scaled: 0.9686172<br />1/2: 0.5","npv_sim:    -6.6323259<br />scaled: 0.9669851<br />1/2: 0.5","npv_sim:   -12.8432790<br />scaled: 0.9652722<br />1/2: 0.5","npv_sim:   -19.0542320<br />scaled: 0.9635431<br />1/2: 0.5","npv_sim:   -25.2651850<br />scaled: 0.9617625<br />1/2: 0.5","npv_sim:   -31.4761381<br />scaled: 0.9599378<br />1/2: 0.5","npv_sim:   -37.6870911<br />scaled: 0.9580902<br />1/2: 0.5","npv_sim:   -43.8980442<br />scaled: 0.9561712<br />1/2: 0.5","npv_sim:   -50.1089972<br />scaled: 0.9542522<br />1/2: 0.5","npv_sim:   -56.3199503<br />scaled: 0.9522457<br />1/2: 0.5","npv_sim:   -62.5309033<br />scaled: 0.9502341<br />1/2: 0.5","npv_sim:   -68.7418563<br />scaled: 0.9481638<br />1/2: 0.5","npv_sim:   -74.9528094<br />scaled: 0.9460618<br />1/2: 0.5","npv_sim:   -81.1637624<br />scaled: 0.9439285<br />1/2: 0.5","npv_sim:   -87.3747155<br />scaled: 0.9417381<br />1/2: 0.5","npv_sim:   -93.5856685<br />scaled: 0.9395428<br />1/2: 0.5","npv_sim:   -99.7966216<br />scaled: 0.9372664<br />1/2: 0.5","npv_sim:  -106.0075746<br />scaled: 0.9349900<br />1/2: 0.5","npv_sim:  -112.2185276<br />scaled: 0.9326503<br />1/2: 0.5","npv_sim:  -118.4294807<br />scaled: 0.9302906<br />1/2: 0.5","npv_sim:  -124.6404337<br />scaled: 0.9278935<br />1/2: 0.5","npv_sim:  -130.8513868<br />scaled: 0.9254533<br />1/2: 0.5","npv_sim:  -137.0623398<br />scaled: 0.9230001<br />1/2: 0.5","npv_sim:  -143.2732928<br />scaled: 0.9204824<br />1/2: 0.5","npv_sim:  -149.4842459<br />scaled: 0.9179647<br />1/2: 0.5","npv_sim:  -155.6951989<br />scaled: 0.9153822<br />1/2: 0.5","npv_sim:  -161.9061520<br />scaled: 0.9127902<br />1/2: 0.5","npv_sim:  -168.1171050<br />scaled: 0.9101573<br />1/2: 0.5","npv_sim:  -174.3280581<br />scaled: 0.9074943<br />1/2: 0.5","npv_sim:  -180.5390111<br />scaled: 0.9048125<br />1/2: 0.5","npv_sim:  -186.7499641<br />scaled: 0.9020820<br />1/2: 0.5","npv_sim:  -192.9609172<br />scaled: 0.8993516<br />1/2: 0.5","npv_sim:  -199.1718702<br />scaled: 0.8965583<br />1/2: 0.5","npv_sim:  -205.3828233<br />scaled: 0.8937640<br />1/2: 0.5","npv_sim:  -211.5937763<br />scaled: 0.8909284<br />1/2: 0.5","npv_sim:  -217.8047293<br />scaled: 0.8880741<br />1/2: 0.5","npv_sim:  -224.0156824<br />scaled: 0.8851977<br />1/2: 0.5","npv_sim:  -230.2266354<br />scaled: 0.8822871<br />1/2: 0.5","npv_sim:  -236.4375885<br />scaled: 0.8793715<br />1/2: 0.5","npv_sim:  -242.6485415<br />scaled: 0.8764086<br />1/2: 0.5","npv_sim:  -248.8594946<br />scaled: 0.8734457<br />1/2: 0.5","npv_sim:  -255.0704476<br />scaled: 0.8704443<br />1/2: 0.5","npv_sim:  -261.2814006<br />scaled: 0.8674331<br />1/2: 0.5","npv_sim:  -267.4923537<br />scaled: 0.8643997<br />1/2: 0.5","npv_sim:  -273.7033067<br />scaled: 0.8613444<br />1/2: 0.5","npv_sim:  -279.9142598<br />scaled: 0.8582808<br />1/2: 0.5","npv_sim:  -286.1252128<br />scaled: 0.8551853<br />1/2: 0.5","npv_sim:  -292.3361659<br />scaled: 0.8520899<br />1/2: 0.5","npv_sim:  -298.5471189<br />scaled: 0.8489618<br />1/2: 0.5","npv_sim:  -304.7580719<br />scaled: 0.8458304<br />1/2: 0.5","npv_sim:  -310.9690250<br />scaled: 0.8426795<br />1/2: 0.5","npv_sim:  -317.1799780<br />scaled: 0.8395164<br />1/2: 0.5","npv_sim:  -323.3909311<br />scaled: 0.8363444<br />1/2: 0.5","npv_sim:  -329.6018841<br />scaled: 0.8331536<br />1/2: 0.5","npv_sim:  -335.8128371<br />scaled: 0.8299622<br />1/2: 0.5","npv_sim:  -342.0237902<br />scaled: 0.8267478<br />1/2: 0.5","npv_sim:  -348.2347432<br />scaled: 0.8235334<br />1/2: 0.5","npv_sim:  -354.4456963<br />scaled: 0.8203049<br />1/2: 0.5","npv_sim:  -360.6566493<br />scaled: 0.8170709<br />1/2: 0.5","npv_sim:  -366.8676024<br />scaled: 0.8138303<br />1/2: 0.5","npv_sim:  -373.0785554<br />scaled: 0.8105809<br />1/2: 0.5","npv_sim:  -379.2895084<br />scaled: 0.8073299<br />1/2: 0.5","npv_sim:  -385.5004615<br />scaled: 0.8040688<br />1/2: 0.5","npv_sim:  -391.7114145<br />scaled: 0.8008077<br />1/2: 0.5","npv_sim:  -397.9223676<br />scaled: 0.7975401<br />1/2: 0.5","npv_sim:  -404.1333206<br />scaled: 0.7942712<br />1/2: 0.5","npv_sim:  -410.3442736<br />scaled: 0.7910001<br />1/2: 0.5","npv_sim:  -416.5552267<br />scaled: 0.7877271<br />1/2: 0.5","npv_sim:  -422.7661797<br />scaled: 0.7844540<br />1/2: 0.5","npv_sim:  -428.9771328<br />scaled: 0.7811805<br />1/2: 0.5","npv_sim:  -435.1880858<br />scaled: 0.7779070<br />1/2: 0.5","npv_sim:  -441.3990389<br />scaled: 0.7746363<br />1/2: 0.5","npv_sim:  -447.6099919<br />scaled: 0.7713658<br />1/2: 0.5","npv_sim:  -453.8209449<br />scaled: 0.7680993<br />1/2: 0.5","npv_sim:  -460.0318980<br />scaled: 0.7648350<br />1/2: 0.5","npv_sim:  -466.2428510<br />scaled: 0.7615741<br />1/2: 0.5","npv_sim:  -472.4538041<br />scaled: 0.7583192<br />1/2: 0.5","npv_sim:  -478.6647571<br />scaled: 0.7550650<br />1/2: 0.5","npv_sim:  -484.8757102<br />scaled: 0.7518225<br />1/2: 0.5","npv_sim:  -491.0866632<br />scaled: 0.7485800<br />1/2: 0.5","npv_sim:  -497.2976162<br />scaled: 0.7453491<br />1/2: 0.5","npv_sim:  -503.5085693<br />scaled: 0.7421217<br />1/2: 0.5","npv_sim:  -509.7195223<br />scaled: 0.7389027<br />1/2: 0.5","npv_sim:  -515.9304754<br />scaled: 0.7356932<br />1/2: 0.5","npv_sim:  -522.1414284<br />scaled: 0.7324870<br />1/2: 0.5","npv_sim:  -528.3523814<br />scaled: 0.7292977<br />1/2: 0.5","npv_sim:  -534.5633345<br />scaled: 0.7261084<br />1/2: 0.5","npv_sim:  -540.7742875<br />scaled: 0.7229386<br />1/2: 0.5","npv_sim:  -546.9852406<br />scaled: 0.7197717<br />1/2: 0.5","npv_sim:  -553.1961936<br />scaled: 0.7166189<br />1/2: 0.5","npv_sim:  -559.4071467<br />scaled: 0.7134764<br />1/2: 0.5","npv_sim:  -565.6180997<br />scaled: 0.7103412<br />1/2: 0.5","npv_sim:  -571.8290527<br />scaled: 0.7072250<br />1/2: 0.5","npv_sim:  -578.0400058<br />scaled: 0.7041087<br />1/2: 0.5","npv_sim:  -584.2509588<br />scaled: 0.7010198<br />1/2: 0.5","npv_sim:  -590.4619119<br />scaled: 0.6979313<br />1/2: 0.5","npv_sim:  -596.6728649<br />scaled: 0.6948629<br />1/2: 0.5","npv_sim:  -602.8838179<br />scaled: 0.6918036<br />1/2: 0.5","npv_sim:  -609.0947710<br />scaled: 0.6887562<br />1/2: 0.5","npv_sim:  -615.3057240<br />scaled: 0.6857272<br />1/2: 0.5","npv_sim:  -621.5166771<br />scaled: 0.6827012<br />1/2: 0.5","npv_sim:  -627.7276301<br />scaled: 0.6797035<br />1/2: 0.5","npv_sim:  -633.9385832<br />scaled: 0.6767057<br />1/2: 0.5","npv_sim:  -640.1495362<br />scaled: 0.6737336<br />1/2: 0.5","npv_sim:  -646.3604892<br />scaled: 0.6707678<br />1/2: 0.5","npv_sim:  -652.5714423<br />scaled: 0.6678183<br />1/2: 0.5","npv_sim:  -658.7823953<br />scaled: 0.6648850<br />1/2: 0.5","npv_sim:  -664.9933484<br />scaled: 0.6619585<br />1/2: 0.5","npv_sim:  -671.2043014<br />scaled: 0.6590579<br />1/2: 0.5","npv_sim:  -677.4152545<br />scaled: 0.6561574<br />1/2: 0.5","npv_sim:  -683.6262075<br />scaled: 0.6532867<br />1/2: 0.5","npv_sim:  -689.8371605<br />scaled: 0.6504190<br />1/2: 0.5","npv_sim:  -696.0481136<br />scaled: 0.6475714<br />1/2: 0.5","npv_sim:  -702.2590666<br />scaled: 0.6447365<br />1/2: 0.5","npv_sim:  -708.4700197<br />scaled: 0.6419118<br />1/2: 0.5","npv_sim:  -714.6809727<br />scaled: 0.6391092<br />1/2: 0.5","npv_sim:  -720.8919257<br />scaled: 0.6363074<br />1/2: 0.5","npv_sim:  -727.1028788<br />scaled: 0.6335367<br />1/2: 0.5","npv_sim:  -733.3138318<br />scaled: 0.6307661<br />1/2: 0.5","npv_sim:  -739.5247849<br />scaled: 0.6280182<br />1/2: 0.5","npv_sim:  -745.7357379<br />scaled: 0.6252788<br />1/2: 0.5","npv_sim:  -751.9466910<br />scaled: 0.6225525<br />1/2: 0.5","npv_sim:  -758.1576440<br />scaled: 0.6198435<br />1/2: 0.5","npv_sim:  -764.3685970<br />scaled: 0.6171384<br />1/2: 0.5","npv_sim:  -770.5795501<br />scaled: 0.6144588<br />1/2: 0.5","npv_sim:  -776.7905031<br />scaled: 0.6117793<br />1/2: 0.5","npv_sim:  -783.0014562<br />scaled: 0.6091233<br />1/2: 0.5","npv_sim:  -789.2124092<br />scaled: 0.6064720<br />1/2: 0.5","npv_sim:  -795.4233622<br />scaled: 0.6038352<br />1/2: 0.5","npv_sim:  -801.6343153<br />scaled: 0.6012108<br />1/2: 0.5","npv_sim:  -807.8452683<br />scaled: 0.5985927<br />1/2: 0.5","npv_sim:  -814.0562214<br />scaled: 0.5959938<br />1/2: 0.5","npv_sim:  -820.2671744<br />scaled: 0.5933949<br />1/2: 0.5","npv_sim:  -826.4781275<br />scaled: 0.5908187<br />1/2: 0.5","npv_sim:  -832.6890805<br />scaled: 0.5882438<br />1/2: 0.5","npv_sim:  -838.9000335<br />scaled: 0.5856834<br />1/2: 0.5","npv_sim:  -845.1109866<br />scaled: 0.5831308<br />1/2: 0.5","npv_sim:  -851.3219396<br />scaled: 0.5805855<br />1/2: 0.5","npv_sim:  -857.5328927<br />scaled: 0.5780534<br />1/2: 0.5","npv_sim:  -863.7438457<br />scaled: 0.5755224<br />1/2: 0.5","npv_sim:  -869.9547987<br />scaled: 0.5730091<br />1/2: 0.5","npv_sim:  -876.1657518<br />scaled: 0.5704957<br />1/2: 0.5","npv_sim:  -882.3767048<br />scaled: 0.5679952<br />1/2: 0.5","npv_sim:  -888.5876579<br />scaled: 0.5654987<br />1/2: 0.5","npv_sim:  -894.7986109<br />scaled: 0.5630091<br />1/2: 0.5","npv_sim:  -901.0095640<br />scaled: 0.5605274<br />1/2: 0.5","npv_sim:  -907.2205170<br />scaled: 0.5580479<br />1/2: 0.5","npv_sim:  -913.4314700<br />scaled: 0.5555790<br />1/2: 0.5","npv_sim:  -919.6424231<br />scaled: 0.5531102<br />1/2: 0.5","npv_sim:  -925.8533761<br />scaled: 0.5506508<br />1/2: 0.5","npv_sim:  -932.0643292<br />scaled: 0.5481928<br />1/2: 0.5","npv_sim:  -938.2752822<br />scaled: 0.5457398<br />1/2: 0.5","npv_sim:  -944.4862353<br />scaled: 0.5432905<br />1/2: 0.5","npv_sim:  -950.6971883<br />scaled: 0.5408431<br />1/2: 0.5","npv_sim:  -956.9081413<br />scaled: 0.5384005<br />1/2: 0.5","npv_sim:  -963.1190944<br />scaled: 0.5359579<br />1/2: 0.5","npv_sim:  -969.3300474<br />scaled: 0.5335198<br />1/2: 0.5","npv_sim:  -975.5410005<br />scaled: 0.5310818<br />1/2: 0.5","npv_sim:  -981.7519535<br />scaled: 0.5286456<br />1/2: 0.5","npv_sim:  -987.9629065<br />scaled: 0.5262101<br />1/2: 0.5","npv_sim:  -994.1738596<br />scaled: 0.5237749<br />1/2: 0.5","npv_sim: -1000.3848126<br />scaled: 0.5213400<br />1/2: 0.5","npv_sim: -1006.5957657<br />scaled: 0.5189050<br />1/2: 0.5","npv_sim: -1012.8067187<br />scaled: 0.5164687<br />1/2: 0.5","npv_sim: -1019.0176718<br />scaled: 0.5140324<br />1/2: 0.5","npv_sim: -1025.2286248<br />scaled: 0.5115934<br />1/2: 0.5","npv_sim: -1031.4395778<br />scaled: 0.5091538<br />1/2: 0.5","npv_sim: -1037.6505309<br />scaled: 0.5067115<br />1/2: 0.5","npv_sim: -1043.8614839<br />scaled: 0.5042666<br />1/2: 0.5","npv_sim: -1050.0724370<br />scaled: 0.5018202<br />1/2: 0.5","npv_sim: -1056.2833900<br />scaled: 0.4993683<br />1/2: 0.5","npv_sim: -1062.4943430<br />scaled: 0.4969164<br />1/2: 0.5","npv_sim: -1068.7052961<br />scaled: 0.4944565<br />1/2: 0.5","npv_sim: -1074.9162491<br />scaled: 0.4919958<br />1/2: 0.5","npv_sim: -1081.1272022<br />scaled: 0.4895288<br />1/2: 0.5","npv_sim: -1087.3381552<br />scaled: 0.4870578<br />1/2: 0.5","npv_sim: -1093.5491083<br />scaled: 0.4845829<br />1/2: 0.5","npv_sim: -1099.7600613<br />scaled: 0.4821000<br />1/2: 0.5","npv_sim: -1105.9710143<br />scaled: 0.4796168<br />1/2: 0.5","npv_sim: -1112.1819674<br />scaled: 0.4771205<br />1/2: 0.5","npv_sim: -1118.3929204<br />scaled: 0.4746242<br />1/2: 0.5","npv_sim: -1124.6038735<br />scaled: 0.4721173<br />1/2: 0.5","npv_sim: -1130.8148265<br />scaled: 0.4696064<br />1/2: 0.5","npv_sim: -1137.0257796<br />scaled: 0.4670886<br />1/2: 0.5","npv_sim: -1143.2367326<br />scaled: 0.4645618<br />1/2: 0.5","npv_sim: -1149.4476856<br />scaled: 0.4620328<br />1/2: 0.5","npv_sim: -1155.6586387<br />scaled: 0.4594891<br />1/2: 0.5","npv_sim: -1161.8695917<br />scaled: 0.4569454<br />1/2: 0.5","npv_sim: -1168.0805448<br />scaled: 0.4543867<br />1/2: 0.5","npv_sim: -1174.2914978<br />scaled: 0.4518251<br />1/2: 0.5","npv_sim: -1180.5024508<br />scaled: 0.4492533<br />1/2: 0.5","npv_sim: -1186.7134039<br />scaled: 0.4466731<br />1/2: 0.5","npv_sim: -1192.9243569<br />scaled: 0.4440880<br />1/2: 0.5","npv_sim: -1199.1353100<br />scaled: 0.4414883<br />1/2: 0.5","npv_sim: -1205.3462630<br />scaled: 0.4388887<br />1/2: 0.5","npv_sim: -1211.5572161<br />scaled: 0.4362701<br />1/2: 0.5","npv_sim: -1217.7681691<br />scaled: 0.4336505<br />1/2: 0.5","npv_sim: -1223.9791221<br />scaled: 0.4310176<br />1/2: 0.5","npv_sim: -1230.1900752<br />scaled: 0.4283777<br />1/2: 0.5","npv_sim: -1236.4010282<br />scaled: 0.4257304<br />1/2: 0.5","npv_sim: -1242.6119813<br />scaled: 0.4230699<br />1/2: 0.5","npv_sim: -1248.8229343<br />scaled: 0.4204082<br />1/2: 0.5","npv_sim: -1255.0338873<br />scaled: 0.4177270<br />1/2: 0.5","npv_sim: -1261.2448404<br />scaled: 0.4150457<br />1/2: 0.5","npv_sim: -1267.4557934<br />scaled: 0.4123487<br />1/2: 0.5","npv_sim: -1273.6667465<br />scaled: 0.4096468<br />1/2: 0.5","npv_sim: -1273.6667465<br />scaled: 0.4096468<br />1/2: 0.5"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,0.55)","dash":"solid"},"fill":"toself","fillcolor":"transparent","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,0,null,397.396653052609,397.396653052609],"y":[-0.05,1.05,null,-0.05,1.05],"text":["xintercept:   0.0000","xintercept:   0.0000",null,"xintercept: 397.3967","xintercept: 397.3967"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[596.094979578914],"y":[0.25],"text":"Median NPV:<br />  397.4","hovertext":"x: 596.095<br />y: 0.25","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[596.094979578914],"y":[0.1],"text":"SD NPV:<br />  1036.85","hovertext":"x: 596.095<br />y: 0.1","textfont":{"size":15.1181102362205,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":10.958904109589},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Distribution of Economic of Effects From Deworming (NPV)","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1718.82791603469,2470.58412980059],"tickmode":"array","ticktext":["-1000","0","1000","2000"],"tickvals":[-1000,0,1000,2000],"categoryorder":"array","categoryarray":["-1000","0","1000","2000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"NPV","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.05,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.65296803652968,"tickwidth":0,"showticklabels":false,"tickfont":{"color":null,"family":null,"size":0},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"607f7c2a6822":{"x":{},"y":{},"alpha":{},"type":"scatter"},"607f441acfdd":{"xintercept":{}},"607f3c44d919":{"x":{},"y":{}},"607f4c091738":{"x":{},"y":{}}},"cur_data":"607f7c2a6822","visdat":{"607f7c2a6822":["function (y) ","x"],"607f441acfdd":["function (y) ","x"],"607f3c44d919":["function (y) ","x"],"607f4c091738":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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
