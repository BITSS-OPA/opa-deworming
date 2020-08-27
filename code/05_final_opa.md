---
pdf_document:
  extra_dependencies: ["xcolor"]
date: "26 August, 2020"
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
    #prevalence_0_so <- c("hookworm" = 0.77, "roundworm" = 0.42, "whipworm" =0.55,
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
    
    #Start renaming to eta throughout
    prevalence_0_so <- 1#0.77 #0.77
    prevalence_r_so <- 1#0.15 #0.15
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
# New costs are all in dollars so, will compute them using ex rate of 1.
s2_in <- s2_f_new(
  interest_var = interest_19,
  unit_cost_local_var = unit_cost_2017usdppp_so,
  ex_rate_var = 1
)
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
# - inputs: lambda1_in_f(), prevalence_0_so, prevalence_r_so
# - outputs: lambda_eff_f
chunk_lambdas_eff<- function(){
###############################################################################
###############################################################################    

    lambda_eff_f <- function(lambda1_var = lambda1_in_f(), 
                           prevalence_0_var = prevalence_0_so, 
                           prevalence_r_var = prevalence_r_so){
        lambda1_eff_temp <- lambda1_var / prevalence_0_var
        return( lambda1_eff_temp * prevalence_r_var )
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
                         staff_time_var = staff_time_so, 
                         country_name_var = costs_data$Country,
                         select_var = list("india", "kenya", "nigeria", "vietnam"), 
                         other_costs = -99) {
      # select countries 
      country_total_var_temp <- country_total_var[country_name_var %in% select_var]
      country_cost_var_temp <- country_cost_var[country_name_var %in% select_var]
      # create country weight
      c_weights <- country_total_var_temp / sum(country_total_var_temp)
      # create country per capita costs, adjusted by staff time
      per_cap <- country_cost_var_temp * (1 + staff_time_var) / country_total_var_temp

      # replace contry costs with new one if there is a new country (only count that new country)
      #  (the weighthed sum of this scalar will just be the same number)
      if (other_costs>0) {
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
           new_costs_var1 = -99,
           countries_var1 = list("india", "kenya", "nigeria", "vietnam"),
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
      prevalence_0_var = prevalence_0_var1,
      prevalence_r_var = prevalence_r_var1
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
                                 prevalence_0_var = prevalence_0_var1,
                                 prevalence_r_var = prevalence_r_var1)
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
                              staff_time_var = staff_time_var1, 
                              country_name_var = df_costs_var1$Country,
                              select_var = countries_var1, 
                              other_costs = new_costs_var1)
    unit_test(cost1_in,  0.08480686, main_run_var = main_run_var1)
    # s2_ea_in <-- cost1_in (costs1_p2_f) <-- cost_data (costs1_p1_f())
    s2_ea_in <- s2_f_new(interest_var = interest_in_new,
                      unit_cost_local_var = cost1_in, 
                      ex_rate_var = 1)
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

    prevalence_r_sim <- rnorm(nsims, prevalence_r_var2, prevalence_r_var2_sd)
    prevalence_r_sim <- ifelse(prevalence_r_sim > 1, yes = 1, 
                          no = ifelse(prevalence_r_sim < 0, 0, prevalence_r_sim) )
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
                prevalence_r_var1 = prevalence_r_sim[i],
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

if  (FALSE){
npv_sim_all <-   sim.data1(nsims = nsims_so,                        
            gov_bonds_var2          = gov_bonds_so             ,                                    
            gov_bonds_var2_sd       = gov_bonds_so * 0.1          ,                                 
            inflation_var2          = inflation_so             ,                                    
            inflation_var2_sd       = inflation_so * 0.1          ,                                 
            gov_bonds_new_var2      = gov_bonds_new_so      ,          
            gov_bonds_new_var2_sd   = gov_bonds_new_so * 0.1,          
            inflation_new_var2      = inflation_new_so      ,        
            inflation_new_var2_sd   = inflation_new_so * 0.1,          
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
            unit_cost_local_new_var2 = unit_cost_2017usdppp_so,
            unit_cost_local_new_var2_sd = unit_cost_2017usdppp_so * 0.1  ,  
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
            prevalence_0_var2            = prevalence_0_so       ,  
            prevalence_0_var2_sd         = prevalence_0_so * 0.1    ,
            prevalence_r_var2            = prevalence_r_so       ,  
            prevalence_r_var2_sd         = prevalence_r_so * 0.1    ,                                                                         
            staff_time_var2         = staff_time_so    ,
            staff_time_var2_sd      = staff_time_so * 0.1,
            counts_par_var2         = counts_par_so    ,
            counts_par_var2_sd      = counts_par_sd_so ,
            costs_par_var2          = costs_par_so     ,
            costs_par_var2_sd       = costs_par_sd_so, 
            new_costs_var2 = 1, 
            countries_var2 = list("india", "kenya", "nigeria", "vietnam"))

}
```



```r
npv_sim_all <-   sim.data1(nsims = nsims_so,                        
            gov_bonds_var2          = gov_bonds_so             ,                                    
            gov_bonds_var2_sd       = gov_bonds_so * 0.1          ,                                 
            inflation_var2          = inflation_so             ,                                    
            inflation_var2_sd       = inflation_so * 0.1          ,                                 
            gov_bonds_new_var2      = gov_bonds_new_so      ,          
            gov_bonds_new_var2_sd   = gov_bonds_new_so * 0.1,          
            inflation_new_var2      = inflation_new_so      ,        
            inflation_new_var2_sd   = inflation_new_so * 0.1,          
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
            unit_cost_local_new_var2 = unit_cost_2017usdppp_so,
            unit_cost_local_new_var2_sd = unit_cost_2017usdppp_so * 0.1  ,  
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
            prevalence_0_var2            = prevalence_0_so       ,  
            prevalence_0_var2_sd         = prevalence_0_so * 0.1    ,
            prevalence_r_var2            = prevalence_r_so       ,  
            prevalence_r_var2_sd         = prevalence_r_so * 0.1    ,                                                                         
            staff_time_var2         = staff_time_so    ,
            staff_time_var2_sd      = staff_time_so * 0.1,
            counts_par_var2         = counts_par_so    ,
            counts_par_var2_sd      = counts_par_sd_so ,
            costs_par_var2          = costs_par_so     ,
            costs_par_var2_sd       = costs_par_sd_so, 
            new_costs_var2 = -99, 
            countries_var2 = list("india", "kenya", "nigeria", "vietnam"))


total_time <- npv_sim_all$total_time
position <- which( policy_estimates == policy_estimate_so )
npv_sim <- npv_sim_all[[ policy_estimates[position] ]]    
npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim), 1))
npv_for_text2 <- paste("SD NPV:\n ", round(sd(npv_sim), 1))
#Unit test the simulations for nsims = 100, 1000, 10000 UPDATE
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

npv_for_text <- paste("Median NPV:\n ", round(median(npv_sim),6))
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
    print(plot1)
```

![](05_final_opa_files/figure-html/run-mc-1.png)<!-- -->

```r
    #FUTURE: 
    #ggplotly(plot1)

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
